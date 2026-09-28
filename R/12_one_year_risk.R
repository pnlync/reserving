# M8: one-year reserve deterioration (SPEC §9 M8). CDR = R_2007 - (X_2008 + R_2008), L = -CDR.

merz_wuthrich <- function(C) {
  mk <- suppressMessages(ChainLadder::MackChainLadder(ChainLadder::as.triangle(C), est.sigma = "Mack"))
  cdr <- ChainLadder::CDR(mk)
  list(cdr_se = cdr["Total", "CDR(1)S.E."], mack_se = cdr["Total", "Mack.S.E."],
       reserve = sum(mk$FullTriangle[, ncol(mk$FullTriangle)] - ChainLadder::getLatestCumulative(mk$Triangle)))
}

# Append a simulated next diagonal to the observed paid triangle (AYs below lag 10 only).
append_diagonal <- function(C, diag) {
  k <- latest_lag(C)
  C1 <- C
  for (i in seq_len(nrow(C))) if (k[i] < ncol(C)) C1[i, k[i] + 1] <- C[i, k[i]] + diag[i]
  C1
}

# Re-reserve every replicate with the locked rules one year on ("actuary in the box").
# Returns per replicate: payments in the year, closing reserve, and the closing reserve's
# own future cash flows by calendar year (for discounting).
# `load_remaining`: incurred load still unpaid after the year (fixed, see locked_method_table()).
simulate_rereserve <- function(C, prem, sims, window, th, load_remaining = NULL, tail = TRUE) {
  R <- nrow(sims$diag_next)
  n <- nrow(C)
  ays <- as.integer(rownames(C))
  next_cy <- max(ays) + 1
  cys <- (next_cy + 1):(next_cy + 10)
  closing <- numeric(R)
  closing_cf <- matrix(0, R, length(cys), dimnames = list(NULL, cys))
  fun <- if (tail) lean_reserve else lean_reserve_notail
  for (s in seq_len(R)) {
    C1 <- append_diagonal(C, sims$diag_paid_part[s, ])
    res <- fun(C1, prem, window, th, method = NULL, load = load_remaining)
    done <- latest_lag(C) >= ncol(C)            # AY already at lag 10: its tail was paid in the year
    res$unpaid[done] <- 0
    closing[s] <- sum(res$unpaid)
    mu <- future_means(res)
    for (i in seq_len(n)) {
      if (res$k[i] >= 11 || done[i]) next
      js <- (res$k[i] + 1):11
      cy <- ays[i] + js - 1
      hit <- match(cy, cys)
      closing_cf[s, hit] <- closing_cf[s, hit] + mu[i, js]
    }
  }
  list(paid_year = rowSums(sims$diag_next), closing = closing, closing_cf = closing_cf)
}

run_one_year <- function() {
  up <- as_at(load_upper(), VALUATION_YEAR)
  a <- read_config("assumptions_2007")
  s2 <- read_config("solvency2")
  tr <- insurer_triangles(up, a$main_insurer)
  sims <- readRDS(path_in("outputs", "sims_main.rds"))
  sel <- locked_csv("outputs/selection_2007.csv")
  lt <- locked_method_table(sel)
  mw <- merz_wuthrich(tr$paid)
  set.seed(read_config("seeds")$package_bootstrap)
  bcl <- ChainLadder::BootChainLadder(ChainLadder::as.triangle(tr$paid), R = 10000, process.distr = "od.pois")
  bcdr <- ChainLadder::CDR(bcl)
  mt0 <- auto_reserve(up, a$main_insurer, a, methods = vapply(a$exceptions, function(e) e$method, character(1)))
  g0 <- 1 / cdf_from(attr(mt0, "pattern")$f, attr(mt0, "pattern")$tail)
  k0 <- mt0$lag
  share_next <- ifelse(k0 < 10, (g0[pmin(k0 + 1, 10)] - g0[k0]) / (1 - g0[k0]), 1)
  load_rem <- lt$load * (1 - share_next)
  rr <- simulate_rereserve(tr$paid, tr$premium, sims, a$ldf$window, a$selection_thresholds, load_remaining = load_rem)
  r2007 <- sum(sel$unpaid)
  cdr <- r2007 - (rr$paid_year + rr$closing)
  # Discounted on the 2007 curve, incl. ULAE: opening = PV of the locked selection's cash flows.
  p <- gsw_parameters()
  u <- 1 + a$ulae$base
  mt <- auto_reserve(up, a$main_insurer, a, methods = vapply(a$exceptions, function(e) e$method, character(1)))
  open_cf <- colSums(predicted_by_cy(mt, 2008:2017)) + tail_by_cy(mt, 2008:2017)
  df <- discount_factor((2008:2017) - VALUATION_YEAR - 0.5, p)
  open_pv <- u * sum(open_cf * df)
  df_close <- discount_factor(as.integer(colnames(rr$closing_cf)) - VALUATION_YEAR - 0.5, p)
  close_pv <- u * (rr$paid_year * df[1] + as.vector(rr$closing_cf %*% df_close))
  lic <- utils::read.csv(path_in("outputs", "lic_2007.csv"))
  be_2007 <- lic$value[lic$item == "discounted_be"]
  l_disc <- close_pv - open_pv
  out <- data.frame(sim_id = seq_along(cdr), paid_2008 = rr$paid_year, reserve_2008 = rr$closing, cdr = cdr,
                    l_over_be = l_disc / be_2007)
  utils::write.csv(out, path_in("outputs", "one_year_cdr_sims.csv"), row.names = FALSE)
  L <- -cdr
  usp_sigma <- s2$usp_credibility_10y * mw$cdr_se / mw$reserve + (1 - s2$usp_credibility_10y) * s2$reserve_risk_sigma_mvl
  summ <- data.frame(
    item = c("mw_cdr_se", "mw_mack_se", "mw_reserve", "boot_cdr_se_package", "sim_cdr_mean", "sim_cdr_sd",
             "sim_L995_undiscounted", "sim_L995_over_selected_unpaid", "l995_over_be", "be_2007_discounted",
             "open_pv_selected", "sf_reserve_risk_factor", "usp_sigma", "usp_factor", "ultimate_sd_selected"),
    value = c(mw$cdr_se, mw$mack_se, mw$reserve, bcdr["Total", "CDR(1)S.E"], mean(cdr), stats::sd(cdr),
              unname(stats::quantile(L, 0.995)), unname(stats::quantile(L, 0.995)) / r2007,
              unname(stats::quantile(l_disc, 0.995)) / be_2007, be_2007, open_pv,
              s2$multiplier * s2$reserve_risk_sigma_mvl, usp_sigma, s2$multiplier * usp_sigma, stats::sd(sims$reserve))
  )
  utils::write.csv(summ, path_in("outputs", "m8_one_year_summary.csv"), row.names = FALSE)
  plot_one_year(out, s2, a$main_insurer)
  invisible(summ)
}

# Tail payments (development year 11) by calendar year for an auto_reserve table.
tail_by_cy <- function(mt, cys) {
  alloc <- allocate_unpaid(mt)
  vapply(cys, function(cy) sum(alloc[mt$ay + 10 == cy, ncol(alloc)]), numeric(1))
}

plot_one_year <- function(out, s2, code) {
  f <- path_in("outputs", "figures", "m8_one_year_distribution.png")
  grDevices::png(f, width = 1100, height = 600, res = 130)
  x <- out$l_over_be * 100
  graphics::hist(x, breaks = 60, col = "#4C9BE8", border = "white", xlim = range(c(x, 30)),
                 main = paste0("One-year reserve deterioration L / opening BE - NAIC ", code,
                               "\nUS commercial auto, net, NAIC Schedule P, as at 2007-12-31 (discounted, incl. ULAE)"),
                 xlab = "L as % of opening discounted net BE", cex.main = 0.9)
  q <- stats::quantile(x, 0.995)
  graphics::abline(v = q, col = "#C62828", lwd = 2)
  graphics::abline(v = 100 * s2$multiplier * s2$reserve_risk_sigma_mvl, col = "black", lwd = 2, lty = 2)
  graphics::legend("topright", c(sprintf("99.5%% simulated: %.1f%%", q), "Standard formula 3 x 9% = 27%"),
                   col = c("#C62828", "black"), lty = c(1, 2), lwd = 2, bty = "n", cex = 0.8)
  grDevices::dev.off()
}

# ---- One-year back-test (post-reveal) ----

# Actual one-year CDR with the automatic rules: R_2007 and R_2008 both from auto_reserve().
actual_cdr_auto <- function(full, code, a, v0 = VALUATION_YEAR) {
  x <- cohort(full, v0)
  r0 <- sum(auto_reserve(as_at(x, v0), code, a)$unpaid)
  r1 <- sum(auto_reserve(as_at(x, v0 + 1), code, a)$unpaid)
  inc <- cum_to_inc(insurer_triangles(x, code)$paid)
  ays <- as.integer(rownames(inc))
  j <- v0 + 1 - ays + 1
  paid <- sum(inc[cbind(seq_along(ays), pmin(j, 10))][j <= 10])
  r0 - (paid + r1)
}

panel_one_year <- function(full, codes, a, spec, seeds, R = spec$replicates$panel_insurer, v0 = VALUATION_YEAR) {
  up0 <- as_at(cohort(full, v0), v0)
  rows <- lapply(seq_along(codes), function(i) {
    code <- codes[i]
    tr <- insurer_triangles(up0, code)
    out <- tryCatch({
      sims <- selected_bootstrap(tr$paid, tr$premium, R = R, seed = seeds$one_year_rereserve + i,
                                 window = a$ldf$window, th = a$selection_thresholds, prior_cv = spec$prior_uncertainty$cv)
      r0 <- sum(lean_reserve(tr$paid, tr$premium, a$ldf$window, a$selection_thresholds)$unpaid)
      rr <- simulate_rereserve(tr$paid, tr$premium, sims, a$ldf$window, a$selection_thresholds)
      cdr_sim <- r0 - (rr$paid_year + rr$closing)
      y <- actual_cdr_auto(full, code, a, v0)
      data.frame(grcode = code, cdr_actual = y, cdr_sim_mean = mean(cdr_sim), cdr_sim_sd = stats::sd(cdr_sim),
                 p = mid_rank_p(cdr_sim, y))
    }, error = function(e) data.frame(grcode = code, cdr_actual = NA, cdr_sim_mean = NA, cdr_sim_sd = NA, p = NA))
    out
  })
  do.call(rbind, rows)
}

run_one_year_backtest <- function() {
  full <- full_data()
  a <- read_config("assumptions_2007")
  spec <- locked_yaml("config/stochastic_spec.yaml")
  seeds <- read_config("seeds")
  sims <- utils::read.csv(path_in("outputs", "one_year_cdr_sims.csv"))
  rolling <- utils::read.csv(path_in("outputs", "backtest_rolling.csv"))
  cdr_2008 <- rolling$cdr[rolling$valuation == VALUATION_YEAR + 1]
  panels <- utils::read.csv(path_in("outputs", "panels.csv"))
  cal <- utils::read.csv(path_in("outputs", "calibration_panel.csv"))
  codes <- cal$grcode[cal$bootstrap_ok]
  po <- panel_one_year(full, codes, a, spec, seeds)
  utils::write.csv(po, path_in("outputs", "one_year_panel.csv"), row.names = FALSE)
  p <- stats::na.omit(po$p)
  st <- calibration_stats(p)
  res <- data.frame(item = c("main_cdr_2008_actual", "main_cdr_2008_percentile", "panel_n", "panel_cov_q75", "panel_ks_d", "panel_ks_crit", "panel_mean_p"),
                    value = c(cdr_2008, mid_rank_p(sims$cdr, cdr_2008), st$n, st$cov_q75, st$ks_d, st$ks_crit, st$mean_p))
  utils::write.csv(res, path_in("outputs", "one_year_backtest.csv"), row.names = FALSE)
  invisible(res)
}
