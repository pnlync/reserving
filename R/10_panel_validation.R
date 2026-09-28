# M6 (post-reveal): calibration of the predictive distributions across panel_stochastic
# (SPEC §7.2, §9 M6), and the one-year back-test across the panel (M8).

mid_rank_p <- function(sims, y) (sum(sims < y) + 0.5 * sum(sims == y)) / length(sims)

calibration_stats <- function(p) {
  n <- length(p)
  ps <- sort(p)
  d <- max(c(seq_len(n) / n - ps, ps - (seq_len(n) - 1) / n))
  two <- vapply(c(0.50, 0.75, 0.90, 0.95), function(c) mean(abs(p - 0.5) <= c / 2), numeric(1))
  list(n = n, cov_q75 = mean(p <= 0.75), cov_two_sided = stats::setNames(two, c("c50", "c75", "c90", "c95")),
       ks_d = d, ks_crit = 1.36 / sqrt(n), mean_p = mean(p))
}

# Actual payments to lag 10 after 2007 for one insurer (AY 1998-2007).
actual_future_to_lag10 <- function(full, code, v0 = VALUATION_YEAR) {
  C <- insurer_triangles(cohort(full, v0), code)$paid
  k <- VALUATION_YEAR - as.integer(rownames(C)) + 1
  sum(C[, 10] - C[cbind(seq_len(nrow(C)), k)])
}

calibrate_panel <- function(full, codes, a, spec, seeds, v0 = VALUATION_YEAR) {
  up0 <- as_at(cohort(full, v0), v0)
  rows <- lapply(seq_along(codes), function(i) {
    code <- codes[i]
    tr <- insurer_triangles(up0, code)
    y <- actual_future_to_lag10(full, code, v0)
    own <- tryCatch(selected_bootstrap(tr$paid, tr$premium, R = spec$replicates$panel_insurer,
                                       seed = seeds$panel_bootstrap + i, window = a$ldf$window,
                                       th = a$selection_thresholds, prior_cv = spec$prior_uncertainty$cv),
                    error = function(e) NULL)
    ok <- !is.null(own) && is.finite(own$phi) && all(is.finite(own$to_lag10))
    set.seed(seeds$panel_bootstrap + 10000 + i)
    pkg <- tryCatch(ChainLadder::BootChainLadder(ChainLadder::as.triangle(tr$paid), R = spec$replicates$panel_insurer,
                                                 process.distr = "od.pois")$IBNR.Totals, error = function(e) NULL)
    mk <- tryCatch(suppressMessages(ChainLadder::MackChainLadder(ChainLadder::as.triangle(tr$paid), est.sigma = "Mack")),
                   error = function(e) NULL)
    p_mack <- NA
    if (!is.null(mk)) {
      m <- sum(mk$FullTriangle[, 10] - ChainLadder::getLatestCumulative(mk$Triangle))
      se <- unname(mk$Total.Mack.S.E)
      if (is.finite(se) && m > 0 && se > 0) {
        s2 <- log(1 + (se / m)^2)
        p_mack <- stats::plnorm(y, log(m) - s2 / 2, sqrt(s2))
      }
    }
    data.frame(grcode = code, actual = y, bootstrap_ok = ok, phi = if (ok) own$phi else NA,
               p_custom = if (ok) mid_rank_p(own$to_lag10, y) else NA,
               mean_custom = if (ok) mean(own$to_lag10) else NA,
               p_package = if (!is.null(pkg)) mid_rank_p(pkg, y) else NA,
               p_mack = p_mack,
               sims = I(list(if (ok) own$to_lag10 else numeric(0))))
  })
  do.call(rbind, rows)
}

# Widening sensitivity (spec §7.2): x' = median * exp(lambda * log(x / median)); lambda is fitted
# on a random half to minimise the total gap between two-sided 50/75/90/95% coverage and nominal
# (the raw result shows intervals that are too narrow rather than off-centre), then tested on
# the other half. Sensitivity only.
widening_sensitivity <- function(cal, seed) {
  ok <- cal[cal$bootstrap_ok, ]
  set.seed(seed)
  fit_idx <- sample(nrow(ok), floor(nrow(ok) / 2))
  p_widen <- function(rows, lambda) vapply(rows, function(r) {
    x <- ok$sims[[r]]
    med <- stats::median(x)
    y <- ok$actual[r]
    if (med <= 0 || y <= 0) return(mid_rank_p(x, y))
    mid_rank_p(x, med * exp(log(y / med) / lambda))
  }, numeric(1))
  levels <- c(0.50, 0.75, 0.90, 0.95)
  gap <- function(p) sum(abs(vapply(levels, function(c) mean(abs(p - 0.5) <= c / 2), numeric(1)) - levels))
  grid <- seq(0.8, 4, by = 0.05)
  obj <- vapply(grid, function(l) gap(p_widen(fit_idx, l)), numeric(1))
  lambda <- grid[which.min(obj)]
  test_idx <- setdiff(seq_len(nrow(ok)), fit_idx)
  pt_raw <- ok$p_custom[test_idx]
  pt_w <- p_widen(test_idx, lambda)
  two <- function(p, c) mean(abs(p - 0.5) <= c / 2)
  list(lambda = lambda, n_fit = length(fit_idx), n_test = length(test_idx),
       cov_q75_test_raw = mean(pt_raw <= 0.75), cov_q75_test_widened = mean(pt_w <= 0.75),
       c50_test_raw = two(pt_raw, 0.5), c50_test_widened = two(pt_w, 0.5),
       c90_test_raw = two(pt_raw, 0.9), c90_test_widened = two(pt_w, 0.9),
       gap_test_raw = gap(pt_raw), gap_test_widened = gap(pt_w))
}

plot_pp <- function(cal, file) {
  grDevices::png(file, width = 800, height = 800, res = 130)
  graphics::plot(c(0, 1), c(0, 1), type = "n", xlab = "Uniform quantile", ylab = "Sorted predicted percentile of actual",
                 main = "Calibration p-p plot, payments to 120 months\nUS commercial auto, net, NAIC Schedule P, valuation 2007-12-31", cex.main = 0.85)
  graphics::abline(0, 1, col = "grey55")
  series <- list(`Custom bootstrap (selected rules)` = cal$p_custom, `Package ODP (pure CL)` = cal$p_package, `Mack lognormal` = cal$p_mack)
  cols <- c("black", "#4C9BE8", "#C62828")
  for (s in seq_along(series)) {
    p <- sort(stats::na.omit(series[[s]]))
    n <- length(p)
    graphics::lines((seq_len(n) - 0.5) / n, p, type = "s", col = cols[s], lwd = 2)
    crit <- 1.36 / sqrt(n)
  }
  graphics::abline(crit, 1, lty = 3, col = "grey40")
  graphics::abline(-crit, 1, lty = 3, col = "grey40")
  graphics::legend("topleft", c(names(series), "KS 5% band"), col = c(cols, "grey40"), lty = c(1, 1, 1, 3), lwd = c(2, 2, 2, 1), bty = "n", cex = 0.75)
  grDevices::dev.off()
}

run_calibration <- function() {
  full <- full_data()
  a <- read_config("assumptions_2007")
  spec <- locked_yaml("config/stochastic_spec.yaml")
  seeds <- read_config("seeds")
  panels <- utils::read.csv(path_in("outputs", "panels.csv"))
  codes <- panels$grcode[panels$panel_stochastic_provisional]
  cal <- calibrate_panel(full, codes, a, spec, seeds)
  out <- cal[, setdiff(names(cal), "sims")]
  utils::write.csv(out, path_in("outputs", "calibration_panel.csv"), row.names = FALSE)
  ok <- cal[cal$bootstrap_ok, ]
  st <- list(custom = calibration_stats(ok$p_custom), package = calibration_stats(stats::na.omit(ok$p_package)),
             mack = calibration_stats(stats::na.omit(ok$p_mack)))
  w <- widening_sensitivity(cal, seeds$widening_split)
  summ <- do.call(rbind, lapply(names(st), function(nm) {
    s <- st[[nm]]
    data.frame(model = nm, n = s$n, cov_q75 = s$cov_q75, c50 = s$cov_two_sided[["c50"]], c75 = s$cov_two_sided[["c75"]],
               c90 = s$cov_two_sided[["c90"]], c95 = s$cov_two_sided[["c95"]], ks_d = s$ks_d, ks_crit = s$ks_crit, mean_p = s$mean_p)
  }))
  utils::write.csv(summ, path_in("outputs", "calibration_summary.csv"), row.names = FALSE)
  utils::write.csv(data.frame(item = names(w), value = unlist(w)), path_in("outputs", "calibration_widening.csv"), row.names = FALSE)
  plot_pp(ok, path_in("outputs", "figures", "m6_calibration_pp.png"))
  invisible(list(cal = cal, summary = summ, widening = w))
}
