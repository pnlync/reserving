# M6: Mack, package ODP bootstrap and deterministic sensitivities (SPEC §9 M6).

mack_tables <- function(tr, tail) {
  one <- function(tri, tl, label) {
    mk <- ChainLadder::MackChainLadder(ChainLadder::as.triangle(tri), est.sigma = "Mack", tail = tl)
    ult <- mk$FullTriangle[, ncol(mk$FullTriangle)]
    latest_c <- ChainLadder::getLatestCumulative(mk$Triangle)
    data.frame(basis = label, ay = c(as.integer(rownames(tri)), NA),
               reserve = c(ult - latest_c, sum(ult - latest_c)),
               mack_se = c(mk$Mack.S.E[, ncol(mk$Mack.S.E)], mk$Total.Mack.S.E),
               process_se = c(mk$Mack.ProcessRisk[, ncol(mk$Mack.ProcessRisk)], utils::tail(mk$Total.ProcessRisk, 1)),
               parameter_se = c(mk$Mack.ParameterRisk[, ncol(mk$Mack.ParameterRisk)], utils::tail(mk$Total.ParameterRisk, 1)))
  }
  out <- rbind(one(tr$paid, tail, "paid"), one(tr$case_inc, incurred_tail(tr, tail), "incurred"))
  out$cv <- out$mack_se / out$reserve
  out
}

package_odp <- function(tr, R, seed) {
  set.seed(seed)
  ChainLadder::BootChainLadder(ChainLadder::as.triangle(tr$paid), R = R, process.distr = "od.pois")
}

# Deterministic sensitivities on the locked selection (tornado).
sensitivities <- function(up, a, exc) {
  base <- sum(auto_reserve(up, a$main_insurer, a, methods = exc)$unpaid)
  run <- function(...) sum(auto_reserve(up, a$main_insurer, a, methods = exc, ...)$unpaid)
  th <- a$selection_thresholds
  mt <- auto_reserve(up, a$main_insurer, a, methods = exc)
  alloc <- allocate_unpaid(mt)
  # +2% a year calendar inflation on future payments: payment in calendar year 2007 + t gets 1.02^t.
  cy_index <- outer(mt$ay, seq_len(ncol(alloc)), function(ay, j) ay + j - 1 - VALUATION_YEAR)
  infl <- sum(alloc * 1.02^pmax(cy_index, 0))
  s <- data.frame(
    assumption = c("ELR prior", "ELR prior", "Tail", "Tail", "LDF window", "Base triangle",
                   "Calendar inflation", "Selection thresholds", "Selection thresholds"),
    scenario = c("-5 points", "+5 points", "(tail-1) x 0.5", "(tail-1) x 1.5", "all years",
                 "incurred instead of paid", "+2% a year", "-10 points", "+10 points"),
    unpaid = c(run(elr_shift = -0.05), run(elr_shift = 0.05), run(tail_multiplier = 0.5), run(tail_multiplier = 1.5),
               run(window = "all"), run(incurred_basis = TRUE), infl,
               run(thresholds = list(cl_min_z = th$cl_min_z - 0.1, bk_min_z = th$bk_min_z - 0.1)),
               run(thresholds = list(cl_min_z = th$cl_min_z + 0.1, bk_min_z = th$bk_min_z + 0.1)))
  )
  s$base <- base
  s$change <- s$unpaid - base
  s$change_pct <- s$change / base
  s
}

plot_tornado <- function(s, code) {
  f <- path_in("outputs", "figures", "m6_tornado.png")
  agg <- do.call(rbind, lapply(split(s, s$assumption), function(d) data.frame(assumption = d$assumption[1],
    lo = min(0, d$change_pct), hi = max(0, d$change_pct))))
  agg <- agg[order(agg$hi - agg$lo), ]
  grDevices::png(f, width = 1100, height = 600, res = 130)
  op <- graphics::par(mar = c(4, 11, 4, 1))
  graphics::barplot(rbind(agg$lo, agg$hi - agg$lo) * 100, horiz = TRUE, names.arg = agg$assumption, las = 1,
                    col = c("white", "white"), border = NA, xlim = range(c(agg$lo, agg$hi)) * 100 * 1.15,
                    xlab = "Change in total unpaid (%)",
                    main = paste0("Sensitivity of selected unpaid - NAIC ", code, "\nUS commercial auto, net, NAIC Schedule P, as at 2007-12-31"), cex.main = 0.9)
  graphics::rect(agg$lo * 100, seq_along(agg$lo) * 1.2 - 1, agg$hi * 100, seq_along(agg$lo) * 1.2 - 0.2, col = "#4C9BE8", border = NA)
  graphics::abline(v = 0)
  graphics::par(op)
  grDevices::dev.off()
  f
}

run_uncertainty_main <- function() {
  up <- as_at(load_upper(), VALUATION_YEAR)
  a <- read_config("assumptions_2007")
  spec <- locked_yaml("config/stochastic_spec.yaml")
  seeds <- read_config("seeds")
  tr <- insurer_triangles(up, a$main_insurer)
  tail <- development_pattern(tr$paid, a$ldf$window)$tail
  mk <- mack_tables(tr, tail)
  utils::write.csv(mk, path_in("outputs", "m6_mack.csv"), row.names = FALSE)
  odp <- package_odp(tr, spec$replicates$main, seeds$package_bootstrap)
  saveRDS(list(IBNR.Totals = odp$IBNR.Totals, IBNR.ByOrigin = odp$IBNR.ByOrigin[, 1, ]), path_in("outputs", "sims_package_odp.rds"))
  sims <- run_main_bootstrap()
  exc <- vapply(a$exceptions, function(e) e$method, character(1))
  s <- sensitivities(up, a, exc)
  utils::write.csv(s, path_in("outputs", "m6_sensitivities.csv"), row.names = FALSE)
  plot_tornado(s, a$main_insurer)
  q <- function(x, p) unname(stats::quantile(x, p))
  sel_unpaid <- sum(locked_csv("outputs/selection_2007.csv")$unpaid)
  summ <- data.frame(
    model = c("Mack paid (tail)", "Mack incurred (tail; IBNR + case reserves)", "Package ODP bootstrap (pure CL, no tail)", "Custom bootstrap of selected reserve"),
    mean = c(mk$reserve[mk$basis == "paid" & is.na(mk$ay)],
             mk$reserve[mk$basis == "incurred" & is.na(mk$ay)] + sum(latest(tr$case_inc) - latest(tr$paid)),
             mean(odp$IBNR.Totals), mean(sims$reserve)),
    sd = c(mk$mack_se[mk$basis == "paid" & is.na(mk$ay)], mk$mack_se[mk$basis == "incurred" & is.na(mk$ay)],
           stats::sd(odp$IBNR.Totals), stats::sd(sims$reserve)),
    q75 = c(NA, NA, q(odp$IBNR.Totals, 0.75), q(sims$reserve, 0.75)),
    q95 = c(NA, NA, q(odp$IBNR.Totals, 0.95), q(sims$reserve, 0.95)),
    q995 = c(NA, NA, q(odp$IBNR.Totals, 0.995), q(sims$reserve, 0.995))
  )
  summ$cv <- summ$sd / summ$mean
  summ$selected_unpaid <- sel_unpaid
  utils::write.csv(summ, path_in("outputs", "m6_uncertainty_summary.csv"), row.names = FALSE)
  plot_reserve_distribution(sims, odp, sel_unpaid, a$main_insurer)
  plot_odp_residuals(tr)
  invisible(summ)
}

plot_reserve_distribution <- function(sims, odp, sel_unpaid, code) {
  f <- path_in("outputs", "figures", "m6_reserve_distribution.png")
  grDevices::png(f, width = 1100, height = 600, res = 130)
  d1 <- stats::density(sims$reserve / 1000)
  d2 <- stats::density(odp$IBNR.Totals / 1000)
  graphics::plot(d1, main = paste0("Distribution of unpaid claims - NAIC ", code, "\nUS commercial auto, net, NAIC Schedule P, as at 2007-12-31"),
                 xlab = "Unpaid, USD m", ylim = c(0, max(d1$y, d2$y)), lwd = 2, cex.main = 0.9, xlim = range(d1$x, d2$x))
  graphics::lines(d2, col = "grey55", lwd = 2, lty = 2)
  graphics::abline(v = sel_unpaid / 1000, col = "#C62828")
  graphics::legend("topright", c("Custom bootstrap of selected reserve", "Package ODP bootstrap (pure CL)", "Selected unpaid"),
                   col = c("black", "grey55", "#C62828"), lty = c(1, 2, 1), lwd = c(2, 2, 1), bty = "n", cex = 0.8)
  grDevices::dev.off()
}

plot_odp_residuals <- function(tr) {
  fit <- odp_fit(tr$paid)
  r <- fit$r * sqrt(fit$n_cells / (fit$n_cells - fit$p))
  lag <- col(r)
  cy <- as.integer(rownames(tr$paid))[row(r)] + lag - 1
  ok <- !is.na(r)
  f <- path_in("outputs", "figures", "m6_odp_residuals.png")
  grDevices::png(f, width = 1100, height = 500, res = 130)
  op <- graphics::par(mfrow = c(1, 2))
  graphics::plot(lag[ok], r[ok], xlab = "Development lag", ylab = "Adjusted Pearson residual", main = "By lag", pch = 16, col = "#4C9BE8")
  graphics::abline(h = 0)
  graphics::plot(cy[ok], r[ok], xlab = "Calendar year", ylab = "Adjusted Pearson residual", main = "By calendar year", pch = 16, col = "#4C9BE8")
  graphics::abline(h = 0)
  graphics::par(op)
  grDevices::dev.off()
}

# Mack (1993) total standard error written out explicitly, as rebuilt in the Excel check.
mack_total_se_explicit <- function(C) {
  n <- nrow(C)
  f <- ldf_vector(C, "all")
  S <- vapply(1:(n - 1), function(j) sum(C[1:(n - j), j]), numeric(1))
  sig2 <- vapply(1:(n - 2), function(j) {
    rows <- 1:(n - j)
    sum(C[rows, j] * (C[rows, j + 1] / C[rows, j] - f[j])^2) / (length(rows) - 1)
  }, numeric(1))
  sig2 <- c(sig2, min(sig2[n - 2]^2 / sig2[n - 3], sig2[n - 3], sig2[n - 2]))
  full <- C
  for (j in 2:n) full[is.na(full[, j]), j] <- full[is.na(full[, j]), j - 1] * f[j - 1]
  k <- latest_lag(C)
  ult <- full[, n]
  msep <- numeric(n)
  cross <- numeric(n)
  for (i in seq_len(n)) {
    if (k[i] >= n) next
    ks <- k[i]:(n - 1)
    msep[i] <- ult[i]^2 * sum(sig2[ks] / f[ks]^2 * (1 / full[i, ks] + 1 / S[ks]))
    younger <- if (i < n) sum(ult[(i + 1):n]) else 0
    cross[i] <- ult[i] * younger * sum(2 * sig2[ks] / f[ks]^2 / S[ks])
  }
  sqrt(sum(msep) + sum(cross))
}

add_mack_sheet <- function(wb, tr) {
  ws <- "mack"
  openxlsx::addWorksheet(wb, ws)
  col <- function(j) openxlsx::int2col(j)
  put <- function(x, r, c) openxlsx::writeData(wb, ws, x, startRow = r, startCol = c, colNames = FALSE)
  fml <- function(f, r, c) openxlsx::writeFormula(wb, ws, f, startRow = r, startCol = c)
  C <- tr$paid
  n <- nrow(C)
  put("Mack (1993) total standard error, paid, all-year volume-weighted factors, no tail (USD thousands)", 1, 1)
  put(t(c("AY", paste0("lag", 1:n))), 3, 1)
  r0 <- 4
  for (i in 1:n) {
    put(as.integer(rownames(C)[i]), r0 + i - 1, 1)
    for (j in which(!is.na(C[i, ]))) put(C[i, j], r0 + i - 1, 1 + j)
  }
  rows <- function(j) c(r0, r0 + n - j - 1)          # AYs with lag j+1 present
  put("f", 15, 1); put("sigma2", 16, 1); put("S", 17, 1)
  for (j in 1:(n - 1)) {
    rr <- rows(j)
    cj <- col(j + 1); cn <- col(j + 2)
    fml(sprintf("SUM(%s%d:%s%d)/SUM(%s%d:%s%d)", cn, rr[1], cn, rr[2], cj, rr[1], cj, rr[2]), 15, j + 1)
    fml(sprintf("SUM(%s%d:%s%d)", cj, rr[1], cj, rr[2]), 17, j + 1)
    if (j <= n - 2) fml(sprintf("SUMPRODUCT(%s%d:%s%d,(%s%d:%s%d/%s%d:%s%d-%s15)^2)/(%d-1)", cj, rr[1], cj, rr[2], cn, rr[1], cn, rr[2], cj, rr[1], cj, rr[2], cj, rr[2] - rr[1] + 1), 16, j + 1)
  }
  a <- col(n - 2); b <- col(n - 1)
  fml(sprintf("MIN(%s16^2/%s16,%s16,%s16)", b, a, a, b), 16, n)
  # Completed triangle, rows 20..29.
  f0 <- 20
  put("Completed triangle", 19, 1)
  for (i in 1:n) {
    r <- f0 + i - 1
    fml(sprintf("A%d", r0 + i - 1), r, 1)
    for (j in 1:n) {
      if (!is.na(C[i, j])) fml(sprintf("%s%d", col(j + 1), r0 + i - 1), r, j + 1)
      else fml(sprintf("%s%d*%s15", col(j), r, col(j)), r, j + 1)
    }
  }
  # MSEP by AY (col M) and cross terms (col N), rows 20..29.
  put(t(c("msep_i", "cross_i")), 19, 13)
  k <- latest_lag(C)
  for (i in 1:n) {
    r <- f0 + i - 1
    if (k[i] >= n) { put(0, r, 13); put(0, r, 14); next }
    c1 <- col(k[i] + 1); c2 <- col(n)
    fml(sprintf("%s%d^2*SUMPRODUCT(%s$16:%s$16/%s$15:%s$15^2,1/%s%d:%s%d+1/%s$17:%s$17)",
                col(n + 1), r, c1, c2, c1, c2, c1, r, c2, r, c1, c2), r, 13)
    younger <- if (i < n) sprintf("SUM(%s%d:%s%d)", col(n + 1), r + 1, col(n + 1), f0 + n - 1) else "0"
    fml(sprintf("%s%d*%s*SUMPRODUCT(2*%s$16:%s$16/%s$15:%s$15^2/%s$17:%s$17)",
                col(n + 1), r, younger, c1, c2, c1, c2, c1, c2), r, 14)
  }
  put("Total Mack SE", 31, 1)
  fml(sprintf("SQRT(SUM(M%d:M%d)+SUM(N%d:N%d))", f0, f0 + n - 1, f0, f0 + n - 1), 31, 2)
  wb
}
