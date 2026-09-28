# M2: diagnostics on the main insurer's upper triangles (SPEC §9 M2).

# Age-to-age factors: rows = AY, cols = 1-2 .. 9-10.
ata_factors <- function(m) {
  f <- m[, -1, drop = FALSE] / m[, -ncol(m), drop = FALSE]
  colnames(f) <- paste0(1:(ncol(m) - 1), "-", 2:ncol(m))
  f
}

# Volume-weighted factor for column j using the latest n diagonals (n = Inf: all).
vw_factor <- function(m, j, n = Inf) {
  rows <- which(!is.na(m[, j + 1]))
  if (length(rows) == 0) return(NA_real_)
  rows <- utils::tail(rows, min(n, length(rows)))
  sum(m[rows, j + 1]) / sum(m[rows, j])
}

ldf_vector <- function(m, window = c("all", "latest5", "latest3")) {
  window <- match.arg(window)
  n <- switch(window, all = Inf, latest5 = 5, latest3 = 3)
  vapply(1:(ncol(m) - 1), function(j) vw_factor(m, j, n), numeric(1))
}

# Table of averages per development period (SPEC §9 M2).
ldf_averages <- function(m) {
  f <- ata_factors(m)
  medial <- apply(f, 2, function(v) {
    v <- v[!is.na(v)]
    if (length(v) <= 3) mean(v) else mean(sort(v)[-c(1, length(v))])
  })
  data.frame(
    period = colnames(f),
    vw_all = ldf_vector(m, "all"),
    simple = colMeans(f, na.rm = TRUE),
    vw_latest5 = ldf_vector(m, "latest5"),
    vw_latest3 = ldf_vector(m, "latest3"),
    medial_excl_hilo = medial,
    n = colSums(!is.na(f))
  )
}

# Pure chain ladder ultimate with a given factor vector (no tail).
cl_ultimate <- function(m, f, tail = 1) {
  k <- latest_lag(m)
  cdf <- rev(cumprod(rev(c(f, tail))))  # cdf[j] = f_j * ... * f_9 * tail; cdf[10] = tail
  latest(m) * cdf[k]
}

run_diagnostics <- function(code = main_insurer()) {
  up <- as_at(load_upper(), VALUATION_YEAR)
  tr <- insurer_triangles(up, code)
  out <- list()
  out$ldf_paid <- ldf_averages(tr$paid)
  out$ldf_inc <- ldf_averages(tr$case_inc)
  ratio <- tr$paid / tr$case_inc
  out$ratio <- ratio
  out$cl_compare <- data.frame(
    ay = as.integer(rownames(tr$paid)),
    ult_cl_paid = cl_ultimate(tr$paid, ldf_vector(tr$paid)),
    ult_cl_inc = cl_ultimate(tr$case_inc, ldf_vector(tr$case_inc)),
    ult_cl_paid_l5 = cl_ultimate(tr$paid, ldf_vector(tr$paid, "latest5")),
    ult_cl_inc_l5 = cl_ultimate(tr$case_inc, ldf_vector(tr$case_inc, "latest5"))
  )
  out$cl_compare$gap <- out$cl_compare$ult_cl_paid / out$cl_compare$ult_cl_inc - 1
  pt <- ChainLadder::as.triangle(tr$paid)
  it <- ChainLadder::as.triangle(tr$case_inc)
  out$cyeff_paid <- ChainLadder::cyEffTest(pt)
  out$cyeff_inc <- ChainLadder::cyEffTest(it)
  out$dfcor_paid <- ChainLadder::dfCorTest(pt)
  out$dfcor_inc <- ChainLadder::dfCorTest(it)
  norm_paid <- ChainLadder::as.triangle(tr$paid / tr$premium)
  out$inflation <- ChainLadder::checkTriangleInflation(norm_paid)
  mack <- ChainLadder::MackChainLadder(pt, est.sigma = "Mack")
  out$mack_paid <- mack

  dir <- path_in("outputs", "figures")
  grDevices::png(file.path(dir, "m2_paid_incurred_ratio.png"), width = 1000, height = 600, res = 120)
  graphics::matplot(t(ratio), type = "l", lty = 1, col = grDevices::hcl.colors(nrow(ratio), "Viridis"),
                    xlab = "Development lag (years)", ylab = "Paid / case incurred",
                    main = paste0("Paid / case incurred by AY - NAIC ", code, "\nUS commercial auto, net, NAIC Schedule P, as at 2007-12-31"))
  graphics::legend("bottomright", legend = rownames(ratio), col = grDevices::hcl.colors(nrow(ratio), "Viridis"), lty = 1, cex = 0.7, ncol = 2)
  grDevices::dev.off()
  grDevices::png(file.path(dir, "m2_mack_residuals_paid.png"), width = 1200, height = 900, res = 120)
  graphics::plot(mack, main = paste0("Mack diagnostics, paid - NAIC ", code))
  grDevices::dev.off()
  grDevices::png(file.path(dir, "m2_ata_paid.png"), width = 1000, height = 600, res = 120)
  f <- ata_factors(tr$paid)
  graphics::matplot(f[, 1:5], type = "b", pch = 1:5, lty = 1, xaxt = "n", xlab = "Accident year", ylab = "Age-to-age factor",
                    main = paste0("Paid age-to-age factors, periods 1-2 to 5-6 - NAIC ", code, "\nUS commercial auto, net, as at 2007-12-31"))
  graphics::axis(1, at = seq_len(nrow(f)), labels = rownames(f))
  graphics::legend("topright", legend = colnames(f)[1:5], pch = 1:5, col = 1:5, cex = 0.8)
  grDevices::dev.off()

  write_diag_tables(out, code)
  saveRDS(out, path_in("data", "upper", "diagnostics_main.rds"))
  invisible(out)
}

write_diag_tables <- function(out, code) {
  w <- function(df, name) utils::write.csv(df, path_in("outputs", name), row.names = FALSE)
  w(out$ldf_paid, "m2_ldf_paid.csv")
  w(out$ldf_inc, "m2_ldf_incurred.csv")
  w(out$cl_compare, "m2_cl_paid_vs_incurred.csv")
  tests <- data.frame(
    test = c("cyEffTest paid", "cyEffTest incurred", "dfCorTest paid", "dfCorTest incurred"),
    statistic = c(out$cyeff_paid$Z, out$cyeff_inc$Z, out$dfcor_paid$T_stat, out$dfcor_inc$T_stat),
    lower = c(out$cyeff_paid$Range[1], out$cyeff_inc$Range[1], out$dfcor_paid$Range[1], out$dfcor_inc$Range[1]),
    upper = c(out$cyeff_paid$Range[2], out$cyeff_inc$Range[2], out$dfcor_paid$Range[2], out$dfcor_inc$Range[2])
  )
  tests$within_range <- tests$statistic >= tests$lower & tests$statistic <= tests$upper
  w(tests, "m2_tests.csv")
  infl <- t(out$inflation$summ_table)
  w(data.frame(lag = seq_len(nrow(infl)), infl, row.names = NULL), "m2_inflation.csv")
}
