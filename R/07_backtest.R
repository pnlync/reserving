# M5: reveal and back-test (SPEC §9 M5, CONTRACT §3). Functions take the full data
# (upper + revealed holdout) as an as_at() object, so they can be exercised on a
# synthetic lower triangle in the tests before the tag exists.

# Upper + holdout as one as_at() object at the end of the run-off. Needs the tag.
full_data <- function() {
  up <- load_upper()
  d <- rbind(as.data.frame(up), as.data.frame(reveal()))
  as_at(d, max(d$cy))
}

# Keep only AYs up to the valuation year (the reserved cohort).
cohort <- function(x, v0) {
  v <- valuation_of(x)
  out <- as.data.frame(unclass(x))[x$ay <= v0, , drop = FALSE]
  attr(out, "valuation") <- v
  class(out) <- c("asat", "data.frame")
  out
}

# Rebuild an allocatable table from the locked selection and the locked pattern.
locked_table <- function(sel, pattern) {
  mt <- data.frame(ay = sel$ay, lag = VALUATION_YEAR - sel$ay + 1, unpaid = sel$unpaid, paid = sel$paid)
  attr(mt, "pattern") <- list(f = pattern$factor[1:9], tail = pattern$factor[10])
  mt
}

predicted_to_lag10 <- function(mt) {
  alloc <- allocate_unpaid(mt)
  rowSums(alloc[, 1:10, drop = FALSE])
}

next_year_predicted <- function(mt) {
  alloc <- allocate_unpaid(mt)
  vapply(seq_len(nrow(mt)), function(i) alloc[i, mt$lag[i] + 1], numeric(1))
}

# Tests A-C for one reserved cohort (valuation v0). `mt` = table with ay, lag, unpaid and
# pattern (the locked selection for the main insurer); `cl` = pure paid CL table.
score_tests_abc <- function(full, code, mt, cl, v0 = VALUATION_YEAR) {
  tr <- insurer_triangles(cohort(full, v0), code)
  C <- tr$paid
  I <- tr$inc_booked
  k <- mt$lag
  idx <- cbind(seq_len(nrow(C)), k)
  c_k <- C[idx]
  actual_f <- C[, 10] - c_k
  pred_sel <- predicted_to_lag10(mt)
  pred_cl <- predicted_to_lag10(cl)
  b_rows <- mt$ay > min(mt$ay)                      # AY 1999-2007 (AY 1998's proxy is the booked figure)
  proxy <- sum(I[b_rows, 10] - c_k[b_rows])
  booked_unpaid <- sum(I[idx][b_rows] - c_k[b_rows])
  inc <- cum_to_inc(C)
  # Test C: AYs already at lag 10 have only the tail left, with no observed next year: excluded.
  actual_next <- vapply(seq_len(nrow(C)), function(i) if (k[i] < 10) inc[i, k[i] + 1] else NA_real_, numeric(1))
  pred_next <- ifelse(k < 10, next_year_predicted(mt), NA_real_)
  by_ay <- data.frame(ay = mt$ay, lag = k, pct_dev = 1 / cdf_from(attr(mt, "pattern")$f, attr(mt, "pattern")$tail)[k],
                      unpaid_sel = mt$unpaid, unpaid_cl = cl$unpaid,
                      pred_to120_sel = pred_sel, pred_to120_cl = pred_cl, actual_to120 = actual_f,
                      booked_unpaid = I[idx] - c_k, proxy_unpaid = I[, 10] - c_k,
                      pred_next = pred_next, actual_next = actual_next)
  totals <- list(
    err_paid120_selected = (sum(pred_sel) - sum(actual_f)) / sum(actual_f),
    err_paid120_cl = (sum(pred_cl) - sum(actual_f)) / sum(actual_f),
    err_inc120_selected = (sum(mt$unpaid[b_rows]) - proxy) / proxy,
    err_inc120_cl = (sum(cl$unpaid[b_rows]) - proxy) / proxy,
    err_inc120_booked = (booked_unpaid - proxy) / proxy,
    ae_next_total = sum(actual_next, na.rm = TRUE) / sum(pred_next, na.rm = TRUE),
    proxy_unpaid = proxy, booked_unpaid_b = booked_unpaid,
    selected_unpaid_b = sum(mt$unpaid[b_rows]), cl_unpaid_b = sum(cl$unpaid[b_rows]),
    actual_to120 = sum(actual_f), pred_to120_sel = sum(pred_sel), pred_to120_cl = sum(pred_cl)
  )
  list(by_ay = by_ay, totals = totals)
}

# Test D: mechanical re-reserve at v0+1..last with the locked rules (thresholds automatic,
# prior and tail recomputed; the locked AY exceptions kept). Reserve path and actual CDR.
rolling_rereserve <- function(full, code, a, locked_unpaid, v0 = VALUATION_YEAR, last = v0 + 8, methods = NULL) {
  x <- cohort(full, v0)
  inc <- cum_to_inc(insurer_triangles(x, code)$paid)
  ays <- as.integer(rownames(inc))
  rows <- list()
  auto0 <- sum(auto_reserve(as_at(x, v0), code, a, methods = methods)$unpaid)
  prev <- locked_unpaid
  prev_auto <- auto0
  rows[[1]] <- data.frame(valuation = v0, reserve = locked_unpaid, reserve_auto = auto0, paid_in_year = NA, cdr = NA, cdr_auto_chain = NA)
  for (v in (v0 + 1):last) {
    r <- sum(auto_reserve(as_at(x, v), code, a, methods = methods)$unpaid)
    j <- v - ays + 1
    paid <- sum(inc[cbind(seq_along(ays), pmin(j, 10))][j <= 10])
    rows[[length(rows) + 1]] <- data.frame(valuation = v, reserve = r, reserve_auto = r, paid_in_year = paid,
                                           cdr = prev - (paid + r), cdr_auto_chain = prev_auto - (paid + r))
    prev <- r
    prev_auto <- r
  }
  do.call(rbind, rows)
}

# Panel back-test: automatic pipeline (no exceptions), pure CL and rule, for every insurer.
panel_backtest_run <- function(full, codes, a, v0 = VALUATION_YEAR) {
  rows <- lapply(codes, function(code) {
    x0 <- as_at(cohort(full, v0), v0)
    rule <- auto_reserve(x0, code, a)
    cl <- auto_reserve(x0, code, a, pure_cl = TRUE)
    sc <- score_tests_abc(full, code, rule, cl, v0)
    t <- sc$totals
    b <- sc$by_ay
    band <- cut(rule$pct_dev, c(-Inf, 0.3, 0.7, Inf), labels = c("z<30%", "30-70%", "z>=70%"), right = FALSE)
    band_err <- tapply(seq_along(band), band, function(ix) {
      d <- sum(b$actual_to120[ix]); if (d <= 0) NA else (sum(b$pred_to120_sel[ix]) - d) / d
    })
    band_err_cl <- tapply(seq_along(band), band, function(ix) {
      d <- sum(b$actual_to120[ix]); if (d <= 0) NA else (sum(b$pred_to120_cl[ix]) - d) / d
    })
    data.frame(grcode = code, prem_2007 = rule$prem_net[rule$ay == v0], actual_to120 = t$actual_to120,
               err_rule = t$err_paid120_selected, err_cl = t$err_paid120_cl,
               booked_below_proxy = t$booked_unpaid_b < t$proxy_unpaid,
               err_band_young_rule = band_err[["z<30%"]], err_band_mid_rule = band_err[["30-70%"]], err_band_mature_rule = band_err[["z>=70%"]],
               err_band_young_cl = band_err_cl[["z<30%"]], err_band_mid_cl = band_err_cl[["30-70%"]], err_band_mature_cl = band_err_cl[["z>=70%"]])
  })
  out <- do.call(rbind, rows)
  out$scored <- is.finite(out$err_rule) & out$actual_to120 > 0
  sc <- out[out$scored, ]
  out$size_tercile <- NA
  out$size_tercile[out$scored] <- as.integer(cut(rank(sc$prem_2007), 3, labels = FALSE))
  out
}

run_backtest <- function() {
  full <- full_data()
  a <- read_config("assumptions_2007")
  code <- a$main_insurer
  sel <- locked_csv("outputs/selection_2007.csv")
  pattern <- locked_csv("outputs/pattern_2007.csv")
  mt <- locked_table(sel, pattern)
  x0 <- as_at(full, VALUATION_YEAR)
  cl <- auto_reserve(x0, code, a, pure_cl = TRUE)
  sc <- score_tests_abc(full, code, mt, cl)
  exc <- vapply(a$exceptions, function(e) e$method, character(1))
  d <- rolling_rereserve(full, code, a, sum(sel$unpaid), methods = exc)
  utils::write.csv(sc$by_ay, path_in("outputs", "backtest_main.csv"), row.names = FALSE)
  utils::write.csv(data.frame(item = names(sc$totals), value = unlist(sc$totals)), path_in("outputs", "backtest_main_totals.csv"), row.names = FALSE)
  utils::write.csv(d, path_in("outputs", "backtest_rolling.csv"), row.names = FALSE)
  panels <- utils::read.csv(path_in("outputs", "panels.csv"))
  pb <- panel_backtest_run(full, panels$grcode[panels$panel_backtest], a)
  utils::write.csv(pb, path_in("outputs", "panel_backtest.csv"), row.names = FALSE)
  plot_three_way(sc$totals, code)
  plot_rolling(d, code)
  plot_panel_errors(pb)
  invisible(list(main = sc, rolling = d, panel = pb))
}

plot_three_way <- function(t, code) {
  f <- path_in("outputs", "figures", "m5_three_way.png")
  grDevices::png(f, width = 1000, height = 600, res = 130)
  vals <- c(t$selected_unpaid_b, t$booked_unpaid_b, t$cl_unpaid_b, t$proxy_unpaid) / 1000
  cols <- c("black", "#C62828", "grey55", "#2E7D32")
  bp <- graphics::barplot(vals, names.arg = c("Locked selection", "Insurer booked", "Pure paid CL", "120-month proxy"),
                          col = cols, border = NA, ylab = "Unpaid at 2007-12-31, USD m (AY 1999-2007)", ylim = c(0, max(vals) * 1.15),
                          main = paste0("Blind back-test: unpaid vs 120-month incurred proxy - NAIC ", code,
                                        "\nUS commercial auto, net, NAIC Schedule P, valuation 2007-12-31"), cex.main = 0.9)
  errs <- c(t$err_inc120_selected, t$err_inc120_booked, t$err_inc120_cl, NA)
  graphics::text(bp, vals, ifelse(is.na(errs), "", sprintf("%+.1f%%", 100 * errs)), pos = 3, cex = 0.85)
  grDevices::dev.off()
}

plot_rolling <- function(d, code) {
  f <- path_in("outputs", "figures", "m5_rolling_reserve.png")
  grDevices::png(f, width = 1000, height = 600, res = 130)
  cum_paid <- c(0, cumsum(d$paid_in_year[-1]))
  graphics::plot(d$valuation, (d$reserve + cum_paid) / 1000, type = "b", pch = 16, lwd = 2,
                 ylab = "Reserve + paid since 2007, USD m", xlab = "Valuation (year end)",
                 main = paste0("Rolling mechanical re-reserve, AY 1998-2007 - NAIC ", code,
                               "\nUS commercial auto, net, NAIC Schedule P; 2007 = locked selection"), cex.main = 0.9)
  graphics::abline(h = d$reserve[1] / 1000, lty = 2, col = "grey55")
  grDevices::dev.off()
}

plot_panel_errors <- function(pb) {
  f <- path_in("outputs", "figures", "m5_panel_errors.png")
  s <- pb[pb$scored, ]
  grDevices::png(f, width = 1000, height = 600, res = 130)
  graphics::boxplot(list(`Pure paid CL` = 100 * s$err_cl, `Locked rule (BF/Benktander/CL)` = 100 * s$err_rule),
                    horizontal = TRUE, las = 1, col = c("grey80", "#4C9BE8"), xlab = "Test A error: (predicted - actual) / actual payments to 120 months (%)",
                    main = sprintf("Panel back-test, %d insurers - US commercial auto, net, NAIC Schedule P, valuation 2007-12-31", nrow(s)), cex.main = 0.85)
  graphics::abline(v = 0, lty = 2)
  grDevices::dev.off()
}
