# M4: automatic reserving rules, pseudo-historical validation, selection (SPEC §9 M4).
# auto_reserve() is the "actuary in the box": the locked rules applied mechanically at any
# valuation. It is reused by the back-test (Test D), the panel runs and the bootstrap.

# Log-linear tail on (f - 1), exactly as ChainLadder:::tailfactor (used by
# MackChainLadder(tail = TRUE)), but returning the extrapolated factor for each
# later development period so that payments can be allocated year by year.
# f: observed factors for periods 1..n. Returns factors for periods n+1..n+100.
loglinear_extension <- function(f) {
  n <- length(f)
  if (n >= 3 && f[n - 2] * f[n - 1] > 1.0001) {
    fn <- which(f > 1)
    co <- stats::coef(stats::lm(log(f[fn] - 1) ~ fn))
    ext <- exp(co[1] + ((max(fn) + 1):(max(fn) + 100)) * co[2]) + 1
    if (prod(ext) > 2) ext <- rep(1, 100)
  } else {
    ext <- rep(1, 100)
  }
  unname(ext)
}

# Factor vector for periods 1..9 (to lag 10) plus the tail beyond lag 10, from a
# triangle with nobs observed lags. Observed periods use the chosen window; later
# periods (only when nobs < 10) and the tail come from the log-linear extension
# fitted on all-year volume-weighted factors (the locked tail method).
development_pattern <- function(m, window) {
  nobs <- max(latest_lag(m))
  f_win <- ldf_vector(m, window)[seq_len(nobs - 1)]
  f_all <- ldf_vector(m, "all")[seq_len(nobs - 1)]
  ext <- loglinear_extension(f_all)
  # period nobs+1.. are indices 1.. of ext; ext[1] is the factor for period nobs.
  f_full <- c(f_win, ext[seq_len(10 - nobs)])
  tail <- prod(ext[(10 - nobs + 1):length(ext)])
  list(f = f_full, tail = tail)
}

# Automatic method by paid percent developed z (SPEC §7.1 thresholds).
auto_method <- function(z, a) {
  th <- a$selection_thresholds
  ifelse(z >= th$cl_min_z, "CL", ifelse(z >= th$bk_min_z, "BK", "BF"))
}

method_ultimate <- function(mt, method) {
  col <- c(CL = "ult_cl_paid", BK = "ult_bk", BF = "ult_bf_paid", CL_INC = "ult_cl_inc",
           BF_INC = "ult_bf_inc", BK_INC = "ult_bk_inc", CC = "ult_cc")[method]
  if (any(is.na(col))) stop("unknown method: ", paste(method[is.na(col)], collapse = ", "))
  vapply(seq_along(method), function(i) mt[[col[i]]][i], numeric(1))
}

# The locked rules at any valuation. `methods` overrides the automatic choice
# (named by AY); `pure_cl = TRUE` gives the pure paid chain ladder benchmark.
auto_reserve <- function(x, code, a = read_config("assumptions_2007"), window = a$ldf$window,
                         methods = NULL, pure_cl = FALSE, thresholds = NULL, elr_shift = 0,
                         tail_multiplier = 1, incurred_basis = FALSE) {
  assert_asat(x)
  if (!is.null(thresholds)) a$selection_thresholds <- thresholds
  tr <- insurer_triangles(x, code)
  pp <- development_pattern(tr$paid, window)
  pp$tail <- 1 + (pp$tail - 1) * tail_multiplier
  pi <- development_pattern(tr$case_inc, window)
  tail_inc <- if (max(latest_lag(tr$paid)) == 10) incurred_tail(tr, pp$tail) else pi$tail
  mt <- methods_table(tr$paid, tr$case_inc, tr$premium, pp$f, pi$f, pp$tail, tail_inc, elr_shift)
  mt$method <- if (pure_cl) "CL" else auto_method(mt$pct_dev, a)
  if (incurred_basis) mt$method <- paste0(auto_method(mt$pct_dev_inc, a), "_INC")
  if (!is.null(methods) && !incurred_basis) {
    hit <- match(names(methods), mt$ay)
    mt$method[hit] <- unname(methods)
  }
  mt$ult_sel <- method_ultimate(mt, mt$method)
  mt$unpaid <- mt$ult_sel - mt$paid
  attr(mt, "pattern") <- pp
  attr(mt, "valuation") <- valuation_of(x)
  mt
}

# Allocation of unpaid to future development years (CONTRACT §3): returns a matrix
# AY x (lag 1..10, "tail") of predicted incremental payments.
allocate_unpaid <- function(mt) {
  pp <- attr(mt, "pattern")
  n <- length(pp$f) + 1                   # number of development years (10 for real data)
  g <- 1 / cdf_from(pp$f, pp$tail)        # g[j], j = 1..n, tail included
  out <- matrix(0, nrow(mt), n + 1, dimnames = list(ay = mt$ay, dev = c(1:n, "tail")))
  for (i in seq_len(nrow(mt))) {
    k <- mt$lag[i]
    denom <- 1 - g[k]
    if (denom <= 0) next
    if (k < n) out[i, (k + 1):n] <- mt$unpaid[i] * (g[(k + 1):n] - g[k:(n - 1)]) / denom
    out[i, n + 1] <- mt$unpaid[i] * (1 - g[n]) / denom
  }
  out
}

# Predicted payments by calendar year (to lag 10), one row per AY.
predicted_by_cy <- function(mt, cys) {
  alloc <- allocate_unpaid(mt)
  sapply(cys, function(cy) {
    n <- ncol(alloc) - 1
    j <- cy - mt$ay + 1
    ifelse(j >= 1 & j <= n, alloc[cbind(seq_len(nrow(mt)), pmin(pmax(j, 1), n))], 0)
  })
}

# ---- Pseudo-historical validation (valuations 2004-2006, diagonals up to 2007) ----

pseudo_historical <- function(up, code, a) {
  actual_inc <- cum_to_inc(insurer_triangles(up, code)$paid)
  variants <- list(
    rule_latest5 = list(window = "latest5", pure_cl = FALSE),
    rule_all = list(window = "all", pure_cl = FALSE),
    cl_latest5 = list(window = "latest5", pure_cl = TRUE),
    cl_all = list(window = "all", pure_cl = TRUE)
  )
  rows <- list()
  for (v in 2004:2006) {
    xv <- as_at(up, v)
    for (nm in names(variants)) {
      va <- variants[[nm]]
      mt <- auto_reserve(xv, code, a, window = va$window, pure_cl = va$pure_cl)
      cys <- (v + 1):VALUATION_YEAR
      pred <- predicted_by_cy(mt, cys)
      for (c in seq_along(cys)) {
        j <- cys[c] - mt$ay + 1
        act <- sum(actual_inc[cbind(match(mt$ay, rownames(actual_inc)), j)])
        rows[[length(rows) + 1]] <- data.frame(valuation = v, variant = nm, cy = cys[c],
                                               expected = sum(pred[, c]), actual = act)
      }
    }
  }
  out <- do.call(rbind, rows)
  out$ae <- out$actual / out$expected
  out
}

# By-AY detail of the pseudo-historical runs, for the young-AY question.
pseudo_by_ay <- function(up, code, a) {
  actual_inc <- cum_to_inc(insurer_triangles(up, code)$paid)
  rows <- list()
  for (v in 2004:2006) {
    xv <- as_at(up, v)
    for (pc in c(FALSE, TRUE)) {
      mt <- auto_reserve(xv, code, a, pure_cl = pc)
      cys <- (v + 1):VALUATION_YEAR
      pred <- predicted_by_cy(mt, cys)
      act <- sapply(cys, function(cy) {
        j <- cy - mt$ay + 1
        actual_inc[cbind(match(mt$ay, rownames(actual_inc)), j)]
      })
      rows[[length(rows) + 1]] <- data.frame(valuation = v, variant = if (pc) "cl" else "rule",
        ay = mt$ay, pct_dev = mt$pct_dev, method = mt$method,
        expected = rowSums(pred), actual = rowSums(matrix(act, nrow = nrow(mt))))
    }
  }
  out <- do.call(rbind, rows)
  out$ae <- out$actual / out$expected
  out
}

# ---- Main selection at 2007 ----

selection_rationale <- function(mt, a) {
  exc <- a$exceptions
  vapply(seq_len(nrow(mt)), function(i) {
    ay <- as.character(mt$ay[i])
    z <- fmt_pct(mt$pct_dev[i])
    auto <- auto_method(mt$pct_dev[i], a)
    txt <- switch(auto,
      CL = sprintf("z = %s >= 70%%: chain ladder territory.", z),
      BK = sprintf("z = %s in 30-70%%: Benktander blends paid CL and BF by maturity.", z),
      BF = sprintf("z = %s < 30%%: BF with leave-one-out Cape Cod prior.", z))
    if (!is.null(exc[[ay]])) txt <- paste(txt, "Exception:", exc[[ay]]$reason)
    gap <- abs(mt$ult_cl_paid[i] - mt$ult_cl_inc[i]) / mt$ult_cl_paid[i]
    if (gap > a$paid_vs_incurred_flag) txt <- paste(txt, sprintf("Paid vs incurred CL gap %s > 10%%.", fmt_pct(gap)))
    if (isTRUE(mt$peer_flag[i])) txt <- paste(txt, sprintf("Prior %s vs peer median %s (> 10 points): peer panel is mostly small insurers with different mix; prior kept.", fmt_pct(mt$elr_loo[i]), fmt_pct(mt$peer_median_ulr[i])))
    txt
  }, character(1))
}

run_select <- function() {
  up <- as_at(load_upper(), VALUATION_YEAR)
  a <- read_config("assumptions_2007")
  code <- a$main_insurer
  ph <- pseudo_historical(up, code, a)
  utils::write.csv(ph, path_in("outputs", "m4_pseudo_historical.csv"), row.names = FALSE)
  utils::write.csv(pseudo_by_ay(up, code, a), path_in("outputs", "m4_pseudo_historical_by_ay.csv"), row.names = FALSE)
  exc <- vapply(a$exceptions, function(e) e$method, character(1))
  mt <- auto_reserve(up, code, a, methods = exc)
  panels <- utils::read.csv(path_in("outputs", "panels.csv"))
  peer <- peer_elr(up, panels$grcode[panels$panel_backtest], a$ldf$window)
  mt$peer_median_ulr <- peer$peer_median_ulr[match(mt$ay, peer$ay)]
  mt$peer_flag <- abs(mt$elr_loo - mt$peer_median_ulr) > 0.10
  sel <- data.frame(
    ay = mt$ay, prem_net = mt$prem_net, paid = mt$paid, case_inc = mt$case_inc, pct_dev = mt$pct_dev,
    ult_cl_paid = mt$ult_cl_paid, ult_cl_inc = mt$ult_cl_inc, ult_bf_paid = mt$ult_bf_paid,
    ult_bf_inc = mt$ult_bf_inc, ult_cc = mt$ult_cc, ult_bk = mt$ult_bk,
    method = mt$method, ult_sel = NA_real_, ulr_sel = NA_real_,
    case_os = mt$case_inc - mt$paid, ibnr = round(mt$ult_sel - mt$case_inc, 1), unpaid = NA_real_,
    rationale = selection_rationale(mt, a)
  )
  # Money to one decimal (SPEC §4.3); the identity ult_sel = paid + case_os + ibnr holds by construction.
  money <- c("ult_cl_paid", "ult_cl_inc", "ult_bf_paid", "ult_bf_inc", "ult_cc", "ult_bk")
  sel[money] <- lapply(sel[money], round, 1)
  sel$ult_sel <- sel$paid + sel$case_os + sel$ibnr
  sel$unpaid <- sel$case_os + sel$ibnr
  sel$ulr_sel <- round(sel$ult_sel / sel$prem_net, 4)
  utils::write.csv(sel, path_in("outputs", "selection_2007.csv"), row.names = FALSE)
  # Pattern used for allocation (locked with the selection): factors and tail.
  pp <- attr(mt, "pattern")
  utils::write.csv(data.frame(period = c(paste0(1:9, "-", 2:10), "tail"), factor = c(pp$f, pp$tail),
                              cdf_to_ult = c(cdf_from(pp$f, pp$tail)[1:9], pp$tail)),
                   path_in("outputs", "pattern_2007.csv"), row.names = FALSE)
  tri <- insurer_triangles(up, code)
  booked_unpaid <- sum(latest(tri$inc_booked) - latest(tri$paid))
  meth_tot <- sapply(c("ult_cl_paid", "ult_cl_inc", "ult_bf_paid", "ult_bf_inc", "ult_cc", "ult_bk"),
                     function(c) sum(sel[[c]] - sel$paid))
  auto <- auto_reserve(up, code, a)
  cmp <- data.frame(
    item = c("selected_unpaid", "selected_case_os", "selected_ibnr", "booked_unpaid",
             "selected_minus_booked", "selected_over_booked_pct", "automatic_rule_unpaid",
             paste0("unpaid_", names(meth_tot)), "method_range_min", "method_range_max", "selected_ultimate", "tail"),
    value = c(sum(sel$unpaid), sum(sel$case_os), sum(sel$ibnr), booked_unpaid,
              sum(sel$unpaid) - booked_unpaid, sum(sel$unpaid) / booked_unpaid - 1, sum(auto$unpaid),
              unname(meth_tot), min(meth_tot), max(meth_tot), sum(sel$ult_sel), pp$tail)
  )
  utils::write.csv(cmp, path_in("outputs", "m4_prelock_comparison.csv"), row.names = FALSE)
  bd <- booked_development(up, code)
  utils::write.csv(bd, path_in("outputs", "m4_booked_development.csv"), row.names = FALSE)
  plot_selection(sel, bd, code)
  invisible(sel)
}

# Insurer's own booked ultimate: first booking (lag 1) vs latest booking, by AY (pre-lock evidence).
booked_development <- function(x, code) {
  tr <- insurer_triangles(x, code)
  data.frame(ay = as.integer(rownames(tr$inc_booked)), booked_lag1 = tr$inc_booked[, 1],
             booked_latest = unname(latest(tr$inc_booked)), lag = unname(latest_lag(tr$inc_booked)),
             change = unname(latest(tr$inc_booked)) / tr$inc_booked[, 1] - 1)
}

plot_selection <- function(sel, booked, code) {
  f <- path_in("outputs", "figures", "m4_selection_vs_methods.png")
  grDevices::png(f, width = 1200, height = 650, res = 130)
  ulr <- function(col) sel[[col]] / sel$prem_net
  graphics::plot(sel$ay, ulr("ult_sel"), type = "n", ylim = c(0.45, 0.85), xlab = "Accident year",
                 ylab = "Ultimate loss ratio (net)", main = paste0("Selected vs method ultimates and insurer's booked - NAIC ", code,
                 "\nUS commercial auto, net, NAIC Schedule P, as at 2007-12-31"), cex.main = 0.9)
  cols <- c(ult_cl_paid = "grey55", ult_cl_inc = "grey30", ult_bf_paid = "#4C9BE8", ult_bk = "#2E7D32")
  for (c in names(cols)) graphics::lines(sel$ay, ulr(c), col = cols[c], lty = 2)
  graphics::lines(sel$ay, booked$booked_latest / sel$prem_net, col = "#C62828", lwd = 2)
  graphics::lines(sel$ay, ulr("ult_sel"), col = "black", lwd = 3)
  graphics::legend("topright", c("Selected", "Insurer booked", "Paid CL", "Incurred CL", "Paid BF", "Benktander"),
                   col = c("black", "#C62828", cols), lwd = c(3, 2, 1, 1, 1, 1), lty = c(1, 1, 2, 2, 2, 2), cex = 0.75, bty = "n")
  grDevices::dev.off()
  f
}
