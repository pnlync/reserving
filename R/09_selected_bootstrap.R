# M6: custom ODP bootstrap of the *selected* reserve (SPEC §7.2, §9 M6).
# Each replicate: resample residuals -> pseudo triangle -> LDFs/tail with the locked
# rules -> leave-one-out Cape Cod prior x prior noise -> locked method per AY ->
# allocate unpaid to future years -> gamma process noise.

# ODP fit by chain ladder back-fitting (same fitted values as the ODP GLM).
odp_fit <- function(C, exclude_zero = TRUE) {
  n <- nrow(C)
  f <- ldf_vector(C, "all")
  k <- latest_lag(C)
  fitted_cum <- matrix(NA_real_, n, ncol(C))
  for (i in seq_len(n)) {
    fitted_cum[i, k[i]] <- C[i, k[i]]
    if (k[i] > 1) for (j in (k[i] - 1):1) fitted_cum[i, j] <- fitted_cum[i, j + 1] / f[j]
  }
  m <- cum_to_inc(fitted_cum)
  X <- cum_to_inc(C)
  upper <- !is.na(C)
  r <- (X - m) / sqrt(abs(m))
  r[!upper | m == 0] <- NA
  n_cells <- sum(upper)
  p <- 2 * n - 1
  phi <- sum(r^2, na.rm = TRUE) / (n_cells - p)
  keep <- upper & !is.na(r)
  if (exclude_zero) keep <- keep & abs(r) > 1e-10     # drop zero-residual corners (SPEC §7.2)
  pool <- r[keep] * sqrt(n_cells / (n_cells - p))
  list(m = m, X = X, upper = upper, r = r, pool = pool, phi = phi, n_cells = n_cells, p = p,
       n_excluded = sum(upper) - sum(keep), dimnames = dimnames(C))
}

# Reserve for one (pseudo) paid triangle with the locked rules. `method` gives the
# method per AY ("CL", "BK", "BF"; NULL = automatic thresholds on z); `load` adds a fixed
# amount to the unpaid of AYs whose locked method is incurred CL (see SPEC §14 v1.3).
lean_reserve <- function(C, prem, window, th, method = NULL, load = NULL, elr_mult = 1) {
  pp <- development_pattern(C, window)
  cdf <- cdf_from(pp$f, pp$tail)
  k <- latest_lag(C)
  cl <- latest(C)
  z <- 1 / cdf[k]
  u_cl <- cl * cdf[k]
  elr <- loo_elr(cl, prem, z) * elr_mult
  u_bf <- cl + elr * prem * (1 - z)
  u_bk <- cl + (1 - z) * u_bf
  if (is.null(method)) method <- ifelse(z >= th$cl_min_z, "CL", ifelse(z >= th$bk_min_z, "BK", "BF"))
  u <- ifelse(method == "BF", u_bf, ifelse(method == "BK", u_bk, u_cl))
  unpaid <- u - cl
  if (!is.null(load)) unpaid <- unpaid + load
  list(unpaid = unname(unpaid), k = unname(k), g = 1 / cdf, method = method, z = unname(z))
}

# Expected future increments: AY x development year 1..11 (11 = tail, paid one year after lag 10).
future_means <- function(res) {
  n <- length(res$unpaid)
  g <- res$g
  mu <- matrix(0, n, 11)
  for (i in seq_len(n)) {
    k <- res$k[i]
    denom <- 1 - g[k]
    if (denom <= 0) next
    if (k < 10) mu[i, (k + 1):10] <- res$unpaid[i] * (g[(k + 1):10] - g[k:9]) / denom
    mu[i, 11] <- res$unpaid[i] * (1 - g[10]) / denom
  }
  mu
}

process_noise <- function(mu, phi) {
  out <- mu
  pos <- mu > 0
  out[pos] <- stats::rgamma(sum(pos), shape = mu[pos] / phi, scale = phi)
  out
}

# Main engine. Returns simulated future payments by calendar year (2008..2017, with the
# tail in development year 11), payments to lag 10, and the simulated 2008 diagonal by AY.
selected_bootstrap <- function(C, prem, R, seed, window = "latest5", th = NULL, method = NULL, load = NULL,
                               prior_cv = 0.10, tail = TRUE, process = TRUE, exclude_zero = TRUE) {
  set.seed(seed)
  fit <- odp_fit(C, exclude_zero)
  n <- nrow(C)
  ays <- as.integer(rownames(C))
  first_cy <- max(ays) + 1
  cys <- first_cy:(first_cy + 9)
  sdlog <- sqrt(log(1 + prior_cv^2))
  pay_cy <- matrix(0, R, 10, dimnames = list(NULL, cys))
  to_lag10 <- numeric(R)
  diag_next <- matrix(0, R, n, dimnames = list(NULL, ays))
  diag_paid_part <- matrix(0, R, n, dimnames = list(NULL, ays))   # next diagonal excluding the load
  reserve <- numeric(R)
  n_nonpos <- 0L
  upper <- fit$upper
  for (s in seq_len(R)) {
    Xs <- fit$m
    Xs[upper] <- fit$m[upper] + sample(fit$pool, sum(upper), replace = TRUE) * sqrt(abs(fit$m[upper]))
    Xs[!upper] <- NA
    Cs <- inc_to_cum(Xs)
    Cs[!upper] <- NA
    dimnames(Cs) <- fit$dimnames
    elr_mult <- if (prior_cv > 0) stats::rlnorm(1, -sdlog^2 / 2, sdlog) else 1
    res <- if (tail) lean_reserve(Cs, prem, window, th, method, load, elr_mult) else
      lean_reserve_notail(Cs, prem, window, th, method, load, elr_mult)
    mu <- future_means(res)
    mu_load <- if (is.null(load)) 0 * mu else future_means(list(unpaid = load, k = res$k, g = res$g))
    n_nonpos <- n_nonpos + sum(mu < 0)
    y <- if (process) process_noise(mu, fit$phi) else mu
    reserve[s] <- sum(y)
    to_lag10[s] <- sum(y[, 1:10])
    for (i in seq_len(n)) {
      js <- (res$k[i] + 1):11
      js <- js[js <= 11]
      if (res$k[i] >= 11) next
      cy <- ays[i] + js - 1
      pay_cy[s, match(cy, cys)] <- pay_cy[s, match(cy, cys)] + y[i, js]
      j1 <- res$k[i] + 1
      diag_next[s, i] <- y[i, j1]
      diag_paid_part[s, i] <- if (mu[i, j1] > 0) y[i, j1] * (1 - mu_load[i, j1] / mu[i, j1]) else y[i, j1] - mu_load[i, j1]
    }
  }
  list(pay_cy = pay_cy, to_lag10 = to_lag10, diag_next = diag_next, diag_paid_part = diag_paid_part, reserve = reserve,
       phi = fit$phi, n_pool = length(fit$pool), n_excluded = fit$n_excluded, n_nonpositive_means = n_nonpos,
       seed = seed, R = R)
}

# Variant without a tail (for the package comparison and the Merz-Wuthrich check).
lean_reserve_notail <- function(C, prem, window, th, method = NULL, load = NULL, elr_mult = 1) {
  f <- ldf_vector(C, window)
  cdf <- cdf_from(f, 1)
  k <- latest_lag(C)
  cl <- latest(C)
  z <- 1 / cdf[k]
  u_cl <- cl * cdf[k]
  elr <- loo_elr(cl, prem, z) * elr_mult
  u_bf <- cl + elr * prem * (1 - z)
  u_bk <- cl + (1 - z) * u_bf
  if (is.null(method)) method <- ifelse(z >= th$cl_min_z, "CL", ifelse(z >= th$bk_min_z, "BK", "BF"))
  u <- ifelse(method == "BF", u_bf, ifelse(method == "BK", u_bk, u_cl))
  unpaid <- u - cl
  if (!is.null(load)) unpaid <- unpaid + load
  list(unpaid = unname(unpaid), k = unname(k), g = 1 / cdf, method = method, z = unname(z))
}

# Locked method table -> paid-only method codes and incurred loads (SPEC §14 v1.3):
# an AY locked on incurred CL is simulated as paid CL plus a fixed load equal to
# (locked unpaid - locked paid-CL unpaid), run off with the paid pattern. The load is kept
# out of the paid triangle when re-reserving one year on, so it is not projected twice.
locked_method_table <- function(sel = locked_csv("outputs/selection_2007.csv")) {
  paid_method <- ifelse(sel$method %in% c("CL_INC", "CL"), "CL", sel$method)
  load <- ifelse(sel$method == "CL_INC", sel$unpaid - (sel$ult_cl_paid - sel$paid), 0)
  list(method = paid_method, load = load, ay = sel$ay)
}

run_main_bootstrap <- function() {
  up <- as_at(load_upper(), VALUATION_YEAR)
  a <- read_config("assumptions_2007")
  spec <- locked_yaml("config/stochastic_spec.yaml")
  seeds <- read_config("seeds")
  tr <- insurer_triangles(up, a$main_insurer)
  lm_tab <- locked_method_table()
  sims <- selected_bootstrap(tr$paid, tr$premium, R = spec$replicates$main, seed = seeds$main_bootstrap,
                             window = a$ldf$window, th = a$selection_thresholds,
                             method = lm_tab$method, load = lm_tab$load, prior_cv = spec$prior_uncertainty$cv)
  sims$spec_hash <- spec_hash()
  sims$code <- a$main_insurer
  saveRDS(sims, path_in("outputs", "sims_main.rds"))
  invisible(sims)
}
