# M7: illustrative net IFRS 17 liability for incurred claims (SPEC §9 M7, §8 curve).

# GSW (Gurkaynak-Sack-Wright) Svensson parameters for the valuation date, or the last
# earlier trading day.
gsw_parameters <- function(date = "2007-12-31") {
  f <- path_in("data", "market", "feds200628.csv")
  lines <- readLines(f, warn = FALSE)
  hdr <- grep("^Date,", lines)[1]
  d <- utils::read.csv(f, skip = hdr - 1, stringsAsFactors = FALSE)
  d$Date <- as.Date(d$Date)
  d <- d[!is.na(d$BETA0) & d$Date <= as.Date(date), ]
  row <- d[which.max(d$Date), ]
  list(date = row$Date, b0 = row$BETA0, b1 = row$BETA1, b2 = row$BETA2, b3 = row$BETA3,
       t1 = row$TAU1, t2 = row$TAU2, sveny = unlist(row[paste0("SVENY", sprintf("%02d", 1:30))]))
}

# Svensson zero yield in percent, continuously compounded; 30-year yield held flat beyond 30.
svensson_yield <- function(n, p) {
  n <- pmin(n, 30)
  a1 <- (1 - exp(-n / p$t1)) / (n / p$t1)
  a2 <- (1 - exp(-n / p$t2)) / (n / p$t2)
  p$b0 + p$b1 * a1 + p$b2 * (a1 - exp(-n / p$t1)) + p$b3 * (a2 - exp(-n / p$t2))
}

discount_factor <- function(t, p, shift_bp = 0) exp(-(svensson_yield(t, p) + shift_bp / 100) / 100 * t)

# Present value of each simulated path: cash flows by calendar year paid mid-year.
path_pv <- function(pay_cy, ulae, df) as.vector((pay_cy * (1 + ulae)) %*% df)

lic_table <- function(pay_cy, p, ulae = 0.05, shift_bp = 0, levels = c(0.65, 0.75, 0.85, 0.90), zero_curve = FALSE) {
  cys <- as.integer(colnames(pay_cy))
  t <- cys - VALUATION_YEAR - 0.5
  df <- if (zero_curve) rep(1, length(t)) else discount_factor(t, p, shift_bp)
  x <- path_pv(pay_cy, ulae, df)
  undisc <- mean(rowSums(pay_cy) * (1 + ulae))
  be <- mean(x)
  ra <- vapply(levels, function(q) unname(stats::quantile(x, q)) - be, numeric(1))
  list(undiscounted_be = undisc, discount_effect = be - undisc, be = be,
       ra = stats::setNames(ra, paste0("ra_", levels * 100)), pv = x)
}

run_lic <- function() {
  a <- read_config("assumptions_2007")
  sims <- readRDS(path_in("outputs", "sims_main.rds"))
  p <- gsw_parameters()
  base <- lic_table(sims$pay_cy, p, a$ulae$base)
  ra75 <- base$ra["ra_75"]
  rows <- data.frame(
    item = c("undiscounted_be", "discount_effect", "discounted_be", "ra_75", "lic", "ra_over_be",
             "ra_65", "ra_85", "ra_90", "lic_ulae_3pct", "lic_ulae_7pct", "lic_curve_plus50bp",
             "be_ulae_3pct", "be_ulae_7pct", "be_curve_plus50bp", "curve_date", "yield_1y", "yield_5y", "yield_10y"),
    value = NA_real_
  )
  s3 <- lic_table(sims$pay_cy, p, a$ulae$sensitivity[1])
  s7 <- lic_table(sims$pay_cy, p, a$ulae$sensitivity[2])
  sb <- lic_table(sims$pay_cy, p, a$ulae$base, shift_bp = a$discount_curve$sensitivity_bp)
  rows$value <- c(base$undiscounted_be, base$discount_effect, base$be, ra75, base$be + ra75, ra75 / base$be,
                  base$ra["ra_65"], base$ra["ra_85"], base$ra["ra_90"],
                  s3$be + s3$ra["ra_75"], s7$be + s7$ra["ra_75"], sb$be + sb$ra["ra_75"],
                  s3$be, s7$be, sb$be, as.numeric(format(p$date, "%Y%m%d")),
                  svensson_yield(1, p), svensson_yield(5, p), svensson_yield(10, p))
  utils::write.csv(rows, path_in("outputs", "lic_2007.csv"), row.names = FALSE)
  saveRDS(list(pv = base$pv, p = p), path_in("outputs", "lic_pv_main.rds"))
  invisible(rows)
}
