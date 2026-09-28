# M1: triangles for one insurer from an as_at() object. Rows = AY, columns = lag 1..10.
# Cells with cy > valuation are NA.

tri_cum <- function(x, field = c("paid", "case_inc", "inc_booked", "bulk")) {
  assert_asat(x)
  field <- match.arg(field)
  if (length(unique(x$grcode)) != 1) stop("tri_cum(): pass one insurer (see for_insurer())")
  ays <- sort(unique(x$ay))
  m <- matrix(NA_real_, nrow = length(ays), ncol = 10, dimnames = list(ay = ays, lag = 1:10))
  m[cbind(match(x$ay, ays), x$lag)] <- x[[field]]
  m
}

# Incremental from cumulative; lag 1 incremental = lag 1 cumulative.
cum_to_inc <- function(m) {
  out <- m
  out[, -1] <- m[, -1, drop = FALSE] - m[, -ncol(m), drop = FALSE]
  out
}

inc_to_cum <- function(m) t(apply(m, 1, cumsum))

# Net earned premium by AY (constant across lags; taken from the lag 1 row).
premium <- function(x, field = "prem_net") {
  assert_asat(x)
  p <- x[x$lag == 1, c("ay", field)]
  stats::setNames(p[[field]][order(p$ay)], sort(p$ay))
}

# Latest diagonal: value at the latest visible lag for each AY.
latest <- function(m) {
  k <- apply(m, 1, function(r) max(which(!is.na(r))))
  stats::setNames(m[cbind(seq_len(nrow(m)), k)], rownames(m))
}

latest_lag <- function(m) {
  stats::setNames(apply(m, 1, function(r) max(which(!is.na(r)))), rownames(m))
}

# The four M1 objects for one insurer.
insurer_triangles <- function(x, code) {
  xi <- for_insurer(x, code)
  paid <- tri_cum(xi, "paid")
  list(
    grcode = code, valuation = valuation_of(xi),
    paid = paid, case_inc = tri_cum(xi, "case_inc"), inc_booked = tri_cum(xi, "inc_booked"),
    inc_paid = cum_to_inc(paid), premium = premium(xi)
  )
}
