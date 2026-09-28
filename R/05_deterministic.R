# M3: point estimates and tail (SPEC §9 M3). Matrix-level functions work for any
# number of lags (the 4x4 toy fixture and the 10x10 real triangles); the entry point
# deterministic_methods() takes an as_at() object.

# cdf[k] = f_k * ... * f_(n-1) * tail for k = 1..n; cdf[n] = tail.
cdf_from <- function(f, tail = 1) rev(cumprod(rev(c(f, tail))))

# Chain ladder on one triangle: factors, CDFs, z and ultimates by AY.
chain_ladder <- function(m, f, tail = 1) {
  k <- latest_lag(m)
  cdf <- cdf_from(f, tail)
  c_latest <- latest(m)
  data.frame(ay = as.integer(rownames(m)), k = unname(k), latest = unname(c_latest),
             cdf = cdf[k], z = 1 / cdf[k], ult_cl = unname(c_latest) * cdf[k])
}

# Cape Cod ELR and its leave-one-out version (prior for AY i excludes AY i).
cape_cod_elr <- function(c_latest, prem, z) sum(c_latest) / sum(prem * z)
loo_elr <- function(c_latest, prem, z) {
  (sum(c_latest) - c_latest) / (sum(prem * z) - prem * z)
}

bf_ultimate <- function(c_latest, prem, z, elr) c_latest + elr * prem * (1 - z)
benktander_ultimate <- function(c_latest, z, u_bf) c_latest + (1 - z) * u_bf

# All methods for one insurer, given paid/incurred triangles, premium and factors.
methods_table <- function(paid, inc, prem, f_paid, f_inc, tail_paid = 1, tail_inc = 1) {
  cp <- chain_ladder(paid, f_paid, tail_paid)
  ci <- chain_ladder(inc, f_inc, tail_inc)
  elr_loo <- loo_elr(cp$latest, prem, cp$z)
  elr_cc <- cape_cod_elr(cp$latest, prem, cp$z)
  ult_bf_paid <- bf_ultimate(cp$latest, prem, cp$z, elr_loo)
  data.frame(
    ay = cp$ay, prem_net = unname(prem), paid = cp$latest, case_inc = ci$latest, lag = cp$k,
    cdf_paid = cp$cdf, pct_dev = cp$z, cdf_inc = ci$cdf, pct_dev_inc = ci$z,
    ult_cl_paid = cp$ult_cl, ult_cl_inc = ci$ult_cl,
    elr_loo = elr_loo, elr_cc = elr_cc,
    ult_bf_paid = ult_bf_paid,
    ult_bf_inc = bf_ultimate(ci$latest, prem, ci$z, elr_loo),
    ult_cc = bf_ultimate(cp$latest, prem, cp$z, elr_cc),
    ult_bk = benktander_ultimate(cp$latest, cp$z, ult_bf_paid)
  )
}

# ---- Tail (SPEC §9 M3) ----

tail_candidates <- function(tr, panel_codes, up) {
  mack <- ChainLadder::MackChainLadder(ChainLadder::as.triangle(tr$paid), est.sigma = "Mack", tail = TRUE)
  loglin <- utils::tail(mack$f, 1)
  inc_paid_1998 <- unname(tr$case_inc[1, 10] / tr$paid[1, 10])
  peer <- vapply(panel_codes, function(code) {
    xi <- for_insurer(up, code)
    r <- xi[xi$ay == min(xi$ay) & xi$lag == 10, ]
    r$inc_booked / r$paid
  }, numeric(1))
  data.frame(
    candidate = c("loglinear_mack", "case_inc_over_paid_ay1998", "peer_median_booked_over_paid_ay1998"),
    tail = c(loglin, inc_paid_1998, stats::median(peer[is.finite(peer)]))
  )
}

# Incurred tail consistent with the paid tail at the oldest AY:
# paid_1998_10 * tail_paid = case_inc_1998_10 * tail_inc.
incurred_tail <- function(tr, tail_paid) unname(tail_paid * tr$paid[1, 10] / tr$case_inc[1, 10])

# ---- Peer ELR check ----

# Median over panel_backtest of each insurer's paid CL ultimate / premium by AY
# (each insurer's own factors, same window as the main insurer, no tail).
peer_elr <- function(up, codes, window) {
  ulr <- sapply(codes, function(code) {
    tr <- insurer_triangles(up, code)
    cl <- chain_ladder(tr$paid, ldf_vector(tr$paid, window))
    cl$ult_cl / tr$premium
  })
  data.frame(ay = as.integer(rownames(ulr)), peer_median_ulr = apply(ulr, 1, stats::median))
}

# ---- Entry point ----

deterministic_methods <- function(x, code, a = read_config("assumptions_2007")) {
  assert_asat(x)
  tr <- insurer_triangles(x, code)
  window <- a$ldf$window
  tail_paid <- if (is.null(a$tail$value)) 1 else a$tail$value
  methods_table(tr$paid, tr$case_inc, tr$premium,
                ldf_vector(tr$paid, window), ldf_vector(tr$case_inc, window),
                tail_paid, incurred_tail(tr, tail_paid))
}

run_deterministic <- function() {
  up <- as_at(load_upper(), VALUATION_YEAR)
  a <- read_config("assumptions_2007")
  code <- a$main_insurer
  tr <- insurer_triangles(up, code)
  panels <- utils::read.csv(path_in("outputs", "panels.csv"))
  tails <- tail_candidates(tr, panels$grcode[panels$panel_backtest], up)
  utils::write.csv(tails, path_in("outputs", "m3_tail_candidates.csv"), row.names = FALSE)
  mt <- deterministic_methods(up, code, a)
  peer <- peer_elr(up, panels$grcode[panels$panel_backtest], a$ldf$window)
  mt <- merge(mt, peer, by = "ay")
  mt$peer_flag <- abs(mt$elr_loo - mt$peer_median_ulr) > 0.10
  utils::write.csv(mt, path_in("outputs", "methods_2007.csv"), row.names = FALSE)
  invisible(mt)
}

# ---- Excel independent check (SPEC §9 M3): live formulas, recalculated by LibreOffice ----

build_excel_check <- function(tr, a) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "check")
  ws <- "check"
  col <- function(j) openxlsx::int2col(j)
  put <- function(x, r, c) openxlsx::writeData(wb, ws, x, startRow = r, startCol = c, colNames = FALSE)
  fml <- function(f, r, c) openxlsx::writeFormula(wb, ws, f, startRow = r, startCol = c)
  n <- nrow(tr$paid)
  r0 <- 4                                  # first AY row of the triangle
  put(paste0("Reserving check - NAIC ", a$main_insurer, " - paid, USD thousands, as at 2007-12-31 (US commercial auto, net, NAIC Schedule P)"), 1, 1)
  put(t(c("AY", paste0("lag", 1:10), "prem_net")), 3, 1)
  for (i in seq_len(n)) {
    put(as.integer(rownames(tr$paid)[i]), r0 + i - 1, 1)
    vals <- tr$paid[i, ]
    for (j in which(!is.na(vals))) put(vals[[j]], r0 + i - 1, 1 + j)
    put(unname(tr$premium[i]), r0 + i - 1, 12)
  }
  # LDFs (volume-weighted over the latest 5 diagonals), row 15; tail row 16; CDF row 17.
  win <- if (a$ldf$window == "latest5") 5 else Inf
  put("LDF", 15, 1); put("tail", 16, 1); put("CDF", 17, 1)
  for (j in 1:9) {
    last <- r0 + (n - j) - 1                # last AY row with lag j+1 present
    first <- max(r0, last - min(win, n - j) + 1)
    fml(sprintf("SUM(%s%d:%s%d)/SUM(%s%d:%s%d)", col(j + 2), first, col(j + 2), last, col(j + 1), first, col(j + 1), last), 15, j + 1)
  }
  put(a$tail$value, 16, 11)
  fml("K16", 17, 11)
  for (j in 9:1) fml(sprintf("%s15*%s17", col(j + 1), col(j + 2)), 17, j + 1)
  # Methods block from row 20.
  h <- c("AY", "latest", "lag", "CDF", "z", "ult_CL", "prem", "ELR_loo", "ult_BF", "", "ult_CC", "ult_BK")
  put(t(h), 19, 1)
  m0 <- 20
  for (i in seq_len(n)) {
    r <- m0 + i - 1
    k <- n - i + 1
    fml(sprintf("A%d", r0 + i - 1), r, 1)
    fml(sprintf("%s%d", col(k + 1), r0 + i - 1), r, 2)
    put(k, r, 3)
    fml(sprintf("INDEX($B$17:$K$17,1,C%d)", r), r, 4)
    fml(sprintf("1/D%d", r), r, 5)
    fml(sprintf("B%d*D%d", r, r), r, 6)
    fml(sprintf("L%d", r0 + i - 1), r, 7)
    fml(sprintf("(SUM($B$%d:$B$%d)-B%d)/(SUMPRODUCT($G$%d:$G$%d,$E$%d:$E$%d)-G%d*E%d)", m0, m0 + n - 1, r, m0, m0 + n - 1, m0, m0 + n - 1, r, r), r, 8)
    fml(sprintf("B%d+H%d*G%d*(1-E%d)", r, r, r, r), r, 9)
    fml(sprintf("B%d+$B$32*G%d*(1-E%d)", r, r, r), r, 11)
    fml(sprintf("B%d+(1-E%d)*I%d", r, r, r), r, 12)
  }
  tot <- m0 + n
  put("Total", tot, 1)
  for (c in c(2, 6, 9, 11, 12)) fml(sprintf("SUM(%s%d:%s%d)", col(c), m0, col(c), tot - 1), tot, c)
  put("ELR_CapeCod", 32, 1)
  fml(sprintf("SUM(B%d:B%d)/SUMPRODUCT(G%d:G%d,E%d:E%d)", m0, tot - 1, m0, tot - 1, m0, tot - 1), 32, 2)
  f <- path_in("excel", "reserving_check.xlsx")
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  f
}

# Recalculate with LibreOffice headless (CSV export evaluates every formula) and compare with R.
excel_check <- function() {
  up <- as_at(load_upper(), VALUATION_YEAR)
  a <- read_config("assumptions_2007")
  tr <- insurer_triangles(up, a$main_insurer)
  f <- build_excel_check(tr, a)
  out <- tempfile("xl")
  dir.create(out)
  soffice <- Sys.which("soffice")
  if (soffice == "") stop("LibreOffice (soffice) not found")
  system2(soffice, c("--headless", "--convert-to", "csv", "--outdir", out, f), stdout = FALSE, stderr = FALSE)
  x <- utils::read.csv(file.path(out, "reserving_check.csv"), header = FALSE, stringsAsFactors = FALSE)
  num <- function(r, c) as.numeric(x[r, c])
  mt <- deterministic_methods(up, a$main_insurer, a)
  mt <- mt[order(mt$ay), ]
  f_r <- ldf_vector(tr$paid, a$ldf$window)
  rows <- 20:29
  chk <- rbind(
    data.frame(item = paste0("LDF ", 1:9), excel = sapply(2:10, function(c) num(15, c)), r = f_r),
    data.frame(item = paste0("CDF lag ", 1:10), excel = sapply(2:11, function(c) num(17, c)), r = cdf_from(f_r, a$tail$value)),
    data.frame(item = paste0("CL ", mt$ay), excel = num(rows, 6), r = mt$ult_cl_paid),
    data.frame(item = paste0("ELR_loo ", mt$ay), excel = num(rows, 8), r = mt$elr_loo),
    data.frame(item = paste0("BF ", mt$ay), excel = num(rows, 9), r = mt$ult_bf_paid),
    data.frame(item = paste0("CC ", mt$ay), excel = num(rows, 11), r = mt$ult_cc),
    data.frame(item = paste0("BK ", mt$ay), excel = num(rows, 12), r = mt$ult_bk),
    data.frame(item = "ELR Cape Cod", excel = num(32, 2), r = mt$elr_cc[1])
  )
  chk$rel_diff <- chk$excel / chk$r - 1
  utils::write.csv(chk, path_in("outputs", "m3_excel_check.csv"), row.names = FALSE)
  invisible(chk)
}
