# M3 acceptance tests (SPEC §9 M3, §11 toy fixture).

toy <- function() {
  d <- utils::read.csv(path_in("tests", "fixtures", "toy_triangle.csv"))
  m <- as.matrix(d[, c("lag12", "lag24", "lag36", "lag48")])
  dimnames(m) <- list(ay = d$ay, lag = 1:4)
  list(m = m, prem = stats::setNames(d$premium, d$ay), case = d$case_inc_2007)
}

test_that("toy fixture reproduces SPEC §11 (tolerance 0.01)", {
  t <- toy()
  f <- ldf_vector(t$m, "all")
  expect_equal(f, c(2.19403, 1.44086, 1.25), tolerance = 1e-5)
  cl <- chain_ladder(t$m, f, 1)
  expect_equal(cl$cdf[4], 3.95161, tolerance = 1e-5)
  expect_equal(round(cl$z, 4), c(1, 0.8, 0.5552, 0.2531), tolerance = 1e-4)
  expect_equal(cl$ult_cl, c(40, 43.75, 48.63, 55.32), tolerance = 0.01 / 55)
  expect_equal(sum(cl$ult_cl), 187.70, tolerance = 0.01 / 187)
  expect_equal(cape_cod_elr(cl$latest, t$prem, cl$z), 0.6939, tolerance = 1e-4 / 0.69)
  expect_equal(unname(loo_elr(cl$latest, t$prem, cl$z)), c(0.7091, 0.6984, 0.6877, 0.6848), tolerance = 1e-4 / 0.69)
  mt <- methods_table(t$m, t$m, t$prem, f, f)
  expect_equal(mt$ult_bf_paid, c(40, 43.94, 47.80, 50.83), tolerance = 0.01 / 50)
  expect_equal(mt$ult_cc, c(40, 43.88, 47.99, 51.32), tolerance = 0.01 / 50)
  expect_equal(mt$ult_bk, c(40, 43.79, 48.26, 51.96), tolerance = 0.01 / 50)
  expect_equal(sum(mt$ult_bf_paid), 182.57, tolerance = 0.01 / 182)
  expect_equal(sum(mt$ult_cc), 183.18, tolerance = 0.01 / 183)
  expect_equal(sum(mt$ult_bk), 184.01, tolerance = 0.01 / 184)
})

test_that("own chain ladder equals MackChainLadder ultimates (1e-8 relative)", {
  up <- as_at(load_upper(), 2007)
  tr <- insurer_triangles(up, main_insurer())
  own <- chain_ladder(tr$paid, ldf_vector(tr$paid, "all"), 1)$ult_cl
  mack <- ChainLadder::MackChainLadder(ChainLadder::as.triangle(tr$paid), est.sigma = "Mack")
  expect_equal(own, unname(mack$FullTriangle[, 10]), tolerance = 1e-8)
})

test_that("BF with ELR = U_CL / P equals chain ladder", {
  up <- as_at(load_upper(), 2007)
  tr <- insurer_triangles(up, main_insurer())
  cl <- chain_ladder(tr$paid, ldf_vector(tr$paid, "latest5"), 1.0023)
  bf <- bf_ultimate(cl$latest, tr$premium, cl$z, cl$ult_cl / tr$premium)
  expect_equal(unname(bf), cl$ult_cl, tolerance = 1e-12)
})

test_that("methods_2007.csv has every method and ultimate >= paid", {
  mt <- utils::read.csv(path_in("outputs", "methods_2007.csv"))
  expect_equal(nrow(mt), 10)
  for (col in c("ult_cl_paid", "ult_cl_inc", "ult_bf_paid", "ult_bf_inc", "ult_cc", "ult_bk")) {
    expect_true(all(mt[[col]] >= mt$paid - 1e-9), info = col)
  }
})

test_that("Excel check agrees with R within 0.01%", {
  chk <- utils::read.csv(path_in("outputs", "m3_excel_check.csv"))
  expect_true(all(abs(chk$rel_diff) < 1e-4), info = paste(chk$item[abs(chk$rel_diff) >= 1e-4], collapse = ", "))
})
