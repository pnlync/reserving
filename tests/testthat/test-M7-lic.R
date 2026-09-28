# M7 acceptance tests (SPEC §9 M7, §11 toy discounting).

test_that("toy discounting: flat 4% annual, mid-year = 63.99; x 1.05 ULAE = 67.19", {
  pay <- c(35.35, 21.63, 9.86)
  t <- c(0.5, 1.5, 2.5)
  pv <- sum(pay * 1.04^(-t))
  expect_equal(pv, 63.99, tolerance = 0.01 / 64)
  expect_equal(pv * 1.05, 67.19, tolerance = 0.01 / 67)
  # the same through path_pv with an equivalent continuously compounded flat curve
  expect_equal(path_pv(matrix(pay, 1), 0.05, exp(-log(1.04) * t)), 67.19, tolerance = 0.01 / 67)
})

test_that("Svensson yields reproduce the Fed's published SVENY zero yields", {
  p <- gsw_parameters()
  expect_equal(format(p$date), "2007-12-31")
  expect_equal(unname(svensson_yield(1:30, p)), unname(p$sveny), tolerance = 1e-4)
})

test_that("zero curve gives discounted BE = undiscounted BE; RA increases with the confidence level", {
  sims <- readRDS(path_in("outputs", "sims_main.rds"))
  p <- gsw_parameters()
  z <- lic_table(sims$pay_cy, p, 0.05, zero_curve = TRUE)
  expect_equal(z$be, z$undiscounted_be, tolerance = 1e-12)
  base <- lic_table(sims$pay_cy, p, 0.05)
  expect_true(all(diff(base$ra) > 0))
  lic <- utils::read.csv(path_in("outputs", "lic_2007.csv"))
  v <- function(i) lic$value[lic$item == i]
  expect_equal(v("lic"), v("discounted_be") + v("ra_75"))
  expect_equal(v("discounted_be"), v("undiscounted_be") + v("discount_effect"))
})
