# M8 acceptance tests (SPEC §9 M8, §11 golden values).

test_that("MW2008 golden test: total CDR(1) S.E. 81,081 and Mack S.E. 108,401", {
  cdr <- ChainLadder::CDR(suppressMessages(ChainLadder::MackChainLadder(ChainLadder::MW2008, est.sigma = "Mack")))
  expect_equal(round(cdr["Total", "CDR(1)S.E."]), 81081)
  expect_equal(round(cdr["Total", "Mack.S.E."]), 108401)
})

test_that("pure CL, no tail: simulated CDR mean ~ 0 (3 SEs) and SD within 10% of Merz-Wuthrich", {
  up <- as_at(load_upper(), 2007)
  tr <- insurer_triangles(up, main_insurer())
  th <- list(cl_min_z = 0, bk_min_z = 0)
  sims <- selected_bootstrap(tr$paid, tr$premium, R = 10000, seed = 201, window = "all",
                             method = rep("CL", 10), prior_cv = 0, tail = FALSE)
  rr <- simulate_rereserve(tr$paid, tr$premium, sims, "all", th, tail = FALSE)
  r07 <- sum(lean_reserve_notail(tr$paid, tr$premium, "all", th)$unpaid)
  cdr <- r07 - (rr$paid_year + rr$closing)
  expect_lt(abs(mean(cdr)), 3 * stats::sd(cdr) / sqrt(length(cdr)))
  mw <- merz_wuthrich(tr$paid)
  expect_lt(abs(stats::sd(cdr) / mw$cdr_se - 1), 0.10)
})

test_that("one_year_cdr_sims.csv has the agreed fields and is consistent", {
  d <- utils::read.csv(path_in("outputs", "one_year_cdr_sims.csv"))
  expect_equal(names(d), c("sim_id", "paid_2008", "reserve_2008", "cdr", "l_over_be"))
  expect_equal(nrow(d), locked_yaml("config/stochastic_spec.yaml")$replicates$one_year_rereserve)
  sel <- locked_csv("outputs/selection_2007.csv")
  expect_equal(d$cdr, sum(sel$unpaid) - d$paid_2008 - d$reserve_2008, tolerance = 1e-9)
})
