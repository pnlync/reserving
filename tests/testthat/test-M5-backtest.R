# M5 acceptance tests (SPEC §9 M5). Mechanics are checked on a synthetic chain-ladder lower
# triangle (no real holdout values); the scoring-from-tag tests run once the tag exists.

test_that("on a chain-ladder world, pure CL (all-year window) has zero Test A error", {
  code <- main_insurer()
  full <- synthetic_full(code)
  a <- read_config("assumptions_2007")
  x0 <- as_at(cohort(full, 2007), 2007)
  cl <- auto_reserve(x0, code, a, window = "all", pure_cl = TRUE)
  sc <- score_tests_abc(full, code, cl, cl)
  expect_equal(sc$totals$err_paid120_cl, 0, tolerance = 1e-10)
  expect_equal(sc$totals$ae_next_total, 1, tolerance = 1e-10)
})

test_that("allocation shares sum to 1 per AY for the locked selection", {
  sel <- locked_csv("outputs/selection_2007.csv")
  mt <- locked_table(sel, locked_csv("outputs/pattern_2007.csv"))
  alloc <- allocate_unpaid(mt)
  expect_equal(unname(rowSums(alloc)), sel$unpaid, tolerance = 1e-9)
})

test_that("Test D at v = 2007 reproduces the automatic version of the locked selection", {
  code <- main_insurer()
  full <- synthetic_full(code)
  a <- read_config("assumptions_2007")
  exc <- vapply(a$exceptions, function(e) e$method, character(1))
  sel <- locked_csv("outputs/selection_2007.csv")
  d <- rolling_rereserve(full, code, a, sum(sel$unpaid), last = 2009, methods = exc)
  expect_equal(d$reserve_auto[d$valuation == 2007], sum(sel$unpaid), tolerance = 1e-6)
  expect_equal(nrow(d), 3)
})

test_that("panel back-test and calibration run end to end on synthetic data", {
  panels <- utils::read.csv(path_in("outputs", "panels.csv"))
  codes <- utils::head(panels$grcode[panels$panel_stochastic_provisional], 3)
  full <- synthetic_full(codes)
  a <- read_config("assumptions_2007")
  a$ldf$window <- "all"
  pb <- panel_backtest_run(full, codes, a)
  expect_equal(nrow(pb), 3)
  expect_true(all(abs(pb$err_cl[pb$scored]) < 1e-8))
  spec <- locked_yaml("config/stochastic_spec.yaml")
  spec$replicates$panel_insurer <- 200
  cal <- calibrate_panel(full, codes, a, spec, read_config("seeds"))
  expect_true(all(cal$p_custom >= 0 & cal$p_custom <= 1, na.rm = TRUE))
  st <- calibration_stats(c(0.1, 0.5, 0.8, 0.9))
  expect_equal(st$cov_q75, 0.5)
})

test_that("scoring reads the selection from the tag", {
  skip_if_not(lock_tag_exists(project_root()), "tag not created yet")
  expect_equal(locked_selection(), locked_csv("outputs/selection_2007.csv"))
})

test_that("one-year panel back-test runs end to end on synthetic data", {
  panels <- utils::read.csv(path_in("outputs", "panels.csv"))
  codes <- utils::head(panels$grcode[panels$panel_stochastic_provisional], 2)
  full <- synthetic_full(codes)
  a <- read_config("assumptions_2007")
  spec <- locked_yaml("config/stochastic_spec.yaml")
  po <- panel_one_year(full, codes, a, spec, read_config("seeds"), R = 100)
  expect_equal(nrow(po), 2)
  expect_true(all(is.finite(po$p)))
})
