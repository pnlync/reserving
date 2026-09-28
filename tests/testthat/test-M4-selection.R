# M4 acceptance tests (SPEC §9 M4, §11 toy selection and allocation).

test_that("toy selection CL, CL, Benktander, BF and allocation reproduce SPEC §11", {
  d <- utils::read.csv(path_in("tests", "fixtures", "toy_triangle.csv"))
  m <- as.matrix(d[, c("lag12", "lag24", "lag36", "lag48")])
  dimnames(m) <- list(ay = d$ay, lag = 1:4)
  prem <- stats::setNames(d$premium, d$ay)
  f <- ldf_vector(m, "all")
  mt <- methods_table(m, m, prem, f, f)
  a <- list(selection_thresholds = list(cl_min_z = 0.70, bk_min_z = 0.30))
  mt$method <- auto_method(mt$pct_dev, a)
  expect_equal(mt$method, c("CL", "CL", "BK", "BF"))
  mt$ult_sel <- method_ultimate(mt, mt$method)
  mt$unpaid <- mt$ult_sel - mt$paid
  expect_equal(sum(mt$ult_sel), 182.84, tolerance = 0.01 / 182)
  expect_equal(sum(mt$unpaid), 66.84, tolerance = 0.01 / 66)
  expect_equal(sum(d$case_inc_2007 - mt$paid), 35.00, tolerance = 1e-9)
  expect_equal(sum(mt$ult_sel - d$case_inc_2007), 31.84, tolerance = 0.01 / 31)
  attr(mt, "pattern") <- list(f = f, tail = 1)
  pay <- colSums(predicted_by_cy(mt, 2008:2010))
  expect_equal(unname(pay), c(35.35, 21.63, 9.86), tolerance = 0.01 / 35)
  expect_equal(rowSums(allocate_unpaid(mt)), stats::setNames(mt$unpaid, mt$ay), tolerance = 1e-12)
})

test_that("every row has a method and a rationale; ult_sel = paid + case_os + ibnr exactly", {
  s <- utils::read.csv(path_in("outputs", "selection_2007.csv"), stringsAsFactors = FALSE)
  expect_equal(nrow(s), 10)
  expect_true(all(nzchar(s$method)) && all(nzchar(s$rationale)))
  expect_equal(round(s$paid + s$case_os + s$ibnr, 1), s$ult_sel, tolerance = 0)
  expect_equal(s$unpaid, s$case_os + s$ibnr, tolerance = 1e-9)
  expect_true(all(c("ay", "prem_net", "paid", "case_inc", "pct_dev", "ult_cl_paid", "ult_cl_inc", "ult_bf_paid",
                    "ult_bf_inc", "ult_cc", "ult_bk", "method", "ult_sel", "ulr_sel", "case_os", "ibnr", "unpaid",
                    "rationale") %in% names(s)))
})

test_that("loglinear tail equals MackChainLadder(tail = TRUE) and the frozen assumption", {
  up <- as_at(load_upper(), 2007)
  tr <- insurer_triangles(up, main_insurer())
  mack <- ChainLadder::MackChainLadder(ChainLadder::as.triangle(tr$paid), est.sigma = "Mack", tail = TRUE)
  pp <- development_pattern(tr$paid, "latest5")
  expect_equal(pp$tail, unname(utils::tail(mack$f, 1)), tolerance = 1e-12)
  expect_equal(pp$tail, read_config("assumptions_2007")$tail$value, tolerance = 1e-6)
})

test_that("automatic selection allocates exactly its unpaid", {
  up <- as_at(load_upper(), 2007)
  mt <- auto_reserve(up, main_insurer())
  expect_equal(unname(rowSums(allocate_unpaid(mt))), mt$unpaid, tolerance = 1e-9)
})

test_that("no holdout-derived output exists in git history before the tag", {
  # Outputs that need the holdout (cy >= 2008). LIC and the one-year simulation use upper data only.
  post_lock <- c("outputs/backtest_main.csv", "outputs/panel_backtest.csv", "outputs/calibration_panel.csv", "outputs/cv_numbers.json")
  root <- project_root()
  tag_exists <- lock_tag_exists(root)
  range <- if (tag_exists) "selection-locked" else "HEAD"
  files <- system2("git", c("-C", root, "log", range, "--name-only", "--format="), stdout = TRUE)
  expect_false(any(files %in% post_lock))
})

test_that("the tag selection-locked exists (set by the owner at Gate 3)", {
  skip_if_not(lock_tag_exists(project_root()), "tag not created yet: the owner tags after Gate 3")
  expect_true(lock_tag_exists(project_root()))
})
