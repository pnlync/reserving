# M1 acceptance tests (SPEC §9 M1).

test_that("own triangle equals ChainLadder::as.triangle()", {
  up <- as_at(load_upper(), 2007)
  code <- main_insurer()
  xi <- for_insurer(up, code)
  own <- tri_cum(xi, "paid")
  pkg <- ChainLadder::as.triangle(as.data.frame(unclass(xi)), origin = "ay", dev = "lag", value = "paid")
  expect_equal(unname(unclass(own)), unname(matrix(as.numeric(pkg), nrow = 10)))
  expect_equal(inc_to_cum(cum_to_inc(own))[!is.na(own)], own[!is.na(own)])
})

test_that("row count = 100 x insurers for complete insurers; upper = 55 each", {
  st <- raw_structure()
  complete <- names(st$rows_per_insurer)[st$rows_per_insurer == 100]
  expect_equal(sum(st$rows_per_insurer[complete]), 100 * length(complete))
  up <- load_upper()
  n_up <- table(up$grcode)[complete]
  expect_true(all(n_up == 55))
  expect_equal(st$identity_failures, 0)
})

test_that("every check in data_checks.md has a result row", {
  txt <- readLines(path_in("reports", "data_checks.md"))
  for (chk in c("Completeness", "Identities", "Negative incremental paid", "Case reserves", "Jumps", "Premium stability", "Ceded share")) {
    expect_true(any(grepl(paste0("^\\| ", chk, " \\|"), txt)), info = chk)
  }
})

test_that("panels: one main, one backup, both in panel_backtest; stochastic is a subset", {
  p <- utils::read.csv(path_in("outputs", "panels.csv"))
  expect_equal(sum(p$role == "main"), 1)
  expect_equal(sum(p$role == "backup"), 1)
  expect_true(all(p$panel_backtest[p$role != ""]))
  expect_true(all(p$panel_backtest[p$panel_stochastic_provisional]))
  expect_false(any(grepl("grname", names(p))))
})
