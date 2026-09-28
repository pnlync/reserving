# M6 acceptance tests (SPEC §9 M6, §11 golden values).

test_that("Mack golden test: GenIns total IBNR 18,680,856 and Mack SE 2,447,095", {
  m <- suppressMessages(ChainLadder::MackChainLadder(ChainLadder::GenIns, est.sigma = "Mack"))
  ibnr <- sum(m$FullTriangle[, 10] - ChainLadder::getLatestCumulative(m$Triangle))
  expect_equal(round(ibnr), 18680856)
  expect_equal(unname(round(m$Total.Mack.S.E)), 2447095)
  expect_equal(round(mack_total_se_explicit(as.matrix(ChainLadder::GenIns))), 2447095)
})

test_that("Excel recomputes the total Mack SE within 0.01%", {
  chk <- utils::read.csv(path_in("outputs", "m3_excel_check.csv"))
  row <- chk[grepl("^Mack total SE", chk$item), ]
  expect_equal(nrow(row), 1)
  expect_lt(abs(row$rel_diff), 1e-4)
})

test_that("custom bootstrap with all AYs on CL matches BootChainLadder mean and SD within 2%", {
  # Like for like: the package keeps the two zero-residual corner cells in the pool, so the
  # engine is compared with exclude_zero = FALSE; the spec'd exclusion is reported in M6.
  up <- as_at(load_upper(), 2007)
  tr <- insurer_triangles(up, main_insurer())
  own <- selected_bootstrap(tr$paid, tr$premium, R = 10000, seed = 101, window = "all",
                            method = rep("CL", 10), prior_cv = 0, tail = FALSE, exclude_zero = FALSE)
  set.seed(102)
  pkg <- ChainLadder::BootChainLadder(ChainLadder::as.triangle(tr$paid), R = 10000, process.distr = "gamma")
  expect_lt(abs(mean(own$reserve) / mean(pkg$IBNR.Totals) - 1), 0.02)
  expect_lt(abs(stats::sd(own$reserve) / stats::sd(pkg$IBNR.Totals) - 1), 0.02)
})

test_that("main simulation used the locked stochastic spec and allocates to the right years", {
  sims <- readRDS(path_in("outputs", "sims_main.rds"))
  expect_equal(sims$spec_hash, spec_hash())
  if (lock_tag_exists(project_root())) {
    tagged <- system2("git", c("-C", project_root(), "rev-parse", "selection-locked:config/stochastic_spec.yaml"), stdout = TRUE)
    expect_equal(sims$spec_hash, tagged)
  }
  expect_equal(sims$R, locked_yaml("config/stochastic_spec.yaml")$replicates$main)
  expect_equal(unname(rowSums(sims$pay_cy)), unname(sims$reserve), tolerance = 1e-9)
})

test_that("with the tag, lock-scope files read from the tag equal the working tree", {
  skip_if_not(lock_tag_exists(project_root()), "tag not created yet")
  for (p in c("outputs/selection_2007.csv", "config/assumptions_2007.yaml", "config/stochastic_spec.yaml")) {
    tagged <- system2("git", c("-C", project_root(), "show", paste0("selection-locked:", p)), stdout = TRUE)
    expect_equal(tagged, readLines(path_in(p), warn = FALSE), info = p)
  }
})
