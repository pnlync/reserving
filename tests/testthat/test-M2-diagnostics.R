# M2 acceptance tests (SPEC §9 M2).

test_that("cyEffTest and dfCorTest ran on the main insurer's paid and incurred triangles", {
  t <- utils::read.csv(path_in("outputs", "m2_tests.csv"))
  expect_setequal(t$test, c("cyEffTest paid", "cyEffTest incurred", "dfCorTest paid", "dfCorTest incurred"))
  expect_true(all(is.finite(t$statistic)))
})

test_that("every conclusion in diagnostics.md cites an existing chart or test output", {
  txt <- readLines(path_in("reports", "diagnostics.md"))
  sections <- split(txt, cumsum(grepl("^## ", txt)))
  numbered <- sections[vapply(sections, function(s) grepl("^## [1-5]\\.", s[1]), logical(1))]
  expect_length(numbered, 5)
  for (s in numbered) {
    body <- paste(s, collapse = " ")
    expect_true(grepl("Decision", body), info = s[1])
    cited <- regmatches(body, gregexpr("m2_[a-z_]+\\.(csv|png)", body))[[1]]
    expect_gt(length(cited), 0)
    for (f in unique(cited)) {
      loc <- if (grepl("png$", f)) path_in("outputs", "figures", f) else path_in("outputs", f)
      expect_true(file.exists(loc), info = f)
    }
  }
})

test_that("ldf_vector matches ChainLadder's volume-weighted factors (all years)", {
  tr <- readRDS(path_in("data", "upper", "triangles_main_backup.rds"))[[as.character(main_insurer())]]
  pkg <- ChainLadder::chainladder(ChainLadder::as.triangle(tr$paid))
  pkg_f <- vapply(pkg$Models, function(m) unname(stats::coef(m)), numeric(1))
  expect_equal(ldf_vector(tr$paid, "all"), pkg_f, tolerance = 1e-10)
})
