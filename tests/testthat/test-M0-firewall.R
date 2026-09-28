# M0 acceptance tests (SPEC §9 M0, §5).

test_that("as_at(d, 2004) contains no cell with cy > 2004", {
  d <- load_upper()
  x <- as_at(d, 2004)
  expect_s3_class(x, "asat")
  expect_equal(max(x$cy), 2004)
  expect_equal(attr(x, "valuation"), 2004L)
  expect_true(all(x$cy == x$ay + x$lag - 1))
})

test_that("the upper-triangle file itself holds nothing after 2007", {
  expect_lte(max(load_upper()$cy), 2007)
})

test_that("estimation code rejects objects not created by as_at()", {
  expect_error(assert_asat(load_upper()), "as_at")
  expect_error(as_at(load_upper(), 2008), "beyond")
})

test_that("reveal() errors without the tag and works with it (temporary repo)", {
  tmp <- tempfile("repo")
  dir.create(file.path(tmp, "data", "holdout"), recursive = TRUE)
  saveRDS(data.frame(cy = 2008), file.path(tmp, "data", "holdout", "comauto_holdout.rds"))
  git <- function(...) system2("git", c("-C", tmp, ...), stdout = FALSE, stderr = FALSE)
  git("init", "-q")
  git("-c", "user.email=t@t", "-c", "user.name=t", "commit", "-q", "--allow-empty", "-m", "x")
  expect_error(reveal(repo = tmp), "does not exist")
  git("tag", "selection-locked")
  expect_equal(reveal(repo = tmp)$cy, 2008)
})

test_that("static check: only 01_load.R and reveal.R name the raw or holdout paths", {
  files <- list.files(path_in("R"), pattern = "\\.R$", full.names = TRUE)
  files <- files[!basename(files) %in% c("01_load.R", "reveal.R")]
  hits <- vapply(files, function(f) any(grepl("data/raw|data/holdout", readLines(f, warn = FALSE))), logical(1))
  expect_false(any(hits), info = paste(basename(files[hits]), collapse = ", "))
})

test_that("selection_rules.yaml and CONTRACT.md were committed before analysis code", {
  first_commit <- function(path) {
    out <- system2("git", c("-C", project_root(), "log", "--diff-filter=A", "--format=%ct", "--reverse", "--", path), stdout = TRUE)
    if (length(out) == 0) Inf else as.numeric(out[1])
  }
  rules_time <- max(first_commit("config/selection_rules.yaml"), first_commit("CONTRACT.md"))
  expect_true(is.finite(rules_time))
  analysis <- setdiff(list.files(path_in("R"), pattern = "\\.R$"), c("01_load.R", "reveal.R", "utils.R"))
  for (f in analysis) {
    expect_gte(first_commit(file.path("R", f)), rules_time)
  }
})
