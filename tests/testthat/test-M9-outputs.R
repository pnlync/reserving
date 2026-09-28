# M9 acceptance tests (SPEC §9 M9, §12).

test_that("cv_numbers.json has the SPEC keys and the git hash", {
  cv <- jsonlite::read_json(path_in("outputs", "cv_numbers.json"))
  keys <- c("err_paid120_selected", "err_paid120_cl", "err_inc120_selected", "err_inc120_cl", "err_inc120_booked",
            "n_panel_backtest", "med_err_cl_panel", "med_err_rule_panel", "n_panel_stochastic", "cov_q75_panel",
            "n_sims", "l995_over_be")
  expect_true(all(keys %in% names(cv$numbers)))
  expect_match(cv$git_hash, "^[0-9a-f]{40}$")
  expect_equal(cv$lock_tag_commit, system2("git", c("-C", project_root(), "rev-parse", "selection-locked^{commit}"), stdout = TRUE))
})

test_that("cv_numbers.json is reproduced from outputs/", {
  cv <- jsonlite::read_json(path_in("outputs", "cv_numbers.json"))
  now <- cv_numbers()
  expect_equal(unlist(now$numbers), unlist(cv$numbers), tolerance = 1e-9)
})

# Every figure quoted in README and the site pages is one of the cv_numbers displays or a
# fixed, labelled constant (dates, the 27% standard-formula factor, the 75% confidence level).
test_that("every number in README and the site is found in outputs", {
  cv <- jsonlite::read_json(path_in("outputs", "cv_numbers.json"))
  allowed <- c(unlist(cv$display), "27%", "75%", "9%", "5%", "3%", "7%", "10%", "50%", "70%", "30%", "90%", "65%", "85%", "95%", "100%")
  txt <- paste(readLines(path_in("README.md")), collapse = " ")
  found <- regmatches(txt, gregexpr("[0-9][0-9,]*\\.?[0-9]*(%|m\\b)", txt))[[1]]
  expect_true(all(found %in% allowed), info = paste(setdiff(found, allowed), collapse = ", "))
  for (f in c("index.qmd", "backtest.qmd", "uncertainty.qmd", "application.qmd")) {
    s <- readLines(path_in("site", f))
    s <- paste(s[!grepl("^#\\|", s)], collapse = " ")
    s <- gsub("`r [^`]*`", "", s)
    hard <- regmatches(s, gregexpr("[0-9][0-9,]*\\.?[0-9]*(%|m\\b)", s))[[1]]
    expect_true(all(hard %in% allowed), info = paste(f, paste(setdiff(hard, allowed), collapse = ", ")))
  }
})

test_that("no insurer names appear in public outputs", {
  names_raw <- unique(load_upper()$grname)
  files <- c(list.files(path_in("outputs"), pattern = "\\.(csv|json)$", full.names = TRUE), path_in("README.md"),
             list.files(path_in("reports"), pattern = "\\.(md|qmd)$", full.names = TRUE),
             list.files(path_in("docs"), pattern = "\\.html$", full.names = TRUE))
  txt <- paste(unlist(lapply(files, readLines, warn = FALSE)), collapse = " ")
  long <- names_raw[nchar(names_raw) >= 8]
  hits <- long[vapply(long, function(n) grepl(n, txt, fixed = TRUE), logical(1))]
  expect_length(hits, 0)
})

test_that("forbidden claims do not appear in public text", {
  txt <- paste(c(readLines(path_in("README.md")), readLines(path_in("reports", "reserving_memo.qmd")),
                 unlist(lapply(list.files(path_in("site"), pattern = "\\.qmd$", full.names = TRUE), readLines))), collapse = " ")
  expect_false(grepl("IFRS 17 compliant", txt, ignore.case = TRUE))
  expect_false(grepl("validated model", txt, ignore.case = TRUE))
  expect_false(grepl("[^t] SCR of|our SCR|the SCR is", txt))
})
