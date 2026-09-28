# Pipeline driver called by the Makefile: Rscript R/run.R <stage>
local({
  here <- normalizePath(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
  for (f in sort(list.files(here, pattern = "\\.R$", full.names = TRUE))) {
    if (basename(f) != "run.R") sys.source(f, envir = globalenv())
  }
})
stage <- commandArgs(TRUE)[1]
switch(stage,
  load = print(load_raw()),
  checks = run_checks(),
  diagnostics = run_diagnostics(),
  deterministic = run_deterministic(),
  excel = excel_check(),
  select = run_select(),
  uncertainty = run_uncertainty_main(),
  lic = run_lic(),
  one_year = run_one_year(),
  backtest = run_backtest(),
  calibration = run_calibration(),
  one_year_backtest = run_one_year_backtest(),
  cv_numbers = { run_cv_numbers(); write_readme() },
  stop("unknown stage: ", stage)
)
