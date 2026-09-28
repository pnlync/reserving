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
  stop("unknown stage: ", stage)
)
