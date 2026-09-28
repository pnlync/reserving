# Source every module file (not the pipeline driver) so tests see the functions.
local({
  root <- normalizePath(file.path(getwd(), "..", ".."))
  for (f in sort(list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE))) {
    if (basename(f) != "run.R") sys.source(f, envir = globalenv())
  }
})
