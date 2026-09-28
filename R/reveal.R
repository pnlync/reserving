# reveal(): the only door to the holdout (SPEC §5.3). Stops unless the git tag
# `selection-locked` exists in the repository.

LOCK_TAG <- "selection-locked"

lock_tag_exists <- function(repo = project_root()) {
  out <- suppressWarnings(system2("git", c("-C", repo, "tag", "--list", LOCK_TAG), stdout = TRUE, stderr = FALSE))
  length(out) > 0 && any(out == LOCK_TAG)
}

reveal <- function(repo = project_root()) {
  if (!lock_tag_exists(repo)) stop("reveal(): the tag '", LOCK_TAG, "' does not exist; the holdout stays closed until the selection is locked")
  f <- file.path(repo, "data", "holdout", "comauto_holdout.rds")
  if (!file.exists(f)) stop("reveal(): holdout file not built")
  readRDS(f)
}

# The locked selection, always read from the tag (SPEC §5.7).
locked_selection <- function(repo = project_root()) {
  if (!lock_tag_exists(repo)) stop("locked_selection(): tag '", LOCK_TAG, "' does not exist")
  txt <- system2("git", c("-C", repo, "show", paste0(LOCK_TAG, ":outputs/selection_2007.csv")), stdout = TRUE)
  utils::read.csv(text = paste(txt, collapse = "\n"), stringsAsFactors = FALSE)
}

# A file in the lock scope (e.g. outputs/selection_2007.csv, config/stochastic_spec.yaml).
# After the tag it is always read from the tag. Before the tag (pre-reveal work such as
# the main-insurer bootstrap) the working-tree copy is used; a test checks that the two
# are identical once the tag exists.
locked_text <- function(path, repo = project_root()) {
  if (lock_tag_exists(repo)) {
    system2("git", c("-C", repo, "show", paste0(LOCK_TAG, ":", path)), stdout = TRUE)
  } else {
    readLines(file.path(repo, path), warn = FALSE)
  }
}

locked_csv <- function(path, repo = project_root()) {
  utils::read.csv(text = paste(locked_text(path, repo), collapse = "\n"), stringsAsFactors = FALSE)
}

locked_yaml <- function(path, repo = project_root()) yaml::yaml.load(paste(locked_text(path, repo), collapse = "\n"))

# git blob hash of the stochastic spec, recorded with every simulation.
spec_hash <- function(repo = project_root()) {
  txt <- locked_text("config/stochastic_spec.yaml", repo)
  f <- tempfile()
  writeLines(txt, f)
  system2("git", c("-C", repo, "hash-object", f), stdout = TRUE)
}
