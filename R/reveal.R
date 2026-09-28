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
