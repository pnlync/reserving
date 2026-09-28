# Shared helpers and the as_at() firewall (SPEC §5).

VALUATION_YEAR <- 2007L

# Project root: the directory that holds SPEC.md (works from tests/testthat too).
project_root <- function() {
  d <- normalizePath(getwd())
  while (!file.exists(file.path(d, "SPEC.md"))) {
    parent <- dirname(d)
    if (parent == d) stop("project root (SPEC.md) not found")
    d <- parent
  }
  d
}

path_in <- function(...) file.path(project_root(), ...)

read_config <- function(name) yaml::read_yaml(path_in("config", paste0(name, ".yaml")))

# Upper triangle for all insurers, written by R/01_load.R (cy <= 2007 only).
load_upper <- function() {
  f <- path_in("data", "upper", "comauto_upper.rds")
  if (!file.exists(f)) stop("data/upper not built: run load_raw() in R/01_load.R first")
  readRDS(f)
}

# as_at(d, v): only cells visible at the end of calendar year v (cy <= v).
# Every estimation function accepts only objects of class "asat" (SPEC §5.2).
as_at <- function(d, v) {
  stopifnot(is.data.frame(d), "cy" %in% names(d), length(v) == 1)
  if (v > max(d$cy)) stop("as_at(): valuation ", v, " is beyond the data supplied (max cy ", max(d$cy), ")")
  out <- as.data.frame(d)[d$cy <= v, , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "valuation") <- as.integer(v)
  class(out) <- c("asat", "data.frame")
  out
}

assert_asat <- function(x) {
  if (!inherits(x, "asat")) stop("estimation functions accept only objects created by as_at()")
  invisible(x)
}

valuation_of <- function(x) attr(assert_asat(x), "valuation")

# Subset one insurer while keeping the as_at() class and valuation.
for_insurer <- function(x, code) {
  v <- valuation_of(x)
  out <- as.data.frame(unclass(x))[x$grcode == code, , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "valuation") <- v
  class(out) <- c("asat", "data.frame")
  out
}

git_hash <- function(path = NULL) {
  args <- if (is.null(path)) c("rev-parse", "HEAD") else c("log", "-1", "--format=%H", "--", path)
  out <- suppressWarnings(system2("git", c("-C", project_root(), args), stdout = TRUE, stderr = FALSE))
  if (length(out) == 0) NA_character_ else out[1]
}

fmt_money <- function(x) formatC(x, format = "f", digits = 1, big.mark = ",")
fmt_pct <- function(x) paste0(formatC(100 * x, format = "f", digits = 1), "%")
