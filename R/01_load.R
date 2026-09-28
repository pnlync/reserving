# M1: the only file that reads data/raw/ (SPEC §5.1). Splits the CAS file into
# data/upper/ (cy <= 2007) and data/holdout/ (cy >= 2008).

RAW_FILE <- "data/raw/comauto_pos_98-07.csv"
RAW_SHA256 <- "5012bd4c9048e300669e2f4fc915850449099e159481b4c3349b574f6f00afe1"

# CAS December 2025 names -> SPEC §4.1 names (see SPEC §14 v1.1 for the mapping).
RAW_COLUMNS <- c(
  GRCODE = "grcode", GRNAME = "grname", AccidentYear = "ay", DevelopmentYear = "cy",
  DevelopmentLag = "lag", IncurredLosses = "inc_booked", CumPaidLoss = "paid",
  BulkLoss = "bulk", EarnedPremDIR = "prem_dir", EarnedPremCeded = "prem_ceded",
  EarnedPremNet = "prem_net", Single = "single", PostedReserves2007 = "posted_reserves_2007"
)

read_raw <- function() {
  f <- path_in(RAW_FILE)
  if (!file.exists(f)) stop("missing ", RAW_FILE, ": see data/raw/README.md")
  sha <- digest::digest(file = f, algo = "sha256")
  if (sha != RAW_SHA256) stop("SHA-256 of ", RAW_FILE, " does not match data/raw/MANIFEST")
  raw <- utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  if (!identical(sort(names(raw)), sort(names(RAW_COLUMNS)))) {
    stop("unexpected columns in ", RAW_FILE, ": ", paste(names(raw), collapse = ", "))
  }
  d <- raw[, names(RAW_COLUMNS)]
  names(d) <- unname(RAW_COLUMNS)
  d$case_inc <- d$inc_booked - d$bulk
  d <- d[order(d$grcode, d$ay, d$lag), ]
  rownames(d) <- NULL
  d
}

# Writes the split; returns only row counts, never holdout values.
load_raw <- function() {
  d <- read_raw()
  upper <- d[d$cy <= VALUATION_YEAR, ]
  holdout <- d[d$cy > VALUATION_YEAR, ]
  saveRDS(upper, path_in("data", "upper", "comauto_upper.rds"))
  saveRDS(holdout, path_in("data", "holdout", "comauto_holdout.rds"))
  invisible(c(total = nrow(d), upper = nrow(upper), holdout = nrow(holdout)))
}

# Structural facts that need the full file but no lower-triangle values:
# number of rows per insurer and the identity cy = ay + lag - 1.
raw_structure <- function() {
  d <- read_raw()
  list(
    rows_per_insurer = table(d$grcode),
    identity_failures = sum(d$cy != d$ay + d$lag - 1),
    n_insurers = length(unique(d$grcode))
  )
}
