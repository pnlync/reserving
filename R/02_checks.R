# M1: data checks (SPEC §9 M1) and blind company selection (SPEC §6).
# Everything here reads the upper triangle through as_at(); the only full-file facts
# used are row counts and the cy identity (raw_structure(), no lower-triangle values).

# Per-insurer metrics on the upper triangle, one row per insurer.
insurer_metrics <- function(x) {
  assert_asat(x)
  do.call(rbind, lapply(sort(unique(x$grcode)), function(code) {
    xi <- for_insurer(x, code)
    ays <- sort(unique(xi$ay))
    complete <- nrow(xi) == 55 && length(ays) == 10
    if (!complete) {
      return(data.frame(grcode = code, n_upper = nrow(xi), complete = FALSE, min_prem_net = NA,
                        max_prem_change = NA, prem_const = NA, paid_decreases = NA, neg_case_os = NA,
                        max_ceded_share = NA, neg_inc_paid = NA, min_col_sum_inc = NA, min_lag1_paid = NA,
                        jumps = NA, prem_net_2007 = NA))
    }
    tr <- insurer_triangles(x, code)
    p <- tr$premium
    inc <- tr$inc_paid
    ratio <- tr$paid[, -1] / tr$paid[, -10]
    prem_const <- all(tapply(xi$prem_net, xi$ay, function(v) length(unique(v)) == 1))
    ceded <- premium(xi, "prem_ceded") / premium(xi, "prem_dir")
    data.frame(
      grcode = code, n_upper = nrow(xi), complete = TRUE,
      min_prem_net = min(p),
      max_prem_change = max(abs(p[-1] / p[-length(p)] - 1)),
      prem_const = prem_const,
      paid_decreases = sum(inc[, -1] < 0, na.rm = TRUE),
      neg_case_os = sum(tr$case_inc - tr$paid < 0, na.rm = TRUE),
      max_ceded_share = max(ifelse(premium(xi, "prem_dir") > 0, ceded, Inf)),
      neg_inc_paid = sum(inc < 0, na.rm = TRUE),
      min_col_sum_inc = min(colSums(inc, na.rm = TRUE)),
      min_lag1_paid = min(tr$paid[, 1]),
      jumps = sum(is.finite(ratio) & (ratio < 0.9 | ratio > 3), na.rm = TRUE),
      prem_net_2007 = unname(p["2007"])
    )
  }))
}

# Apply config/selection_rules.yaml.
apply_selection_rules <- function(m, rules = read_config("selection_rules")) {
  r <- rules$main
  s <- rules$panel_stochastic
  ok <- function(v) !is.na(v) & v
  m$pass_complete <- ok(m$complete & m$n_upper == r$upper_cells_required)
  m$pass_premium <- ok(m$min_prem_net > 0 & m$min_prem_net >= r$min_prem_net)
  m$pass_premium_panel <- ok(m$min_prem_net > 0 & m$min_prem_net >= rules$panel_backtest$min_prem_net)
  m$pass_stability <- ok(m$max_prem_change <= r$max_prem_change)
  m$pass_paid_monotone <- ok(m$paid_decreases <= r$max_paid_decreases)
  m$pass_case <- ok(m$neg_case_os <= r$max_negative_case_os)
  m$pass_ceded <- ok(m$max_ceded_share <= r$max_ceded_share)
  m$eligible_main <- with(m, pass_complete & pass_premium & pass_stability & pass_paid_monotone & pass_case & pass_ceded)
  m$panel_backtest <- with(m, pass_complete & pass_premium_panel & pass_stability & pass_paid_monotone & pass_case & pass_ceded)
  col_ok <- if (isTRUE(s$column_sums_positive)) ok(m$min_col_sum_inc > 0) else TRUE
  m$panel_stochastic_provisional <- with(m, panel_backtest &
    ok(neg_inc_paid <= s$max_negative_incremental_paid) & col_ok & ok(min_lag1_paid > 0))
  ranked <- m[m$eligible_main, ]
  ranked <- ranked[order(-ranked$prem_net_2007), ]
  m$role <- ""
  m$role[m$grcode == ranked$grcode[1]] <- "main"
  m$role[m$grcode == ranked$grcode[2]] <- "backup"
  m
}

main_insurer <- function() {
  p <- utils::read.csv(path_in("outputs", "panels.csv"))
  p$grcode[p$role == "main"]
}

run_checks <- function() {
  up <- as_at(load_upper(), VALUATION_YEAR)
  st <- raw_structure()
  m <- apply_selection_rules(insurer_metrics(up))
  panels <- m[, c("grcode", "role", "eligible_main", "panel_backtest", "panel_stochastic_provisional",
                  "pass_complete", "pass_premium", "pass_premium_panel", "pass_stability", "pass_paid_monotone", "pass_case", "pass_ceded",
                  "prem_net_2007")]
  utils::write.csv(panels, path_in("outputs", "panels.csv"), row.names = FALSE)
  write_data_checks(up, m, st)
  tri <- lapply(stats::setNames(nm = m$grcode[m$role != ""]), function(code) insurer_triangles(up, code))
  saveRDS(tri, path_in("data", "upper", "triangles_main_backup.rds"))
  invisible(m)
}

write_data_checks <- function(up, m, st) {
  full <- st$rows_per_insurer
  mc <- m[m$complete, ]
  main <- m$grcode[m$role == "main"]
  backup <- m$grcode[m$role == "backup"]
  fails <- function(col) sum(!m[[col]])
  cnt <- function(v) sum(!is.na(v) & v)
  lines <- c(
    "# Data checks (M1)",
    "",
    "US commercial auto, net, NAIC Schedule P (CAS December 2025 file), valuation 2007-12-31. Amounts in USD thousands (Schedule P is reported with 000 omitted; the CAS page does not state units, and premium magnitudes are consistent with thousands). All checks below use the upper triangle (cy <= 2007) only, except the row counts and the cy identity, which need no lower-triangle values.",
    "",
    "## Checks",
    "",
    "| Check | Rule | Result | Action |",
    "|---|---|---|---|",
    sprintf("| Completeness | 100 cells per insurer | %d insurers in file; %d have 100 cells; %d are missing whole accident years (%s rows) | Dropped: their upper triangles are incomplete (fewer than 55 cells) |",
            st$n_insurers, sum(full == 100), sum(full != 100), paste(sort(unique(as.integer(full[full != 100]))), collapse = "/")),
    sprintf("| Identities | cy = ay + lag - 1 | %d failures in the full file | none needed |", st$identity_failures),
    sprintf("| Identities | premium constant across lags within an AY | %d of %d complete insurers fail | investigate if any |", cnt(!mc$prem_const), nrow(mc)),
    sprintf("| Negative incremental paid | count by insurer | %d insurers have at least one; %d cells in total | flagged, kept as reported (never deleted or floored) |", cnt(mc$neg_inc_paid > 0), sum(mc$neg_inc_paid)),
    sprintf("| Case reserves | case_inc - paid >= 0 | %d insurers have negative case reserves in at least one cell; %d cells | counted; affects trust in incurred methods |", cnt(mc$neg_case_os > 0), sum(mc$neg_case_os)),
    sprintf("| Jumps | cumulative paid ratio outside [0.9, 3] (lag 2+) | %d insurers flagged; %d cells | flagged; large 24/12 ratios are normal for immature lags |", cnt(mc$jumps > 0), sum(mc$jumps)),
    sprintf("| Premium stability | adjacent-AY prem_net change > 50%% | %d insurers flagged | excluded from panels by rule |", cnt(mc$max_prem_change > 0.5)),
    sprintf("| Ceded share | prem_ceded / prem_dir | median of insurer maxima %s; %d insurers above 50%% | excluded from panels by rule |", fmt_pct(stats::median(mc$max_ceded_share[is.finite(mc$max_ceded_share)])), cnt(mc$max_ceded_share > 0.5)),
    "",
    "## Selection (config/selection_rules.yaml, frozen before analysis)",
    "",
    "| Rule | Insurers failing (of all in file) |",
    "|---|---|",
    sprintf("| 55 upper cells | %d |", fails("pass_complete")),
    sprintf("| prem_net >= 5,000 every AY (main only) | %d |", fails("pass_premium")),
    sprintf("| prem_net > 0 every AY (panels) | %d |", fails("pass_premium_panel")),
    sprintf("| adjacent prem_net change within 50%% | %d |", fails("pass_stability")),
    sprintf("| paid decreases in at most 2 cells | %d |", fails("pass_paid_monotone")),
    sprintf("| negative case reserve in at most 2 cells | %d |", fails("pass_case")),
    sprintf("| ceded share <= 50%% | %d |", fails("pass_ceded")),
    "",
    sprintf("- Eligible for main (all main rules): **%d** insurers.", sum(m$eligible_main)),
    sprintf("- `panel_backtest`: **%d** insurers.", sum(m$panel_backtest)),
    sprintf("- `panel_stochastic` (provisional; the bootstrap criterion is completed in M6): **%d** insurers.", sum(m$panel_stochastic_provisional)),
    sprintf("- Main insurer: NAIC code **%s** (largest AY 2007 net earned premium among eligible insurers, %s). Backup: **%s** (%s).",
            main, fmt_money(m$prem_net_2007[m$role == "main"]), backup, fmt_money(m$prem_net_2007[m$role == "backup"])),
    "",
    "Rule change v1.2 (before any estimation, upper-triangle counts only): the frozen v1.0 panel rules left 17 insurers in `panel_backtest` and 10 in `panel_stochastic`, because most insurers in the file are small (median of each insurer's smallest AY net premium about 570) and many have no payments at late lags. The 5,000 floor now applies to the main insurer only (panels need prem_net > 0, and M5 reports error by size tercile), and the stochastic rule 'every column sum of incremental paid > 0' is dropped (zero-mean cells get no noise, per the stochastic spec). The main insurer and backup are unchanged. See SPEC §14.",
    "",
    "Note: the rule 'paid decreases in at most 2 cells' and the stochastic-panel rule 'negative incremental paid in at most 2 cells' differ only at lag 1 (a negative lag 1 payment), which the stochastic panel also excludes through 'paid at lag 1 > 0'.",
    "",
    "Survivorship: CAS kept insurers that kept filing with consistent data; insurers that left the market are absent, so method performance may look better than it would on all insurers.",
    "",
    "Full panel flags: `outputs/panels.csv`. Insurer names never appear in outputs."
  )
  writeLines(lines, path_in("reports", "data_checks.md"))
}
