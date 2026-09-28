# M9: every public number, written by the pipeline with the git hash that produced it.

read_items <- function(file) {
  d <- utils::read.csv(path_in("outputs", file), stringsAsFactors = FALSE)
  stats::setNames(d$value, d$item)
}

cv_numbers <- function() {
  bt <- read_items("backtest_main_totals.csv")
  pre <- read_items("m4_prelock_comparison.csv")
  lic <- read_items("lic_2007.csv")
  oy <- read_items("m8_one_year_summary.csv")
  oyb <- read_items("one_year_backtest.csv")
  wid <- read_items("calibration_widening.csv")
  cal <- utils::read.csv(path_in("outputs", "calibration_summary.csv"))
  pb <- utils::read.csv(path_in("outputs", "panel_backtest.csv"))
  unc <- utils::read.csv(path_in("outputs", "m6_uncertainty_summary.csv"))
  sens <- utils::read.csv(path_in("outputs", "m6_sensitivities.csv"))
  spec <- locked_yaml("config/stochastic_spec.yaml")
  s <- pb[pb$scored, ]
  cust <- unc[grepl("^Custom", unc$model), ]
  num <- list(
    selected_unpaid = pre[["selected_unpaid"]], selected_case_os = pre[["selected_case_os"]], selected_ibnr = pre[["selected_ibnr"]],
    booked_unpaid = pre[["booked_unpaid"]], selected_over_booked = pre[["selected_over_booked_pct"]],
    err_paid120_selected = bt[["err_paid120_selected"]], err_paid120_cl = bt[["err_paid120_cl"]],
    err_inc120_selected = bt[["err_inc120_selected"]], err_inc120_cl = bt[["err_inc120_cl"]], err_inc120_booked = bt[["err_inc120_booked"]],
    ae_next_2008 = bt[["ae_next_total"]],
    n_panel_backtest = nrow(s), med_err_cl_panel = stats::median(abs(s$err_cl)), med_err_rule_panel = stats::median(abs(s$err_rule)),
    share_panel_booked_below_proxy = mean(pb$booked_below_proxy),
    n_panel_stochastic = cal$n[cal$model == "custom"], cov_q75_panel = cal$cov_q75[cal$model == "custom"],
    cov_q75_panel_package_odp = cal$cov_q75[cal$model == "package"], cov_q75_panel_mack = cal$cov_q75[cal$model == "mack"],
    cov_c50_panel = cal$c50[cal$model == "custom"], cov_c90_panel = cal$c90[cal$model == "custom"],
    ks_d_panel = cal$ks_d[cal$model == "custom"], ks_crit_panel = cal$ks_crit[cal$model == "custom"],
    widening_lambda = wid[["lambda"]],
    n_sims = spec$replicates$main,
    boot_mean = cust$mean, boot_cv = cust$cv, boot_q75 = cust$q75,
    sens_elr_5pp = max(abs(sens$change_pct[sens$assumption == "ELR prior"])),
    sens_inflation_2pct = sens$change_pct[sens$assumption == "Calendar inflation"],
    lic = lic[["lic"]], lic_be = lic[["discounted_be"]], lic_ra75 = lic[["ra_75"]], lic_ra_over_be = lic[["ra_over_be"]],
    l995_over_be = oy[["l995_over_be"]], sf_reserve_factor = oy[["sf_reserve_risk_factor"]], usp_factor = oy[["usp_factor"]],
    mw_cdr_se = oy[["mw_cdr_se"]], main_cdr_2008 = oyb[["main_cdr_2008_actual"]], main_cdr_2008_percentile = oyb[["main_cdr_2008_percentile"]],
    one_year_cov_q75_panel = oyb[["panel_cov_q75"]]
  )
  list(git_hash = git_hash(), lock_tag_commit = system2("git", c("-C", project_root(), "rev-parse", "selection-locked^{commit}"), stdout = TRUE),
       units = "USD thousands for money; ratios as decimals", numbers = num)
}

# Display formats used in the README / memo / CV (so every quoted number is traceable).
fmt_cv <- function(key, v) {
  pct_keys <- c("selected_over_booked", "err_paid120_selected", "err_paid120_cl", "err_inc120_selected", "err_inc120_cl",
                "err_inc120_booked", "med_err_cl_panel", "med_err_rule_panel", "share_panel_booked_below_proxy",
                "cov_q75_panel", "cov_q75_panel_package_odp", "cov_q75_panel_mack", "cov_c50_panel", "cov_c90_panel",
                "boot_cv", "sens_elr_5pp", "sens_inflation_2pct", "lic_ra_over_be", "l995_over_be", "sf_reserve_factor",
                "usp_factor", "main_cdr_2008_percentile", "one_year_cov_q75_panel")
  if (key %in% pct_keys) return(paste0(formatC(100 * abs(v), format = "f", digits = if (key %in% c("sf_reserve_factor")) 0 else 1), "%"))
  if (key %in% c("n_panel_backtest", "n_panel_stochastic", "n_sims")) return(formatC(v, format = "d", big.mark = ","))
  if (key %in% c("ae_next_2008", "widening_lambda", "ks_d_panel", "ks_crit_panel")) return(formatC(v, format = "f", digits = 2))
  paste0(formatC(v / 1000, format = "f", digits = 1), "m")
}

run_cv_numbers <- function() {
  cv <- cv_numbers()
  cv$display <- stats::setNames(lapply(names(cv$numbers), function(k) fmt_cv(k, cv$numbers[[k]])), names(cv$numbers))
  jsonlite::write_json(cv, path_in("outputs", "cv_numbers.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA)
  invisible(cv)
}

# README with every number taken from outputs/cv_numbers.json.
write_readme <- function() {
  cv <- jsonlite::read_json(path_in("outputs", "cv_numbers.json"))
  d <- cv$display
  tag_date <- system2("git", c("-C", project_root(), "log", "-1", "--format=%cs", "selection-locked"), stdout = TRUE)
  txt <- c(
    "# General Insurance Reserving & Reserve Risk Model",
    "",
    "A blind reserving study of a US commercial auto insurer (NAIC 1767) at **31 December 2007**, built from NAIC Schedule P paid and incurred triangles. The reserve selection was locked in git (tag `selection-locked`) before the 2008–2016 run-off was revealed and used to score it. The same rules were then run across dozens of insurers to test whether the model's stated uncertainty held up.",
    "",
    "**Site:** https://pnlync.github.io/reserving/ · **Memo (3 pages):** [reports/reserving_memo.pdf](reports/reserving_memo.pdf) · **Build contract:** [SPEC.md](SPEC.md), [CONTRACT.md](CONTRACT.md)",
    "",
    "## Business questions and answers",
    "",
    "| Question | Answer |",
    "|---|---|",
    sprintf("| How much should the insurer hold for unpaid claims at 2007-12-31? | USD %s net (case %s, IBNR %s) |", d$selected_unpaid, d$selected_case_os, d$selected_ibnr),
    sprintf("| How does that compare with what it booked? | %s above the booked USD %s |", d$selected_over_booked, d$booked_unpaid),
    sprintf("| How good was the locked estimate? | Projected payments to 120 months fell %s short of actual (pure chain ladder: %s short). Against 120-month incurred, the selection was %s low vs %s for chain ladder and %s for the insurer's booked estimate. |", d$err_paid120_selected, d$err_paid120_cl, d$err_inc120_selected, d$err_inc120_cl, d$err_inc120_booked),
    sprintf("| Did the stated uncertainty hold up? | Across %s insurers the bootstrap's 75th percentile covered %s of outcomes (package ODP and Mack: %s); intervals too narrow. |", d$n_panel_stochastic, d$cov_q75_panel, d$cov_q75_panel_package_odp),
    sprintf("| Illustrative net IFRS 17 LIC? | USD %s (BE %s + risk adjustment %s at a 75%% confidence level) |", d$lic, d$lic_be, d$lic_ra75),
    sprintf("| One-year reserve deterioration? | 1-in-200: %s of opening BE vs %s under the Solvency II standard formula (reserve risk only; not an SCR) |", d$l995_over_be, d$sf_reserve_factor),
    "",
    "Reserving is the subject; IFRS 17 and Solvency II are application layers. Every number above is written by the pipeline to `outputs/cv_numbers.json` with the git hash that produced it.",
    "",
    "## Method in one paragraph",
    "",
    "Paid and case-incurred chain ladder, BF with a leave-one-out Cape Cod prior, Cape Cod and Benktander, a log-linear tail. Methods were selected by accident year with a rule on percent developed (CL at 70% or more, Benktander at 30–70%, BF below 30%) and one documented exception (incurred chain ladder for mature years, based on a pre-lock pseudo-historical test). Uncertainty comes from Mack, the ChainLadder ODP bootstrap and a custom ODP bootstrap of the selected reserve itself. The illustrative net LIC discounts the simulated cash flows on the GSW US Treasury curve. The one-year measure re-reserves 10,000 simulated years with the locked rules, benchmarked against Merz–Wüthrich. An Excel workbook (`excel/reserving_check.xlsx`) independently rebuilds the LDFs, chain ladder, BF, Cape Cod and the Mack total standard error.",
    "",
    "## Hindsight firewall",
    "",
    "- Only `R/01_load.R` reads the raw file; `reveal()` refuses to read the 2008–2016 holdout unless the tag `selection-locked` exists. A static test enforces this.",
    sprintf("- Selection rules and scoring formulas were committed before any analysis; the selection, assumptions and stochastic spec were locked on %s. Scoring reads the selection from the tag.", tag_date),
    "- Rule changes before the lock, and modelling decisions after it, are logged in SPEC.md §14.",
    "",
    readLines(path_in("reports", "readme_reproduce.md")),
    "",
    "## Limitations",
    "",
    "US business in USD at 2007; net of reinsurance; no claim counts, ULAE or rate changes; back-test only to 120 months (tail untested); one vintage and small panels; survivorship bias from the CAS selection; IFRS 17 did not exist in 2007; the one-year measure covers reserve risk only.",
    "",
    "## Data and disclaimer",
    "",
    "> Data: CAS Loss Reserving Database (NAIC Schedule P), compiled by S&P Global Market Intelligence for the Casualty Actuarial Society. Net of reinsurance, US business. The IFRS 17 and Solvency II figures are illustrative methodology demonstrations, not accounting or regulatory numbers for any company. Insurers are identified by NAIC code only.",
    ""
  )
  writeLines(txt, path_in("README.md"))
}
