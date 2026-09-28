# SPEC: General Insurance Reserving & Reserve Risk Model

Version 1.2 (2026-09-28). Change log in §14.

A reserving study of a real US commercial auto insurer at 31 December 2007, built from NAIC Schedule P data (CAS Loss Reserving Database). The selection is made and locked using only data visible at year-end 2007. The 2008–2016 run-off is then revealed and used to score it. On top of the locked selection the project quantifies uncertainty, tests calibration across many insurers, derives an illustrative net IFRS 17 LIC and a one-year reserve deterioration measure.

This file is the contract for the coding agent. The companion guide (Chinese, "GI Reserving Project Guide") explains every concept and gives a worked toy example. If this file is unclear, contradicts itself or looks wrong, stop and ask. Do not guess. `CONTRACT.md` (written in M0) is the short, frozen version of the rules in sections 4 and 5.

Disclaimer to carry in the README and every report:

> Data: CAS Loss Reserving Database (NAIC Schedule P), compiled by S&P Global Market Intelligence for the Casualty Actuarial Society. Net of reinsurance, US business. The IFRS 17 and Solvency II figures are illustrative methodology demonstrations, not accounting or regulatory numbers for any company. Insurers are identified by NAIC code only.

---

## 1. How we work

1. One module at a time, in the order of section 9 (M0, M1, ...). Do not start the next module until the user replies "continue".
2. Before coding a module, explain it in Chinese (max 300 words): the question it answers, inputs, outputs, key formulas, which data it is allowed to read, common mistakes. Wait for the user's OK.
3. Write the module's acceptance tests first (`tests/testthat/test-M<n>-*.R`). After implementing, report each test as PASS or FAIL with the numbers.
4. Show 3–5 key numbers and at most 2 charts, each with one sentence saying whether it looks reasonable and why.
5. Ask the module's teach-back questions (listed per module) and correct the user's answers.
6. Commit once per module with the message `M<n>: <module name>`.
7. Gates (section 9) end with the user answering the gate questions in the guide. Gate 3 (the lock) is the only irreversible step.

## 2. Business questions

1. At 2007-12-31, how much should the insurer hold for unpaid claims, by accident year, split into case reserves and IBNR?
2. How does that compare with what the insurer itself booked?
3. How good was the locked estimate against the actual 2008–2016 run-off?
4. How uncertain is the estimate, and did the model's stated uncertainty hold up across many insurers?
5. What does the estimate imply for an illustrative net IFRS 17 liability for incurred claims (LIC)?
6. How much could the reserve deteriorate over one year, compared with the Solvency II standard formula?

Framing for all reports: **reserving is the subject; IFRS 17 and Solvency II are application layers.** Never write "IFRS 17 compliant", "SCR", "validated model", or name an insurer.

## 3. Scope

In scope: paid and case-incurred chain ladder, BF, Cape Cod, Benktander, tail; rule-based selection with documented exceptions; blind lock; back-tests A–D; cross-insurer back-test and calibration; Mack; ODP bootstrap (package and custom selected-reserve version); sensitivities; illustrative net LIC (confidence-level RA); one-year CDR (Merz–Wüthrich benchmark and simulate–re-reserve); Solvency II standard formula comparison.

Out of scope for the core (list as limitations): gross-of-reinsurance measurement; frequency–severity and Berquist–Sherman (no claim counts); premium on-levelling (no rate data); ULAE study (assumed); premium risk, catastrophe, diversification; full SCR; full IFRS 17 (LRC, CSM, reinsurance held). Optional extensions (after v3 only): Meyers replication on the 1988–97 data; Other Liability – Occurrence; Munich chain ladder / PaidIncurredChain; cost-of-capital RA and `QuantileIFRS17()`.

## 4. Data, dates and conventions

### 4.1 Source

| Item | Value |
|---|---|
| Dataset | CAS "Loss Reserving Data Pulled from NAIC Schedule P" (December 2025 version) |
| File | `comauto_pos_98-07.csv` (Commercial Auto) |
| Page | https://www.casact.org/publications-research/research/research-resources/loss-reserving-data-pulled-naic-schedule-p |
| Coverage | accident years 1998–2007 × development lags 1–10 per insurer; lower triangle from later filings, to 2016 |
| Basis | US business, net of reinsurance, losses include DCC, no ULAE |

Download the file into `data/raw/`, record its SHA-256 and download date in `data/raw/MANIFEST`. Read the column names from the file; the expected ones are below. If they differ, stop and report.

| Original | Renamed | Meaning |
|---|---|---|
| `GRCODE`, `GRNAME` | `grcode`, `grname` | insurer code, name (name never appears in outputs) |
| `AccidentYear`, `DevelopmentLag`, `DevelopmentYear` | `ay`, `lag`, `cy` | must satisfy `cy = ay + lag - 1` |
| `CumPaidLoss_C` | `paid` | cumulative paid loss + DCC |
| `IncurLoss_C` | `inc_booked` | insurer's booked ultimate (paid + case + bulk/IBNR) |
| `BulkLoss_C` | `bulk` | bulk and IBNR reserves |
| derived | `case_inc = inc_booked - bulk` | case incurred |
| `EarnedPremDIR_C`, `EarnedPremCeded_C`, `EarnedPremNet_C` | `prem_dir`, `prem_ceded`, `prem_net` | earned premium; all ratios use `prem_net` |

Units: as in the file (Schedule P amounts are reported in USD thousands; confirm from the CAS documentation and state it on every table).

### 4.2 Dates

| Item | Value |
|---|---|
| Valuation date | 2007-12-31 |
| Upper triangle (visible) | `cy <= 2007` |
| Holdout (lower triangle) | `2008 <= cy <= 2016` |
| Pseudo-historical valuations (pre-lock) | 2004, 2005, 2006 |
| Rolling mechanical re-reserves (post-lock) | 2008–2015 |
| Cash-flow timing | payments mid-year: development year j of accident year i is paid at time `(i + j - 1) - 2007 - 0.5` years from the valuation date |

### 4.3 Conventions

- All triangles are cumulative unless named `inc_*`. Lags 1–10 = 12–120 months.
- LDFs are volume-weighted unless the M2 decision says otherwise.
- Percent developed `z_k = 1 / CDF_k`, CDF including the selected tail.
- Money to one decimal in the unit of the file; ratios to one decimal percent.
- Seeds in `config/seeds.yaml`; every simulation records its seed and the git hash of `stochastic_spec.yaml`.

## 5. Hindsight firewall and lock (rules in `CONTRACT.md`)

1. Only `R/01_load.R` may read `data/raw/`. It writes `data/upper/` (cy <= 2007) and `data/holdout/` (cy >= 2008).
2. `as_at(data, v)` returns only cells with `cy <= v`. Every estimation function accepts only an object created by `as_at()` (class check).
3. `reveal()` reads `data/holdout/` and stops with an error unless the git tag `selection-locked` exists.
4. Static test: no file in `R/` except `01_load.R` and `reveal.R` contains the strings `data/raw` or `data/holdout`.
5. Committed before any analysis (M0): `config/selection_rules.yaml`, `CONTRACT.md` (lock scope, scoring formulas of section 9 M5, bug policy).
6. Committed before the tag (M4): `outputs/selection_2007.csv`, `config/assumptions_2007.yaml`, `config/stochastic_spec.yaml`, memo page 1 and first half of page 2.
7. Scoring scripts read the selection from the tag: `git show selection-locked:outputs/selection_2007.csv`.
8. Bug policy: after the tag, only pure code errors may be fixed. Publish both the locked and corrected numbers with the reason in `reports/post_lock_changes.md`. Judgement choices (methods, ELR, tail, thresholds, spec) never change.
9. Exception to "upper triangle only": selecting insurers for scoring requires lower-triangle cells to exist. CAS already keeps only insurers with complete 10×10 data (its Step III), so no filter reads lower-triangle values.

## 6. Company selection (`config/selection_rules.yaml`)

Main insurer and backup (upper triangle only):

| Rule | Default (user confirms in M0, then frozen) |
|---|---|
| Upper triangle complete | all 55 cells present |
| Net earned premium | > 0 for every AY, and >= 5,000 (USD thousands) for every AY |
| Premium stability | adjacent-AY change in `prem_net` within ±50% |
| Paid monotone | cumulative paid decreases in at most 2 upper cells |
| Case reserves | `case_inc - paid < 0` in at most 2 upper cells |
| Ceded share | `prem_ceded / prem_dir <= 50%` for every AY |
| Choice | largest 2007 AY `prem_net` = main; second = backup |

Panels:

| Panel | Rule | Use |
|---|---|---|
| `panel_backtest` | all rules above except "Choice" | deterministic back-test (M5) |
| `panel_stochastic` | `panel_backtest` plus: upper incremental paid negative in at most 2 cells; every development column sum of incremental paid > 0; paid at lag 1 > 0 for every AY; custom bootstrap runs without error and its scale parameter is finite | calibration (M6, M8) |

Report the counts; do not preset them. The counts are CV numbers.

## 7. Assumption files

### 7.1 `config/assumptions_2007.yaml` (frozen at the lock)

| Item | Value / rule |
|---|---|
| LDF window | from M2: `all` or `latest5` diagonals; volume-weighted |
| Base triangle | paid (incurred used for cross-checks and documented exceptions) |
| Tail | selected value and method (see M3); sensitivity (tail − 1) × 0.5 and × 1.5 |
| Selection thresholds | z >= 70% paid CL; 30% <= z < 70% Benktander; z < 30% BF |
| Paid vs incurred flag | |U_paid − U_inc| / U_paid > 10% requires a rationale |
| BF prior | leave-one-out Cape Cod ELR on paid (formula in M3) |
| Peer ELR check | median panel ELR by AY; used only as a reasonableness check |
| ELR sensitivity | ±5 percentage points |
| ULAE | 5% of future claim payments; sensitivity 3% and 7% |
| Confidence levels | 75% (base); also 65%, 85%, 90% |
| Discount curve | GSW nominal zero-coupon curve, 2007-12-31; no illiquidity premium; sensitivity +50 bp |

### 7.2 `config/stochastic_spec.yaml` (frozen at the lock)

| Item | Value |
|---|---|
| Replicates | main insurer 10,000; each panel insurer 2,000; one-year re-reserve 10,000 |
| Residuals | CL back-fitted ODP means m; unscaled Pearson r = (X − m)/sqrt(|m|); adjusted by sqrt(n/(n − p)); corner cells with zero residual excluded from the resampling pool |
| Resampling | with replacement, from all non-excluded adjusted residuals |
| Process distribution | gamma with mean m and variance φ·m (φ = Pearson scale); for m <= 0 use m (no noise) and record the count |
| Prior uncertainty | ELR multiplied by lognormal with mean 1 and CV 10% |
| Method table | locked `method` column of `selection_2007.csv` (main); automatic thresholds (panel) |
| Outputs | future payments by calendar year, to lag 10 and with tail |
| Calibration metric | one-sided coverage of the 75th percentile of future payments to lag 10 (headline); two-sided 50/75/90/95% coverage; KS at 1.36/sqrt(n) |
| Widening sensitivity | factor λ on log deviations from the median, fitted on a random half of `panel_stochastic` (seed fixed) and tested on the other half; sensitivity only |

## 8. External data

| Data | Use | Source |
|---|---|---|
| GSW nominal yield curve (`feds200628.csv`: Svensson parameters BETA0–BETA3, TAU1, TAU2; zero yields SVENYxx) | M7 discounting | https://www.federalreserve.gov/data/nominal-yield-curve.htm |
| ChainLadder datasets `GenIns`, `MW2008` | golden tests | R package ChainLadder (pin version with renv; guide assumes 0.2.22) |
| Solvency II parameters: σ reserve, motor vehicle liability = 9%; 3σV formula; USP credibility c = 74% for 10 years | M8 | Delegated Regulation (EU) 2015/35 Art. 115, Annex II, Annex XVII (store in `config/solvency2.yaml` with references) |

Svensson zero yield (continuously compounded, percent; confirm in the Fed documentation):

```
y(n) = b0 + b1 * (1 - exp(-n/t1)) / (n/t1)
          + b2 * ((1 - exp(-n/t1)) / (n/t1) - exp(-n/t1))
          + b3 * ((1 - exp(-n/t2)) / (n/t2) - exp(-n/t2))
DF(n) = exp(-y(n)/100 * n)
```

Use 2007-12-31; if that row is missing, the last earlier trading day, and state it. Beyond 30 years hold the 30-year yield flat.

## 9. Modules

Each module lists purpose, method, outputs, acceptance tests and teach-back questions. Gates: 1 after M2, 2 after M3, 3 (lock) after M4, 4 after M5 (CV v1), 5 after M6 (CV v2), 6 after M8, then M9 (CV v3).

### M0 Setup, contract and firewall

- `renv`, repo skeleton (section 10), `Makefile` with `make all`, `config/seeds.yaml`.
- Write `CONTRACT.md` (one page): valuation date, what may be read pre-lock, lock scope (section 5), scoring formulas (M5), bug policy.
- Commit `selection_rules.yaml` (section 6) after the user confirms the defaults.
- `R/01_load.R` stub, `as_at()`, `reveal()`, firewall tests.
- Tests: `as_at(d, 2004)` contains no `cy > 2004`; `reveal()` errors without the tag (test in a temporary repo); static check of section 5.4; git log shows `selection_rules.yaml` and `CONTRACT.md` committed before any file in `R/` other than the loader and firewall.
- Teach-back: what does `as_at(d, 2005)` return? What exactly does the lock freeze? What happens if a bug is found after the lock?

### M1 Data and triangles

- Load, rename, derive `case_inc`, split upper/holdout, write `reports/data_checks.md`:

| Check | Rule | Action |
|---|---|---|
| Completeness | 100 cells per insurer | CAS already filtered; verify, drop if not |
| Identities | `cy = ay + lag - 1`; premium constant across lags within an AY | investigate |
| Negative incremental paid | count by insurer | flag; never delete or floor at zero |
| Case reserves | `case_inc - paid >= 0` | count negatives |
| Jumps | ratio of consecutive cumulative paid outside [0.9, 3] | flag |
| Premium stability | adjacent-AY `prem_net` change > ±50% | flag |
| Ceded share | `prem_ceded / prem_dir` | report |

- Apply section 6: main, backup, `panel_backtest`, `panel_stochastic` (the "custom bootstrap runs" criterion is completed in M6; record provisional membership here).
- Triangles per insurer: cumulative paid, cumulative case incurred, incremental paid, premium vector.
- Outputs: `data_checks.md`, `outputs/panels.csv` (grcode, panel flags), triangle objects in `data/upper/`.
- Tests: own triangle equals `ChainLadder::as.triangle()`; row count = 100 × insurers; every check has a result row.
- Teach-back: why is `IncurLoss` not case incurred? Why keep negative increments? Why two panels?

### M2 Diagnostics

- `ata()` with volume-weighted, simple, latest-3 and latest-5 averages and excluding high/low; paid ÷ case incurred by lag and AY; paid CL vs incurred CL by AY; `cyEffTest()`; `dfCorTest()`; `checkTriangleInflation()` on premium-normalised paid (it fits Y = a(1+b)^x down each development column and is meant for average amounts); `plot(MackChainLadder(...))` residuals by AY, CY and lag.
- Output `reports/diagnostics.md` (one page) answering: paid or incurred more reliable (and whether a rising paid/incurred ratio looks like settlement speed-up or case-reserve weakening); recent vs early development; outlier factors and treatment; calendar-year effects; LDF window. Write the decisions into a draft `assumptions_2007.yaml`.
- Tests: every conclusion cites a chart or test output; `cyEffTest` and `dfCorTest` run on the main insurer's paid and incurred triangles.
- Teach-back: what does a 24/12 factor of 1.8 mean? If paid/incurred rises by AY, which method is biased and in which direction under each explanation?

### M3 Point estimates and tail

Formulas (C = latest cumulative, k = latest lag, P = `prem_net`):

```
f_j        = sum_i C[i, j+1] / sum_i C[i, j]      (over AYs with both cells, within the window)
CDF_k      = f_k * f_(k+1) * ... * f_9 * tail
z_k        = 1 / CDF_k
U_CL       = C * CDF_k
ELR_CC     = sum_i C_i / sum_i (P_i * z_i)
ELR_-i     = sum_(j != i) C_j / sum_(j != i) (P_j * z_j)        (leave-one-out prior for AY i)
U_BF       = C + ELR * P * (1 - z)
U_CC       = C + ELR_CC * P * (1 - z)
U_GB       = C + (1 - z) * U_BF                      (Benktander)
```

- Run on paid and case incurred: paid CL, incurred CL, paid BF, incurred BF (same prior), Cape Cod, Benktander.
- Tail candidates: `MackChainLadder(..., tail = TRUE)` (log-linear on f − 1); case incurred ÷ paid for AY 1998 at lag 10; median over `panel_backtest` of `inc_booked / paid` for AY 1998 at lag 10 (visible in 2007). Select one with a written reason.
- Peer ELR check: median panel ELR by AY from the upper triangles; flag if the prior differs by more than 10 points.
- Excel check (`excel/reserving_check.xlsx`, live formulas, built from the upper triangle): paid LDFs, CDFs, CL ultimates, BF, Cape Cod ELR. Recalculate with LibreOffice headless and compare.
- Output `outputs/methods_2007.csv`.
- Tests: toy fixture (section 11) reproduced; own CL equals `MackChainLadder` ultimates (tolerance 1e-8 relative); BF with ELR = U_CL/P equals CL; Excel within 0.01%.
- Teach-back: what is CDF = 4? Why is young-AY chain ladder unstable, and what does BF cost? What is "used-up premium"? Why leave-one-out?

### M4 Selection and lock

- Pseudo-historical validation: for v in 2004, 2005, 2006, run the default rules on `as_at(d, v)` and compare predicted with actual paid on diagonals up to 2007 (A/E by diagonal). May inform window and tail.
- Apply the thresholds of 7.1 to paid z; list exceptions with rationale; flag paid vs incurred gaps > 10%.
- `outputs/selection_2007.csv`: `ay, prem_net, paid, case_inc, pct_dev, ult_cl_paid, ult_cl_inc, ult_bf_paid, ult_bf_inc, ult_cc, ult_bk, method, ult_sel, ulr_sel, case_os, ibnr, unpaid, rationale`.
- Pre-lock comparisons: selected total unpaid vs insurer's booked unpaid (`sum(inc_booked - paid)` on the 2007 diagonal); range across methods; case/IBNR split.
- Write memo page 1 and the first half of page 2. Freeze `assumptions_2007.yaml` and `stochastic_spec.yaml`.
- Lock: commit, then `git tag selection-locked` with a message summarising total unpaid, case/IBNR and the difference from the booked figure.
- Tests: every row has `method` and `rationale`; `ult_sel = paid + case_os + ibnr` exactly; the tag exists; no output derived from holdout exists in history before the tag.
- Teach-back: why is each AY's method chosen? Is the selection above or below the insurer's booked reserve, and why? What may still change after the lock?

**Gate 3.** The user answers the gate questions, then tags.

### M5 Reveal and back-test

Allocation of the selected unpaid to future development years (used by A, C and M7), with g_j = 1/CDF_j (tail included, g after the tail = 1):

```
share_(i,j) = (g_j - g_(j-1)) / (1 - g_k)          for j = k+1, ..., 10, then the tail remainder (1 - g_10)/(1 - g_k)
predicted payment in dev year j = unpaid_sel_i * share_(i,j)
```

| Test | Definition | Scope |
|---|---|---|
| A paid to 120 months (headline) | F̂_i = predicted payments to lag 10; F_i = C[i,10] − C[i,k_i]; e_A = (Σ F̂ − Σ F) / Σ F | all AYs |
| B incurred proxy, three-way | R_proxy = Σ (I[i,10] − C[i,k_i]); e_B = (R̂ − R_proxy) / R_proxy for R̂ = selected unpaid, booked unpaid, pure paid CL unpaid (with tail) | AY 1999–2007 only (AY 1998's proxy equals the booked figure) |
| C next-year A/E | actual 2008 incremental paid / predicted (share for j = k+1) by AY and total | AY 1998–2007 |
| D rolling re-reserve | for v = 2008..2015 rerun the locked rules mechanically on `as_at(d, v)` (automatic thresholds, prior and tail recomputed by the locked method); record reserve path and actual CDR per year | AY 1998–2007 |

- Panel: for each `panel_backtest` insurer run the automatic pipeline (no manual exceptions) with pure CL and with the rule; report median |e_A| (pure CL vs rule), error by maturity band and by size tercile, and the share of insurers whose booked 2007 unpaid was below the proxy.
- Charts: three-way bar chart (Test B), rolling reserve path (Test D), panel error distribution.
- Write memo page 3 (post-reveal addendum): results, what hindsight would change, whether it was knowable in 2007.
- `cv_numbers.json` keys: `err_paid120_selected`, `err_paid120_cl`, `err_inc120_selected`, `err_inc120_cl`, `err_inc120_booked`, `n_panel_backtest`, `med_err_cl_panel`, `med_err_rule_panel`.
- Tests: scoring reads the selection from the tag; toy allocation reproduces section 11; shares sum to 1 per AY; Test D at v = 2007 reproduces the automatic version of the locked selection.
- Teach-back: why is Test A the headline? Why is 120-month incurred only a proxy? Why divide by future payments? What if the booked estimate won?

**Gate 4.** Update the CV to v1.

### M6 Uncertainty and calibration

- Mack: `MackChainLadder(paid, est.sigma = "Mack", tail = <selected>)` and on incurred; SE and CV by AY and total; process vs parameter split. Excel recomputes the total Mack SE (0.01%).
- Package ODP: `BootChainLadder(paid, R = 10000, process.distr = "od.pois", seed = ...)`; residual plots by lag and CY. It returns pseudo-triangles in `simClaims` but projects pure CL.
- Custom selected-reserve bootstrap (spec 7.2): per replicate, pseudo-triangle (own resampling, or `simClaims`) → LDFs and CDFs → leave-one-out Cape Cod ELR on pseudo data × prior noise → locked method per AY → process noise on future increments. Save future payments by calendar year (to lag 10 and with tail): `outputs/sims_main.rds`.
- Sensitivities (tornado, deterministic): ELR ±5pp; tail (tail − 1) × 0.5 and × 1.5; window all vs latest 5; incurred instead of paid; +2% a year calendar inflation on future payments; thresholds ±10pp.
- Calibration on `panel_stochastic`: per insurer run the custom bootstrap with automatic rules; F_k = empirical CDF of simulated future payments to lag 10; y_k = actual 2008–16 payments to lag 10; p_k = F_k(y_k) (mid-rank for ties). Report one-sided coverage of the 75th percentile (`cov_q75_panel`), two-sided 50/75/90/95% coverage, p-p plot, KS D with critical 1.36/sqrt(n). Repeat with package ODP and Mack (lognormal with Mack mean and SE) for comparison with Meyers (2019): commercial auto, 1988–97 data, Mack incurred KS 16.4 vs critical 19.2; ODP paid 23.1, rejected, biased high.
- Interpretation rule: coverage below nominal means the distribution is biased low or too narrow; use the p-p shape and two-sided coverage to tell which. Raw coverage is the base result; widening (7.2) is a sensitivity only.
- `cv_numbers.json` keys: `n_panel_stochastic`, `cov_q75_panel`, `n_sims`.
- Tests: Mack golden test (section 11); custom bootstrap with all AYs set to CL matches `BootChainLadder` mean and SD within ±2%; its SD compared with Mack SE (report, no tolerance); spec hash recorded equals the tagged spec.
- Teach-back: process vs parameter vs model risk; Mack vs bootstrap; why the package bootstrap is not the selected reserve's distribution; what 58% coverage would mean.

**Gate 5.** Update the CV to v2.

### M7 Illustrative net IFRS 17 LIC

- Cash flows: from each simulated path (with tail), claims by calendar year × (1 + ULAE 5%), paid mid-year.
- Discount with the GSW curve (section 8): X^(s) = Σ_t CF_t^(s) · DF(t − 0.5).
- BE = mean of X; RA_p = Q_p(X) − BE for p = 65, 75, 85, 90%; LIC = BE + RA_75.
- Output `outputs/lic_2007.csv`: undiscounted BE, discount effect, discounted BE, RA_75, LIC, RA/BE, plus the other confidence levels; sensitivities ULAE 3%/7%, curve +50 bp. Disclosure sentence: "The risk adjustment corresponds to a 75% confidence level."
- Tests: zero curve gives discounted BE = undiscounted BE; RA increasing in p; toy discounting check (section 11).
- Teach-back: LIC vs LRC; why RA is not the 75th percentile; why discount each path first; why "illustrative net".

### M8 One-year reserve deterioration

```
CDR = R_2007 - (X_2008 + R_2008)        L = -CDR
one-year measure = Q_99.5%(L) / BE_2007 (discounted, net, incl. ULAE)
```

- Analytic benchmarks: `CDR(MackChainLadder(paid, est.sigma = "Mack"))` (Merz–Wüthrich; pure CL, no tail) and `CDR(BootChainLadder(...))`.
- Simulate–re-reserve: for each of 10,000 replicates take the simulated 2008 diagonal, append it, rerun the locked rules at 2008 (LDFs, leave-one-out prior, tail by the locked method, thresholds on the new z) → R_2008^(s) for AY 1998–2007 with tail. R_2007 = locked selected reserve. Compute L undiscounted (compare with Merz–Wüthrich) and discounted on the 2007 curve (compare with the standard formula).
- Standard formula: 3 × 9% = 27% of net BE (reserve risk only, single segment). Optional USP method 2: σ_USP = c · sqrt(MSEP(CDR))/R + (1 − c) · 9%, c = 74%.
- One-year back-test: percentile of the main insurer's actual 2008 CDR (from M5 Test D) in the simulated distribution; the same for `panel_stochastic` (p-p plot).
- Output `outputs/one_year_cdr_sims.csv`: `sim_id, paid_2008, reserve_2008, cdr, l_over_be` (L ÷ opening discounted net BE). Key `l995_over_be`.
- Tests: MW2008 golden test (section 11); with all AYs pure CL and no tail, simulated CDR mean ≈ 0 (within 3 simulation SEs) and SD within 10% of the Merz–Wüthrich SE.
- Teach-back: what does a negative CDR mean? Why is the one-year SE usually smaller? Why is this not an SCR? Where does 27% come from?

**Gate 6.**

### M9 Outputs

- `reports/reserving_memo.qmd` → PDF, English, 3 pages: 1 Executive reserve review; 2 Evidence & uncertainty; 3 Validation & limitations (post-reveal addendum). Structured like an Irish ARTP (methods and key assumptions, data quality, key uncertainties).
- Quarto site (GitHub Pages): first screen shows only the selected reserve, the blind back-test (three-way chart), uncertainty and calibration, the one-year measure / IFRS bridge; link to the memo; details on later pages.
- README: business question, data credit and disclaimer, how to reproduce (`make all`), lock tag date.
- `outputs/cv_numbers.json`: every number in the CV, README and memo with the git hash that produced it.
- Tests: `make all` from a clean clone reproduces `cv_numbers.json`; every number in README and memo found in `outputs/`.

**Final.** Update the CV to v3.

## 10. Repository

```
gi-reserving/
  README.md  SPEC.md  CONTRACT.md  Makefile  renv.lock
  config/    selection_rules.yaml, assumptions_2007.yaml, stochastic_spec.yaml,
             seeds.yaml, solvency2.yaml
  data/      raw/ (csv + MANIFEST), upper/, holdout/, market/ (feds200628.csv)
  R/         01_load.R 02_checks.R 03_triangles.R 04_diagnostics.R 05_deterministic.R
             06_select_lock.R 07_backtest.R 08_mack_bootstrap.R 09_selected_bootstrap.R
             10_panel_validation.R 11_ifrs17_lic.R 12_one_year_risk.R reveal.R utils.R
  tests/     testthat/ (one file per module), fixtures/toy_triangle.csv
  excel/     reserving_check.xlsx
  outputs/   selection_2007.csv, methods_2007.csv, backtest_main.csv, lic_2007.csv,
             one_year_cdr_sims.csv, cv_numbers.json, figures/
  reports/   data_checks.md, diagnostics.md, post_lock_changes.md, reserving_memo.qmd
  site/      Quarto website
```

## 11. Golden values and fixtures

| Test | Expected |
|---|---|
| `MackChainLadder(GenIns, est.sigma = "Mack")` | total IBNR 18,680,856; total Mack S.E. 2,447,095 (rounded) |
| `CDR(MackChainLadder(MW2008, est.sigma = "Mack"))` | total CDR(1) S.E. 81,081; total Mack S.E. 108,401 (rounded) |

Toy triangle (`tests/fixtures/toy_triangle.csv`; USD m; lag 4 = ultimate, tail 1):

| AY | 12 | 24 | 36 | 48 | Premium | Case incurred 2007 |
|---|---|---|---|---|---|---|
| 2004 | 10.0 | 22.0 | 32.0 | 40.0 | 60 | 40 |
| 2005 | 11.0 | 24.5 | 35.0 | | 64 | 41 |
| 2006 | 12.5 | 27.0 | | | 68 | 40 |
| 2007 | 14.0 | | | | 72 | 30 |

Expected (tolerance 0.01): f = 2.19403, 1.44086, 1.25; CDF 12 m = 3.95161; z = 25.31%, 55.52%, 80.00%, 100%; CL ultimates 40.00, 43.75, 48.63, 55.32 (total 187.70); Cape Cod ELR 69.39%; leave-one-out ELR 70.91%, 69.84%, 68.77%, 68.48%; BF 40.00, 43.94, 47.80, 50.83 (182.57); Cape Cod 40.00, 43.88, 47.99, 51.32 (183.18); Benktander 40.00, 43.79, 48.26, 51.96 (184.01); selection CL, CL, Benktander, BF → ultimate 182.84, unpaid 66.84, case 35.00, IBNR 31.84; allocated payments 2008/2009/2010 = 35.35 / 21.63 / 9.86; PV at flat 4% annual, mid-year = 63.99, × 1.05 ULAE = 67.19.

## 12. Output and labelling rules

- Figures: three-way back-test; rolling reserve path; panel error distribution; tornado; calibration p-p plot; one-year L distribution with the 27% line. Each states "US commercial auto, net, NAIC Schedule P" and the valuation date.
- No insurer names anywhere; NAIC codes only.
- Every public number comes from `outputs/`, written by the pipeline. No hand-typed numbers in the README, memo or CV.

## 13. Limitations to state

US business in USD at 2007; net of reinsurance (no gross or reinsurance-held IFRS 17 measurement); no claim counts, ULAE or rate changes; back-test only to 120 months, so the tail is untested; one vintage (AY 1998–2007), results not extrapolated; survivorship bias from the CAS selection of insurers with complete data; IFRS 17 did not exist in 2007 (method demonstration); one-year measure covers reserve risk only and is not an SCR; ELR prior CV of 10% and ULAE 5% are judgements.

## 14. Change log

- **v1.1 (2026-09-28, M0).** (a) The December 2025 CAS file uses different column names from §4.1: `IncurredLosses` (was `IncurLoss_C`), `CumPaidLoss`, `BulkLoss`, `EarnedPremDIR`, `EarnedPremCeded`, `EarnedPremNet` (no `_C` suffix), plus `Single` and `PostedReserves2007`. The CAS definitions are unchanged ("incurred losses and allocated expenses reported at year end", etc.), so the mapping is one-to-one and `01_load.R` renames them to the §4.1 names. (b) The file has 157 insurers; 137 have all 100 cells and 20 are missing whole accident years (contrary to §5.9). M1 drops the 20 because their upper triangles are incomplete, so no lower-triangle value is read. (c) CRAN's current ChainLadder is 0.2.21, not 0.2.22; pinned in `renv.lock`. (d) Raw and market data are not committed (third-party files); `data/raw/MANIFEST` holds SHA-256 hashes and `make all` verifies them. (e) The pipeline driver is `R/run.R` (called by the Makefile); module code stays in the §10 files.
- **v1.2 (2026-09-28, M1, before any estimation).** The §6 panel rules gave only 17 insurers in `panel_backtest` and 10 in `panel_stochastic` (the placeholders [140]/[112] in the guide assumed a larger usable file). Most insurers are small: the median of each insurer's smallest AY net premium is about 570 (USD thousands), and many have zero payments at late lags. Changes, decided on upper-triangle counts only: the 5,000 floor applies to the main insurer only; panels require `prem_net > 0` for every AY (M5 reports error by size tercile); `panel_stochastic` drops "every development column sum of incremental paid > 0" (zero-mean cells get no process noise under §7.2). Main (1767) and backup (2135) are unchanged. With samples of this size, calibration results are indicative; say so in the memo.
