# Data checks (M1)

US commercial auto, net, NAIC Schedule P (CAS December 2025 file), valuation 2007-12-31. Amounts in USD thousands (Schedule P is reported with 000 omitted; the CAS page does not state units, and premium magnitudes are consistent with thousands). All checks below use the upper triangle (cy <= 2007) only, except the row counts and the cy identity, which need no lower-triangle values.

## Checks

| Check | Rule | Result | Action |
|---|---|---|---|
| Completeness | 100 cells per insurer | 157 insurers in file; 137 have 100 cells; 20 are missing whole accident years (20/30/40/50/60/70/80/90 rows) | Dropped: their upper triangles are incomplete (fewer than 55 cells) |
| Identities | cy = ay + lag - 1 | 0 failures in the full file | none needed |
| Identities | premium constant across lags within an AY | 0 of 137 complete insurers fail | investigate if any |
| Negative incremental paid | count by insurer | 77 insurers have at least one; 187 cells in total | flagged, kept as reported (never deleted or floored) |
| Case reserves | case_inc - paid >= 0 | 41 insurers have negative case reserves in at least one cell; 102 cells | counted; affects trust in incurred methods |
| Jumps | cumulative paid ratio outside [0.9, 3] (lag 2+) | 58 insurers flagged; 170 cells | flagged; large 24/12 ratios are normal for immature lags |
| Premium stability | adjacent-AY prem_net change > 50% | 47 insurers flagged | excluded from panels by rule |
| Ceded share | prem_ceded / prem_dir | median of insurer maxima 27.5%; 54 insurers above 50% | excluded from panels by rule |

## Selection (config/selection_rules.yaml, frozen before analysis)

| Rule | Insurers failing (of all in file) |
|---|---|
| 55 upper cells | 20 |
| prem_net >= 5,000 every AY (main only) | 128 |
| prem_net > 0 every AY (panels) | 53 |
| adjacent prem_net change within 50% | 92 |
| paid decreases in at most 2 cells | 45 |
| negative case reserve in at most 2 cells | 35 |
| ceded share <= 50% | 74 |

- Eligible for main (all main rules): **17** insurers.
- `panel_backtest`: **47** insurers.
- `panel_stochastic` (provisional; the bootstrap criterion is completed in M6): **45** insurers.
- Main insurer: NAIC code **1767** (largest AY 2007 net earned premium among eligible insurers, 370,607.0). Backup: **2135** (235,493.0).

Rule change v1.2 (before any estimation, upper-triangle counts only): the frozen v1.0 panel rules left 17 insurers in `panel_backtest` and 10 in `panel_stochastic`, because most insurers in the file are small (median of each insurer's smallest AY net premium about 570) and many have no payments at late lags. The 5,000 floor now applies to the main insurer only (panels need prem_net > 0, and M5 reports error by size tercile), and the stochastic rule 'every column sum of incremental paid > 0' is dropped (zero-mean cells get no noise, per the stochastic spec). The main insurer and backup are unchanged. See SPEC §14.

Note: the rule 'paid decreases in at most 2 cells' and the stochastic-panel rule 'negative incremental paid in at most 2 cells' differ only at lag 1 (a negative lag 1 payment), which the stochastic panel also excludes through 'paid at lag 1 > 0'.

Survivorship: CAS kept insurers that kept filing with consistent data; insurers that left the market are absent, so method performance may look better than it would on all insurers.

Full panel flags: `outputs/panels.csv`. Insurer names never appear in outputs.
