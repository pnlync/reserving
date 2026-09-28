# General Insurance Reserving & Reserve Risk Model

A blind reserving study of a US commercial auto insurer (NAIC 1767) at **31 December 2007**, built from NAIC Schedule P paid and incurred triangles. The reserve selection was locked in git (tag `selection-locked`) before the 2008–2016 run-off was revealed and used to score it. The same rules were then run across dozens of insurers to test whether the model's stated uncertainty held up.

**Site:** https://pnlync.github.io/reserving/ · **Memo (3 pages):** [reports/reserving_memo.pdf](reports/reserving_memo.pdf) · **Build contract:** [SPEC.md](SPEC.md), [CONTRACT.md](CONTRACT.md)

## Business questions and answers

| Question | Answer |
|---|---|
| How much should the insurer hold for unpaid claims at 2007-12-31? | USD 365.4m net (case 117.0m, IBNR 248.4m) |
| How does that compare with what it booked? | 44.4% above the booked USD 253.1m |
| How good was the locked estimate? | Projected payments to 120 months fell 10.3% short of actual (pure chain ladder: 15.6% short). Against 120-month incurred, the selection was 11.2% low vs 16.6% for chain ladder and 38.6% for the insurer's booked estimate. |
| Did the stated uncertainty hold up? | Across 45 insurers the bootstrap's 75th percentile covered 66.7% of outcomes (package ODP and Mack: 57.8%); intervals too narrow. |
| Illustrative net IFRS 17 LIC? | USD 375.2m (BE 360.6m + risk adjustment 14.7m at a 75% confidence level) |
| One-year reserve deterioration? | 1-in-200: 12.0% of opening BE vs 27% under the Solvency II standard formula (reserve risk only; not an SCR) |

Reserving is the subject; IFRS 17 and Solvency II are application layers. Every number above is written by the pipeline to `outputs/cv_numbers.json` with the git hash that produced it.

## Method in one paragraph

Paid and case-incurred chain ladder, BF with a leave-one-out Cape Cod prior, Cape Cod and Benktander, a log-linear tail. Methods were selected by accident year with a rule on percent developed (CL at 70% or more, Benktander at 30–70%, BF below 30%) and one documented exception (incurred chain ladder for mature years, based on a pre-lock pseudo-historical test). Uncertainty comes from Mack, the ChainLadder ODP bootstrap and a custom ODP bootstrap of the selected reserve itself. The illustrative net LIC discounts the simulated cash flows on the GSW US Treasury curve. The one-year measure re-reserves 10,000 simulated years with the locked rules, benchmarked against Merz–Wüthrich. An Excel workbook (`excel/reserving_check.xlsx`) independently rebuilds the LDFs, chain ladder, BF, Cape Cod and the Mack total standard error.

## Hindsight firewall

- Only `R/01_load.R` reads the raw file; `reveal()` refuses to read the 2008–2016 holdout unless the tag `selection-locked` exists. A static test enforces this.
- Selection rules and scoring formulas were committed before any analysis; the selection, assumptions and stochastic spec were locked on 2026-09-28. Scoring reads the selection from the tag.
- Rule changes before the lock, and modelling decisions after it, are logged in SPEC.md §14.

## Reproduce

1. Download the two public files listed in `data/raw/README.md` (CAS Schedule P commercial auto, Federal Reserve GSW yield curve). The loader checks the CAS file's SHA-256 against `data/raw/MANIFEST`.
2. `R -e 'renv::restore()'` (R 4.5; LibreOffice for the Excel recalculation check; Quarto for the memo and site).
3. `make all` rebuilds everything from the raw CSV to `outputs/cv_numbers.json`, the memo, the site and the tests (about 6 minutes). A clean clone reproduces all 41 numbers in `cv_numbers.json` exactly.

## Limitations

US business in USD at 2007; net of reinsurance; no claim counts, ULAE or rate changes; back-test only to 120 months (tail untested); one vintage and small panels; survivorship bias from the CAS selection; IFRS 17 did not exist in 2007; the one-year measure covers reserve risk only.

## Data and disclaimer

> Data: CAS Loss Reserving Database (NAIC Schedule P), compiled by S&P Global Market Intelligence for the Casualty Actuarial Society. Net of reinsurance, US business. The IFRS 17 and Solvency II figures are illustrative methodology demonstrations, not accounting or regulatory numbers for any company. Insurers are identified by NAIC code only.

