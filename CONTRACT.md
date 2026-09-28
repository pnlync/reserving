# CONTRACT

The frozen short version of SPEC §4–5. Committed in M0, before any analysis code. Changing this file after the tag `selection-locked` is not allowed.

## 1. Valuation date and visible data

- Valuation date: **2007-12-31**. Data: CAS Schedule P, Commercial Auto (`comauto_pos_98-07.csv`, December 2025 version), net of reinsurance, USD thousands.
- Before the lock, analysis may read only cells with `cy = ay + lag - 1 <= 2007` (upper triangle), obtained through `as_at()`.
- Pseudo-historical valuations 2004, 2005 and 2006 use `as_at(d, v)` and may be compared with diagonals up to 2007 only.
- Holdout (`2008 <= cy <= 2016`) is read only by `reveal()`, which stops unless the tag `selection-locked` exists.
- Firewall: only `R/01_load.R` reads `data/raw/`; only `R/reveal.R` reads `data/holdout/`; estimation functions accept only objects created by `as_at()`; a static test enforces this.
- Exception: insurer selection needs the lower triangle to exist. Completeness is judged on upper cells only; insurers missing whole accident years are dropped in M1 because their upper triangle is incomplete.

## 2. Lock scope

Committed before the tag: `config/selection_rules.yaml` (M0), `outputs/selection_2007.csv`, `config/assumptions_2007.yaml`, `config/stochastic_spec.yaml`, memo page 1 and the first half of page 2. The tag message states total unpaid, the case/IBNR split and the difference from the insurer's booked unpaid. Scoring scripts read the selection with `git show selection-locked:outputs/selection_2007.csv`.

## 3. Scoring formulas (M5), fixed now

Notation: `C[i,j]` cumulative paid, `I[i,j]` booked incurred (`inc_booked`), `k_i` latest lag of AY i at 2007, `g_j = 1/CDF_j` from the locked selection (tail included; `g` after the tail = 1).

- **Allocation** of selected unpaid to future development years: `share(i,j) = (g_j - g_{j-1}) / (1 - g_{k_i})` for `j = k_i+1..10`, then the tail remainder `(1 - g_10)/(1 - g_{k_i})`. Predicted payment = `unpaid_sel_i * share(i,j)`.
- **Test A (headline)**, all AYs: `F̂_i` = predicted payments to lag 10; `F_i = C[i,10] - C[i,k_i]`; `e_A = (Σ F̂ - Σ F) / Σ F`.
- **Test B**, AY 1999–2007 only: `R_proxy = Σ (I[i,10] - C[i,k_i])`; `e_B = (R̂ - R_proxy) / R_proxy` for R̂ = selected unpaid, booked unpaid (`Σ I[i,k_i] - C[i,k_i]`), pure paid CL unpaid with tail. "Beat" means smaller `|e_B|`.
- **Test C**, AY 1998–2007: actual 2008 incremental paid ÷ predicted (share for `j = k_i+1`), by AY and total.
- **Test D**: for v = 2008..2015, rerun the locked rules mechanically on `as_at(d, v)` (automatic thresholds; prior and tail recomputed by the locked method); record the reserve path and actual CDR per year.
- **Panel** (`panel_backtest`): automatic pipeline, no manual exceptions, pure CL and rule; median `|e_A|`, error by maturity band and size tercile, share of insurers whose booked 2007 unpaid was below the proxy.
- **Calibration** (`panel_stochastic`): `p_k = F_k(y_k)` for future payments to lag 10 (mid-rank for ties); headline = share with `p_k <= 0.75`; also two-sided 50/75/90/95% coverage and KS D against `1.36/sqrt(n)`. Raw coverage is the base result; widening is a cross-validated sensitivity only.

## 4. Bug policy

After the tag, only pure code errors (e.g. an index off by one) may be fixed. Both the locked and corrected numbers are published, with the reason, in `reports/post_lock_changes.md`. Judgement choices (methods, ELR, tail, window, thresholds, stochastic spec) never change.
