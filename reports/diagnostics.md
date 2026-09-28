# Diagnostics (M2) — main insurer NAIC 1767

US commercial auto, net, NAIC Schedule P, as at 2007-12-31, upper triangle only. USD thousands. Sources: `outputs/m2_ldf_paid.csv`, `outputs/m2_ldf_incurred.csv`, `outputs/m2_cl_paid_vs_incurred.csv`, `outputs/m2_tests.csv`, `outputs/m2_inflation.csv`; charts `outputs/figures/m2_paid_incurred_ratio.png`, `m2_ata_paid.png`, `m2_mack_residuals_paid.png`.

**Book.** Net earned premium grows from 245m (AY 1998) to 371m (AY 2007), with adjacent-year changes of at most 11%. The triangles are complete and smooth, with no negative paid increments and no negative case reserves.

## 1. Paid or incurred?

The paid ÷ case incurred ratio is stable by AY at each lag: 0.81–0.83 at lag 2 and 0.88–0.91 at lag 3 (`m2_paid_incurred_ratio.png`). There is no rising trend (which would suggest faster settlement) and no falling trend (slower settlement or stronger case reserves). The only drift is a small dip at lag 3 for AY 2004–2005 (0.880, 0.891 vs 0.90–0.91 before). Paid and incurred chain ladder agree within 2.1% for every AY (`m2_cl_paid_vs_incurred.csv`; largest gap AY 2000, −2.0%), so no AY triggers the 10% flag. **Decision: paid is the base triangle** (objective, and its pattern drives the cash flows in M5 and M7); incurred is a cross-check.

## 2. Recent vs early development

Mid-lag paid factors have risen with AY. The 3–4 factor goes from 1.13 (AY 1998–2001) to 1.158, 1.166 and 1.182 (AY 2002–2004), and the 4–5 factor from 1.065–1.068 to 1.076 and 1.087 (`m2_ata_paid.png`). The case incurred factors rise in the same way (3–4: 1.07–1.08, then 1.10–1.125), so this is heavier development in both triangles, not a paid-only timing shift. Volume-weighted latest-3 factors exceed all-year factors at every lag up to 5 (paid 3–4: 1.169 vs 1.147). The Mack residuals trend upward by origin period and are positive on the 2004–2006 diagonals (`m2_mack_residuals_paid.png`). **Decision: recent development is heavier than early development**, and the LDF window should weight recent diagonals (section 5).

## 3. Outlier factors

AY 2005 2–3 (`m2_ata_paid.png`) is high on both paid (1.399 vs 1.30 volume-weighted) and incurred (1.275 vs 1.18). AY 2000 1–2 paid is 1.869 (incurred 1.444, also high). AY 2006 1–2 paid is low (1.675), but incurred is normal (1.337). **Decision: keep all factors.** The high ones are confirmed by incurred, so they look like real experience rather than errors. Volume weighting already limits the influence of any single AY. The AY 2006 low paid factor is a timing effect that the incurred cross-check will show.

## 4. Calendar-year effects

`cyEffTest` does not reject for paid (Z = 12, 95% range 9.3–17.1) or incurred (Z = 10, range 8.9–16.1) (`m2_tests.csv`). The residual plot still shows the latest diagonals above zero, consistent with section 2. Premium-normalised paid (`checkTriangleInflation`) falls by about 2% a year at lags 1–4 (R² 0.20–0.45; `m2_inflation.csv`), i.e. premium grew faster than early payments (rate increases after 2001). Normalising by premium is not a claims-inflation measure, so this is only indicative. `dfCorTest` rejects independence of adjacent factors for paid (T = 0.284 vs ±0.127 at the 50% level); incurred is at the edge (0.128). So Mack's independence assumption is questionable: M6 treats the Mack SE as a benchmark, not as the distribution. **Decision: no explicit calendar-year adjustment.** The recent-window choice (section 5) and the +2% a year inflation sensitivity in M6 cover it.

## 5. LDF window

The all-year average dilutes the recent heavier mid-lag development with 1998–2001 experience; the latest-5 average keeps AY 2002 onwards where the rise is visible, and at lags 6–10 the two are identical (four or fewer AYs). **Decision: volume-weighted, latest 5 diagonals (`latest5`)**, to be confirmed by the M4 pseudo-historical A/E before the lock. For the main insurer the choice moves total paid CL ultimate by only +0.2% (1,847,388 → 1,850,621) (`m2_cl_paid_vs_incurred.csv`), because latest-5 is higher at lags 3–5 but lower at lag 2–3.

## Summary for `config/assumptions_2007.yaml` (draft)

| Item | Decision | Evidence |
|---|---|---|
| Base triangle | paid | §1 |
| LDF window | `latest5`, volume-weighted | §2, §5 |
| Factor exclusions | none | §3 |
| Calendar-year adjustment | none; +2% a year sensitivity in M6 | §4 |
| Mack assumptions | adjacent-factor independence questionable (dfCorTest) | §4 |
