# Changes after the lock (tag `selection-locked`, commit b037a50)

Bug policy (CONTRACT §4): after the tag only pure code errors may be fixed, with both numbers published. Judgement choices never change.

## Code bugs fixed after the tag

None. The locked selection, assumptions and stochastic spec are identical to the tagged versions (checked by `tests/testthat/test-M6-uncertainty.R`).

## Disclosures

1. **Widening sensitivity: fitting criterion chosen after seeing the raw calibration.** The stochastic spec (§7.2) defines the widening factor λ and the split-half design but not the fitting criterion. The first run fitted λ to one-sided q75 coverage on the fit half; the fit half was already at 72.7%, so it returned λ = 1.00 (no widening). The raw results showed the intervals were too narrow rather than off-centre, so λ was refitted to minimise the gap between two-sided 50/75/90/95% coverage and nominal. That gave λ = 1.40, and on the held-out half the gap fell from 0.53 to 0.10 (q75 coverage 60.9% → 65.2%). This affects a sensitivity only; the headline raw coverage (66.7%) is unchanged. Both versions are recorded here.
2. **Pre-reveal work done before the tag existed.** The main-insurer bootstrap, the LIC and the one-year simulation were run before the owner created the tag, reading the lock-scope files from the working tree. Those files are identical to the tagged versions, so the results do not depend on this.
