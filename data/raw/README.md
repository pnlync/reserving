# Raw data (not committed)

1. CAS "Loss Reserving Data Pulled from NAIC Schedule P" (December 2025 version), Commercial Auto file `comauto_pos_98-07.csv`, from
   https://www.casact.org/publications-research/research/research-resources/loss-reserving-data-pulled-naic-schedule-p
   → save here as `data/raw/comauto_pos_98-07.csv`.
2. Federal Reserve GSW nominal yield curve `feds200628.csv`, from https://www.federalreserve.gov/data/nominal-yield-curve.htm
   → save as `data/market/feds200628.csv`.

`make all` checks both files against the SHA-256 in `MANIFEST`. Data compiled by S&P Global Market Intelligence for the Casualty Actuarial Society.
