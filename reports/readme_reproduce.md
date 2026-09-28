## Reproduce

1. Download the two public files listed in `data/raw/README.md` (CAS Schedule P commercial auto, Federal Reserve GSW yield curve). The loader checks the CAS file's SHA-256 against `data/raw/MANIFEST`.
2. `R -e 'renv::restore()'` (R 4.5; LibreOffice for the Excel recalculation check; Quarto for the memo and site).
3. `make all` rebuilds everything from the raw CSV to `outputs/cv_numbers.json`, the memo, the site and the tests (about 6 minutes). A clean clone reproduces all 41 numbers in `cv_numbers.json` exactly.
