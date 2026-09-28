# make all: raw CSV -> outputs. Post-reveal stages (backtest, calibration, one_year_backtest)
# need the git tag selection-locked; reveal() stops without it.
R := Rscript


.PHONY: cv_numbers site
.PHONY: all prelock load checks diagnostics deterministic select excel uncertainty lic one_year backtest calibration one_year_backtest memo test
prelock: load checks diagnostics deterministic select excel uncertainty lic one_year
all: prelock backtest calibration one_year_backtest cv_numbers memo site test

load:
	$(R) R/run.R load
checks:
	$(R) R/run.R checks
diagnostics:
	$(R) R/run.R diagnostics
deterministic:
	$(R) R/run.R deterministic
select:
	$(R) R/run.R select
excel:
	$(R) R/run.R excel
uncertainty:
	$(R) R/run.R uncertainty
lic:
	$(R) R/run.R lic
one_year:
	$(R) R/run.R one_year
backtest:
	$(R) R/run.R backtest
calibration:
	$(R) R/run.R calibration
one_year_backtest:
	$(R) R/run.R one_year_backtest
test:
	$(R) -e 'testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)'

memo:
	cd reports && RENV_PROJECT=$(CURDIR) quarto render reserving_memo.qmd
cv_numbers:
	$(R) R/run.R cv_numbers

site:
	cd site && RENV_PROJECT=$(CURDIR) quarto render
