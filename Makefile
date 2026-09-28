# make all: raw CSV -> outputs (pre-lock stages; post-lock stages are added after the tag).
R := Rscript

.PHONY: all load checks diagnostics deterministic select excel uncertainty lic one_year memo test
all: load checks diagnostics deterministic select excel uncertainty lic one_year memo test

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
test:
	$(R) -e 'testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)'

memo:
	cd reports && RENV_PROJECT=$(CURDIR) quarto render reserving_memo.qmd
