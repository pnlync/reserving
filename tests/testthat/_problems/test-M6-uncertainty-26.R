# Extracted from test-M6-uncertainty.R:26

# test -------------------------------------------------------------------------
up <- as_at(load_upper(), 2007)
tr <- insurer_triangles(up, main_insurer())
own <- selected_bootstrap(tr$paid, tr$premium, R = 10000, seed = 101, window = "all",
                            method = rep("CL", 10), prior_cv = 0, tail = FALSE)
set.seed(102)
pkg <- ChainLadder::BootChainLadder(ChainLadder::as.triangle(tr$paid), R = 10000, process.distr = "gamma")
expect_lt(abs(mean(own$reserve) / mean(pkg$IBNR.Totals) - 1), 0.02)
expect_lt(abs(stats::sd(own$reserve) / stats::sd(pkg$IBNR.Totals) - 1), 0.02)
