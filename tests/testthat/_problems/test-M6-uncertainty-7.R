# Extracted from test-M6-uncertainty.R:7

# test -------------------------------------------------------------------------
m <- suppressMessages(ChainLadder::MackChainLadder(ChainLadder::GenIns, est.sigma = "Mack"))
ibnr <- sum(m$FullTriangle[, 10] - ChainLadder::getLatestCumulative(m$Triangle))
expect_equal(round(ibnr), 18680856)
expect_equal(round(m$Total.Mack.S.E), 2447095)
