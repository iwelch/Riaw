# Extracted from test-parallel.R:49

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "iaw", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
skip_on_os("windows")
expect_message(iaw$mc.by.cripple.toggle())
