
message("Loading devtools")
# devtools::test()

library(testthat)
# library(Riaw)

testthat::set_max_fails(Inf)
test_dir("testthat/")
