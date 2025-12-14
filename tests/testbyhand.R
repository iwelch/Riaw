
message("Loading devtools")
# devtools::test()

library(testthat)
# library(Riaw)

testthat::set_max_fails(Inf)
invisible(test_dir("testthat/"))
