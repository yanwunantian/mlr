library(testthat)
if (identical(Sys.getenv("TRAVIS"), "true") || identical(Sys.getenv("R_EXPENSIVE_TEST_OK"), "true")) {
  R.utils::gcDLLs()
  test_check("mlr", "_learners_all_general")
}

