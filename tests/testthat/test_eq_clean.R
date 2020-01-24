context("testing data cleaning functions")
test_that("See whether cleaning functions produce same data as tremors", {
  #only testing exported functions, not unexported helper functions.  Want to test results, not ways of doing things.
  temp <- eq_get_data_raw()
  temp2 <- eq_get_data()
  # this also tests ymd_bc and RemoveBefore, which are called from eq_clean_data() and eq_location_clean() respectively
  testthat::expect_identical(temp %>%
                               eq_clean_data() %>%
                               eq_location_clean(), temp2)
  # tremors is a cleaned version of the dataset, so the clean functions should produce an equivalent dataset
  testthat::expect_identical(temp2, tremors)

# are dates before 1000 a.d. working?
  testthat::expect_less_than(as.integer(tremors[1,'DATE']), as.integer(lubridate::ymd('1000,01,01')))

})
