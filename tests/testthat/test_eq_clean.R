context("testing data cleaning functions")
test_that("See whether cleaning functions produce same data as tremors", {
  #only testing exported functions, not unexported helper functions.  Want to test results, not ways of doing things.
  temp <- eq_get_data_raw()
  temp2 <- eq_get_data()
  testthat::expect_identical(temp %>%
                               eq_clean_data() %>%
                               eq_location_clean(), temp2)
  testthat::expect_identical(temp2, tremors)

# are dates before 1000 ad working?
  testthat::expect_less_than(as.integer(tremors[1,'DATE']), as.integer(lubridate::ymd('1000,01,01')))

})
