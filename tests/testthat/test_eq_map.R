context("Check whether map files are working")

test_that("Checking whether map files are working", {
  #only testing exported functions, not unexported helper functions.  Want to test results, not ways of doing things.
  m<-tremors %>%
    dplyr::select(COUNTRY, LATITUDE, LONGITUDE, EQ_PRIMARY, DATE, YEAR, DEATHS) %>%
    dplyr::filter(COUNTRY == "MEXICO", YEAR >= 2000) %>%
    Earthquakes::eq_map(annot_col = "DATE")
  testthat::expect_s3_class(m, 'leaflet')
  testthat::expect_s3_class(m, 'htmlwidget')
  testthat::expect_identical(Earthquakes:::Bold('howdy'),'<b>howdy</b>')
})
