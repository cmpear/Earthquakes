context("Check whether geom_timeline functions are working")

test_that("checking whether geom_timeline functions are working", {
  #only testing exported functions, not unexported helper functions.  Want to test results, not ways of doing things.
  temp3<-tremors %>%
    dplyr::filter(YEAR >= 2000 ) %>%
    dplyr::filter(COUNTRY %in% c('CHINA','JAPAN','KOREA')) %>%
    dplyr::select(DATE, COUNTRY, LOCATION_NAME, EQ_PRIMARY, DEATHS) %>%
    dplyr::mutate(COUNTRY = as.factor(COUNTRY)) %>%
    ggplot(aes(x = DATE, y = COUNTRY, labels = LOCATION_NAME, size = EQ_PRIMARY, col = DEATHS, n_max = 4)) +
      Earthquakes::geom_timeline() + Earthquakes:::geom_timeline_label()
  testthat::expect_s3_class(temp3, 'gg')
  # this also tests draw_timeline_panel(), which can only be called from geom_timeline()
  temp3 <- temp3 + geom_timeline_label(aes(labels = LOCATION_NAME))
  testthat::expect_s3_class(temp3,'ggplot')
})
