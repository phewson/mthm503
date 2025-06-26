library(testthat)
source(here::here("R", "regression.R"))

test_that("get_datetime returns correct POSIXlt object", {
  dt <- get_datetime("01/01/2020", 13)
  expect_s3_class(dt, "POSIXlt")
  expect_equal(format(dt, "%Y-%m-%d %H:%M:%S"), "2020-01-01 13:00:00")
})

test_that("get_datetime pads single-digit hour correctly", {
  dt <- get_datetime("15/06/2020", 7)
  expect_equal(format(dt, "%H:%M:%S"), "07:00:00")
})

test_that("get_datetime handles invalid date string format", {
  expect_warning(result <- get_datetime("2020-06-15", 12), regexp = NA)
  expect_true(is.na(result))
})

test_that("get_datetime warns for invalid hour input", {
  expect_warning(result <- get_datetime("01/01/2020", 25),
                 paste("One or more entries in 'hour' ",
                       "are invalid \\(not between 0 and 23\\).", sep = ""))
  expect_true(is.na(result))
})

test_that("get_time_factors creates correct columns", {
  df <- data.frame(
    datetime = as.Date(c("2020-01-01 13:00:00", "2020-01-02 14:00:00")),
    hr = c(13, 14)
  )
  out <- get_time_factors(df)

  expect_true(all(c("doy", "hour", "dow", "datetime_day") %in% colnames(out)))
  expect_equal(out$doy, c(1, 2))
  expect_equal(out$hour, c(13, 14))
  expect_equal(levels(out$dow)[out$dow], weekdays(df$datetime))
  expect_equal(out$datetime_day[1], 0)
})

test_that("get_time_factors handles NA values", {
  df <- data.frame(
    datetime = c(as.POSIXct("2020-01-01 13:00:00"), NA),
    hr = c(13, NA)
  )
  out <- get_time_factors(df)

  expect_true(is.na(out$datetime_day[2]))
  expect_equal(out$datetime_day[1], 0)
})
