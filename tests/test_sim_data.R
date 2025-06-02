source(here::here("R", "sim_data.R"))

test_that("I get some sim data", {
  df <- day_sim(n = 300, time_to_subscribe = 180, time_to_leave = 90,
                today = as.Date("2025-01-01"))
  expect_true(is.data.frame(df))
  expect_true(count_subscribers(df, as.Date("2025-03-01")) <
                count_users(df, as.Date("2025-03-01")))
})
