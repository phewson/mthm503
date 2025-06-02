
day_sim <- function(n, time_to_subscribe, time_to_leave,
                    today) {
  x <- today + floor(rexp(n, 1 / time_to_subscribe))
  y <- today + floor(rexp(n, 1 / time_to_leave))
  z <- ifelse(y < x, y, NA)
  data.frame(user_starts = today, user_leaves = x,
             user_subscribes = as.Date(z))
}

count_users <- function(df, target_date) {
  length(df[df$user_starts <= target_date &
              df$user_leaves > target_date, 1])
}

count_subscribers <- function(df, target_date) {
  length(df[df$user_subscribes <= target_date &
              df$user_leaves > target_date, 1])
}
