library(dplyr)

age_bands <- c("0-16", "17-24", "25-39", "40-64", "65+", "Unknown")
financial_years <- c("2010/11", "2011/12", "2012/13", "2013/14", "2014/15",
                     "2015/16", "2016/17", "2017/18", "2018/19", "2019/20")


clean_extrications <- function(df) {
  df %>%
    mutate(extrication = as.factor(.data$extrication),
           sex = as.factor(.data$sex),
           age_band = factor(.data$age_band, levels = age_bands),
           financial_year = factor(.data$financial_year,
                                   levels = financial_years,
                                   ordered = TRUE))
}
