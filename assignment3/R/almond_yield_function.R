#' almond_yield_function
#'
#' Computes almond temp anomalies given January precipitation avg and February minimum temperature avgs 
#' @param almond_df dataframe of almond min and max temperatures and precipitation over time #' @param precip_jan dataframe of January precipitation averages 


almond_yield_function <- function(almond_df) {
  # get T_n,2
  Tn2 <- almond_df %>%
    filter(month == 2) %>% # will filter out years that don't have data in Feb
    group_by(year) %>%
    summarise(avg_tmin_c = mean(tmin_c, na.rm = TRUE))
  
  # get P_1
  P1 <- almond_df %>%
    filter(month == 1) %>% # will filter out years that don't have data in Jan
    group_by(year) %>%
    summarise(sum_precip = sum(precip, na.rm = TRUE))
  
  # combine back to one df
  result <- merge(Tn2, P1, by = "year", all = TRUE)
  
  #............................YIELD ANOMALIES................................
  
  # get Yield
  almond <- result %>%
    mutate(yield_anomaly = if_else(
      is.na(avg_tmin_c) | is.na(sum_precip),
      NA_real_,
      (-0.015 * avg_tmin_c) + (-0.0046 * avg_tmin_c^2) +
        (-0.07 * sum_precip) + (0.0043 * sum_precip^2) + 0.28
    ))
  
  almond


} 