#' almond_profit_fun 
#'
#' Computes almond temp anomalies given January precipitation avg and February minimum temperature avgs 
#' @param almond_df dataframe of almond min and max temperatures and precipitation over time  
#' @param price the default is the average price from the 2024 California Almond Objective Measurement Report or user can input a list of prices in which the function will average 

almond_profit_function <- function(almond_df, prices = 398.7586) {
  
  #....................PULL OUT VARAIBLES FOR EQUATION........................
  
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
  
  # get Y
  almond <- result %>%
    mutate(
      yield_anomaly = if_else(
        is.na(avg_tmin_c) | is.na(sum_precip),
        NA_real_,
        (-0.015 * avg_tmin_c) + (-0.0046 * avg_tmin_c^2) + 
          (-0.07 * sum_precip) + (0.0043 * sum_precip^2) + 0.28
        )
      )
  
  #............................AVG PRICE................................... 
  
  # AVERAGE almond prices & Convert price ($/lb) to price ($/ton) 
  avg_price_ton <- mean(prices) * (1/0.005) ## 1lb = 0.005 tons
  
  almond$avg_price_ton <- avg_price_ton 
  
  #................................COST................................... 
  # Discount Rate function 
  cost_function <- function(t, base_price = 3807, discount_rate = 0.09) { 
    base_price / (1 + discount_rate)^(2024-t) } # base price and discount rates from UCAR 2024 paper 
  
  
  almond$cost <- cost_function(t = almond$year) # apply `cost_function` to almond years 
  
  #.............................PROFIT............................
  almond$yield = (0.9 + almond$yield_anomaly) # baseline + nomaly = yield (ton /acre)
  
  almond$revenue = almond$yield * almond$avg_price_ton # yield (ton /acre) * price ($/ton) = rev ($/acre)
  
  almond$profit = almond$revenue - almond$cost # revenue ($/acre) - cost ($/acre) = profit ($/acre)
  
  almond # return entire df 
  
  }
  
