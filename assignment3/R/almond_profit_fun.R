#' almond_profit_fun 
#'
#' Computes almond temp anomalies given January precipitation avg and February minimum temperature avgs 
#' @param almond_df dataframe of almond min and max temperatures and precipitation over time  

almond_profit_fun <- function(almond_df) {
  
  #....................PULL OUT VARAIBLES FOR EQUATION........................ 
  # Pulling out Feb min temps  
  tmin_feb <- almond_df %>% 
    filter(month == 2) %>% 
    select(tmin)
  
  # Pulling out January precipitation 
  precip_jan <- almond_df %>% 
    filter(month == 1) %>% 
    select(precip)
  
  # Creating df of years 
  yr <- almond_df %>% 
    filter(month == 1) %>% 
    select(year)
  
  #...........................YIELD ANOMALY......................... 
  
  # Equation for Almond YIELD ANOMALY
  Y = -0.015 * (tmin_feb) - 0.0046 * (tmin_feb)^2 - 0.07 * (precip_jan) + 0.0043 * (precip_jan)^2 + 0.28
  
  
  # Create a dataframe of year and ANOMALY
  almond <- data.frame(year = yr, 
                       yield_anomaly = Y)
  
  # Change column names (`data.frame` function was not changing the colnames)
  colnames(almond) <- c("year", "yield_anomaly")
  
  
  #..............................PRICE............................. 

  # Dataframe of almond prices from 2024 California Almond Objective Measurement Report
  price_lb_df <- data.frame(year = c(1995:2023), 
                            price_lb = c(2.48, 2.08, 1.56, 1.41, 0.86, 0.97, 0.91, 1.11, 1.57, 2.21, 2.81, 2.06, 1.75, 1.45, 1.65, 1.79, 1.99, 2.58, 3.21, 4.00, 3.13, 2.39, 2.53, 2.50, 2.45, 1.71, 1.86, 1.40, 1.40))
  
  
  # Predicting prices of 1988:1994 with above data with an lm_model 
  lm_model <- lm(price_lb ~ year, 
                 data = price_lb_df)
  
  missing_years <- data.frame(year = 1988:1994) # dataframe of missing year 
  
  missing_years$price_lb <- predict(lm_model, newdata = missing_years) # predict price of missing years with `lm_model`
  
  price_lb_df <- rbind(missing_years, price_lb_df) # Make one dataframe of prices 
  
  # Convert price ($/lb) to price ($/ton) 
  price_lb_df$price_ton <- price_lb_df$price_lb * (1/0.005) ## 1lb = 0.005 tons
  
  # Add prices ($/ton) to almond df 
  almond <- left_join(almond, price_lb_df, by= "year") %>% 
    select(-c("price_lb")) # Remove price per lb 
  
  
  #..............................COST............................. 
  # Discount Rate function 
  cost_function <- function(t, base_price = 1569, discount_rate = 0.09) { 
    base_price / (1 + discount_rate)^(2024-t) } # base price and discount rates from paper 
  
  almond$cost <- cost_function(t = almond$year) # apply `cost_function` to almond years 
  
  
  #.............................PROFIT............................
  almond$yield = (0.9 + almond$yield_anomaly) # baseline + nomaly = yield (ton /acre)
  
  almond$revenue = almond$yield * almond$price_ton # yield (ton /acre) * price ($/ton) = rev ($/acre)
  
  almond$profit = almond$revenue - almond$cost # revenue ($/acre) - cost ($/acre) = profit ($/acre)
  
  almond # return entire df 
}