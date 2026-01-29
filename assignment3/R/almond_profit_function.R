#' almond_profit_function
#'
#' Computes almond temp anomalies given January precipitation avg and February minimum temperature avgs 
#' @param almond_df Dataframe of almond min and max temperatures and precipitation over time  
#' @param price The default is the average price from the 2024 California Almond Objective Measurement Report (2.48) or user can input a list of prices in which the function will find the average price. This input must be given in price per lb. 
#' @param discount_rate The discount rate of almond costs over time. The default is the discount rate from the paper (9%), but could be changed. 

almond_profit_function <- function(almond_df, prices = 2.48, discount_rate = 0.09) {
  
  #...............PULL ALMOND YEILD FROM `almond_yield_function`.................
  
  almond <- almond_yield_function(almond_df)
  
  #............................AVG PRICE................................... 
  
  # AVERAGE almond prices & Convert price ($/lb) to price ($/ton) 
  avg_price_ton <- mean(prices) * (2000) # 1lb = 0.0005 tons
  
  almond$avg_price_ton <- avg_price_ton 
  
  #................................COST................................... 
  
  base_price <- 3807
  almond$cost <- base_price / (1 + discount_rate)^(2024 - almond$year) # Discount rate equation to find cost in the years before 2024
  
  #.............................PROFIT............................
  almond$yield = (0.9 + almond$yield_anomaly) # baseline + anomaly = yield (ton /acre)
  
  almond$revenue = almond$yield * almond$avg_price_ton # yield (ton /acre) * price ($/ton) = rev ($/acre)
  
  almond$profit = almond$revenue - almond$cost # revenue ($/acre) - cost ($/acre) = profit ($/acre)
  
  almond # return entire df 
  
  }
  
