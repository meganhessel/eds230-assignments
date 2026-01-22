#' walgrape_function
#'
#' Computes almond temp anomalies given January precipitation avg and February minimum temperature avgs 
#' @param tmin_feb dataframe of February minimum temperature averages 
#' @param precip_jan dataframe of January precipitation averages 
#' @param yr dataframe of years 
#' @param graph TRUE/FALSE  graph results default=TRUE

winegrape_function <- function(almond_df, graph = TRUE) {
  
  # Getting input dfs 
    # Pulling out just Feb tmin 
    tmin_april <- almond_df %>% 
      filter(month == 4) %>% 
      filter(year < 2010) %>% 
      select(tmin)
    
    # Pullung out just January precipitation 
    precip_june <- almond_df %>% 
      filter(month == 6) %>% 
      filter(year < 2010) %>% 
      select(precip)
    
    # Pullung out just January precipitation 
    precip_sept_pre <- almond_df %>% # no 2010 in this df 
      filter(month == 9) %>% 
      filter(year < 2010) %>% 
      select(precip)
  
    
    # Creating df of years 
    year <- almond_df %>% 
      filter(month == 1) %>% 
      filter(year < 2010) %>% 
      select(year)
  
  
  # Equation 
  Y = 2.56 * (tmin_april) -0.17 * (tmin_april)^2 + 4.78 * (precip_june) - 4.93 * (precip_june)^2 - 2.24 * (precip_sept_pre) + 1.54 * (precip_sept_pre)^2 - 10.50
  
  
  # Create a dataframe
  winegrape_anomaly <- 
    data.frame(year = year, 
               yield_anomaly = Y)
  # Change column names (`data.frame` function was not changing the colnames)
  colnames(winegrape_anomaly) <- c("year", "yield_anomaly")
  
  
  # Plot 
  g <- ggplot(winegrape_anomaly, aes(x = year, y = yield_anomaly)) + 
    geom_point() +
    geom_line() + 
    labs(
      x = "Year", 
      y = "Yield Anomalies", 
      title = "Wine Grape Yield Anomalies"
    )
  
  # max, min, and mean values 
  checking_work <- data.frame(max_almond_yield = max(winegrape_anomaly$yield_anomaly),
                              min_almond_yield = min(winegrape_anomaly$yield_anomaly), 
                              mean_almond_yield = mean(winegrape_anomaly$yield_anomaly))
  
  table <- 
    kable(checking_work, 
          col.names = c("max", "min", "mean"), 
          digits = 2,
          caption = "Wine Grape Yield Characteristics"
    )
  #checking_work %>% 
  #kbl(col.names = c("max", "min", "mean")) %>% 
  #kable_styling(bootstrap_options = c("striped", "hover", "condensed")) 
  
  # Create kabel table of predictions
  #kabel <- almond_anomaly %>% 
  #kbl(col.names = c("Year", "Yield Anomaly")) %>% 
  #kable_styling(bootstrap_options = c("striped", "hover", "condensed")) 
  
  
  # RETURN If statement 
  if (graph == "TRUE") { # print graph and data set 
    return(list(
      data = winegrape_anomaly, 
      table = table,
      plot = g 
    ))
  } else {  # Print just data set
    return(winegrape_anomaly)
  }
  
  
}


