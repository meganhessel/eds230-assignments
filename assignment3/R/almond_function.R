#' almond_function
#'
#' Computes almond temp anomalies given January precipitation avg and February minimum temperature avgs 
#' @param tmin_feb dataframe of February minimum temperature averages 
#' @param precip_jan dataframe of January precipitation averages 
#' @param yr dataframe of years 
#' @param graph TRUE/FALSE  graph results default=TRUE


almond_function <- function(almond_df, graph = TRUE) {
  
  # Getting input dfs 
    # Pulling out just Feb tmin 
    tmin_feb <- almond_df %>% 
      filter(month == 2) %>% 
      select(tmin)
    
    # Pullung out just January precipitation 
    precip_jan <- almond_df %>% 
      filter(month == 1) %>% 
      select(precip)
    
    # Creating df of years 
    year <- almond_df %>% 
      filter(month == 1) %>% 
      select(year)
  

  # Equation 
  Y = -0.015 * (tmin_feb) - 0.0046 * (tmin_feb)^2 - 0.07 * (precip_jan) + 0.0043 * (precip_jan)^2 + 0.28
  
  
  # Create a dataframe
  almond_anomaly <- 
    data.frame(year = year, 
               yield_anomaly = Y)
  # Change column names (`data.frame` function was not changing the colnames)
   colnames(almond_anomaly) <- c("year", "yield_anomaly")
  
   
  # Plot 
  g <- ggplot(almond_anomaly, aes(x = year, y = yield_anomaly)) + 
    geom_point() +
    geom_line() + 
    labs(
      x = "Year", 
      y = "Yield Anomalies", 
      title = "Almond Yield Anomalies"
    )
  
  # max, min, and mean values 
  checking_work <- data.frame(max_almond_yield = max(almond_anomaly$yield_anomaly),
                              min_almond_yield = min(almond_anomaly$yield_anomaly), 
                              mean_almond_yield = mean(almond_anomaly$yield_anomaly))
    
  table <- 
    kable(checking_work, 
          col.names = c("max", "min", "mean"), 
          digits = 2,
          caption = "Almond Yield Characteristics"
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
      data = almond_anomaly, 
      table = table,
      plot = g 
    ))
  } else {  # Print just data set
    return(almond_anomaly)
  }


}
