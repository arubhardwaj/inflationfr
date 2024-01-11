library(tidyverse)

cpi <- read_csv("../data/FRACPIALLMINMEI.csv")
interest_rates <- read_csv("../data/IRSTCI01FRM156N.csv")
exchange_rate <- read_csv("../data/CCUSMA02FRM618N.csv")
unemployment_rate <- read_csv("../data/LRHUTTTTFRM156S.csv")
oil_prices <- read_csv("../data/MCOILWTICO.csv")


var_data <- cpi %>% left_join(interest_rates) %>% 
  left_join(exchange_rate) %>% 
  left_join(unemployment_rate) %>% 
  left_join(oil_prices)

names(var_data) <- c("DATE","CPI",
                     "Interest_Rates","Exchange_Rates",
                     "Unemployment_Rate","Oil_Prices")



# year on year log change
calculate_yoy_change_and_log <- function(series) {
  # Calculate year-on-year change
  yoy_change <- diff(series) / lag(series) * 100  # Percentage change
  
  # Take the logarithm of the year-on-year change
  log_yoy_change <- log(1 + yoy_change / 100)
  
  return(list(yoy_change = yoy_change, log_yoy_change = log_yoy_change))
}


yoy<-calculate_yoy_change_and_log(var_data$Oil_Prices)
var_data$Oil_Prices_log_yoy <- yoy$log_yoy_change


calculate_yoy_change_and_log(var_data$Oil_Prices)  
  

var_data_time <- var_data %>% filter(DATE >= '1986-01-01')
  