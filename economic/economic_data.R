library(TTR)
library(ggplot2)
data  <- read.csv('economic_data.csv')
names(data) <- c("Year", "Inflation", "OvernightRate", "one_yr", "ten_yr")
overnight_rates <- data$OvernightRate

ggplot(data) +
  geom_line(aes(x=data$Year, y=data$one_yr, color="blue")) +
  geom_line(aes(x=data$Year, y=data$ten_yr, color="red")) +
  scale_color_manual(names='', values=c("blue", "red"), labels=c("1 Year Spot Rates", "10 Year Spot Rates")) +
  labs(x="Year", y="Yield (%)") +
  theme_bw()

# # plot of daily returns
daily_ret <- diff(data$OvernightRate)/data$OvernightRate[-length(data$OvernightRate)]
daily_ret <- data.frame(Year=data$Year[-nrow(data)],Return=daily_ret)
ggplot(daily_ret, aes(x=Year, y=Return)) +
  geom_line()

# Function to calculate Exponential Moving Average (EMA)
calculate_ema <- function(data, alpha = 0.2) {
  ema_result <- EMA(data, n = 1 / alpha)
  return(ema_result)
}

calculate_ema(overnight_rates)

ggplot() +
  geom_line(aes(x=1:length(overnight_rates), y=calculate_ema(overnight_rates)
  ), color="blue") +
  geom_line(aes(x=1:length(overnight_rates), y=overnight_rates
  ), color="red")

# Function to predict the next 20 values using EMA
predict_next_values <- function(data, alpha = 0.2, num_predictions = 20) {
  ema_values <- calculate_ema(data, alpha)
  
  # Predict the next 20 values
  predictions <- numeric(num_predictions)
  for (i in 1:num_predictions) {
    next_value <- tail(ema_values, 1)
    predictions[i] <- next_value
    ema_values <- calculate_ema(c(ema_values, next_value), alpha)
  }
  
  return(predictions)
}

# Set the smoothing factor (you can adjust this as needed)
alpha <- 0.2

# Predict the next 20 values using EMA
predicted_values <- predict_next_values(overnight_rates, alpha, num_predictions = 20)

