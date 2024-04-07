setwd("/Users/robertzheng/Desktop/R/Year 4/SOA-Lumeria-2024")
library(ggplot2)
library(dplyr)

data <- read.csv("superlife_inforce_data.csv")

#Finding the number of na in the death indicator 
sum(is.na(data$Death.indicator))
#Finding number of the death indicator to compare the number of deaths
sum(data$Death.indicator == 1, na.rm = TRUE)

columns_to_factor <- c("Policy.type", "Sex", "Smoker.Status", "Underwriting.Class",
                       "Urban.vs.Rural", "Region", "Distribution.Channel",
                       "Death.indicator", "Lapse.Indicator","Cause.of.Death")

# Convert columns to factors using lapply
data[columns_to_factor] <- lapply(data[columns_to_factor], as.factor)

#Lapse Indicator has factors of 1, Y and NA --> converting Y to 1s
data$Lapse.Indicator <- ifelse(data$Lapse.Indicator == "Y", 1,data$Lapse.Indicator)
sum(data$Lapse.Indicator == 1, na.rm = TRUE)

# Replace blank with NA's
data$Cause.of.Death <- replace(data$Cause.of.Death, data$Cause.of.Death=="", NA)
summary(data$Cause.of.Death) 

# Number of policy holders who neither withdrew nor died
nrow(data[is.na(data$Year.of.Death) & is.na(data$Year.of.Lapse), ])

# Number of policy holders who died
nrow(data[!is.na(data$Year.of.Death), ])
# Number of policy holders who withdrew
nrow(data[!is.na(data$Year.of.Lapse), ])

# No duplicate policy numbers
length(unique(data$Policy.number))
length(data$Policy.number)

# Calculate age of death/withdrawal
data <- data %>% mutate(Age.at.Death = Year.of.Death-Issue.year+Issue.age, 
                Age.at.Lapse = Year.of.Lapse-Issue.year+Issue.age)

# Changing 
data$Lapse.Indicator <- as.numeric(!is.na(data$Lapse.Indicator))
data$Death.indicator <- as.numeric(!is.na(data$Death.indicator))



# Finding the Kaplan Meier Survival Curve Estimates ----
library("survminer")
library("survival")
library("KMsurv")


# Macks code
smoking_time <- data %>% 
  select(Death.indicator, Smoker.Status, Age.at.Death, Age.at.Lapse, Issue.year, Issue.age) %>%
  filter(!is.na(Issue.year) & !is.na(Issue.age)) %>%
  mutate(time = ifelse(!is.na(Age.at.Death), (Age.at.Death),
                       ifelse(!is.na(Age.at.Lapse), (Age.at.Lapse),
                              2023 - Issue.year + Issue.age)))




survival_obj <- Surv(time = smoking_time$time, event = smoking_time$Death.indicator == 1)
km_fit <- survfit(survival_obj ~ Smoker.Status, data = smoking_time)
ggsurvplot(km_fit, data = smoking_time, risk.table = TRUE, pval = TRUE) +
  labs(title = "Kaplan-Meier Curves by Smoking Status", 
       x = "Age", y = "Survival Probability")

logrank <- survdiff(survival_obj ~ Smoker.Status, data = smoking_time)





# Total Payout ----
yearly_payout <- data %>% 
  group_by(Year.of.Death) %>% 
  mutate(Total.Payout = sum(Face.amount * Death.indicator)) %>% 
  select(Year.of.Death, Total.Payout)

ggplot(yearly_payout, aes(x = Year.of.Death, y = Total.Payout)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Total Payout by Year",
       x = "Year",
       y = "Total Payout") +
  theme_minimal()

# Issue age of T20 ----
Issue.age <- data %>%
  filter(Policy.type == 'T20') %>%
  select(Issue.age)

summary(Issue.age)

# Interest Rates ----
library(readxl)
economic_data <- read_xlsx('lumaria-economic-data.xlsx')

summary(economic_data)


# Age and sum insured ----
Age_sum_insured <- data %>%
  select(Issue.age, Face.amount)

#ggplot(Age_sum_insured, aes(x = Issue.age, y = Face.amount)) +
#  geom_density(stat = "identity", fill = "skyblue", color = "black") +
 # labs(title = "Age and Sum insured",
  #     x = "Year",
   #    y = "Total Payout") +
#  theme_minimal()

# 

 
# Lapse Rates ----
lapse_rates <- data %>% 
  filter(data$Policy.type=="T20") %>%
  group_by(Lapse.Duration) %>%
  summarise(Number.of.Lapses = sum(Lapse.Indicator, na.rm=T)) %>%
  mutate(exposed_to_risk = sapply(Lapse.Duration, function(x) sum(data$duration >= x - 1))) %>%
  mutate(lapse_rate=Number.of.Lapses/exposed_to_risk)

t20_data <- data %>% filter(data$Policy.type=="T20")

lapse_rates <- t20_data %>%
  group_by(Lapse.Duration) %>%
  summarise(Number.of.Lapses = sum(Lapse.Indicator, na.rm=T)) %>%
  mutate(exposed_to_risk = sapply(Lapse.Duration, function(x) sum(t20_data$duration >= x - 1))) %>%
  mutate(lapse_rate=Number.of.Lapses/exposed_to_risk*100)

# Calculate age of death/withdrawal
data <- data %>% mutate(Age.at.Death = Year.of.Death-Issue.year+Issue.age, 
                        Age.at.Lapse = Year.of.Lapse-Issue.year+Issue.age,
                        Lapse.Duration = Year.of.Lapse-Issue.year+1,
                        Death.Duration = Year.of.Death-Issue.year+1,
                        duration = ifelse(!is.na(Age.at.Death), Year.of.Death-Issue.year,
                                          ifelse(!is.na(Age.at.Lapse), Year.of.Lapse-Issue.year,
                                                 2023 - Issue.year)))

# Smoking Data ----
smoker_proportion <- nrow(data %>% filter(Smoker.Status == 'S')) / sum(nrow(data %>% filter(Smoker.Status == 'S')), nrow(data %>% filter(Smoker.Status == 'NS')))
smoker_proportion_r1 <- nrow(data %>% filter(Smoker.Status == 'S', Region == 1)) / sum(nrow(data %>% filter(Smoker.Status == 'S', Region == 1)), nrow(data %>% filter(Smoker.Status == 'NS', Region == 1)))
smoker_proportion_r2 <- nrow(data %>% filter(Smoker.Status == 'S', Region == 2)) / sum(nrow(data %>% filter(Smoker.Status == 'S', Region == 2)), nrow(data %>% filter(Smoker.Status == 'NS', Region == 2)))
smoker_proportion_r3 <- nrow(data %>% filter(Smoker.Status == 'S', Region == 3)) / sum(nrow(data %>% filter(Smoker.Status == 'S', Region == 3)), nrow(data %>% filter(Smoker.Status == 'NS', Region == 3)))
smoker_proportion_r4 <- nrow(data %>% filter(Smoker.Status == 'S', Region == 4)) / sum(nrow(data %>% filter(Smoker.Status == 'S', Region == 4)), nrow(data %>% filter(Smoker.Status == 'NS', Region == 4)))
smoker_proportion_r5 <- nrow(data %>% filter(Smoker.Status == 'S', Region == 5)) / sum(nrow(data %>% filter(Smoker.Status == 'S', Region == 5)), nrow(data %>% filter(Smoker.Status == 'NS', Region == 5)))
smoker_proportion_r6 <- nrow(data %>% filter(Smoker.Status == 'S', Region == 6)) / sum(nrow(data %>% filter(Smoker.Status == 'S', Region == 6)), nrow(data %>% filter(Smoker.Status == 'NS', Region == 6)))

smoker_prop_data <- c(smoker_proportion, smoker_proportion_r1, smoker_proportion_r2, smoker_proportion_r3, smoker_proportion_r4, smoker_proportion_r5, smoker_proportion_r6)
smoker_prop_headers <- c("All", "Region 1", "Region 2", "Region 3", "Region 4", "Region 5", "Region 6")
smoker_prop_df <- data.frame(
  name = smoker_prop_headers,
  value = smoker_prop_data
)

ggplot(smoker_prop_df, aes(x=name, y=value)) + 
  geom_bar(stat = "identity", width=0.4) + 
  labs(
    title = "Proportion of Policyholders who are Smokers per Population Subset",
    x = "Populaiton Subset",
    y = "Proportion of Smokers"
  ) + 
  theme_bw()
  


# Policyholder Data ----
death_data <- data %>%
  filter(!is.na(Year.of.Death))
withdraw_data <- data %>% 
  filter(!is.na(Year.of.Lapse))

num_new_policyholders_per_year <- c()
num_died_policyholders_per_year <- c()
num_withdraw_policyholders_per_year <- c()
policy_years <- unique(data$Issue.year)

for (i in policy_years) {
  num_new_policyholders = 0
  for (j in 1:nrow(data)) {
    if (data$Issue.year[j] == i) {
      
      num_new_policyholders = num_new_policyholders + 1
    }
    
  }
  num_new_policyholders_per_year <- c(num_new_policyholders_per_year, num_new_policyholders)

}

for (i in policy_years) {
  num_policyholders_died = 0
  for (j in 1:nrow(death_data)) {
    if (death_data$Year.of.Death[j] == i) {
      num_policyholders_died = num_policyholders_died + 1
    }
  }
  num_died_policyholders_per_year <- c(num_died_policyholders_per_year, num_policyholders_died)

}

for (i in policy_years) {
  num_policyholders_withdraw = 0 
  for (j in 1:nrow(withdraw_data)) {
    if (withdraw_data$Year.of.Lapse[j] == i) {
      num_policyholders_died = num_policyholders_died + 1
    }
  }
  num_withdraw_policyholders_per_year <- c(num_withdraw_policyholders_per_year, num_policyholders_withdraw)
  
}

num_new_policyholders_per_year
num_died_policyholders_per_year
num_withdraw_policyholders_per_year


## Plotting ----
policy_numbers <- data.frame(Year = policy_years, New_Policyholders = num_new_policyholders_per_year)
p <- ggplot(policy_numbers, aes(x=Year, y=New_Policyholders)) +
  geom_line() + 
  labs(
    title = "New Policyholders Per Year",
    x = "Year",
    y = "New Policyholders"
  ) +
  theme_minimal()


# Predicting the growth of Policyholder numbers ----
library('forecast')
library('ggplot2')

## Linear Regression ----
# Linear Regression is chosen over the alternative of 
years <- seq(2001,2023)
policies.data <- data.frame(years, num_new_policyholders_per_year)
mylm<-lm(formula = num_new_policyholders_per_year ~ years, data = policies.data)
newyears <- data.frame(years = seq(2001, 2100))
predictions <- predict.lm(mylm, newdata = newyears)
plot(newyears$years, predictions, type='l', ylab = "number policyholders", xlab = "year", main="Predicted new policyholders per year")
lines(policies.data, type='l', col='red')

# Percentage increases in new Policyholders
# SMOKER CESSATION:
# - 18% of population over 18 years old are smokers
# - We will assume that in Lumaria, around 50% of the population has life insurance
# meaning that 



## ARIMA ----
policy_numbers_ts <- ts(policy_numbers$New_Policyholders, frequency = 23)
policy_arima <- arima(policy_numbers_ts, order = c(2,1,2))
accuracy(policy_arima)
policy_num_forecast_values <- forecast(policy_arima, h = 20)

policy_num_forecast_df <- data.frame(
  Time = time(policy_num_forecast_values$mean),
  Forecast = policy_num_forecast_values$mean,
  Lower = policy_num_forecast_values$lower,
  Upper = policy_num_forecast_values$upper
)

autoplot(forecast(policy_arima))

ggplot() +
  geom_line(data = policy_num_forecast_df, aes(x = Time, y = Forecast), color = "red") +
  #geom_ribbon(data = policy_num_forecast_df, aes(x = Time, ymin = Lower, ymax = Upper), fill = "lightblue", alpha = 0.5) +
  labs(title = "ARIMA Forecast", x = "Time", y = "Value") +
  theme_minimal()
