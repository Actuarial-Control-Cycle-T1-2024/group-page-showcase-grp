setwd("~/ACTL4001")
library(ggplot2)
library(dplyr)

data <- read.csv("superlife_inforce_data.csv")

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
                        Age.at.Lapse = Year.of.Lapse-Issue.year+Issue.age,
                        Lapse.Duration = Year.of.Lapse-Issue.year+1,
                        Death.Duration = Year.of.Death-Issue.year+1,
                        duration = ifelse(!is.na(Age.at.Death), Year.of.Death-Issue.year,
                                          ifelse(!is.na(Age.at.Lapse), Year.of.Lapse-Issue.year,
                                                 2023 - Issue.year)))
data$Death.indicator <- as.numeric(!is.na(data$Death.indicator))
data$Lapse.Indicator <- as.numeric(!is.na(data$Lapse.Indicator))

smoking_time <- data %>% 
  select(Death.indicator, Smoker.Status, Age.at.Death, Age.at.Lapse, Issue.year, Issue.age) %>%
  filter(!is.na(Issue.year) & !is.na(Issue.age)) %>%
  mutate(time = ifelse(!is.na(Age.at.Death), Age.at.Death,
                       ifelse(!is.na(Age.at.Lapse), Age.at.Lapse,
                              2023 - Issue.year + Issue.age)))

smoking_time_s <- smoking_time %>% filter(Smoker.Status=="S")
smoking_time_ns <- smoking_time %>% filter(Smoker.Status=="NS")

smokers.cens <- Surv(smoking_time_s$time, smoking_time_s$Death.indicator)
non_smokers.cens <- Surv(smoking_time_ns$time, smoking_time_ns$Death.indicator)

smokers_km <- survfit(smokers.cens~1)
non_smokers_km <- survfit(non_smokers.cens~1)

calculate_q_from_s <- function(s) {
  -diff(s)/s[-length(s)]
}

loadings <- data.frame(time=smokers_km$time[-length(smokers_km$time)], Smokers_qx=calculate_q_from_s(smokers_km$surv), non_smokers_qx=calculate_q_from_s(non_smokers_km$surv)) %>%
  mutate(Ratio_NS_to_S=non_smokers_qx/Smokers_qx)

write.csv(loadings, '~/ACTL4001/smokers_loading.csv')
  
  