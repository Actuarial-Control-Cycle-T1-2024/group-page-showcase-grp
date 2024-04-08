library(dplyr)
library(ggplot2)
library(survival)
library(survminer)


data <- read.csv("inforce-data.csv")

# There are no duplicates
num_duplicates <- sum(duplicated(data$Policy.number))
#Finding the number of na in the death indicator 
sum(is.na(data$Death.indicator))
#Finding number of the death indicator to compare the number of deaths
sum(data$Death.indicator == 1, na.rm = TRUE)

columns_to_factor <- c("Policy.type", "Sex", "Smoker.Status", "Underwriting.Class",
                       "Urban.vs.Rural", "Region", "Distribution.Channel",
                       "Death.indicator", "Lapse.Indicator", "Cause.of.Death")

# Convert columns to factors using lapply
data[columns_to_factor] <- lapply(data[columns_to_factor], as.factor)

#Lapse Indicator has factors of 1, Y and NA --> converting Y to 1s
data$Lapse.Indicator <- ifelse(data$Lapse.Indicator == "Y", 1,data$Lapse.Indicator)
sum(data$Lapse.Indicator == 1, na.rm = TRUE)

#Replacing blank factor in cause of death with NAs
data$Cause.of.Death <- replace(data$Cause.of.Death, data$Cause.of.Death =="", NA)

#Replacing death indicators
data$Death.indicator <- as.numeric(!is.na(data$Death.indicator))
#Replacing lapse indicators
data$Lapse.Indicator <- as.numeric(!is.na(data$Lapse.Indicator))



summary(data)
data <- data %>% mutate(Age.at.Death = Year.of.Death-Issue.year+Issue.age, 
                        Age.at.Lapse = Year.of.Lapse-Issue.year+Issue.age)

#### KM and Log Test for smoker status
smoking_time <- data %>% 
  select(Death.indicator, Smoker.Status, Age.at.Death, Age.at.Lapse, Issue.year, Issue.age) %>%
  filter(!is.na(Issue.year) & !is.na(Issue.age)) %>%
  mutate(time = ifelse(!is.na(Age.at.Death), Age.at.Death,
                       ifelse(!is.na(Age.at.Lapse), Age.at.Lapse,
                              2023 - Issue.year + Issue.age)))


smoke_survival_obj <- Surv(time = smoking_time$time, event = smoking_time$Death.indicator == 1)
smoke_km_fit <- survfit(smoke_survival_obj ~ Smoker.Status, data = smoking_time)
ggsurvplot(smoke_km_fit, data = smoking_time, risk.table = TRUE, pval = TRUE) +
  labs(title = "Kaplan-Meier Curves by Smoking Status", 
       x = "Age", y = "Survival Probability")

smoke_log_rank_test <- survdiff(smoke_survival_obj ~ Smoker.Status, data = smoking_time)
age_range <- seq(20, 80)
surv_summary <- summary(smoke_km_fit,age_range)

smoking_time_s <- smoking_time %>% filter(Smoker.Status=="S")
smoking_time_ns <- smoking_time %>% filter(Smoker.Status=="NS")

smokers.cens <- Surv(smoking_time_s$time, smoking_time_s$Death.indicator)
non_smokers.cens <- Surv(smoking_time_ns$time, smoking_time_ns$Death.indicator)

smokers_km <- survfit(smokers.cens~1)
non_smokers_km <- survfit(non_smokers.cens~1)

# calculate_q_from_s <- function(s) {
#   -diff(s)/s[-length(s)]
# }
# 
# loadings <- data.frame(time=smokers_km$time[-length(smokers_km$time)], Smokers_qx=calculate_q_from_s(smokers_km$surv), non_smokers_qx=calculate_q_from_s(non_smokers_km$surv)) %>%
#   mutate(Ratio_NS_to_S=non_smokers_qx/Smokers_qx)


# smoke_cox<- coxph(smoke_survival_obj ~ Smoker.Status, data = smoking_time)
# cox_zph <- cox.zph(smoke_cox)
# ggcoxzph(cox_zph)

#### Smoker Status and Underwriting Class ######################################
smoking_undewriting_time <- data %>% 
  select(Death.indicator, Smoker.Status, Underwriting.Class, Age.at.Death, Age.at.Lapse, Issue.year, Issue.age) %>%
  filter(!is.na(Issue.year) & !is.na(Issue.age)) %>%
  mutate(time = ifelse(!is.na(Age.at.Death), Age.at.Death,
                       ifelse(!is.na(Age.at.Lapse), Age.at.Lapse,
                              2023 - Issue.year + Issue.age)))


smoke_undewrite_survival_obj <- Surv(time = smoking_undewriting_time$time, event = smoking_undewriting_time$Death.indicator == 1)
smoke_underwrite_km_fit <- survfit(smoke_undewrite_survival_obj ~ Smoker.Status + Underwriting.Class, data = smoking_undewriting_time)
ggsurvplot(smoke_underwrite_km_fit, data = smoking_undewriting_time, risk.table = FALSE, pval = TRUE) +
  labs(title = "Kaplan-Meier Curves by Smoking Status and Underwriting Class", 
       x = "Age", y = "Survival Probability")

smoke_log_rank_test <- survdiff(smoke_undewrite_survival_obj ~ Smoker.Status + Underwriting.Class, data = smoking_undewriting_time)
age_range <- seq(20, 80)
surv_smoke_underwrite_summary <- summary(smoke_underwrite_km_fit,age_range)

smoking_time_ns_hr <- smoking_undewriting_time %>% filter(Smoker.Status=="NS" & Underwriting.Class == "high risk")
non_smokers_hr.cens <- Surv(smoking_time_ns_hr$time, smoking_time_ns_hr$Death.indicator)
non_smokers_hr_km <- survfit(non_smokers_hr.cens~1)

smoking_time_ns_mr <- smoking_undewriting_time %>% filter(Smoker.Status=="NS" & Underwriting.Class == "moderate risk")
non_smokers_mr.cens <- Surv(smoking_time_ns_mr$time, smoking_time_ns_mr$Death.indicator)
non_smokers_mr_km <- survfit(non_smokers_mr.cens~1)

smoking_time_ns_lr <- smoking_undewriting_time %>% filter(Smoker.Status=="NS" & Underwriting.Class == "low risk")
non_smokers_lr.cens <- Surv(smoking_time_ns_lr$time, smoking_time_ns_lr$Death.indicator)
non_smokers_lr_km <- survfit(non_smokers_lr.cens~1)

smoking_time_ns_vlr <- smoking_undewriting_time %>% filter(Smoker.Status=="NS" & Underwriting.Class == "very low risk")
non_smokers_vlr.cens <- Surv(smoking_time_ns_vlr$time, smoking_time_ns_vlr$Death.indicator)
non_smokers_vlr_km <- survfit(non_smokers_vlr.cens~1)

smoking_time_s_hr <- smoking_undewriting_time %>% filter(Smoker.Status=="S" & Underwriting.Class == "high risk")
smokers_hr.cens <- Surv(smoking_time_s_hr$time, smoking_time_s_hr$Death.indicator)
smokers_hr_km <- survfit(smokers_hr.cens~1)

smoking_time_s_mr <- smoking_undewriting_time %>% filter(Smoker.Status=="S" & Underwriting.Class == "moderate risk")
smokers_mr.cens <- Surv(smoking_time_s_mr$time, smoking_time_s_mr$Death.indicator)
smokers_mr_km <- survfit(smokers_mr.cens~1)


calculate_q_from_s <- function(s) {
  -diff(s)/s[-length(s)]
}

smokers_high_qx <- c(calculate_q_from_s(smokers_hr_km$surv),1)

loadings <- data.frame(time=smokers_mr_km$time[-length(smokers_mr_km$time)],
                       smokers_high_qx,
                       smokers_moderate_qx=calculate_q_from_s(smokers_mr_km$surv),
                       non_smokers_high_qx=calculate_q_from_s(non_smokers_hr_km$surv),
                       non_smokers_moderate_qx=calculate_q_from_s(non_smokers_mr_km$surv),
                       non_smokers_low_qx=calculate_q_from_s(non_smokers_lr_km$surv),
                       non_smokers_very_low_qx=calculate_q_from_s(non_smokers_vlr_km$surv))

write.csv(loadings, "loadings.csv", row.names = FALSE)

