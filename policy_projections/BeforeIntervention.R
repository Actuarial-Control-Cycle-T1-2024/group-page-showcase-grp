library(dplyr)

#################### Data calculating premiums and profits for each policy #####
# List of CSV files
file_paths <- paste0("beforeinterrvention/", 1:19, ".csv")
data_frames <- lapply(file_paths, read.csv)
combined_data <- bind_rows(data_frames)
# write.csv(combined_data, "before_intervention_combined.csv", row.names = FALSE)
names(combined_data) <- c("Policy.type", "Face.amount", "Issue.age", "Premium", "Loading","Profit",
                          "Expenses",  "Issue.year","Intervention", "Mortality")
inflation_factors <- c("2004" = 1.650662064, "2014" = 1.279037672, "2024" = 1)

combined_data <- combined_data %>%
  mutate(Profit = Profit * inflation_factors[as.character(Issue.year)])

########################### Inforce dataset ####################################
inforce <- read.csv("inforce-data.csv")

# There are no duplicates
num_duplicates <- sum(duplicated(inforce$Policy.number))
#Finding the number of na in the death indicator 
sum(is.na(inforce$Death.indicator))
#Finding number of the death indicator to compare the number of deaths
sum(inforce$Death.indicator == 1, na.rm = TRUE)

columns_to_factor <- c("Policy.type", "Sex", "Smoker.Status", "Underwriting.Class",
                       "Urban.vs.Rural", "Region", "Distribution.Channel",
                       "Death.indicator", "Lapse.Indicator", "Cause.of.Death")

# Convert columns to factors using lapply
inforce[columns_to_factor] <- lapply(inforce[columns_to_factor], as.factor)

#Lapse Indicator has factors of 1, Y and NA --> converting Y to 1s
inforce$Lapse.Indicator <- ifelse(inforce$Lapse.Indicator == "Y", 1,inforce$Lapse.Indicator)
sum(inforce$Lapse.Indicator == 1, na.rm = TRUE)

#Replacing blank factor in cause of death with NAs
inforce$Cause.of.Death <- replace(inforce$Cause.of.Death, inforce$Cause.of.Death =="", NA)

#Replacing death indicators
inforce$Death.indicator <- as.numeric(!is.na(inforce$Death.indicator))
#Replacing lapse indicators
inforce$Lapse.Indicator <- as.numeric(!is.na(inforce$Lapse.Indicator))

summary(inforce)
inforce <- inforce %>% mutate(Age.at.Death = Year.of.Death-Issue.year+Issue.age, 
                        Age.at.Lapse = Year.of.Lapse-Issue.year+Issue.age)

################################################################################
# inforce$Issue.year after 2004 round the year of issue to nearest: 2004, 2014, 2024
# inforce$Issue.age round to  26,35,45,55
# lookup the profit for each policy based on policy type, death benefit, age (rounded),
# mortality (1=SH, 2=SM, ...), issue year (rounded) from combined_data

# Profit matching
profit_matched <- inforce %>%
  select(Policy.number, Policy.type, Face.amount, Issue.year, Issue.age, Smoker.Status, Underwriting.Class) %>%
  filter(Issue.year >= 2004) %>%
  mutate(Issue.year = case_when(
    Issue.year > 2004 & Issue.year <= 2014 ~ if_else(Issue.year < 2009, 2004, 2014),
    Issue.year <= 2024 ~ if_else(Issue.year < 2019, 2014, 2024))) %>%
  mutate(Issue.age = case_when(
    Issue.age <= 30 ~ 26,
    Issue.age <= 40 ~ 35,
    Issue.age <= 50 ~ 45,
    TRUE ~ 55 )) %>%
  mutate(Mortality = case_when(
  (Smoker.Status == "S" & Underwriting.Class == "high risk") ~ 1,
  (Smoker.Status == "S" & Underwriting.Class == "moderate risk") ~ 2,
  (Smoker.Status == "NS" & Underwriting.Class == "high risk") ~ 3,
  (Smoker.Status == "NS" & Underwriting.Class == "low risk") ~ 4,
  (Smoker.Status == "NS" & Underwriting.Class == "moderate risk") ~ 5,
  (Smoker.Status == "NS" & Underwriting.Class == "very low risk") ~ 6))
  
merged_data <- merge(profit_matched, combined_data, 
                     by = c("Mortality", "Face.amount" ,"Policy.type", "Issue.age", "Issue.year"), 
                     all.x = TRUE)

profit_sum <- merged_data %>%
  group_by(Expenses) %>%
  summarise(total_profit = sum(Profit))

