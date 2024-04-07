
#---- Profit in 5 years
# predict how many people in the future:
inforce <- read.csv("inforce-dataset.csv") # the SuperLife inforce dataset.
forecasts <- read.csv("customer_forecasts.csv")

inforce <- inforce %>% mutate(Issue.age.band = case_when(
  Issue.age <= 30 ~ 26,
  Issue.age <= 40 ~ 35,
  Issue.age <= 50 ~ 45,
  TRUE ~ 55 ))
junk <- inforce %>%
  mutate(policyholder.type = paste(Policy.type, Issue.age.band, Face.amount, 
                                   case_when(
                                     (Smoker.Status == "S" & Underwriting.Class == "high risk") ~ 1,
                                     (Smoker.Status == "S" & Underwriting.Class == "moderate risk") ~ 2,
                                     (Smoker.Status == "NS" & Underwriting.Class == "high risk") ~ 3,
                                     (Smoker.Status == "NS" & Underwriting.Class == "low risk") ~ 4,
                                     (Smoker.Status == "NS" & Underwriting.Class == "moderate risk") ~ 5,
                                     (Smoker.Status == "NS" & Underwriting.Class == "very low risk") ~ 6
                                   ),
                                   sep = '.'
  )
  )

issue.ts <- junk %>% 
  select(Issue.year, policyholder.type) %>% 
  group_by(Issue.year, policyholder.type) %>% 
  summarise(n=n())
issue.ts$policyholder.type <- as.factor(issue.ts$policyholder.type)
write.csv(issue.ts, "issue_timeseries.csv")

customer_forecasts <- data.frame(Issue.year = seq(2024, 2074))
customer_issues <- data.frame(Issue.year = seq(2001, 2023))
customer_forecasts_lwr <- data.frame(Issue.year = seq(2024, 2074))
customer_forecasts_upr <- data.frame(Issue.year = seq(2024, 2074))

for (ph.type in levels(issue.ts$policyholder.type)) {
  ts <- issue.ts %>% filter(policyholder.type == ph.type) %>% select(n, Issue.year)
  customer_issues <- merge(x = customer_issues, y = ts, by = "Issue.year", all = TRUE)
  #customer_issues <- cbind(customer_issues, ts$n)
  next.lm <- lm(formula =  n  ~ Issue.year, data = ts)
  pred <- predict.lm(next.lm, newdata = customer_forecasts, interval = "confidence")
  customer_forecasts <- cbind(customer_forecasts, pred[, 'fit'])
  customer_forecasts_lwr <- cbind(customer_forecasts_lwr, pred[, 'lwr'])
  customer_forecasts_upr <- cbind(customer_forecasts_upr, pred[,'upr'])
}

colnames(customer_forecasts)[2:length(customer_forecasts)] <- levels(issue.ts$policyholder.type)
colnames(customer_forecasts_lwr)[2:length(customer_forecasts_lwr)] <- levels(issue.ts$policyholder.type)
colnames(customer_forecasts_upr)[2:length(customer_forecasts_upr)] <- levels(issue.ts$policyholder.type)
colnames(customer_issues)[2:length(customer_issues)] <- levels(issue.ts$policyholder.type)
View(customer_forecasts)
View(customer_issues)
write.csv(customer_forecasts, "better_customer_forecasts.csv")
write.csv(customer_forecasts_lwr, "better_customer_forecasts_lower.csv")
write.csv(customer_forecasts_upr, "better_customer_forecasts_upper.csv")
all.issue.counts <- rbind(customer_issues, customer_forecasts)
write.csv(all.issue.counts, "better_customer_forecasts3.csv")

customer_forecasts <- data.frame(Issue.year = seq(2024, 2074))
customer_issues <- data.frame(Issue.year = seq(2001, 2023))
for (ph.type in levels(issue.ts$policyholder.type)) {
  ts <- issue.ts %>% filter(policyholder.type == ph.type) %>% select(n, Issue.year)
  customer_issues <- merge(x = customer_issues, y = ts, by = "Issue.year", all = TRUE)
  #customer_issues <- cbind(customer_issues, ts$n)
  #next.lm <- lm(formula =  n  ~ Issue.year, data = ts)
  #pred <- predict.lm(next.lm, newdata = customer_forecasts, interval = "confidence")
  #customer_forecasts <- cbind(customer_forecasts, pred[, 'fit'])
  #customer_forecasts_lwr <- cbind(customer_forecasts_lwr, pred[, 'lwr'])
  #customer_forecasts_upr <- cbind(customer_forecasts_upr, pred[,'upr'])
  customer_forecasts <- cbind(customer_forecasts, rep( min(ts$n),nrow(customer_forecasts)))
}
colnames(customer_forecasts)[2:length(customer_forecasts)] <- levels(issue.ts$policyholder.type)

write.csv(customer_forecasts, "min_customer_forecasts.csv")
