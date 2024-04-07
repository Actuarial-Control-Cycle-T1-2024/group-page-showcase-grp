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

years <- seq(2001,2023)
policies.data <- data.frame(years, num_new_policyholders_per_year)
mylm<-lm(formula = num_new_policyholders_per_year ~ years, data = policies.data)
newyears <- data.frame(years = seq(2001, 2100))
predictions <- predict.lm(mylm, newdata = newyears)
plot(newyears$years, predictions, type='l', ylab = "number policyholders", xlab = "year", main="Predicted new policyholders per year")
lines(policies.data, type='l', col='red')

underwriting.entrants <- data %>% select(Issue.year, Policy.number, Underwriting.Class) %>% group_by(Issue.year, Underwriting.Class) %>% summarise(n=n())

# ---- Underwriting proportions over time.
underwriting.props <- data.frame(levels(data$Underwriting.Class))
for (y in 2001:2023) {
  temp <- underwriting.entrants %>% filter(Issue.year == y)
  mat <- data.frame(underwriting = temp$Underwriting.Class, n= prop.table(temp$n))
  print(mat)
  underwriting.props <- cbind(underwriting.props, prop.table(temp$n))
}

colnames(underwriting.props)[2:24] <- seq(2001, 2023)
a <- t(underwriting.props[2:24])
colnames(a) <- levels(data$Underwriting.Class)
plot(x = seq(2001,2023), y = a[,"high risk"], type='l', ylim=c(0,0.7), ylab="Proportion", xlab="Year of issue")
lines(x = seq(2001,2023), a[,"moderate risk"], type='l', ylim=c(0,0.5), col='darkgreen')
lines(x = seq(2001,2023), a[,"low risk"], type='l', ylim=c(0,0.5), col='red')
lines(x = seq(2001,2023), a[,"very low risk"], type='l', ylim=c(0,0.5), col='blue')
legend("topright", legend = levels(data$Underwriting.Class), col = c("black", "darkgreen", "red", "blue"), lty = 1, pt.cex = 0.5)

# ---- underwriting counts over time.
underwriting.counts <- data.frame(levels(data$Underwriting.Class))
for (y in 2001:2023) {
  temp <- underwriting.entrants %>% filter(Issue.year == y)
  mat <- data.frame(underwriting = temp$Underwriting.Class, n= temp$n)
  underwriting.counts <- cbind(underwriting.counts, temp$n)
}

colnames(underwriting.counts)[2:24] <- seq(2001, 2023)
underwriting.counts.t <- t(underwriting.counts[2:24])
colnames(underwriting.counts.t) <- levels(data$Underwriting.Class)
plot(x = seq(2001,2023), y = underwriting.counts.t[,"high risk"], type='l', ylim=c(0, 50000), ylab="Count", xlab="Year of issue")
lines(x = seq(2001,2023), underwriting.counts.t[,"moderate risk"], type='l', col='darkgreen')
lines(x = seq(2001,2023), underwriting.counts.t[,"low risk"], type='l',  col='red')
lines(x = seq(2001,2023), underwriting.counts.t[,"very low risk"], type='l', col='blue')
legend("topright", legend = levels(data$Underwriting.Class), col = c("black", "darkgreen", "red", "blue"), lty = 1, pt.cex = 0.5)

underwriting.counts.df <- data.frame(underwriting.counts.t)
underwriting.counts.df <- cbind(underwriting.counts.df, seq(2001,2023))
colnames(underwriting.counts.df)[5] = "year"
vl.lm <- lm(formula =  very.low.risk ~ year , data = underwriting.counts.df)
predictions <- predict.lm(vl.lm, newdata = data.frame(year = seq(2001, 2023)))
lines(x = seq(2001,2023), y=predictions, type='l',  col='blue', lty="dotted")

#---- more specified underwriting counts over time
data2 <- data %>% mutate(underwrite.code = case_when(
  (Smoker.Status == "S" & Underwriting.Class == "high risk") ~ "SH",
  (Smoker.Status == "S" & Underwriting.Class == "moderate risk") ~ "SM",
  (Smoker.Status == "NS" & Underwriting.Class == "high risk") ~ "NSH",
  (Smoker.Status == "NS" & Underwriting.Class == "low risk") ~ "NSL",
  (Smoker.Status == "NS" & Underwriting.Class == "moderate risk") ~ "NSM",
  (Smoker.Status == "NS" & Underwriting.Class == "very low risk") ~ "NSVL"
  )
)
data2$underwrite.code <- as.factor(data2$underwrite.code)
underwriting.entrants2 <- data2 %>% select(Issue.year, Policy.number, underwrite.code) %>% group_by(Issue.year, underwrite.code) %>% summarise(n=n())
underwriting.counts2 <- data.frame(levels(data2$underwrite.code))
for (y in 2001:2023) {
  temp <- underwriting.entrants2 %>% filter(Issue.year == y)
  underwriting.counts2 <- cbind(underwriting.counts2, temp$n)
}
colnames(underwriting.counts2)[2:24] <- seq(2001, 2023)
underwriting.counts2.t <- t(underwriting.counts2[2:24])
colnames(underwriting.counts2.t) <- levels(data2$underwrite.code)

plot(x = seq(2001,2023), y = underwriting.counts2.t[,"SH"], type='l', ylim = c(0, 30000), main="Counts of Non/Smokers of different underwriting classes being issued policies", ylab="Count", xlab="Year of issue")
lines(x = seq(2001,2023), underwriting.counts2.t[,"SM"], type='l', col='darkgreen')
lines(x = seq(2001,2023), underwriting.counts2.t[,"NSH"], type='l',  col='red')
lines(x = seq(2001,2023), underwriting.counts2.t[,"NSL"], type='l', col='blue')
lines(x = seq(2001,2023), underwriting.counts2.t[,"NSM"], type='l', col='purple')
lines(x = seq(2001,2023), underwriting.counts2.t[,"NSVL"], type='l', col='pink')
legend("topleft", legend = levels(data2$underwrite.code), col = c("red", "blue", "purple", "pink", "black", "darkgreen"), lty = 1, pt.cex = 0.5)

# now make lms:
issue.years.df <- data.frame(year = seq(2001, 2023))
underwriting.counts2.df <- data.frame(underwriting.counts2.t)
underwriting.counts2.df <- cbind(underwriting.counts2.df, seq(2001,2023))
colnames(underwriting.counts2.df)[7] = "year"

nsvl.lm <- lm(formula =  NSVL ~ year , data = underwriting.counts2.df)
predictions.nsvl <- predict.lm(nsvl.lm, newdata = issue.years.df)
lines(x = seq(2001,2023), y=predictions.nsvl, type='l',  col='pink', lty="dotted")

nsm.lm <- lm(formula =  NSM ~ year , data = underwriting.counts2.df)
predictions.nsm <- predict.lm(nsm.lm, newdata = issue.years.df)
lines(x = seq(2001,2023), y=predictions.nsm, type='l',  col='purple', lty="dotted")

nsl.lm <- lm(formula =  NSL ~ year , data = underwriting.counts2.df)
predictions.nsl <- predict.lm(nsl.lm, newdata = issue.years.df)
lines(x = seq(2001,2023), y=predictions.nsl, type='l',  col='blue', lty="dotted")

nsh.lm <- lm(formula =  NSH ~ year , data = underwriting.counts2.df)
predictions.nsh <- predict.lm(nsh.lm, newdata = issue.years.df)
lines(x = seq(2001,2023), y=predictions.nsh, type='l',  col='red', lty="dotted")

sm.lm <- lm(formula =  SM ~ year , data = underwriting.counts2.df)
predictions.sm <- predict.lm(sm.lm, newdata = issue.years.df)
lines(x = seq(2001,2023), y=predictions.sm, type='l',  col='darkgreen', lty="dotted")

sh.lm <- lm(formula =  SH ~ year , data = underwriting.counts2.df)
predictions.sh <- predict.lm(sh.lm, newdata = issue.years.df)
lines(x = seq(2001,2023), y=predictions.sh, type='l',  col='black', lty="dotted")

# ---- future forecasts
future <- data.frame(year = seq(2001, 2100))
predictions.nsvl.f <- predict.lm(nsvl.lm, newdata = future)
predictions.nsh.f <- predict.lm(nsh.lm, newdata = future)
predictions.nsl.f <- predict.lm(nsl.lm, newdata = future)
predictions.nsm.f <- predict.lm(nsm.lm, newdata = future)
predictions.sh.f <- predict.lm(sh.lm, newdata = future)
predictions.sm.f <- predict.lm(sm.lm, newdata = future)

plot(future$year, y=predictions.sh.f, type='l', ylim=c(0, 150000), main="Forecasted counts of Non/Smokers of different underwriting classes being issued policies", ylab="Count", xlab = "Year")
legend("topleft", legend = levels(data2$underwrite.code), col = c("red", "blue", "purple", "pink", "black", "darkgreen"), lty = 1, pt.cex = 0.5)
lines(future$year, y=predictions.nsvl.f, type='l',  col='pink')
lines(future$year, y=predictions.nsm.f, type='l',  col='purple')
lines(future$year, y=predictions.nsh.f, type='l',  col='red')
lines(future$year, y=predictions.nsl.f, type='l',  col='blue')
lines(future$year, y=predictions.sm.f, type='l',  col='darkgreen')

forecasts <- data.frame(year = seq(2001, 2100), nsvl = predictions.nsvl.f, nsh = predictions.nsh.f,
                        nsl = predictions.nsl.f, nsm = predictions.nsm.f, sh = predictions.sh.f,
                        sm = predictions.sm.f)
write.csv(forecasts, "customer_forecasts.csv", row.names = FALSE)
