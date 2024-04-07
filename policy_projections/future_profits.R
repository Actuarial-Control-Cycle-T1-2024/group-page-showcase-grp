library(dplyr)
library(ggplot2)
library(ggtext)

#################### Data calculating premiums and profits for each policy #####
before_intervention <- read.csv("before_intervention_combined.csv")
before_intervention <- before_intervention %>% filter(Year == 2024)
after_intervention <- read.csv("after_intervention_combined.csv")
after_intervention <- after_intervention %>% filter(Year == 2024)

projections <- read.csv("better_customer_forecasts.csv")
projections[projections < 0] <- 0
projections <- projections[,-1]

low_inflation <- 0.00190
med_inflation <- 0.0665
high_inflation <- 0.12620
quit_portion <- 0.1

########################### Inforce dataset ####################################

get_profit <- function(policy_type, lookup_table) {
  policy <- strsplit(policy_type, "\\.")[[1]]
  profit <- lookup_table$Profit[lookup_table$Type == policy[1] &
                                lookup_table$Age == policy[2] &
                                lookup_table$Death.Benefit == policy[3] &
                                lookup_table$Mortality == policy[4]]
  return(profit)
}

sum_profit <- function(lookup_table) {
  profit_table <- projections
  for (i in 1:nrow(projections)) {
    for (j in 2:ncol(projections)) {
      policy_type <- colnames(projections)[j]
      num_policies <- projections[i, j]
      profit <- get_profit(policy_type, lookup_table)
      total_profit <- num_policies * profit
      profit_table[i, j] <- total_profit
    }
  }
  return(profit_table)
}

low_before_intervention <- before_intervention %>% filter(Expenses == 1)
med_before_intervention <- before_intervention %>% filter(Expenses == 2)
high_before_intervention <- before_intervention %>% filter(Expenses == 3)

low_before_profit <- sum_profit(low_before_intervention)
med_before_profit <- sum_profit(med_before_intervention)
high_before_profit <- sum_profit(high_before_intervention)

no_smoke <- c(1, 2, 3, 4, 5, 6)
smoke <- c(3, 4, 5, 6, 7, 8)

low_after_intervention_m1_quit <- after_intervention %>%
  filter(Expenses == 1 & Reduction == 1 & Mortality %in% no_smoke)
med_after_intervention_m1_quit <- after_intervention %>%
  filter(Expenses == 2 & Reduction == 1 & Mortality %in% no_smoke)
high_after_intervention_m1_quit <- after_intervention %>%
  filter(Expenses == 3 & Reduction == 1 & Mortality %in% no_smoke)

low_after_intervention_m1_smoke <- after_intervention %>%
  filter(Expenses == 1 & Reduction == 1 & Mortality %in% smoke)
med_after_intervention_m1_smoke <- after_intervention %>%
  filter(Expenses == 2 & Reduction == 1 & Mortality %in% smoke)
high_after_intervention_m1_smoke <- after_intervention %>%
  filter(Expenses == 3 & Reduction == 1 & Mortality %in% smoke)
low_after_intervention_m1_smoke$Mortality[low_after_intervention_m1_smoke$Mortality == 7] <- 1
low_after_intervention_m1_smoke$Mortality[low_after_intervention_m1_smoke$Mortality == 8] <- 2
med_after_intervention_m1_smoke$Mortality[med_after_intervention_m1_smoke$Mortality == 7] <- 1
med_after_intervention_m1_smoke$Mortality[med_after_intervention_m1_smoke$Mortality == 8] <- 2
high_after_intervention_m1_smoke$Mortality[high_after_intervention_m1_smoke$Mortality == 7] <- 1
high_after_intervention_m1_smoke$Mortality[high_after_intervention_m1_smoke$Mortality == 8] <- 2

low_after_m1_total <- low_after_intervention_m1_quit
low_after_m1_total$Profit <- quit_portion * low_after_intervention_m1_quit$Profit + (1 - quit_portion) * low_after_intervention_m1_smoke$Profit
med_after_m1_total <- med_after_intervention_m1_quit
med_after_m1_total$Profit <- quit_portion * med_after_intervention_m1_quit$Profit + (1 - quit_portion) * med_after_intervention_m1_smoke$Profit
high_after_m1_total <- high_after_intervention_m1_quit
high_after_m1_total$Profit <- quit_portion * high_after_intervention_m1_quit$Profit + (1 - quit_portion) * high_after_intervention_m1_smoke$Profit

low_after_intervention_m2_quit <- after_intervention %>%
  filter(Expenses == 1 & Reduction == 2 & Mortality %in% no_smoke)
med_after_intervention_m2_quit <- after_intervention %>%
  filter(Expenses == 2 & Reduction == 2 & Mortality %in% no_smoke)
high_after_intervention_m2_quit <- after_intervention %>%
  filter(Expenses == 3 & Reduction == 2 & Mortality %in% no_smoke)

low_after_intervention_m2_smoke <- after_intervention %>%
  filter(Expenses == 1 & Reduction == 2 & Mortality %in% smoke)
med_after_intervention_m2_smoke <- after_intervention %>%
  filter(Expenses == 2 & Reduction == 2 & Mortality %in% smoke)
high_after_intervention_m2_smoke <- after_intervention %>%
  filter(Expenses == 3 & Reduction == 2 & Mortality %in% smoke)
low_after_intervention_m2_smoke$Mortality[low_after_intervention_m2_smoke$Mortality == 7] <- 1
low_after_intervention_m2_smoke$Mortality[low_after_intervention_m2_smoke$Mortality == 8] <- 2
med_after_intervention_m2_smoke$Mortality[med_after_intervention_m2_smoke$Mortality == 7] <- 1
med_after_intervention_m2_smoke$Mortality[med_after_intervention_m2_smoke$Mortality == 8] <- 2
high_after_intervention_m2_smoke$Mortality[high_after_intervention_m2_smoke$Mortality == 7] <- 1
high_after_intervention_m2_smoke$Mortality[high_after_intervention_m2_smoke$Mortality == 8] <- 2

low_after_m2_total <- low_after_intervention_m2_quit
low_after_m2_total$Profit <- quit_portion * low_after_intervention_m2_quit$Profit + (1 - quit_portion) * low_after_intervention_m2_smoke$Profit
med_after_m2_total <- med_after_intervention_m2_quit
med_after_m2_total$Profit <- quit_portion * med_after_intervention_m2_quit$Profit + (1 - quit_portion) * med_after_intervention_m2_smoke$Profit
high_after_m2_total <- high_after_intervention_m2_quit
high_after_m2_total$Profit <- quit_portion * high_after_intervention_m2_quit$Profit + (1 - quit_portion) * high_after_intervention_m2_smoke$Profit

low_after_m1 <- sum_profit(low_after_m1_total)
med_after_m1 <- sum_profit(med_after_m1_total)
high_after_m1 <- sum_profit(high_after_m1_total)

low_after_m2 <- sum_profit(low_after_m2_total)
med_after_m2 <- sum_profit(med_after_m2_total)
high_after_m2 <- sum_profit(high_after_m2_total)

years <- low_after_m1[,1]

ag_low_before <- data.frame(Year = years)
ag_med_before <- data.frame(Year = years)
ag_high_before <- data.frame(Year = years)
ag_low_after_m1 <- data.frame(Year = years)
ag_med_after_m1 <- data.frame(Year = years)
ag_high_after_m1 <- data.frame(Year = years)
ag_low_after_m2 <- data.frame(Year = years)
ag_med_after_m2 <- data.frame(Year = years)
ag_high_after_m2 <- data.frame(Year = years)

for (i in 1:length(years)) {
  year <- years[i]
  t1 <- sum(low_after_m1[low_after_m1$Issue.year == year], -1)
  t2 <- sum(med_after_m1[med_after_m1$Issue.year == year], -1)
  t3 <- sum(high_after_m1[high_after_m1$Issue.year == year], -1)
  t4 <- sum(low_after_m2[low_after_m2$Issue.year == year], -1)
  t5 <- sum(med_after_m2[med_after_m2$Issue.year == year], -1)
  t6 <- sum(high_after_m2[high_after_m2$Issue.year == year], -1)
  t7 <- sum(low_before_profit[low_before_profit$Issue.year == year], -1)
  t8 <- sum(med_before_profit[med_before_profit$Issue.year == year], -1)
  t9 <- sum(high_before_profit[high_before_profit$Issue.year == year], -1)
  ag_low_after_m1[i, "Profit"] <- t1
  ag_med_after_m1[i, "Profit"] <- t2
  ag_high_after_m1[i, "Profit"] <- t3
  ag_low_after_m2[i, "Profit"] <- t4
  ag_med_after_m2[i, "Profit"] <- t5
  ag_high_after_m2[i, "Profit"] <- t6
  ag_low_before[i, "Profit"] <- t7
  ag_med_before[i, "Profit"] <- t8
  ag_high_before[i, "Profit"] <- t9
}

ag_low_after_m1_1 <- ag_low_after_m1
ag_med_after_m1_1 <- ag_med_after_m1
ag_high_after_m1_1 <- ag_high_after_m1
ag_low_after_m2_1 <- ag_low_after_m2
ag_med_after_m2_1 <- ag_med_after_m2
ag_high_after_m2_1 <- ag_high_after_m2
ag_low_before_1 <- ag_low_before
ag_med_before_1 <- ag_med_before
ag_high_before_1 <- ag_high_before

ag_low_after_m1_2 <- ag_low_after_m1
ag_med_after_m1_2 <- ag_med_after_m1
ag_high_after_m1_2 <- ag_high_after_m1
ag_low_after_m2_2 <- ag_low_after_m2
ag_med_after_m2_2 <- ag_med_after_m2
ag_high_after_m2_2 <- ag_high_after_m2
ag_low_before_2 <- ag_low_before
ag_med_before_2 <- ag_med_before
ag_high_before_2 <- ag_high_before

ag_low_after_m1_3 <- ag_low_after_m1
ag_med_after_m1_3 <- ag_med_after_m1
ag_high_after_m1_3 <- ag_high_after_m1
ag_low_after_m2_3 <- ag_low_after_m2
ag_med_after_m2_3 <- ag_med_after_m2
ag_high_after_m2_3 <- ag_high_after_m2
ag_low_before_3 <- ag_low_before
ag_med_before_3 <- ag_med_before
ag_high_before_3 <- ag_high_before

for (i in 1:length(years)) {
  inflation <- low_inflation
  ag_low_after_m1_1[i, 2] <- ag_low_after_m1[i, 2] * (1 + inflation) ^ (1-i)
  ag_med_after_m1_1[i, 2] <- ag_med_after_m1[i, 2] * (1 + inflation) ^ (1-i)
  ag_high_after_m1_1[i, 2] <- ag_high_after_m1[i, 2] * (1 + inflation) ^ (1-i)
  ag_low_after_m2_1[i, 2] <- ag_low_after_m2[i, 2] * (1 + inflation) ^ (1-i)
  ag_med_after_m2_1[i, 2] <- ag_med_after_m2[i, 2] * (1 + inflation) ^ (1-i)
  ag_high_after_m2_1[i, 2] <- ag_high_after_m2[i, 2] * (1 + inflation) ^ (1-i)
  ag_low_before_1[i, 2] <- ag_low_before[i, 2] * (1 + inflation) ^ (1-i)
  ag_med_before_1[i, 2] <- ag_med_before[i, 2] * (1 + inflation) ^ (1-i)
  ag_high_before_1[i, 2] <- ag_high_before[i, 2] * (1 + inflation) ^ (1-i)
  inflation <- med_inflation
  ag_low_after_m1_2[i, 2] <- ag_low_after_m1[i, 2] * (1 + inflation) ^ (1-i)
  ag_med_after_m1_2[i, 2] <- ag_med_after_m1[i, 2] * (1 + inflation) ^ (1-i)
  ag_high_after_m1_2[i, 2] <- ag_high_after_m1[i, 2] * (1 + inflation) ^ (1-i)
  ag_low_after_m2_2[i, 2] <- ag_low_after_m2[i, 2] * (1 + inflation) ^ (1-i)
  ag_med_after_m2_2[i, 2] <- ag_med_after_m2[i, 2] * (1 + inflation) ^ (1-i)
  ag_high_after_m2_2[i, 2] <- ag_high_after_m2[i, 2] * (1 + inflation) ^ (1-i)
  ag_low_before_2[i, 2] <- ag_low_before[i, 2] * (1 + inflation) ^ (1-i)
  ag_med_before_2[i, 2] <- ag_med_before[i, 2] * (1 + inflation) ^ (1-i)
  ag_high_before_2[i, 2] <- ag_high_before[i, 2] * (1 + inflation) ^ (1-i)
  inflation <- high_inflation
  ag_low_after_m1_3[i, 2] <- ag_low_after_m1[i, 2] * (1 + inflation) ^ (1-i)
  ag_med_after_m1_3[i, 2] <- ag_med_after_m1[i, 2] * (1 + inflation) ^ (1-i)
  ag_high_after_m1_3[i, 2] <- ag_high_after_m1[i, 2] * (1 + inflation) ^ (1-i)
  ag_low_after_m2_3[i, 2] <- ag_low_after_m2[i, 2] * (1 + inflation) ^ (1-i)
  ag_med_after_m2_3[i, 2] <- ag_med_after_m2[i, 2] * (1 + inflation) ^ (1-i)
  ag_high_after_m2_3[i, 2] <- ag_high_after_m2[i, 2] * (1 + inflation) ^ (1-i)
  ag_low_before_3[i, 2] <- ag_low_before[i, 2] * (1 + inflation) ^ (1-i)
  ag_med_before_3[i, 2] <- ag_med_before[i, 2] * (1 + inflation) ^ (1-i)
  ag_high_before_3[i, 2] <- ag_high_before[i, 2] * (1 + inflation) ^ (1-i)
}

ag_low_after_m1_1$CumulativeProfit <- cumsum(ag_low_after_m1_1$Profit)
ag_med_after_m1_1$CumulativeProfit <- cumsum(ag_med_after_m1_1$Profit)
ag_high_after_m1_1$CumulativeProfit <- cumsum(ag_high_after_m1_1$Profit)
ag_low_after_m2_1$CumulativeProfit <- cumsum(ag_low_after_m2_1$Profit)
ag_med_after_m2_1$CumulativeProfit <- cumsum(ag_med_after_m2_1$Profit)
ag_high_after_m2_1$CumulativeProfit <- cumsum(ag_high_after_m2_1$Profit)
ag_low_before_1$CumulativeProfit <- cumsum(ag_low_before_1$Profit)
ag_med_before_1$CumulativeProfit <- cumsum(ag_med_before_1$Profit)
ag_high_before_1$CumulativeProfit <- cumsum(ag_high_before_1$Profit)

ag_low_after_m1_2$CumulativeProfit <- cumsum(ag_low_after_m1_2$Profit)
ag_med_after_m1_2$CumulativeProfit <- cumsum(ag_med_after_m1_2$Profit)
ag_high_after_m1_2$CumulativeProfit <- cumsum(ag_high_after_m1_2$Profit)
ag_low_after_m2_2$CumulativeProfit <- cumsum(ag_low_after_m2_2$Profit)
ag_med_after_m2_2$CumulativeProfit <- cumsum(ag_med_after_m2_2$Profit)
ag_high_after_m2_2$CumulativeProfit <- cumsum(ag_high_after_m2_2$Profit)
ag_low_before_2$CumulativeProfit <- cumsum(ag_low_before_2$Profit)
ag_med_before_2$CumulativeProfit <- cumsum(ag_med_before_2$Profit)
ag_high_before_2$CumulativeProfit <- cumsum(ag_high_before_2$Profit)

ag_low_after_m1_3$CumulativeProfit <- cumsum(ag_low_after_m1_3$Profit)
ag_med_after_m1_3$CumulativeProfit <- cumsum(ag_med_after_m1_3$Profit)
ag_high_after_m1_3$CumulativeProfit <- cumsum(ag_high_after_m1_3$Profit)
ag_low_after_m2_3$CumulativeProfit <- cumsum(ag_low_after_m2_3$Profit)
ag_med_after_m2_3$CumulativeProfit <- cumsum(ag_med_after_m2_3$Profit)
ag_high_after_m2_3$CumulativeProfit <- cumsum(ag_high_after_m2_3$Profit)
ag_low_before_3$CumulativeProfit <- cumsum(ag_low_before_3$Profit)
ag_med_before_3$CumulativeProfit <- cumsum(ag_med_before_3$Profit)
ag_high_before_3$CumulativeProfit <- cumsum(ag_high_before_3$Profit)

ag_low_1 <- cbind(ag_low_after_m1_1$Year,
                  ag_low_before_1$CumulativeProfit,
                  ag_low_after_m1_1$CumulativeProfit,
                  ag_low_after_m2_1$CumulativeProfit)
ag_low_2 <- cbind(ag_low_after_m1_2$Year,
                  ag_low_before_2$CumulativeProfit,
                  ag_low_after_m1_2$CumulativeProfit,
                  ag_low_after_m2_2$CumulativeProfit)
ag_low_3 <- cbind(ag_low_after_m1_3$Year,
                  ag_low_before_3$CumulativeProfit,
                  ag_low_after_m1_3$CumulativeProfit,
                  ag_low_after_m2_3$CumulativeProfit)
ag_med_1 <- cbind(ag_med_after_m1_1$Year,
                  ag_med_before_1$CumulativeProfit,
                  ag_med_after_m1_1$CumulativeProfit,
                  ag_med_after_m2_1$CumulativeProfit)
ag_med_2 <- cbind(ag_med_after_m1_2$Year,
                  ag_med_before_2$CumulativeProfit,
                  ag_med_after_m1_2$CumulativeProfit,
                  ag_med_after_m2_2$CumulativeProfit)
ag_med_3 <- cbind(ag_med_after_m1_3$Year,
                  ag_med_before_3$CumulativeProfit,
                  ag_med_after_m1_3$CumulativeProfit,
                  ag_med_after_m2_3$CumulativeProfit)
ag_high_1 <- cbind(ag_high_after_m1_1$Year,
                   ag_high_before_1$CumulativeProfit,
                   ag_high_after_m1_1$CumulativeProfit,
                   ag_high_after_m2_1$CumulativeProfit)
ag_high_2 <- cbind(ag_high_after_m1_2$Year,
                   ag_high_before_2$CumulativeProfit,
                   ag_high_after_m1_2$CumulativeProfit,
                   ag_high_after_m2_2$CumulativeProfit)
ag_high_3 <- cbind(ag_high_after_m1_3$Year,
                   ag_high_before_3$CumulativeProfit,
                   ag_high_after_m1_3$CumulativeProfit,
                   ag_high_after_m2_3$CumulativeProfit)


colnames(ag_low_1) <- c("Year", "Before", "Conservative", "Optimistic")
colnames(ag_low_2) <- c("Year", "Before", "Conservative", "Optimistic")
colnames(ag_low_3) <- c("Year", "Before", "Conservative", "Optimistic")
colnames(ag_med_1) <- c("Year", "Before", "Conservative", "Optimistic")
colnames(ag_med_2) <- c("Year", "Before", "Conservative", "Optimistic")
colnames(ag_med_3) <- c("Year", "Before", "Conservative", "Optimistic")
colnames(ag_high_1) <- c("Year", "Before", "Conservative", "Optimistic")
colnames(ag_high_2) <- c("Year", "Before", "Conservative", "Optimistic")
colnames(ag_high_3) <- c("Year", "Before", "Conservative", "Optimistic")

l_min <- min(data.frame(ag_low_1)$Before, data.frame(ag_low_2)$Before, data.frame(ag_low_3)$Before)
l_max <- max(data.frame(ag_low_1)$Optimistic, data.frame(ag_low_2)$Optimistic, data.frame(ag_low_3)$Optimistic)
m_min <- min(data.frame(ag_med_1)$Before, data.frame(ag_med_2)$Before, data.frame(ag_med_3)$Before)
m_max <- max(data.frame(ag_med_1)$Optimistic, data.frame(ag_med_2)$Optimistic, data.frame(ag_med_3)$Optimistic)
h_min <- min(data.frame(ag_high_1)$Before, data.frame(ag_high_2)$Before, data.frame(ag_high_3)$Before)
h_max <- max(data.frame(ag_high_1)$Optimistic, data.frame(ag_high_2)$Optimistic, data.frame(ag_high_3)$Optimistic)

min <- min(l_min, m_min, h_min)
max <- max(l_max, m_max, h_max)


l1 <- ggplot(data=data.frame(ag_low_1)) +
  geom_line(aes(x=Year,Before, col="Before Intervention")) +
  geom_line(aes(x=Year,Conservative, col="Conservative")) +
  geom_line(aes(x=Year,Optimistic, col="Optimistic")) +
  scale_y_continuous(limits = c(min, max)) +
  scale_colour_discrete(name='Scenario') +
  labs(x="Year", y="Profits") +
  theme_bw() +
  ggtitle("<span style='font-size: 8pt;'>Optimistic Expenses with Low Interest") +
  theme(plot.title = element_markdown())
l2 <- ggplot(data=data.frame(ag_low_2)) +
  geom_line(aes(x=Year,Before, col="Before Intervention")) +
  geom_line(aes(x=Year,Conservative, col="Conservative")) +
  geom_line(aes(x=Year,Optimistic, col="Optimistic")) +
  scale_y_continuous(limits = c(min, max)) +
  scale_colour_discrete(name='Scenario') +
  labs(x="Year", y="Profits") +
  theme_bw() +
  ggtitle("<span style='font-size: 8pt;'>Optimistic Expenses with Moderate Interest") +
  theme(plot.title = element_markdown())
l3 <- ggplot(data=data.frame(ag_low_3)) +
  geom_line(aes(x=Year,Before, col="Before Intervention")) +
  geom_line(aes(x=Year,Conservative, col="Conservative")) +
  geom_line(aes(x=Year,Optimistic, col="Optimistic")) +
  scale_y_continuous(limits = c(min, max)) +
  scale_colour_discrete(name='Scenario') +
  labs(x="Year", y="Profits") +
  theme_bw() +
  ggtitle("<span style='font-size: 8pt;'>Optimistic Expenses with High Interest") +
  theme(plot.title = element_markdown())

m1 <- ggplot(data=data.frame(ag_med_1)) +
  geom_line(aes(x=Year,Before, col="Before Intervention")) +
  geom_line(aes(x=Year,Conservative, col="Conservative")) +
  geom_line(aes(x=Year,Optimistic, col="Optimistic")) +
  scale_y_continuous(limits = c(min, max)) +
  scale_colour_discrete(name='Scenario') +
  labs(x="Year", y="Profits") +
  theme_bw() +
  ggtitle("<span style='font-size: 8pt;'>Central Expenses with Low Interest") +
  theme(plot.title = element_markdown())
m2 <- ggplot(data=data.frame(ag_med_2)) +
  geom_line(aes(x=Year,Before, col="Before Intervention")) +
  geom_line(aes(x=Year,Conservative, col="Conservative")) +
  geom_line(aes(x=Year,Optimistic, col="Optimistic")) +
  scale_y_continuous(limits = c(min, max)) +
  scale_colour_discrete(name='Scenario') +
  labs(x="Year", y="Profits") +
  theme_bw() +
  ggtitle("<span style='font-size: 8pt;'>Central Expenses with Moderate Interest") +
  theme(plot.title = element_markdown())
m3 <- ggplot(data=data.frame(ag_med_3)) +
  geom_line(aes(x=Year,Before, col="Before Intervention")) +
  geom_line(aes(x=Year,Conservative, col="Conservative")) +
  geom_line(aes(x=Year,Optimistic, col="Optimistic")) +
  scale_y_continuous(limits = c(min, max)) +
  scale_colour_discrete(name='Scenario') +
  labs(x="Year", y="Profits") +
  theme_bw() +
  ggtitle("<span style='font-size: 8pt;'>Central Expenses with High Interest") +
  theme(plot.title = element_markdown())

h1 <- ggplot(data=data.frame(ag_high_1)) +
  geom_line(aes(x=Year,Before, col="Before Intervention")) +
  geom_line(aes(x=Year,Conservative, col="Conservative")) +
  geom_line(aes(x=Year,Optimistic, col="Optimistic")) +
  scale_y_continuous(limits = c(min, max)) +
  scale_colour_discrete(name='Scenario') +
  labs(x="Year", y="Profits") +
  theme_bw() +
  ggtitle("<span style='font-size: 8pt;'>Pessimistic Expenses with Low Interest") +
  theme(plot.title = element_markdown())
h2 <- ggplot(data=data.frame(ag_high_2)) +
  geom_line(aes(x=Year,Before, col="Before Intervention")) +
  geom_line(aes(x=Year,Conservative, col="Conservative")) +
  geom_line(aes(x=Year,Optimistic, col="Optimistic")) +
  scale_y_continuous(limits = c(min, max)) +
  scale_colour_discrete(name='Scenario') +
  labs(x="Year", y="Profits") +
  theme_bw() +
  ggtitle("<span style='font-size: 8pt;'>Pessimistic Expenses with Moderate Interest") +
  theme(plot.title = element_markdown())
h3 <- ggplot(data=data.frame(ag_high_3)) +
  geom_line(aes(x=Year,Before, col="Before Intervention")) +
  geom_line(aes(x=Year,Conservative, col="Conservative")) +
  geom_line(aes(x=Year,Optimistic, col="Optimistic")) +
  scale_y_continuous(limits = c(min, max)) +
  scale_colour_discrete(name='Scenario') +
  labs(x="Year", y="Profits") +
  theme_bw() +
  ggtitle("<span style='font-size: 8pt;'>Pessimistic Expenses with High Interest") +
  theme(plot.title = element_markdown())

ggpubr::ggarrange(l1, l2, l3, m1, m2, m3, h1, h2, h3, nrow=3, ncol=3, common.legend=T, legend = "bottom")

central <- ggplot(data=data.frame(ag_med_2)) +
  geom_line(aes(x=Year,Before, col="Before Intervention")) +
  geom_line(aes(x=Year,Conservative, col="Conservative")) +
  geom_line(aes(x=Year,Optimistic, col="Optimistic")) +
  scale_colour_discrete(name='Scenario') +
  labs(x="Year", y="Profits", title = "Profit for Central Expenses with Central Interest Estimate") +
  theme_bw()
