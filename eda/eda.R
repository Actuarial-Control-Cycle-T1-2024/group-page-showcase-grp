setwd("C:/Users/andre/Documents/actl4001/soaRProject")
library(dplyr)
# plots:
library(ggplot2)
library(viridis) 
options(scipen=999)


data <- read.csv("inforce-dataset2.csv")

#Finding the number of na in the death indicator 
sum(is.na(data$Death.indicator))
#Finding number of the death indicator to compare the number of deaths
sum(data$Death.indicator == 1, na.rm = TRUE)

columns_to_factor <- c("Policy.type", "Sex", "Smoker.Status", "Underwriting.Class",
                       "Urban.vs.Rural", "Region", "Distribution.Channel",
                       "Death.indicator", "Lapse.Indicator","Cause.of.Death")
colnames(data)[1] <- "Policy.number"
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

# Alter Death.indicator to use 0's, not NA
data$Death.indicator <- ifelse(is.na(data$Death.indicator), 0, 1)

# some data showing exposure to a contract.
data <- data %>% 
  # Every record will give a time (checked)
  mutate(time = ifelse(!is.na(Age.at.Death), Age.at.Death,
                       ifelse(!is.na(Age.at.Lapse), Age.at.Lapse,
                              2023 - Issue.year + Issue.age)))

# Time exposure
data <- data %>% 
  mutate(holding_time = ifelse(!is.na(Age.at.Death), Age.at.Death - Issue.year,
                               ifelse(!is.na(Age.at.Lapse), Age.at.Lapse - Issue.year,
                                      2023 - Issue.year)))

# rural vs urban  ----
# death ages
data %>% filter(Death.indicator == 1) %>%
  ggplot( aes(x=Urban.vs.Rural, y=Age.at.Death, fill=Urban.vs.Rural)) +
  geom_violin() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Rural vs urban death age") +
  xlab("")
# exposure time (death age, lapse, and present day if still holding)
data %>%
  ggplot( aes(x=Urban.vs.Rural, y=time, fill=Urban.vs.Rural)) +
  geom_violin() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Exposure to policies in rural and urban") +
  xlab("")

# distribution channels for urban and rural (near identical)
prop.table(table(data %>% filter(Urban.vs.Rural=="Rural") %>% select(Urban.vs.Rural, Distribution.Channel)))
prop.table(table(data %>% filter(Urban.vs.Rural=="Urban") %>% select(Urban.vs.Rural, Distribution.Channel)))

# how much rural and urban
prop.table(table(data %>% select(Urban.vs.Rural , Region)))
regions = as.data.frame(data %>% select(Urban.vs.Rural, Region)  %>% group_by(Region, Urban.vs.Rural) %>% summarise(citizens = n()))
ggplot(regions, aes(fill=Urban.vs.Rural, y=citizens, x=Region)) + 
  geom_bar(position="stack", stat="identity")
ggplot(regions, aes(fill=Urban.vs.Rural, y=citizens, x=Region)) + 
  geom_bar(position="fill", stat="identity")

data %>%  filter(Policy.type == "T20") %>% select(Issue.age) %>% summary()
data %>%  filter(Policy.type == "SPWL") %>% select(Issue.age) %>% summary()

# face amount ----
data %>% filter(Policy.type == "T20") %>% filter(Death.indicator == 1) %>%  select(Face.amount) %>% summary()
t20.claimsev = as.vector(data %>% filter(Policy.type == "T20") %>% filter(Death.indicator == 1) %>%  select(Face.amount))
hist(t20.claimsev[[1]])
data %>% filter(Policy.type == "SPWL") %>% filter(Death.indicator == 1) %>%  select(Face.amount) %>% summary()

data %>% filter(Policy.type == "T20") %>%
  select(Policy.type, Lapse.Indicator) %>% 
  group_by(Lapse.Indicator) %>% 
  summarise(n=n())

data %>% filter(Policy.type == "T20") %>% filter(Death.indicator == 0) %>% select(Policy.type, Lapse.Indicator) %>% group_by(Lapse.Indicator) %>% summarise(n=n())
data %>% filter(Policy.type == "T20") %>% filter(Death.indicator == 0) %>% filter(Lapse.Indicator == 1) %>% select(Lapse.Indicator)
 #nobody who doesn't die nor lapse.
# assume that the final year is not actually a lapse.


temp <- data%>% group_by(Age.at.Death) %>% summarise(n = n())
temp <- data%>% filter(Underwriting.Class == "very low risk") %>% group_by(Age.at.Death) %>% filter(!is.na(Age.at.Death)) %>% summarise(n = n())
temp[2]

data %>%select(time) %>% summary()
