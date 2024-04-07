library(ggplot2)
library(dplyr)
library(readxl)

clean <- read.csv("cleaned_superlife.csv")

clean$Underwriting.Class <- ifelse(clean$Underwriting.Class == "very low risk", 1,
                                   ifelse(clean$Underwriting.Class == "low risk", 2,
                                          ifelse(clean$Underwriting.Class == "moderate risk", 3,
                                                 4)))
clean$Urban.vs.Rural <- ifelse(clean$Urban.vs.Rural == "Urban", 1, 2)

# Totals
total_payout <- sum(clean$Face.amount[!is.na(clean$Death.indicator)])

surrender_fee <- 0.1
total_surrender <- sum(surrender_fee * clean$Face.amount[!is.na(clean$Lapse.Indicator)])

#Underwriting class plots
class_urban <- as.data.frame(table(clean$Underwriting.Class, clean$Urban.vs.Rural))
names(class_urban)[names(class_urban) == "Var1"] <- "Underwriting.Risk"
names(class_urban)[names(class_urban) == "Var2"] <- "Urban.Rural"
ggplot(class_urban, aes(fill=Underwriting.Risk, y=Freq, x=Urban.Rural)) +
  geom_bar(position="fill", stat="identity")

class_region <- as.data.frame(table(clean$Underwriting.Class, clean$Region))
names(class_region)[names(class_region) == "Var1"] <- "Underwriting.Risk"
names(class_region)[names(class_region) == "Var2"] <- "Region"
ggplot(class_region, aes(fill=Underwriting.Risk, y=Freq, x=Region)) +
  geom_bar(position="fill", stat="identity")

#Deaths urban plot
clean$Death.indicator <- ifelse(is.na(clean$Death.indicator), 0, 1)
deaths_urban <- as.data.frame(table(clean$Death.indicator, clean$Urban.vs.Rural))
names(deaths_urban)[names(deaths_urban) == "Var1"] <- "Death"
names(deaths_urban)[names(deaths_urban) == "Var2"] <- "Urban.Rural"
ggplot(deaths_urban, aes(fill = Death, y=Freq, x=Urban.Rural)) +
  geom_bar(position="fill", stat="identity")

econ <- read_excel("lumaria-economic-data.xlsx")[5:67, 1:5]
names(econ) <- c("Year", "Inflation", "Overnight", "1-yr", "10-yr")
econ <- econ[2:66,]

mort <- read_excel("srcsc-2024-lumaria-mortality-table.xlsx")[8:127, 1:2]
names(mort) <- c("x", "q")
mort$x <- as.numeric(mort$x)
mort$q <- as.numeric(mort$q)
mort$p <- 1 - mort$q

ggplot(clean, aes(x=Smoker.Status, y=Underwriting.Class)) +
  geom_violin()
