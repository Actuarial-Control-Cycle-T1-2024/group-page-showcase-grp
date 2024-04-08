library(dplyr)
data <- read.csv("cleaned_inforce_data.csv")

# Every person who has 20 year term insurance should lapse eventually
nrow(data %>% filter(data$Policy.type=="T20") %>% filter(is.na(Lapse.Indicator)))

# Lapse rates for T20 policies
na.omit(data %>% filter(data$Policy.type=="T20") %>%
          group_by(Issue.year) %>%
          summarise(Deaths=sum(Death.indicator), 
                    Lapses=sum(Lapse.Indicator),
                    Alive=n()))
# deaths panel
deaths_panel <- na.omit(data %>% filter(data$Policy.type=="T20") %>%
                          group_by(Issue.year, Year.of.Death) %>%
                          summarise(Deaths=sum(Death.indicator)))

# lapses panel
lapses_panel <- na.omit(data %>% filter(data$Policy.type=="T20") %>%
                          group_by(Issue.year, Year.of.Lapse) %>%
                          summarise(Deaths=sum(Lapse.Indicator)))

# Estimation of lapse rates for each duration
t20_data <- data %>% filter(data$Policy.type=="T20")
lapse_rates <- t20_data %>%
  group_by(Lapse.Duration) %>%
  summarise(Number.of.Lapses = sum(Lapse.Indicator, na.rm=T)) %>%
  mutate(exposed_to_risk = sapply(Lapse.Duration, function(x) sum(t20_data$duration >= x - 1))) %>%
  mutate(lapse_rate=Number.of.Lapses/exposed_to_risk*100)

# Estimation of death rate for each duration
death_rates <- t20_data %>%
  group_by(Death.Duration) %>%
  summarise(Number.of.Deaths = sum(Death.indicator, na.rm=T)) %>%
  mutate(exposed_to_risk = sapply(Death.Duration, function(x) sum(t20_data$duration >= x - 1))) %>%
  mutate(death_rates=Number.of.Deaths/exposed_to_risk*100)

# No lapses for Single premium whole life (as it involves lump sum payment in advance
# rather than recurring)
data %>% filter(data$Policy.type=="SPWL") %>% filter(!is.na(Lapse.Indicator))

