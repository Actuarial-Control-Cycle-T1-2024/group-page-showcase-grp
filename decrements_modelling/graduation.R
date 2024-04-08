library(dplyr)
library(ggplot2)

# Graduation using reference to standard table
qs <- read.csv('qs_smoking_underwriting_class.csv')[1:61, ]
names(qs)
ggplot(qs) + 
  geom_line(aes(x=time, y=qx))

# q1 - to be graduated, q2 - reference life table
minimize_function <- function(params, q1, q2) {
  penalty <- 10^6
  qx_grad <-  f(q2, params)
  sum((q1 - qx_grad)^2) + penalty*ifelse(any(qx_grad < 0), 1, 0)
}

inforce_data <- read.csv('superlife_inforce_data.csv')

inforce_data %>% group_by(Smoker.Status, Underwriting.Class) %>% summarise(count=n())
  
f <- function(q, params) {
  a <- params[1]
  b <- params[2]
  a + b*q
}

# Initial guess for parameters a and b
initial_guess <- c(1, 1)

# Perform optimization
params_SH <- optim(initial_guess, minimize_function, q1=qs$SH, q2=qs$qx)$par
params_SM <- optim(initial_guess, minimize_function, q1=qs$SM, q2=qs$qx)$par
params_NSH <- optim(initial_guess, minimize_function, q1=qs$NSH, q2=qs$qx)$par
params_NSM <- optim(initial_guess, minimize_function, q1=qs$NSM, q2=qs$qx)$par
params_NSL <- optim(initial_guess, minimize_function, q1=qs$NSL, q2=qs$qx)$par
params_NSVL <- optim(initial_guess, minimize_function, q1=qs$NSVL, q2=qs$qx)$par

rss_std <- function(a, b) {
  sum((a-b)^2)/var(b)
}

rss_std(qs$SH, qs$qx_SH_g)
rss_std(qs$SM, qs$qx_SM_g)
rss_std(qs$NSH, qs$qx_NSH_g)
rss_std(qs$NSM, qs$qx_NSM_g)
rss_std(qs$NSL, qs$qx_NSL_g)
rss_std(qs$NSVL, qs$qx_NSVL_g)

max(qs$SH-qs$qx_SH_g)

var(qs$NSL-qs$qx_NSL_g)
max(qs$SH-qs$qx_SH_g)
max(qs$NSL-qs$qx_NSL_g)

qs <- qs %>% mutate (
  qx_SH_g=f(qs$qx, params_SH),
  qx_SM_g=f(qs$qx, params_SM),
  qx_NSH_g=f(qs$qx, params_NSH),
  qx_NSM_g=f(qs$qx, params_NSM),
  qx_NSL_g=f(qs$qx, params_NSL),
  qx_NSVL_g=f(qs$qx, params_NSVL)
)

qs_df <- qs %>% select(qx_SH_g, qx_SM_g, qx_NSH_g, qx_NSM_g, qx_NSL_g, qx_NSVL_g)
  
library(tidyr)
qs.long <- gather(qs_df, key="variable", value="value", -time)
ggplot(qs.long, aes(x=time, y=value, color = variable)) +
  geom_line() +
  scale_color_discrete(name = "Subgroups",
                     labels = c("NSH", "NSL", "NSM", "NSVL", "SH", "SM")) +
  labs(x="Age", y="Smoothed qx") +
  theme_bw()

p_SH <- ggplot(qs) +
  geom_line(aes(x=time, y=SH), col="red") +
  geom_line(aes(x=time, y=qx_SH_g), col="blue") +
  labs(x="Age", y="qx_SH") +
  scale_color_manual(name='', values=c("red", "blue"), labels=c("Smoothed", "Crude")) +
  theme_bw()

p_SM <- ggplot(qs) +
  geom_line(aes(x=time, y=SM), col="red") +
  geom_line(aes(x=time, y=qx_SM_g), col="blue") +
  labs(x="Age", y="qx_SM") +
  scale_color_manual(name='', values=c("red", "blue"), labels=c("Smoothed", "Crude")) +
  theme_bw()


p_NSH <- ggplot(qs) +
  geom_line(aes(x=time, y=NSH), col="red") +
  geom_line(aes(x=time, y=qx_NSH_g), col="blue") + 
  labs(x="Age", y="qx_NSH") +
  scale_color_manual(name='', values=c("red", "blue"), labels=c("Smoothed", "Crude")) +
  theme_bw()

p_NSM <- ggplot(qs) +
  geom_line(aes(x=time, y=NSM), col="red") +
  geom_line(aes(x=time, y=qx_NSM_g), col="blue") +
  labs(x="Age", y="qx_NSM") +
  scale_color_manual(name='', values=c("red", "blue"), labels=c("Smoothed", "Crude")) +
  theme_bw()

p_NSL <- ggplot(qs) +
  geom_line(aes(x=time, y=NSL), col="red") +
  geom_line(aes(x=time, y=qx_NSL_g), col="blue") +
  labs(x="Age", y="qx_NSL") +
  scale_color_manual(name='', values=c("red", "blue"), labels=c("Smoothed", "Crude")) +
  theme_bw()

p_NSVL <- ggplot(qs) +
  geom_line(aes(x=time, y=NSVL), col="red") +
  geom_line(aes(x=time, y=qx_NSVL_g), col="blue") +
  labs(x="Age", y="qx_NSVL") +
  scale_color_manual(name='', values=c("red", "blue"), labels=c("Smoothed", "Crude")) +
  theme_bw()

p_SM

library(ggpubr)
ggarrange(p_SH, p_SM, p_NSH, p_NSM, p_NSL, p_NSVL, ncol=2, nrow=3)

write.csv(qs, '~/ACTL4001/graduated_qs.csv')

