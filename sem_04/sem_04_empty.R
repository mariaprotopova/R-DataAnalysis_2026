library(tidyverse)
library(rstatix)

###============= Comparing Means =============
data <- tibble(males = rnorm(30, mean=175, sd=15),
               females = rnorm(30, mean=160, sd=10))

summary(data)

data %>% pivot_longer(cols=colnames(data)) %>%
  ggplot(aes(name, value, fill = name)) +
  geom_boxplot()

t_val <- (mean(data$males) - mean(data$females)) /
  sqrt(sd(data$males)**2/nrow(data) + sd(data$females)**2/nrow(data)); t_val

t.test(data$males, data$females)

data <- tibble(males = rnorm(30, mean=108, sd=13),
               females = rnorm(30, mean=111, sd=10))

summary(data)
data %>% pivot_longer(cols=colnames(data)) %>%
  ggplot(aes(name, value, fill = name)) +
  geom_boxplot()


t.test(...)

###============= Normality Tests =============

data <- read_csv('StudentsPerformance_UPD.csv')
View(data)

table(data$gender, data$`parental level of education`)

data %>% filter(gender == 'female',
                `parental level of education` == "master's degree") -> data_part_small

# which test to choose to check the nirmality assumption?

# let's plot
ggplot(data_part_small, aes(`reading score`)) +
  geom_density()

data %>% filter(gender == 'female',
                `parental level of education` == "associate's degree") -> data_part_large

# which test to choose to check the nirmality assumption?

# let's plot
ggplot(data_part_large, aes(`reading score`)) +
  geom_density()

table(data$gender)

ks.test(data[data$gender =='female',]$`math score`, y='pnorm')
ks.test(data[data$gender =='male',]$`math score`, y='pnorm')

# how to compare means? which test to use?

ggplot(data, aes(gender, `math score`, fill = gender)) +
  geom_boxplot()

summary(data[data$gender=='male',]$IQ)
summary(data[data$gender=='female',]$IQ)

ks.test(data[data$gender=='male',]$IQ, 'pnorm')
ks.test(data[data$gender=='female',]$IQ, 'pnorm')

# how to compare means? which test to use?

ks.test(data[data$gender=='female',]$`writing score`, 'pnorm')
ks.test(data[data$gender=='female',]$`reading score`, 'pnorm')


# how to compare means? which test to use?
