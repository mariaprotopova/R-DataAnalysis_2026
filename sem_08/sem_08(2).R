### Data taken from https://osf.io/preprints/psyarxiv/bmh7q_v1
library(tidyverse)
library(lme4)
library(lmerTest)

data <- read_csv('kenya_loneliness.csv')
data$UCLA_Total <- data$UCLA_Total + rnorm(nrow(data), 0, 1)
data %>% group_by(Gender, Tribe) %>%
  summarize(UCLA_Total=mean(UCLA_Total),
            GAD_Total=mean(GAD_Total)) %>% 
  ungroup() %>% 
  ggplot(aes(UCLA_Total, GAD_Total)) +
  geom_point() +
  geom_smooth(method='lm', se=F)+
  theme_minimal()

ggplot(data, aes(UCLA_Total, GAD_Total, color = Tribe)) +
  geom_point(size=.5, alpha=.5) +
  geom_smooth(method='lm', se=F)+
  facet_wrap(~Tribe) +
  theme_minimal() +
  theme(legend.position="none")

data %>% group_by(Gender, Tribe) %>% summarize(UCLA_Total=mean(UCLA_Total),
                                       GAD_Total=mean(GAD_Total),
                                       PHQ_Total = mean(PHQ_Total)) %>%
  ungroup() -> d
model_0 <- lm(UCLA_Total ~ GAD_Total + PHQ_Total + Gender, d)
summary(model_0)
model <- lmer(UCLA_Total ~ GAD_Total + PHQ_Total + Gender + (1|Tribe), data)
summary(model)


full.diag <- fortify.merMod(model)
#Проверка нормальности распределения остатков
ggplot(aes(sample = .resid), data = full.diag) +
  geom_qq() +
  geom_qq_line()

ggplot(aes(x = .fitted, y = .scresid), data = full.diag) +
  geom_point() +
  geom_smooth(method = 'loess')

full.data <- full.diag[(full.diag$.scresid > -4) & (full.diag$.scresid < 4),]
