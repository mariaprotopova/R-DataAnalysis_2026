### Data taken from https://kristopherkyle.github.io/IntroQuantALRM/8_Linear_Mixed_Effects_Models.html
library(tidyverse)
library(lme4)
library(lmerTest)

data <- read_csv('RM_sample.csv')
View(data)

ggplot(data, aes(x = as.factor(Time), y = MLT)) +
  geom_boxplot()


ggplot(data, aes(x = as.factor(Time), y = MLT,group =Participant)) +
  geom_point(aes(color=Participant)) +
  geom_line(aes(color=Participant)) +
  theme_minimal()
  

### Simple Linear Regression, ignore participants

model_0 <- lm(MLT ~ Time, data)
summary(model_0)

ggplot(data, aes(Time, MLT)) +
  geom_point() +
  geom_smooth(method='lm')

### Simple Linear Regression, add participants

model_1 <- lm(MLT ~ Time + Participant, data)
summary(model_1)

ggplot(data, aes(Time, MLT, color = Participant)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE)

### Average across participants

df <- data %>% group_by(Time) %>% summarize(MLT = mean(MLT)) %>% ungroup()
View(df)

model_2 <- lm(MLT ~ Time, df)
summary(model_2)

ggplot(df, aes(Time, MLT)) +
  geom_point() +
  geom_smooth(method='lm')

### Mixed effects model

model_3 <- lmer(MLT ~ Time + (1|Participant), data)
summary(model_3)

as.data.frame(ranef(model_3)) -> rand_eff

data$lme_predict <- predict(model_3, re.form = ~ (1 | Participant))
predict(model_2)

data$lm_predict <- rep(predict(model_2), 9)
ggplot(data, aes(x = Time, y = MLT,group =Participant)) +
  geom_point(aes(color=Participant)) +
  geom_line(aes(y = lme_predict, color=Participant)) +
  facet_wrap(~Participant) +
  geom_line(aes(y=lm_predict), linetype='dashed') +
  theme_minimal()+
  theme(legend.position = 'none')

### LMER (with random intercept and slope)

model_4 <- lmer(MLT ~ Time + (Time|Participant), data)
summary(model_4)

as.data.frame(ranef(model_4)) -> rand_eff

data$lme_predict <- predict(model_3, re.form = ~ (1 | Participant))
predict(model_2)

data$lmer_predict <- predict(model_4)
ggplot(data, aes(x = Time, y = MLT,group =Participant)) +
  geom_point(aes(color=Participant)) +
  geom_line(aes(y = lmer_predict, color=Participant, linetype = 'dashed'), linewidth=2, alpha=.5) +
  geom_line(aes(y = lme_predict, color=Participant), linetype=2) +
  geom_line(aes(y = lm_predict)) +
  facet_wrap(~Participant) +
  theme_minimal()+
  theme(legend.position = 'none')


model_fort <- fortify.merMod(model_3)
ggplot(aes(sample = .resid), data = model_fort) +
  geom_qq() +
  geom_qq_line()

ggplot(data, aes(MLT**.01)) +
  geom_density()

lmbd <- car::powerTransform(model_3)$maximum; lmbd
lmmlog <- lmer(MLT**lmbd ~ Time + (1|Participant), data = data)
lmmlog_fort <- fortify.merMod(lmmlog)
ggplot(aes(sample = .resid), data = lmmlog_fort) +
  geom_qq() +
  geom_qq_line()
