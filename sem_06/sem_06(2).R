library(tidyverse)
library(MASS)

data <- read.csv('insurance.csv')
data
View(data)
colSums(is.na(data))
head(data)

data %>% ggplot(aes(age, bmi)) +
  geom_point() +
  geom_smooth(method='lm') +
  geom_hline(yintercept = mean(data$age))

model <- lm(bmi ~ age, data=data)

par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))

summary(model)

# ============================================================================== 

data %>% ggplot(aes(smoker, charges, fill = smoker)) + 
  geom_boxplot()

data %>% ggplot(aes(smoker, charges, fill = sex)) + 
  geom_boxplot()

model <- lm(charges ~ bmi, data=data)

par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))

### How to deal with non-linearity? (1) add/remove variables; (2) data transformation! ###
ggplot(data = NULL, aes(data$charges)) + geom_density()

ggplot(data = NULL, aes((data$charges)**.5)) + geom_density()

ggplot(data = NULL, aes(log(data$charges))) + geom_density()



MASS::boxcox(model)

# library(car)
lmbd <- car::powerTransform(model)

dv <- data$charges^lmbd$lambda

ggplot(data = NULL, aes(dv)) + geom_density()

summary(model)
data$charges_boxcox <- data$charges^lmbd$lambda
model <- lm(charges_boxcox ~ bmi, data=data)

par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))

summary(model)

model_scaled <- lm(scale(charges_boxcox) ~ scale(bmi), data=data)

summary(model_scaled)

### =============== Multiple linear regression =============== ###
data_corr <- data[, ifelse(sapply(data, class) != 'character', TRUE, FALSE)]
corrplot::corrplot(
  cor(data_corr),
  type = "upper",
  p.mat = corrplot::cor.mtest(data_corr)$p,
  sig.level = 0.01
)


model_1 <- lm(charges_boxcox ~ bmi + smoker, data)
par(mfrow=c(2,2))
plot(model_1)
par(mfrow=c(1,1))

### Check the assumptions
# 1) VIF (variance inflation factor)

car::vif(model_1)

#use drop1 to find one variable that can be removed
drop1(model_1, test = 'F')

# update your model
model_2 <- update(model_1, .~. -variable)
summary(model_2)

#and see if the model did not get worse!
anova(model, model_1)


### Linear regression with categorical variables
### What if we add categorical variables to the model?
head(data)

# Basically, nothing changes in the model-fitting part, but let's look at the output of the model:
model_2 <- lm(charges_boxcox ~ bmi + smoker + age, data)

car::vif(model_2)
par(mfrow=c(2,2))
plot(model_2)
par(mfrow=c(1,1))

summary(model_2)


### Add interaction
model_3 <- lm(charges_boxcox ~ bmi + smoker + sex, data)

car::vif(model_3)
par(mfrow=c(2,2))
plot(model_3)
par(mfrow=c(1,1))

summary(model_3)

model_4 <- lm(charges_boxcox ~ bmi + smoker * sex, data)

car::vif(model_4, type='predictor')
par(mfrow=c(2,2))
plot(model_4)
par(mfrow=c(1,1))

summary(model_4)

#### Pairwise comparissons
library(emmeans)
emmeans(model_4, ~ smoker)
pairs(emmeans(model_4, ~ smoker))

pairs(emmeans(model_4, ~ sex))
# NOTE: Results may be misleading due to involvement in interactions

pairs(emmeans(model_4, ~ sex*smoker))
# NOTE: Results may be misleading due to involvement in interactions

### How to choose the best model?
anova(model_3, model_4)

# AIC (lower AIC indicates smaller information loss and better fit)
AIC(model_3); AIC(model_4)

# BIC (lower BIC indicates smaller information loss and better fit)
BIC(model_3); BIC(model_4)

### Higher-order interactions

model_new <- lm(charges_boxcox ~ sex * smoker * region + bmi, data)
car::vif(model_new, type='predictor')
par(mfrow=c(2,2))
plot(model_4)
par(mfrow=c(1,1))

summary(model_new)
drop1(model_new, text = 'f') # drop 1 predictor and see if the model changes significantly

model_upd <- update(model_new, .~. -sex:smoker:region)
anova(model_new, model_upd)
# the difference is not significant --> we did not loose information by deleting this predictor

summary(model_upd)
drop1(model_upd, text = 'f') # drop 1 predictor and see if the model changes significantly

model_upd2 <- update(model_upd, .~. -sex:region)
anova(model_upd, model_upd2)

summary(model_upd2)
drop1(model_upd2, text = 'f') # drop 1 predictor and see if the model changes significantly

model_upd2 <- update(model_upd, .~. -sex:region)
anova(model_upd, model_upd2)
