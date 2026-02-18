library(tidyverse)
library(car)
library(pROC) # install
theme_set(theme_bw())

data <- readxl::read_excel('burnout.xlsx')
View(data)
str(data)

data$burnout <- ifelse(data$burnout=='Burnt Out', 1, 0)
model0 <- lm(burnout  ~ ., data)
summary(model0) 

data %>%
  mutate(
    L = loc + cope + teaching + research + pastoral
  ) %>%
  ggplot(aes(L, burnout)) +
  geom_point(size = 2, alpha = .3) +
  geom_point(aes(y = model0$fitted.values),
             color = 'pink', alpha = 0.8) +
  geom_smooth(aes(y = model0$fitted.values), method = 'lm')

y <- (model0$fitted.values-min(model0$fitted.values))/(max(model0$fitted.values)-min(model0$fitted.values))
data %>%
  mutate(
    L = loc + cope + teaching + research + pastoral
  ) %>% ggplot(aes(L, burnout)) +
  geom_point(size = 2, alpha = .3) +
  geom_point(aes(y = y),
             color = 'darkred', alpha = 0.5) +
  geom_smooth(aes(y=y),method='lm')


y <- model0$fitted.values

data %>%
  mutate(
    L = loc + cope + teaching + research + pastoral
  ) %>% ggplot(aes(L, burnout)) +
  geom_point(size = 2, alpha = .3) +
  geom_point(aes(y = exp(y) / (1 + exp(y))),
             color = 'darkred', alpha = 0.5) +
  geom_smooth(aes(y = exp(y) / (1 + exp(y))))

y <- (model0$fitted.values-min(model0$fitted.values))/(max(model0$fitted.values)-min(model0$fitted.values))
odds <- y / (1-y) 

data %>%
  mutate(
    L = loc + cope + teaching + research + pastoral
  ) %>% ggplot(aes(L, burnout)) +
  # geom_point(size = 2, alpha = .3) +
  geom_point(aes(y = odds),
             color = 'darkred', alpha = 0.5) +
  geom_smooth(aes(y = odds))


data %>%
  mutate(
    L = loc + cope + teaching + research + pastoral
  ) %>% ggplot(aes(L, burnout)) +
  geom_point(aes(y = log(odds)),
             color = 'darkred', alpha = 0.5) +
  geom_smooth(aes(y = log(odds)), method='lm')



### GLM

model <- glm(burnout ~ ., data, family = binomial(link = 'logit'))

logdiag <- tibble(fitted = fitted(model, type = 'response'),
                  resid = resid(model, type = 'pearson'))

logdiag %>% ggplot(aes(fitted, resid)) +
  geom_point() +
  geom_smooth(method='loess') +
  geom_hline(yintercept = 0)

pchisq(sum(residuals(model, type = "deviance")^2), model$df.residual)
# if the result of the test is > .05, then NO heteroscedasticity is in the model

summary(model)

model_null <- glm(burnout ~ 1, data, family = binomial(link = 'logit'))
anova(model_null, model, test = 'Chi')
AIC(model_null); AIC(model)
BIC(model_null); BIC(model)

result <- summary(model)
coefs <- as.data.frame(result$coefficients)
coefs$Odds_ratio <- exp(coefs$Estimate)
coefs$`Pr(>|z|)` <- ifelse(coefs$`Pr(>|z|)` < .05, '*', '')

coefs


drop1(model, test='Chi')
model2 <- update(model, . ~ . -degree)
anova(model, model2, test = 'Chi')
AIC(model); AIC(model2)
summary(model2)




drop1(model2, test='Chi')
model3 <- update(model2, . ~ . -gender)
anova(model2, model3, test = 'Chi')
AIC(model2); AIC(model3)

summary(model3)


drop1(model3, test='Chi')
model4 <- update(model3, . ~ . -research)
anova(model3, model4, test = 'Chi')
AIC(model3); AIC(model4)
drop1(model4, test='Chi')

###
car::vif(model4)

ggplot(data, aes(teaching, cope)) +
  geom_point() +
  geom_smooth()
ks.test(scale(data$cope), 'pnorm')
ks.test(scale(data$teaching), 'pnorm')
cor.test(data$teaching, data$cope, method='spearman')

summary(model4)

logdiag <- tibble(fitted = fitted(model4, type = 'response'),
                  resid = resid(model4, type = 'pearson'))

logdiag %>% ggplot(aes(fitted, resid)) +
  geom_point() +
  geom_smooth(method='loess') +
  geom_hline(yintercept = 0)

pchisq(sum(residuals(model4, type = "deviance")^2), model4$df.residual)
# if the result of the test is > .05,
# then NO heteroscedasticity is in the model

drop1(model4, test='Chi')
model5 <- update(model4, . ~ . -teaching)
anova(model4, model5, test = 'Chi')

car::vif(model5)
plot(model5)

fortify(model5) -> fort
View(fort)

ggplot(fort, aes(seq(1, length(.cooksd)),.cooksd)) +
  geom_point() +
  geom_hline(yintercept = 0.5, color='red') +
  geom_hline(yintercept = 1, color='purple')

pchisq(sum(residuals(model5, type = "deviance")^2), model5$df.residual)
# if the result of the test is > .05,
# then NO heteroscedasticity is in the model

model6 <- glm(burnout ~ loc + cope + teaching*pastoral, data, family = binomial(link = 'logit'))
car::vif(model6)

logdiag <- tibble(fitted = fitted(model6, type = 'response'),
                  resid = resid(model6, type = 'pearson'))

logdiag %>% ggplot(aes(fitted, resid)) +
  geom_point() +
  geom_smooth(method='loess') +
  geom_hline(yintercept = 0)

pchisq(sum(residuals(model6, type = "deviance")^2), model6$df.residual)

anova(model5, model6, test = 'Chi')


### Метрики качества модели
summary(model4)
predict(model4, data, type = 'response')

data$predicted_class <- ifelse(predict(model4, data, type = 'response') > .5, 1, 0)
data$predicted_proba <- predict(model4, data, type = 'response')

accuracy <- mean(data$burnout == data$predicted_class)

precision <- sum((data$burnout==1) & (data$predicted_class == 1)) /
  (sum(((data$burnout==1) & (data$predicted_class == 1))) + sum(((data$burnout==0) & (data$predicted_class == 1))))

table(data$burnout, data$predicted_class)

recall <- sum((data$burnout==1) & (data$predicted_class == 1)) /
  (sum(((data$burnout==1) & (data$predicted_class == 1))) + sum(((data$burnout==1) & (data$predicted_class == 0))))

recall

f1 <- 2 * precision * recall / (precision+recall)

auc(roc(data$burnout, data$predicted_proba))



model6 <- glm(burnout ~ , data, family = binomial(link = 'logit'))

df <- read_csv('parkinsons_disease_data.csv')
View(df)
str(df)
df$Diagnosis <- as.factor(df$Diagnosis)
df$Tremor <- as.factor(df$Tremor)
model6 <- glm(Diagnosis ~ Age + Tremor + MoCA + PhysicalActivity, df, family = binomial(link = 'logit'))

car::vif(model6)

plot(model6)

pchisq(sum(residuals(model6, type = "deviance")^2), model6$df.residual)
# if the result of the test is > .05,
# then NO heteroscedasticity is in the model
summary(model6)

model_null <- glm(Diagnosis ~ 1, df, family = binomial(link = 'logit'))
anova(model_null, model6, test = 'Chi')
AIC(model_null); AIC(model6)
BIC(model_null); BIC(model6)

result <- summary(model6)
coefs <- as.data.frame(result$coefficients)
coefs$Odds_ratio <- exp(coefs$Estimate)
coefs$`Pr(>|z|)` <- ifelse(coefs$`Pr(>|z|)` < .05, '*', '')

coefs
