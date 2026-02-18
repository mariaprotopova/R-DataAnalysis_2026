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
             color = 'darkred', alpha = 0.5) +
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
  # geom_point(size = 2, alpha = .3) +
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

result <- summary(model)
coefs <- as.data.frame(result$coefficients)
coefs$Odds_ratio <- exp(coefs$Estimate)
coefs$`Pr(>|z|)` <- ifelse(coefs$`Pr(>|z|)` < .05, '*', '')

coefs


drop1(model, test='Chi')
model2 <- update(model, . ~ . -degree)
anova(model, model2, test = 'Chi')
AIC(model); AIC(model2)

drop1(model2, test='Chi')
model3 <- update(model2, . ~ . -gender)
anova(model2, model3, test = 'Chi')
AIC(model3); AIC(model2)

drop1(model3, test='Chi')
model4 <- update(model3, . ~ . -research)
anova(model3, model4, test = 'Chi')
AIC(model3); AIC(model4)

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

drop1(model4, test='Chi')
model5 <- update(model4, . ~ . -pastoral)
anova(model4, model5, test = 'Chi')

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
