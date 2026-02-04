library(tidyverse)
library(Matrix)

theme_set(theme_bw())
data <- read_csv('ScreenTime vs MentalWellness.csv')
View(data)
Y <- as.matrix(data$productivity_0_100)

X <- data$sleep_hours

X <- cbind(rep(1, length(X)), X)

b <- solve((t(X) %*% X)) %*% t(X) %*% Y
model <- lm(productivity_0_100 ~ sleep_hours, data=data)
summary(model)
res <- tibble('lm_coefs' = model$coefficients,
              'b' = b)
res$lm_coefs <- model$coefficients
res$b <- b
print(res)

colnames(data)

model <- lm(scale(sleep_hours) ~ scale(leisure_screen_hours), data)
summary(model)

ggplot(data, aes(leisure_screen_hours, sleep_hours)) +
  geom_point() +
  geom_smooth(method='lm')

cor(data$sleep_hours, data$leisure_screen_hours)

par(mfrow=c(2,2))
plot(model)

model_1 <- lm(social_hours_per_week ~ stress_level_0_10, data=data)
summary(model_1)


model_1 <- lm(scale(social_hours_per_week) ~ scale(stress_level_0_10), data=data)
summary(model_1)
plot(model_1)
