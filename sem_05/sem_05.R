library(tidyverse)

df <- read_csv('ScreenTime vs MentalWellness.csv')
df %>% filter(gender %in% c('Female', 'Male')) ->df
View(df)
# ================= Covariance =================
ggplot(df, aes(screen_time_hours, sleep_hours)) +
  geom_point() +
  geom_smooth(method='lm')

var1 <- df$screen_time_hours

ggplot(data=NULL) +
  geom_density(aes(var1)) +
  geom_density(data=NULL, aes(rnorm(length(var1), mean(var1), sd(var1))), linetype = 'dashed')

var2 <- df$sleep_hours

ggplot(NULL) +
  geom_density(aes(var2)) +
  geom_density(aes(rnorm(length(var2), mean(var2), sd(var2))), linetype = 'dashed')


covariance <- 1/(length(var1)-1)*sum((var1-mean(var1))*(var2-mean(var2)))

cov(df$sleep_hours, df$screen_time_hours)

correlation <- 1/(length(var1)-1)*sum(scale(var1)*scale(var2)); correlation

cor(var1, var2)


# ================= Correlation tests =================
ks.test(var1, 'pnorm', mean(var1), sd(var1))
ks.test(var2, 'pnorm', mean(var2), sd(var2))

cor.test(var1, var2)
# ================= Spearman correlation =================
var1 <- df$age
ks.test(var1, 'pnorm', mean(var1), sd(var1))
var2 <- df$mental_wellness_index_0_100
ks.test(var2, 'pnorm', mean(var2), sd(var2))

cor.test(var1, var2, method='spearman')
ggplot(df, aes(age, mental_wellness_index_0_100)) +
  geom_point() +
  geom_smooth(method='lm')


# ================= Kendall correlation =================
var1 <- df$sleep_quality_1_5
var2 <- df$stress_level_0_10

cor.test(var1, var2, method='kendall')
ggplot(df, aes(sleep_quality_1_5, stress_level_0_10)) +
  geom_point() +
  geom_smooth(method='lm')

# ================= Chi-square test =================
chi <- chisq.test(df$work_mode, df$gender)
chi$expected
chi

chi <- chisq.test(df$gender, df$occupation)
chi$expected
chi