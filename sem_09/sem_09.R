library(tidyverse)
# install.packages('EFAtools')
library(EFAtools)
library(performance)
library(openxlsx)
library(psych)
library(MASS)
theme_set(theme_minimal())

data <- read_csv('chocolates_data.csv')

data_corr <- data[, sapply(data, class) == 'numeric']
data_corr <- data_corr[, 1:11]

corrplot::corrplot(
  cor(data_corr, method='spearman'),
  type = "upper",
  p.mat = corrplot::cor.mtest(data_corr, method='spearman')$p,
  sig.level = 0.05
)

model <- lm(Review_1_to_5 ~ ., data[, -c(1,2,3)])
car::vif(model)
plot(model)
summary(model)

check_sphericity_bartlett(data_corr)

check_kmo(data_corr)

pca <- prcomp(data_corr, scale. = TRUE) # обязательно стандартизовать переменные!
summary(pca)

eigenvalues <- eigen(cor(scale(data_corr)))
ggplot(data=NULL, aes(x=1:length(eigenvalues$values), y=eigenvalues$values)) +
  geom_point(size = 2) +
  geom_line(aes(group = 1)) +
  xlab('Principal components') + ylab('Eigenvalues')+
  ggtitle('Scree plot') +
  geom_hline(yintercept = 1, linetype='dashed', color='red')

fa.parallel(data_corr, fa="fa")
efa <- factanal(data_corr, factors=3, rotation = 'varimax', scores = 'regression')

factors <- as.data.frame(ifelse(abs(efa$loadings) < .5, 0, round(efa$loadings, 4)))

factors[order(factors$Factor1, decreasing = TRUE),]


colnames(efa$scores) <- c(...)

data <- cbind(data, efa$scores)
View(data)
colnames(data)
model_2 <- lm(Review_1_to_5 ~ Price + Factor1 + Factor2 + Factor3 + Type, data)

car::vif(model_2)

model_3 <- lm(scale(Review_1_to_5) ~ scale(Price) + scale(Factor1) +
                scale(Factor2) + scale(Factor3), data)

car::vif(model_3)
plot(model_3)
summary(model_3)
