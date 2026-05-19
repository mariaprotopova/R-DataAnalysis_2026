library(tidyverse)
set.seed(42)
set_theme(theme_minimal())

mtx <- as.data.frame(matrix(data = rnorm(500, 0, 1), nrow = 50, ncol = 10))
mtx$V11 <- -mtx$V1*.1 + mtx$V4*.5

corrplot::corrplot(
  cor(mtx),
  type = "upper",
  p.mat = corrplot::cor.mtest(mtx)$p,
  sig.level = 0.05,
  title = 'No adjustment'
)

pv <- corrplot::cor.mtest(mtx)$p
padj <- p.adjust(pv[upper.tri(pv)], method='BH')
pv[upper.tri(pv)] <- padj
corrplot::corrplot(
  cor(mtx),
  type = "upper",
  p.mat = pv,
  sig.level = 0.05,
  title = 'BH adjustment'
)


################################################################################

data <- tibble(group = rep(c('Treatment', 'Placebo'), each = 10),
               measurement = c(rnorm(10, mean=11.2, .8), rnorm(10, mean=10, 1)))

ggplot(data, aes(group, measurement)) +
  geom_boxplot()

nsim <- 1000
res <- numeric(nsim) ## set aside space for results
for (i in 1:nsim) {
  ## standard approach: scramble response value
  perm <- sample(nrow(data))
  bdat <- transform(data,measurement=measurement[perm])
  ## compute & store difference in means; store the value
  res[i] <- mean(bdat$measurement[bdat$group=="Treatment"])-
    mean(bdat$measurement[bdat$group=="Placebo"])
}
obs <- mean(data$measurement[data$group=="Treatment"])-
  mean(data$measurement[data$group=="Placebo"])
## append the observed value to the list of results
res <- c(res,obs)

ggplot(NULL, aes(res)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "lightblue") +
  geom_density() +
  geom_vline(aes(xintercept = obs), color='red', linewidth=1)

mean(abs(res)>=abs(obs))

rstatix::t_test(data, measurement ~ group) -> res
res$p
##############################################res################################################################################

nsim <- 1000
res <- numeric(nsim) ## set aside space for results
for (i in 1:nsim) {
  ## standard approach: scramble response value
  perm <- sample(nrow(data))
  bdat <- transform(data,measurement=measurement[perm])
  ## compute & store difference in means; store the value
  res[i] <- rstatix::t_test(bdat, measurement ~ group)$p
}
obs <- rstatix::t_test(data, measurement ~ group)$p
## append the observed value to the list of results
res <- c(res,obs)

ggplot(NULL, aes(res)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "lightblue") +
  geom_density() +
  geom_vline(aes(xintercept = obs), color='red', linewidth=1)

mean(abs(res)<=abs(obs))
