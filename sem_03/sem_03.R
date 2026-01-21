set.seed(42)
###============= Normal Distribution =============
library(tidyverse)
n_iterations <- 1000
n_cubes <- 5

cubic <- c()
for (i in seq(1, n_iterations)){
  cubic <- c(cubic, sum(sample(seq(1,6), n_cubes, replace = T)))
}

hist(cubic)

m <- mean(cubic); m
std <- sd(cubic); std
ref <- rnorm(n=n_iterations, mean=m, sd=std)
ggplot(data=NULL) +
  geom_histogram(aes(cubic))+
  geom_density(aes(ref, color='white')) +
  geom_vline(xintercept = m, color='red') +
  geom_vline(xintercept = mean(ref), color='purple')

N=10e5
M=5
STD=.5
ggplot(data=NULL) +
  geom_density(aes(rnorm(n=10000, mean=0, sd=1)), color='black', linetype = 'dashed')+
  geom_density(aes(rnorm(n=1000, mean=5, sd=7)), color='blue', linewidth = 1)+
  geom_density(aes(rnorm(n=100, mean=-3, sd=3.5)), color='purple', linewidth = 1)+
  geom_density(aes(rnorm(n=10, mean=4, sd=1.9)), color='pink', linewidth = 1)+
  geom_density(aes(rnorm(n=N, mean=M, sd=STD)), color='red', linewidth = 1)+
  theme_bw()

ggplot(data=NULL) +
  geom_density(aes(rnorm(n=10000, mean=0, sd=1)), color='black')+
  geom_density(aes(rnorm(n=10000, mean=0, sd=2)), color='red')+
  geom_density(aes(rnorm(n=10000, mean=0, sd=.7)), color='green')+
  theme_bw()


ggplot(data=NULL) +
  geom_density(aes(rnorm(n=10000, mean=0, sd=1)), color='black')+
  geom_density(aes(rnorm(n=10000, mean=-3, sd=1)), color='red')+
  geom_density(aes(rnorm(n=10000, mean=5, sd=1)), color='green')+
  theme_bw()

###============= The first momentum =============
set.seed(42)
a <- rnorm(n=100, mean=1, sd=.1)
b <- rgamma(n=100, shape=.4)
ggplot(data=NULL) +
  geom_density(aes(a), color='black') +
  geom_density(aes(b), color='blue') +
  labs(x='values')+
  theme_bw()

mean(a); mean(b)
median(a); median(b)

ggplot(data=NULL) +
  geom_density(aes(a), color='black') +
  geom_density(aes(b), color='blue') +
  labs(x='values')+
  geom_vline(xintercept = mean(a), color='green', linetype='dashed') +
  geom_vline(xintercept = mean(b), color='green', linetype='dashed') +
  geom_vline(xintercept = median(a), color='red', linetype=4) +
  geom_vline(xintercept = median(b), color='red', linetype=4) +
  theme_bw()

y <- c(1,2,3,2,3,4,3,5,2)
mode <- function(y) {
  ind <- which(table(y)==max(table(y)))
  return(table(y)[ind])
}

mode(y)

###============= Deviations =============
x1 <- c(1,  1,  2,  5,  5,  6,  8,  8, 10, 10)
x2 <- c(1,  3,  4,  4,  5,  6,  7,  7,  9, 10)

mean(x1); mean(x2)
median(x1); median(x2)
range(x1); range(x2)

hist(x1)
hist(x2)

sd(x1)
sd(x2)

quantile(x1)
quantile(x2)

IQR(x1)
IQR(x2)
x <- x1
q1 <- quantile(x, .25)
q3 <- quantile(x, .75)
iqr <- IQR(x)
lower_bound <- q1 - iqr * 1.5
higher_bound <- q3 + iqr * 1.5
ans <- which((x < lower_bound) | (x > higher_bound))
lower_bound; higher_bound

is_outlier <- function(x){
  q1 <- quantile(x, .25)
  q3 <- quantile(x, .75)
  iqr <- IQR(x)
  lower_bound <- q1 - iqr * 1.5
  higher_bound <- q3 + iqr * 1.5
  ans <- which((x < lower_bound) | (x > higher_bound))
  return(x[ans])
}

outs <- c(1,2,3,1,4,2,3,1,4,2,3,2,2,3,1,0,755, 756)
is_outlier(outs)
