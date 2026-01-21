library(tidyverse)

# # https://www.kaggle.com/datasets/neurocipher/heartdisease?select=Heart_Disease_Prediction.csv
# data <- read.csv("~/Downloads/Heart_Disease_Prediction.csv")

diamonds

View(diamonds)
summary(diamonds)

ggplot()

ggplot(data = diamonds, aes(x=carat, y=price)) +
  geom_point()

?geom_smooth
ggplot(data = diamonds, aes(x=carat, y=price)) +
  geom_point() +
  geom_smooth(method='lm') +
  xlim(3, 5) +
  ylim(10000, 20000)


ggplot(data = diamonds[1:10000,], aes(x=carat, y=price, color=cut)) +
  geom_point() +
  geom_smooth(method='loess')

ggplot(data = diamonds[1:10000,], aes(x=carat, y=price, color=cut)) +
  geom_point() +
  geom_smooth(method='loess') +
  theme_minimal()


ggplot(data=diamonds, aes(x, y)) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_bw() +
  ylim(0, 15)


ggplot(data=diamonds, aes(x, y)) +
  geom_smooth(method='lm') +
  geom_point() +
  theme_bw() +
  ylim(0, 15)

# распределение веса бриллианта в зависимости от огранки.
# Добавьте на график средний вес бриллианта каждого типа огранки

ggplot(diamonds, aes(cut, carat, color=cut)) +
  geom_boxplot()


ggplot(diamonds, aes(cut, carat, fill=cut)) +
  geom_boxplot()

ggplot(diamonds, aes(cut, carat, fill=color)) +
  geom_boxplot()

ggplot(diamonds, aes(cut, carat, fill=cut)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = 'point', shape = 8, size = 2)


# связь между глубиной бриллианта и его цветом.
# Добавьте на график среднюю глубину бриллианта каждого вида
ggplot(diamonds, aes(color, depth, fill=color)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = 'point', shape = 1, size = 2)

ggplot(diamonds, aes(depth, fill=color)) +
  geom_histogram() +
  facet_grid(color~.)
