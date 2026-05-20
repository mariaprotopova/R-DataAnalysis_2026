library(tidyverse)
library(plotly)
library(fpc)

data <- read_csv2("marketing_campaign.csv")
View(data)

data_scaled<- as.data.frame(scale(data[, 10:15]))

ggplot(data_scaled, aes(MntWines, MntGoldProds)) +
  geom_point() +
  geom_smooth(method='lm')

################## Иерархический кластерный анализ #############################
colnames(data)
d <- dist(data_scaled[, 'MntWines':'MntGoldProds'], method = 'manhattan')

hc.single <- hclust(d, method = 'single')
plot(hc.single)

hc.complete <- hclust(d, method = 'complete')
plot(hc.complete)

hc.average <- hclust(d, method = 'average')
plot(hc.average)

hc.ward <- hclust(d, method = 'ward.D2')
plot(hc.ward)

### Сколько кластеров оставить?
rect.hclust(hc.ward, k = 2, border = "blue")
rect.hclust(hc.ward, k = 3, border = "red")
rect.hclust(hc.ward, k = 4, border = "green")
rect.hclust(hc.ward, k = 5, border = "yellow")

# Попробовать другие методы?

data_scaled$cluster <- as.factor(cutree(hc.ward, 4))

fig <- plot_ly(data, x = ~Calories, y = ~Sodium, z = ~Cost, color=~cluster)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Calories'),
                                   yaxis = list(title = 'Sodium'),
                                   zaxis = list(title = 'Alcohol')))

fig

# Интерпретация кластеров
data$cluster <- data_scaled$cluster
data %>% group_by(cluster) %>% 
  summarise(N = n(),
            MntFruits = mean(MntFruits),
            MntMeatProducts = mean(MntMeatProducts),
            MntFishProducts = mean(MntFishProducts),
            MntSweetProducts = mean(MntSweetProducts),
            MntGoldProds = mean(MntGoldProds),)

################## K-Means #############################
data_scaled$cluster <- NULL

km2 <- kmeans(data_scaled, centers = 2)
km2$cluster

wss <- function(x, k) {
  wss <- numeric(k)
  names(wss) <- 1:k
  for (i in 2:k) {
    wss[i] <- kmeans(x, i)$tot.withinss
  }
  return(wss[-1])
}

km_wss <- wss(data_scaled, 10)

ggplot(NULL, aes(2:10, km_wss)) +
  geom_line() +
  geom_point() +
  labs(x = 'Number of Clusters',
       y = 'Within group sum of squares')

data$cluster_km4 <- kmeans(data_scaled, centers = 4)$cluster

# Интерпретация кластеров
data %>% group_by(cluster_km4) %>% 
  summarise(N = n(),
            Alcohol = mean(Alcohol),
            Sodium = mean(Sodium),
            Calories = mean(Calories),
            Cost = mean(Cost))
