library(tidyverse)

# https://www.kaggle.com/datasets/neurocipher/heartdisease?select=Heart_Disease_Prediction.csv
data <- read.csv("~/Downloads/Heart_Disease_Prediction.csv")
View(data)
summary(data)

ggplot(data, aes(Age, Max.HR)) +
  geom_point() +
  geom_smooth(method='lm')
