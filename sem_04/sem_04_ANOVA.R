library(tidyverse)
library(ez)

diet <- read.csv('diet.csv')
View(diet)


diet <- diet %>%
  mutate(weight.loss = final.weight - initial.weight,
         Diet = factor(diet.type, levels = c('A', 'B', 'C')),
         id = factor(id))
colSums(is.na(diet))
View(diet)

aov_model <- aov(weight.loss ~ Diet, diet)
summary(aov_model)

# POST-HOC TESTS
pairwise.t.test(diet$weight.loss, diet$Diet)
t.test(diet[diet$Diet =='A',]$weight.loss, diet[diet$Diet =='B',]$weight.loss)

pairs(emmeans::emmeans(aov_model, ~ Diet))

# Visualization
sem <- function(x) sd(x)/sqrt(length(x)) #пишем функцию для стандартной ошибки
pd <- position_dodge(0.1) #немного раздвигаем положение точек на будущем графике

diet %>%
  group_by(Diet) %>%
  summarise(meanloss = mean(weight.loss),
            se = sem(weight.loss)) %>%
  ggplot(aes(x = Diet, 
             y = meanloss, color=Diet)) +
  geom_errorbar(aes(ymin = meanloss - se, 
                    ymax = meanloss + se), position = pd,
                width=.1) +
  geom_point(position=pd, size=5) +
  theme_minimal() +
  theme(legend.position='None')


# ASSUMPTIONS of ANOVA

# 1. Normally distributed residuals
diet$residuals <- residuals(aov_model)


ggplot(data=diet, aes(residuals)) +
  geom_density()

ks.test(diet$residuals, 'pnorm')
# 2. Homogeneity of dispersions

# Visually
ggplot(diet, aes(x = Diet, y = residuals)) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  theme_bw()

# Formally (Levene's test)
library(ez)
ez_model <- ezANOVA(data = diet,
                    dv= weight.loss,
                    wid = id, 
                    between = Diet,
                    detailed = T, 
                    return_aov = T);

# MANOVA

diet <- diet %>%
  mutate(gender = factor(gender)) #превращаем в бинарную переменную в фактор

sem <- function(x) sd(x)/sqrt(length(x)) #пишем функцию для стандартной ошибки
pd <- position_dodge(0.1) #немного раздвигаем положение точек на будущем графике

diet %>%
  group_by(Diet, gender) %>%
  summarise(meanloss = mean(weight.loss),
            se = sem(weight.loss)) %>%
  ggplot(aes(x = Diet, 
             y = meanloss, 
             colour = gender)) +
  geom_line(aes(group = gender), position = pd) +
  geom_errorbar(aes(ymin = meanloss - se, 
                      ymax = meanloss + se), position = pd,
                width=.1) +
  geom_point(position=pd, size=5) +
  theme_minimal()
# difference for women only (based on the graph)

ezANOVA(data = diet,
        dv= weight.loss,
        wid = id, 
        between = c(Diet, gender),
        detailed = T, 
        return_aov = T) -> manova

pairwise.t.test(diet$weight.loss, diet$Diet)
pairs(emmeans::emmeans(manova$aov, ~ Diet | gender))

emm <- pairs(emmeans::emmeans(manova$aov, ~ Diet | gender)); emm
