#=============== Basic operations ===============
2 + 3
sin(3.14)

a <- 5
b = 5

a == b

a != b + a

a %in% c(1,2,3,4,5)
a %in% seq(b+1, b+5)

TRUE & TRUE
TRUE & FALSE
TRUE | TRUE
TRUE | FALSE
FALSE | FALSE
FALSE & FALSE

#=============== Data types ===============
class(a)
d <-TRUE
class(d)
f <- 'i <3 programming'
class(f)

#=============== Data structures ===============
### 1) VECTORS
vector <- c(1,2,3,'qwerty'); vector
class(vector)

# indexation
vector[1] # 1 (not 0) is the 1st element!

# what will be Python output of the following line?
vector[-1]

vector[c(1,4)]

vector + 4 # ERROR

vector_2 <- c(1,2,3); vector_2

vector_2 + 4

### 2) LISTS
list_1 <- list(sample(1:13, 5)); list_1
class(list1)

# indexation
list1[1]
list1[[1]][3]

### 3) MATRIX
mtx <- matrix(sample(1:1000, 50, replace=TRUE), nrow=10, ncol=5); mtx

# indexation
mtx[1,2]
mtx[1:5]

mtx[6:9, 3:4]

mtx[c(1,5,10)]

mtx[,c(3,5)]

### 4) DATAFRAME
df <- data.frame(name=c("Маша", "Homer"),
                 age=c(25, 36),
                 city=c("Москва", "Springfield")
                 ); df


dim(df)
nrow(df); ncol(df)

# indexation
df[1,1]; df[1,2]; df[1,3]

df$age
View(df)
str(df)

df_2 <- data.frame(A=sample(1:10e2, size=99, replace=T)/33,
                   B=c(1,2,3),
                   C=sample(0:1, size=99, replace=T),
                   D=-5); df_2
head(df_2)
tail(df_2)

str(df_2)

# change data type:
df_2$C <- as.logical(df_2$C)

df_2$B <- as.factor(df_2$B)

str(df_2)

# add new variable
df_2$E <- round(df_2$A)
View(df_2)

# filter data based on value
df_2[df_2$A>10,]
df_2[(df_2$A>10) & (df_2$C) & ((df_2$E<20) | (df_2$E>=30)),]

### 5) TIBBLE

data <- tibble::tibble(age = sample(18:60, size = 30),
               group = ifelse(age > 40, 'old', 'young'),
               group_factor = factor(group) # factor levels are defined by the alphabetic order
               ); View(data)

str(data)


data <- tibble::tibble(age = sample(18:60, size = 30),
                       group = ifelse(age > 40, 'old', 'young'),
                       group_factor = factor(group, levels=c('young', 'old')) # custom define factor levels
                       ); str(data)

data <- tibble::tibble(age = sample(18:60, size = 30),
                       group = ifelse(age > 40, 'old', 'young'),
                       group_factor = factor(group, levels=c('young', 'middle', 'old')) # custom define factor levels and add a new level
                       ); str(data)



#=============== Tidyverse and tidy data ===============
library(tidyverse)

data <- us_rent_income

View(data)

# select()
select(data, NAME)
select(data, c(NAME, variable, estimate))
select(data, -GEOID)

# using pipe:
data %>% select(-GEOID) -> data_selected
View(data_selected)

# sapply()
?sapply
data_selected[1:10,] %>% select(estimate, moe) %>% sapply(mean)

data_selected[1:10,] %>% select(estimate, moe) %>% lapply(mean)

# check NAs
is.na(data_selected)
sum(is.na(data_selected))

sapply(lapply(data_selected, is.na), sum)
# equivalent to:
data_selected %>% lapply(is.na) %>% sapply(sum)

# how to deal with NAs?
data_selected[is.na(data_selected$estimate),]

data_selected[is.na(data_selected$estimate), c("estimate", "moe")] <- 0
data_selected %>% lapply(is.na) %>% sapply(sum)

data_selected %>% fill(c(estimate, moe), .direction = "up") %>% lapply(is.na) %>% sapply(sum)

# filter()
data_selected %>% filter(NAME=='Alaska')

data_selected %>% filter(NAME %in% c('Alaska', 'Nevada', 'Texas')) -> data_filtered

# mutate()
data_filtered %>% mutate(
  first_letter = startsWith(NAME, 'N')
  )

# *_join()
data_filtered %>% select(-moe) -> data_left
data_filtered[1:3,] %>% select(-estimate) -> data_right

left_join(data_left, data_right)

right_join(data_left, data_right)
full_join(data_left, data_right)

inner_join(data_left, data_right)

anti_join(data_left, data_right)

data_left$id <- 1:6
data_right$id <- 4:6
left_join(data_left, data_right, by='id')

# groupping and aggregation

data_filtered %>% group_by(NAME)

data_filtered %>% group_by(NAME) %>% summarize(mean_moe=mean(moe),
                                               median_moe = median(moe),
                                               sd_moe=sd(moe),
                                               min_moe=min(moe),
                                               max_moe=max(moe))

data_filtered %>% group_by(variable) %>% summarize(mean_moe=mean(moe),
                                               median_moe = median(moe),
                                               sd_moe=sd(moe),
                                               min_moe=min(moe),
                                               max_moe=max(moe)) -> data_groupped

summary(data_groupped)

summary(ungroup(data_groupped))

# Long vs. wide format
data_selected %>% select(-moe) %>% pivot_wider(names_from = variable, values_from = c(estimate)) -> data_wide

data_wide %>% pivot_longer(cols=2:3, names_to='variable', values_to='number')
# equivalent to
data_wide %>% pivot_longer(cols=c('income', 'rent'), names_to='variable', values_to='number')
# equivalent to
data_wide %>% pivot_longer(cols=colnames(data_wide)[2:3], names_to='variable', values_to='number')
