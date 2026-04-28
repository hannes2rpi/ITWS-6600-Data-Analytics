library(readr)
library(EnvStats)
library(nortest)
library("ggplot2")
library(GGally)
library(psych)
library(cluster)
library(dendextend)
library(colorspace)
library(factoextra)
library(caret)

# set working directory (relative path)
setwd("C:/Users/shann/Documents/School/spring26/itws6600/github/ITWS-6600-Data-Analytics/assignment6")

# read data
dataset <- read_csv("DATA.csv")


# HISTOGRAMS

# histogram (frequency distribution)
hist(dataset$`29`)

# histogram (frequency distribution)
hist(dataset$`13`)

# histogram (frequency distribution)
hist(dataset$`25`)

# histogram (frequency distribution)
hist(dataset$`26`)

# histogram (frequency distribution)
hist(dataset$`11`)

# histogram (frequency distribution)
hist(dataset$`12`)

# histogram (frequency distribution)
hist(dataset$`17`)

# histogram (frequency distribution)
hist(dataset$`22`)


#LINEAR REGRESSION LAT VS SCORE

ggplot(dataset, aes(x = `13`, y = `29`)) +
  geom_point()

lmod1 <- lm(`13` ~ `29`, data = dataset)

## print model output
summary(lmod1)

## better scatter plot of 2 variables with best fit line

ggplot(dataset, aes(x = `13`, y = `29`)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")


ggplot(lmod1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)


# NOTE TAKING AND LISTENING PREDICT GPA
### K-Means ###

## set seed value for random number generator
set.seed(6)

## number of clusters to look for
k = 5

## learn clusters
dataset.km1 <- kmeans(dataset[c(26, 27)], centers = k)

## Total (for all clusters) within clusters sum of squares
wcss1 <- dataset.km1$tot.withinss
wcss1

### plot clustering output 
## put cluster assignments into a variable
assigned.clusters1 <- as.factor(dataset.km1$cluster)

## scatterplot of the data colored by cluster membership
ggplot(dataset, aes(x = `25`, y = `26`, colour = assigned.clusters1)) +
  geom_point()

## compute pairwise distances between all points: 150 x 150 matrix
dataset.dist1 <- as.matrix(dist(dataset[c(26, 27)]))

## Silhouette Plot
sil1 <- silhouette(dataset.km1$cluster, dist(dataset[c(26, 27)]))
fviz_silhouette(sil1)

true_factor1 <- factor(dataset$`29`)

cm1 <- confusionMatrix(assigned.clusters1, true_factor1)
cm1
cm1$byClass[, "Precision"]
cm1$byClass[, "Recall"]
cm1$byClass[, "F1"]


# MOTHER'S EDUCATION AND FATHER'S EDUCATION PREDICT GPA
### K-Means ###

## set seed value for random number generator
set.seed(6)

## number of clusters to look for
k = 5

## learn clusters
dataset.km2 <- kmeans(dataset[c(12, 13)], centers = k)

## Total (for all clusters) within clusters sum of squares
wcss2 <- dataset.km2$tot.withinss
wcss2

### plot clustering output 
## put cluster assignments into a variable
assigned.clusters2 <- as.factor(dataset.km2$cluster)

## scatterplot of the data colored by cluster membership
ggplot(dataset, aes(x = `11`, y = `12`, colour = assigned.clusters2)) +
  geom_point()

## compute pairwise distances between all points: 150 x 150 matrix
dataset.dist2 <- as.matrix(dist(dataset[c(12, 13)]))

## Silhouette Plot
sil2 <- silhouette(dataset.km2$cluster, dist(dataset[c(12, 13)]))
fviz_silhouette(sil2)

true_factor2 <- factor(dataset$`29`)

cm2 <- confusionMatrix(assigned.clusters2, true_factor2)
cm2
cm2$byClass[, "Precision"]
cm2$byClass[, "Recall"]
cm2$byClass[, "F1"]


# WEEKLY STUDY HOURS AND ATTENDANCE TO CLASSES PREDICT GPA
### K-Means ###

## set seed value for random number generator
set.seed(6)

## number of clusters to look for
k = 5

## learn clusters
dataset.km3 <- kmeans(dataset[c(18, 23)], centers = k)

## Total (for all clusters) within clusters sum of squares
wcss3 <- dataset.km3$tot.withinss
wcss3

### plot clustering output 
## put cluster assignments into a variable
assigned.clusters3 <- as.factor(dataset.km3$cluster)

## scatterplot of the data colored by cluster membership
ggplot(dataset, aes(x = `17`, y = `22`, colour = assigned.clusters3)) +
  geom_point()

## compute pairwise distances between all points: 150 x 150 matrix
dataset.dist3 <- as.matrix(dist(dataset[c(18, 23)]))

## Silhouette Plot
sil3 <- silhouette(dataset.km3$cluster, dist(dataset[c(18, 23)]))
fviz_silhouette(sil3)

true_factor3 <- factor(dataset$`29`)

cm3 <- confusionMatrix(assigned.clusters3, true_factor3)
cm3
cm3$byClass[, "Precision"]
cm3$byClass[, "Recall"]
cm3$byClass[, "F1"]
