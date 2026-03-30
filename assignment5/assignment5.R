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

# set working directory (relative path)
setwd("C:/Users/shann/Documents/School/spring26/itws6600/github/ITWS-6600-Data-Analytics/assignment5")

# read data
epi.data <- read_csv("NYC_Citywide_Annualized_Calendar_Sales_Update_20241107.csv")

epi.b5 <- epi.data[epi.data$BOROUGH==5,]


## 1. REGRESSION ##

## A ##

epi.b5.filtered <- epi.b5[epi.b5$`SALE PRICE` < 1500000 & epi.b5$`SALE PRICE` >= 100000,]

epi.b5.filtered <- epi.b5.filtered[epi.b5.filtered$`YEAR BUILT` >= 1880 & !is.na(epi.b5.filtered$`YEAR BUILT`),]

epi.b5.filtered <- epi.b5.filtered[!is.na(epi.b5.filtered$Latitude),]

epi.b5.filtered <- epi.b5.filtered[!is.na(epi.b5.filtered$Longitude),]

# histogram (frequency distribution)
hist(epi.b5.filtered$`SALE PRICE`)

# histogram (frequency distribution)
hist(epi.b5.filtered$`YEAR BUILT`)

# histogram (frequency distribution)
hist(epi.b5.filtered$Latitude)

# histogram (frequency distribution)
hist(epi.b5.filtered$Longitude)


## B ##

lmod1 <- lm(`YEAR BUILT`~`SALE PRICE`, data = epi.b5.filtered)

## print model output
summary(lmod1)

## better scatter plot of 2 variables with best fit line

ggplot(epi.b5.filtered, aes(x = `YEAR BUILT`, y = `SALE PRICE`)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")


ggplot(lmod1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)


lmod2 <- lm(Latitude~`SALE PRICE`, data = epi.b5.filtered)

## print model output
summary(lmod2)

## better scatter plot of 2 variables with best fit line

ggplot(epi.b5.filtered, aes(x = Latitude, y = `SALE PRICE`)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")


ggplot(lmod2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)


lmod3 <- lm(Longitude~`SALE PRICE`, data = epi.b5.filtered)

## print model output
summary(lmod3)

## better scatter plot of 2 variables with best fit line

ggplot(epi.b5.filtered, aes(x = Longitude, y = `SALE PRICE`)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")


ggplot(lmod3, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)


## 2. CLASSIFICATION ##

epi.b5.classification <- epi.b5.filtered[epi.b5.filtered$NEIGHBORHOOD == "MANOR HEIGHTS" | epi.b5.filtered$NEIGHBORHOOD == "BULLS HEAD" | epi.b5.filtered$NEIGHBORHOOD == "ROSEBANK",]

### K-Means ###

## set seed value for random number generator
set.seed(6)

## number of clusters to look for
k = 3

## learn clusters
epi.b5.classification.km1 <- kmeans(epi.b5.classification[c(20, 22)], centers = k)

## Total (for all clusters) within clusters sum of squares
wcss1 <- epi.b5.classification.km1$tot.withinss
wcss1

### plot clustering output 
## put cluster assignments into a variable
assigned.clusters1 <- as.factor(epi.b5.classification.km1$cluster)

## scatterplot of the data colored by cluster membership
ggplot(epi.b5.classification, aes(x = `SALE PRICE`, y = Latitude, colour = assigned.clusters1)) +
  geom_point()

## compute pairwise distances between all points: 150 x 150 matrix
epi.b5.classification.dist1 <- as.matrix(dist(epi.b5.classification[c(20, 22)]))

## Silhouette Plot
sil1 <- silhouette(epi.b5.classification.km1$cluster, dist(epi.b5.classification[c(20, 22)]))
fviz_silhouette(sil1)


## learn clusters
epi.b5.classification.km2 <- kmeans(epi.b5.classification[c(20, 23)], centers = k)

## Total (for all clusters) within clusters sum of squares
wcss2 <- epi.b5.classification.km2$tot.withinss
wcss2

### plot clustering output 
## put cluster assignments into a variable
assigned.clusters2 <- as.factor(epi.b5.classification.km2$cluster)

## scatterplot of the data colored by cluster membership
ggplot(epi.b5.classification, aes(x = `SALE PRICE`, y = Longitude, colour = assigned.clusters2)) +
  geom_point()

## compute pairwise distances between all points: 150 x 150 matrix
epi.b5.classification.dist2 <- as.matrix(dist(epi.b5.classification[c(20, 23)]))

## Silhouette Plot
sil2 <- silhouette(epi.b5.classification.km2$cluster, dist(epi.b5.classification[c(20, 23)]))
fviz_silhouette(sil2)


## learn clusters
epi.b5.classification.km3 <- kmeans(epi.b5.classification[c(22, 23)], centers = k)

## Total (for all clusters) within clusters sum of squares
wcss3 <- epi.b5.classification.km3$tot.withinss
wcss3

### plot clustering output 
## put cluster assignments into a variable
assigned.clusters3 <- as.factor(epi.b5.classification.km3$cluster)

## scatterplot of the data colored by cluster membership
ggplot(epi.b5.classification, aes(x = Latitude, y = Longitude, colour = assigned.clusters3)) +
  geom_point()

## compute pairwise distances between all points: 150 x 150 matrix
epi.b5.classification.dist3 <- as.matrix(dist(epi.b5.classification[c(22, 23)]))

## Silhouette Plot
sil3 <- silhouette(epi.b5.classification.km3$cluster, dist(epi.b5.classification[c(22, 23)]))
fviz_silhouette(sil3)


