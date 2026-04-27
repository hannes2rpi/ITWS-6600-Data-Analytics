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
setwd("C:/Users/shann/Documents/School/spring26/itws6600/project")

# read data
vegan_cities <- read_csv("2019 Best U.S. Cities for Vegans and Vegetarians.csv")
vegan_rest <- read_csv("Datafiniti_Vegetarian_and_Vegan_Restaurants.csv")

vegan_cities_filt <- vegan_cities[!is.na(vegan_cities$City),]
vegan_rest_filt <- vegan_rest[!duplicated(vegan_rest[,c(1)]),]
vegan_rest_filt2 <- vegan_rest_filt[!is.na(vegan_rest_filt$priceRangeMax),]


# HISTOGRAMS

# histogram (frequency distribution)
hist(vegan_rest_filt$latitude)

# histogram (frequency distribution)
hist(vegan_rest_filt$longitude)

# histogram (frequency distribution)
hist(vegan_rest_filt2$priceRangeMax)

# histogram (frequency distribution)
hist(vegan_cities_filt$Latitude)

# histogram (frequency distribution)
hist(vegan_cities_filt$Longitude)

# histogram (frequency distribution)
hist(vegan_cities_filt$`Total Score`)



# CUISINE TYPE BY LAT AND LONG 
### K-Means ###

vegan_rest_filt$cluster_categories <- c(1, 2, 3, 3, 15, 5, 6, 6, 6, 3, 2, 2, 7, 2, 6, 2, 4, 8, 2, 9, 1, 10, 7, 4, 11, 2, 10, 13, 2, 2, 7, 2, 12, 1, 12, 9, 2, 7, 6, 13, 10, 10, 3, 14, 12, 3, 3, 5, 5, 14, 14, 5, 5, 2, 12, 12, 15, 15, 16, 2, 13, 2, 17, 18, 18, 2, 15, 2, 3, 17, 5, 10, 10, 10, 12, 10, 10, 19, 3, 20, 14, 17, 14, 3, 3, 3, 14, 21, 2, 14, 19, 5, 2, 2, 14, 7, 2, 17, 1, 22, 2, 19, 22, 3, 17, 19, 21, 21, 2, 2, 22, 14, 1, 17, 21, 21, 2, 22, 15, 2, 17, 5, 21, 6, 2, 15, 14, 1, 12, 22, 2, 1, 22, 17, 2, 19, 3, 21, 21, 8, 17, 5, 2, 2, 2, 19, 22, 2, 12, 22, 21, 21, 3, 15, 15, 21, 22, 21, 6, 21, 22, 14, 16, 22, 19, 7, 22, 19, 22, 3, 21, 2, 19, 6, 17, 1, 2, 22, 7, 21, 3, 2, 17, 21, 22, 22, 23, 21, 21, 12, 2, 22, 18, 21, 10, 15, 17, 3, 10, 10, 2, 22, 10, 3, 5, 7, 17, 10, 22, 22, 17)
## set seed value for random number generator
set.seed(6)

## number of clusters to look for
k = 23

## learn clusters
vegan_rest_filt.km1 <- kmeans(vegan_rest_filt[c(24, 25)], centers = k)

## Total (for all clusters) within clusters sum of squares
wcss1 <- vegan_rest_filt.km1$tot.withinss
wcss1

### plot clustering output 
## put cluster assignments into a variable
assigned.clusters1 <- as.factor(vegan_rest_filt.km1$cluster)

## scatterplot of the data colored by cluster membership
ggplot(vegan_rest_filt, aes(x = latitude, y = longitude, colour = assigned.clusters1)) +
  geom_point()

## compute pairwise distances between all points: 150 x 150 matrix
vegan_rest_filt.dist1 <- as.matrix(dist(vegan_rest_filt[c(24, 25)]))

## Silhouette Plot
sil1 <- silhouette(vegan_rest_filt.km1$cluster, dist(vegan_rest_filt[c(24, 25)]))
fviz_silhouette(sil1)

true_factor1 <- factor(vegan_rest_filt$cluster_categories)

cm1 <- confusionMatrix(assigned.clusters1, true_factor1)
cm1
cm1$byClass[, "Precision"]
cm1$byClass[, "Recall"]
cm1$byClass[, "F1"]



#MAX SPENDING BY LAT AND LONG

### K-Means ###

vegan_rest_filt2$cluster_categories <- c(1, 2, 1, 2, 1, 2, 1, 2, 2, 2, 3, 4, 4, 3, 1, 1, 1, 3, 1, 1, 1, 3, 4, 1, 4, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 3, 1, 3, 4, 3, 1, 1, 1, 2, 1, 2, 1, 2, 2, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 2, 1, 1, 1, 2, 1, 2, 1, 3, 2, 1, 1, 1, 1, 2, 1, 3, 1, 2, 3, 3, 3, 1, 3)
## set seed value for random number generator
set.seed(6)

## number of clusters to look for
k = 4

## learn clusters
vegan_rest_filt2.km2 <- kmeans(vegan_rest_filt2[c(24, 25)], centers = k)

## Total (for all clusters) within clusters sum of squares
wcss2 <- vegan_rest_filt2.km2$tot.withinss
wcss2

### plot clustering output 
## put cluster assignments into a variable
assigned.clusters2 <- as.factor(vegan_rest_filt2.km2$cluster)

## scatterplot of the data colored by cluster membership
ggplot(vegan_rest_filt2, aes(x = latitude, y = longitude, colour = assigned.clusters2)) +
  geom_point()

## compute pairwise distances between all points: 150 x 150 matrix
vegan_rest_filt2.dist2 <- as.matrix(dist(vegan_rest_filt2[c(24, 25)]))

## Silhouette Plot
sil2 <- silhouette(vegan_rest_filt2.km2$cluster, dist(vegan_rest_filt2[c(24, 25)]))
fviz_silhouette(sil2)

true_factor2 <- factor(vegan_rest_filt2$cluster_categories)

cm2 <- confusionMatrix(assigned.clusters2, true_factor2)
cm2
cm2$byClass[, "Precision"]
cm2$byClass[, "Recall"]
cm2$byClass[, "F1"]


#LINEAR REGRESSION LAT VS SCORE

ggplot(vegan_cities_filt, aes(x = Latitude, y = `Total Score`)) +
  geom_point()

lmod1 <- lm(Latitude ~`Total Score`, data = vegan_cities_filt)

## print model output
summary(lmod1)

## better scatter plot of 2 variables with best fit line

ggplot(vegan_cities_filt, aes(x = Latitude, y = `Total Score`)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")


ggplot(lmod1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)



#LINEAR REGRESSION LONG VS SCORE

ggplot(vegan_cities_filt, aes(x = Longitude, y = `Total Score`)) +
  geom_point()

lmod2 <- lm(Longitude ~`Total Score`, data = vegan_cities_filt)

## print model output
summary(lmod2)

## better scatter plot of 2 variables with best fit line

ggplot(vegan_cities_filt, aes(x = Longitude, y = `Total Score`)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")


ggplot(lmod2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)

