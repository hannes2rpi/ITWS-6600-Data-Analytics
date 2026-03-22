##########################################
### Principal Component Analysis (PCA) ###
##########################################

## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)
library(cluster)
library(dendextend)
library(colorspace)
library(factoextra)

## set working directory so that files can be referenced without the full path
setwd("C:/Users/shann/Documents/School/spring26/itws6600/github/ITWS-6600-Data-Analytics/lab4")

## read dataset
wine <- read_csv("wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
head(wine)

## change the data type of the "Type" column from character to factor
####
# Factors look like regular strings (characters) but with factors R knows 
# that the column is a categorical variable with finite possible values
# e.g. "Type" in the Wine dataset can only be 1, 2, or 3
####

wine$Type <- as.factor(wine$Type)


## visualize variables
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

#ggpairs(wine, ggplot2::aes(colour = Type))

###

X <- wine[,-1]
Y <- wine$Type

###
####### PCA #######

Xmat <- as.matrix(X)

Xc <- scale(Xmat, center = T, scale = F)

principal_components <- princomp(Xc)

principal_components$loadings


## using autoplot() function to plot the components
autoplot(principal_components, data = wine, colour = 'Type',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, scale = 0)

### K-Means ###

## set seed value for random number generator
set.seed(6)

## number of clusters to look for
k = 3

## learn clusters
wine.km <- kmeans(wine[c(2, 4, 6)], centers = k)


## Total (for all clusters) within clusters sum of squares
wcss <- wine.km$tot.withinss
wcss

### plot clustering output 
## put cluster assignments into a variable
assigned.clusters <- as.factor(wine.km$cluster)

## scatterplot of the data colored by cluster membership
ggplot(wine, aes(x = Alcohol, y = Magnesium, colour = assigned.clusters)) +
  geom_point()

ggplot(wine, aes(x = Ash, y = Magnesium, colour = assigned.clusters)) +
  geom_point()

## compute pairwise distances between all points: 150 x 150 matrix
wine.dist <- as.matrix(dist(wine[c(2, 4, 6)]))

## Silhouette Plot
sil <- silhouette(wine.km$cluster, dist(wine[c(2, 4, 6)]))
fviz_silhouette(sil)


### K-Means ###

## set seed value for random number generator
set.seed(6)

## number of clusters to look for
k = 3

## learn clusters
proj.km <- kmeans(Xc[, c(1, 3, 5)], centers = k)


## Total (for all clusters) within clusters sum of squares
wcss2 <- proj.km$tot.withinss
wcss2

### plot clustering output 
## put cluster assignments into a variable
assigned.clusters2 <- as.factor(proj.km$cluster)

## scatterplot of the data colored by cluster membership
ggplot(Xc, aes(x = Alcohol, y = Magnesium, colour = assigned.clusters2)) +
  geom_point()

ggplot(Xc, aes(x = Ash, y = Magnesium, colour = assigned.clusters2)) +
  geom_point()

## compute pairwise distances between all points: 150 x 150 matrix
proj.dist <- as.matrix(dist(Xc[, c(1, 3, 5)]))

## Silhouette Plot
sil2 <- silhouette(proj.km$cluster, dist(Xc[, c(1, 3, 5)]))
fviz_silhouette(sil2)

