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
library(caret)

## set working directory so that files can be referenced without the full path
setwd("C:/Users/shann/Documents/School/spring26/itws6600/github/ITWS-6600-Data-Analytics/lab5")

## read dataset
wine <- read_csv("wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
head(wine)

# PCA with wine dataset
X <- wine[,-1]
Y <- wine$Type

####### PCA #######

Xmat <- as.matrix(X)

Xc <- scale(Xmat, center = T, scale = F)

principal_components <- princomp(Xc)

summary(principal_components)

principal_components$loadings


# using the plot() function, we can plot the principal components.
plot(principal_components)

# plotting the principal_components using the a line in plot() functions 
plot(principal_components, type = "l")

# using rhw biplot() function we can plot the components
biplot(principal_components)

## using autoplot() function to plot the components
autoplot(principal_components, data = wine, colour = 'Type',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, scale = 0)

## scatter plot of dataset in PCs
ggplot(principal_components$scores, aes(x = Comp.1, y = Comp.2)) + geom_point(color="blue")

# loadings
principal_components$loadings


####### PCA Manually #########

C <- cov(Xc)

C.eigens <-  eigen(C)

C.eigens$vectors
C.eigens$values

V <- C.eigens$vectors

Z <- Xc %*% V

## scatter plot of dataset in PCs
ggplot(Z, aes(x = Z[,1], y = -Z[,2])) + geom_point(color="blue")


### K-Means ###

## set seed value for random number generator
set.seed(6)

## number of clusters to look for
k = 3

## learn clusters
wine.km <- kmeans(wine[-c(1)], centers = k)


## Total (for all clusters) within clusters sum of squares
wcss <- wine.km$tot.withinss
wcss

### plot clustering output 
## put cluster assignments into a variable
assigned.clusters <- as.factor(wine.km$cluster)

## plot of the data colored by cluster membership
fviz_cluster(wine.km, data = wine[-c(1)],
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

## compute pairwise distances between all points: 150 x 150 matrix
wine.dist <- as.matrix(dist(wine[-c(1)]))

## Silhouette Plot
sil <- silhouette(wine.km$cluster, dist(wine[-c(1)]))
fviz_silhouette(sil)

wine_factor <- factor(wine$Type)

cm <- confusionMatrix(assigned.clusters, wine_factor)
cm
cm$byClass[, "Precision"]
cm$byClass[, "Recall"]
cm$byClass[, "F1"]

