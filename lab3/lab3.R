####################################
##### Abalone Data Preparation #####
####################################

## imports
library(GGally)
library(ggplot2)
library(psych)
library(cluster)
library(dendextend)
library(colorspace)
library(factoextra)

# read dataset
abalone.data <- read.csv("C:/Users/shann/Documents/School/spring26/itws6600/github/ITWS-6600-Data-Analytics/lab3/abalone.data")

colnames(abalone.data) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_weight', 'viscera_wieght', 'shell_weight', 'rings' ) 

## add new column age.group with 3 values based on the number of rings 
abalone.data$age.group <- cut(abalone.data$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))


#PART 1

#FIRST K-MEANS
## ggplo2 scatterplots
ggplot(abalone.data, aes(x = whole_weight, y = shucked_weight, colour = age.group)) +
  geom_point()


### K-Means ###

## set seed value for random number generator
set.seed(6)

## number of clusters to look for
k = 3

## learn clusters
abalone.km2 <- kmeans(abalone.data[-c(1, 10)], centers = k)


## Total (for all clusters) within clusters sum of squares
wcss2 <- abalone.km2$tot.withinss
wcss2

### plot clustering output 
## put cluster assignments into a variable
assigned.clusters2 <- as.factor(abalone.km2$cluster)

## scatterplot of the data colored by cluster membership
ggplot(abalone.data, aes(x = whole_weight, y = shucked_weight, colour = assigned.clusters2)) +
  geom_point()

## compute pairwise distances between all points: 150 x 150 matrix
abalone.dist2 <- as.matrix(dist(abalone.data[-c(1, 10)]))

## Silhouette Plot
sil <- silhouette(abalone.km2$cluster, dist(abalone.data[-c(1, 10)]))
fviz_silhouette(sil)


#SECOND K-MEANS

## ggplo2 scatterplots
ggplot(abalone.data, aes(x = length, y = diameter, colour = sex)) +
  geom_point()


### K-Means ###

## set seed value for random number generator
set.seed(6)

## number of clusters to look for
k = 3

## learn clusters
abalone.km <- kmeans(abalone.data[-c(1, 10)], centers = k)


## Total (for all clusters) within clusters sum of squares
wcss <- abalone.km$tot.withinss
wcss

### plot clustering output 
## put cluster assignments into a variable
assigned.clusters <- as.factor(abalone.km$cluster)

## scatterplot of the data colored by cluster membership
ggplot(abalone.data, aes(x = length, y = diameter, colour = assigned.clusters)) +
  geom_point()

## compute pairwise distances between all points: 150 x 150 matrix
abalone.dist <- as.matrix(dist(abalone.data[-c(1, 10)]))

## Silhouette Plot
sil <- silhouette(abalone.km$cluster, dist(abalone.data[-c(1, 10)]))
fviz_silhouette(sil)

## run tests with multiple k values and plot WCSS
k.list <- c(1,2,3,4,5,6,7)

## create empty lists
wcss.list <- c()
si.list <- c()

## for each value of k, run the algorithm, compute wcss and average silhouette width and append them to list
for (k in k.list) {
  
  abalone.km <- kmeans(abalone.data[-c(1, 10)], centers = k)
  
  wcss <- abalone.km$tot.withinss
  
  wcss.list <- c(wcss.list,wcss)
  
  if (k > 1){
    
    si <- silhouette(abalone.km$cluster, dist(abalone.data[-c(1, 10)]))
    
    avg.si <- mean(si[, 3])  
    
    si.list <- c(si.list,avg.si)
  }
  
}

## plot wcss vs. k
plot(k.list,wcss.list,type = "b")

## plot avg silhouette width vs. k
plot(k.list[-1],si.list,type = "b")


wcss.list

diff(wcss.list)

diff(diff(wcss.list))


#PART 2

#FIRST PAM
### Partitioning Around Medoids ###

k = 3

abalone.pam <- pam(abalone.data[-c(1, 10)], k)

abalone.pam$objective

## get and plot clustering output 
assigned.clusters <- as.factor(abalone.pam$cluster)

ggplot(abalone.data, aes(x = whole_weight, y = shucked_weight, colour = assigned.clusters)) +
  geom_point()

## Silhouette Plot
sil <- silhouette(abalone.pam$cluster, dist(abalone.data[-c(1, 10)]))
fviz_silhouette(sil)

## run tests with multiple k values and plot sum of dissimilarities (sum of distances)
k.list <- c(1,2,3,4,5,6,7)

sumdiss.list <- c()
si.list <- c()

for (k in k.list) {
  
  abalone.pam <- pam(abalone.data[-c(1, 10)], k)
  
  sumdiss <- abalone.pam$objective[2]
  
  sumdiss.list <- c(sumdiss.list,sumdiss)
  
  if (k>1){
    si <- silhouette(abalone.pam$cluster, dist(abalone.data[-c(1, 10)]))
    
    avg.si <- mean(si[, 3])  
    
    si.list <- c(si.list,avg.si)
  }
  
  
}

plot(k.list,sumdiss.list,type = "b")

plot(k.list[-1],si.list,type = "b")

#SECOND PAM
### Partitioning Around Medoids ###

k = 3

abalone.pam <- pam(abalone.data[-c(1, 10)], k)

abalone.pam$objective

## get and plot clustering output 
assigned.clusters <- as.factor(abalone.pam$cluster)

ggplot(abalone.data, aes(x = length, y = diameter, colour = assigned.clusters)) +
  geom_point()

## Silhouette Plot
sil <- silhouette(abalone.pam$cluster, dist(abalone.data[-c(1, 10)]))
fviz_silhouette(sil)

## run tests with multiple k values and plot sum of dissimilarities (sum of distances)
k.list <- c(1,2,3,4,5,6,7)

sumdiss.list <- c()
si.list <- c()

for (k in k.list) {
  
  abalone.pam <- pam(abalone.data[-c(1, 10)], k)
  
  sumdiss <- abalone.pam$objective[2]
  
  sumdiss.list <- c(sumdiss.list,sumdiss)
  
  if (k>1){
    si <- silhouette(abalone.pam$cluster, dist(abalone.data[-c(1, 10)]))
    
    avg.si <- mean(si[, 3])  
    
    si.list <- c(si.list,avg.si)
  }
  
  
}

plot(k.list,sumdiss.list,type = "b")

plot(k.list[-1],si.list,type = "b")

