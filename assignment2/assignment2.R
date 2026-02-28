library(readr)
library(EnvStats)
library(nortest)
library(GGally)
library(ggplot2)
library(psych)
library(cluster)
library(dendextend)
library(colorspace)
library(factoextra)


# set working directory (relative path)
setwd("C:/Users/shann/Documents/School/spring26/itws6600/github/ITWS-6600-Data-Analytics/assignment2")

# read data
epi.data <- read_csv("epi_results_2024_pop_gdp.csv")

# view dataframe
View(epi.data)


## 1.1
## take a copy of a variable from a dataframe into a separate variable
BDH <- epi.data$BDH.new

# find NAs in variable: function outputs vector of logical values, true if NA, false otherwise
NAs <- is.na(BDH)

# take subset of NOT NAs from variable
BDH.complete <- BDH[!NAs]

# define sequence of values over which to plot histogram
x <- seq(0., 100., 5)

# histogram (frequency distribution) over specified range
hist(BDH.complete, x, prob=TRUE)

# print estimated density curve for variable
lines(density(BDH.complete,bw="SJ")) # or try bw=“SJ”

# print rug under histogram
rug(BDH.complete)


## 1.2
boxplot(BDH.complete ~ epi.data$region)

## 2.1
region1 <- epi.data[epi.data$region=="Global West",]
region1.bdh <- region1$BDH.new

# define sequence of values over which to plot histogram
x <- seq(0., 100., 5)

# histogram (frequency distribution) over specified range
hist(region1.bdh, x, prob=TRUE)

# print estimated density curve for variable
lines(density(region1.bdh,bw="SJ")) # or try bw=“SJ”

# print rug under histogram
rug(region1.bdh)

region2 <- epi.data[epi.data$region=="Asia-Pacific",]
region2.bdh <- region2$BDH.new

# define sequence of values over which to plot histogram
x <- seq(0., 100., 5)

# histogram (frequency distribution) over specified range
hist(region2.bdh, x, prob=TRUE)

# print estimated density curve for variable
lines(density(region2.bdh,bw="SJ")) # or try bw=“SJ”

# print rug under histogram
rug(region2.bdh)


## 2.2
# print quantile-quantile plot of two variables

qqplot(region1.bdh, region2.bdh, xlab = "Q-Q plot for BDH in Global West vs Asia-Pacific") 


## 3.1

#region1
region1.population <- region1$population

ggplot(region1, aes(x = log10(region1.bdh), y = log10(region1.population))) +
  geom_point()

## column names
names(region1)

lmod1 <- lm(log10(region1.bdh)~log10(region1.population), data = region1)

## print model output
summary(lmod1)

## better scatter plot of 2 variables with best fit line

ggplot(region1, aes(x = log10(region1.bdh), y = log10(region1.population))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")


ggplot(lmod1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)

region1.gdp <- region1$gdp

ggplot(region1, aes(x = log10(region1.bdh), y = log10(region1.gdp))) +
  geom_point()

## column names
names(region1)

lmod2 <- lm(log10(region1.bdh)~log10(region1.gdp), data = region1)

## print model output
summary(lmod2)

## better scatter plot of 2 variables with best fit line

ggplot(region1, aes(x = log10(region1.bdh), y = log10(region1.gdp))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")


ggplot(lmod2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)

#region2
region2.population <- region2$population

ggplot(region2, aes(x = log10(region2.bdh), y = log10(region2.population))) +
  geom_point()

## column names
names(region2)

lmod11 <- lm(log10(region2.bdh)~log10(region2.population), data = region2)

## print model output
summary(lmod11)

## better scatter plot of 2 variables with best fit line

ggplot(region2, aes(x = log10(region2.bdh), y = log10(region2.population))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")


ggplot(lmod11, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)

region2.gdp <- region2$gdp

ggplot(region2, aes(x = log10(region2.bdh), y = log10(region2.gdp))) +
  geom_point()

## column names
names(region2)

lmod21 <- lm(log10(region2.bdh)~log10(region2.gdp), data = region2)

## print model output
summary(lmod21)

## better scatter plot of 2 variables with best fit line

ggplot(region2, aes(x = log10(region2.bdh), y = log10(region2.gdp))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")


ggplot(lmod21, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)


## 4.1

region.group <- epi.data[epi.data$region=="Global West" | epi.data$region=="Asia-Pacific" | epi.data$region=="Eastern Europe",]

### K-Means ###

## set seed value for random number generator
set.seed(6)

## number of clusters to look for
k = 10

## learn clusters
region.group.km1 <- kmeans(region.group[c(5, 6, 12)], centers = k)


## Total (for all clusters) within clusters sum of squares
wcss <- region.group.km1$tot.withinss
wcss

### plot clustering output 
## put cluster assignments into a variable
assigned.clusters <- as.factor(region.group.km1$cluster)

## scatterplot of the data colored by cluster membership
ggplot(region.group, aes(x = population, y = gdp, colour = assigned.clusters)) +
  geom_point()

## compute pairwise distances between all points: 150 x 150 matrix
region.group.dist <- as.matrix(dist(region.group[c(5, 6, 12)]))

## Silhouette Plot
sil <- silhouette(region.group.km1$cluster, dist(region.group[c(5, 6, 12)]))
fviz_silhouette(sil)


## 4.2

### K-Means ###

## set seed value for random number generator
set.seed(6)

## number of clusters to look for
k = 10

## learn clusters
region.group.km2 <- kmeans(region.group[c(5, 6, 10)], centers = k)


## Total (for all clusters) within clusters sum of squares
wcss2 <- region.group.km2$tot.withinss
wcss2

### plot clustering output 
## put cluster assignments into a variable
assigned.clusters2 <- as.factor(region.group.km2$cluster)

## scatterplot of the data colored by cluster membership
ggplot(region.group, aes(x = population, y = gdp, colour = assigned.clusters2)) +
  geom_point()

## compute pairwise distances between all points: 150 x 150 matrix
region.group.dist2 <- as.matrix(dist(region.group[c(5, 6, 10)]))

## Silhouette Plot
sil2 <- silhouette(region.group.km2$cluster, dist(region.group[c(5, 6, 10)]))
fviz_silhouette(sil2)

