library("ggplot2")
library("readr")



## read dataset
NY_House_Dataset <- read_csv("C:/Users/shann/Documents/School/spring26/itws6600/github/ITWS-6600-Data-Analytics/lab2/NY-House-Dataset.csv")

dataset <- NY_House_Dataset


## PROPERTYSQFT

ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()


## filter data
dataset <- dataset[dataset$PRICE<195000000,]

## column names
names(dataset)

## fit linear model
lmod0 <- lm(PRICE~PROPERTYSQFT, data = dataset)

## print model output
summary(lmod0)

## scatter plot of 2 variables
plot(PRICE~PROPERTYSQFT, data = dataset)
abline(lmod0)


## BEDS

ggplot(dataset, aes(x = BEDS, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = log10(BEDS), y = log10(PRICE))) +
  geom_point()

## column names
names(dataset)

## fit linear model
lmod0 <- lm(PRICE~BEDS, data = dataset)

## print model output
summary(lmod0)

## scatter plot of 2 variables
plot(PRICE~BEDS, data = dataset)
abline(lmod0)


## BATH

ggplot(dataset, aes(x = BATH, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = log10(BATH), y = log10(PRICE))) +
  geom_point()

## column names
names(dataset)

## fit linear model
lmod0 <- lm(PRICE~BATH, data = dataset)

## print model output
summary(lmod0)

## scatter plot of 2 variables
plot(PRICE~BATH, data = dataset)
abline(lmod0)

