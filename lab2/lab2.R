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

dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]

## column names
names(dataset)

lmod1 <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)

## print model output
summary(lmod1)

## better scatter plot of 2 variables with best fit line

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")


ggplot(lmod1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)


## BEDS

ggplot(dataset, aes(x = BEDS, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = log10(BEDS), y = log10(PRICE))) +
  geom_point()

## column names
names(dataset)

lmod2 <- lm(log10(PRICE)~log10(BEDS), data = dataset)

## print model output
summary(lmod2)

## better scatter plot of 2 variables with best fit line

ggplot(dataset, aes(x = log10(BEDS), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")


ggplot(lmod2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)


## BATH

ggplot(dataset, aes(x = BATH, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = log10(BATH), y = log10(PRICE))) +
  geom_point()

#FILTERS
NAs <- is.na(dataset$BATH)
dataset <- dataset[dataset$BATH!=NAs,]
dataset <- dataset[dataset$BATH<33,]

## column names
names(dataset)

lmod3 <- lm(log10(PRICE)~log10(BATH), data = dataset)

## print model output
summary(lmod3)

## better scatter plot of 2 variables with best fit line

ggplot(dataset, aes(x = log10(BATH), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")


ggplot(lmod3, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)

