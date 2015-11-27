```
##################################### The following fields are available ################################################
Store / Store Type / Assortment
Day of the week / Date
Sales / # Customers
Open/Closed
Promo / promo interval/ promo since week
Distance from competitor / Competitor open since

################################# Loading the libraries and creating training testing datasets ##########################
library(data.table)
library(zoo)
library(lubridate)
library(forecast)
library(ggplot2)
train <- read.csv('train.csv')
test <- read.csv('test.csv')
store <- read.csv('store.csv')
train <- data.table(train)
test <- data.table(test)
store <- data.table(store)
###################################################### Data Cleaning ####################################################
train$Date <- as.Date(train$Date)
test$Date <- as.Date(test$Date)
train$month <- as.factor(format(train$Date, "%m"))
train$year <- as.factor(format(train$Date, "%y"))
train$day <- as.factor(format(train$Date, "%d"))

test$month <- as.factor(format(test$Date, "%m"))
test$year <- as.factor(format(test$Date, "%y"))
test$day <- as.factor(format(test$Date, "%d"))

train <- train[order(Date)]
test <- test[order(Date)]
summary(train)

train[, lapply(.SD, function(x) length(unique(x)))]
test[, lapply(.SD, function(x) length(unique(x)))]
sum(unique(test$Store) %in% unique(train$Store)) 

#Open/Closed
table(train$Open)/nrow(train)
table(test$Open)/nrow(test)

#Promo/No promo
table(train$Promo) / nrow(train)
table(test$Promo) / nrow(test)

#State Holiday or not
table(train$StateHoliday) / nrow(train)
table(test$StateHoliday) / nrow(test)

#School holiday
table(train$SchoolHoliday) / nrow(train)
table(test$SchoolHoliday) / nrow(test)
```

