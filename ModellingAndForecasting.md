## Running various models such as Random forest, GBM. Kaggle rank 730
```{r}
#Trend plot
agg <- aggregate(train[Sales != 0]$Sales, by = list(train[Sales!=0]$month,train[Sales!=0]$year), FUN=sum)
SalesTS <- ts(agg$x, start=2013, frequency=12)
col = rainbow(3)
seasonplot(SalesTS, col=col, year.labels.left = TRUE, pch=19, las=1)

#Histogram:
hist(train$Sales,50)

#Aggregating the sales at a store level and taking the mean
hist(aggregate(train[Sales != 0]$Sales, 
               by = list(train[Sales != 0]$Store), mean)$x, 100, 
     main = "Mean sales per store", xlab = "mean sales")

#Customers 
hist(train$Customers,50)

#Aggregating the sales at a store level and taking the mean
hist(aggregate(train[Sales != 0]$Customers, 
               by = list(train[Sales != 0]$Store), mean)$x, 100, 
     main = "Mean Customers per store", xlab = "mean Customers")

#plotting scatter plot between sales & customers to check correlation
plot(aggregate(train[Sales!=0]$Sales, by = list(train[Sales!= 0]$Store), mean)$x,aggregate(train[Sales!=0]$Customers, by = list(train[Sales!= 0]$Store), mean)$x)
cor(train$Sales,train$Customers)

#Log transformation
plot(log(train$Sales),log(train$Customers))

#Checking if school holiday has an effect on sales:
ggplot(train[Sales != 0], aes(x = factor(SchoolHoliday), y = Sales)) +
    geom_jitter(alpha = 0.1) +
    geom_boxplot(color = "blue", outlier.colour = NA, fill = NA)

#Checking if promo has an effect on sales:
ggplot(train[Sales != 0], aes(x = factor(Promo), y = Sales)) +
    geom_jitter(alpha = 0.1) +
    geom_boxplot(color = "blue", outlier.colour = NA, fill = NA)

#Checking if day of week has an effect on sales:
ggplot(train[Sales != 0], aes(x = factor(DayOfWeek), y = Sales)) +
    geom_jitter(alpha = 0.1) +
    geom_boxplot(color = "blue", outlier.colour = NA, fill = NA)

# Analyzing store data
head(store)
table(store$StoreType)
table(store$Assortment)

# There is a connection between store type and type of assortment
table(store$Assortment,store$StoreType)
#Store b has only assortment a,b mainly

hist(store$CompetitionDistance, 50)


store$CompetitionOpenSince <- as.yearmon(paste(store$CompetitionOpenSinceYear, 
                                               store$CompetitionOpenSinceMonth, sep = "-"))

store$Promo2Since <- as.POSIXct(paste(store$Promo2SinceYear, 
                                   store$Promo2SinceWeek, 1, sep = "-"),
                             format = "%Y-%U-%u")

## Feauture engineering
train_store <- merge(train, store, by = "Store")
test_store <- merge(test, store, by = "Store")

ggplot(train_store[Sales != 0], aes(x = factor(PromoInterval), y = Sales)) + 
    geom_jitter(alpha = 0.1) + 
    geom_boxplot(color = "blue", outlier.colour = NA, fill = NA)

ggplot(train_store[Sales != 0], aes(x = factor(StoreType), y = Sales)) + 
    geom_jitter(alpha = 0.1) + 
    geom_boxplot(color = "blue", outlier.colour = NA, fill = NA)

ggplot(train_store[Sales != 0], 
       aes(x = as.Date(Date), y = Sales, color = factor(StoreType))) + 
    geom_smooth(size = 2)

ggplot(train_store[Sales != 0], 
       aes(x = as.Date(Date), y = Sales, color = factor(Assortment))) + 
    geom_smooth(size = 2)

ggplot(train_store[Sales != 0],
       aes(x = factor(!is.na(CompetitionOpenSinceYear)), y = Sales)) +
    geom_jitter(alpha = 0.1) +
    geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA) +
    ggtitle("Competition yes/no")

# Tuning for random forest and taking care of the levels
#tuneRF(x=train_store[,feature.names, with = FALSE], y=log(train_store$Sales+1), mtryStart=4, ntreeTry=100, stepFactor=1.5)

levels(test_store$StateHoliday) <- levels(train_store$StateHoliday)
levels(test_store$month) <- levels(train_store$month)
levels(test_store$year) <- levels(train_store$year)
test_store$Open[is.na(test_store$Open)] <- 1
test_store$Open <- as.integer(test_store$Open)

feature.names <- names(train_store)[c(1,2,6:12)]
library(randomForest)
rf <- randomForest(train_store[,feature.names, with = FALSE], 
                    log(train_store$Sales+1),
                    mtry=3,
                    ntree=50,
                    sampsize=100000,
                    do.trace=TRUE,
                    na.action=na.omit)

cat("Forecasting Sales\n")

predition <- exp(predict(rf, test_store[,feature.names, with = FALSE])) -1
submission1 <- data.frame(Id=test$Id, Sales=predition)

#Writing the submission file
cat("saving the submission file\n")
write.csv(submission1, "rf.csv")

#Variable mportance plots
cat("Variable Importance Plots\n")
importance(rf)
varImpPlot(rf)

#GBM
library("gbm")
data.names <- names(train_store)[c(1,2,4,6:12)]
gbm <- gbm(Sales ~ ., data=train_store[,data.names, with = FALSE], distribution="gaussian", n.trees=1000, interaction.depth=4)

cat("Predicting Sales\n")
pred2 <- predict(gbm, newdata=test_store, n.trees=5000)

submission2 <- data.frame(Id=test$Id, Sales=pred2)

#Writing the submission file
cat("saving the submission file\n")
write.csv(submission2, "gbm.csv")

## Ensemble of the two models*
submission3 <- merge(submission1,submission2,by = "Id")
submission3$ensemble = (pred1 + pred2)/2
cat("saving the submission file\n")
write.csv(submission3, "ensembele.csv")
```
