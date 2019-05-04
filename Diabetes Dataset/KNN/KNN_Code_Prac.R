# Basic Step 
install.packages("ISLR")
library(ISLR)
summary(Caravan$Purchase)
str(Caravan)
any(is.na(Caravan))
var(Caravan[,1])
var(Caravan[,2])
purchase <- Caravan[,86]
summary((purchase))
standardized.Caravan <- scale(Caravan[,-86])
print(standardized.Caravan[,1])
print(standardized.Caravan[,2])
# Train Test Split 
test.index <- 1:1000
test.data <- standardized.Caravan[test.index,]
test.purchase <- purchase[test.index]
# Train 
train.data <- standardized.Caravan[-test.index,]
train.purchase <- purchase[-test.index]
# KNN Model 
library(class)
set.seed(101)

predicted.purchase <- knn(train.data,test.data,train.purchase,k=1)
print(head(predicted.purchase))
mean(test.purchase != predicted.purchase)
misclass.error <- mean(test.purchase != predicted.purchase)
print(misclass.error)
#Choosing a K value 
predicted.purchase = NULL
error.rate <- NULL
for (i in 1:20) {
    set.seed(101)
    predicted.purchase = knn(train.data,test.data,train.purchase,k=i)
    error.rate[i] = mean(test.purchase != predicted.purchase)
}
print(error.rate)
# Visualization K Elbro method
library(ggplot2)
k.values <- 1:20 
error.df <- data.frame(error.rate,k.values)
print(error.df)
ggplot(error.df,aes(k.values,error.rate))+geom_point()+geom_line(lty="dotted",color='red')

