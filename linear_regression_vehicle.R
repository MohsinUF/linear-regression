# libraries
library(mice)
library(VIM)
library(faraway)
library(tidyverse)

# reading data
data = read.csv(file.choose(), header = T)
head(data)
str(data)
summary(data)
data$State = as.factor(data$State)

# creating a function for working with missing values in the data
func = function(x){
  sum(is.na(x))/length(x)*100
}

# finding the missing values
apply(data, 2, func)
md.pattern(data)
md.pairs(data)
marginplot(data[, c("lh", "Mileage")])

# imputing the missing values
impute = mice(data[,2:7], m = 5, seed = 1212)
impute
impute$imp$lh

# getting the cleaned data
n.data = complete(impute, 1)

# data partitioning
part = sample(2, nrow(n.data), replace = T, prob = c(0.7, 0.3))
train = n.data[part == 1,]
test = n.data[part == 2,]

# creating the linear model
model = lm(lc ~ lh + mc, train)
model
summary(model)
plot(lc ~ lh, train)
abline(model, col = "red")

# looking for multicolinearity in train data
pairs(train[2:6])
vif(model) # since the value is 1 for lh which is less than 10, no multicolinearity exists.

# making prediction with test data
pred = predict(model, test)
head(pred)
head(test$lc)
predict(model, data.frame(lh = 10, mc = 5))
