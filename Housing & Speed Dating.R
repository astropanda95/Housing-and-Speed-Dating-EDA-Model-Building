#John Victor Kanaparthy
#G01283591
#Homework 8

#Calling relevant libraries
library(tidyverse)
library(GGally)
library(corrplot)
library(rpart)
library(rpart.plot)
library(DMwR2) 
library(Ecdat)

#Housing Data

#Calling the housing data
data(Housing)
Housing

#Understanding the housing dataset
help(package="Ecdat", Housing)

#Creating another housing data dataframe to coonvert the price to thousands
Housing_data<-Housing

#Converting the price column and removing the dupplicate price 
Housing_data$price_thousands<- round(Housing_data$price/1000,0)
Housing_data<- subset(Housing_data,select = -price)
str(Housing_data)

#Creating a plot to understand the relationship between the data
vars <- dplyr::select(Housing_data,price_thousands,everything())
GGally::ggpairs(vars,
                lower=list(continuous='blank',
                           combo='blank', 
                           discrete='blank'),
                upper=list(continuous="points"
                           , combo="facethist", discrete="facetbar"),
                switch="y")


#Setting seed for randomization
SEED<-9301
set.seed(SEED)

#Calling rpart()with small cp to create object for call to plotcp()
rpart.housing=rpart(price_thousands~.,data = Housing_data, 
                    method="anova", cp=0.001)
plotcp(rpart.housing)

#Pruning data with se=1 
rpart.housing.1se <- rt.prune(rpart.housing, se=1)

#Creating a plot for 1se tree
rpart.plot(rpart.housing.1se , extra=1, roundint=FALSE,
           digits=3, main="1-se Housing regression tree")

var(Housing_data$price_thousands)
printcp(rpart.housing.1se)

#The cross-validated training MSE is 711.64 x 0.54620 = 388.69
#The half-width of the 95% confidence interval is
# 2 x 711.64 x 0.048244 = 68.66. 



#Speed Dating Data

#Calling relevant libraries
library(randomForest)
library(MASS)

#Reading csv
dating<-read_csv('Speed Dating Subset.csv')
str(dating)

#Omitting blank cells
dating<-na.omit(dating)
dating

#Converting the dec column as factor
dating$dec<-as.factor(dating$dec)

str(dating)

#Subtask 2.1

# Create a training data set so can compare test MSE with 
# that from regression trees

set.seed(9301)

train=sample(1:nrow(dating),size=3500)
test<-(-train)

#Calling randomForest to create the model
set.seed(8974)
rf.dating=randomForest(dec~.-gender,data=dating,subset = train,importance=TRUE)
rf.dating

#Plotting the model
plot(rf.dating, main="Bagged trees, mtry=3, ntrees=500")

#Computing test MSE 
dating.test=dating[test,"dec"]
yhat.dating = predict(rf.dating,newdata=dating[test,])
mean((yhat.dating!=dating.test$dec)^2)


#Subtast 2.2

#Creating another dataframe filtering out for males
dating_male = filter(dating,gender==1)
dating_male

#Creating a model by calling randomForest
set.seed(9301)
rf.dating_male=randomForest(dec~.-gender,data=dating_male,
                            importance=TRUE)
rf.dating_male

#Plotting the model
varImpPlot(rf.dating_male,
           main="Plots of importance measures for male dating data")

#Creating another dataframe filtering out for females
dating_female=filter(dating,gender==0)
dating_female

#Creating a model by calling randomForest
set.seed(9301)
rf.dating_female=randomForest(dec~.-gender,data=dating_female,
                            importance=TRUE)
rf.dating_female

#Plotting the model
varImpPlot(rf.dating_female,
           main="Plots of importance measures for female dating data")

#2.3.c 
#Using logictic regression to understand the relationship between
#decision and ambition
summary(glm(dec~amb,data=dating,family=binomial))
#The p values are well below 0.05, thus it is statistically significant
