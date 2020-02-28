library(dplyr)
library(tidyr)
library(caTools)
setwd("G:\\R dataScience\\WineQuality")

RedWine =read.csv("winequality-red.csv",sep = ';')
WhiteWine =read.csv("winequality-white.csv", sep=';')

RedWine_split= sample.split(RedWine,SplitRatio = 0.8)
WhiteWine_split = sample.split(RedWine,SplitRatio = 0.8)
RedWine_train = subset(RedWine, RedWine_split==TRUE)
RedWine_test = subset(RedWine, RedWine_split==FALSE)
WhiteWine_train = subset(WhiteWine, WhiteWine_split==TRUE)
WhiteWine_test = subset(WhiteWine, WhiteWine_split==FALSE)


#####linearRegression
#Redwine
linear_model=lm(quality~volatile.acidity+chlorides+free.sulfur.dioxide+
                  total.sulfur.dioxide+sulphates+alcohol,data=RedWine_train)
linear_model2=lm(quality~.,data=RedWine_train)

results=predict(linear_model,newdata=RedWine_test,type="response")
results2=predict(linear_model2,newdata=RedWine_test,type="response")

table(RedWine_test$quality,round(results))
table(RedWine_test$quality,round(results2))

#whitewine
linear_model3=lm(quality~volatile.acidity+chlorides+free.sulfur.dioxide+
                  total.sulfur.dioxide+sulphates+alcohol,data=WhiteWine_train)
linear_model4=lm(quality~.,data=WhiteWine_train)
summary(linear_model3)
summary(linear_model4)
results=predict(linear_model3,newdata=WhiteWine_test,type="response")
results2=predict(linear_model4,newdata=WhiteWine_test,type="response")
table(WhiteWine_test$quality,round(results))
table(WhiteWine_test$quality,round(results2))

###randomForest
#redWine
install.packages("randomForest")
library(randomForest)
set.seed(123)
classifier=randomForest(x=RedWine_train[,-12],y=RedWine_train$quality,ntree=500)
ypred=predict(classifier,RedWine_test[,-12])
(table(RedWine_test[,12],round(ypred)))

#WhiteWine
classifier2=randomForest(x=WhiteWine_train[,-12],y=WhiteWine_train$quality,ntree=500)
ypred=predict(classifier2,WhiteWine_test[,-12])
(table(WhiteWine_test[,12],round(ypred)))

#####decisionTree

#redWine
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

treemodel = rpart(quality~.,RedWine_train,method = "class")
RedWinepre=predict(treemodel,RedWine_test,type="class")
table(RedWine_test$quality,RedWinepre)
error=mean(RedWinepre!=RedWine_test$quality)
print(paste("Accuracy=",1-error))

treemodel2 = rpart(quality~.,WhiteWine_train,method = "class")
WhiteWinepre=predict(treemodel,WhiteWine_test,type="class")
table(WhiteWine_test$quality,WhiteWinepre)
error=mean(WhiteWinepre!=WhiteWine_test$quality)
print(paste("Accuracy=",1-error))
