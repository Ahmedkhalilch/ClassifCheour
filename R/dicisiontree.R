#'@param the chosen parametre must be betwen 0 and 1 and that will be the pourcentage of the train and the rest will be test
#'@description dicision tree model



DecisTree = function(splitrat){
  library(ggplot2)
  library(readr)
  library(lattice)
  library(caret)
  library(randomForest)
  library(caTools)
  library(rpart)
  library(rpart.plot)
  library(dplyr)
  library(FactoMineR)
  data(iris)
  mushrooms=data.frame(iris)

  set.seed(101)

  sample = sample.split(mushrooms$Species, SplitRatio = splitrat)

  x_train = subset(mushrooms,sample==TRUE)
  x_test = subset(mushrooms,sample== FALSE)


  y_train <- x_train$Species
  y_test <- x_test$Species

  x_train$Species <- NULL
  x_test$Species <- NULL


  #Cross Validation

  cv.10.folds <- createMultiFolds(y_train,k=10,times=2)
  ctrl.1 <- trainControl(method="repeatedcv",number=10,repeats = 2,index = cv.10.folds)

  mydt <- train (x= x_train,y=y_train,method="rpart",trControl = ctrl.1, tuneLength = 5)

  #Variable Importance Plot

  plot(varImp(mydt),main="DT - Variable Importance Plot")

}
