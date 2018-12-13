
#'@description this function will allow you to know the level of each variable

LevelsIris <- function () {for (i in 1:ncol(mushrooms)) {
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
  print (c(names(mushrooms)[i],length(levels(mushrooms[,i]))))

}}
