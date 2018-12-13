


  #Function to count levels for each variable

  #'@description this function will allow you to have a general view of the data "iris" with the function summary

  IrisSummary <- function () {
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
    data("iris")
    mushrooms=data.frame(iris)
    return(summary(mushrooms))
  }


