source("F:/BD_erosion/analyse/random_forest/SCRIPTS/environnement.R")
source(dirPROG %+% "/na_values.R")
library(pROC)
library(ranger)
library(ggplot2)
library(caret)
source(dirBD)
library(randomForest)
set.seed(8652)


#Chargement des packages et de la DB       

##Erosion class
  training$erosion <- log10(training$erosion)
  training$erosionclass <- cut(training$erosion, breaks = log10(c(0.0001,0.1,50)), labels = c("slow",  "high"))
  training <- training[,-which(colnames(training)=="erosion")]
  testing$erosion <- log10(testing$erosion)
  testing$erosionclass <- cut(testing$erosion, breaks = log10(c(0.0001,0.1,50)), labels = c("slow",  "high"))
  testing <- testing[,-which(colnames(testing)=="erosion")]

##Random forest
  ## RF, whith best parameters
  modFitRF <- randomForest(erosionclass~. , training, ntree=3000, mtry=2, nodesize = 1)
  predRF <- predict(modFitRF,testing,type="prob")
  
  
  #Courbe ROC
  myauc <- roc(testing$erosionclass, predRF[,1])
  plot(myauc)
  
  #Matrice de confusion
  predRF <- predict(modFitRF,testing)
  matconf <- table(testing$erosionclass,predRF)
  
  #Importance des variables
  varImpPlot(modFitRF)
  
  
  