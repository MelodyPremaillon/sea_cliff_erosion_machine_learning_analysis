source("F:/BD_erosion/analyse/analyseR/random_forest/SCRIPTS/environnement.R")
source(dirPROG %+% "/na_values.R")
library(ROCR)

set.seed(8652)
##Split in 10 folds
  training$erosion <- log10(training$erosion)
  myfolds <- createFolds(training$erosion, k=10, list=F, returnTrain = F)
  testfold <- function(fold) training[which(myfolds == fold), ]
  trainfolds <- function(fold) training[which(myfolds != fold), ]
  accuracy <-1; kappa <- 2

##Evaluation matrix
eval <- data.frame(model="", ntree=0, mtry=0, nodesize=rep(0,10),RMSE=0,Rsquared=0)
eval$model <- as.character(eval$model)

##Erosion class
training$erosionclass <- cut(training$erosion, breaks = log10(c(0.0001,0.05,0.2,50)), labels = c("slow", "intermediate", "high"))
ggplot(training, aes(erosionclass, erosion)) + geom_boxplot() +theme_light() + scale_y_continuous(breaks=c(-3,-2,-1,0,1,2),labels=c("1mm/yr","1cm/yr","10cm/yr","1m/yr","10m/yr",""))
  ggsave(dirFIG %+% "/erosion_class_boxplot.png", width=5, height=5)

  training <- training[,-8]

##One tree, fold 1
modFittree <- train(erosionclass~. , trainfolds(1), method="rpart")
predtree <- predict(modFittree, testfold(1))
confusionMatrix(predtree, testfold(1)$erosionclass)
    #Accuracy 0.8306, kappa = 0.5485

  #plot matrice de confusion
  confusion <- as.data.frame(confusionMatrix(predtree, testfold(1)$erosion)[[2]])
  for (i in 0:2){
    confusion$FreqPercent[(i*3+1):((i+1)*3)] <- round(confusion$Freq[(i*3+1):((i+1)*3)] / sum(confusion$Freq[(i*3+1):((i+1)*3)]) * 100 , 1)
  }
  ggplot(confusion, aes(x=Reference, y=Prediction, fill=FreqPercent)) + geom_tile() + geom_text(aes(x=Reference, y=Prediction, label= Freq)) + theme_minimal() + scale_fill_gradient(low="orange", high="cyan4") + ggtitle("One tree")
    ggsave(dirFIG %+% "/confusionmatrix_1tree.png", height=4, width=5)
  
##RF, defaults parameters
modFitRF <- randomForest(erosionclass~. , trainfolds(1))
predRF <- predict(modFitRF, testfold(1))
confusionMatrix(predRF, testfold(1)$erosionclass)
varImpPlot(modFitRF)
  #Accuracy 0.84, kappa=0.5416
  #plot matrice de confusion
  confusion <- as.data.frame(confusionMatrix(predRF, testfold(1)$erosion)[[2]])
  for (i in 0:2){
    confusion$FreqPercent[(i*3+1):((i+1)*3)] <- round(confusion$Freq[(i*3+1):((i+1)*3)] / sum(confusion$Freq[(i*3+1):((i+1)*3)]) * 100 , 1)
  }
  ggplot(confusion, aes(x=Reference, y=Prediction, fill=FreqPercent)) + geom_tile() + geom_text(aes(x=Reference, y=Prediction, label= Freq)) + theme_minimal() + scale_fill_gradient(low="orange", high="cyan4") + ggtitle("Random Forest (default parameters)")
    ggsave(dirFIG %+% "/confusionmatrix_rfdefault.png", height=4, width=5)

##RF, 10 trees
modFitRF2 <- randomForest(erosionclass~. , trainfolds(3), ntree=20)
predRF2 <- predict(modFitRF2, testfold(3))
confusionMatrix(predRF2, testfold(3)$erosion)

  ##Evaluation nombre d'arbre
  evalntree <- data.frame(ntree=0, mtry=0, accuracy=0, kappa=0)
  for (i in seq(1:100)){
    rf <- randomForest(erosionclass~. , trainfolds(4), ntree=i*5)
    pred <- predict(rf, testfold(4))
    evalntree[i,] <- c(rf$ntree, rf$mtry, postResample(pred, testfold(4)$erosion)[accuracy],postResample(pred, testfold(4)$erosion)[kappa])
  }
  ggplot(evalntree, aes(ntree, accuracy)) + geom_point(color=summer5[5], size=1.8)  + geom_point(aes(ntree, kappa), color=summer5[4], size=1.8) + ylab("kappa      -      accuracy") +theme_light() + scale_x_continuous(breaks = seq(0,500,by=40))
    ggsave(dirFIG %+% "/eval_ntree_erosionclass.png", height=5,width=5)
  ##Evaluation mtry
  evalmtry <- data.frame(ntree=0, mtry=0, accuracy=0, kappa=0)
  for (i in seq(1:17)){
    rf <- randomForest(erosionclass~. , trainfolds(4), mtry=i)
    pred <- predict(rf, testfold(4))
    evalmtry[i,] <- c(rf$ntree, rf$mtry, postResample(pred, testfold(4)$erosion)[accuracy],postResample(pred, testfold(4)$erosion)[kappa])
  }
  ggplot(evalmtry, aes(mtry, accuracy)) + geom_point(color=summer5[5], size=2)  + geom_point(aes(mtry, kappa), color=summer5[4], size=2) + ylab("kappa      -      accuracy") +theme_light() + scale_x_continuous(breaks = seq(1,17,by=2)) 
  
  ##Evaluation maxnode
  evalmaxnode <- data.frame(ntree=0, mtry=0, maxnode=0,accuracy=0, kappa=0)
  for (i in seq(1:100)){
    rf <- randomForest(erosionclass~. , trainfolds(4), maxnode=i)
    pred <- predict(rf, testfold(4))
    evalmaxnode[i,] <- c(rf$ntree, rf$mtry, i, postResample(pred, testfold(4)$erosion)[accuracy],postResample(pred, testfold(4)$erosion)[kappa])
  }
  ggplot(evalmtry, aes(mtry, accuracy)) + geom_point(color=summer5[5], size=2)  + geom_point(aes(mtry, kappa), color=summer5[4], size=2) + ylab("kappa      -      accuracy") +theme_light() + scale_x_continuous(breaks = seq(1,17,by=2)) 
  
##Boosting
  modfitboost <- train(erosionclass~., trainfolds(5), method="gbm")
  predboost <- predict(modfitboost, testfold(5))
  confusionMatrix(predboost, testfold(5)$erosion)
  
  