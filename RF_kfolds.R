source("F:/BD_erosion/analyse/analyseR/random_forest/SCRIPTS/environnement.R")
source(dirPROG %+% "/na_values.R")
library(ROCR)



set.seed(4687)
##Split in 10 folds
training$erosion <- log10(training$erosion)
myfolds <- createFolds(training$erosion, k=10, list=F, returnTrain = F)
testfold <- function(fold) training[which(myfolds == fold), ]
trainfolds <- function(fold) training[which(myfolds != fold), ]
rmse <-1; rsq <- 2


##Evaluation matrix
eval <- data.frame(model="", ntree=0, mtry=0, nodesize=rep(0,10),RMSE=0,Rsquared=0)
eval$model <- as.character(eval$model)

#RF1, random forest variables par defaut
  rf1 <- randomForest(erosion~. , trainfolds(1))
  varImpPlot(rf1)
  pred1 <- predict(rf1, testfold(1))
  summary(pred1 - testfold(1)$erosion)
  ggplot(data.frame(pred = pred1, true=testfold(1)$erosion), aes(true, pred)) + geom_point() + theme_bw() + geom_abline(col="cyan4") + ggtitle("random forest, \ndefault parameters")
    ggsave(dirFIG %+% "/rf_pred_default_params.png", width=4,height=3.5)
  eval[1,] <- c("rf1", rf1$ntree, rf1$mtry, rf1$forest$nrnodes, postResample(pred1, testfold(1)$erosion)[rmse],postResample(pred1, testfold(1)$erosion)[rsq])

#Influence du nombre d'arbre, fold 2
  evalntree <- data.frame(ntree=0, mtry=0, nodesize=rep(0,100),RMSE=0,Rsquared=0)
  
  for (i in seq(1:100)){
    rf <- randomForest(erosion~. , trainfolds(2), ntree=i*5)
    pred <- predict(rf, testfold(2))
    evalntree[i,] <- c(rf$ntree, rf$mtry, rf$forest$nrnodes, postResample(pred, testfold(2)$erosion)[rmse],postResample(pred, testfold(2)$erosion)[rsq])
  }
  ggplot(evalntree, aes(ntree, 1-RMSE)) + geom_point() + theme_bw() 
  ggplot(evalntree, aes(ntree, Rsquared)) + geom_point() + theme_bw() 
  ggplot(evalntree, aes(ntree, 1-RMSE)) + geom_point(color=summer5[5], size=1.8)  + geom_point(aes(ntree, Rsquared), color=summer5[4], size=1.8) + ylab("R²   -   1-RMSE") +theme_light() + scale_x_continuous(breaks = seq(0,500,by=40))
    ggsave(dirFIG %+% "/ntree_influence.png", width=5,height=6)  

#Influence du mtry, fold 3
  evalmtry <- data.frame(ntree=0, mtry=0, nodesize=rep(0,17),RMSE=0,Rsquared=0)
  
  for (i in seq(1:17)){
    rf <- randomForest(erosion~. , trainfolds(3), mtry=i)
    pred <- predict(rf, testfold(3))
    evalmtry[i,] <- c(rf$ntree, rf$mtry, rf$forest$nrnodes, postResample(pred, testfold(3)$erosion)[rmse],postResample(pred, testfold(3)$erosion)[rsq])
  }
  ggplot(evalmtry, aes(mtry, 1-RMSE)) + geom_point(color=summer5[5], size=1.8) + theme_bw() + geom_point(aes(mtry, Rsquared), color=summer5[4], size=1.8) + ylab("R²   -   1-RMSE") + scale_x_continuous(breaks = seq(1,17,by=2)) + geom_vline(xintercept = 2, color="gold", alpha=0.2, size=10)
    ggsave(dirFIG %+% "/mtry_influence_new.png", width=5,height=5) 
    
#Influence of maxnode, fold 4    
    evalmaxnode <- data.frame(ntree=0, mtry=0, maxnode=rep(0,50),RMSE=0,Rsquared=0)
    
    for (i in seq(1:100)){
      rf <- randomForest(erosion~. , trainfolds(4), maxnode=i)
      pred <- predict(rf, testfold(4))
      evalmaxnode[i,] <- c(rf$ntree, rf$mtry, i, postResample(pred, testfold(4)$erosion)[rmse],postResample(pred, testfold(4)$erosion)[rsq])
    }
    ggplot(evalmaxnode, aes(maxnode, 1-RMSE)) + geom_point(color=summer5[5], size=1.8) + theme_bw() + geom_point(aes(maxnode, Rsquared), color=summer5[4], size=1.8) + ylab("R²   -   1-RMSE") + scale_x_continuous(breaks = seq(0,200,by=20))
      ggsave(dirFIG %+% "/maxnode_influence_new.png", width=5,height=5) 
    
#Avec paramètres "optimaux" maxnode=150, ntree=200,  mtry=2
  rfopt <- randomForest(erosion~., trainfolds(5), maxnode=50, ntree=120,  mtry=2)  
  predopt <- predict(rfopt, testfold(5)) 
  summary(predopt - testfold(5)$erosion)
  ggplot(data.frame(pred = predopt, true=testfold(5)$erosion), aes(true, pred)) + geom_point() + theme_bw() + geom_abline(col="cyan4") + ggtitle("random forest, \ndefault parameters")
  postResample(predopt, testfold(5)$erosion)  
  eval[4,] <- c("rfopt", rfopt$ntree, rfopt$mtry, rfopt$forest$nrnodes, postResample(predopt, testfold(5)$erosion)[rmse],postResample(predopt, testfold(5)$erosion)[rsq])
  
  
  #r boost
    rboost <- train(erosion~., trainfolds(5), method="gbm")
    predboost <- predict(rboost, testfold(5))
    summary(predboost)
    postResample(predboost, testfold(5)$erosion)
    
  #Combinaison boost rf
    dfcomb <- data.frame(predopt, predboost, erosion = testfold(5)$erosion)
    modcombo <- randomForest(erosion~. , data=dfcomb)
    predcombo <- predict(modcombo, dfcomb)
    ggplot(data.frame(pred = predcombo, true=testfold(5)$erosion), aes(true, pred)) + geom_point() + theme_bw() + geom_abline(col="cyan4") + ggtitle("random forest, \ndefault parameters")
    postResample(predcombo, testfold(5)$erosion)
    eval[3,] <- c("combo", modcombo$ntree, modcombo$mtry, modcombo$forest$nrnodes, postResample(predcombo, testfold(5)$erosion)[rmse],postResample(predcombo, testfold(5)$erosion)[rsq])
    
