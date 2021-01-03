  source("F:/BD_erosion/analyse/random_forest/SCRIPTS/environnement.R")
  source(dirPROG %+% "/na_values.R")
  library(pROC)
  library(ranger)
  
  set.seed(8652)
  ##Split in 10 folds
  training$erosion <- log10(training$erosion)
  
  
  
  nfolds <- 10
  myfolds <- createFolds(training$erosion, k=nfolds, list=F, returnTrain = F)
  
  testfold <- function(fold) training[which(myfolds == fold), ]
  trainfolds <- function(fold) training[which(myfolds != fold), ]
  
  
  # accuracy <-1; kappa <- 2
  
  ##Erosion class
  training$erosionclass <- cut(training$erosion, breaks = log10(c(0.0001,0.1,50)), labels = c("slow",  "high"))
  ggplot(training, aes(erosionclass, erosion)) + geom_boxplot() +theme_light() + scale_y_continuous(breaks=c(-3,-2,-1,0,1,2),labels=c("1mm/yr","1cm/yr","10cm/yr","1m/yr","10m/yr",""))
    #ggsave(dirFIG %+% "/2erosion_class_boxplot.png", width=5, height=5)
  
  training <- training[,-which(colnames(training)=="erosion")]
  testing$erosion <- log10(testing$erosion)
  testing$erosionclass <- cut(testing$erosion, breaks = log10(c(0.0001,0.1,50)), labels = c("slow",  "high"))
  testing <- testing[,-which(colnames(testing)=="erosion")]
  
##One tree, fold 1

    eval <- data.frame(auc=rep(0,nfolds*2),meth="")
    for (i in 1:nfolds){
      modFittree <- train(erosionclass~. , trainfolds(i), method="rpart")
      predTree <- predict(modFittree, testfold(i), type="prob")
      myauc <- roc(testfold(i)$erosionclass, predTree[,1] )$auc
      eval$auc[i] <- as.numeric(myauc)
    }
    eval$meth <- as.character(eval$meth)
    eval$meth[1:nfolds] <- "1tree"
    ggplot(eval[which(eval$meth!= ""),], aes(meth,auc)) + geom_boxplot() 
    
    modFittree <- train(erosionclass~. , training, method="rpart")
    plot(modFittree$finalModel, uniform=TRUE, main="Classification Tree")
    text(modFittree$finalModel, use.n=TRUE, all=TRUE, cex=.8)
    
    library(rattle)
    library(rpart)
    modFittree <- rpart(erosionclass~. , training)
    plot(modFittree)
    fancyRpartPlot(modFittree)

##RF, defaults parameters
    for (i in 1:nfolds){
      modFitRF <- randomForest(erosionclass~. , trainfolds(i), ntree=500, mtry=1, nodesize=1)
      predRF <- predict(modFitRF, testfold(i), type="prob")
      myauc <- roc(testfold(i)$erosionclass, predRF[,1] )$auc
      eval$auc[i+nfolds] <- as.numeric(myauc)
    }
    eval$meth[(nfolds+1):(2*nfolds)] <- "RFdefault" 
    ggplot(eval[which(eval$meth!= ""),], aes(meth,auc)) + geom_boxplot(fill="gold", alpha=0.7) + scale_y_continuous(breaks = seq(0.75,1,by=0.05),limits = c(0.75,1)) + theme_light() + xlab(NULL)
      ggsave(dirFIG %+% "/comp_tree_rfdefault_boxplot_auc.png")
      ggsave("F:/manuscrit/chap3/figures/comp_tree_rfdefault_boxplot_auc.pdf")
    
    modRF <- randomForest(erosionclass~. , training)
    predRFtesty <- predict(modRF, testing)
    varImpPlot(modRF)
    
    ROCtree <- roc(testfold(5)$erosionclass, predRFtesty )
    ROCtree$auc
    plot(ROCtree)


## RF, whith best parameters
    modFitRF <- randomForest(erosionclass~. , training, ntree=3000, mtry=2, nodesize = 1)
    predRF <- predict(modFitRF,testing, type="prob")
    myauc <- roc(testing$erosionclass, predRF[,1] )


