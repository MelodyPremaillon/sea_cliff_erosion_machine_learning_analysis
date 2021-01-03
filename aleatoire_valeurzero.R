source("F:/BD_erosion/analyse/analyseR/random_forest/SCRIPTS/environnement.R")
source(dirPROG %+% "/na_values.R")

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
training$ID <- seq(1:length(training$publication_key))

##Création d'une nouvelle valeur d'érosion corrigée avec une valeur aléatoire entre 0 et le seuil de détection  
  training$eroscorr <- training$erosion
  for (i in which(training$erosion == 0.001)){
    training$eroscorr[i] <- runif(1,0.001,0.10)
  }
  ggplot(training, aes(log10(eroscorr))) + geom_density(fill=summer5[4], alpha=0.8) +theme_light() + scale_x_continuous(breaks=c(-3,-2,-1,0,1,2),labels=c("1mm/yr","1cm/yr","10cm/yr","1m/yr","10m/yr","")) 
  
  training$sourcetmp <- 'publi' ; training$sourcetmp[which(training$publication_key == "070_cerema_2016")] <- 'cerema'
  ggplot(training, aes(log10(eroscorr), fill=sourcetmp)) + geom_density(alpha=0.5, color="grey30") +theme_light() + scale_x_continuous(breaks=c(-3,-2,-1,0,1,2),labels=c("1mm/yr","1cm/yr","10cm/yr","1m/yr","10m/yr","")) + scale_fill_manual(values = summer5[c(1,4)])
  
  ggplot(training[which(training$erosion == 0.001),], aes(ID, erreur.harmo)) + geom_point() + theme_bw() + geom_point(aes(ID, eroscorr), color="gold") + xlim(200,1600) 
  
  
##Valeur aléatoire avec une loie normale centrée en 1mm/an
  
  training$eroscorr2 <- training$erosion
  for (i in which(training$erosion == 0.001)){
    x <- rnorm(1,mean = 0.001,sd=2)
    while (x < 0.01 | x > 10){
      x <- rnorm(1,mean = 0.001,sd= 0.05)
    } 
    training$eroscorr2[i] <- x
  }
  
  ggplot(training, aes(log10(eroscorr2))) + geom_density(fill=summer5[4], alpha=0.8) +theme_light() + scale_x_continuous(breaks=c(-3,-2,-1,0,1,2),labels=c("1mm/yr","1cm/yr","10cm/yr","1m/yr","10m/yr","")) 
  training$sourcetmp <- 'publi' ; training$sourcetmp[which(training$publication_key == "070_cerema_2016")] <- 'cerema'
  ggplot(training, aes(log10(eroscorr2), fill=sourcetmp)) + geom_density(alpha=0.5, color="grey30") +theme_light() + scale_x_continuous(breaks=c(-3,-2,-1,0,1,2),labels=c("1mm/yr","1cm/yr","10cm/yr","1m/yr","10m/yr","")) + scale_fill_manual(values = summer5[c(1,4)])
  ggplot(training[which(training$erosion == 0.001),], aes(ID, erreur.harmo)) + geom_point() + theme_bw() + geom_point(aes(ID, eroscorr2), color="gold") + xlim(200,1600) 
  ggplot(training, aes(log10(eroscorr2))) + geom_density(fill=summer5[4], alpha=0.8) +theme_light() + scale_x_continuous(breaks=c(-3,-2,-1,0,1,2),labels=c("1mm/yr","1cm/yr","10cm/yr","1m/yr","10m/yr",""))  + geom_density(aes(log10(eroscorr)), color="gold")
  
  
  myfolds <- createFolds(training$erosion, k=10, list=F, returnTrain = F)
  testfold <- function(fold) training[which(myfolds == fold), ]
  trainfolds <- function(fold) training[which(myfolds != fold), ]
  rmse <-1; rsq <- 2
  
  
  #RF1, random forest variables par defaut
  rf1 <- randomForest(eroscorr2~. , trainfolds(1)[,-c(8,19)])
  varImpPlot(rf1)
  pred1 <- predict(rf1, testfold(1))
  summary(pred1 - testfold(1)$eroscorr2)
  plot(pred1, testfold(1)$eroscorr2)
  ggplot(data.frame(pred = pred1, true=testfold(1)$eroscorr2), aes(true, pred)) + geom_point() + theme_bw() + geom_abline(col="cyan4") + ggtitle("random forest, \ndefault parameters")
  ggsave(dirFIG %+% "/rf_pred_default_params.png", width=4,height=3.5)
  eval[1,] <- c("rf1", rf1$ntree, rf1$mtry, rf1$forest$nrnodes, postResample(pred1, testfold(1)$erosion)[rmse],postResample(pred1, testfold(1)$erosion)[rsq])
  
  