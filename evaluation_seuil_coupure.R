source("E:/these/BD_erosion/analyse/random_forest/SCRIPTS/environnement.R")
library(ggplot2)
library(caret)
library(randomForest)
library(pROC)
library(ranger)
library(corrplot)
library(RColorBrewer)
source(dirBD)
set.seed(4654)

###Preparation de la BD, suppression des variables inutiles
  table_gp <- table_gp[which(is.na(table_gp$erosion) == F ),]
  table_gp$hoek_brown_simpli <- as.factor(table_gp$hoek_brown_simpli)
  table_gp <- table_gp[which(table_gp$duree < 1000),]
  table_gp$technique_simple <- as.factor(table_gp$technique_simple)
 
  
####################################################################################################################
#  Influence du seuil de coupure
####################################################################################################################
  
  
  nfolds <- 10
  #myfolds <- createFolds(training$erosion, k=nfolds, list=F, returnTrain = F)
  testfold <- function(fold) training[which(myfolds == fold), ]
  trainfolds <- function(fold) training[which(myfolds != fold), ]
  
  ##Erosion class
  
  inflcut <- data.frame()
  mycuts <- seq(0.1,0.2,by=0.01)
  for (mycut in mycuts){
    table_varnet <- table_gp[,c("tidal_range","mean_swp","mean_swh","rainfalls","frozen_day","cliff_direction","q95_swp","q95_swh",
                            "duree_temp","nb_tempete","mean_temp","calcheightmoy", "calcheightmax", "calcheightmin",
                            "erosion","ampl_temp","hoek_brown_simpli", "wave_energy_flux" )]
 
    table_varnet$erosion <- log10(table_varnet$erosion)
    table_varnet$erosionclass <- cut(table_varnet$erosion, breaks = log10(c(0.0001,mycut,50)), labels = c("slow",  "high"))
    ggplot(table_varnet, aes(erosionclass, erosion)) + geom_boxplot() +theme_light() + scale_y_continuous(breaks=c(-3,-2,-1,0,1,2),labels=c("1mm/yr","1cm/yr","10cm/yr","1m/yr","10m/yr",""))
    table_varnet <- table_varnet[is.na(table_varnet$erosionclass)==F,-which(colnames(table_varnet)=="erosion")]
    
    inTrain <- createDataPartition(y=table_varnet$erosionclass, p=0.8, list=F)
    training <- table_varnet[inTrain, ] 
    testing <- table_varnet[-inTrain, ] 
    
    modRF <- randomForest(erosionclass~. , training,ntree=3000,mtry=2,nodesize=1)
    #assign(paste("modRF", mycut,sep=""), modRF)

    tmp <- data.frame(var=row.names(modRF$importance),MeanDecreaseGini=modRF$importance,cut=mycut)
    tmp <- tmp[with(tmp,order(-MeanDecreaseGini)),] ; tmp$rank <- seq(1:(length(table_varnet[1,])-1))
    inflcut <- rbind(inflcut,tmp)
  }
  
  
  ###Table du nombre de fois ou chaque variable est apparue en place n
  mapal <- colorRampPalette(c("blue","cyan","cadetblue","white","white","cadetblue2","aquamarine4","orange","red3"))
  a <- table(inflcut$var,inflcut$rank)
  corrplot(t(a), method = "square",tl.col = "black", tl.srt = 45, is.corr = F, col=mapal(length(mycuts)*2),
           cl.lim = c(0, length(mycuts)))
  
    #Pour les trois premier rang, variation selon le cut
  
    ggplot(inflcut[which(inflcut$rank < 4 & inflcut$rank > 1),], aes(cut, as.factor(rank), color=var)) + geom_point(size=3) + theme_light() +
      scale_color_brewer(palette ="Dark2")
  
  ##Coefficient de Gini
  ginisum <- data.frame(MeanDecreaseGini = as.numeric(tapply(inflcut$MeanDecreaseGini, inflcut$var, sum)/length(seq(0.05,0.2,by=0.01))), pred = row.names(tapply(inflcut$MeanDecreaseGini, inflcut$var, sum)) )
  ginisum$pred <- factor(ginisum$pred, levels = ginisum$pred[order(ginisum$MeanDecreaseGini)])
  ggplot(ginisum, aes(MeanDecreaseGini, pred)) + geom_point()
  

