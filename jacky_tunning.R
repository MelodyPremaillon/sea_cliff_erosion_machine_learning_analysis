source("F:/BD_erosion/analyse/analyseR/random_forest/SCRIPTS/environnement.R")
library(ggplot2)
library(caret)
library(randomForest)
library(pROC)
library(ranger)
library(corrplot)

source(dirBD)
set.seed(4654)

###Préparation de la BD, suppression des variables inutiles
table_gp <- table_gp[which(is.na(table_gp$erosion) == F ),]
table_gp$hoek_brown_simpli <- as.factor(table_gp$hoek_brown_simpli)
table_gp <- table_gp[which(table_gp$duree < 1000),]
table_gp$technique_simple <- as.factor(table_gp$technique_simple)
table_varnet <- table_gp[,c("tidal_range","mean_swp","mean_swh","rainfalls","frozen_day","cliff_direction","q95_swp","q95_swh",
                            "duree_temp","nb_tempete","mean_temp","calcheightmoy", "calcheightmax", "calcheightmin",
                             "erosion","ampl_temp","technique_simple","hoek_brown_simpli", "wave_energy_flux" )]
table_varnet$erosion <- log10(table_varnet$erosion)
table_varnet$erosionclass <- cut(table_varnet$erosion, breaks = log10(c(0.0001,0.1,50)), labels = c("lente",  "rapide"))
table_varnet <- table_varnet[is.na(table_varnet$erosionclass)==F,]
ggplot(table_varnet, aes(erosionclass, erosion)) + geom_boxplot() +theme_light() + scale_y_continuous(breaks=c(-3,-2,-1,0,1,2),labels=c("1mm/yr","1cm/yr","10cm/yr","1m/yr","10m/yr","")) + xlab("")
table_varnet <- table_varnet[is.na(table_varnet$erosionclass)==F,-which(colnames(table_varnet)=="erosion")]

inTrain <- createDataPartition(y=table_varnet$erosionclass, p=0.8, list=F)
training <- table_varnet[inTrain, ] 
testing <- table_varnet[-inTrain, ] 

nfolds <- 10
myfolds <- createFolds(training$erosion, k=nfolds, list=F, returnTrain = F)
testfold <- function(fold) training[which(myfolds == fold), ]
trainfolds <- function(fold) training[which(myfolds != fold), ]

###Cross validation to improove parameters
ntree <- c(100, 200, 300, 500, 1000, 2000, 3000, 5000) #seq(1000,3100, by=100)
mtry <- seq(1,4)
nodesize <-c(1,3,5,10)
folds <- seq(1,nfolds)
treetry <- (length(ntree)*length(mtry)*length(nodesize))
#mes.params <- data.frame(ntree= rep(ntree, length(folds), each=length(mtry)) , mtry = rep(mtry, length(ntree)*length(folds)), fold=rep(folds,each=length(ntree)*length(mtry)), auc=0)
mes.params <- read.table(dirRES %+% "/paramset_vsansduree_sssd.txt", sep="\t", header=T )

mes.params <- expand.grid(ntree, mtry, nodesize, folds)
colnames(mes.params) <- c("ntree", "mtry","nodesize","folds")

deb <- proc.time()
for (i in 1:10){
  for (param in 1:treetry){
    modFitRF <- ranger(erosionclass~. , trainfolds(i), num.trees = mes.params$ntree[param], mtry=mes.params$mtry[param], min.node.size = mes.params$nodesize[param], importance = "impurity", classification = T, probability = T)
    predRF <- predict(modFitRF, testfold(i))
    mes.params$auc[param + (i-1)*treetry] <- as.numeric(roc(testfold(i)$erosionclass, predRF[[1]][,1] )$auc)
    mes.params$accuracy <- 2
  }
}
fin <- proc.time()
fin-deb

mes.params$paramset <- rep(seq(1:treetry),length(folds))
write.table(mes.params, dirRES %+% "/paramset_vsansduree_sssd.txt", sep="\t", row.names = F)

ggplot(mes.params, aes(ntree, auc, color=as.factor(mtry) )) + geom_point() + facet_grid(folds~. )

meanvalues <- data.frame(auc=tapply(mes.params$auc, mes.params$paramset, mean), rep(seq(1:treetry)), mes.params[which(mes.params$folds==1), c(1,2,3)] )

ggplot(meanvalues, aes(ntree, auc, color=as.factor(mtry))) + geom_point(size=2) + theme_light() + 
  scale_color_manual(values = summer5, name="Variables \nselectionnées \nà chaque \nnoeud") + facet_grid(.~nodesize) + geom_line(aes(ntree, auc, color=as.factor(mtry))) +
  scale_y_continuous(breaks = seq(0.939,0.945,by=0.001)) + xlab("Nombre d'arbres") + ylab("Aire sous la courbe") #???+ ggtitle("Taille minimale des feuilles terminales")
ggsave(dirFIG %+% "/auc_vs_ntreemtry.png", height = 5, width = 10)
ggsave("F:/manuscrit/chap3/figures/optimisation_params.png", height = 6, width = 11)



ggplot(meanvalues[which(meanvalues$ntree == 3000),], aes(nodesize, auc, color=as.factor(mtry))) + geom_point(size=2) + theme_light() + 
  scale_color_manual(values = summer5) + geom_line(aes(ntree, auc, color=as.factor(mtry))) 


ggplot(meanvalues[which(meanvalues$mtry == 3), ], aes(ntree, auc, color=as.factor(nodesize))) + geom_point(size=2) + theme_light() + 
  scale_color_manual(values = summer5, name="node size") + geom_line(aes(ntree, auc, color=as.factor(nodesize))) 


###Quel est la meilleure combinaison?
### Tracé de la courbe ROC
modFitRF <- randomForest(erosionclass~. , training, ntree=2000, mtry=2, nodesize = 1)
predRF <- predict(modFitRF,testing, type="prob")
myauc <- roc(testing$erosionclass, predRF[,1] )

modFitRF <- ranger(erosionclass~. , training, num.trees = 300, mtry=1, min.node.size =  1, importance = "impurity")
tmp0 <- data.frame(permutation=as.numeric(importance(modFitRF)), pred= names(importance(modFitRF)), type="impurity")
tmp0 <- tmp0[with(tmp0,order(-permutation)),] ; tmp0$rank <- seq(1:18)

modFitRF <- ranger(erosionclass~. , training, num.trees = 3000, mtry=2, min.node.size =  1, importance = "permutation")
tmp <- data.frame(permutation=as.numeric(importance(modFitRF)), pred= names(importance(modFitRF)), type="permutaion")
tmp <- tmp[with(tmp,order(-permutation)),] ; tmp$rank <- seq(1:18)

tmp$pred <- factor(tmp$pred, levels = tmp$pred[order(tmp$permutation)]); tmp$rank <- seq(1:18)
ggplot(tmp, aes(permutation, pred)) + geom_point()

ginisum$pred <- factor(ginisum$pred, levels = ginisum$pred[order(ginisum$MeanDecreaseGini)])

impopermut <- rbind(tmp0, tmp)
ggplot(impopermut, aes(rank, pred, color=type)) + geom_point()


varImpPlot(modFitRF)
plot.roc(myauc, xlim = c(1,0),print.auc = T,auc.polygon = T,auc.polygon.border=NA)#,print.thres = "best"
myauc$auc


  ##Matrice de confusion
  
  prediction <- data.frame(pred=predict(modFitRF,testing), realite = testing$erosionclass)
  matconf <-  table(prediction$realite,prediction$pred)
  
  mapal <- colorRampPalette(c("white","#FFFF6B","#CC5500"))
  matconprct <- matconf; matconprct[,1] <- round(matconprct[,1]/262*100,1) ;  matconprct[,2] <- round(matconprct[,2]/81*100,1)
  corrplot(matconprct,method = "square",tl.col = "black", is.corr = F, cl.lim = c(0,100),p.mat = matconf,insig = "p-value",
           col=mapal(20), tl.srt = 0)
  

  sensitivity <- round(matconf[1,1]/(matconf[1,1]+matconf[2,1]),2)
  specificity <- round(matconf[2,2]/(matconf[1,2]+matconf[2,2]),2)
  accuracy <- round((matconf[1,1]+matconf[2,2])/(matconf[1,1]+matconf[2,1]+matconf[1,2]+matconf[2,2]) ,2)







