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


###################################################################################################################
# Influence des cerema 0
####################################################################################################################

table_varnet <- table_gp[,c("tidal_range","mean_swp","mean_swh","rainfalls","frozen_day","cliff_direction","q95_swp","q95_swh",
                            "duree_temp","nb_tempete","mean_temp","calcheightmoy", "calcheightmax", "calcheightmin",
                             "erosion","ampl_temp","technique_simple","hoek_brown_simpli", "wave_energy_flux" )]

table_varnet <- table_varnet[which(table_varnet$erosion > 0.001),]
table_varnet$erosion <- log10(table_varnet$erosion)
table_varnet$erosionclass <- cut(table_varnet$erosion, breaks = log10(c(0.0001,0.1,50)), labels = c("slow",  "high"))
ggplot(table_varnet, aes(erosionclass, erosion)) + geom_boxplot() +theme_light() + scale_y_continuous(breaks=c(-3,-2,-1,0,1,2),labels=c("1mm/yr","1cm/yr","10cm/yr","1m/yr","10m/yr",""))
table_varnet <- table_varnet[is.na(table_varnet$erosionclass)==F,-which(colnames(table_varnet)=="erosion")]

inTrain <- createDataPartition(y=table_varnet$erosionclass, p=0.8, list=F)
training <- table_varnet[inTrain, ] 
testing <- table_varnet[-inTrain, ] 

modFitRF <- randomForest(erosionclass~. , training, ntree=500, mtry=1, nodesize = 1)
predRF <- predict(modFitRF,testing, type="prob")
myauc <- roc(testing$erosionclass, predRF[,1] )

varImpPlot(modFitRF)
plot.roc(myauc, xlim = c(1,0))
myauc$auc

myrocdf <- data.frame(sensitivity=myauc$sensitivities, specificity <- myauc$specificities)
ggplot(myrocdf, aes(1-specificity, sensitivity)) + geom_point()  


###################################################################################################################
# Avec cerema mais sans la durrée
####################################################################################################################
table_varnet <- table_gp[,c("tidal_range","mean_swp","mean_swh","rainfalls","frozen_day","cliff_direction","q95_swp","q95_swh",
                            "duree_temp","nb_tempete","mean_temp","calcheightmoy","calcheightsd", "calcheightmax", "calcheightmin",
                             "erosion","ampl_temp","technique_simple","hoek_brown_simpli", "wave_energy_flux" )]

table_varnet$erosion <- log10(table_varnet$erosion)
table_varnet$erosionclass <- cut(table_varnet$erosion, breaks = log10(c(0.0001,0.1,50)), labels = c("slow",  "high"))
ggplot(table_varnet, aes(erosionclass, erosion)) + geom_boxplot() +theme_light() + scale_y_continuous(breaks=c(-3,-2,-1,0,1,2),labels=c("1mm/yr","1cm/yr","10cm/yr","1m/yr","10m/yr",""))
table_varnet <- table_varnet[is.na(table_varnet$erosionclass)==F,-which(colnames(table_varnet)=="erosion")]

inTrain <- createDataPartition(y=table_varnet$erosionclass, p=0.8, list=F)
training <- table_varnet[inTrain, ] 
testing <- table_varnet[-inTrain, ] 

modFitRF <- randomForest(erosionclass~. , training, ntree=3000, mtry=2, nodesize = 1)
predRF <- predict(modFitRF,testing, type="prob")
myauc <- roc(testing$erosionclass, predRF[,1] )

varImpPlot(modFitRF)
plot.roc(myauc, xlim = c(1,0))
myauc$auc


table_gp$source <- "publi"; table_gp$source[which(table_gp$publication_key == "070_cerema_2016")] <- "cerema"
table_varnet <- table_gp[,c("tidal_range","mean_swp","mean_swh","rainfalls","frozen_day","cliff_direction","q95_swp","q95_swh",
                            "duree_temp","nb_tempete","mean_temp","calcheightmoy","calcheightsd", "calcheightmax", "calcheightmin",
                            "erosion","ampl_temp","technique_simple","hoek_brown_simpli", "wave_energy_flux","duree","source" )]
#table_varnet$source <- "publis"; table_varnet$source[which(table_varnet$erosion == 0.001)] <- "cerema"
table_varnet$erosion <- log10(table_varnet$erosion)
table_varnet$erosionclass <- cut(table_varnet$erosion, breaks = log10(c(0.0001,0.1,50)), labels = c("slow",  "high"))
ggplot(table_varnet, aes(erosionclass, erosion)) + geom_boxplot() +theme_light() + scale_y_continuous(breaks=c(-3,-2,-1,0,1,2),labels=c("1mm/yr","1cm/yr","10cm/yr","1m/yr","10m/yr",""))
table_varnet <- table_varnet[is.na(table_varnet$erosionclass)==F,]

# inTrain <- createDataPartition(y=table_varnet$erosionclass, p=0.8, list=F)
# training <- table_varnet[inTrain, ] 
# testing <- table_varnet[-inTrain, ] 


ggplot(table_varnet, aes(erosionclass,duree)) + geom_boxplot() 
ggplot(table_varnet, aes(duree,erosion, color=source)) + geom_point() + facet_grid(.~erosionclass)
