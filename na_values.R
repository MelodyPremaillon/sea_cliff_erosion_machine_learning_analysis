source("F:/BD_erosion/analyse/random_forest/SCRIPTS/environnement.R")

#Chargement des packages et de la DB       
library(ggplot2)
library(caret)
source(dirBD)
library(randomForest)

###Préparation de la BD, suppression des variables inutiles
  table_gp <- table_gp[which(is.na(table_gp$erosion) == F ),]
  #table_gp <- table_gp[which(table_gp$continent == "Europe"),]
  table_gp$hoek_brown_simpli <- as.factor(table_gp$hoek_brown_simpli)
  
  #table_gp <- table_gp[which(table_gp$erosion > 0.001),]
  
  #table_gp$nb_tempete[which(table_gp$nb_tempete == 0)] <- NA; table_gp$duree_tempete[which(table_gp$duree_tempete == 0)] <- NA
  table_gp <- table_gp[which(table_gp$duree < 1000),]
  table_gp <- table_gp[which(table_gp$mean_swp > 0),]
  ###Suppresion de tous les truc de publi
  table_varnet <- table_gp[,c("tidal_range","mean_swp","mean_swh","rainfalls","frozen_day","cliff_direction","q95_swp",
                              "q95_swh","duree_temp","nb_tempete","mean_temp","calcheightmoy",
                              "calcheightmax","calcheightmin","erosion","ampl_temp","technique_simple","hoek_brown_simpli", "wave_energy_flux"
                              )] #,,"erreur.harmo""cliff_length",
  
  #table_gp$technique_simple <- as.factor(table_gp$technique_simple)
#  "swell_direction_mod","swell_direction_mod_frequency","koppen_geiger","q95_energy","cliff_normal","swell_direction_modulo","wave_incidence","hauteur_moy"

  
  
  # predicteurs <- c("tidal_range","rainfalls","frozen_day","koppen_geiger","duree_tempete","nb_tempete","hoek_brown_term","duree","erosion","erreur.harmo","mean_temp","ampl_temp","technique_simple","hoek_brown_simpli","wave_incidence","wave_energy_flux","calcheightmoy","calcheightsd","calcheightmax","calcheightmin")
  # predicteursnum <- c("cliff_length","tidal_range","swell_period","swell_height","swell_direction_mod","swell_direction_mod_frequency","rainfalls","frozen_day","cliff_direction","q95_period","q95_energy","q95_height","duree_tempete","nb_tempete","mean_energy_tempete","duree","hauteur_moy","erosion","erreur.harmo","mean_temp","ampl_temp","wave_power","cliff_normal","swell_direction_modulo","wave_incidence","wave_energy_flux","calcheightmoy","calcheightsd","calcheightmax","calcheightmin")
  
  
#######################################################################################  
###Split and predictor building  
#######################################################################################
  
  set.seed(42)
  inTrain <- createDataPartition(y=table_varnet$erosion, p=0.8, list=F)
  training <- table_varnet[inTrain, ] 
  testing <- table_varnet[-inTrain, ]  

  
#######################################################################################    
##Missing values
#######################################################################################  
  # nalength <- function(col){length(which(is.na(col)==T))} #fonction pour calculer nombre de valeurs NA
  #   missv <- data.frame(name=colnames(training), missval=apply(training,2,nalength));  missv <- missv[order(-missv$missval),]
  #   missv[1:7,]
  # 
  # #Valeurs d'erreur : Traité sur tout les data set car je vais les rentrer à la main
  # #Pour méthode dont les valeurs d'erreu existent, input de la moyenne
  #   training$erreur.harmo.compl <- training$erreur.harmo
  #   funcsumarize <- function(x){as.vector(summary(x))}
  #    for (meth in unique(training$measurement_technic)){
  #      training$erreur.harmo.compl[which(is.na(training$erreur.harmo) == T & training$measurement_technic== meth)] <- mean(training$erreur.harmo[which(training$measurement_technic == meth)], na.rm=T)
  #    }
  #   missv <- data.frame(name=colnames(training), missval=apply(training,2,nalength));  missv <- missv[order(-missv$missval),]
  #   missv[1:7,]
  #   training <- training[,-which(colnames(training)=="erreur.harmo")]
  # 
    # #Là ou aucune erreur n'existe, je les rentre à la main
    # unique(training$technique_simple[which(is.na(training$erreur.harmo.compl) ==T )])
    # 
    
    
  
  #Duree tempete et nombre tempetes
    # #prédicteurs environnementaux pour calculer les données manquantes
    # predTempete <- c("tidal_range","swell_period","swell_height","swell_direction_mod","swell_direction_mod_frequency","rainfalls","frozen_day","q95_period","q95_energy","q95_height","duree_tempete","nb_tempete","mean_energy_tempete","mean_temp","ampl_temp","wave_power") 
    # # Utilisation methode knn dans la fonction preProcess
    # preObj<- preProcess(training[,predTempete], method="knnImpute")
    # # Création nouveaux champs avec les valeurs calculées 
    # training$newduree_tempete <- predict(preObj, training[,predTempete])$duree_tempete*sd(training$duree_tempete, na.rm=T) + mean(training$duree_tempete,na.rm = T)
    # training$newnb_tempete <- (predict(preObj, training[,predTempete])$nb_tempete*sd(training$nb_tempete, na.rm=T) + mean(training$nb_tempete,na.rm = T))
    # # Vérification qu'il n'y a plus de NA
    # missv <- data.frame(name=colnames(training), missval=apply(training,2,nalength));  missv <- missv[order(-missv$missval),]
    # missv[1:7,]
  
   #training$koppen_geiger <- as.factor(training$koppen_geiger)
  training$technique_simple <- as.factor(training$technique_simple)
  testing$technique_simple <- as.factor(testing$technique_simple)
  
rm(missv, table_gp, table_varnet)  
  
    