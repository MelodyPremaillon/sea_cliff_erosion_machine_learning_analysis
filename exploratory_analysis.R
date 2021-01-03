source("F:/BD_erosion/analyse/analyseR/random_forest/SCRIPTS/environnement.R")

#Chargement des packages et de la DB       
library(ggplot2)
library(caret)
source(dirBD)

###Préparation de la BD, suppression des variables inutiles
  table_gp <- table_gp[which(is.na(table_gp$erosion) == F ),]
  table_gp <- table_gp[which(table_gp$continent == "Europe"),]
  table_gp$hoek_brown_simpli <- as.factor(table_gp$hoek_brown_simpli)
  table_gp$nb_tempete[which(table_gp$nb_tempete == 0)] <- NA; table_gp$duree_tempete[which(table_gp$duree_tempete == 0)] <- NA
  ###Suppresion de tous les truc de publi
    table_varnet <- table_gp[,c("publication_key","measure_key","measurement_technic","cliff_ref","sea","ocean","continent","country","cliff_length","fracturation","weathering","notch_presence","bedding","lithology_location","lithology_name","tidal_range","swell_period","swell_height","swell_direction_mod","swell_direction_mod_frequency","rainfalls","frozen_day","cliff_direction","koppen_geiger","q95_period","q95_energy","q95_height","duree_tempete","nb_tempete","mean_energy_tempete","hoek_brown_term","calcheightmoy","calcheightsd","calcheightmax","calcheightmin","cliff_key","height_id","duree","hauteur_moy","erosion","erreur.harmo","mean_temp","ampl_temp","technique_simple","hoek_brown_simpli","poids_mesure","wave_power","cliff_normal","swell_direction_modulo","wave_incidence","wave_energy_flux")]

###Split and predictor building  
  set.seed(42)
  inTrain <- createDataPartition(y=table_varnet$erosion, p=0.8, list=F)
  training <- table_varnet[inTrain, ] 
  testing <- table_varnet[-inTrain, ]
  # predicteurs <- c("tidal_range","rainfalls","frozen_day","koppen_geiger","duree_tempete","nb_tempete","hoek_brown_term","duree","hauteur_moy","erosion","erreur.harmo","mean_temp","ampl_temp","technique_simple","hoek_brown_simpli","wave_incidence","wave_energy_flux","calcheightmoy","calcheightsd","calcheightmax","calcheightmin")
  # predicteursnum <- c("cliff_length","tidal_range","swell_period","swell_height","swell_direction_mod","swell_direction_mod_frequency","rainfalls","frozen_day","cliff_direction","q95_period","q95_energy","q95_height","duree_tempete","nb_tempete","mean_energy_tempete","duree","hauteur_moy","erosion","erreur.harmo","mean_temp","ampl_temp","wave_power","cliff_normal","swell_direction_modulo","wave_incidence","wave_energy_flux","calcheightmoy","calcheightsd","calcheightmax","calcheightmin")

##Near zero variation
  nsv <- nearZeroVar(training, saveMetrics = T)
  colnames(testing[,which(nsv$nzv == TRUE)])

##Missing values
  nalength <- function(col){length(which(is.na(col)==T))}
  missv <- data.frame(name=colnames(training), missval=apply(training,2,nalength));  missv <- missv[order(-missv$missval),]
  missv[1:10,]

#Remplacement des valeurs d'erreur
  training$erreur.harmo.compl <- training$erreur.harmo
  funcsumarize <- function(x){as.vector(summary(x))}
  for (meth in unique(training$measurement_technic)){
    training$erreur.harmo.compl[which(is.na(training$erreur.harmo) == T & training$measurement_technic== meth)] <- mean(training$erreur.harmo[which(training$measurement_technic == meth)], na.rm=T)
  }


##Exploration de données
  predicteurs <- c("tidal_range","rainfalls","frozen_day","koppen_geiger","newduree_tempete","newnb_tempete","duree","erosion","mean_temp","ampl_temp","technique_simple","hoek_brown_simpli","wave_incidence","wave_energy_flux","calcheightmoy","calcheightsd","calcheightmax","calcheightmin")
  training <- training[,predicteurs]  
  
  featurePlot(x=training[,c("frozen_day","rainfalls","mean_temp","ampl_temp","eroslog")], y =training$hoek_brown_simpli, plot="pairs",auto.key = list(columns = 3))
  testPca <- preProcess(training[,predicteurs], method="pca")
  testPca  
  
  #Erosion distribution
  ggplot(training, aes(erosion)) + geom_density(fill="gold", color="darkgrey", alpha=0.7) + theme_bw()
    ggsave(dirFIG %+% "/erosiondensity.png", width=5,height=4)
  ggplot(training, aes(log10(erosion))) + geom_density(fill="gold", color="darkgrey",alpha=0.7) + theme_bw()
    ggsave(dirFIG %+% "/erosionlogdensity.png", width=5,height=4)
    
    #Publi/cerema
    training$sourcetmp <- 'publi' ; training$sourcetmp[which(training$publication_key == "070_cerema_2016")] <- 'cerema'
    ggplot(training, aes(log10(erosion), fill=sourcetmp)) + geom_density(alpha=0.5) + scale_fill_manual(values = summer5[c(1,3)]) + theme_bw() + scale_x_continuous(breaks=c(-3,-2,-1,0,1,2),labels=c("1mm/yr","1cm/yr","10cm/yr","1m/yr","10m/yr",""))
      ggsave(dirFIG %+% "/erosion_distrib_publi_cerema.png", width=5,height=4.5)
     
  ###
    
   ggplot(training, aes(calcheightmin,log10(erosion))) + geom_point(color="darkcyan") + theme_bw()
        
        
##Par identifiant
  training$ID <- seq(1:length(training$publication_key))
  ggplot(training, aes(ID, erosion)) + geom_point() + scale_y_log10()
  ggplot(training, aes(ID, erosion, color=hoek_brown_simpli)) + geom_point() + scale_y_log10()
  
    
##Choix des variables d'évenement extrème
  library(Factoshiny)
  PCAshiny(training[,25:30])
  res.PCA<-PCA(training[,25:30],quali.sup=NULL,quanti.sup=NULL,ind.sup=NULL,scale.unit=TRUE,graph=FALSE,ncp=5)
  plot.PCA(res.PCA,axes=c(1,2),choix='var',select=NULL,cex=1,cex.main=1,cex.axis=1,title='Graphe des variables (ACP)',unselect=0,col.quanti.sup='blue',col.var='#000000')

#Les porteurs des axes 1 et 2 sont la durée et le nombre (les plus anticorrélés)
  ggplot(training, aes (duree_tempete, nb_tempete)) + geom_point() + geom_smooth(method="lm", color="darkcyan") + theme_bw()
  lmtempete <- lm(nb_tempete~duree_tempete, data=training)  
  ggplot(data.frame(res=lmtempete$residuals, nbtemp = training$nb_tempete[which(training$nb_tempete >0)]), aes(nbtemp,res)) + geom_point()
## Spline
  tempBasis <- bs(training$nb_tempete, df=3)
  lm1 <- lm(duree_tempete~tempBasis, data=training)
  ggplot(training, aes(nb_tempete, duree_tempete)) + geom_point() + geom_point(data=data.frame(monlm=predict(lm1,newdata = training), nbtemp=training$nb_tempete), aes(nbtemp,monlm), col="orangered", pch=18) + theme_bw()
  lm2 <- lm(training$duree_tempete[which(training$nb_tempete != 0)]~ poly(training$nb_tempete[which(training$nb_tempete != 0)],degree=2))


