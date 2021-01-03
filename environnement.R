"%+%" <- function(x,y) paste(x,y,sep="")

dir <- "E:/thes/BD_erosion/analyse/random_forest/"
dirPROG <- dir %+% "/SCRIPTS"
dirFIG <- dir %+% "/FIG"
dirDATA <- dir %+% "/DATA"
dirRES <- dir %+% "/RESULT"

dirGIS <- "E:/these/GIS"
dircopernicus <- dirGIS %+% "/mnt_europe_copernicus"

dirBD <- "E:/these/BD_erosion/analyse/traitement_bd_propre/fonction_mise_en_forme_bd.R"

summer5 <- c("#C22326", "#F37338", "#FDB632", "#027878","#801638")

plotx <-' theme_light() + scale_x_continuous(breaks=c(-3,-2,-1,0,1,2),labels=c("1mm/yr","1cm/yr","10cm/yr","1m/yr","10m/yr",""))'
ploty <-' theme_light() + scale_y_continuous(breaks=c(-3,-2,-1,0,1,2),labels=c("1mm/yr","1cm/yr","10cm/yr","1m/yr","10m/yr",""))'
