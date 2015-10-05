#===== Mapping data on us county map=====

#loading packages
library("rgdal")
library("RColorBrewer")
library("sp")
library("GISTools")
library("classInt")
library("maptools")
library("SmarterPoland")
library(sqldf)
library(fields)
library(colorRamps)
library(plotrix)
library(TeachingDemos)
library(wordcloud)


# loading data
US.counties <- readOGR(dsn="C:/Users/Gebruiker/Google Drive/SIM/Thesis/R/mthesis3/geo/UScounties2",layer="cb_2014_us_county_5m")
US.states <-  readOGR(dsn="C:/Users/Gebruiker/Google Drive/SIM/Thesis/R/mthesis3/geo/USstates",layer="cb_2014_us_state_5m")

#leave out AK, HI, and PR (state FIPS: 02, 15, and 72)
CoToRemove <- US.counties@data$STATEFP %in% c("02","15","72")
USCO <- US.counties[!CoToRemove,]
StToRemove <- US.states@data$STATEFP %in% c("02","15","72")
USST <- US.states[!StToRemove,]

USCO@data$FIPS <- paste0(USCO@data$STATEFP,USCO@data$COUNTYFP)

#EU_NUTS <- spTransform(EU_NUTS, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))


#load data


plotUSTL3 <- function(df, mainTitle, nlabeled = 5){
names(df)<- c('FIPS','values', 'label')
  
USCO@data = data.frame(USCO@data, df[
  match(USCO@data[, "FIPS"],df[, "FIPS"]),])
USCO <- USCO[!is.na(USCO@data$values),]

# labels
label_ids <- head(df[with(df, order(-values)),],nlabeled)[,1]
label_names <- head(df[with(df, order(-values)),],nlabeled)[,3]
label_names <- sub(',[^,]*,[^,]+$', '', label_names)
label_coords <- matrix(NA, nrow = nlabeled, ncol = 2)
for(i in 1:length(label_ids)){
  label_coords[i,] <- coordinates(USCO[USCO@data[,'GEOID'] == label_ids[i],])
}

# plot
dev.new(width=10,height=2)
plot <- plot(USCO, col = rgb(colorRamp(
  matlab.like2(10))
  ((USCO@data$values-min(USCO@data$values))/
    (max(USCO@data$values)-min(USCO@data$values)))/255), 
  axes = FALSE, border = NA, main=mainTitle )    

plot <- plot(USST, border = "#707070", add = TRUE)

image.plot(add=TRUE,
           zlim=c(min(USCO@data$values),max(USCO@data$values)),
           col=rgb(colorRamp(
             matlab.like2(10))(seq(0,1,length.out=50))/255),
           legend.only=TRUE,
           smallplot=c(.02,.06,.15,.85))
textplot(label_coords[,1], label_coords[,2], label_names, cex = 0.8, new = FALSE)
}




