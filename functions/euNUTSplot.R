#===== Mapping data from eurostat using R=====

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

# getting data
# create a new empty object called 'temp' in which to store a zip file
# containing boundary data
# temp <- tempfile(fileext = ".zip")
# now download the zip file from its location on the Eurostat website and
# put it into the temp object
# download.file("http://ec.europa.eu/eurostat/cache/GISCO/geodatafiles/NUTS_2010_60M_SH.zip", temp)
# now unzip the boundary data
# unzip(temp)

# loading data
EU_NUTS <- readOGR(dsn = "./NUTS_2010_60M_SH/data", layer = "NUTS_RG_60M_2010")
EU_NUTS <- spTransform(EU_NUTS, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
CountryBorder <- EU_NUTS[EU_NUTS@data$STAT_LEVL_ == 0, ]

#Remove overseas areas of France
ToRemove <- EU_NUTS@data$STAT_LEVL!=2 | grepl('FR9',EU_NUTS@data$NUTS_ID)
EUN <- EU_NUTS[!ToRemove,]

#load data
#upRegStats <- readRDS('data/upRegStats.Rds')

plotNUTS2 <- function(df, mainTitle){
names(df)<- c('NUTS_code','values')
  
EUN@data = data.frame(EUN@data[,1:4], df[
  match(EUN@data[, "NUTS_ID"],df[, "NUTS_code"]),])
EUN <- EUN[!is.na(EUN@data$values),]



# plot

plot <- plot(EUN, col = rgb(colorRamp(
  matlab.like2(10))
  ((EUN@data$values-min(EUN@data$values))/
    (max(EUN@data$values)-min(EUN@data$values)))/255), 
  axes = FALSE, border = NA, main=mainTitle )    

plot <- plot(CountryBorder, border = "#707070", add = TRUE)

image.plot(add=TRUE,
           zlim=c(min(EUN@data$values),max(EUN@data$values)),
           col=rgb(colorRamp(
             matlab.like2(10))(seq(0,1,length.out=50))/255),
           legend.only=TRUE,
           smallplot=c(.02,.06,.15,.85))

}




