#====== 0. Creating Technology Maps ======
# Loading dependencies 
source('~/mthesis2/technologymap.R')

#===== 0.1 Fetching data =====
data <- getDataYearMonth(198001,201212, class='ipc')  

for(y in c( 2000, 2003, 2005, 2006, 2008, 2009, 2011, 2012)){
  print(paste('Processing year',y))
  
  data <- getLocalDataYear(y-9,y)
  map <- techMap(data, sfields)  
  saveRDS(map, file=paste("maps/10year/techmap_", y, ".RDa", sep=""))
  
}