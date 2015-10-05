#===== Backwards Divesity Measures =====
#sourcing dependencies
source('functions/load packages.R') #Packages
source('functions/backwards_diversity.R')
source('functions/techdiv-techrel2.R')
require(devtools)
source_gist(4676064)


#=====Acquiring data=====
backCitOccur <- readRDS('data/RPbackCitOccur.Rds') # obtained through query 10
applns <- unique(backCitOccur$appln_id) # or any subset of 
applns <- applns[sample(length(applns), 100000)]
map <- readRDS(file = 'maps/jaccard/allyear/techmap_2012.RDa')
map <- scaleMap(map)

#=====Calculating diversities=====
system.time(tmp <- pblapply(applns, techDivBackCit, data = backCitOccur, map) )
tmp <- tmp[! sapply(tmp, is.null)] # Remove the empty list entries, resulting from groups with less than two classes.
BackCitDiv <- as.data.frame(tmp) #convert list of vectors to dataframe
names(BackCitDiv) <- c("appln_id", "back_cit_div") #setting correct column-names
##BackCitDiv$back_cit_div <-  as.numeric(levels(BackCitDiv$back_cit_div))[BackCitDiv$back_cit_div]
