## --- Create technology map ---
# 
# Some more explanation

#configure global parameters
setparam <- function(){
  project <<- 'smart-bridge-91709' # Google bigquery poject name
  sfields <-  read.table("data/sfields.csv", header = TRUE, quote='"', skip=0, sep="," ) # Load ifris field names
  sfields <<- unlist(sfields, use.names = FALSE)
  dataset <<- 'thesis'

}

#install required packages
install <- function(){
  install.packages("bigrquery")    #Required for fetching data from Google BigQuery
# options("httr_oob_default" = TRUE) after first install of bigrquery run this to set correct authentication protocol.
   install.packages("Matrix")       #Required for generating matrix
#  install.packages("beepr")        #Can be used to notify user when a script completes
  install.packages("foreach")   #can be used to run multiple loops simultaneously
  install.packages("doMC")
  install.packages("qdapTools")
  }


#load required packages
loader <- function(){
  library(bigrquery)    #Required for fetching data from Google BigQuery
  library(Matrix)       #Required for generating matrix
#  library(beepr)        #Can be used to notify user when a script completes
  library(foreach)
  library(doMC)
  registerDoMC(3)
}


# Fetching data by year
getDataYear <- function(startYear, endYear, EPPCT = FALSE){
  print('fetching data from Google BigQuery...')
if(EPPCT==TRUE){sql <- paste("SELECT   t1.appln_id AS appln_id,   t3.prior_earliest_year AS year,   t1.ifris_class AS ifris_class FROM   [thesis.appln_ipc_ifris] t1 INNER JOIN EACH [thesis.priorities] t2 ON   t1.appln_id = t2.appln_id INNER JOIN EACH [thesis.tls201_appln] t3 ON   t1.appln_id = t3.appln_id WHERE   (t3.appln_kind like 'W%' OR t3.appln_auth = 'EP')   AND   t3.prior_earliest_year BETWEEN ", startYear, " AND ", endYear, " GROUP EACH BY   appln_id,   year,   ifris_class")}
else{  sql <- paste("SELECT t1.appln_id as appln_id, t3.prior_earliest_year as year, t1.ifris_class as ifris_class FROM [thesis.appln_ipc_ifris] t1 INNER JOIN EACH [thesis.priorities] t2 ON t1.appln_id = t2.appln_id INNER JOIN EACH [thesis.tls201_appln] t3 ON t1.appln_id = t3.appln_id WHERE t3.prior_earliest_year BETWEEN ", startYear, " AND ", endYear, "GROUP EACH BY appln_id, year, ifris_class ;", sep="")}
query_exec(sql, project = project, destination_table = paste("temp.fetch_",startYear,"_",endYear, sep=""), page_size = 100000, max_pages = Inf, warn = TRUE)

}

# Fetching data by yearmonth
getDataYearMonth <- function(startYearMonth, endYearMonth, class = 'ifris'){
  print('fetching data from Google BigQuery...')
  
  sql <- paste("SELECT t1.appln_id, t3.prior_earliest_year_month, t1.", class, "_class FROM [thesis.appln_ipc_ifris] t1 INNER JOIN EACH [thesis.priorities] t2 ON t1.appln_id = t2.appln_id INNER JOIN EACH [thesis.tls201_appln] t3 ON t1.appln_id = t3.appln_id WHERE t3.prior_earliest_year_month BETWEEN ", startYearMonth, " AND ", endYearMonth, " ;", sep="")
  tmp <-  query_exec(sql, project = project, max_pages = Inf, destination_table = paste("temp.fetch_", startYearMonth, "_", endYearMonth, sep=''))
  tmp
}

#
getLocalDataYear <- function(startYear, endYear){
  print('subsetting data')
  data <- subset(alldata, alldata$year >= startYear & alldata$year <= endYear)
  data
}

# create technology map 

techMap <- function(data, sfields){
  print('creating co-occurrence matrix...')
      #filter duplicates
  subfieldsNames <- unique(sfields)
  patents <- unique(data[,1])
  
  pat.fac <- factor(data[,1]) #create categories
  tf.fac <- factor(data[,3]) 
  tf.occ <- table(tf.fac)
  
  #The three lines below make sure the relevant subfields come first. It is probably not the most elegant solution, but it works.
  subfields.cur<-(attributes(tf.fac)$levels) 
  subfields.all<- c(subfields.cur,as.character(subfieldsNames))
  subfields.all<-unique(subfields.all)
  
  #create sparse matrix
  s <- sparseMatrix(
    as.numeric(pat.fac), 
    as.numeric(tf.fac),
    dims = c(length(patents),length(subfields.all)),
    dimnames = list(
      as.character(patents), 
      as.character(subfields.all)),
    x = 1)
  
  com <- t(s) %*% s #create co-occurence matrix
  com <- com[order(rownames(com)),order(colnames(com))]
  com <- as.matrix(com)
  diag(com) <- 0
  list(com, tf.occ)
}

distance.index <- function(com, tf.occ, fun="jaccard"){
  #calculate techn-distance from index of each combination of classifications (twice)
  mphi <- com #creating copy for indexing
  print('calculating technological distances...')
  for (i in colnames(com)){
    for (j in rownames(com)){
      if(fun=="jaccard")   if(com[i,j] > 0)mphi[i,j] <- jaccard(com,tf.occ,i,j)
      if(fun=="salton")   if(com[i,j] > 0)mphi[i,j] <- salton(com,tf.occ,i,j)
      }
  }
  
  mphi
}

# Jaccard index
jaccard <- function(matrix,tf.occ,i,j){
  occ.ij <- matrix[i,j]
  occ.i <-  as.numeric(tf.occ[i])
  occ.j <-  as.numeric(tf.occ[j])
  phi <- occ.ij/(occ.i+occ.j-occ.ij)
  phi
}

# Salton's cosine
salton <- function(matrix,tf.occ,i,j){
  occ.ij <- matrix[i,j]
  occ.i <-  as.numeric(tf.occ[i])
  occ.j <-  as.numeric(tf.occ[j])
  phi <- occ.ij/sqrt(occ.i*occ.j)
  phi
}



# clear temp tables from BigQuery
cleartables <- function(){
  temptables <- list_tables(project, "temp")
  for (i in 1:length(temptables)){
    delete_table(project, 'temp', temptables[i])
  }
}






