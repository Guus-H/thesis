# Main function
coherenceFun <- function(y,fun, publish=FALSE){
  gc() # clear garbage

  print(paste('Processing ', y))
  map <- readRDS(file = paste('maps/', fun, '/10year/techmap_', y, '.RDa', sep = ""))
  data <- fetchConcat(y)
  result <- as.data.frame(coherence3(map,data))
  colnames(result) <- c("appln_id", "rad_mean") 
  result$appln_id <- as.integer(result$appln_id) #set correct data type for bigquery
   print('results are in..')
  if(publish==TRUE){ pushConcat(result, "appln_tech_coherence")}
  else {result}
}

## supporting functions

#get data from bigquery
fetchConcat <- function(year){
  sql <- paste("SELECT t1.appln_id AS appln_id, GROUP_CONCAT(t1.ifris_class) AS classes
               FROM (SELECT  appln_id,  ifris_class  FROM  thesis.appln_ipc_ifris GROUP EACH BY appln_id, ifris_class) t1 
               INNER JOIN EACH thesis.tls201_appln t2
               ON t1.appln_id = t2.appln_id
               WHERE t2.prior_earliest_year = ", year, "  GROUP EACH BY appln_id ")
  
  tmp <-  query_exec(sql, project = project, destination_table = paste("temp.", sample(0:999,1), "fetch_concat", year, sep=""), page_size = 100000, max_pages = Inf, warn = TRUE)
  tmp
}


#write back data to bigquery
pushConcat <- function(values, table){
  dataset <- 'thesis'
  insert_upload_job(project, dataset, table, values, billing = project) 
}




# coherence of data frame, returns list, prints processbar
coherence3 <- function(map,data){
  print(paste(nrow(data),'rows to process..'))
  pblapply(1:nrow(data), coh, data = data, map = map) 
}

# coherence of application (row 'r' of dataframe 'data')
coh <- function(r,data, map){
  loose <- unlist(strsplit(as.character(data[r,2]),','))
  md <- 0
  if(length(loose) > 1){ 
    comb <- combn(loose,2)
  md <- mean(sapply(col(comb)[1,], distn, comb = comb, map = map))
  }
  
c(as.integer(data[r,1]), md)
  
}

distn <- function(i, comb, map){
  1-(map[comb[1,i],comb[2,i]])# 1-(coherence(ij))
}

