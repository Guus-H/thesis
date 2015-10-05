# Main function
radicalFun <- function(y,fun,period,class = ""){
  gc() # clear garbage
  
  
  print(paste('Processing ', y))
  map <- readRDS(file = paste(class,'maps/', fun, '/', period, 'year/techmap_', y, '.RDa', sep = ""))
  data <- fetchConcat(y,class)
  result <- as.data.frame(raostirling(map,data))
  colnames(result) <- c("appln_id", "tech_rad") 
  result$appln_id <- as.integer(result$appln_id) #set correct data type for bigquery
  print('results are in..')
  if(publish==TRUE){ pushConcat(result, paste('appln_tech_rad', fun, period, 'year', class, sep="_"))}
    else {result}
}

## supporting functions

#get data from bigquery
fetchConcat <- function(year,class){
if(class != "ipc"){ 
 sql <- paste("SELECT t1.appln_id AS appln_id, GROUP_CONCAT(t1.ifris_class) AS classes
               FROM (SELECT  appln_id,  ifris_class  FROM  thesis.appln_ipc_ifris GROUP EACH BY appln_id, ifris_class) t1 
               INNER JOIN EACH thesis.tls201_appln t2
               ON t1.appln_id = t2.appln_id
               WHERE t2.prior_earliest_year = ", year, "  GROUP EACH BY appln_id ")
}else{
  sql <- paste("SELECT t1.appln_id AS appln_id, GROUP_CONCAT(t1.ipc_class) AS classes
               FROM (SELECT  t1.appln_id as appln_id, t2.ipc_subclass_symbol as ipc_class  FROM  thesis.appln_ipc_ifris t1 inner join each thesis.tls209_appln_ipc t2 on t1.appln_id=t2.appln_id GROUP EACH BY appln_id, ipc_class) t1 
               INNER JOIN EACH thesis.tls201_appln t2
               ON t1.appln_id = t2.appln_id
               WHERE t2.prior_earliest_year = ", year, "  GROUP EACH BY appln_id ")
  
}
  
  tmp <-  query_exec(sql, project = project, destination_table = paste("temp.", sample(0:999,1), "fetch_concat", year, sep=""), page_size = 100000, max_pages = Inf, warn = TRUE)
  tmp
}


#write back data to bigquery
pushConcat <- function(values, table){
  dataset <- 'thesis'
  insert_upload_job(project, dataset, table, values, billing = project) 
}




# technological radicality (or rao-stirling diveristy) of data frame, returns list, prints processbar
raostirling <- function(map,data){
  df <- data.frame(appln_id = as.integer(0), tech_coherence = 0)
  print(paste(nrow(data),'rows to process..'))
  pblapply(1:nrow(data), trad, data = data, map = map) 
}

# tech radicality of application (row 'r' of dataframe 'data')
trad <- function(r,data, map){
  loose <- unlist(strsplit(as.character(data[r,2]),','))
  rs <- 0
  if(length(loose) > 1){ 
    comb <- combn(loose,2)
 rs <- sum(sapply(col(comb)[1,], distn, comb = comb, map = map, nloose = length(loose) ))
  }
  
c(as.integer(data[r,1]), rs)
  
}

distn <- function(i, comb, map, nloose){
  (1/nloose)^2*(1-(map[comb[1,i],comb[2,i]])) ## p_i*p_j*d_ij // p_i == p_j, d_ij == 1-(coherence(ij))
}

