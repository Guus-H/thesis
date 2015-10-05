#install.packages("data.table")
#library(data.table)



#calculate technological diversity of set of patents (i.e. all patents assigned to given region)

techDivReg <- function(region){
data <- data.table(getRegData(region))
map <- readRDS(file = 'maps/aggregate/techmap_2012.RDa')
regFields <- data[, (count = .N), by = ifris_class ]
combFields <- combn(regFields[[1]],2)
fieldsCount <- matrix(data = regFields[[2]], dimnames =  list(regFields[[1]],"Count"))
sum(sapply(1:ncol(combFields), RaoSt, map = map, combFields = combFields, fieldsCount = fieldsCount))
  }

# Fetch patent
getRegData <- function(region){
  sql <- paste('SELECT t1.Reg_code as reg_code, t1.Appln_id as appln_id, t2.ifris_class as ifris_class FROM thesis.regpat_all_inv t1
INNER JOIN EACH
  (SELECT  appln_id,  ifris_class  FROM  thesis.appln_ipc_ifris GROUP EACH BY appln_id, ifris_class) t2
  on t1.Appln_id = t2.appln_id
  WHERE reg_code = "', region, '"', sep="")
  data <- query_exec(sql, project = project, destination_table =  paste("temp.", sample(0:999,1), "fetch_reg_class", region, sep=""), max_pages = Inf)
  data 
}

# Rao Stirling diversity
RaoSt <- function(i, map, combFields, fieldsCount){  
  phi <- map[combFields[1,i],combFields[2,i]]
  p_i <- fieldsCount[combFields[1,i],1]/sum(fieldsCount)
  p_j <- fieldsCount[combFields[2,i],1]/sum(fieldsCount)
  (1-phi)*p_i*p_j 
  
}
  
# Loop to calculate diversity for every region in 'regions'
regLoop <- function(i,regions, istart){
  print(paste('Ronde',i))
  region <- regions[i,1]
  div <- techDivReg(region)
  tuple <- (cbind(region,div))
  if(i==istart){ divresults <<- as.data.frame(tuple)} 
  else{ divresults <<- rbind(divresults,tuple)} 
  saveRDS(divresults, file = "regdiv.RDa")
  if(i%%10 == 0) {
    pushConcat(tail(divresults,10), "regdiv")
    save(i, file = "lastupload.RData")
  }
  if(i == 97) {
    pushConcat(tail(divresults,7), "regdiv")
    save(i, file = "lastupload.RData")
  }
}