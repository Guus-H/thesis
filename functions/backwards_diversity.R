#==== Backwards Diversity ====
#Inputs appln_id, dataset, technologymap (scaled)

#calculate technological diversity of set of patents (i.e. all patents assigned to given application)
techDivBackCit <- function(application, data, map){
  applndata <- subset(data, data$appln_id == application)
  if (nrow(applndata)>1){
    combFields <- combn(as.character(applndata[,2]),2)
    fieldsCount <- matrix(data = applndata$ifris_occur, dimnames =  list(applndata$ifris_class,"Count"))
    c(application,sum(sapply(1:ncol(combFields), RaoSt, map = map, combFields = combFields, fieldsCount = fieldsCount)))
  }  
}

# Rao Stirling diversity
RaoSt <- function(i, map, combFields, fieldsCount){  
  phi <- map[combFields[1,i],combFields[2,i]]
  p_i <- fieldsCount[combFields[1,i],1]/sum(fieldsCount)
  p_j <- fieldsCount[combFields[2,i],1]/sum(fieldsCount)
  (1-phi)*p_i*p_j 
}


