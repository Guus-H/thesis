#install.packages("data.table")
#library(data.table)

#calculate technological diversity of set of patents (i.e. all patents assigned to given region)
techDivReg <- function(region, data, map){
regdata <- subset(data, data$reg_code == region)
if (nrow(regdata)>1){
combFields <- combn(regdata[,2],2)
fieldsCount <- matrix(data = regdata$ifris_occur, dimnames =  list(regdata$ifris_class,"Count"))
c(region,sum(sapply(1:ncol(combFields), RaoSt, map = map, combFields = combFields, fieldsCount = fieldsCount)))
}  
}

# Rao Stirling diversity
RaoSt <- function(i, map, combFields, fieldsCount){  
  phi <- map[combFields[1,i],combFields[2,i]]
  p_i <- fieldsCount[combFields[1,i],1]/sum(fieldsCount)
  p_j <- fieldsCount[combFields[2,i],1]/sum(fieldsCount)
  (1-phi)*p_i*p_j 
}

# Average Relatedness Density
avgRelDens <- function(regOccur, map){
  adj <- acast(regOccur, reg_code~ifris_class, value.var="ifris_occur")
  adj[is.na(adj)] <- 0
#  matrta <- adj
cat('Determining relative technological advantages... (step 1/2)', sep="\n")  
matrta <-  t(pbsapply(rownames(adj), regrta, adj=adj))
  
cat('Calculating relatedness densitities... (step 2/2)', sep="\n")  
allRelDens <- t(pbsapply(rownames(matrta), regRelDens, matrta, map))
as.data.frame(rowMeans(allRelDens, na.rm = TRUE))
}

#run rta for every reg
regrta <- function(reg, adj){
  sapply(colnames(adj), rta, reg=reg, adj=adj)
}

#Relative technological advantage
rta <- function(ifris, reg, adj) {
((adj[reg,ifris]/sum(adj[reg,]))/(sum(adj[,ifris])/sum(adj)) > 1)*1
}

#run RelDens for every reg
regRelDens <- function(reg, matrta=matrta, map=map){
  sapply(colnames(matrta), RelDens, reg=reg, matrta=matrta, map=map)
}

RelDens <- function(ifris, reg, matrta, map){
  RCAs <- colnames(matrta[0,matrta[reg,] == 1])
  sum(map[ifris,RCAs]) / sum(map[ifris,])   # As map[i,i] == 0, it does not affect the sum Phi(ij) where i != j
}

#==== Normalize proximity values ====

scaleMap <- function(map){
  lmap <- apply(map, 1, FUN=function(x) log(x))  # Take natural logarithm
  lmap[lmap == -Inf] <- NA
  lmap <- reshape::rescaler.default(lmap, type = "range")
  lmap[is.na(lmap)] <- 0
  diag(lmap) <- NA
  lmap
}

invertMap <- function(map){
  imap <- apply(map, 1, FUN=function(x) 1/x)
  imap[imap == Inf] <- NA
  imap <- reshape::rescaler.default(imap, type = "range")
  imap[is.na(imap)] <- 1 
  diag(imap) <- NA
  imap <- apply(imap, 1, FUN=function(x) 1-x)
  imap
}
