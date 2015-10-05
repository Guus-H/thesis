#======== 1.Calculating patent radicality measures ========
# loading/sourcing dependencies
source('~/mthesis2/load packages.R') #Packages
source('~/mthesis2/techcoherence3.R') #Functions
source('~/mthesis2/techradicality.R') 

#===== 1.1 Patent radicality by mean(1-technological coherence) =====
tmp <- lapply(1980:2012, coherenceFun, fun = "jaccard", publish=FALSE) # uses 10-year technology maps, returns 1-coherence for each application in given period
applnRadMean <- rbind.fill(tmp)

#===== 1.2 Patent radicality by rao-stirling diversity of classes =====
tmp <- lapply(1980:2012, radicalFun, fun = "jaccard", period = "10", publish=FALSE)
applnRadRS <- rbind.fill(tmp)