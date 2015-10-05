#======== 2.Calculating regional diversity measures ========
# loading/sourcing dependencies
source('functions/techdiv-techrel2.R') #Custom functions
source('functions/load_packages.R') #Packages
regpat_regions <- readRDS(file="data/regpat_regions.Rds") #same as the thesis.regpat_regions table in bigquery



#===== 2.1 Regional diversity by Rao-Stirling =====
RegOccur <-  readRDS(file="data/reg_ifris_occur.Rds") # regOccur was created by bigquery #03
RegOccur <- query_exec("SELECT
  t1.Reg_code AS reg_code,
                       t2.ifris_class AS ifris_class,
                       COUNT(t1.Appln_id) AS ifris_occur
                       FROM
                       thesis.regpat_all_inv t1
                       INNER JOIN EACH (
                       SELECT
                       appln_id,
                       ifris_class
                       FROM
                       thesis.appln_ipc_ifris
                       ) t2
                       ON
                       t1.Appln_id = t2.appln_id
                       INNER JOIN EACH thesis.tls201_appln t3 on t1.Appln_id = t3.appln_id 
                       WHERE t3.prior_earliest_year BETWEEN 1996 and 2000
                       AND reg_code NOT LIKE '%ZZ%'
                       AND reg_code NOT LIKE ''
                       GROUP EACH BY reg_code, ifris_class
                       ORDER BY reg_code, ifris_class
                       ", project, max_pages = Inf)

map <- readRDS(file = 'maps/jaccard/allyear/techmap_2012.RDa')
map <- scaleMap(map) # redistributes data with log
#map <- invertMap(map) 
## TL3 (small regions in oecd area)
res3 <- pblapply(unique(RegOccur$reg_code), techDivReg, data = RegOccur, map) #Use custom function techDivReg to create a list of vectors with technological diversity for each region
res3 <- res3[! sapply(res3, is.null)] # Remove the empty list entries, resulting from regions with less than two classes.
RegDiv <- as.data.frame(res3) #convert list of vectors to dataframe
names(RegDiv) <- c("reg_code", "reg_div") #setting correct column-names
RegDiv$reg_div <-  as.numeric(levels(RegDiv$reg_div))[RegDiv$reg_div]

## TL2 (large regions in oecd area)
query <- 'SELECT t2.up_reg_code AS reg_code, t1.ifris_class AS ifris_class, sum(t1.ifris_occur) AS ifris_occur
FROM RegOccur t1
INNER JOIN regpat_regions t2 on t1.reg_code=t2.reg_code
GROUP BY t2.up_reg_code, ifris_class'
upRegOccur <- sqldf(query)
### Or use the RDS stored version
upRegOccur <-  readRDS(file="data/upRegOccur.Rds")

res2 <- pblapply(unique(upRegOccur$reg_code), techDivReg, data = upRegOccur, map) #Use custom function techDivReg to create a list of vectors with technological diversity for each region
res2 <- res2[! sapply(res2, is.null)] # Remove the empty list entries, resulting from regions with less than two classes.
upRegDiv <- as.data.frame(res2) #convert list of vectors to dataframe
names(upRegDiv) <- c("up_reg_code", "reg_div") #setting correct column-names
upRegDiv$reg_div <-  as.numeric(levels(upRegDiv$reg_div))[upRegDiv$reg_div]

#===== 2.2 Regional diversity by average relatedness density =====

## TL3 (small regions in oecd area)
tmp <- avgRelDens(RegOccur,map)
RegAvgRelDens <- data.frame(reg_code=rownames(tmp), AvgRelDens = tmp[,1])

## TL2 (large region in oecd area)
tmp <- avgRelDens(upRegOccur,map)
upRegAvgRelDens <- data.frame(up_reg_code=rownames(tmp), AvgRelDens = tmp[,1])

#===== 2.3 Patent counts =====
## TL3
RegApplnCount <- query_exec('SELECT Reg_code as reg_code, count(distinct t1.Appln_id) as npatents from thesis.regpat_all_inv t1
                            INNER JOIN EACH thesis.tls201_appln t3 on t1.Appln_id = t3.appln_id 
                            WHERE t3.prior_earliest_year BETWEEN 1980 and 2012 
                            GROUP BY Reg_code', project)
## TL2
upRegApplnCount <- query_exec('SELECT up_reg_code, COUNT(distinct t2.appln_id) AS npatents FROM thesis.regpat_regions t1
                              INNER JOIN EACH thesis.regpat_all_inv t2 on t1.reg_code = t2.Reg_code
                              INNER JOIN EACH thesis.tls201_appln t3 on t2.Appln_id = t3.appln_id 
                              WHERE t3.prior_earliest_year BETWEEN 1980 and 2012
                              GROUP BY up_reg_code', project)

#===== 2.4 Names =====
## TL3
RegNames <- sqldf("SELECT DISTINCT reg_code, reg_label||', '||up_reg_label||', '||ctry_code AS reg_name FROM regpat_regions")
## TL2  
upRegNames <- sqldf("SELECT DISTINCT up_reg_code, up_reg_label||', '||ctry_code AS up_reg_name FROM regpat_regions")

#===== Collecting regional statistics =====

RegStats <- sqldf('SELECT * FROM RegNames
                    NATURAL JOIN RegApplnCount
                    NATURAL JOIN RegAvgRelDens
                    NATURAL JOIN RegDiv')  

upRegStats <- sqldf('SELECT * FROM upRegNames
                      NATURAL JOIN upRegApplnCount
                      NATURAL JOIN upRegAvgRelDens
                      NATURAL JOIN upRegDiv')  

### WITHOUT REL_DENS ###

nRegStats <- sqldf('SELECT * FROM RegNames
                    NATURAL JOIN RegApplnCount
                  NATURAL JOIN RegDiv
                  WHERE npatents > 999')  

nupRegStats <- sqldf('SELECT * FROM upRegNames
                    NATURAL JOIN upRegApplnCount
                    NATURAL JOIN upRegDiv
                    WHERE npatents > 999')  



####

rm(res3)
rm(res2)
rm(tmp)
rm(RegNames)
rm(RegApplnCount)
rm(RegAvgRelDens)
rm(RegDiv)
rm(upRegNames)
rm(upRegApplnCount)
rm(upRegAvgRelDens)
rm(upRegDiv)
gc()


#==== Continents ====
## Assigning continent
load("data/EU_NUTS2.RDa")
upRegStats$continent <- ""
upRegStats$continent[grep('US', upRegStats$up_reg_code)] <- 'US'
upRegStats$continent[upRegStats$up_reg_code %in% EU_NUTS2$NUTS_ID] <- 'EU'


