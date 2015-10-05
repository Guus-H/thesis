#====== 3. Create patent statistics dataframe ======
# loading/sourcing dependencies
source('functions/load_packages.R') #Packages
regpat_regions <- readRDS(file="data/regpat_regions.Rds") #same as the thesis.regpat_regions table in bigquery


#===== 3.0 load stored files =====
RegStats <-  readRDS( file='results/RegStats05.Rds')  ## created with from 'Regional_diversity_measures.R'
upRegStats <- readRDS( file='results/upRegStats05.Rds')

#ApplnPrioYear <- readRDS(file = 'data/ApplnPrioYear.Rds') ## Obtained with query 07
ApplnPrioYear <- query_exec("SELECT t1.appln_id as appln_id, prior_earliest_year AS year FROM thesis.tls201_appln t1
INNER JOIN EACH thesis.regpat_all_inv t2 on t1.appln_id = t2.Appln_id
                            WHERE source = 'EP'
                            GROUP EACH BY appln_id, year", project, max_pages = Inf)

ApplnStats <- readRDS('results/applnStats18-8.Rds')
ApplnStatsOld <- ApplnStats[,c(1,2,5,8:11)]
rm(ApplnStats)
### Update radicality to new measure!!!!!
ApplnStatsBare <- sqldf('SELECT appln_id, reg_code, up_reg_code, 1/(1-tech_rad_mean) as radicality FROM ApplnStatsOld')
gc()

#===== 3.1 Join appln stats with regional data =====
RegStats$reg_name <- NULL
upRegStats$up_reg_name <- NULL

ApplnStats9600 <- sqldf('SELECT * from ApplnStatsBare 
                        NATURAL JOIN ApplnPrioYear 
                        NATURAL JOIN RegStats
                        WHERE year BETWEEN 1996 AND 2000
                        AND npatents > 999')

ApplnStats9600u <- sqldf('SELECT * from ApplnStatsBare 
                        NATURAL JOIN ApplnPrioYear 
                        NATURAL JOIN upRegStats
                        WHERE year BETWEEN 1996 AND 2000
                        AND npatents > 999')

#===== 3.2 Complete regional data with average radicality measures
RegStats9600 <- sqldf("
SELECT t1.reg_code AS reg_code,
                  reg_label||', '||up_reg_label||', '||ctry_code AS region,
                  COUNT(DISTINCT appln_id) AS npatents,
                  AvgRelDens, 
                  reg_div, 
                  AVG(tech_rad_mean) AS avg_tech_rad_mean, 
                  AVG(tech_rad_rs) AS avg_tech_rad_rs,
                  AVG(regionality) AS avg_regionality
                  FROM ApplnStats9600 t1
                  INNER JOIN regpat_regions t2 on t1.reg_code = t2.reg_code
                  GROUP BY t1.reg_code")

upRegStats9600 <- sqldf("
                  SELECT t1.up_reg_code AS up_reg_code,
                  up_reg_label||', '||ctry_code AS region,
                  COUNT(DISTINCT appln_id) AS npatents,
                  AvgRelDens, 
                  reg_div, 
                  AVG(tech_rad_mean) AS avg_tech_rad_mean, 
                  AVG(tech_rad_rs) AS avg_tech_rad_rs,
                  AVG(regionality) AS avg_regionality
                  FROM ApplnStats9600u t1
                  INNER JOIN (select distinct up_reg_code, up_reg_label, ctry_code FROM regpat_regions) t2 
                    on t1.up_reg_code = t2.up_reg_code
                  GROUP BY t1.up_reg_code")

## Only n > 999 

RegStats9600L <- RegStats9600[RegStats9600[,3] > 999,]
upRegStats9600L <- upRegStats9600[upRegStats9600[,3] > 999,]
#===== 3.x.1 correlations =====
rcorr(as.matrix(RegStats9600[,4:8]))
rcorr(as.matrix(upRegStats9600[,4:8]))
rcorr(as.matrix(ApplnStats9600[,c(4:6,10,11)]))
rcorr(as.matrix(ApplnStats9600u[,c(4,5,7,10,11)]))

rcorr(as.matrix(RegStats9600L[,4:8]))
rcorr(as.matrix(upRegStats9600L[,4:8]))


pairs(ApplnStatsu9600[,4:8])

#===== 3.x.2 plot density plots =====

ApplnStats9600 -> sample
sample$group <- ''
boundary <- 0.5 # 

# reg div + rad rs
for(boundary in boundaries){
topname <- paste('top ', (1-boundary)*100, '%', sep='')
botname <- paste('bottom ', (boundary)*100, '%', sep='')
sample[sample$reg_div > quantile(sample$reg_div, prob= boundary),12] <- topname
sample[sample$reg_div <= quantile(sample$reg_div, prob= boundary),12] <- botname


ggplot(sample, aes(tech_rad_rs, fill=group, colour=group)) +
  geom_histogram(aes(y=..density..), alpha=0.6, 
                 position="identity", lwd=0.2) +
  ggtitle("Histogram of radicality")
ggsave(filename = paste('graphics/dens/RegDiv_RadRS9600_PCT_', boundary, '.png', sep=''))
cat(boundary)
}

# reg div + rad mean
for(boundary in boundaries){
  topname <- paste('top ', (1-boundary)*100, '%', sep='')
  botname <- paste('bottom ', (boundary)*100, '%', sep='')
  sample[sample$reg_div > quantile(sample$reg_div, prob= boundary),12] <- topname
  sample[sample$reg_div <= quantile(sample$reg_div, prob= boundary),12] <- botname
  
  
  ggplot(sample, aes(tech_rad_mean, fill=group, colour=group)) +
    geom_histogram(aes(y=..density..), alpha=0.6, 
                   position="identity", lwd=0.2) +
    ggtitle("Histogram of radicality")
  ggsave(filename = paste('graphics/dens/RegDiv_RadMean9600_PCT_', boundary, '.png', sep=''))
  cat(boundary)
}

# rel dens + rad mean
for(boundary in boundaries){
  topname <- paste('top ', (1-boundary)*100, '%', sep='')
  botname <- paste('bottom ', (boundary)*100, '%', sep='')
  sample[sample$AvgRelDens > quantile(sample$AvgRelDens, prob= boundary),12] <- topname
  sample[sample$AvgRelDens <= quantile(sample$AvgRelDens, prob= boundary),12] <- botname
  
  
  ggplot(sample, aes(tech_rad_mean, fill=group, colour=group)) +
    geom_histogram(aes(y=..density..), alpha=0.6, 
                   position="identity", lwd=0.2) +
    ggtitle("Histogram of radicality")
  ggsave(filename = paste('graphics/dens/RelDens_RadMean9600_PCT_', boundary, '.png', sep=''))
  cat(boundary)
}

# rel dens + rad rs
for(boundary in boundaries){
  topname <- paste('top ', (1-boundary)*100, '%', sep='')
  botname <- paste('bottom ', (boundary)*100, '%', sep='')
  sample[sample$AvgRelDens > quantile(sample$AvgRelDens, prob= boundary),12] <- topname
  sample[sample$AvgRelDens <= quantile(sample$AvgRelDens, prob= boundary),12] <- botname
  
  
  ggplot(sample, aes(tech_rad_rs, fill=group, colour=group)) +
    geom_histogram(aes(y=..density..), alpha=0.6, 
                   position="identity", lwd=0.2) +
    ggtitle("Histogram of radicality")
  ggsave(filename = paste('graphics/dens/RelDens_RadRS9600_PCT_', boundary, '.png', sep=''))
  cat(boundary)
}


#=====3.9 Save results =====

joined_stats_ep <- list(ApplnStats9600, ApplnStats9600u, RegStats9600, upRegStats9600)
saveRDS(joined_stats_ep, file="results/joined_stats_9600.Rds")
rm(joined_stats_ep)
gc()




#=====3.10 Join control variables domain and applicant type ====
tmp <- readRDS('results/joined_stats_ep.Rds')
ApplnStats <- tmp[[1]]
applnApplicantType <- list_tabledata(project, 'thesis', 'appln_applicant_type', max_pages = Inf)
applnDomains <- list_tabledata(project, 'thesis', 'appln_domains', max_pages = Inf)

allStat <- sqldf('SELECT * FROM ApplnStats NATURAL JOIN applnApplicantType NATURAL JOIN applnDomains')

ApplnStats <- readRDS('data/applnStatsExt')
allStat <- sqldf('SELECT * FROM ApplnStats NATURAL JOIN applnApplicantType NATURAL JOIN applnDomains')



#=====3.11 Join all variables for main hypotheses =====
# application data
ApplnStatsBare <- readRDS('data/ApplnStatsBare.Rds')
ApplnStatsExt <- readRDS('data/applnStatsExt')
ApplnFwdCit <- sqldf('select ti_appln_id as appln_id, tx_fwd_cits5_xy as fwd_cits5_xy FROM ApplnStatsExt')
rm(ApplnStatsExt)
ApplnBackCitDiv <- readRDS('data/backCitDiv.Rds')
ApplnApplicantType <- readRDS('data/applnApplicantType')[,1:2]
# regional data
upRegStats <- readRDS('data/upRegStats.Rds')
RegStats <- readRDS('data/RegStats.Rds')
upRegStatsL <- sqldf('select up_reg_code, reg_div as up_reg_div FROM upRegStats WHERE npatents > 999')
RegStatsL <- sqldf('select reg_code, reg_div FROM RegStats WHERE npatents > 999')
upRegStatsL <- upRegStatsL[-grep('ZZ', upRegStatsL$up_reg_code),] 
RegStatsL <- RegStatsL[-grep('ZZ', RegStatsL$reg_code),] 
## Assigning continent
load("data/EU_NUTS2.RDa")
upRegStatsL$continent <- ""
upRegStatsL$continent[grep('US', upRegStatsL$up_reg_code)] <- 'US'
upRegStatsL$continent[upRegStatsL$up_reg_code %in% EU_NUTS2$NUTS_ID] <- 'EU'
# upRegStatsL <- upRegStatsL[- grep('ZZ', upRegStatsL$up_reg_code),] # not required anymore
#joining vars
ApplnStats <- sqldf('SELECT t1.appln_id as appln_id, reg_code, up_reg_code, radicality, regionality, up_regionality, fwd_cits5_xy, back_cit_div, applicant_type, reg_div, up_reg_div, continent
                    FROM ApplnStatsBare t1
                    LEFT JOIN ApplnFwdCit t2 on t1.appln_id = t2.appln_id
                    LEFT JOIN ApplnBackCitDiv t3 on t1.appln_id = t3.appln_id
                    LEFT JOIN ApplnApplicantType t4 on t1.appln_id = t4.appln_id
                    NATURAL JOIN upRegStatsL
                    NATURAL JOIN RegStatsL
                    GROUP by t1.appln_id, reg_code, up_reg_code, radicality, regionality, up_regionality, fwd_cits5_xy, back_cit_div, applicant_type, reg_div, up_reg_div, continent
                    ')
ApplnStats$applicant_type <- as.factor(ApplnStats$applicant_type)
ApplnStats$continent <- as.factor(ApplnStats$continent)

# ==== Back to region level with average values ====
RegStats$reg_name <- NULL
upRegStats$up_reg_name <- NULL

ApplnStats9600 <- sqldf('SELECT * from ApplnStatsBare 
                        NATURAL JOIN ApplnPrioYear 
                        NATURAL JOIN RegStats
                        WHERE year BETWEEN 1996 AND 2000
                        AND npatents > 999')

ApplnStats9600u <- sqldf('SELECT * from ApplnStatsBare 
                         NATURAL JOIN ApplnPrioYear 
                         NATURAL JOIN upRegStats
                         WHERE year BETWEEN 1996 AND 2000
                         AND npatents > 999')

#===== 3.2 Complete regional data with average radicality measures
eRegStats <- sqldf("
                      SELECT t1.reg_code AS reg_code,
                      reg_label||', '||up_reg_label||', '||ctry_code AS region,
                      COUNT(DISTINCT appln_id) AS npatents,
                      reg_div, 
                      AVG(radicality) AS avg_radicality,
                      AVG(regionality) AS avg_regionality,
                      AVG(fwd_cits5_xy) AS avg_impact
                      FROM ApplnStats t1
                      INNER JOIN regpat_regions t2 on t1.reg_code = t2.reg_code
                      GROUP BY t1.reg_code")

eupRegStats <- sqldf("
                        SELECT t1.up_reg_code AS up_reg_code,
                        up_reg_label||', '||ctry_code AS region,
                        COUNT(DISTINCT appln_id) AS npatents,
                        up_reg_div, 
                        AVG(radicality) AS avg_radicality, 
                        AVG(regionality) AS avg_regionality
                        FROM ApplnStats t1
                        INNER JOIN (select distinct up_reg_code, up_reg_label, ctry_code FROM regpat_regions) t2 
                        on t1.up_reg_code = t2.up_reg_code
                        GROUP BY t1.up_reg_code")

## Only n > 999 

RegStats9600L <- RegStats9600[RegStats9600[,3] > 999,]
upRegStats9600L <- upRegStats9600[upRegStats9600[,3] > 999,]