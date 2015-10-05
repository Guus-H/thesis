source('functions/load_packages.R') #Packages
#==== check impact of radical innovations on average ====
ApplnStats <- readRDS('results/applnStats18-8.Rds')
ApplnStats <- ApplnStats[ApplnStats$radicality!=1,]
probs <- c(0.9, 0.95, 0.99)
quantiles <- quantile(ApplnStats$radicality, prob=probs, na.rm = TRUE)
threshold <- as.numeric(quantiles['90%'])

#sampleregs
regs <- unique(ApplnStats$reg_code)
sampleregs <- regs[sample(length(regs), 100)]
sampleregs <- paste(shQuote(sampleregs), collapse=", ")

query <- paste0("SELECT
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
                WHERE t1.Appln_id NOT IN (select appln_id from thesis.appln_stats where radicality > ", threshold, " group by appln_id) 
                AND reg_code in (", sampleregs,")
                GROUP EACH BY reg_code, ifris_class
                ORDER BY reg_code, ifris_class" )

RegOccur <- query_exec(query, project, max_pages = Inf )
map <- readRDS(file = 'maps/jaccard/allyear/techmap_2012.RDa')
map <- scaleMap(map) 
# calculate new diversities
## TL3 regions
res3 <- pblapply(unique(RegOccur$reg_code), techDivReg, data = RegOccur, map) #Use custom function techDivReg to create a list of vectors with technological diversity for each region
res3 <- res3[! sapply(res3, is.null)] # Remove the empty list entries, resulting from regions with less than two classes.
RegDiv <- as.data.frame(res3) #convert list of vectors to dataframe
names(RegDiv) <- c("reg_code", "reg_div") #setting correct column-names
RegDiv$reg_div <-  as.numeric(levels(RegDiv$reg_div))[RegDiv$reg_div]


# comparing
compareregdiv <- sqldf('SELECT t1.reg_code as reg_code, t1.reg_div as reg_div_orig, t2.reg_div as reg_div_corr, avg_radicality
                       FROM eRegStats t1
                       INNER JOIN RegDiv t2 on t1.reg_code == t2.reg_code')
cor(compareregdiv[,2:3])
summary(lm(reg_div_orig ~ reg_div_corr, data = compareregdiv))

fit1 <- lm(avg_radicality ~ reg_div_corr, data = compareregdiv)
fit2 <- lm(avg_radicality ~ reg_div_orig, data = compareregdiv)

require(stargazer)
stargazer(fit1, fit2, type="html",
          single.row=TRUE,
          column.labels=c("corrected model","original model" ), 
          out = paste0('export/regressiontable',3,'.html'), 
          dep.var.labels = c('Average Radicality'),
          ord.intercepts = FALSE)
