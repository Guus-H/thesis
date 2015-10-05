#==== OECD stat filtering cleaning and matching ====
# First converted files from UTF-8 to ANSI using Notepad ++
require(reshape2)
require(sqldf)
#loading data
reg_acc <- read.csv("data/controls/REG_ACC_TL3_18092015151738072.csv")
reg_dem <- read.csv("data/controls/REG_DEMO_TL3_18092015151211807.csv")
reg_inn <- read.csv("data/controls/REG_INNO_TL2_18092015154529697.csv")
reg_acc_tl2 <- read.csv("data/controls/REGACC_TL2_18092015122336914.csv")
reg_dem_tl2 <- read.csv("data/controls/REG_DEMO_TL2_18092015152334354.csv")




#filtering and casting data
reg_acc <- reg_acc[,c('REG_TL3', 'VAR', 'MEAS', 'TIME', 'Value')]
mean_reg_acc <- dcast(reg_acc, REG_TL3 ~ VAR + MEAS, fun.aggregate = mean)

reg_dem <- reg_dem[,c('REG_TL3', 'VAR', 'TIME', 'Value')]
mean_reg_dem <- dcast(reg_dem, REG_TL3 ~ VAR, fun.aggregate = mean)
mean_reg_dem <- sqldf('select REG_TL3, POP_DEN, INMIG_ALL/T_T as INMIG_REL, TYPE as REG_TYPE FROM mean_reg_dem')

reg_inn <- reg_inn[,c('REG_TL2', 'VAR', 'TIME', 'Value')]
mean_reg_inn <- dcast(reg_inn, REG_TL2 ~ VAR , fun.aggregate = mean)

reg_acc_tl2 <- reg_acc_tl2[,c('REG_TL2', 'VAR', 'MEAS', 'TIME', 'Value')]
mean_reg_acc_tl2 <- dcast(reg_acc_tl2, REG_TL2 ~ VAR + MEAS, fun.aggregate = mean)

reg_dem_tl2 <- reg_dem_tl2[,c('REG_ID', 'VAR', 'TIME', 'Value')]
mean_reg_dem_tl2 <- dcast(reg_dem_tl2, REG_ID ~ VAR, fun.aggregate = mean)
mean_reg_dem_tl2 <- sqldf('select REG_ID, POP_DEN, INMIG_ALL/T as INMIG_REL FROM mean_reg_dem_tl2')


#Joining with regional results
absolute <- sqldf('SELECT reg_code, 
  SUM(CASE WHEN applicant_type LIKE "COMPANY" THEN 1 ELSE 0 END) as COMPANY,
 SUM(CASE WHEN applicant_type LIKE "GOV NON-PROFIT" THEN 1 ELSE 0 END) as GOVNONPROFIT,
 SUM(CASE WHEN applicant_type LIKE "UNIVERSITY" THEN 1 ELSE 0 END) as UNIVERSITY,
 SUM(CASE WHEN applicant_type LIKE "INDIVIDUAL" THEN 1 ELSE 0 END) as INDIVIDUAL,
 SUM(CASE WHEN applicant_type LIKE "MULTI" THEN 1 ELSE 0 END) as MULTI
 FROM ApplnStats GROUP BY reg_code
                         ')
 reg_appln_types <- cbind(reg_code = absolute$reg_code,absolute[,2:6]/rowSums(absolute[,2:6]))
 reg_appln_types$MULTI <- NULL # is already explained by the other types
upsolute <- sqldf('SELECT up_reg_code, 
  SUM(CASE WHEN applicant_type LIKE "COMPANY" THEN 1 ELSE 0 END) as COMPANY,
  SUM(CASE WHEN applicant_type LIKE "GOV NON-PROFIT" THEN 1 ELSE 0 END) as GOVNONPROFIT,
  SUM(CASE WHEN applicant_type LIKE "UNIVERSITY" THEN 1 ELSE 0 END) as UNIVERSITY,
  SUM(CASE WHEN applicant_type LIKE "INDIVIDUAL" THEN 1 ELSE 0 END) as INDIVIDUAL,
  SUM(CASE WHEN applicant_type LIKE "MULTI" THEN 1 ELSE 0 END) as MULTI
  FROM ApplnStats GROUP BY up_reg_code
                         ')
 up_reg_appln_types <- cbind(up_reg_code = upsolute$up_reg_code,upsolute[,2:6]/rowSums(upsolute[,2:6]))
 up_reg_appln_types$MULTI <- NULL  # is already explained by the other types
 
###############################################################################

eeRegStats <- sqldf('SELECT * FROM eRegStats t1
                    LEFT JOIN reg_appln_types t3 on t1.reg_code = t3.reg_code
                    ')
#eeRegStats$short <- eeRegStats$reg_code
#long <- eeRegStats$reg_code[is.na(eeRegStats$REG_TL3)]
#short <- substr(long, 1, 3)
#eeRegStats$short[is.na(eeRegStats$REG_TL3)] <- short
# rematch




#eeUpRegStats <- sqldf('SELECT * FROM eupRegStats t1 
 #                   LEFT JOIN mean_reg_acc_tl2 t2 on t1.up_reg_code = t2.REG_TL2
#                      LEFT JOIN mean_reg_inn t3 on t1.up_reg_code = t3.REG_TL2
 #                     ')
 
 
#Find the non-matched regions, remove one digit
eupRegStats$short <- eupRegStats$up_reg_code
long <- eupRegStats$up_reg_code[is.na(eeUpRegStats$REG_TL2)]
short <- substr(long, 1, 3)
eupRegStats$short[is.na(eeUpRegStats$REG_TL2)] <- short
# rematch
eeUpRegStats <- sqldf('SELECT * FROM eupRegStats t1 
                    LEFT JOIN mean_reg_acc_tl2 t2 on t1.short = t2.REG_TL2
                    LEFT JOIN mean_reg_inn t3 on t1.short = t3.REG_TL2
                    LEFT JOIN mean_reg_dem_tl2 t5 on t1.short = t5.REG_ID
                    LEFT JOIN up_reg_appln_types t6 on t1.up_reg_code = t6.up_reg_code
                      ')

