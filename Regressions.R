require(stargazer);

#==== Basic variables ====
fit <- lm(radicality ~ up_reg_div + continent + applicant_type ,data = ApplnStats, na.action = na.omit)
summary(fit)

fit <- lm(radicality ~ continent + applicant_type ,data = ApplnStats, na.action = na.omit)
summary(fit)

fit <- lm(radicality ~ up_reg_div ,data = ApplnStats, na.action = na.omit)
summary(fit)

fit <- lm(radicality ~ reg_div + I(reg_div^2),data = ApplnStats, na.action = na.omit)
summary(fit)

fit <- lm(fwd_cits5_xy ~ radicality, data = uApplnStats, na.action = na.omit)
summary(fit)

#filter for eu
EuStats <- sqldf('select * from ApplnStats WHERE continent = "EU"')

fit <- lm(radicality ~ up_reg_div ,data = EuStats, na.action = na.omit)
summary(fit)

#==== Region level regressions ==== 
# requires eRegstats dataframe as joined in 'Join_patent_statistics.R'

fit <- lm(avg_radicality ~ reg_div ,data = eRegStats, na.action = na.omit)
summary(fit)

fit <- lm(avg_radicality ~ up_reg_div ,data = eupRegStats, na.action = na.omit)
summary(fit)

## regionality

fit <- lm(avg_regionality ~ reg_div ,data = eRegStats, na.action = na.omit)
summary(fit)

fit <- lm(avg_regionality ~ up_reg_div ,data = eupRegStats, na.action = na.omit)
summary(fit)

fit <- lm(radicality ~ up_regionality ,data = uApplnStats, na.action = na.omit)
summary(fit)

fit <- lm(radicality ~ regionality ,data = uApplnStats, na.action = na.omit)
summary(fit)

fit <- lm(avg_regionality ~ avg_radicality ,data = eRegStats, na.action = na.omit)
summary(fit)

## knowledge base diversity
data <- na.omit(uApplnStats[c('radicality', 'back_cit_div', 'applicant_type')])
fit <- lm(log(radicality) ~ back_cit_div + applicant_type, data = data)
summary(fit)
fit0 <- lm(log(radicality) ~ applicant_type, data = data)
summary(fit0)
html <- stargazer(fit0, fit, type="html", single.row=TRUE, column.labels=c("control","model"))
write(html, file = paste0('export/regressiontable',9999,'.html'))

#==== Regional regression with oecd controls ====
# requires eeRegStats from 'join_OECD_stats.R'

data <- eeRegStats[,sapply(eeRegStats, is.numeric)] #remove region names etc.
data$avg_impact <- NULL
data$avg_regionality <- NULL

## stepAIC 

# Idenfity variables with multicolliniarity (with vif value > 10)
require(usdm)

datacorr <- rcorr(as.matrix(data))
View(datacorr$r)
datacoll <- vif(data)
# colsToKeep <- as.character(datacoll[datacoll$VIF<10,1]) # not applicable anymore
# data <- data[,colsToKeep]

rfit <- lm(avg_radicality ~ reg_div + ., na.omit(data)) # Original fit
rfit1 <- stepAIC(rfit, direction = "both")
modelssum <- summary(rfit1)
### Gives model 
modelssum$call
modelssum
### Control 
rfit0 <- lm(formula = avg_radicality ~ npatents + COMPANY + 
             INDIVIDUAL, data = na.omit(data))
### Only reg_div
rfit2 <- lm(formula = avg_radicality ~ reg_div, data = na.omit(data))
### all 
rfit3 <- lm(formula = avg_radicality ~ reg_div +. , data = na.omit(data))


# make a nice table
html <- stargazer(rfit0, rfit1, rfit2, rfit3, type="html", single.row=TRUE, column.labels=c("control","model"))
write(html, file = paste0('export/regressiontable',1,'.html'))


###### now for the upRegStats
data <- eeUpRegStats[,sapply(eeUpRegStats, is.numeric)] #remove region names etc.
data$avg_impact <- NULL
data$avg_regionality <- NULL
data$RD_EXP_TOT_PERC <-data$RD_PER_TOT_PERC <- data$INMIG_REL <- NULL
names(data)[2] <- 'reg_div'
datacorr <- rcorr(as.matrix(data))
View(datacorr$r)
datacoll <- vif(data)
#colsToKeep <- as.character(datacoll[datacoll$VIF<10,1])
#data <- data[,colsToKeep]


fit <- lm(avg_radicality ~ reg_div + ., data = data[])
summary(fit)
# Now without reg div
#fit <- lm(avg_radicality ~  .+.^2, data = data[,-2])
#summary(fit)
# Now without regionality
fit <- lm(avg_radicality ~  ., data = data[,-4])
summary(fit)
## stepAIC of first fit 

# Idenfity variables with multicolliniarity (with vif value > 10)
require(usdm)
datacoll <- vif(data)
colsToKeep <- as.character(datacoll[datacoll$VIF<10,1])
data <- data[,colsToKeep]

fit <- lm(avg_radicality ~ reg_div + ., na.omit(data)) # Original fit
upfit1 <- stepAIC(fit, direction = "both")
modelssum <- summary(upfit1)
### Gives model 
modelssum$call
### Control 
upfit0 <- lm(formula = avg_radicality ~ npatents + EDU_LF_ISCED_56_PERC + 
               GDP_PC_REAL_PPP + POP_DEN + GOVNONPROFIT + UNIVERSITY + INDIVIDUAL, 
             data = na.omit(data))
summary(upfit0)

stargazer(upfit0, upfit1, rfit0, rfit1, type="html",
          single.row=TRUE,
          column.labels=c("control TL2","model TL2", "control TL3","model TL3" ), 
          out = paste0('export/regressiontable',2,'.html'), 
          covariate.labels = c('Region diversity', 'N patents', 'Education level workforce', 'GDP per capita', 'Population density','applicant_type GOV NON-PROFIT', 'applicant_type UNIVERSITY', 'applicant_type COMPANY', 'applicant_type INDIVIDUAL'),
          dep.var.labels = c('Average Radicality'),
          ord.intercepts = FALSE,
          omit = c('Constant'))

################################################################################


#==== Generalized Linear Models // log link functions ====

fit <- glm(avg_radicality ~ up_reg_div ,data = eupRegStats, na.action = na.omit, family =poisson(link = "log"))
summary(fit)

fit <- glm(avg_radicality ~ up_reg_div ,data = eupRegStats, na.action = na.omit, family =gaussian(link = "identity"))
summary(fit)

fit <- glm(avg_radicality ~ up_reg_div ,data = eupRegStats, na.action = na.omit, family =Gamma(link="inverse"))
summary(fit)

fit <- glm(avg_radicality ~ up_reg_div ,data = eupRegStats, na.action = na.omit, family =binominal())
summary(fit)

fit <- glm(avg_radicality ~ up_reg_div ,data = eupRegStats, na.action = na.omit, family =binominal())
summary(fit)

























#===== Old regressions ====

#==== with extended controls ====
#all
fit <- lm(tech_rad_rs ~ reg_div + npatents + applicant_type + EE + ME + Ins + Oth,  data=ApplnStats, na.action = na.omit)
summary(fit)
# 0.1035

#only control
fit <- lm(tech_rad_rs ~ applicant_type + EE + ME + Ins + Oth,  data=allStat, na.action = na.omit)
summary(fit)
#0.1037

#all including regionality
fit <- lm(tech_rad_rs ~ reg_div + regionality + npatents + applicant_type + EE + ME + Ins + Oth,  data=allStat, na.action = na.omit)
summary(fit)
#0.1015

vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics


