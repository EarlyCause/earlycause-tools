############################# Cohort-level statistical analyses and summary statistics on the relationship of childhood maltreatment and depression co-occurrence with metabolic outcomes within each cohort #############################



#################### 0. General settings ####################

# Upload packages
library('foreign')
library('table1')
library('tidyverse')
library('dplyr')
library('gt')
library('plyr')
library('nnet')
library('pscl')
library('broom')
library('xtable')
library('car')
library('emmeans')

# Set working directory, adjust with your own pathway
setwd("")



#################### 1. Load datasets #################### 

# Adjust pathway names to your own
load("~/Samplemain.Rdata")
load("~/Samplemales.Rdata")
load("~/Samplefemales.Rdata")
load("~/Samplenomed.Rdata")



#################### 2. Functions to extract summary statistics #################### 

models_main_cm_info <- function(modelx, outcome_quotes){
  
  #Select only complete cases in the dataset
  mydata <- Sample_main[,all.vars(formula(modelx))]
  mydata <- mydata[complete.cases(mydata),]
  
  #Model to estimate means
  model.emm <- emmeans(modelx, pairwise ~ cm)
  model.data <- augment(modelx) %>% 
    dplyr::mutate(index = 1:n())
  
  #Create data frame to store desired model characteristics
  Stats_model = data.frame(durbinWatson_autocorrelation = durbinWatsonTest(modelx)$r,       #Independence of residuals
  durbinWatson_statistic = durbinWatsonTest(modelx)$dw,
  durbinWatson_p = durbinWatsonTest(modelx)$p,
  vif = car::vif(modelx)[1,'GVIF'],
  N = nobs(modelx),                                                                         #Number of observations used in model
  n_males = nrow(mydata[mydata$sex == 0,]),
  n_females = nrow(mydata[mydata$sex == 1,]),
  n_cm0 = nrow(mydata[mydata$cm == 0,]),
  n_cm1 = nrow(mydata[mydata$cm == 1,]),
  mean_age = mean(mydata$age,na.rm=T),
  median_age = median(mydata$age,na.rm = T),
  sd_age = sd(mydata$age,na.rm = T),
  n_educ0 = table(mydata$educ)[1],
  n_educ1 = table(mydata$educ)[2],
  n_educ2 = table(mydata$educ)[3],
  n_ethn0 = table(mydata$ethn)[1],
  n_ethn1 = table(mydata$ethn)[2],
  #Raw mean/median/sd of each group
  cm0_mean_outcome = mean(subset(mydata, cm == 0, select = outcome_quotes)[,1], na.rm=T),
  cm0_median_outcome = median(subset(mydata, cm == 0, select = outcome_quotes)[,1], na.rm=T),
  cm0_sd_outcome = sd(subset(mydata, cm == 0, select = outcome_quotes)[,1], na.rm=T),
  cm1_mean_outcome = mean(subset(mydata, cm == 1, select = outcome_quotes)[,1], na.rm=T),
  cm1_median_outcome = median(subset(mydata, cm == 1, select = outcome_quotes)[,1], na.rm=T),
  cm1_sd_outcome = sd(subset(mydata, cm == 1, select = outcome_quotes)[,1], na.rm=T),
  mean_outcome = mean(mydata[,outcome_quotes], na.rm=T),
  median_outcome = median(mydata[,outcome_quotes], na.rm=T),
  sd_outcome = sd(mydata[,outcome_quotes], na.rm=T),
  
  # Record coefficient estimates, standard errors and 95% CI of log(OR) of IV's effects on the dependent variable
  Estimate_cm1 = summary(modelx)$coefficients['cm1','Estimate'],
  Std.Error_cm1 = summary(modelx)$coefficients['cm1','Std. Error'],
  p_cm1 = summary(modelx)$coefficients['cm1','Pr(>|t|)'],
  Estimate_age = summary(modelx)$coefficients['age','Estimate'],
  Std.Error_age = summary(modelx)$coefficients['age','Std. Error'],
  p_age = summary(modelx)$coefficients['age','Pr(>|t|)'],
  Estimate_sex1 = summary(modelx)$coefficients['sex1','Estimate'],
  Std.Error_sex1 = summary(modelx)$coefficients['sex1','Std. Error'],
  p_sex1 = summary(modelx)$coefficients['sex1','Pr(>|t|)'],
  Estimate_educ1 = summary(modelx)$coefficients['educ1','Estimate'],
  Std.Error_educ1 = summary(modelx)$coefficients['educ1','Std. Error'],
  p_educ1 = summary(modelx)$coefficients['educ1','Pr(>|t|)'],
  Estimate_educ2 = summary(modelx)$coefficients['educ2','Estimate'],
  Std.Error_educ2 = summary(modelx)$coefficients['educ2','Std. Error'],
  p_educ2 = summary(modelx)$coefficients['educ2','Pr(>|t|)'],
  Estimate_ethn = summary(modelx)$coefficients['ethn1','Estimate'],
  Std.Error_ethn = summary(modelx)$coefficients['ethn1','Std. Error'],
  p_ethn = summary(modelx)$coefficients['ethn1','Pr(>|t|)'],
  
  #Estimated mean of each group and their SE's
  emm_cm0 = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cm']=='0','emmean'],
  emm_cm1 = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cm']=='1','emmean'],
  emm.SE_cm0  = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cm']=='0','SE'],
  emm.SE_cm1  = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cm']=='1','SE'],
  
  #Contrasts of estimated means
  emm.contr = as.data.frame(pairs(model.emm$emmeans, reverse=T))[1,1],
  emm.contr.est = as.data.frame(pairs(model.emm$emmeans, reverse=T))[1,'estimate'],
  emm.contr.SE = as.data.frame(pairs(model.emm$emmeans, reverse=T))[1,'SE'],
  emm.contr.p = as.data.frame(pairs(model.emm$emmeans, reverse=T))[1,'p.value'],
  
  #Standardized mean difference (cohen's d)
  cohensd = -as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[,'effect.size'],
  cohensd.SE = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[,'SE'],
  cohensd.lowerCL = -as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[,'upper.CL'],
  cohensd.upperCL = -as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[,'lower.CL'])
  
  return(Stats_model)
}

models_main_mdd_info <- function(modelx, outcome_quotes){
  #Select only complete cases in the dataset
  mydata <- Sample_main[,all.vars(formula(modelx))]
  mydata <- mydata[complete.cases(mydata),]
  
  #Model to estimate means
  model.emm <- emmeans(modelx, pairwise ~ mdd)
  model.data <- augment(modelx) %>% 
    dplyr::mutate(index = 1:n())
  
  # Create data frame to store desired model characteristics
  Stats_model = data.frame(durbinWatson_autocorrelation = durbinWatsonTest(modelx)$r,       #Independence of residuals/error terms
  durbinWatson_statistic = durbinWatsonTest(modelx)$dw,
  durbinWatson_p = durbinWatsonTest(modelx)$p,
  vif = car::vif(modelx)[1,'GVIF'],
  N = nobs(modelx),                                                                         #Number of observations used in model
  n_males = nrow(mydata[mydata$sex == 0,]),
  n_females = nrow(mydata[mydata$sex == 1,]),
  n_mdd0 = nrow(mydata[mydata$mdd == 0,]),
  n_mdd1 = nrow(mydata[mydata$mdd == 1,]),
  mean_age = mean(mydata$age,na.rm=T),
  median_age = median(mydata$age,na.rm = T),
  sd_age = sd(mydata$age,na.rm = T),
  n_educ0 = table(mydata$educ)[1],
  n_educ1 = table(mydata$educ)[2],
  n_educ2 = table(mydata$educ)[3],
  n_ethn0 = table(mydata$ethn)[1],
  n_ethn1 = table(mydata$ethn)[2],
  mdd0_mean_outcome = mean(subset(mydata, mdd == 0, select = outcome_quotes)[,1], na.rm=T),
  mdd0_median_outcome = median(subset(mydata, mdd == 0, select = outcome_quotes)[,1], na.rm=T),
  mdd0_sd_outcome = sd(subset(mydata, mdd == 0, select = outcome_quotes)[,1], na.rm=T),
  mdd1_mean_outcome = mean(subset(mydata, mdd == 1, select = outcome_quotes)[,1], na.rm=T),
  mdd1_median_outcome = median(subset(mydata, mdd == 1, select = outcome_quotes)[,1], na.rm=T),
  mdd1_sd_outcome = sd(subset(mydata, mdd == 1, select = outcome_quotes)[,1], na.rm=T),
  mean_outcome = mean(mydata[,outcome_quotes], na.rm=T),
  median_outcome = median(mydata[,outcome_quotes], na.rm=T),
  sd_outcome = sd(mydata[,outcome_quotes], na.rm=T),
  
  # Record coefficient estimates of log(OR), standard errors and 95% CI of log(OR) of cms' effects on the dependent variable
  Estimate_mdd1 = summary(modelx)$coefficients['mdd1','Estimate'],
  Std.Error_mdd1 = summary(modelx)$coefficients['mdd1','Std. Error'],
  p_mdd1 = summary(modelx)$coefficients['mdd1','Pr(>|t|)'],
  Estimate_age = summary(modelx)$coefficients['age','Estimate'],
  Std.Error_age = summary(modelx)$coefficients['age','Std. Error'],
  p_age = summary(modelx)$coefficients['age','Pr(>|t|)'],
  Estimate_sex1 = summary(modelx)$coefficients['sex1','Estimate'],
  Std.Error_sex1 = summary(modelx)$coefficients['sex1','Std. Error'],
  p_sex1 = summary(modelx)$coefficients['sex1','Pr(>|t|)'],
  Estimate_educ1 = summary(modelx)$coefficients['educ1','Estimate'],
  Std.Error_educ1 = summary(modelx)$coefficients['educ1','Std. Error'],
  p_educ1 = summary(modelx)$coefficients['educ1','Pr(>|t|)'],
  Estimate_educ2 = summary(modelx)$coefficients['educ2','Estimate'],
  Std.Error_educ2 = summary(modelx)$coefficients['educ2','Std. Error'],
  p_educ2 = summary(modelx)$coefficients['educ2','Pr(>|t|)'],
  Estimate_ethn = summary(modelx)$coefficients['ethn1','Estimate'],
  Std.Error_ethn = summary(modelx)$coefficients['ethn1','Std. Error'],
  p_ethn = summary(modelx)$coefficients['ethn1','Pr(>|t|)'],
  
  #Estimated mean of each group and their SE's
  emm_mdd0 = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['mdd']=='0','emmean'],
  emm_mdd1 = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['mdd']=='1','emmean'],
  emm.SE_mdd0  = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['mdd']=='0','SE'],
  emm.SE_mdd1  = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['mdd']=='1','SE'],
  
  #Contrasts of estimated means
  emm.contr = as.data.frame(pairs(model.emm$emmeans, reverse=T))[1,1],
  emm.contr.est = as.data.frame(pairs(model.emm$emmeans, reverse=T))[1,'estimate'],
  emm.contr.SE = as.data.frame(pairs(model.emm$emmeans, reverse=T))[1,'SE'],
  emm.contr.p = as.data.frame(pairs(model.emm$emmeans, reverse=T))[1,'p.value'],
  
  #Standardized mean difference (cohen's d)
  cohensd = -as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[,'effect.size'],
  cohensd.SE = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[,'SE'],
  cohensd.lowerCL = -as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[,'upper.CL'],
  cohensd.upperCL = -as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[,'lower.CL'])
  
  return(Stats_model)
}

models_cooccur_info <- function(modelx, outcome_quotes){
  #Select only complete cases in the dataset
  mydata <- Sample_main[,all.vars(formula(modelx))]
  mydata <- mydata[complete.cases(mydata),]
  
  #Model to estimate means
  model.emm <- emmeans(modelx, pairwise ~ cooccur)
  model.data <- augment(modelx) %>% 
    dplyr::mutate(index = 1:n())
  
  #Create data frame to store desired model characteristics
  Stats_model = data.frame(durbinWatson_autocorrelation = durbinWatsonTest(modelx)$r,       #Independence of residuals/error terms
  durbinWatson_statistic = durbinWatsonTest(modelx)$dw,
  durbinWatson_p = durbinWatsonTest(modelx)$p,
  vif = car::vif(modelx)[1,'GVIF'],
  N = nobs(modelx),
  n_males = nrow(mydata[mydata$sex == 0,]),
  n_females = nrow(mydata[mydata$sex == 1,]),
  n_cooccur0 = nrow(mydata[mydata$cooccur == 0,]),
  n_cooccur1 = nrow(mydata[mydata$cooccur == 1,]),
  n_cooccur2 = nrow(mydata[mydata$cooccur == 2,]),
  n_cooccur3 = nrow(mydata[mydata$cooccur == 3,]),
  mean_age = mean(mydata$age,na.rm=T),
  median_age = median(mydata$age,na.rm = T),
  sd_age = sd(mydata$age,na.rm = T),
  n_educ0 = table(mydata$educ)[1],
  n_educ1 = table(mydata$educ)[2],
  n_educ2 = table(mydata$educ)[3],
  n_ethn0 = table(mydata$ethn)[1],
  n_ethn1 = table(mydata$ethn)[2],
  cooccur0_mean_outcome = mean(subset(mydata, cooccur == 0, select = outcome_quotes)[,1], na.rm=T),
  cooccur0_median_outcome = median(subset(mydata, cooccur == 0, select = outcome_quotes)[,1], na.rm=T),
  cooccur0_sd_outcome = sd(subset(mydata, cooccur == 0, select = outcome_quotes)[,1], na.rm=T),
  cooccur1_mean_outcome = mean(subset(mydata, cooccur == 1, select = outcome_quotes)[,1], na.rm=T),
  cooccur1_median_outcome = median(subset(mydata, cooccur == 1, select = outcome_quotes)[,1], na.rm=T),
  cooccur1_sd_outcome = sd(subset(mydata, cooccur == 1, select = outcome_quotes)[,1], na.rm=T),
  cooccur2_mean_outcome = mean(subset(mydata, cooccur == 2, select = outcome_quotes)[,1], na.rm=T),
  cooccur2_median_outcome = median(subset(mydata, cooccur == 2, select = outcome_quotes)[,1], na.rm=T),
  cooccur2_sd_outcome = sd(subset(mydata, cooccur == 2, select = outcome_quotes)[,1], na.rm=T),
  cooccur3_mean_outcome = mean(subset(mydata, cooccur == 3, select = outcome_quotes)[,1], na.rm=T),
  cooccur3_median_outcome = median(subset(mydata, cooccur == 3, select = outcome_quotes)[,1], na.rm=T),
  cooccur3_sd_outcome = sd(subset(mydata, cooccur == 3, select = outcome_quotes)[,1], na.rm=T),
  mean_outcome = mean(mydata[,outcome_quotes], na.rm=T),
  median_outcome = median(mydata[,outcome_quotes], na.rm=T),
  sd_outcome = sd(mydata[,outcome_quotes], na.rm=T),
  
  # Record coefficient estimates of log(OR), standard errors and 95% CI of log(OR) of cms' effects on the dependent variable
  Estimate_cooccur1 = summary(modelx)$coefficients['cooccur1','Estimate'],
  Std.Error_cooccur1 = summary(modelx)$coefficients['cooccur1','Std. Error'],
  p_cooccur1 = summary(modelx)$coefficients['cooccur1','Pr(>|t|)'],
  Estimate_cooccur2 = summary(modelx)$coefficients['cooccur2','Estimate'],
  Std.Error_cooccur2 = summary(modelx)$coefficients['cooccur2','Std. Error'],
  p_cooccur2 = summary(modelx)$coefficients['cooccur2','Pr(>|t|)'],
  Estimate_cooccur3 = summary(modelx)$coefficients['cooccur3','Estimate'],
  Std.Error_cooccur3 = summary(modelx)$coefficients['cooccur3','Std. Error'],
  p_cooccur3 = summary(modelx)$coefficients['cooccur3','Pr(>|t|)'],
  Estimate_age = summary(modelx)$coefficients['age','Estimate'],
  Std.Error_age = summary(modelx)$coefficients['age','Std. Error'],
  p_age = summary(modelx)$coefficients['age','Pr(>|t|)'],
  Estimate_sex1 = summary(modelx)$coefficients['sex1','Estimate'],
  Std.Error_sex1 = summary(modelx)$coefficients['sex1','Std. Error'],
  p_sex1 = summary(modelx)$coefficients['sex1','Pr(>|t|)'],
  Estimate_educ1 = summary(modelx)$coefficients['educ1','Estimate'],
  Std.Error_educ1 = summary(modelx)$coefficients['educ1','Std. Error'],
  p_educ1 = summary(modelx)$coefficients['educ1','Pr(>|t|)'],
  Estimate_educ2 = summary(modelx)$coefficients['educ2','Estimate'],
  Std.Error_educ2 = summary(modelx)$coefficients['educ2','Std. Error'],
  p_educ2 = summary(modelx)$coefficients['educ2','Pr(>|t|)'],
  Estimate_ethn = summary(modelx)$coefficients['ethn1','Estimate'],
  Std.Error_ethn = summary(modelx)$coefficients['ethn1','Std. Error'],
  p_ethn = summary(modelx)$coefficients['ethn1','Pr(>|t|)'],
  
  #Estimated mean of each group and their SE's
  emm_cooccur0 = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cooccur']=='0','emmean'],
  emm_cooccur1 = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cooccur']=='1','emmean'],
  emm_cooccur2 = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cooccur']=='2','emmean'],
  emm_cooccur3 = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cooccur']=='3','emmean'],
  emm.SE_cooccur0  = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cooccur']=='0','SE'],
  emm.SE_cooccur1  = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cooccur']=='1','SE'],
  emm.SE_cooccur2  = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cooccur']=='2','SE'],
  emm.SE_cooccur3  = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cooccur']=='3','SE'],
  
  #Contrasts of estimated means
  emm.contr1 = as.data.frame(pairs(model.emm$emmeans, reverse=T))[1,'contrast'],
  emm.contr1.est = as.data.frame(pairs(model.emm$emmeans, reverse=T))[1,'estimate'],
  emm.contr1.SE = as.data.frame(pairs(model.emm$emmeans, reverse=T))[1,'SE'],
  emm.contr1.p = as.data.frame(pairs(model.emm$emmeans, reverse=T))[1,'p.value'],
  emm.contr2 = as.data.frame(pairs(model.emm$emmeans, reverse=T))[2,'contrast'],
  emm.contr2.est = as.data.frame(pairs(model.emm$emmeans, reverse=T))[2,'estimate'],
  emm.contr2.SE = as.data.frame(pairs(model.emm$emmeans, reverse=T))[2,'SE'],
  emm.contr2.p = as.data.frame(pairs(model.emm$emmeans, reverse=T))[2,'p.value'],
  emm.contr3 = as.data.frame(pairs(model.emm$emmeans, reverse=T))[3,'contrast'],
  emm.contr3.est = as.data.frame(pairs(model.emm$emmeans, reverse=T))[3,'estimate'],
  emm.contr3.SE = as.data.frame(pairs(model.emm$emmeans, reverse=T))[3,'SE'],
  emm.contr3.p = as.data.frame(pairs(model.emm$emmeans, reverse=T))[3,'p.value'],
  emm.contr4 = as.data.frame(pairs(model.emm$emmeans, reverse=T))[4,'contrast'],
  emm.contr4.est = as.data.frame(pairs(model.emm$emmeans, reverse=T))[4,'estimate'],
  emm.contr4.SE = as.data.frame(pairs(model.emm$emmeans, reverse=T))[4,'SE'],
  emm.contr4.p = as.data.frame(pairs(model.emm$emmeans, reverse=T))[4,'p.value'],
  emm.contr5 = as.data.frame(pairs(model.emm$emmeans, reverse=T))[5,'contrast'],
  emm.contr5.est = as.data.frame(pairs(model.emm$emmeans, reverse=T))[5,'estimate'],
  emm.contr5.SE = as.data.frame(pairs(model.emm$emmeans, reverse=T))[5,'SE'],
  emm.contr5.p = as.data.frame(pairs(model.emm$emmeans, reverse=T))[5,'p.value'],
  emm.contr6 = as.data.frame(pairs(model.emm$emmeans, reverse=T))[6,'contrast'],
  emm.contr6.est = as.data.frame(pairs(model.emm$emmeans, reverse=T))[6,'estimate'],
  emm.contr6.SE = as.data.frame(pairs(model.emm$emmeans, reverse=T))[6,'SE'],
  emm.contr6.p = as.data.frame(pairs(model.emm$emmeans, reverse=T))[6,'p.value'],

  #Standardized mean difference (cohen's d)
  cohensd.contr1 = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[1,'contrast'],
  cohensd.contr1.est = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[1,'effect.size'],
  cohensd.contr1.SE = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[1,'SE'],
  cohensd.contr1.lowerCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[1,'lower.CL'],
  cohensd.contr1.upperCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[1,'upper.CL'],
  cohensd.contr2 = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[2,'contrast'],
  cohensd.contr2.est = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[2,'effect.size'],
  cohensd.contr2.SE = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[2,'SE'],
  cohensd.contr2.lowerCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[2,'lower.CL'],
  cohensd.contr2.upperCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[2,'upper.CL'],
  cohensd.contr3 = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[3,'contrast'],
  cohensd.contr3.est = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[3,'effect.size'],
  cohensd.contr3.SE = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[3,'SE'],
  cohensd.contr3.lowerCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[3,'lower.CL'],
  cohensd.contr3.upperCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[3,'upper.CL'],
  cohensd.contr4 = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[4,'contrast'],
  cohensd.contr4.est = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[4,'effect.size'],
  cohensd.contr4.SE = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[4,'SE'],
  cohensd.contr4.lowerCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[4,'lower.CL'],
  cohensd.contr4.upperCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[4,'upper.CL'],
  cohensd.contr5 = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[5,'contrast'],
  cohensd.contr5.est = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[5,'effect.size'],
  cohensd.contr5.SE = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[5,'SE'],
  cohensd.contr5.lowerCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[5,'lower.CL'],
  cohensd.contr5.upperCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[5,'upper.CL'],
  cohensd.contr6 = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[6,'contrast'],
  cohensd.contr6.est = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[6,'effect.size'],
  cohensd.contr6.SE = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[6,'SE'],
  cohensd.contr6.lowerCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[6,'lower.CL'],
  cohensd.contr6.upperCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[6,'upper.CL'],
  
  #Record overall effect of cooccur on outcome
  anova_df = anova(modelx)[1,]$Df,
  anova_F = anova(modelx)[1,]$"F value",
  anova_p = anova(modelx)[1,]$"Pr(>F)")
  
  return(Stats_model)
}

model_bmi_cat_info <- function(modelx){
  #Select only complete cases in the dataset
  mydata <- Sample_main[,all.vars(formula(modelx))]
  mydata <- mydata[complete.cases(mydata),]
  
  #Crosstab of exposure x outcome categories
  source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
  crosstab <- crosstab(mydata, row.vars = all.vars(formula(modelx))[2], col.vars = all.vars(formula(modelx))[1], type = "f")
  
  # Create data frame to store desired model characteristics
  Stats_model = data.frame(vif = as.data.frame(car::vif(modelx))[grep("^cooccur", rownames(as.data.frame(car::vif(modelx))),value=T),'GVIF'],
  N = nrow(mydata),
  n_males = nrow(mydata[mydata$sex == 0,]),
  n_females = nrow(mydata[mydata$sex == 1,]),
  n_cooccur0 = nrow(mydata[mydata$cooccur == 0,]),
  n_cooccur1 = nrow(mydata[mydata$cooccur == 1,]),
  n_cooccur2 = nrow(mydata[mydata$cooccur == 2,]),
  n_cooccur3 = nrow(mydata[mydata$cooccur == 3,]),
  n_bmi_cat1 = nrow(mydata[mydata$bmi_cat == 1,]),
  n_bmi_cat2  = nrow(mydata[mydata$bmi_cat == 2,]),
  n_bmi_cat3  = nrow(mydata[mydata$bmi_cat == 3,]),
  n_bmi_cat4  = nrow(mydata[mydata$bmi_cat == 4,]),
  mean_age = mean(mydata$age,na.rm=T),
  median_age = median(mydata$age,na.rm = T),
  sd_age = sd(mydata$age,na.rm = T),
  n_educ0 = table(mydata$educ)[1],
  n_educ1 = table(mydata$educ)[2],
  n_educ2 = table(mydata$educ)[3],
  n_ethn0 = table(mydata$ethn)[1],
  n_ethn1 = table(mydata$ethn)[2],
  
  n_cooccur0_bmi_cat1 = crosstab$crosstab['0','1'],
  n_cooccur0_bmi_cat2 = crosstab$crosstab['0','2'],
  n_cooccur0_bmi_cat3 = crosstab$crosstab['0','3'],
  n_cooccur0_bmi_cat4 = crosstab$crosstab['0','4'],
  n_cooccur0_bmi_catsum = crosstab$crosstab['0','Sum'],
  
  n_cooccur1_bmi_cat1 = crosstab$crosstab['1','1'],
  n_cooccur1_bmi_cat2 = crosstab$crosstab['1','2'],
  n_cooccur1_bmi_cat3 = crosstab$crosstab['1','3'],
  n_cooccur1_bmi_cat4 = crosstab$crosstab['1','4'],
  n_cooccur1_bmi_catsum = crosstab$crosstab['1','Sum'],
  
  n_cooccur2_bmi_cat1 = crosstab$crosstab['2','1'],
  n_cooccur2_bmi_cat2 = crosstab$crosstab['2','2'],
  n_cooccur2_bmi_cat3 = crosstab$crosstab['2','3'],
  n_cooccur2_bmi_cat4 = crosstab$crosstab['2','4'],
  n_cooccur2_bmi_catsum = crosstab$crosstab['2','Sum'],
  
  n_cooccur3_bmi_cat1 = crosstab$crosstab['3','1'],
  n_cooccur3_bmi_cat2 = crosstab$crosstab['3','2'],
  n_cooccur3_bmi_cat3 = crosstab$crosstab['3','3'],
  n_cooccur3_bmi_cat4 = crosstab$crosstab['3','4'],
  n_cooccur3_bmi_catsum = crosstab$crosstab['3','Sum'],
  
  n_cooccursum_bmi_cat1 = crosstab$crosstab['Sum','1'],
  n_cooccursum_bmi_cat2 = crosstab$crosstab['Sum','2'],
  n_cooccursum_bmi_cat3 = crosstab$crosstab['Sum','3'],
  n_cooccursum_bmi_cat4 = crosstab$crosstab['Sum','4'],
  n_cooccursum_bmi_catsum = crosstab$crosstab['Sum','Sum'],
  
  # Record coefficient estimates of log(OR), standard errors and 95% CI of log(OR) of effects of coccurring MDD and CM on the dependent variable
  estimate_cooccur1_bmi_cat1 = summary(modelx)$coefficients['1','cooccur1'],
  SE_cooccur1_bmi_cat1 = summary(modelx)$standard.errors['1','cooccur1'],
  CI_LL_logOR_cooccur1_bmi_cat1 = confint(modelx, level=0.95)['cooccur1',1,'1'],
  CI_UL_logOR_cooccur1_bmi_cat1 = confint(modelx, level=0.95)['cooccur1',2,'1'],
  
  estimate_cooccur1_bmi_cat3 = summary(modelx)$coefficients['3','cooccur1'],
  SE_cooccur1_bmi_cat3 = summary(modelx)$standard.errors['3','cooccur1'],
  CI_LL_logOR_cooccur1_bmi_cat3 = confint(modelx, level=0.95)['cooccur1',1,'3'],
  CI_UL_logOR_cooccur1_bmi_cat3 = confint(modelx, level=0.95)['cooccur1',2,'3'],
  
  estimate_cooccur1_bmi_cat4 = summary(modelx)$coefficients['4','cooccur1'],
  SE_cooccur1_bmi_cat4 = summary(modelx)$standard.errors['4','cooccur1'],
  CI_LL_logOR_cooccur1_bmi_cat4 = confint(modelx, level=0.95)['cooccur1',1,'4'],
  CI_UL_logOR_cooccur1_bmi_cat4 = confint(modelx, level=0.95)['cooccur1',2,'4'],
  
  estimate_cooccur2_bmi_cat1 = summary(modelx)$coefficients['1','cooccur2'],
  SE_cooccur2_bmi_cat1 = summary(modelx)$standard.errors['1','cooccur2'],
  CI_LL_logOR_cooccur2_bmi_cat1 = confint(modelx, level=0.95)['cooccur2',1,'1'],
  CI_UL_logOR_cooccur2_bmi_cat1 = confint(modelx, level=0.95)['cooccur2',2,'1'],
  
  estimate_cooccur2_bmi_cat3 = summary(modelx)$coefficients['3','cooccur2'],
  SE_cooccur2_bmi_cat3 = summary(modelx)$standard.errors['3','cooccur2'],
  CI_LL_logOR_cooccur2_bmi_cat3 = confint(modelx, level=0.95)['cooccur2',1,'3'],
  CI_UL_logOR_cooccur2_bmi_cat3 = confint(modelx, level=0.95)['cooccur2',2,'3'],
  
  estimate_cooccur2_bmi_cat4 = summary(modelx)$coefficients['4','cooccur2'],
  SE_cooccur2_bmi_cat4 = summary(modelx)$standard.errors['4','cooccur2'],
  CI_LL_logOR_cooccur2_bmi_cat4 = confint(modelx, level=0.95)['cooccur2',1,'4'],
  CI_UL_logOR_cooccur2_bmi_cat4 = confint(modelx, level=0.95)['cooccur2',2,'4'],
  
  estimate_cooccur3_bmi_cat1 = summary(modelx)$coefficients['1','cooccur3'],
  SE_cooccur3_bmi_cat1 = summary(modelx)$standard.errors['1','cooccur3'],
  CI_LL_logOR_cooccur3_bmi_cat1 = confint(modelx, level=0.95)['cooccur3',1,'1'],
  CI_UL_logOR_cooccur3_bmi_cat1 = confint(modelx, level=0.95)['cooccur3',2,'1'],
  
  estimate_cooccur3_bmi_cat3 = summary(modelx)$coefficients['3','cooccur3'],
  SE_cooccur3_bmi_cat3 = summary(modelx)$standard.errors['3','cooccur3'],
  CI_LL_logOR_cooccur3_bmi_cat3 = confint(modelx, level=0.95)['cooccur3',1,'3'],
  CI_UL_logOR_cooccur3_bmi_cat3 = confint(modelx, level=0.95)['cooccur3',2,'3'],
  
  estimate_cooccur3_bmi_cat4 = summary(modelx)$coefficients['4','cooccur3'],
  SE_cooccur3_bmi_cat4 = summary(modelx)$standard.errors['4','cooccur3'],
  CI_LL_logOR_cooccur3_bmi_cat4 = confint(modelx, level=0.95)['cooccur3',1,'4'],
  CI_UL_logOR_cooccur3_bmi_cat4 = confint(modelx, level=0.95)['cooccur3',2,'4'])

  return(Stats_model)
}

models_outcomes_dic_info <- function(modelx){
  #Select only complete cases in the dataset
  mydata <- Sample_main[,all.vars(formula(modelx))]
  mydata <- mydata[complete.cases(mydata),]
 
  #Create a crosstab of exposure x outcome categories
  source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
  crosstab <- crosstab(mydata, row.vars = all.vars(formula(modelx))[2], col.vars = all.vars(formula(modelx))[1], type = "f")
  
  # Create data frame to store desired model characteristics
  Stats_model <- data.frame(vif = as.data.frame(car::vif(modelx))[grep("^cooccur", rownames(as.data.frame(car::vif(modelx))),value=T),'GVIF'],
  N = nrow(mydata),
  n_males = nrow(mydata[mydata$sex == 0,]),
  n_females = nrow(mydata[mydata$sex == 1,]),
  n_cooccur0 = nrow(mydata[mydata$cooccur == 0,]),
  n_cooccur1 = nrow(mydata[mydata$cooccur == 1,]),
  n_cooccur2 = nrow(mydata[mydata$cooccur == 2,]),
  n_cooccur3 = nrow(mydata[mydata$cooccur == 3,]),
  n_outcome_dic0 = nrow(subset(mydata, subset = mydata[,1] == 0)),
  n_outcome_dic1 = nrow(subset(mydata, subset = mydata[,1] == 1)),
  mean_age = mean(mydata$age,na.rm=T),
  median_age = median(mydata$age,na.rm = T),
  sd_age = sd(mydata$age,na.rm = T),
  n_educ0 = table(mydata$educ)[[1]],
  n_educ1 = table(mydata$educ)[[2]],
  n_educ2 = table(mydata$educ)[[3]],
  n_ethn0 = table(mydata$ethn)[[1]],
  n_ethn1 = table(mydata$ethn)[[2]],
  
  n_cooccur0_outcome_dic0 = crosstab$crosstab['0','0'],
  n_cooccur0_outcome_dic1 = crosstab$crosstab['0','1'],
  n_cooccur0_outcome_dicsum = crosstab$crosstab['0','Sum'],
  
  n_cooccur1_outcome_dic0 = crosstab$crosstab['1','0'],
  n_cooccur1_outcome_dic1 = crosstab$crosstab['1','1'],
  n_cooccur1_outcome_dicsum = crosstab$crosstab['1','Sum'],
  
  n_cooccur2_outcome_dic0 = crosstab$crosstab['2','0'],
  n_cooccur2_outcome_dic1 = crosstab$crosstab['2','1'],
  n_cooccur2_outcome_dicsum = crosstab$crosstab['2','Sum'],
  
  n_cooccur3_outcome_dic0 = crosstab$crosstab['3','0'],
  n_cooccur3_outcome_dic1 = crosstab$crosstab['3','1'],
  n_cooccur3_outcome_dicsum = crosstab$crosstab['3','Sum'],
  
  n_cooccursum_outcome_dic0 = crosstab$crosstab['Sum','0'],
  n_cooccursum_outcome_dic1 = crosstab$crosstab['Sum','1'],
  n_cooccursum_outcome_dicsum = crosstab$crosstab['Sum','Sum'],
  
  # Record coefficient estimates of log(OR), standard errors and 95% CI of log(OR) of effects of coccurring MDD and CM on the dependent variable
  estimate_cooccur1_outcome_dic = summary(modelx)$coefficients['cooccur1', 'Estimate'],
  SE_cooccur1_outcome_dic = summary(modelx)$coefficients['cooccur1', 'Std. Error'],
  CI_LL_logOR_cooccur1_outcome_dic = confint(modelx, level=0.95)['cooccur1',1],
  CI_UL_logOR_cooccur1_outcome_dic = confint(modelx, level=0.95)['cooccur1',2],
  
  estimate_cooccur2_outcome_dic = summary(modelx)$coefficients['cooccur2', 'Estimate'],
  SE_cooccur2_outcome_dic = summary(modelx)$coefficients['cooccur1', 'Std. Error'],
  CI_LL_logOR_cooccur2_outcome_dic = confint(modelx, level=0.95)['cooccur2',1],
  CI_UL_logOR_cooccur2_outcome_dic = confint(modelx, level=0.95)['cooccur2',2],
  
  estimate_cooccur3_outcome_dic = summary(modelx)$coefficients['cooccur3', 'Estimate'],
  SE_cooccur3_outcome_dic = summary(modelx)$coefficients['cooccur3', 'Std. Error'],
  CI_LL_logOR_cooccur3_outcome_dic = confint(modelx, level=0.95)['cooccur3',1],
  CI_UL_logOR_cooccur3_outcome_dic = confint(modelx, level=0.95)['cooccur3',2])

  return(Stats_model)
}

models_lifestyle_info <- function(modelx, outcome_quotes){
  # Select only complete cases in the dataset
  mydata <- Sample_main[,all.vars(formula(modelx))]
  mydata <- mydata[complete.cases(mydata),]
  
  # Model to estimate means
  model.emm <- emmeans(modelx, pairwise ~ cooccur)
  
  model.data <- augment(modelx) %>% 
    dplyr::mutate(index = 1:n())
  
  # Create data frame to store desired model characteristics
  Stats_model = data.frame(durbinWatson_autocorrelation = durbinWatsonTest(modelx)$r,       #Independence of residuals/error terms
  durbinWatson_statistic = durbinWatsonTest(modelx)$dw,
  durbinWatson_p = durbinWatsonTest(modelx)$p,
  vif = car::vif(modelx)[1,'GVIF'],
  N = nobs(modelx),
  n_males = nrow(mydata[mydata$sex == 0,]),
  n_females = nrow(mydata[mydata$sex == 1,]),
  n_cooccur0 = nrow(mydata[mydata$cooccur == 0,]),
  n_cooccur1 = nrow(mydata[mydata$cooccur == 1,]),
  n_cooccur2 = nrow(mydata[mydata$cooccur == 2,]),
  n_cooccur3 = nrow(mydata[mydata$cooccur == 3,]),
  mean_age = mean(mydata$age,na.rm=T),
  median_age = median(mydata$age,na.rm = T),
  sd_age = sd(mydata$age,na.rm = T),
  n_educ0 = table(mydata$educ)[1],
  n_educ1 = table(mydata$educ)[2],
  n_educ2 = table(mydata$educ)[3],
  n_ethn0 = table(mydata$ethn)[1],
  n_ethn1 = table(mydata$ethn)[2],
  mean_phyact = mean(mydata$phyact, na.rm =T),
  median_phyact = ifelse(sum(all.vars(formula(modelx)) == "phyact") == 1, median(mydata$phyact, na.rm = T), NA),
  sd_phyact = sd(mydata$phyact, na.rm = T),
  n_smoke = nrow(mydata[mydata$smoke == 1,]),
  mean_alc = mean(mydata$alcohol, na.rm =T),
  median_alc = ifelse(sum(all.vars(formula(modelx)) == "alcohol") == 1, median(mydata$alcohol, na.rm = T), NA),
  sd_alc = sd(mydata$alcohol, na.rm = T),
  
  cooccur0_mean_outcome = mean(subset(mydata, cooccur == 0, select = outcome_quotes)[,1], na.rm=T),
  cooccur0_median_outcome = median(subset(mydata, cooccur == 0, select = outcome_quotes)[,1], na.rm=T),
  cooccur0_sd_outcome = sd(subset(mydata, cooccur == 0, select = outcome_quotes)[,1], na.rm=T),
  cooccur1_mean_outcome = mean(subset(mydata, cooccur == 1, select = outcome_quotes)[,1], na.rm=T),
  cooccur1_median_outcome = median(subset(mydata, cooccur == 1, select = outcome_quotes)[,1], na.rm=T),
  cooccur1_sd_outcome = sd(subset(mydata, cooccur == 1, select = outcome_quotes)[,1], na.rm=T),
  cooccur2_mean_outcome = mean(subset(mydata, cooccur == 2, select = outcome_quotes)[,1], na.rm=T),
  cooccur2_median_outcome = median(subset(mydata, cooccur == 2, select = outcome_quotes)[,1], na.rm=T),
  cooccur2_sd_outcome = sd(subset(mydata, cooccur == 2, select = outcome_quotes)[,1], na.rm=T),
  cooccur3_mean_outcome = mean(subset(mydata, cooccur == 3, select = outcome_quotes)[,1], na.rm=T),
  cooccur3_median_outcome = median(subset(mydata, cooccur == 3, select = outcome_quotes)[,1], na.rm=T),
  cooccur3_sd_outcome = sd(subset(mydata, cooccur == 3, select = outcome_quotes)[,1], na.rm=T),
  mean_outcome = mean(mydata[,outcome_quotes], na.rm=T),
  median_outcome = median(mydata[,outcome_quotes], na.rm=T),
  sd_outcome = sd(mydata[,outcome_quotes], na.rm=T),
  
  # Record coefficient estimates of log(OR), standard errors and 95% CI of log(OR) of cms' effects on the dependent variable
  Estimate_cooccur1 = summary(modelx)$coefficients['cooccur1','Estimate'],
  Std.Error_cooccur1 = summary(modelx)$coefficients['cooccur1','Std. Error'],
  p_cooccur1 = summary(modelx)$coefficients['cooccur1','Pr(>|t|)'],
  Estimate_cooccur2 = summary(modelx)$coefficients['cooccur2','Estimate'],
  Std.Error_cooccur2 = summary(modelx)$coefficients['cooccur2','Std. Error'],
  p_cooccur2 = summary(modelx)$coefficients['cooccur2','Pr(>|t|)'],
  Estimate_cooccur3 = summary(modelx)$coefficients['cooccur3','Estimate'],
  Std.Error_cooccur3 = summary(modelx)$coefficients['cooccur3','Std. Error'],
  p_cooccur3 = summary(modelx)$coefficients['cooccur3','Pr(>|t|)'],
  Estimate_age = summary(modelx)$coefficients['age','Estimate'],
  Std.Error_age = summary(modelx)$coefficients['age','Std. Error'],
  p_age = summary(modelx)$coefficients['age','Pr(>|t|)'],
  Estimate_sex1 = summary(modelx)$coefficients['sex1','Estimate'],
  Std.Error_sex1 = summary(modelx)$coefficients['sex1','Std. Error'],
  p_sex1 = summary(modelx)$coefficients['sex1','Pr(>|t|)'],
  Estimate_educ1 = summary(modelx)$coefficients['educ1','Estimate'],
  Std.Error_educ1 = summary(modelx)$coefficients['educ1','Std. Error'],
  p_educ1 = summary(modelx)$coefficients['educ1','Pr(>|t|)'],
  Estimate_educ2 = summary(modelx)$coefficients['educ2','Estimate'],
  Std.Error_educ2 = summary(modelx)$coefficients['educ2','Std. Error'],
  p_educ2 = summary(modelx)$coefficients['educ2','Pr(>|t|)'],
  Estimate_ethn = summary(modelx)$coefficients['ethn1','Estimate'],
  Std.Error_ethn = summary(modelx)$coefficients['ethn1','Std. Error'],
  p_ethn = summary(modelx)$coefficients['ethn1','Pr(>|t|)'],
  Estimate_phyact = summary(modelx)$coefficients['phyact','Estimate'],
  Std.Error_phyact = summary(modelx)$coefficients['phyact','Std. Error'],
  p_phyact = summary(modelx)$coefficients['phyact','Pr(>|t|)'],
  Estimate_alcohol = summary(modelx)$coefficients['alcohol','Estimate'],
  Std.Error_alcohol = summary(modelx)$coefficients['alcohol','Std. Error'],
  p_alcohol = summary(modelx)$coefficients['alcohol','Pr(>|t|)'],
  Estimate_smoke1 = summary(modelx)$coefficients['smoke1','Estimate'],
  Std.Error_smoke1 = summary(modelx)$coefficients['smoke1','Std. Error'],
  p_smoke1 = summary(modelx)$coefficients['smoke1','Pr(>|t|)'],
  
  # Estimated mean of each group and their SE's
  emm_cooccur0 = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cooccur']=='0','emmean'],
  emm_cooccur1 = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cooccur']=='1','emmean'],
  emm_cooccur2 = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cooccur']=='2','emmean'],
  emm_cooccur3 = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cooccur']=='3','emmean'],
  emm.SE_cooccur0  = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cooccur']=='0','SE'],
  emm.SE_cooccur1  = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cooccur']=='1','SE'],
  emm.SE_cooccur2  = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cooccur']=='2','SE'],
  emm.SE_cooccur3  = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cooccur']=='3','SE'],
  
  # Contrasts of estimated means
  emm.contr1 = as.data.frame(pairs(model.emm$emmeans, reverse=T))[1,'contrast'],
  emm.contr1.est = as.data.frame(pairs(model.emm$emmeans, reverse=T))[1,'estimate'],
  emm.contr1.SE = as.data.frame(pairs(model.emm$emmeans, reverse=T))[1,'SE'],
  emm.contr1.p = as.data.frame(pairs(model.emm$emmeans, reverse=T))[1,'p.value'],
  emm.contr2 = as.data.frame(pairs(model.emm$emmeans, reverse=T))[2,'contrast'],
  emm.contr2.est = as.data.frame(pairs(model.emm$emmeans, reverse=T))[2,'estimate'],
  emm.contr2.SE = as.data.frame(pairs(model.emm$emmeans, reverse=T))[2,'SE'],
  emm.contr2.p = as.data.frame(pairs(model.emm$emmeans, reverse=T))[2,'p.value'],
  emm.contr3 = as.data.frame(pairs(model.emm$emmeans, reverse=T))[3,'contrast'],
  emm.contr3.est = as.data.frame(pairs(model.emm$emmeans, reverse=T))[3,'estimate'],
  emm.contr3.SE = as.data.frame(pairs(model.emm$emmeans, reverse=T))[3,'SE'],
  emm.contr3.p = as.data.frame(pairs(model.emm$emmeans, reverse=T))[3,'p.value'],
  emm.contr4 = as.data.frame(pairs(model.emm$emmeans, reverse=T))[4,'contrast'],
  emm.contr4.est = as.data.frame(pairs(model.emm$emmeans, reverse=T))[4,'estimate'],
  emm.contr4.SE = as.data.frame(pairs(model.emm$emmeans, reverse=T))[4,'SE'],
  emm.contr4.p = as.data.frame(pairs(model.emm$emmeans, reverse=T))[4,'p.value'],
  emm.contr5 = as.data.frame(pairs(model.emm$emmeans, reverse=T))[5,'contrast'],
  emm.contr5.est = as.data.frame(pairs(model.emm$emmeans, reverse=T))[5,'estimate'],
  emm.contr5.SE = as.data.frame(pairs(model.emm$emmeans, reverse=T))[5,'SE'],
  emm.contr5.p = as.data.frame(pairs(model.emm$emmeans, reverse=T))[5,'p.value'],
  emm.contr6 = as.data.frame(pairs(model.emm$emmeans, reverse=T))[6,'contrast'],
  emm.contr6.est = as.data.frame(pairs(model.emm$emmeans, reverse=T))[6,'estimate'],
  emm.contr6.SE = as.data.frame(pairs(model.emm$emmeans, reverse=T))[6,'SE'],
  emm.contr6.p = as.data.frame(pairs(model.emm$emmeans, reverse=T))[6,'p.value'],
  
  # Standardized mean difference (cohen's d)
  cohensd.contr1 = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[1,'contrast'],
  cohensd.contr1.est = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[1,'effect.size'],
  cohensd.contr1.SE = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[1,'SE'],
  cohensd.contr1.lowerCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[1,'lower.CL'],
  cohensd.contr1.upperCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[1,'upper.CL'],
  cohensd.contr2 = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[2,'contrast'],
  cohensd.contr2.est = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[2,'effect.size'],
  cohensd.contr2.SE = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[2,'SE'],
  cohensd.contr2.lowerCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[2,'lower.CL'],
  cohensd.contr2.upperCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[2,'upper.CL'],
  cohensd.contr3 = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[3,'contrast'],
  cohensd.contr3.est = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[3,'effect.size'],
  cohensd.contr3.SE = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[3,'SE'],
  cohensd.contr3.lowerCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[3,'lower.CL'],
  cohensd.contr3.upperCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[3,'upper.CL'],
  cohensd.contr4 = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[4,'contrast'],
  cohensd.contr4.est = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[4,'effect.size'],
  cohensd.contr4.SE = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[4,'SE'],
  cohensd.contr4.lowerCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[4,'lower.CL'],
  cohensd.contr4.upperCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[4,'upper.CL'],
  cohensd.contr5 = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[5,'contrast'],
  cohensd.contr5.est = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[5,'effect.size'],
  cohensd.contr5.SE = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[5,'SE'],
  cohensd.contr5.lowerCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[5,'lower.CL'],
  cohensd.contr5.upperCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[5,'upper.CL'],
  cohensd.contr6 = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[6,'contrast'],
  cohensd.contr6.est = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[6,'effect.size'],
  cohensd.contr6.SE = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[6,'SE'],
  cohensd.contr6.lowerCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[6,'lower.CL'],
  cohensd.contr6.upperCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[6,'upper.CL'],
  
  # Record overall effect of cooccur on outcome
  anova_df = anova(modelx)[1,]$Df,
  anova_F = anova(modelx)[1,]$"F value",
  anova_p = anova(modelx)[1,]$"Pr(>F)")
  
  return(Stats_model)
}

models_sex_strat_info <- function(modelx, outcome_quotes){
  # Select only complete cases in the dataset
  mydata <- Sample_main[,all.vars(formula(modelx))]
  mydata <- mydata[complete.cases(mydata),]
  
  # Model to estimate means
  model.emm <- emmeans(modelx, pairwise ~ cooccur)
  
  model.data <- augment(modelx) %>% 
    dplyr::mutate(index = 1:n())
 
  # Create data frame to store desired model characteristics
  Stats_model <- data.frame(durbinWatson_autocorrelation = durbinWatsonTest(modelx)$r,       #Independence of residuals/error terms
  durbinWatson_statistic = durbinWatsonTest(modelx)$dw,
  durbinWatson_p = durbinWatsonTest(modelx)$p,
  vif = car::vif(modelx)[1,'GVIF'],
  N = nobs(modelx),
  n_cooccur0 = nrow(mydata[mydata$cooccur == 0,]),
  n_cooccur1 = nrow(mydata[mydata$cooccur == 1,]),
  n_cooccur2 = nrow(mydata[mydata$cooccur == 2,]),
  n_cooccur3 = nrow(mydata[mydata$cooccur == 3,]),
  mean_age = mean(mydata$age,na.rm=T),
  median_age = median(mydata$age,na.rm = T),
  sd_age = sd(mydata$age,na.rm = T),
  n_educ0 = table(mydata$educ)[1],
  n_educ1 = table(mydata$educ)[2],
  n_educ2 = table(mydata$educ)[3],
  n_ethn0 = table(mydata$ethn)[1],
  n_ethn1 = table(mydata$ethn)[2],
  cooccur0_mean_outcome = mean(subset(mydata, cooccur == 0, select = outcome_quotes)[,1], na.rm=T),
  cooccur0_median_outcome = median(subset(mydata, cooccur == 0, select = outcome_quotes)[,1], na.rm=T),
  cooccur0_sd_outcome = sd(subset(mydata, cooccur == 0, select = outcome_quotes)[,1], na.rm=T),
  cooccur1_mean_outcome = mean(subset(mydata, cooccur == 1, select = outcome_quotes)[,1], na.rm=T),
  cooccur1_median_outcome = median(subset(mydata, cooccur == 1, select = outcome_quotes)[,1], na.rm=T),
  cooccur1_sd_outcome = sd(subset(mydata, cooccur == 1, select = outcome_quotes)[,1], na.rm=T),
  cooccur2_mean_outcome = mean(subset(mydata, cooccur == 2, select = outcome_quotes)[,1], na.rm=T),
  cooccur2_median_outcome = median(subset(mydata, cooccur == 2, select = outcome_quotes)[,1], na.rm=T),
  cooccur2_sd_outcome = sd(subset(mydata, cooccur == 2, select = outcome_quotes)[,1], na.rm=T),
  cooccur3_mean_outcome = mean(subset(mydata, cooccur == 3, select = outcome_quotes)[,1], na.rm=T),
  cooccur3_median_outcome = median(subset(mydata, cooccur == 3, select = outcome_quotes)[,1], na.rm=T),
  cooccur3_sd_outcome = sd(subset(mydata, cooccur == 3, select = outcome_quotes)[,1], na.rm=T),
  mean_outcome = mean(mydata[,outcome_quotes], na.rm=T),
  median_outcome = median(mydata[,outcome_quotes], na.rm=T),
  sd_outcome = sd(mydata[,outcome_quotes], na.rm=T),
  
  # Record coefficient estimates of log(OR), standard errors and 95% CI of log(OR) of cms' effects on the dependent variable
  Estimate_cooccur1 = summary(modelx)$coefficients['cooccur1','Estimate'],
  Std.Error_cooccur1 = summary(modelx)$coefficients['cooccur1','Std. Error'],
  p_cooccur1 = summary(modelx)$coefficients['cooccur1','Pr(>|t|)'],
  Estimate_cooccur2 = summary(modelx)$coefficients['cooccur2','Estimate'],
  Std.Error_cooccur2 = summary(modelx)$coefficients['cooccur2','Std. Error'],
  p_cooccur2 = summary(modelx)$coefficients['cooccur2','Pr(>|t|)'],
  Estimate_cooccur3 = summary(modelx)$coefficients['cooccur3','Estimate'],
  Std.Error_cooccur3 = summary(modelx)$coefficients['cooccur3','Std. Error'],
  p_cooccur3 = summary(modelx)$coefficients['cooccur3','Pr(>|t|)'],
  Estimate_age = summary(modelx)$coefficients['age','Estimate'],
  Std.Error_age = summary(modelx)$coefficients['age','Std. Error'],
  p_age = summary(modelx)$coefficients['age','Pr(>|t|)'],
  Estimate_educ1 = summary(modelx)$coefficients['educ1','Estimate'],
  Std.Error_educ1 = summary(modelx)$coefficients['educ1','Std. Error'],
  p_educ1 = summary(modelx)$coefficients['educ1','Pr(>|t|)'],
  Estimate_educ2 = summary(modelx)$coefficients['educ2','Estimate'],
  Std.Error_educ2 = summary(modelx)$coefficients['educ2','Std. Error'],
  p_educ2 = summary(modelx)$coefficients['educ2','Pr(>|t|)'],
  Estimate_ethn = summary(modelx)$coefficients['ethn1','Estimate'],
  Std.Error_ethn = summary(modelx)$coefficients['ethn1','Std. Error'],
  p_ethn = summary(modelx)$coefficients['ethn1','Pr(>|t|)'],
  
  # Estimated mean of each group and their SE's
  emm_cooccur0 = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cooccur']=='0','emmean'],
  emm_cooccur1 = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cooccur']=='1','emmean'],
  emm_cooccur2 = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cooccur']=='2','emmean'],
  emm_cooccur3 = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cooccur']=='3','emmean'],
  emm.SE_cooccur0  = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cooccur']=='0','SE'],
  emm.SE_cooccur1  = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cooccur']=='1','SE'],
  emm.SE_cooccur2  = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cooccur']=='2','SE'],
  emm.SE_cooccur3  = as.data.frame(model.emm$emmeans)[as.data.frame(model.emm$emmeans)['cooccur']=='3','SE'],
  
  # Contrasts of estimated means
  emm.contr1 = as.data.frame(pairs(model.emm$emmeans, reverse=T))[1,'contrast'],
  emm.contr1.est = as.data.frame(pairs(model.emm$emmeans, reverse=T))[1,'estimate'],
  emm.contr1.SE = as.data.frame(pairs(model.emm$emmeans, reverse=T))[1,'SE'],
  emm.contr1.p = as.data.frame(pairs(model.emm$emmeans, reverse=T))[1,'p.value'],
  emm.contr2 = as.data.frame(pairs(model.emm$emmeans, reverse=T))[2,'contrast'],
  emm.contr2.est = as.data.frame(pairs(model.emm$emmeans, reverse=T))[2,'estimate'],
  emm.contr2.SE = as.data.frame(pairs(model.emm$emmeans, reverse=T))[2,'SE'],
  emm.contr2.p = as.data.frame(pairs(model.emm$emmeans, reverse=T))[2,'p.value'],
  emm.contr3 = as.data.frame(pairs(model.emm$emmeans, reverse=T))[3,'contrast'],
  emm.contr3.est = as.data.frame(pairs(model.emm$emmeans, reverse=T))[3,'estimate'],
  emm.contr3.SE = as.data.frame(pairs(model.emm$emmeans, reverse=T))[3,'SE'],
  emm.contr3.p = as.data.frame(pairs(model.emm$emmeans, reverse=T))[3,'p.value'],
  emm.contr4 = as.data.frame(pairs(model.emm$emmeans, reverse=T))[4,'contrast'],
  emm.contr4.est = as.data.frame(pairs(model.emm$emmeans, reverse=T))[4,'estimate'],
  emm.contr4.SE = as.data.frame(pairs(model.emm$emmeans, reverse=T))[4,'SE'],
  emm.contr4.p = as.data.frame(pairs(model.emm$emmeans, reverse=T))[4,'p.value'],
  emm.contr5 = as.data.frame(pairs(model.emm$emmeans, reverse=T))[5,'contrast'],
  emm.contr5.est = as.data.frame(pairs(model.emm$emmeans, reverse=T))[5,'estimate'],
  emm.contr5.SE = as.data.frame(pairs(model.emm$emmeans, reverse=T))[5,'SE'],
  emm.contr5.p = as.data.frame(pairs(model.emm$emmeans, reverse=T))[5,'p.value'],
  emm.contr6 = as.data.frame(pairs(model.emm$emmeans, reverse=T))[6,'contrast'],
  emm.contr6.est = as.data.frame(pairs(model.emm$emmeans, reverse=T))[6,'estimate'],
  emm.contr6.SE = as.data.frame(pairs(model.emm$emmeans, reverse=T))[6,'SE'],
  emm.contr6.p = as.data.frame(pairs(model.emm$emmeans, reverse=T))[6,'p.value'],
  
  # Standardized mean difference (cohen's d)
  cohensd.contr1 = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[1,'contrast'],
  cohensd.contr1.est = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[1,'effect.size'],
  cohensd.contr1.SE = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[1,'SE'],
  cohensd.contr1.lowerCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[1,'lower.CL'],
  cohensd.contr1.upperCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[1,'upper.CL'],
  cohensd.contr2 = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[2,'contrast'],
  cohensd.contr2.est = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[2,'effect.size'],
  cohensd.contr2.SE = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[2,'SE'],
  cohensd.contr2.lowerCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[2,'lower.CL'],
  cohensd.contr2.upperCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[2,'upper.CL'],
  cohensd.contr3 = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[3,'contrast'],
  cohensd.contr3.est = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[3,'effect.size'],
  cohensd.contr3.SE = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[3,'SE'],
  cohensd.contr3.lowerCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[3,'lower.CL'],
  cohensd.contr3.upperCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[3,'upper.CL'],
  cohensd.contr4 = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[4,'contrast'],
  cohensd.contr4.est = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[4,'effect.size'],
  cohensd.contr4.SE = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[4,'SE'],
  cohensd.contr4.lowerCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[4,'lower.CL'],
  cohensd.contr4.upperCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[4,'upper.CL'],
  cohensd.contr5 = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[5,'contrast'],
  cohensd.contr5.est = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[5,'effect.size'],
  cohensd.contr5.SE = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[5,'SE'],
  cohensd.contr5.lowerCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[5,'lower.CL'],
  cohensd.contr5.upperCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[5,'upper.CL'],
  cohensd.contr6 = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[6,'contrast'],
  cohensd.contr6.est = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[6,'effect.size'],
  cohensd.contr6.SE = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[6,'SE'],
  cohensd.contr6.lowerCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[6,'lower.CL'],
  cohensd.contr6.upperCL = as.data.frame(eff_size(model.emm, sigma = sigma(modelx), edf = df.residual(modelx)))[6,'upper.CL'],
  
  # Record overall effect of cooccur on outcome
  anova_df = anova(modelx)[1,]$Df,
  anova_F = anova(modelx)[1,]$"F value",
  anova_p = anova(modelx)[1,]$"Pr(>F)")
  
  return(Stats_model)
}



#################### 3. Statistical analyses and assumption checks #################### 

### 3.1. Models 1, 2, 3, and 4 - Association of childhood maltreatment with continuous metabolic outcomes
Sample_main$cm <- relevel(Sample_main$cm, ref='0')
Sample_main$sex <- relevel(Sample_main$sex, ref='0')
Sample_main$educ <- relevel(Sample_main$educ, ref='0')
Sample_main$ethn <- relevel(Sample_main$ethn, ref='0')

model1 <- lm(bmi ~ cm + sex + age + educ + ethn,  data = Sample_main)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity) 
plot(model1, c(1,2,3))
# Extract summary statistics of model
results_model1 <- models_main_cm_info(model1, 'bmi')
write.csv(results_model1,"~\cohort1_model1_date.csv", row.names = TRUE)    #Change path to working directory and re-name file with cohort's name and date

model2 <- lm(waisthip_rat ~ cm + sex + age + educ + ethn,  data = Sample_main)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity) 
plot(model2, c(1,2,3))
# Extract summary statistics of model
results_model2 <- models_main_cm_info(model2, 'waisthip_rat')
write.csv(results_model2,"~\cohort1_model2_date.csv", row.names = TRUE) 

model3 <- lm(ldlhdl_rat ~ cm + sex + age + educ + ethn,  data = Sample_main)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity) 
plot(model3, c(1,2,3))
# Extract summary statistics of model
results_model3 <- models_main_cm_info(model3, 'ldlhdl_rat')
write.csv(results_model3,"~\cohort1_model3_date.csv", row.names = TRUE) 

model4 <- lm(trig ~ cm + sex + age + educ + ethn,  data = Sample_main)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity) 
plot(model4, c(1,2,3))
# Extract summary statistics of model
results_model4 <- models_main_cm_info(model4, 'trig')
write.csv(results_model4,"~\cohort1_model4_date.csv", row.names = TRUE) 

### 3.2. Models 5, 6, 7, and 8 - Association of depression with continuous metabolic outcomes
Sample_main$mdd <- relevel(Sample_main$mdd, ref='0')

model5 <- lm(bmi ~ mdd + sex + age + educ + ethn,  data = Sample_main)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity) 
plot(model5, c(1,2,3))
# Extract summary statistics of model
results_model5 <- models_main_mdd_info(model9, 'bmi')
write.csv(results_model5,"~\cohort1_model5_date.csv", row.names = TRUE) 

model6 <- lm(waisthip_rat ~ mdd + sex + age + educ + ethn,  data = Sample_main)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity) 
plot(model6, c(1,2,3))
# Extract summary statistics of model
results_model6 <- models_main_mdd_info(model6, 'waisthip_rat')
write.csv(results_model6,"~\cohort1_model6_date.csv", row.names = TRUE) 

model7 <- lm(ldlhdl_rat ~ mdd + sex + age + educ + ethn,  data = Sample_main)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity) 
plot(model7, c(1,2,3))
# Extract summary statistics of model
results_model7 <- models_main_mdd_info(model7, 'ldlhdl_rat')
write.csv(results_model7,"~\cohort1_model7_date.csv", row.names = TRUE) 

model8 <- lm(trig ~ mdd + sex + age + educ + ethn,  data = Sample_main)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity) 
plot(model8, c(1,2,3))
# Extract summary statistics of model
results_model8 <- models_main_mdd_info(model8, 'trig')
write.csv(results_model8,"~\cohort1_model8_date.csv", row.names = TRUE) 

### 3.3. Models 9, 10, 11, and 12 - Association of co-occurrence of maltreatment and depression with continuous metabolic outcomes
Sample_main$cooccur <- relevel(Sample_main$cooccur, ref='0')

model9 <- lm(bmi ~ cooccur + sex + age + educ + ethn,  data = Sample_main)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity) 
plot(model9, c(1,2,3))
# Extract summary statistics of model
results_model9 <- models_cooccur_info(model9, 'bmi')
write.csv(results_model9,"~\cohort1_model9_date.csv", row.names = TRUE) 

model10 <- lm(waisthip_rat ~ cooccur + sex + age + educ + ethn,  data = Sample_main)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity) 
plot(model10, c(1,2,3))
# Extract summary statistics of model
results_model10 <- models_cooccur_info(model10, 'waisthip_rat')
write.csv(results_model10,"~\cohort1_model10_date.csv", row.names = TRUE) 

model11 <- lm(ldlhdl_rat ~ cooccur + sex + age + educ + ethn,  data = Sample_main)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity) 
plot(model11, c(1,2,3))
# Extract summary statistics of model
results_model11 <- models_cooccur_info(model11, 'ldlhdl_rat')
write.csv(results_model11,"~\cohort1_model11_date.csv", row.names = TRUE) 

model12 <- lm(trig ~ cooccur + sex + age + educ + ethn,  data = Sample_main)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity) 
plot(model12, c(1,2,3))
# Extract summary statistics of model
results_model12 <- models_cooccur_info(model12, 'trig')
write.csv(results_model12,"~\cohort1_model12_date.csv", row.names = TRUE) 

### 3.4. Models 13, 14, 15, and 16 - Association of co-occurrence of maltreatment and depression with categorical metabolic outcomes
Sample_main$cooccur <- relevel(Sample_main$cooccur, ref='0')
Sample_main$bmi_cat <- relevel(Sample_main$bmi_cat, ref='2')
Sample_main$waisthip_rat_dic <- relevel(Sample_main$waisthip_rat_dic, ref='0')
Sample_main$trig_dic <- relevel(Sample_main$trig_dic, ref='0')
Sample_main$ldlhdl_rat_dic <- relevel(Sample_main$ldlhdl_rat_dic, ref='0')

model13 <- multinom(bmi_cat ~ cooccur + sex + age + educ + ethn,  data = Sample_main)
# Linearity assumption
probabilities13 <- predict(model13, type = "prob", na.rm=T)
probablities13_bmi_cat1 <- probabilities13[,'1']
probablities13_bmi_cat2 <- probabilities13[,'2']
probablities13_bmi_cat3 <- probabilities13[,'3']
probablities13_bmi_cat4 <- probabilities13[,'4']
mydata13 <- Sample_main[,c('bmi_cat','cooccur','age','sex','educ','ethn')]
mydata13 <- mydata13[complete.cases(mydata13),]
mydata13_num <- mydata13 %>% dplyr::select_if(is.numeric)
predictors13 <- colnames(mydata13_num)

mydata13_bmi_cat1_age_num <- data.frame(mydata13_num[,'age']) %>%
  mutate(logit = log(probablities13_bmi_cat1/(1-probablities13_bmi_cat1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata13_bmi_cat2_age_num <- data.frame(mydata13_num[,'age']) %>%
  mutate(logit = log(probablities13_bmi_cat2/(1-probablities13_bmi_cat2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata13_bmi_cat3_age_num <- data.frame(mydata13_num[,'age']) %>%
  mutate(logit = log(probablities13_bmi_cat3/(1-probablities13_bmi_cat3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata13_bmi_cat4_age_num <- data.frame(mydata13_num[,'age']) %>%
  mutate(logit = log(probablities13_bmi_cat4/(1-probablities13_bmi_cat4))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata13_bmi_cat1_age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at bmi_cat = 1, Model #13")
ggplot(mydata13_bmi_cat2_age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at bmi_cat = 2, Model #13")
ggplot(mydata13_bmi_cat3_age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at bmi_cat = 3, Model #13")
ggplot(mydata13_bmi_cat4_age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at bmi_cat = 4, Model #13")

# Extract summary statistics of model
results_model13 <- model_bmi_cat_info(model13)
write.csv(results_model13,"~\cohort1_model13_date.csv", row.names = TRUE) 

model14 <- glm(waisthip_rat_dic ~ cooccur + sex + age + educ + ethn, family = binomial(link='logit'), data = Sample_main)
# Linearity assumption
probabilities14 <- predict(model14, type = "response", na.rm=T)
probablities14_waisthip_rat_dic1 <- probabilities14
mydata14 <- Sample_main[,c('waisthip_rat_dic','cooccur','age','sex','educ','ethn')]
mydata14 <- mydata14[complete.cases(mydata14),]
mydata14_num <- mydata14 %>% dplyr::select_if(is.numeric)
predictors14 <- colnames(mydata14_num)

mydata14_waisthip_rat_dic1_age_num <- data.frame(mydata14_num[,'age']) %>%
  mutate(logit = log(probablities14_waisthip_rat_dic1/(1-probablities14_waisthip_rat_dic1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata14_waisthip_rat_dic1_age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at waisthip_rat_dic = 1, Model #14")

# Extract summary statistics of model
results_model14 <- models_outcomes_dic_info(model14)
write.csv(results_model14,"~\cohort1_model14_date.csv", row.names = TRUE) 

model15 <- glm(trig_dic ~ cooccur + sex + age + educ + ethn, family = binomial(link='logit'), data = Sample_main)
# Linearity assumption
probabilities15 <- predict(model15, type = "response", na.rm=T)
probablities15_trig_dic1 <- probabilities15
mydata15 <- Sample_main[,c('trig_dic','cooccur','age','sex','educ','ethn')]
mydata15 <- mydata15[complete.cases(mydata15),]
mydata15_num <- mydata15 %>% dplyr::select_if(is.numeric)
predictors15 <- colnames(mydata15_num)

mydata15_trig_dic1_age_num <- data.frame(mydata15_num[,'age']) %>%
  mutate(logit = log(probablities15_trig_dic1/(1-probablities15_trig_dic1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata15_trig_dic1_age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at trig_dic = 1, Model #15")

# Extract summary statistics of model
results_model15 <- models_outcomes_dic_info(model15)
write.csv(results_model15,"~\cohort1_model15_date.csv", row.names = TRUE)

model16 <- glm(ldlhdl_rat_dic ~ cooccur + sex + age + educ + ethn, family = binomial(link='logit'), data = Sample_main)
# Linearity assumption
probabilities16 <- predict(model16, type = "response", na.rm=T)
probablities16_ldlhdl_rat_dic1 <- probabilities16
mydata16 <- Sample_main[,c('ldlhdl_rat_dic','cooccur','age','sex','educ','ethn')]
mydata16 <- mydata16[complete.cases(mydata16),]
mydata16_num <- mydata16 %>% dplyr::select_if(is.numeric)
predictors16 <- colnames(mydata16_num)

mydata16_ldlhdl_rat_dic1_age_num <- data.frame(mydata16_num[,'age']) %>%
  mutate(logit = log(probablities16_ldlhdl_rat_dic1/(1-probablities16_ldlhdl_rat_dic1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata16_ldlhdl_rat_dic1_age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at ldlhdl_rat_dic = 1, Model #16")

# Extract summary statistics of model
results_model16 <- models_outcomes_dic_info(model16)
write.csv(results_model16,"~\cohort1_model16_date.csv", row.names = TRUE) 

### 3.5. Models 17, 18, 19, and 20 - Lifestyle adjustment
Sample_main$cooccur <- relevel(Sample_main$cooccur, ref='0')

model17 <- lm(bmi ~ cooccur + sex + age + educ + ethn + smoke + alcohol + phyact, data = Sample_main)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity) 
plot(model17, c(1,2,3))
# Extract summary statistics of model
results_model17 <- models_lifestyle_info(model17, 'bmi')
write.csv(results_model17,"~\cohort1_model17_date.csv", row.names = TRUE) 

model18 <- lm(waisthip_rat ~ cooccur + sex + age + educ + ethn + smoke + alcohol + phyact, data = Sample_main)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity) 
plot(model18, c(1,2,3))
# Extract summary statistics of model
results_model18 <- models_lifestyle_info(model18, 'waisthip_rat')
write.csv(results_model18,"~\cohort1_model18_date.csv", row.names = TRUE) 

model19 <- lm(trig ~ cooccur + sex + age + educ + ethn + smoke + alcohol + phyact, data = Sample_main)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity) 
plot(model19, c(1,2,3))
# Extract summary statistics of model
results_model19 <- models_lifestyle_info(model19, 'trig')
write.csv(results_model19,"~\cohort1_model19_date.csv", row.names = TRUE) 

model20 <- lm(ldlhdl_rat ~ cooccur + sex + age + educ + ethn + smoke + alcohol + phyact, data = Sample_main)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity) 
plot(model20, c(1,2,3))
# Extract summary statistics of model
results_model20 <- models_lifestyle_info(model20, 'ldlhdl_rat')
write.csv(results_model20,"~\cohort1_model20_date.csv", row.names = TRUE) 

### 3.6. Models 21, 22, 23, and 24 - Exclusion of all medication users
Sample_nomed$cooccur <- relevel(Sample_nomed$cooccur, ref='0')
Sample_nomed$sex <- relevel(Sample_nomed$sex, ref='0')
Sample_nomed$educ <- relevel(Sample_nomed$educ, ref='0')
Sample_nomed$ethn <- relevel(Sample_nomed$ethn, ref='0')

model21 <- lm(bmi ~ cooccur + sex + age + educ + ethn, data = Sample_nomed)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity)
plot(model21, c(1,2,3))
# Extract summary statistics of model
results_model21 <- models_cooccur_info(model21, 'bmi')
write.csv(results_model21,"~\cohort1_model21_date.csv", row.names = TRUE) 

model22 <- lm(waisthip_rat ~ cooccur + sex + age + educ + ethn, data = Sample_nomed)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity)
plot(model22, c(1,2,3))
# Extract summary statistics of model
results_model22 <- models_cooccur_info(model22, 'waisthip_rat')
write.csv(results_model22,"~\cohort1_model22_date.csv", row.names = TRUE) 

model23 <- lm(trig ~ cooccur + sex + age + educ + ethn, data = Sample_nomed)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity)
plot(model23, c(1,2,3))
# Extract summary statistics of model
results_model23 <- models_cooccur_info(model23, 'trig')
write.csv(results_model23,"~\cohort1_model23_date.csv", row.names = TRUE) 

model24 <- lm(ldlhdl_rat ~ cooccur + sex + age + educ + ethn, data = Sample_nomed)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity)
plot(model24, c(1,2,3))
# Extract summary statistics of model
results_model24 <- models_cooccur_info(model24, 'ldlhdl_rat')
write.csv(results_model24,"~\cohort1_model24_date.csv", row.names = TRUE) 

### 3.10. Models 25, 26, 27, 28, 29, 30, 31, and 32 - Sex stratification
# Males
Sample_males$cooccur <- relevel(Sample_males$cooccur, ref='0')
Sample_males$sex <- relevel(Sample_males$sex, ref='0')
Sample_males$educ <- relevel(Sample_males$educ, ref='0')
Sample_males$ethn <- relevel(Sample_males$ethn, ref='0')

model25 <- lm(bmi ~ cooccur + age + educ + ethn, data = Sample_males)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity)
plot(model25, c(1,2,3))
# Extract summary statistics of model
results_model25 <- models_sex_strat_info(model25, 'bmi')
write.csv(results_model25,"~\cohort1_model25_date.csv", row.names = TRUE) 

model26 <- lm(waisthip_rat ~ cooccur + age + educ + ethn, data = Sample_males)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity)
plot(model26, c(1,2,3))
# Extract summary statistics of model
results_model26 <- models_sex_strat_info(model26, 'waisthip_rat')
write.csv(results_model26,"~\cohort1_model26_date.csv", row.names = TRUE) 

model27 <- lm(trig ~ cooccur + age + educ + ethn, data = Sample_males)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity)
plot(model27, c(1,2,3))
# Extract summary statistics of model
results_model27 <- models_sex_strat_info(model27, 'trig')
write.csv(results_model27,"~\cohort1_model27_date.csv", row.names = TRUE) 

model28 <- lm(ldlhdl_rat ~ cooccur + age + educ + ethn, data = Sample_males)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity)
plot(model28, c(1,2,3))
# Extract summary statistics of model
results_model28 <- models_sex_strat_info(model28, 'ldlhdl_rat')
write.csv(results_model28,"~\cohort1_model28_date.csv", row.names = TRUE) 

# Females
Sample_females$cooccur <- relevel(Sample_females$cooccur, ref='0')
Sample_females$sex <- relevel(Sample_females$sex, ref='0')
Sample_females$educ <- relevel(Sample_females$educ, ref='0')
Sample_females$ethn <- relevel(Sample_females$ethn, ref='0')

model29 <- lm(bmi ~ cooccur + age + educ + ethn, data = Sample_females)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity)
plot(model29, c(1,2,3))
# Extract summary statistics of model
results_model29 <- models_sex_strat_info(model29, 'bmi')
write.csv(results_model29,"~\cohort1_model29_date.csv", row.names = TRUE) 

model30 <- lm(waisthip_rat ~ cooccur + age + educ + ethn, data = Sample_females)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity)
plot(model30, c(1,2,3))
# Extract summary statistics of model
results_model30 <- models_sex_strat_info(model30, 'waisthip_rat')
write.csv(results_model30,"~\cohort1_model30_date.csv", row.names = TRUE) 

model31 <- lm(trig ~ cooccur + age + educ + ethn, data = Sample_females)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity)
plot(model31, c(1,2,3))
# Extract summary statistics of model
results_model31 <- models_sex_strat_info(model31, 'trig')
write.csv(results_model31,"~\cohort1_model31_date.csv", row.names = TRUE) 

model32 <- lm(ldlhdl_rat ~ cooccur + age + educ + ethn, data = Sample_females)
# Assumptions check - Linearity of the data, normality of residuals, homogeneity of residuals variance (homoscedasticity)
plot(model32, c(1,2,3))
# Extract summary statistics of model
results_model32 <- models_sex_strat_info(model32, 'ldlhdl_rat')
write.csv(results_model32,"~\cohort1_model32_date.csv", row.names = TRUE) 


# This the end of the code used to run models and extract summary statistics in each cohort study
# To continue with the data synthesis across the different cohorts, follow the code example that was used in "3-Data_synthesis.R"


