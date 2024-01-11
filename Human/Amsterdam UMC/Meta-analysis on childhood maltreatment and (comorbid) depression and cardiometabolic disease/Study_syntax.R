############################# Associations between childhood maltreatment and (comorbid) depression and cardiometabolic disease #############################

# Note1: The data needs to be uploaded in wide format with one case per row and the different variables in columns
# Note2: Please, keep the same order of the predictors when building the models as in the code below
# Note3: When running the function on the models, warning messages about the vif.default(model) and argument returning NA are to be expected

# Set working directory, adjust with your own pathway
setwd(~/"")

# Load necessary packages
library('foreign')
library('table1')
library('tidyverse')
library('dplyr')
library('gt')
library('plyr')
library('nnet')
library('pscl')
library('broom')



#################### 1. Load data #################### 

Data <- read.spss('Data.sav',
                  use.value.labels=FALSE,
                  to.data.frame=TRUE,
                  max.value.labels=Inf)



#################### 2. Create samples and extract descriptive statistics #################### 

# Sample
Sample <- Data[!is.na(Data$CM_pa) & !is.na(Data$CM_ea) & ( !is.na(Data$MDD) | !is.na(Data$CMD)),] #CM_pa = presence of physical abuse (0 = no vs. 1 = yes), CM_ea = presence of emotional abuse (0 = no vs. 1 = yes), MDD = presence of depression (0 = no vs. 1 = yes), CMD = presence of cardiometabolic disease (0 = no vs. 1 = yes)
Sample <- Sample[,c('MDD',
                    'DM', #DM = presence of diabetes mellitus (0 = no vs. 1 = yes)
                    'CM', #CM = presence of childhood maltreatment (0 = no vs. 1 = yes)
                    'Age',
                    'Sex', 
                    'Educ', #Educ = education level (0 = ISCED 2011 0-2 vs. 1 = ISCED 2011 3-4 vs. 2 = ISCED 2011 5+)
                    'CVD1', #CVD1 = presence of cardiovascular disease (main definition) (0 = no vs. 1 = yes)
                    'CMD',
                    'DISEASE_STAT', #DISEASE_STAT = disease status (0 = healthy controls vs. 1 = depression only vs. 2 = cardiometabolic disease only vs. 3 = comorbid depression and cardiometabolic disease)
                    'Smoke', #Smoke = smoking status (0 = no current smoking vs. 1 = current smoking)
                    'PhyAct', #PhyAct = physical activity (number of hours of physical activity per week)
                    'Alcohol', #Alcohol = alcohol consumption (number of alcohol drinks consumed per week)
                    'DISEASE_STAT_CD', #DISEASE_STAT_CD = disease status with cardiovascular disease based on strict definition
                    'DISEASE_STAT_CVD2', #DISEASE_STAT_CVD2 = disease status with cardiovascular disease based on broad definition
                    'DISEASE_STAT_med', #DISEASE_STAT_med = disease status with medication use as diagnosis inclusion criterion
                    'CM_pa', #CM_pa = presence of physical abuse (0 = no or less often than regularly, 1 = at least regular occurrences)
                    'CM_ea', #CM_ea = presence of emotional abuse (0 = no or less often than regularly, 1 = at least regularly)
                    'CM_sa', #CM_sa = presence of sexual abuse (0 = never vs. 1 = at least once)
                    'CM_sev')] #CM_sev = number of maltreatment types (0 vs. 1 vs. 2+)

# Sample of males only
Sample_males <- Sample[Sample$Sex == 0 & !is.na(Sample$Sex),]

# Sample of females only
Sample_females <- Sample[Sample$Sex == 1 & !is.na(Sample$Sex),]

# Descriptive statistics
N <- nrow(Sample) #Total sample size
n_males <- nrow(Sample[Sample$Sex==0 & !is.na(Sample$Sex),]) #Number of males
n_females <- nrow(Sample[Sample$Sex==1 & !is.na(Sample$Sex),]) #Number of females
n_noCM <- nrow(Sample[Sample$CM==0 & !is.na(Sample$CM),]) #Number of individuals without childhood maltreatment
n_CM <- nrow(Sample[Sample$CM==1 & !is.na(Sample$CM),]) #Number of individuals with childhood maltreatment
n_noCM_pa <- nrow(Sample[Sample$CM_pa == 0 & !is.na(Sample$CM_pa),]) #Number of individuals without physical abuse
n_CM_pa <- nrow(Sample[Sample$CM_pa == 1 & !is.na(Sample$CM_pa),]) #Number of individuals with physical abuse
n_noCM_ea <- nrow(Sample[Sample$CM_ea == 0 & !is.na(Sample$CM_ea),]) #Number of individuals without emotional abuse
n_CM_ea <- nrow(Sample[Sample$CM_ea == 1 & !is.na(Sample$CM_ea),]) #Number of individuals with emotional abuse
n_noCM_sa <- nrow(Sample[Sample$CM_sa == 0 & !is.na(Sample$CM_sa),]) #Number of individuals without sexual abuse
n_CM_sa <- nrow(Sample[Sample$CM_sa == 1 & !is.na(Sample$CM_sa),]) #Number of individuals with sexual abuse
n_CM_sev0 <- nrow(Sample[Sample$CM_sev == 0 & !is.na(Sample$CM_sev),]) #Number of individuals reporting no type of abuse
n_CM_sev1 <- nrow(Sample[Sample$CM_sev == 1 & !is.na(Sample$CM_sev),]) #Number of individuals reporting one type of abuse
n_CM_sev2 <- nrow(Sample[Sample$CM_sev == 2 & !is.na(Sample$CM_sev),]) #Number of individuals reporting two or more types of abuse
n_noMDD <- nrow(Sample[Sample$MDD==0 & !is.na(Sample$MDD),]) #Number of individuals without depression
n_MDD <- nrow(Sample[Sample$MDD==1 & !is.na(Sample$MDD),]) #Number of individuals with depression
n_noCVD1 <- nrow(Sample[Sample$CVD1==0 & !is.na(Sample$CVD1),]) #Number of individuals without cardiovascular disease (based on main definition)
n_CVD1 <- nrow(Sample[Sample$CVD1==1 & !is.na(Sample$CVD1),]) #Number of individuals with cardiovascular disease (based on main definition)
n_noDM <- nrow(Sample[Sample$DM==0 & !is.na(Sample$DM),]) #Number of individuals without diabetes mellitus
n_DM <- nrow(Sample[Sample$DM==1 & !is.na(Sample$DM),]) #Number of individuals with diabetes mellitus
n_noCMD <- nrow(Sample[Sample$CMD==0 & !is.na(Sample$CMD),]) #Number of individuals without cardiometabolic disease
n_CMD <- nrow(Sample[Sample$CMD==1 & !is.na(Sample$CMD),]) #Number of individuals with cardiometabolic disease
n_DISEASE_STAT0 <- nrow(Sample[Sample$DISEASE_STAT==0 & !is.na(Sample$DISEASE_STAT),]) #Number of healthy controls = individuals without depression and without cardiometabolic disease
n_DISEASE_STAT1 <- nrow(Sample[Sample$DISEASE_STAT==1 & !is.na(Sample$DISEASE_STAT),]) #Number of individuals with depression only (no cardiometabolic disease)
n_DISEASE_STAT2 <- nrow(Sample[Sample$DISEASE_STAT==2 & !is.na(Sample$DISEASE_STAT),]) #Number of individuals with cardiometabolic disease only (no depression)
n_DISEASE_STAT3 <- nrow(Sample[Sample$DISEASE_STAT==3 & !is.na(Sample$DISEASE_STAT),]) #Number of individuals with comorbid depression and cardiometabolic disease
mean_age <- mean(Sample$Age, na.rm=T) #Average age
median_age <- median(Sample$Age, na.rm=T) #Median age
sd_age <- sd(Sample$Age, na.rm=T) #Standard deviation of age
n_Educ0 <- nrow(Sample[Sample$Educ==0 & !is.na(Sample$Educ),]) #Number of individuals with education level 0 (ISCED 2011 0-2)
n_Educ1 <- nrow(Sample[Sample$Educ==1 & !is.na(Sample$Educ),]) #Number of individuals with education level 1 (ISCED 2011 3-4)
n_Educ2 <- nrow(Sample[Sample$Educ==2 & !is.na(Sample$Educ),]) #Number of individuals with education level 2 (ISCED 2011 5+)
n_Smoke0 <- nrow(Sample[Sample$Smoke==0 & !is.na(Sample$Smoke),]) #Number of individuals currently not smoking
n_Smoke1 <- nrow(Sample[Sample$Smoke==1 & !is.na(Sample$Smoke),]) #Number of individuals currently smoking
mean_PhyAct <- mean(Sample$PhyAct, na.rm=T) #Average number of hours of physical activity per week
median_PhyAct <- median(Sample$PhyAct, na.rm=T) #Median number of hours of physical activity per week
sd_PhyAct <- sd(Sample$PhyAct, na.rm=T) #Standard deviation of number of hours of physical activity per week
mean_Alcohol <- mean(Sample$Alcohol, na.rm=T) #Average number of alcoholic drinks consumed per week
median_Alcohol <- median(Sample$Alcohol, na.rm=T) #Median number of alcoholic drinks consumed per week
sd_Alcohol <- sd(Sample$Alcohol, na.rm=T) #Standard deviation of number of alcoholic drinks consumed per week

DescriptivesStatistics <- data.frame(N,
                                     n_males,
                                     n_females,
                                     n_noCM,
                                     n_CM,
                                     n_noCM_pa,
                                     n_CM_pa,
                                     n_noCM_ea,
                                     n_CM_ea,
                                     n_noCM_sa,
                                     n_CM_sa,
                                     n_CM_sev0,
                                     n_CM_sev1,
                                     n_CM_sev2,
                                     n_noMDD,
                                     n_MDD,
                                     n_noCVD1,
                                     n_CVD1,
                                     n_noDM,
                                     n_DM,
                                     n_noCMD,
                                     n_CMD,
                                     n_DISEASE_STAT0,
                                     n_DISEASE_STAT1,
                                     n_DISEASE_STAT2,
                                     n_DISEASE_STAT3,
                                     mean_age,
                                     median_age,
                                     sd_age,
                                     n_Educ0,
                                     n_Educ1,
                                     n_Educ2,
                                     n_Smoke0,
                                     n_Smoke1,
                                     mean_PhyAct,
                                     median_PhyAct,
                                     sd_PhyAct,
                                     mean_Alcohol,
                                     median_Alcohol,
                                     sd_Alcohol)

# Export cohort's descriptive statistics into a csv file 
write.csv(DescriptivesStatistics,"~/.../Cohort1_descriptives.csv", row.names = TRUE)

#################### 3. Functions to extract models summary statistics #################### 

logisticmodels_stat <- function(model){
  #Select only complete cases in the dataset
  mydata <- Sample[,all.vars(formula(model))]
  mydata <- mydata[complete.cases(mydata),]
  # Create objects to store desired model characteristics
  model.data <- augment(model) %>% 
    dplyr::mutate(index = 1:n())
  inflobs <- model.data %>% filter(abs(.std.resid) > 3) #influential observations 
  N_inflobs <- nrow(inflobs) #number of influential observations
  vif <- car::vif(model)['CM','GVIF'] #variance inflation factor
  N <- nrow(mydata)
  n_males <- nrow(mydata[mydata$Sex == 0,])
  mean_age <- mean(mydata$Age,na.rm=T)
  median_age <- median(mydata$Age,na.rm = T)
  sd_age <- sd(mydata$Age,na.rm = T)
  n_educ0 <- table(mydata$Educ)[1]
  n_educ1 <- table(mydata$Educ)[2]
  n_educ2 <- table(mydata$Educ)[3]
  source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
  crosstab <- crosstab(mydata, row.vars = "CM", col.vars = all.vars(formula(model))[1], type = "f")
  n_noCM_DISEASE_STAT0 <- crosstab$crosstab['0','0']
  n_CM_DISEASE_STAT0 <- crosstab$crosstab['1','0']
  n_CMsum_DISEASE_STAT0 <- crosstab$crosstab['Sum','0']
  n_noCM_DISEASE_STAT1 <- crosstab$crosstab['0','1']
  n_CM_DISEASE_STAT1 <- crosstab$crosstab['1','1']
  n_CMsum_DISEASE_STAT1 <- crosstab$crosstab['Sum','1']
  n_noCM_DISEASE_STATsum <- crosstab$crosstab['0','Sum']
  n_CM_DISEASE_STATsum <- crosstab$crosstab['1','Sum']
  n_CMsum_DISEASE_STATsum <- crosstab$crosstab['Sum','Sum']
  # Record coefficient estimates of log(OR), standard errors and 95% CI of log(OR) of predictors' effects on the dependent variable
  Estimate_CM <- summary(model)$coefficients['CM1','Estimate']
  Std.Error_CM <- summary(model)$coefficients['CM1','Std. Error']
  CI_LL_logOR_CM <- confint.default(model, level = 0.95)['CM1',1]
  CI_UL_logOR_CM <- confint.default(model, level = 0.95)['CM1',2]
  Estimate_Age <- summary(model)$coefficients['Age','Estimate']
  Std.Error_Age <- summary(model)$coefficients['Age','Std. Error']
  CI_LL_logOR_Age <- confint.default(model, level = 0.95)['Age',1]
  CI_UL_logOR_Age <- confint.default(model, level = 0.95)['Age',2]
  Estimate_Sex <- summary(model)$coefficients['Sex1','Estimate']
  Std.Error_Sex <- summary(model)$coefficients['Sex1','Std. Error']
  CI_LL_logOR_Sex <- confint.default(model, level = 0.95)['Sex1',1]
  CI_UL_logOR_Sex <- confint.default(model, level = 0.95)['Sex1',2]
  Estimate_Educ1 <- summary(model)$coefficients['Educ1','Estimate']
  Std.Error_Educ1 <- summary(model)$coefficients['Educ1','Std. Error']
  CI_LL_logOR_Educ1 <- confint.default(model, level = 0.95)['Educ1',1]
  CI_UL_logOR_Educ1 <- confint.default(model, level = 0.95)['Educ1',2]
  Estimate_Educ2 <- summary(model)$coefficients['Educ2','Estimate']
  Std.Error_Educ2 <- summary(model)$coefficients['Educ2','Std. Error']
  CI_LL_logOR_Educ2 <- confint.default(model, level = 0.95)['Educ2',1]
  CI_UL_logOR_Educ2 <- confint.default(model, level = 0.95)['Educ2',2]
  # Gather all model information in dataframe
  Stats_model <- data.frame(N_inflobs,
                            vif,
                            N,
                            n_males,
                            mean_age,
                            median_age,
                            sd_age,
                            n_educ0,
                            n_educ1,
                            n_educ2,
                            n_noCM_DISEASE_STAT0,
                            n_CM_DISEASE_STAT0,
                            n_CMsum_DISEASE_STAT0,
                            n_noCM_DISEASE_STAT1,
                            n_CM_DISEASE_STAT1,
                            n_CMsum_DISEASE_STAT1,
                            n_noCM_DISEASE_STATsum,
                            n_CM_DISEASE_STATsum,
                            n_CMsum_DISEASE_STATsum,
                            Estimate_CM,
                            Std.Error_CM,
                            CI_LL_logOR_CM,
                            CI_UL_logOR_CM,
                            Estimate_Age,
                            Std.Error_Age,
                            CI_LL_logOR_Age,
                            CI_UL_logOR_Age,
                            Estimate_Sex,
                            Std.Error_Sex,
                            CI_LL_logOR_Sex,
                            CI_UL_logOR_Sex,
                            Estimate_Educ1,
                            Std.Error_Educ1,
                            CI_LL_logOR_Educ1,
                            CI_UL_logOR_Educ1,
                            Estimate_Educ2,
                            Std.Error_Educ2,
                            CI_LL_logOR_Educ2,
                            CI_UL_logOR_Educ2)
  return(Stats_model)
}

multinomialmodels_stat <- function(model){
  #Select only complete cases in the dataset
  mydata <- Sample[,all.vars(formula(model))]
  mydata <- mydata[complete.cases(mydata),]
  # Create objects to store desired model characteristics
  vif <- as.data.frame(car::vif(model))[grep("^CM", rownames(as.data.frame(car::vif(model))),value=T),'GVIF']
  N <- nrow(mydata)
  n_males <- nrow(mydata[mydata$Sex == 0,])
  mean_age <- mean(mydata$Age,na.rm=T)
  median_age <- median(mydata$Age,na.rm = T)
  sd_age <- sd(mydata$Age,na.rm = T)
  n_educ0 <- table(mydata$Educ)[1]
  n_educ1 <- table(mydata$Educ)[2]
  n_educ2 <- table(mydata$Educ)[3]
  mean_phyact <- mean(mydata$PhyAct, na.rm =T)
  median_phyact <- ifelse(sum(all.vars(formula(model)) == "PhyAct") == 1, median(mydata$PhyAct, na.rm = T), NA)
  sd_phyact <- sd(mydata$PhyAct, na.rm = T)
  n_smoke <- nrow(mydata[mydata$Smoke == 1,])
  mean_alc <- mean(mydata$Alcohol, na.rm =T)
  median_alc <- ifelse(sum(all.vars(formula(model)) == "PhyAct") == 1, median(mydata$Alcohol, na.rm = T), NA)
  sd_alc <- sd(mydata$Alcohol, na.rm = T)
  source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
  crosstab <- crosstab(mydata, row.vars = all.vars(formula(model))[2], col.vars = all.vars(formula(model))[1], type = "f")
  n_noCM_DISEASE_STAT0 <- crosstab$crosstab['0','0']
  n_CM_DISEASE_STAT0 <- crosstab$crosstab['1','0']
  n_CMsum_DISEASE_STAT0 <- crosstab$crosstab['Sum','0']
  n_noCM_DISEASE_STAT1 <- crosstab$crosstab['0','1']
  n_CM_DISEASE_STAT1 <- crosstab$crosstab['1','1']
  n_CMsum_DISEASE_STAT1 <- crosstab$crosstab['Sum','1']
  n_noCM_DISEASE_STAT2 <- crosstab$crosstab['0','2']
  n_CM_DISEASE_STAT2 <- crosstab$crosstab['1','2']
  n_CMsum_DISEASE_STAT2 <- crosstab$crosstab['Sum','2']
  n_noCM_DISEASE_STAT3 <- crosstab$crosstab['0','3']
  n_CM_DISEASE_STAT3 <- crosstab$crosstab['1','3']
  n_CMsum_DISEASE_STAT3 <- crosstab$crosstab['Sum','3']
  n_noCM_DISEASE_STATsum <- crosstab$crosstab['0','Sum']
  n_CM_DISEASE_STATsum <- crosstab$crosstab['1','Sum']
  n_CMsum_DISEASE_STATsum <- crosstab$crosstab['Sum','Sum']
  # Record coefficient estimates log(OR), standard errors, and 95% CI of log(OR) of predictors' effects on the three levels of the outcome variable
  Estimate_CM_DISEASE_STAT1 <- summary(model)$coefficients[1,grep("^CM",colnames(summary(model)$coefficients))]
  Std.Error_CM_DISEASE_STAT1 <- summary(model)$standard.errors[1,grep("^CM",colnames(summary(model)$coefficients))]
  CI_LL_logOR_CM_DISEASE_STAT1 <- confint(model)[grep("^CM",rownames(confint(model))),1,1]
  CI_UL_logOR_CM_DISEASE_STAT1 <- confint(model)[grep("^CM",rownames(confint(model))),2,1]
  Estimate_CM_DISEASE_STAT2 <- summary(model)$coefficients[2,grep("^CM",colnames(summary(model)$coefficients))]
  Std.Error_CM_DISEASE_STAT2 <- summary(model)$standard.errors[2,grep("^CM",colnames(summary(model)$coefficients))]
  CI_LL_logOR_CM_DISEASE_STAT2 <- confint(model)[grep("^CM",rownames(confint(model))),1,2]
  CI_UL_logOR_CM_DISEASE_STAT2 <- confint(model)[grep("^CM",rownames(confint(model))),2,2]
  Estimate_CM_DISEASE_STAT3 <- summary(model)$coefficients[3,grep("^CM",colnames(summary(model)$coefficients))]
  Std.Error_CM_DISEASE_STAT3 <- summary(model)$standard.errors[3,grep("^CM",colnames(summary(model)$coefficients))]
  CI_LL_logOR_CM_DISEASE_STAT3 <- confint(model)[grep("^CM",rownames(confint(model))),1,3]
  CI_UL_logOR_CM_DISEASE_STAT3 <- confint(model)[grep("^CM",rownames(confint(model))),2,3]
  Estimate_Age_DISEASE_STAT1 <- summary(model)$coefficients['1','Age']
  Std.Error_Age_DISEASE_STAT1 <- summary(model)$standard.errors[1,'Age']
  CI_LL_logOR_Age_DISEASE_STAT1 <- confint(model)['Age',1,1]
  CI_UL_logOR_Age_DISEASE_STAT1 <- confint(model)['Age',2,1]
  Estimate_Sex_DISEASE_STAT1 <- summary(model)$coefficients['1','Sex1']
  Std.Error_Sex_DISEASE_STAT1 <- summary(model)$standard.errors[1,'Sex1']
  CI_LL_logOR_Sex_DISEASE_STAT1 <- confint(model)['Sex1',1,1]
  CI_UL_logOR_Sex_DISEASE_STAT1 <- confint(model)['Sex1',2,1]
  Estimate_Educ1_DISEASE_STAT1 <- summary(model)$coefficients['1','Educ1']
  Std.Error_Educ1_DISEASE_STAT1 <- summary(model)$standard.errors[1,'Educ1']
  CI_LL_logOR_Educ1_DISEASE_STAT1 <- confint(model)['Educ1',1,1]
  CI_UL_logOR_Educ1_DISEASE_STAT1 <- confint(model)['Educ1',2,1]
  Estimate_Educ2_DISEASE_STAT1 <- summary(model)$coefficients['1','Educ2']
  Std.Error_Educ2_DISEASE_STAT1 <- summary(model)$standard.errors['1','Educ2']
  CI_LL_logOR_Educ2_DISEASE_STAT1 <- confint(model)['Educ2',1,1]
  CI_UL_logOR_Educ2_DISEASE_STAT1 <- confint(model)['Educ2',2,1]
  Estimate_Age_DISEASE_STAT2 <- summary(model)$coefficients['2','Age']
  Std.Error_Age_DISEASE_STAT2 <- summary(model)$standard.errors[2,'Age']
  CI_LL_logOR_Age_DISEASE_STAT2 <- confint(model)['Age',1,2]
  CI_UL_logOR_Age_DISEASE_STAT2 <- confint(model)['Age',2,2]
  Estimate_Sex_DISEASE_STAT2 <- summary(model)$coefficients['2','Sex1']
  Std.Error_Sex_DISEASE_STAT2 <- summary(model)$standard.errors[2,'Sex1']
  CI_LL_logOR_Sex_DISEASE_STAT2 <- confint(model)['Sex1',1,2]
  CI_UL_logOR_Sex_DISEASE_STAT2 <- confint(model)['Sex1',2,2]
  Estimate_Educ1_DISEASE_STAT2 <- summary(model)$coefficients['2','Educ1']
  Std.Error_Educ1_DISEASE_STAT2 <- summary(model)$standard.errors[2,'Educ1']
  CI_LL_logOR_Educ1_DISEASE_STAT2 <- confint(model)['Educ1',1,2]
  CI_UL_logOR_Educ1_DISEASE_STAT2 <- confint(model)['Educ1',2,2]
  Estimate_Educ2_DISEASE_STAT2 <- summary(model)$coefficients['2','Educ2']
  Std.Error_Educ2_DISEASE_STAT2 <- summary(model)$standard.errors['2','Educ2']
  CI_LL_logOR_Educ2_DISEASE_STAT2 <- confint(model)['Educ2',1,2]
  CI_UL_logOR_Educ2_DISEASE_STAT2 <- confint(model)['Educ2',2,2]
  Estimate_Age_DISEASE_STAT3 <- summary(model)$coefficients['3','Age']
  Std.Error_Age_DISEASE_STAT3 <- summary(model)$standard.errors[3,'Age']
  CI_LL_logOR_Age_DISEASE_STAT3 <- confint(model)['Age',1,3]
  CI_UL_logOR_Age_DISEASE_STAT3 <- confint(model)['Age',2,3]
  Estimate_Sex_DISEASE_STAT3 <- summary(model)$coefficients['3','Sex1']
  Std.Error_Sex_DISEASE_STAT3 <- summary(model)$standard.errors[3,'Sex1']
  CI_LL_logOR_Sex_DISEASE_STAT3 <- confint(model)['Sex1',1,3]
  CI_UL_logOR_Sex_DISEASE_STAT3 <- confint(model)['Sex1',2,3]
  Estimate_Educ1_DISEASE_STAT3 <- summary(model)$coefficients['3','Educ1']
  Std.Error_Educ1_DISEASE_STAT3 <- summary(model)$standard.errors[3,'Educ1']
  CI_LL_logOR_Educ1_DISEASE_STAT3 <- confint(model)['Educ1',1,3]
  CI_UL_logOR_Educ1_DISEASE_STAT3 <- confint(model)['Educ1',2,3]
  Estimate_Educ2_DISEASE_STAT3 <- summary(model)$coefficients['3','Educ2']
  Std.Error_Educ2_DISEASE_STAT3 <- summary(model)$standard.errors['3','Educ2']
  CI_LL_logOR_Educ2_DISEASE_STAT3 <- confint(model)['Educ2',1,3]
  CI_UL_logOR_Educ2_DISEASE_STAT3 <- confint(model)['Educ2',2,3]
  # Gather all model information in dataframe
  Stats_model <- data.frame(vif,
                            N,
                            n_males,
                            mean_age,
                            median_age,
                            sd_age,
                            n_educ0,
                            n_educ1,
                            n_educ2,
                            mean_phyact,
                            median_phyact,
                            sd_phyact,
                            n_smoke,
                            mean_alc,
                            median_alc,
                            sd_alc,
                            n_noCM_DISEASE_STAT0,
                            n_CM_DISEASE_STAT0,
                            n_CMsum_DISEASE_STAT0,
                            n_noCM_DISEASE_STAT1,
                            n_CM_DISEASE_STAT1,
                            n_CMsum_DISEASE_STAT1,
                            n_noCM_DISEASE_STAT2,
                            n_CM_DISEASE_STAT2,
                            n_CMsum_DISEASE_STAT2,
                            n_noCM_DISEASE_STAT3,
                            n_CM_DISEASE_STAT3,
                            n_CMsum_DISEASE_STAT3,
                            n_noCM_DISEASE_STATsum,
                            n_CM_DISEASE_STATsum,
                            n_CMsum_DISEASE_STATsum,
                            Estimate_CM_DISEASE_STAT1,
                            Std.Error_CM_DISEASE_STAT1,
                            CI_LL_logOR_CM_DISEASE_STAT1,
                            CI_UL_logOR_CM_DISEASE_STAT1,
                            Estimate_CM_DISEASE_STAT2,
                            Std.Error_CM_DISEASE_STAT2,
                            CI_LL_logOR_CM_DISEASE_STAT2,
                            CI_UL_logOR_CM_DISEASE_STAT2,
                            Estimate_CM_DISEASE_STAT3,
                            Std.Error_CM_DISEASE_STAT3,
                            CI_LL_logOR_CM_DISEASE_STAT3,
                            CI_UL_logOR_CM_DISEASE_STAT3,
                            Estimate_Age_DISEASE_STAT1,
                            Std.Error_Age_DISEASE_STAT1,
                            CI_LL_logOR_Age_DISEASE_STAT1,
                            CI_UL_logOR_Age_DISEASE_STAT1,
                            Estimate_Sex_DISEASE_STAT1,
                            Std.Error_Sex_DISEASE_STAT1,
                            CI_LL_logOR_Sex_DISEASE_STAT1,
                            CI_UL_logOR_Sex_DISEASE_STAT1,
                            Estimate_Educ1_DISEASE_STAT1,
                            Std.Error_Educ1_DISEASE_STAT1,
                            CI_LL_logOR_Educ1_DISEASE_STAT1,
                            CI_UL_logOR_Educ1_DISEASE_STAT1,
                            Estimate_Educ2_DISEASE_STAT1,
                            Std.Error_Educ2_DISEASE_STAT1,
                            CI_LL_logOR_Educ2_DISEASE_STAT1,
                            CI_UL_logOR_Educ2_DISEASE_STAT1,
                            Estimate_Age_DISEASE_STAT2,
                            Std.Error_Age_DISEASE_STAT2,
                            CI_LL_logOR_Age_DISEASE_STAT2,
                            CI_UL_logOR_Age_DISEASE_STAT2,
                            Estimate_Sex_DISEASE_STAT2,
                            Std.Error_Sex_DISEASE_STAT2,
                            CI_LL_logOR_Sex_DISEASE_STAT2,
                            CI_UL_logOR_Sex_DISEASE_STAT2,
                            Estimate_Educ1_DISEASE_STAT2,
                            Std.Error_Educ1_DISEASE_STAT2,
                            CI_LL_logOR_Educ1_DISEASE_STAT2,
                            CI_UL_logOR_Educ1_DISEASE_STAT2,
                            Estimate_Educ2_DISEASE_STAT2,
                            Std.Error_Educ2_DISEASE_STAT2,
                            CI_LL_logOR_Educ2_DISEASE_STAT2,
                            CI_UL_logOR_Educ2_DISEASE_STAT2,
                            Estimate_Age_DISEASE_STAT3,
                            Std.Error_Age_DISEASE_STAT3,
                            CI_LL_logOR_Age_DISEASE_STAT3,
                            CI_UL_logOR_Age_DISEASE_STAT3,
                            Estimate_Sex_DISEASE_STAT3,
                            Std.Error_Sex_DISEASE_STAT3,
                            CI_LL_logOR_Sex_DISEASE_STAT3,
                            CI_UL_logOR_Sex_DISEASE_STAT3,
                            Estimate_Educ1_DISEASE_STAT3,
                            Std.Error_Educ1_DISEASE_STAT3,
                            CI_LL_logOR_Educ1_DISEASE_STAT3,
                            CI_UL_logOR_Educ1_DISEASE_STAT3,
                            Estimate_Educ2_DISEASE_STAT3,
                            Std.Error_Educ2_DISEASE_STAT3,
                            CI_LL_logOR_Educ2_DISEASE_STAT3,
                            CI_UL_logOR_Educ2_DISEASE_STAT3)
  return(Stats_model)
}

multinomial_multippredictorsmodels_stat <- function(model){
  #Select only complete cases in the dataset
  mydata <- Sample[,all.vars(formula(model))]
  mydata <- mydata[complete.cases(mydata),]
  # Create objects to store desired model characteristics
  vif_CM_pa <- car::vif(model)['CM_pa','GVIF']
  vif_CM_ea <- car::vif(model)['CM_ea','GVIF']
  vif_CM_sa <- car::vif(model)['CM_sa','GVIF']
  N <- nrow(mydata)
  n_males <- nrow(mydata[mydata$Sex == 0,])
  mean_age <- mean(mydata$Age,na.rm=T)
  median_age <- median(mydata$Age,na.rm = T)
  sd_age <- sd(mydata$Age,na.rm = T)
  n_educ0 <- table(mydata$Educ)[1]
  n_educ1 <- table(mydata$Educ)[2]
  n_educ2 <- table(mydata$Educ)[3]
  source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
  crosstabCM_pa <- crosstab(mydata, row.vars = "CM_pa", col.vars = "DISEASE_STAT", type = "f")
  crosstabCM_ea <- crosstab(mydata, row.vars = "CM_ea", col.vars = "DISEASE_STAT", type = "f")
  crosstabCM_sa <- crosstab(mydata, row.vars = "CM_sa", col.vars = "DISEASE_STAT", type = "f")
  n_noCM_pa_DISEASE_STAT0 <- crosstabCM_pa$crosstab['0','0']
  n_CM_pa_DISEASE_STAT0 <- crosstabCM_pa$crosstab['1','0']
  n_CM_pasum_DISEASE_STAT0 <- crosstabCM_pa$crosstab['Sum','0']
  n_noCM_pa_DISEASE_STAT1 <- crosstabCM_pa$crosstab['0','1']
  n_CM_pa_DISEASE_STAT1 <- crosstabCM_pa$crosstab['1','1']
  n_CM_pasum_DISEASE_STAT1 <- crosstabCM_pa$crosstab['Sum','1']
  n_noCM_pa_DISEASE_STAT2 <- crosstabCM_pa$crosstab['0','2']
  n_CM_pa_DISEASE_STAT2 <- crosstabCM_pa$crosstab['1','2']
  n_CM_pasum_DISEASE_STAT2 <- crosstabCM_pa$crosstab['Sum','2']
  n_noCM_pa_DISEASE_STAT3 <- crosstabCM_pa$crosstab['0','3']
  n_CM_pa_DISEASE_STAT3 <- crosstabCM_pa$crosstab['1','3']
  n_CM_pasum_DISEASE_STAT3 <- crosstabCM_pa$crosstab['Sum','3']
  n_noCM_pa_DISEASE_STATsum <- crosstabCM_pa$crosstab['0','Sum']
  n_CM_pa_DISEASE_STATsum <- crosstabCM_pa$crosstab['1','Sum']
  n_CM_pasum_DISEASE_STATsum <- crosstabCM_pa$crosstab['Sum','Sum']
  n_noCM_ea_DISEASE_STAT0 <- crosstabCM_ea$crosstab['0','0']
  n_CM_ea_DISEASE_STAT0 <- crosstabCM_ea$crosstab['1','0']
  n_CM_easum_DISEASE_STAT0 <- crosstabCM_ea$crosstab['Sum','0']
  n_noCM_ea_DISEASE_STAT1 <- crosstabCM_ea$crosstab['0','1']
  n_CM_ea_DISEASE_STAT1 <- crosstabCM_ea$crosstab['1','1']
  n_CM_easum_DISEASE_STAT1 <- crosstabCM_ea$crosstab['Sum','1']
  n_noCM_ea_DISEASE_STAT2 <- crosstabCM_ea$crosstab['0','2']
  n_CM_ea_DISEASE_STAT2 <- crosstabCM_ea$crosstab['1','2']
  n_CM_easum_DISEASE_STAT2 <- crosstabCM_ea$crosstab['Sum','2']
  n_noCM_ea_DISEASE_STAT3 <- crosstabCM_ea$crosstab['0','3']
  n_CM_ea_DISEASE_STAT3 <- crosstabCM_ea$crosstab['1','3']
  n_CM_easum_DISEASE_STAT3 <- crosstabCM_ea$crosstab['Sum','3']
  n_noCM_ea_DISEASE_STATsum <- crosstabCM_ea$crosstab['0','Sum']
  n_CM_ea_DISEASE_STATsum <- crosstabCM_ea$crosstab['1','Sum']
  n_CM_easum_DISEASE_STATsum <- crosstabCM_ea$crosstab['Sum','Sum']
  n_noCM_sa_DISEASE_STAT0 <- crosstabCM_sa$crosstab['0','0']
  n_CM_sa_DISEASE_STAT0 <- crosstabCM_sa$crosstab['1','0']
  n_CM_sasum_DISEASE_STAT0 <- crosstabCM_sa$crosstab['Sum','0']
  n_noCM_sa_DISEASE_STAT1 <- crosstabCM_sa$crosstab['0','1']
  n_CM_sa_DISEASE_STAT1 <- crosstabCM_sa$crosstab['1','1']
  n_CM_sasum_DISEASE_STAT1 <- crosstabCM_sa$crosstab['Sum','1']
  n_noCM_sa_DISEASE_STAT2 <- crosstabCM_sa$crosstab['0','2']
  n_CM_sa_DISEASE_STAT2 <- crosstabCM_sa$crosstab['1','2']
  n_CM_sasum_DISEASE_STAT2 <- crosstabCM_sa$crosstab['Sum','2']
  n_noCM_sa_DISEASE_STAT3 <- crosstabCM_sa$crosstab['0','3']
  n_CM_sa_DISEASE_STAT3 <- crosstabCM_sa$crosstab['1','3']
  n_CM_sasum_DISEASE_STAT3 <- crosstabCM_sa$crosstab['Sum','3']
  n_noCM_sa_DISEASE_STATsum <- crosstabCM_sa$crosstab['0','Sum']
  n_CM_sa_DISEASE_STATsum <- crosstabCM_sa$crosstab['1','Sum']
  n_CM_sasum_DISEASE_STATsum <- crosstabCM_sa$crosstab['Sum','Sum']
  # Record coefficient estimates log(OR), standard errors, and 95% CI of log(OR) of predictors' effects on the three levels of the outcome variable
  Estimate_CM_pa_DISEASE_STAT1 <- summary(model)$coefficients[1,'CM_pa1']
  Std.Error_CM_pa_DISEASE_STAT1 <- summary(model)$standard.errors[1,'CM_pa1']
  CI_LL_logOR_CM_pa_DISEASE_STAT1 <- confint(model)['CM_pa1',1,1]
  CI_UL_logOR_CM_pa_DISEASE_STAT1 <- confint(model)['CM_pa1',2,1]
  Estimate_CM_pa_DISEASE_STAT2 <- summary(model)$coefficients[2,'CM_pa1']
  Std.Error_CM_pa_DISEASE_STAT2 <- summary(model)$standard.errors[2,'CM_pa1']
  CI_LL_logOR_CM_pa_DISEASE_STAT2 <- confint(model)['CM_pa1',1,2]
  CI_UL_logOR_CM_pa_DISEASE_STAT2 <- confint(model)['CM_pa1',2,2]
  Estimate_CM_pa_DISEASE_STAT3 <- summary(model)$coefficients[3,'CM_pa1']
  Std.Error_CM_pa_DISEASE_STAT3 <- summary(model)$standard.errors[3,'CM_pa1']
  CI_LL_logOR_CM_pa_DISEASE_STAT3 <- confint(model)['CM_pa1',1,3]
  CI_UL_logOR_CM_pa_DISEASE_STAT3 <- confint(model)['CM_pa1',2,3]
  Estimate_CM_ea_DISEASE_STAT1 <- summary(model)$coefficients[1,'CM_ea1']
  Std.Error_CM_ea_DISEASE_STAT1 <- summary(model)$standard.errors[1,'CM_ea1']
  CI_LL_logOR_CM_ea_DISEASE_STAT1 <- confint(model)['CM_ea1',1,1]
  CI_UL_logOR_CM_ea_DISEASE_STAT1 <- confint(model)['CM_ea1',2,1]
  Estimate_CM_ea_DISEASE_STAT2 <- summary(model)$coefficients[2,'CM_ea1']
  Std.Error_CM_ea_DISEASE_STAT2 <- summary(model)$standard.errors[2,'CM_ea1']
  CI_LL_logOR_CM_ea_DISEASE_STAT2 <- confint(model)['CM_ea1',1,2]
  CI_UL_logOR_CM_ea_DISEASE_STAT2 <- confint(model)['CM_ea1',2,2]
  Estimate_CM_ea_DISEASE_STAT3 <- summary(model)$coefficients[3,'CM_ea1']
  Std.Error_CM_ea_DISEASE_STAT3 <- summary(model)$standard.errors[3,'CM_ea1']
  CI_LL_logOR_CM_ea_DISEASE_STAT3 <- confint(model)['CM_ea1',1,3]
  CI_UL_logOR_CM_ea_DISEASE_STAT3 <- confint(model)['CM_ea1',2,3]
  Estimate_CM_sa_DISEASE_STAT1 <- summary(model)$coefficients[1,'CM_sa1']
  Std.Error_CM_sa_DISEASE_STAT1 <- summary(model)$standard.errors[1,'CM_sa1']
  CI_LL_logOR_CM_sa_DISEASE_STAT1 <- confint(model)['CM_sa1',1,1]
  CI_UL_logOR_CM_sa_DISEASE_STAT1 <- confint(model)['CM_sa1',2,1]
  Estimate_CM_sa_DISEASE_STAT2 <- summary(model)$coefficients[2,'CM_sa1']
  Std.Error_CM_sa_DISEASE_STAT2 <- summary(model)$standard.errors[2,'CM_sa1']
  CI_LL_logOR_CM_sa_DISEASE_STAT2 <- confint(model)['CM_sa1',1,2]
  CI_UL_logOR_CM_sa_DISEASE_STAT2 <- confint(model)['CM_sa1',2,2]
  Estimate_CM_sa_DISEASE_STAT3 <- summary(model)$coefficients[3,'CM_sa1']
  Std.Error_CM_sa_DISEASE_STAT3 <- summary(model)$standard.errors[3,'CM_sa1']
  CI_LL_logOR_CM_sa_DISEASE_STAT3 <- confint(model)['CM_sa1',1,3]
  CI_UL_logOR_CM_sa_DISEASE_STAT3 <- confint(model)['CM_sa1',2,3]
  Estimate_Age_DISEASE_STAT1 <- summary(model)$coefficients['1','Age']
  Std.Error_Age_DISEASE_STAT1 <- summary(model)$standard.errors[1,'Age']
  CI_LL_logOR_Age_DISEASE_STAT1 <- confint(model)['Age',1,1]
  CI_UL_logOR_Age_DISEASE_STAT1 <- confint(model)['Age',2,1]
  Estimate_Sex_DISEASE_STAT1 <- summary(model)$coefficients['1','Sex1']
  Std.Error_Sex_DISEASE_STAT1 <- summary(model)$standard.errors[1,'Sex1']
  CI_LL_logOR_Sex_DISEASE_STAT1 <- confint(model)['Sex1',1,1]
  CI_UL_logOR_Sex_DISEASE_STAT1 <- confint(model)['Sex1',2,1]
  Estimate_Educ1_DISEASE_STAT1 <- summary(model)$coefficients['1','Educ1']
  Std.Error_Educ1_DISEASE_STAT1 <- summary(model)$standard.errors[1,'Educ1']
  CI_LL_logOR_Educ1_DISEASE_STAT1 <- confint(model)['Educ1',1,1]
  CI_UL_logOR_Educ1_DISEASE_STAT1 <- confint(model)['Educ1',2,1]
  Estimate_Educ2_DISEASE_STAT1 <- summary(model)$coefficients['1','Educ2']
  Std.Error_Educ2_DISEASE_STAT1 <- summary(model)$standard.errors['1','Educ2']
  CI_LL_logOR_Educ2_DISEASE_STAT1 <- confint(model)['Educ2',1,1]
  CI_UL_logOR_Educ2_DISEASE_STAT1 <- confint(model)['Educ2',2,1]
  Estimate_Age_DISEASE_STAT2 <- summary(model)$coefficients['2','Age']
  Std.Error_Age_DISEASE_STAT2 <- summary(model)$standard.errors[2,'Age']
  CI_LL_logOR_Age_DISEASE_STAT2 <- confint(model)['Age',1,2]
  CI_UL_logOR_Age_DISEASE_STAT2 <- confint(model)['Age',2,2]
  Estimate_Sex_DISEASE_STAT2 <- summary(model)$coefficients['2','Sex1']
  Std.Error_Sex_DISEASE_STAT2 <- summary(model)$standard.errors[2,'Sex1']
  CI_LL_logOR_Sex_DISEASE_STAT2 <- confint(model)['Sex1',1,2]
  CI_UL_logOR_Sex_DISEASE_STAT2 <- confint(model)['Sex1',2,2]
  Estimate_Educ1_DISEASE_STAT2 <- summary(model)$coefficients['2','Educ1']
  Std.Error_Educ1_DISEASE_STAT2 <- summary(model)$standard.errors[2,'Educ1']
  CI_LL_logOR_Educ1_DISEASE_STAT2 <- confint(model)['Educ1',1,2]
  CI_UL_logOR_Educ1_DISEASE_STAT2 <- confint(model)['Educ1',2,2]
  Estimate_Educ2_DISEASE_STAT2 <- summary(model)$coefficients['2','Educ2']
  Std.Error_Educ2_DISEASE_STAT2 <- summary(model)$standard.errors['2','Educ2']
  CI_LL_logOR_Educ2_DISEASE_STAT2 <- confint(model)['Educ2',1,2]
  CI_UL_logOR_Educ2_DISEASE_STAT2 <- confint(model)['Educ2',2,2]
  Estimate_Age_DISEASE_STAT3 <- summary(model)$coefficients['3','Age']
  Std.Error_Age_DISEASE_STAT3 <- summary(model)$standard.errors[3,'Age']
  CI_LL_logOR_Age_DISEASE_STAT3 <- confint(model)['Age',1,3]
  CI_UL_logOR_Age_DISEASE_STAT3 <- confint(model)['Age',2,3]
  Estimate_Sex_DISEASE_STAT3 <- summary(model)$coefficients['3','Sex1']
  Std.Error_Sex_DISEASE_STAT3 <- summary(model)$standard.errors[3,'Sex1']
  CI_LL_logOR_Sex_DISEASE_STAT3 <- confint(model)['Sex1',1,3]
  CI_UL_logOR_Sex_DISEASE_STAT3 <- confint(model)['Sex1',2,3]
  Estimate_Educ1_DISEASE_STAT3 <- summary(model)$coefficients['3','Educ1']
  Std.Error_Educ1_DISEASE_STAT3 <- summary(model)$standard.errors[3,'Educ1']
  CI_LL_logOR_Educ1_DISEASE_STAT3 <- confint(model)['Educ1',1,3]
  CI_UL_logOR_Educ1_DISEASE_STAT3 <- confint(model)['Educ1',2,3]
  Estimate_Educ2_DISEASE_STAT3 <- summary(model)$coefficients['3','Educ2']
  Std.Error_Educ2_DISEASE_STAT3 <- summary(model)$standard.errors['3','Educ2']
  CI_LL_logOR_Educ2_DISEASE_STAT3 <- confint(model)['Educ2',1,3]
  CI_UL_logOR_Educ2_DISEASE_STAT3 <- confint(model)['Educ2',2,3]
  # Gather all model information in dataframe
  Stats_model <- data.frame(vif_CM_pa,
                            vif_CM_ea,
                            vif_CM_sa,
                            N,
                            n_males,
                            mean_age,
                            median_age,
                            sd_age,
                            n_educ0,
                            n_educ1,
                            n_educ2,
                            n_noCM_pa_DISEASE_STAT0,
                            n_CM_pa_DISEASE_STAT0,
                            n_CM_pasum_DISEASE_STAT0,
                            n_noCM_pa_DISEASE_STAT1,
                            n_CM_pa_DISEASE_STAT1,
                            n_CM_pa_DISEASE_STAT1,
                            n_noCM_pa_DISEASE_STAT2,
                            n_CM_pa_DISEASE_STAT2,
                            n_CM_pasum_DISEASE_STAT2,
                            n_noCM_pa_DISEASE_STAT3,
                            n_CM_pa_DISEASE_STAT3,
                            n_CM_pasum_DISEASE_STAT3,
                            n_noCM_pa_DISEASE_STATsum,
                            n_CM_pa_DISEASE_STATsum,
                            n_CM_pasum_DISEASE_STATsum,
                            n_noCM_ea_DISEASE_STAT0,
                            n_CM_ea_DISEASE_STAT0,
                            n_CM_easum_DISEASE_STAT0,
                            n_noCM_ea_DISEASE_STAT1,
                            n_CM_ea_DISEASE_STAT1,
                            n_CM_easum_DISEASE_STAT1,
                            n_noCM_ea_DISEASE_STAT2,
                            n_CM_ea_DISEASE_STAT2,
                            n_CM_easum_DISEASE_STAT2,
                            n_noCM_ea_DISEASE_STAT3,
                            n_CM_ea_DISEASE_STAT3,
                            n_CM_easum_DISEASE_STAT3,
                            n_noCM_ea_DISEASE_STATsum,
                            n_CM_ea_DISEASE_STATsum,
                            n_CM_easum_DISEASE_STATsum,
                            n_noCM_sa_DISEASE_STAT0,
                            n_CM_sa_DISEASE_STAT0,
                            n_CM_sasum_DISEASE_STAT0,
                            n_noCM_sa_DISEASE_STAT1,
                            n_CM_sa_DISEASE_STAT1,
                            n_CM_sasum_DISEASE_STAT1,
                            n_noCM_sa_DISEASE_STAT2,
                            n_CM_sa_DISEASE_STAT2,
                            n_CM_sasum_DISEASE_STAT2,
                            n_noCM_sa_DISEASE_STAT3,
                            n_CM_sa_DISEASE_STAT3,
                            n_CM_sasum_DISEASE_STAT3,
                            n_noCM_sa_DISEASE_STATsum,
                            n_CM_sa_DISEASE_STATsum,
                            n_CM_sasum_DISEASE_STATsum,
                            Estimate_CM_pa_DISEASE_STAT1,
                            Std.Error_CM_pa_DISEASE_STAT1,
                            CI_LL_logOR_CM_pa_DISEASE_STAT1,
                            CI_UL_logOR_CM_pa_DISEASE_STAT1,
                            Estimate_CM_pa_DISEASE_STAT2,
                            Std.Error_CM_pa_DISEASE_STAT2,
                            CI_LL_logOR_CM_pa_DISEASE_STAT2,
                            CI_UL_logOR_CM_pa_DISEASE_STAT2,
                            Estimate_CM_pa_DISEASE_STAT3,
                            Std.Error_CM_pa_DISEASE_STAT3,
                            CI_LL_logOR_CM_pa_DISEASE_STAT3,
                            CI_UL_logOR_CM_pa_DISEASE_STAT3,
                            Estimate_CM_ea_DISEASE_STAT1,
                            Std.Error_CM_ea_DISEASE_STAT1,
                            CI_LL_logOR_CM_ea_DISEASE_STAT1,
                            CI_UL_logOR_CM_ea_DISEASE_STAT1,
                            Estimate_CM_ea_DISEASE_STAT2,
                            Std.Error_CM_ea_DISEASE_STAT2,
                            CI_LL_logOR_CM_ea_DISEASE_STAT2,
                            CI_UL_logOR_CM_ea_DISEASE_STAT2,
                            Estimate_CM_ea_DISEASE_STAT3,
                            Std.Error_CM_ea_DISEASE_STAT3,
                            CI_LL_logOR_CM_ea_DISEASE_STAT3,
                            CI_UL_logOR_CM_ea_DISEASE_STAT3,
                            Estimate_CM_sa_DISEASE_STAT1,
                            Std.Error_CM_sa_DISEASE_STAT1,
                            CI_LL_logOR_CM_sa_DISEASE_STAT1,
                            CI_UL_logOR_CM_sa_DISEASE_STAT1,
                            Estimate_CM_sa_DISEASE_STAT2,
                            Std.Error_CM_sa_DISEASE_STAT2,
                            CI_LL_logOR_CM_sa_DISEASE_STAT2,
                            CI_UL_logOR_CM_sa_DISEASE_STAT2,
                            Estimate_CM_sa_DISEASE_STAT3,
                            Std.Error_CM_sa_DISEASE_STAT3,
                            CI_LL_logOR_CM_sa_DISEASE_STAT3,
                            CI_UL_logOR_CM_sa_DISEASE_STAT3,
                            Estimate_Age_DISEASE_STAT1,
                            Std.Error_Age_DISEASE_STAT1,
                            CI_LL_logOR_Age_DISEASE_STAT1,
                            CI_UL_logOR_Age_DISEASE_STAT1,
                            Estimate_Sex_DISEASE_STAT1,
                            Std.Error_Sex_DISEASE_STAT1,
                            CI_LL_logOR_Sex_DISEASE_STAT1,
                            CI_UL_logOR_Sex_DISEASE_STAT1,
                            Estimate_Educ1_DISEASE_STAT1,
                            Std.Error_Educ1_DISEASE_STAT1,
                            CI_LL_logOR_Educ1_DISEASE_STAT1,
                            CI_UL_logOR_Educ1_DISEASE_STAT1,
                            Estimate_Educ2_DISEASE_STAT1,
                            Std.Error_Educ2_DISEASE_STAT1,
                            CI_LL_logOR_Educ2_DISEASE_STAT1,
                            CI_UL_logOR_Educ2_DISEASE_STAT1,
                            Estimate_Age_DISEASE_STAT2,
                            Std.Error_Age_DISEASE_STAT2,
                            CI_LL_logOR_Age_DISEASE_STAT2,
                            CI_UL_logOR_Age_DISEASE_STAT2,
                            Estimate_Sex_DISEASE_STAT2,
                            Std.Error_Sex_DISEASE_STAT2,
                            CI_LL_logOR_Sex_DISEASE_STAT2,
                            CI_UL_logOR_Sex_DISEASE_STAT2,
                            Estimate_Educ1_DISEASE_STAT2,
                            Std.Error_Educ1_DISEASE_STAT2,
                            CI_LL_logOR_Educ1_DISEASE_STAT2,
                            CI_UL_logOR_Educ1_DISEASE_STAT2,
                            Estimate_Educ2_DISEASE_STAT2,
                            Std.Error_Educ2_DISEASE_STAT2,
                            CI_LL_logOR_Educ2_DISEASE_STAT2,
                            CI_UL_logOR_Educ2_DISEASE_STAT2,
                            Estimate_Age_DISEASE_STAT3,
                            Std.Error_Age_DISEASE_STAT3,
                            CI_LL_logOR_Age_DISEASE_STAT3,
                            CI_UL_logOR_Age_DISEASE_STAT3,
                            Estimate_Sex_DISEASE_STAT3,
                            Std.Error_Sex_DISEASE_STAT3,
                            CI_LL_logOR_Sex_DISEASE_STAT3,
                            CI_UL_logOR_Sex_DISEASE_STAT3,
                            Estimate_Educ1_DISEASE_STAT3,
                            Std.Error_Educ1_DISEASE_STAT3,
                            CI_LL_logOR_Educ1_DISEASE_STAT3,
                            CI_UL_logOR_Educ1_DISEASE_STAT3,
                            Estimate_Educ2_DISEASE_STAT3,
                            Std.Error_Educ2_DISEASE_STAT3,
                            CI_LL_logOR_Educ2_DISEASE_STAT3,
                            CI_UL_logOR_Educ2_DISEASE_STAT3)
  return(Stats_model)
}

multinomial_multilevelspredictormodels_stat <- function(model){
  #Select only complete cases in the dataset
  mydata <- Sample[,all.vars(formula(model))]
  mydata <- mydata[complete.cases(mydata),]
  # Create objects to store desired model characteristics
  vif_CM_pa <- car::vif(model)['CM_sev','GVIF']
  N <- nrow(mydata)
  n_males <- nrow(mydata[mydata$Sex == 0,])
  mean_age <- mean(mydata$Age,na.rm=T)
  median_age <- median(mydata$Age,na.rm = T)
  sd_age <- sd(mydata$Age,na.rm = T)
  n_educ0 <- table(mydata$Educ)[1]
  n_educ1 <- table(mydata$Educ)[2]
  n_educ2 <- table(mydata$Educ)[3]
  source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
  crosstab <- crosstab(mydata, row.vars = "CM_sev", col.vars = "DISEASE_STAT", type = "f")
  n_CM_sev0_DISEASE_STAT0 <- crosstab$crosstab['0','0']
  n_CM_sev1_DISEASE_STAT0 <- crosstab$crosstab['1','0']
  n_CM_sev2_DISEASE_STAT0 <- crosstab$crosstab['2','0']
  n_CM_sevsum_DISEASE_STAT0 <- crosstab$crosstab['Sum','0']
  n_CM_sev0_DISEASE_STAT1 <- crosstab$crosstab['0','1']
  n_CM_sev1_DISEASE_STAT1 <- crosstab$crosstab['1','1']
  n_CM_sev2_DISEASE_STAT1 <- crosstab$crosstab['2','1']
  n_CM_sevsum_DISEASE_STAT1 <- crosstab$crosstab['Sum','1']
  n_CM_sev0_DISEASE_STAT2 <- crosstab$crosstab['0','2']
  n_CM_sev1_DISEASE_STAT2 <- crosstab$crosstab['1','2']
  n_CM_sev2_DISEASE_STAT2 <- crosstab$crosstab['2','2']
  n_CM_sevsum_DISEASE_STAT2 <- crosstab$crosstab['Sum','2']
  n_CM_sev0_DISEASE_STAT3 <- crosstab$crosstab['0','3']
  n_CM_sev1_DISEASE_STAT3 <- crosstab$crosstab['1','3']
  n_CM_sev2_DISEASE_STAT3 <- crosstab$crosstab['2','3']
  n_CM_sevsum_DISEASE_STAT3 <- crosstab$crosstab['Sum','3']
  n_CM_sev0_DISEASE_STATsum <- crosstab$crosstab['0','Sum']
  n_CM_sev1_DISEASE_STATsum <- crosstab$crosstab['1','Sum']
  n_CM_sev2_DISEASE_STATsum <- crosstab$crosstab['2','Sum']
  n_CM_sevsum_DISEASE_STATsum <- crosstab$crosstab['Sum','Sum']
  # Record coefficient estimates log(OR), standard errors, and 95% CI of log(OR) of predictors' effects on the three levels of the outcome variable
  Estimate_CM_sev1_DISEASE_STAT1 <- summary(model)$coefficients[1,'CM_sev1']
  Std.Error_CM_sev1_DISEASE_STAT1 <- summary(model)$standard.errors[1,'CM_sev1']
  CI_LL_logOR_CM_sev1_DISEASE_STAT1 <- confint(model16)['CM_sev1',1,1]
  CI_UL_logOR_CM_sev1_DISEASE_STAT1 <- confint(model)['CM_sev1',2,1]
  Estimate_CM_sev1_DISEASE_STAT2 <- summary(model)$coefficients[2,'CM_sev1']
  Std.Error_CM_sev1_DISEASE_STAT2 <- summary(model)$standard.errors[2,'CM_sev1']
  CI_LL_logOR_CM_sev1_DISEASE_STAT2 <- confint(model)['CM_sev1',1,2]
  CI_UL_logOR_CM_sev1_DISEASE_STAT2 <- confint(model)['CM_sev1',2,2]
  Estimate_CM_sev1_DISEASE_STAT3 <- summary(model)$coefficients[3,'CM_sev1']
  Std.Error_CM_sev1_DISEASE_STAT3 <- summary(model)$standard.errors[3,'CM_sev1']
  CI_LL_logOR_CM_sev1_DISEASE_STAT3 <- confint(model)['CM_sev1',1,3]
  CI_UL_logOR_CM_sev1_DISEASE_STAT3 <- confint(model)['CM_sev1',2,3]
  Estimate_CM_sev2_DISEASE_STAT1 <- summary(model)$coefficients[1,'CM_sev2']
  Std.Error_CM_sev2_DISEASE_STAT1 <- summary(model)$standard.errors[1,'CM_sev2']
  CI_LL_logOR_CM_sev2_DISEASE_STAT1 <- confint(model)['CM_sev2',1,1]
  CI_UL_logOR_CM_sev2_DISEASE_STAT1 <- confint(model)['CM_sev2',2,1]
  Estimate_CM_sev2_DISEASE_STAT2 <- summary(model)$coefficients[2,'CM_sev2']
  Std.Error_CM_sev2_DISEASE_STAT2 <- summary(model)$standard.errors[2,'CM_sev2']
  CI_LL_logOR_CM_sev2_DISEASE_STAT2 <- confint(model)['CM_sev2',1,2]
  CI_UL_logOR_CM_sev2_DISEASE_STAT2 <- confint(model)['CM_sev2',2,2]
  Estimate_CM_sev2_DISEASE_STAT3 <- summary(model)$coefficients[3,'CM_sev2']
  Std.Error_CM_sev2_DISEASE_STAT3 <- summary(model)$standard.errors[3,'CM_sev2']
  CI_LL_logOR_CM_sev2_DISEASE_STAT3 <- confint(model)['CM_sev2',1,3]
  CI_UL_logOR_CM_sev2_DISEASE_STAT3 <- confint(model)['CM_sev2',2,3]
  Estimate_Age_DISEASE_STAT1 <- summary(model)$coefficients['1','Age']
  Std.Error_Age_DISEASE_STAT1 <- summary(model)$standard.errors[1,'Age']
  CI_LL_logOR_Age_DISEASE_STAT1 <- confint(model)['Age',1,1]
  CI_UL_logOR_Age_DISEASE_STAT1 <- confint(model)['Age',2,1]
  Estimate_Sex_DISEASE_STAT1 <- summary(model)$coefficients['1','Sex1']
  Std.Error_Sex_DISEASE_STAT1 <- summary(model)$standard.errors[1,'Sex1']
  CI_LL_logOR_Sex_DISEASE_STAT1 <- confint(model)['Sex1',1,1]
  CI_UL_logOR_Sex_DISEASE_STAT1 <- confint(model)['Sex1',2,1]
  Estimate_Educ1_DISEASE_STAT1 <- summary(model)$coefficients['1','Educ1']
  Std.Error_Educ1_DISEASE_STAT1 <- summary(model)$standard.errors[1,'Educ1']
  CI_LL_logOR_Educ1_DISEASE_STAT1 <- confint(model)['Educ1',1,1]
  CI_UL_logOR_Educ1_DISEASE_STAT1 <- confint(model)['Educ1',2,1]
  Estimate_Educ2_DISEASE_STAT1 <- summary(model)$coefficients['1','Educ2']
  Std.Error_Educ2_DISEASE_STAT1 <- summary(model)$standard.errors['1','Educ2']
  CI_LL_logOR_Educ2_DISEASE_STAT1 <- confint(model)['Educ2',1,1]
  CI_UL_logOR_Educ2_DISEASE_STAT1 <- confint(model)['Educ2',2,1]
  Estimate_Age_DISEASE_STAT2 <- summary(model)$coefficients['2','Age']
  Std.Error_Age_DISEASE_STAT2 <- summary(model)$standard.errors[2,'Age']
  CI_LL_logOR_Age_DISEASE_STAT2 <- confint(model)['Age',1,2]
  CI_UL_logOR_Age_DISEASE_STAT2 <- confint(model)['Age',2,2]
  Estimate_Sex_DISEASE_STAT2 <- summary(model)$coefficients['2','Sex1']
  Std.Error_Sex_DISEASE_STAT2 <- summary(model)$standard.errors[2,'Sex1']
  CI_LL_logOR_Sex_DISEASE_STAT2 <- confint(model)['Sex1',1,2]
  CI_UL_logOR_Sex_DISEASE_STAT2 <- confint(model)['Sex1',2,2]
  Estimate_Educ1_DISEASE_STAT2 <- summary(model)$coefficients['2','Educ1']
  Std.Error_Educ1_DISEASE_STAT2 <- summary(model)$standard.errors[2,'Educ1']
  CI_LL_logOR_Educ1_DISEASE_STAT2 <- confint(model)['Educ1',1,2]
  CI_UL_logOR_Educ1_DISEASE_STAT2 <- confint(model)['Educ1',2,2]
  Estimate_Educ2_DISEASE_STAT2 <- summary(model)$coefficients['2','Educ2']
  Std.Error_Educ2_DISEASE_STAT2 <- summary(model)$standard.errors['2','Educ2']
  CI_LL_logOR_Educ2_DISEASE_STAT2 <- confint(model)['Educ2',1,2]
  CI_UL_logOR_Educ2_DISEASE_STAT2 <- confint(model)['Educ2',2,2]
  Estimate_Age_DISEASE_STAT3 <- summary(model)$coefficients['3','Age']
  Std.Error_Age_DISEASE_STAT3 <- summary(model)$standard.errors[3,'Age']
  CI_LL_logOR_Age_DISEASE_STAT3 <- confint(model)['Age',1,3]
  CI_UL_logOR_Age_DISEASE_STAT3 <- confint(model)['Age',2,3]
  Estimate_Sex_DISEASE_STAT3 <- summary(model)$coefficients['3','Sex1']
  Std.Error_Sex_DISEASE_STAT3 <- summary(model)$standard.errors[3,'Sex1']
  CI_LL_logOR_Sex_DISEASE_STAT3 <- confint(model)['Sex1',1,3]
  CI_UL_logOR_Sex_DISEASE_STAT3 <- confint(model)['Sex1',2,3]
  Estimate_Educ1_DISEASE_STAT3 <- summary(model)$coefficients['3','Educ1']
  Std.Error_Educ1_DISEASE_STAT3 <- summary(model)$standard.errors[3,'Educ1']
  CI_LL_logOR_Educ1_DISEASE_STAT3 <- confint(model)['Educ1',1,3]
  CI_UL_logOR_Educ1_DISEASE_STAT3 <- confint(model)['Educ1',2,3]
  Estimate_Educ2_DISEASE_STAT3 <- summary(model)$coefficients['3','Educ2']
  Std.Error_Educ2_DISEASE_STAT3 <- summary(model)$standard.errors['3','Educ2']
  CI_LL_logOR_Educ2_DISEASE_STAT3 <- confint(model)['Educ2',1,3]
  CI_UL_logOR_Educ2_DISEASE_STAT3 <- confint(model)['Educ2',2,3]
  # Gather all model information in dataframe
  Stats_model <- data.frame(vif_CM_pa,
                            vif_CM_ea,
                            vif_CM_sa,
                            N,
                            n_males,
                            mean_age,
                            median_age,
                            sd_age,
                            n_educ0,
                            n_educ1,
                            n_educ2,
                            n_CM_sev0_DISEASE_STAT0,
                            n_CM_sev1_DISEASE_STAT0,
                            n_CM_sev2_DISEASE_STAT0,
                            n_CM_sevsum_DISEASE_STAT0,
                            n_CM_sev0_DISEASE_STAT1,
                            n_CM_sev1_DISEASE_STAT1,
                            n_CM_sev2_DISEASE_STAT1,
                            n_CM_sevsum_DISEASE_STAT1,
                            n_CM_sev0_DISEASE_STAT2,
                            n_CM_sev1_DISEASE_STAT2,
                            n_CM_sev2_DISEASE_STAT2,
                            n_CM_sevsum_DISEASE_STAT2,
                            n_CM_sev0_DISEASE_STAT3,
                            n_CM_sev1_DISEASE_STAT3,
                            n_CM_sev2_DISEASE_STAT3,
                            n_CM_sevsum_DISEASE_STAT3,
                            n_CM_sev0_DISEASE_STATsum,
                            n_CM_sev1_DISEASE_STATsum,
                            n_CM_sev2_DISEASE_STATsum,
                            n_CM_sevsum_DISEASE_STATsum,
                            Estimate_CM_sev1_DISEASE_STAT1,
                            Std.Error_CM_sev1_DISEASE_STAT1,
                            CI_LL_logOR_CM_sev1_DISEASE_STAT1,
                            CI_UL_logOR_CM_sev1_DISEASE_STAT1,
                            Estimate_CM_sev1_DISEASE_STAT2,
                            Std.Error_CM_sev1_DISEASE_STAT2,
                            CI_LL_logOR_CM_sev1_DISEASE_STAT2,
                            CI_UL_logOR_CM_sev1_DISEASE_STAT2,
                            Estimate_CM_sev1_DISEASE_STAT3,
                            Std.Error_CM_sev1_DISEASE_STAT3,
                            CI_LL_logOR_CM_sev1_DISEASE_STAT3,
                            CI_UL_logOR_CM_sev1_DISEASE_STAT3,
                            Estimate_CM_sev2_DISEASE_STAT1,
                            Std.Error_CM_sev2_DISEASE_STAT1,
                            CI_LL_logOR_CM_sev2_DISEASE_STAT1,
                            CI_UL_logOR_CM_sev2_DISEASE_STAT1,
                            Estimate_CM_sev2_DISEASE_STAT2,
                            Std.Error_CM_sev2_DISEASE_STAT2,
                            CI_LL_logOR_CM_sev2_DISEASE_STAT2,
                            CI_UL_logOR_CM_sev2_DISEASE_STAT2,
                            Estimate_CM_sev2_DISEASE_STAT3,
                            Std.Error_CM_sev2_DISEASE_STAT3,
                            CI_LL_logOR_CM_sev2_DISEASE_STAT3,
                            CI_UL_logOR_CM_sev2_DISEASE_STAT3,
                            Estimate_Age_DISEASE_STAT1,
                            Std.Error_Age_DISEASE_STAT1,
                            CI_LL_logOR_Age_DISEASE_STAT1,
                            CI_UL_logOR_Age_DISEASE_STAT1,
                            Estimate_Sex_DISEASE_STAT1,
                            Std.Error_Sex_DISEASE_STAT1,
                            CI_LL_logOR_Sex_DISEASE_STAT1,
                            CI_UL_logOR_Sex_DISEASE_STAT1,
                            Estimate_Educ1_DISEASE_STAT1,
                            Std.Error_Educ1_DISEASE_STAT1,
                            CI_LL_logOR_Educ1_DISEASE_STAT1,
                            CI_UL_logOR_Educ1_DISEASE_STAT1,
                            Estimate_Educ2_DISEASE_STAT1,
                            Std.Error_Educ2_DISEASE_STAT1,
                            CI_LL_logOR_Educ2_DISEASE_STAT1,
                            CI_UL_logOR_Educ2_DISEASE_STAT1,
                            Estimate_Age_DISEASE_STAT2,
                            Std.Error_Age_DISEASE_STAT2,
                            CI_LL_logOR_Age_DISEASE_STAT2,
                            CI_UL_logOR_Age_DISEASE_STAT2,
                            Estimate_Sex_DISEASE_STAT2,
                            Std.Error_Sex_DISEASE_STAT2,
                            CI_LL_logOR_Sex_DISEASE_STAT2,
                            CI_UL_logOR_Sex_DISEASE_STAT2,
                            Estimate_Educ1_DISEASE_STAT2,
                            Std.Error_Educ1_DISEASE_STAT2,
                            CI_LL_logOR_Educ1_DISEASE_STAT2,
                            CI_UL_logOR_Educ1_DISEASE_STAT2,
                            Estimate_Educ2_DISEASE_STAT2,
                            Std.Error_Educ2_DISEASE_STAT2,
                            CI_LL_logOR_Educ2_DISEASE_STAT2,
                            CI_UL_logOR_Educ2_DISEASE_STAT2,
                            Estimate_Age_DISEASE_STAT3,
                            Std.Error_Age_DISEASE_STAT3,
                            CI_LL_logOR_Age_DISEASE_STAT3,
                            CI_UL_logOR_Age_DISEASE_STAT3,
                            Estimate_Sex_DISEASE_STAT3,
                            Std.Error_Sex_DISEASE_STAT3,
                            CI_LL_logOR_Sex_DISEASE_STAT3,
                            CI_UL_logOR_Sex_DISEASE_STAT3,
                            Estimate_Educ1_DISEASE_STAT3,
                            Std.Error_Educ1_DISEASE_STAT3,
                            CI_LL_logOR_Educ1_DISEASE_STAT3,
                            CI_UL_logOR_Educ1_DISEASE_STAT3,
                            Estimate_Educ2_DISEASE_STAT3,
                            Std.Error_Educ2_DISEASE_STAT3,
                            CI_LL_logOR_Educ2_DISEASE_STAT3,
                            CI_UL_logOR_Educ2_DISEASE_STAT3)
  return(Stats_model)
}



#################### 4. Model #1 #################### 

Sample$MDD <- relevel(Sample$MDD, ref='0')
model1 <- glm(MDD ~ CM + Age + Sex + Educ, family = binomial(link='logit'), data = Sample)
Cohort1_model1 <- logisticmodels_stat(model1)

# Export dataframe of summary statistics of model #1 into a csv file 
write.csv(Cohort1_model1,"~/.../Cohort1_model1.csv", row.names = TRUE)

# Test assumptions for logistic regression (source: http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/)
# Linearity assumption
probabilities1 <- predict(model1, type = "response", na.rm=T)
mydata1 <- Sample[,c('MDD','CM','Age','Sex','Educ')]
mydata1 <- mydata1[complete.cases(mydata1),]
mydata1_num <- mydata1 %>% dplyr::select_if(is.numeric)
predictors1 <- colnames(mydata1_num)
mydata1_num <- mydata1_num %>%
  mutate(logit = log(probabilities1/(1-probabilities1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
ggplot(mydata1_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, Model #1")



#################### 5. Model #2, #2a(2_Diab) and #2b(2_CVD1) #################### 

Sample$CMD <- relevel(Sample$CMD, ref='0')
model2 <- glm(CMD ~ CM + Age + Sex + Educ, family = binomial(link='logit'), data = Sample)
Cohort1_model2 <- logisticmodels_stat(model2)
write.csv(Cohort1_model2,"~/.../Cohort1_model2.csv", row.names = TRUE)

Sample$DM <- relevel(Sample$DM, ref='0')
model2_Diab <- glm(DM ~ CM + Age + Sex + Educ, family = binomial(link='logit'), data = Sample)
Cohort1_model2_Diab <- logisticmodels_stat(model2_Diab)
write.csv(Cohort1_model2_Diab,"~/.../Cohort1_model2a.csv", row.names = TRUE)

Sample$CVD1 <- relevel(Sample$CVD1, ref='0')
model2_CVD1 <- glm(CVD1 ~ CM + Age + Sex + Educ, family = binomial(link='logit'), data = Sample)
Cohort1_model2_CVD1 <- logisticmodels_stat(model2_CVD1)
write.csv(Cohort1_model2_CVD1,"~/.../Cohort1_model2b.csv", row.names = TRUE)

# Test assumptions for logistic regression
# Linearity assumption model 2
probabilities2 <- predict(model2, type = "response", na.rm=T)
mydata2 <- Sample[,c('CMD','CM','Age','Sex','Educ')]
mydata2 <- mydata2[complete.cases(mydata2),]
mydata2_num <- mydata2 %>% dplyr::select_if(is.numeric)
predictors2 <- colnames(mydata2_num)
mydata2_num <- mydata2_num %>%
  mutate(logit = log(probabilities2/(1-probabilities2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
ggplot(mydata2_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, Model #2")

# Linearity assumption model 2a - outcome DM only
probabilities2_Diab <- predict(model2_Diab, type = "response", na.rm=T)
mydata2_Diab <- Sample[,c('DM','CM','Age','Sex','Educ')]
mydata2_Diab <- mydata2_Diab[complete.cases(mydata2_Diab),]
mydata2_Diab_num <- mydata2_Diab %>% dplyr::select_if(is.numeric)
predictors2_Diab <- colnames(mydata2_Diab_num)
mydata2_Diab_num <- mydata2_Diab_num %>%
  mutate(logit = log(probabilities2_Diab/(1-probabilities2_Diab))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
ggplot(mydata2_Diab_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, Model #2, outcome DM only")

# Linearity assumption model 2b - outcome CVD1 only
probabilities2_CVD1 <- predict(model2_CVD1, type = "response", na.rm=T)
mydata2_CVD1 <- Sample[,c('CVD1','CM','Age','Sex','Educ')]
mydata2_CVD1 <- mydata2_CVD1[complete.cases(mydata2_CVD1),]
mydata2_CVD1_num <- mydata2_CVD1 %>% dplyr::select_if(is.numeric)
predictors2_CVD1 <- colnames(mydata2_CVD1_num)
mydata2_CVD1_num <- mydata2_CVD1_num %>%
  mutate(logit = log(probabilities2_CVD1/(1-probabilities2_CVD1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
ggplot(mydata2_CVD1_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, Model #2, outcome CVD1 only")



#################### 6. Model #3 #################### 

Sample$DISEASE_STAT <- relevel(Sample$DISEASE_STAT, ref='0')
model3 <- multinom(DISEASE_STAT ~ CM + Age + Sex + Educ , data = Sample)
Cohort1_model3 <- multinomialmodels_stat(model3)
write.csv(Cohort1_model3,"~/.../Cohort1_model3.csv", row.names = TRUE)

# Test assumptions for multinomial regression
# Linearity assumption
probabilities3 <- predict(model3, type = "prob", na.rm=T)
probablities3_DISEASE_STAT1 <- probabilities3[,'1']
probablities3_DISEASE_STAT2 <- probabilities3[,'2']
probablities3_DISEASE_STAT3 <- probabilities3[,'3']
mydata3 <- Sample[,c('DISEASE_STAT','CM','Age','Sex','Educ')]
mydata3 <- mydata3[complete.cases(mydata3),]
mydata3_num <- mydata3 %>% dplyr::select_if(is.numeric)
predictors3 <- colnames(mydata3_num)

mydata3_DISEASE_STAT1Age_num <- data.frame(mydata3_num[,'Age']) %>%
  mutate(logit = log(probablities3_DISEASE_STAT1/(1-probablities3_DISEASE_STAT1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata3_DISEASE_STAT2Age_num <- data.frame(mydata3_num[,'Age']) %>%
  mutate(logit = log(probablities3_DISEASE_STAT2/(1-probablities3_DISEASE_STAT2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata3_DISEASE_STAT3Age_num <- data.frame(mydata3_num[,'Age']) %>%
  mutate(logit = log(probablities3_DISEASE_STAT3/(1-probablities3_DISEASE_STAT3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata3_DISEASE_STAT1Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT = 1, Model #3")                        
ggplot(mydata3_DISEASE_STAT2Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT = 2, Model #3")                        
ggplot(mydata3_DISEASE_STAT3Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT = 3, Model #3") 



#################### 7. Model #4 #################### 

Sample$DISEASE_STAT <- relevel(Sample$DISEASE_STAT, ref='0')
model4 <- multinom(DISEASE_STAT ~ CM + Age + Sex + Educ + Smoke + PhyAct + Alcohol, data = Sample)
Cohort1_model4 <- multinomialmodels_stat(model4)
write.csv(Cohort1_model4,"~/.../Cohort1_model4.csv", row.names = TRUE)

# Test assumptions for multinomial regression
# Linearity assumption
probabilities4 <- predict(model4, type = "prob", na.rm=T)
probablities4_DISEASE_STAT1 <- probabilities4[,'1']
probablities4_DISEASE_STAT2 <- probabilities4[,'2']
probablities4_DISEASE_STAT3 <- probabilities4[,'3']
mydata4 <- Sample[,c('DISEASE_STAT','CM','Age','Sex','Educ','Smoke','PhyAct','Alcohol')]
mydata4 <- mydata4[complete.cases(mydata4),]
mydata4_num <- mydata4 %>% dplyr::select_if(is.numeric)
predictors4 <- colnames(mydata4_num)

mydata4_DISEASE_STAT1Age_num <- data.frame(mydata4_num[,'Age']) %>%
  mutate(logit = log(probablities4_DISEASE_STAT1/(1-probablities4_DISEASE_STAT1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata4_DISEASE_STAT1PhyAct_num <- data.frame(mydata4_num[,'PhyAct']) %>%
  mutate(logit = log(probablities4_DISEASE_STAT1/(1-probablities4_DISEASE_STAT1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata4_DISEASE_STAT1Alcohol_num <- data.frame(mydata4_num[,'Alcohol']) %>%
  mutate(logit = log(probablities4_DISEASE_STAT1/(1-probablities4_DISEASE_STAT1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata4_DISEASE_STAT2Age_num <- data.frame(mydata4_num[,'Age']) %>%
  mutate(logit = log(probablities4_DISEASE_STAT2/(1-probablities4_DISEASE_STAT2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata4_DISEASE_STAT2PhyAct_num <- data.frame(mydata4_num[,'PhyAct']) %>%
  mutate(logit = log(probablities4_DISEASE_STAT2/(1-probablities4_DISEASE_STAT2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata4_DISEASE_STAT2Alcohol_num <- data.frame(mydata4_num[,'Alcohol']) %>%
  mutate(logit = log(probablities4_DISEASE_STAT2/(1-probablities4_DISEASE_STAT2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata4_DISEASE_STAT3Age_num <- data.frame(mydata4_num[,'Age']) %>%
  mutate(logit = log(probablities4_DISEASE_STAT3/(1-probablities4_DISEASE_STAT3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata4_DISEASE_STAT3PhyAct_num <- data.frame(mydata4_num[,'PhyAct']) %>%
  mutate(logit = log(probablities4_DISEASE_STAT3/(1-probablities4_DISEASE_STAT3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata4_DISEASE_STAT3Alcohol_num <- data.frame(mydata4_num[,'Alcohol']) %>%
  mutate(logit = log(probablities4_DISEASE_STAT3/(1-probablities4_DISEASE_STAT3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata4_DISEASE_STAT1Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT = 1, Model #4")                        
ggplot(mydata4_DISEASE_STAT1PhyAct_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="PhyAct per logit, at DISEASE_STAT = 1, Model #4")                     
ggplot(mydata4_DISEASE_STAT1Alcohol_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Alcohol per logit, at DISEASE_STAT = 1, Model #4")                    

ggplot(mydata4_DISEASE_STAT2Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT = 2, Model #4")                        
ggplot(mydata4_DISEASE_STAT2PhyAct_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="PhyAct per logit, at DISEASE_STAT = 2, Model #4")                     
ggplot(mydata4_DISEASE_STAT2Alcohol_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Alcohol per logit, at DISEASE_STAT = 2, Model #4")                    

ggplot(mydata4_DISEASE_STAT3Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT = 3, Model #4")                        
ggplot(mydata4_DISEASE_STAT3PhyAct_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="PhyAct per logit, at DISEASE_STAT = 3, Model #4")                     
ggplot(mydata4_DISEASE_STAT3Alcohol_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Alcohol per logit, at DISEASE_STAT = 3, Model #4")                    



#################### 8. Models #5a and #5b #################### 

# Model #5a - in males only
Sample_males$DISEASE_STAT <- relevel(Sample_males$DISEASE_STAT, ref='0')
model5a <- multinom(DISEASE_STAT ~ CM + Age + Sex + Educ, data = Sample_males)
Cohort1_model5a <- multinomialmodels_stat(model5a)
write.csv(Cohort1_model5a,"~/.../Cohort1_model5a.csv", row.names = TRUE)

# Test assumptions for multinomial regression 
# Linearity assumption
probabilities5a <- predict(model5a, type = "prob", na.rm=T)
probablities5a_DISEASE_STAT1 <- probabilities5a[,'1']
probablities5a_DISEASE_STAT2 <- probabilities5a[,'2']
probablities5a_DISEASE_STAT3 <- probabilities5a[,'3']
mydata5a <- Sample_males[,c('DISEASE_STAT','CM','Age','Sex','Educ')]
mydata5a <- mydata5a[complete.cases(mydata5a),]
mydata5a_num <- mydata5a %>% dplyr::select_if(is.numeric)
predictors5a <- colnames(mydata5a_num)

mydata5a_DISEASE_STAT1Age_num <- data.frame(mydata5a_num[,'Age']) %>%
  mutate(logit = log(probablities5a_DISEASE_STAT1/(1-probablities5a_DISEASE_STAT1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata5a_DISEASE_STAT2Age_num <- data.frame(mydata5a_num[,'Age']) %>%
  mutate(logit = log(probablities5a_DISEASE_STAT2/(1-probablities5a_DISEASE_STAT2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata5a_DISEASE_STAT3Age_num <- data.frame(mydata5a_num[,'Age']) %>%
  mutate(logit = log(probablities5a_DISEASE_STAT3/(1-probablities5a_DISEASE_STAT3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata5a_DISEASE_STAT1Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT = 1, Model #5a")                       
ggplot(mydata5a_DISEASE_STAT2Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT = 2, Model #5a")                    
ggplot(mydata5a_DISEASE_STAT3Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT = 3, Model #5a")        

# Model #5b - in females only 
Sample_females$DISEASE_STAT <- relevel(Sample_females$DISEASE_STAT, ref='0')
model5b <- multinom(DISEASE_STAT ~ CM + Age + Sex + Educ, data = Sample_females)
Cohort1_model5b <- multinomialmodels_stat(model5b)
write.csv(Cohort1_model5b,"~/.../Cohort1_model5b.csv", row.names = TRUE)

# Test assumptions for multinomial regression
# Linearity assumption
probabilities5b <- predict(model5b, type = "prob", na.rm=T)
probablities5b_DISEASE_STAT1 <- probabilities5b[,'1']
probablities5b_DISEASE_STAT2 <- probabilities5b[,'2']
probablities5b_DISEASE_STAT3 <- probabilities5b[,'3']
mydata5b <- Sample_females[,c('DISEASE_STAT','CM','Age','Sex','Educ')]
mydata5b <- mydata5b[complete.cases(mydata5b),]
mydata5b_num <- mydata5b %>% dplyr::select_if(is.numeric)
predictors5b <- colnames(mydata5b_num)

mydata5b_DISEASE_STAT1Age_num <- data.frame(mydata5b_num[,'Age']) %>%
  mutate(logit = log(probablities5b_DISEASE_STAT1/(1-probablities5b_DISEASE_STAT1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata5b_DISEASE_STAT2Age_num <- data.frame(mydata5b_num[,'Age']) %>%
  mutate(logit = log(probablities5b_DISEASE_STAT2/(1-probablities5b_DISEASE_STAT2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata5b_DISEASE_STAT3Age_num <- data.frame(mydata5b_num[,'Age']) %>%
  mutate(logit = log(probablities5b_DISEASE_STAT3/(1-probablities5b_DISEASE_STAT3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata5b_DISEASE_STAT1Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT = 1, Model #5b")
ggplot(mydata5b_DISEASE_STAT2Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT = 2, Model #5b")
ggplot(mydata5b_DISEASE_STAT3Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT = 3, Model #5b")



#################### 9. Model #6 #################### 
Sample$CM_pa <- relevel(Sample$CM_pa, ref='0')
Sample$CM_ea <- relevel(Sample$CM_ea, ref='0')
Sample$CM_sa <- relevel(Sample$CM_sa, ref='0')
model6 <- multinom(DISEASE_STAT ~ CM_pa + CM_ea + CM_sa + Age + Sex + Educ, data = Sample)
Cohort1_model6 <- multinomial_multippredictorsmodels_stat(model6)
write.csv(Cohort1_model6,"~/.../Cohort1_model6.csv", row.names = TRUE)

# Test assumptions for multinomial regression
# Linearity assumption
probabilities6 <- predict(model6, type = "prob", na.rm=T)
probablities6_DISEASE_STAT1 <- probabilities6[,'1']
probablities6_DISEASE_STAT2 <- probabilities6[,'2']
probablities6_DISEASE_STAT3 <- probabilities6[,'3']
mydata6 <- Sample[,c('DISEASE_STAT','CM_pa','CM_ea', 'CM_sa','Age','Sex','Educ')]
mydata6 <- mydata6[complete.cases(mydata6),]
mydata6_num <- mydata6 %>% dplyr::select_if(is.numeric)
predictors6 <- colnames(mydata6_num)

mydata6_DISEASE_STAT1Age_num <- data.frame(mydata6_num[,'Age']) %>%
  mutate(logit = log(probablities6_DISEASE_STAT1/(1-probablities6_DISEASE_STAT1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata6_DISEASE_STAT2Age_num <- data.frame(mydata6_num[,'Age']) %>%
  mutate(logit = log(probablities6_DISEASE_STAT2/(1-probablities6_DISEASE_STAT2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata6_DISEASE_STAT3Age_num <- data.frame(mydata6_num[,'Age']) %>%
  mutate(logit = log(probablities6_DISEASE_STAT3/(1-probablities6_DISEASE_STAT3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata6_DISEASE_STAT1Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT = 1, Model #6")                   
ggplot(mydata6_DISEASE_STAT2Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT = 2, Model #6")
ggplot(mydata6_DISEASE_STAT3Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT = 3, Model #6")  



#################### 10. Model #7 #################### 
Sample$CM_sev <- relevel(Sample$CM_sev, ref='0')
model7 <- multinom(DISEASE_STAT ~ CM_sev + Age + Sex + Educ, data = Sample)
Cohort1_model7 <- multinomial_multilevelspredictormodels_stat(model7)
write.csv(Cohort1_model7,"~/.../Cohort1_model7.csv", row.names = TRUE)

# Test assumptions for multinomial regression
# Linearity assumption
probabilities7 <- predict(model7, type = "prob", na.rm=T)
probablities7_DISEASE_STAT1 <- probabilities7[,'1']
probablities7_DISEASE_STAT2 <- probabilities7[,'2']
probablities7_DISEASE_STAT3 <- probabilities7[,'3']
mydata7 <- Sample[,c('DISEASE_STAT','CM_sev','Age','Sex','Educ')]
mydata7 <- mydata7[complete.cases(mydata7),]
mydata7_num <- mydata7 %>% dplyr::select_if(is.numeric)
predictors7 <- colnames(mydata7_num)

mydata7_DISEASE_STAT1Age_num <- data.frame(mydata7_num[,'Age']) %>%
  mutate(logit = log(probablities7_DISEASE_STAT1/(1-probablities7_DISEASE_STAT1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata7_DISEASE_STAT2Age_num <- data.frame(mydata7_num[,'Age']) %>%
  mutate(logit = log(probablities7_DISEASE_STAT2/(1-probablities7_DISEASE_STAT2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata7_DISEASE_STAT3Age_num <- data.frame(mydata7_num[,'Age']) %>%
  mutate(logit = log(probablities7_DISEASE_STAT3/(1-probablities7_DISEASE_STAT3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata7_DISEASE_STAT1Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT = 1, Model #7")                   
ggplot(mydata7_DISEASE_STAT2Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT = 2, Model #7")
ggplot(mydata7_DISEASE_STAT3Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT = 3, Model #7")  



#################### 11. Model #8 #################### 

Sample$DISEASE_STAT_CD <- relevel(Sample$DISEASE_STAT_CD, ref='0')
model8 <- multinom(DISEASE_STAT_CD ~ CM + Age + Sex + Educ, data = Sample)
Cohort1_model8 <- multinomialmodels_stat(model8)
write.csv(Cohort1_model8,"~/.../Cohort1_model8.csv", row.names = TRUE)

# Test assumptions for multinomial regression
# Linearity assumption
probabilities8 <- predict(model7, type = "prob", na.rm=T)
probablities8_DISEASE_STAT_CD1 <- probabilities8[,'1']
probablities8_DISEASE_STAT_CD2 <- probabilities8[,'2']
probablities8_DISEASE_STAT_CD3 <- probabilities8[,'3']
mydata8 <- Sample[,c('DISEASE_STAT_CD','CM','Age','Sex','Educ')]
mydata8 <- mydata8[complete.cases(mydata8),]
mydata8_num <- mydata8 %>% dplyr::select_if(is.numeric)
predictors8 <- colnames(mydata8_num)

mydata8_DISEASE_STAT_CD1Age_num <- data.frame(mydata8_num[,'Age']) %>%
  mutate(logit = log(probablities8_DISEASE_STAT_CD1/(1-probablities8_DISEASE_STAT_CD1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata8_DISEASE_STAT_CD2Age_num <- data.frame(mydata8_num[,'Age']) %>%
  mutate(logit = log(probablities8_DISEASE_STAT_CD2/(1-probablities8_DISEASE_STAT_CD2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata8_DISEASE_STAT_CD3Age_num <- data.frame(mydata8_num[,'Age']) %>%
  mutate(logit = log(probablities8_DISEASE_STAT_CD3/(1-probablities8_DISEASE_STAT_CD3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata8_DISEASE_STAT_CD1Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT_CD = 1, Model #8")                        
ggplot(mydata8_DISEASE_STAT_CD2Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT_CD = 2, Model #8")                        
ggplot(mydata8_DISEASE_STAT_CD3Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT_CD = 3, Model #8")                      



#################### 12. Model #9 #################### 

Sample$DISEASE_STAT_CVD2 <- relevel(Sample$DISEASE_STAT_CVD2, ref='0')
model9 <- multinom(DISEASE_STAT_CVD2 ~ CM + Age + Sex + Educ, data = Sample)
Cohort1_model9 <- multinomialmodels_stat(model9)
write.csv(Cohort1_model9,"~/.../Cohort1_model9.csv", row.names = TRUE)

# Test assumptions for multinomial regression
# Linearity assumption
probabilities9 <- predict(model9, type = "prob", na.rm=T)
probablities9_DISEASE_STAT_CVD21 <- probabilities9[,'1']
probablities9_DISEASE_STAT_CVD22 <- probabilities9[,'2']
probablities9_DISEASE_STAT_CVD23 <- probabilities9[,'3']
mydata9 <- Sample[,c('DISEASE_STAT_CVD2','CM','Age','Sex','Educ')]
mydata9 <- mydata9[complete.cases(mydata9),]
mydata9_num <- mydata9 %>% dplyr::select_if(is.numeric)
predictors9 <- colnames(mydata9_num)

mydata9_DISEASE_STAT_CVD21Age_num <- data.frame(mydata9_num[,'Age']) %>%
  mutate(logit = log(probablities9_DISEASE_STAT_CVD21/(1-probablities9_DISEASE_STAT_CVD21))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata9_DISEASE_STAT_CVD22Age_num <- data.frame(mydata9_num[,'Age']) %>%
  mutate(logit = log(probablities9_DISEASE_STAT_CVD22/(1-probablities9_DISEASE_STAT_CVD22))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata9_DISEASE_STAT_CVD23Age_num <- data.frame(mydata9_num[,'Age']) %>%
  mutate(logit = log(probablities9_DISEASE_STAT_CVD23/(1-probablities9_DISEASE_STAT_CVD23))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata9_DISEASE_STAT_CVD21Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT_CVD2 = 1, Model #9")                        
ggplot(mydata9_DISEASE_STAT_CVD22Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT_CVD2 = 2, Model #9")                        
ggplot(mydata9_DISEASE_STAT_CVD23Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT_CVD2 = 3, Model #9")                        



#################### 13. Model #10 #################### 

Sample$DISEASE_STAT_med <- relevel(Sample$DISEASE_STAT_med, ref='0')
model10 <- multinom(DISEASE_STAT_med ~ CM + Age + Sex + Educ, data = Sample)
Cohort1_model10 <- multinomialmodels_stat(model10)
write.csv(Cohort1_model10,"~/.../Cohort1_model10.csv", row.names = TRUE)

# Test assumptions for multinomial regression
# Linearity assumption
probabilities10 <- predict(model10, type = "prob", na.rm=T)
probablities10_DISEASE_STAT_med1 <- probabilities9[,'1']
probablities10_DISEASE_STAT_med2 <- probabilities9[,'2']
probablities10_DISEASE_STAT_med3 <- probabilities9[,'3']
mydata10 <- Sample[,c('DISEASE_STAT_med','CM','Age','Sex','Educ')]
mydata10 <- mydata10[complete.cases(mydata10),]
mydata10_num <- mydata10 %>% dplyr::select_if(is.numeric)
predictors10 <- colnames(mydata10_num)

mydata10_DISEASE_STAT_med1Age_num <- data.frame(mydata10_num[,'Age']) %>%
  mutate(logit = log(probablities10_DISEASE_STAT_med1/(1-probablities10_DISEASE_STAT_med1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata10_DISEASE_STAT_med2Age_num <- data.frame(mydata10_num[,'Age']) %>%
  mutate(logit = log(probablities10_DISEASE_STAT_med2/(1-probablities10_DISEASE_STAT_med2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
mydata10_DISEASE_STAT_med3Age_num <- data.frame(mydata10_num[,'Age']) %>%
  mutate(logit = log(probablities10_DISEASE_STAT_med3/(1-probablities10_DISEASE_STAT_med3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata10_DISEASE_STAT_med1Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT_med = 1, Model #10")                        
ggplot(mydata10_DISEASE_STAT_med2Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT_med = 2, Model #10")                        
ggplot(mydata10_DISEASE_STAT_med3Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at DISEASE_STAT_med = 3, Model #10")                        


#################### END ######################
