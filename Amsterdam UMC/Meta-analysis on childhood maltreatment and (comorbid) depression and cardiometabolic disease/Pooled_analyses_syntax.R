############################# Associations between childhood maltreatment and (comorbid) depression and cardiometabolic disease #############################

#################### 1. Set up ####################
# Set working directory, adjust with own pathway name
setwd(~/"")

# Load necessary packages
library('foreign')
library("metafor")
library('gridExtra')
library(tibble)

#################### 2. Load descriptive statistics ####################
# Load  descriptive statistics of all cohorts
Descriptives_Cohort1 <- read.table(file = "~/.../Cohort1_descriptives.csv", header = T, sep = ",")
# Repeat for all 13 cohorts

Descriptives_all <- rbind(Descriptives_Cohort1,
                          Descriptives_Cohort2, 
                          Descriptives_Cohort3,
                          Descriptives_Cohort4,
                          Descriptives_Cohort5,
                          Descriptives_Cohort6,
                          Descriptives_Cohort7,
                          Descriptives_Cohort8,
                          Descriptives_Cohort9,
                          Descriptives_Cohort10,
                          Descriptives_Cohort11,
                          Descriptives_Cohort12,
                          Descriptives_Cohort13)

rownames(Descriptives_all) <- c('Cohort1',
                                'Cohort2',
                                'Cohort3',
                                'Cohort4',
                                'Cohort5',
                                'Cohort6',
                                'Cohort7',
                                'Cohort8',
                                'Cohort9',
                                'Cohort10',
                                'Cohort11',
                                'Cohort12',
                                'Cohort13')

#################### 3. Load models' summary statistics for each cohort ####################
# Example of cohort 1 (repeat for each cohort)
# Load aggregate data of all models
Cohort1_model1 <- read.table(file = "Cohort1_model1.csv", header = T, sep = ",")
Cohort1_model2 <-  read.table(file = "Cohort1_model2.csv", header = T, sep = ",")
Cohort1_model2a <- read.table(file = "Cohort1_model2_Diab.csv", header = T, sep = ",")
Cohort1_model2b <- read.table(file = "Cohort1_model2_CVD1.csv", header = T, sep = ",")
Cohort1_model3 <- read.table(file = "Cohort1_model3.csv", header = T, sep = ",")
Cohort1_model4 <- read.table(file = "Cohort1_model4.csv", header = T, sep = ",")
Cohort1_model5a <- read.table(file = "Cohort1_model5a.csv", header = T, sep = ",")
Cohort1_model5b <- read.table(file = "Cohort1_model5b.csv", header = T, sep = ",")
Cohort1_model6 <- read.table(file = "Cohort1_model6.csv", header = T, sep = ",")
Cohort1_model7 <- read.table(file = "Cohort1_model7.csv", header = T, sep = ",")
Cohort1_model8 <- read.table(file = "Cohort1_model8.csv", header = T, sep = ",")
Cohort1_model9 <- read.table(file = "Cohort1_model9.csv", header = T, sep = ",")
Cohort1_model10 <- read.table(file = "Cohort1_model10.csv", header = T, sep = ",")

# Create data frame with each model's regression coefficient and standard error of the estimated association
Cohort1 <- as.data.frame(matrix(NA, ncol = 2, nrow = 40))

# Model 1 -  Association between childhood maltreatment and depression
Cohort1[1,1] <- Cohort1_model1$Estimate_CM 
Cohort1[1,2] <- Cohort1_model1$Std.Error_CM
Cohort1_model2_Diab
# Model 2, 2a, and 2b - Associations between childhood maltreatment and cardiometabolic disease (model 2), diabetes mellitus (model 2a) and cardiovascular disease (model 2b)
Cohort1[2,1] <- Cohort1_model2$Estimate_CM 
Cohort1[2,2] <- Cohort1_model2$Std.Error_CM 
Cohort1[3,1] <- Cohort1_model2a$Estimate_CM 
Cohort1[3,2] <- Cohort1_model2a$Std.Error_CM
Cohort1[4,1] <- Cohort1_model2b$Estimate_CM 
Cohort1[4,2] <- Cohort1_model2b$Std.Error_CM 

# Model 3 - Association between childhood maltreatment and disease status (coding of disease status variable: 0 (reference category) = healthy controls vs. 1 = depression only vs. 2 = cardiometabolic disease only vs. 3 = comorbid depression and cardiometabolic disease)
Cohort1[5,1] <- Cohort1_model3$Estimate_CM_DISEASE_STAT1 
Cohort1[5,2] <- Cohort1_model3$Std.Error_CM_DISEASE_STAT1
Cohort1[6,1] <- Cohort1_model3$Estimate_CM_DISEASE_STAT2
Cohort1[6,2] <- Cohort1_model3$Std.Error_CM_DISEASE_STAT2
Cohort1[7,1] <- Cohort1_model3$Estimate_CM_DISEASE_STAT3
Cohort1[7,2] <- Cohort1_model3$Std.Error_CM_DISEASE_STAT3

# Model 4 - Association between childhood maltreatment and disease status, additionally adjusting for lifestyle factors
Cohort1[8,1] <- Cohort1_model4$Estimate_CM_DISEASE_STAT1 
Cohort1[8,2] <- Cohort1_model4$Std.Error_CM_DISEASE_STAT1
Cohort1[9,1] <- Cohort1_model4$Estimate_CM_DISEASE_STAT2
Cohort1[9,2] <- Cohort1_model4$Std.Error_CM_DISEASE_STAT2
Cohort1[10,1] <- Cohort1_model4$Estimate_CM_DISEASE_STAT3
Cohort1[10,2] <- Cohort1_model4$Std.Error_CM_DISEASE_STAT3

# Model 5a - Association between childhood maltreatment and disease status, in males only
Cohort1[11,1] <- Cohort1_model5a$Estimate_CM_DISEASE_STAT1   
Cohort1[11,2] <- Cohort1_model5a$Std.Error_CM_DISEASE_STAT1
Cohort1[12,1] <- Cohort1_model5a$Estimate_CM_DISEASE_STAT2
Cohort1[12,2] <- Cohort1_model5a$Std.Error_CM_DISEASE_STAT2
Cohort1[13,1] <- Cohort1_model5a$Estimate_CM_DISEASE_STAT3
Cohort1[13,2] <- Cohort1_model5a$Std.Error_CM_DISEASE_STAT3 

# Model 5b - Association between childhood maltreatment and disease status, in females only
Cohort1[14,1] <- Cohort1_model5b$Estimate_CM_DISEASE_STAT1  
Cohort1[14,2] <- Cohort1_model5b$Std.Error_CM_DISEASE_STAT1 
Cohort1[15,1] <- Cohort1_model5b$Estimate_CM_DISEASE_STAT2 
Cohort1[15,2] <- Cohort1_model5b$Std.Error_CM_DISEASE_STAT2
Cohort1[16,1] <- Cohort1_model5b$Estimate_CM_DISEASE_STAT3  
Cohort1[16,2] <- Cohort1_model5b$Std.Error_CM_DISEASE_STAT3 

# Model 6 - Associations of physical, emotional, and sexual abuse with disease status
Cohort1[17,1] <- Cohort1_model6$Estimate_CM_pa_DISEASE_STAT1
Cohort1[17,2] <- Cohort1_model6$Std.Error_CM_pa_DISEASE_STAT1
Cohort1[18,1] <- Cohort1_model6$Estimate_CM_pa_DISEASE_STAT2
Cohort1[18,2] <- Cohort1_model6$Std.Error_CM_pa_DISEASE_STAT2
Cohort1[19,1] <- Cohort1_model6$Estimate_CM_pa_DISEASE_STAT3
Cohort1[19,2] <- Cohort1_model6$Std.Error_CM_pa_DISEASE_STAT3

Cohort1[20,1] <- Cohort1_model6$Estimate_CM_ea_DISEASE_STAT1   
Cohort1[20,2] <- Cohort1_model6$Std.Error_CM_ea_DISEASE_STAT1  
Cohort1[21,1] <- Cohort1_model6$Estimate_CM_ea_DISEASE_STAT2 
Cohort1[21,2] <- Cohort1_model6$Std.Error_CM_ea_DISEASE_STAT2
Cohort1[22,1] <- Cohort1_model6$Estimate_CM_ea_DISEASE_STAT3
Cohort1[22,2] <- Cohort1_model6$Std.Error_CM_ea_DISEASE_STAT3

Cohort1[23,1] <- Cohort1_model6$Estimate_CM_sa_DISEASE_STAT1  
Cohort1[23,2] <- Cohort1_model6$Std.Error_CM_sa_DISEASE_STAT1  
Cohort1[24,1] <- Cohort1_model6$Estimate_CM_sa_DISEASE_STAT2 
Cohort1[24,2] <- Cohort1_model6$Std.Error_CM_sa_DISEASE_STAT2
Cohort1[25,1] <- Cohort1_model6$Estimate_CM_sa_DISEASE_STAT3 
Cohort1[25,2] <- Cohort1_model6$Std.Error_CM_sa_DISEASE_STAT3

# Model 7 - Association of childhood maltreatment severity - operationalized with number of types (0 vs. 1 vs 2+ types) - with disease status
Cohort1[26,1] <- Cohort1_model7$Estimate_CM_sev1_DISEASE_STAT1  
Cohort1[26,2] <- Cohort1_model7$Std.Error_CM_sev1_DISEASE_STAT1 
Cohort1[27,1] <- Cohort1_model7$Estimate_CM_sev2_DISEASE_STAT1
Cohort1[27,2] <- Cohort1_model7$Std.Error_CM_sev2_DISEASE_STAT1
Cohort1[28,1] <- Cohort1_model7$Estimate_CM_sev1_DISEASE_STAT2
Cohort1[28,2] <- Cohort1_model7$Std.Error_CM_sev1_DISEASE_STAT2 
Cohort1[29,1] <- Cohort1_model7$Estimate_CM_sev2_DISEASE_STAT2
Cohort1[29,2] <- Cohort1_model7$Std.Error_CM_sev2_DISEASE_STAT2
Cohort1[30,1] <- Cohort1_model7$Estimate_CM_sev1_DISEASE_STAT3
Cohort1[30,2] <- Cohort1_model7$Std.Error_CM_sev1_DISEASE_STAT3 
Cohort1[31,1] <- Cohort1_model7$Estimate_CM_sev2_DISEASE_STAT3
Cohort1[31,2] <- Cohort1_model7$Std.Error_CM_sev2_DISEASE_STAT3

# Model 8 - Association between childhood maltreatment and disease status (based on strict definition of cardiovascular disease)
Cohort1[32,1] <- Cohort1_model8$Estimate_CM_DISEASE_STAT1  
Cohort1[32,2] <- Cohort1_model8$Std.Error_CM_DISEASE_STAT1 
Cohort1[33,1] <- Cohort1_model8$Estimate_CM_DISEASE_STAT2 
Cohort1[33,2] <- Cohort1_model8$Std.Error_CM_DISEASE_STAT2
Cohort1[34,1] <- Cohort1_model8$Estimate_CM_DISEASE_STAT3
Cohort1[34,2] <- Cohort1_model8$Std.Error_CM_DISEASE_STAT3 

# Model 9 - Association between childhood maltreatment and disease status (based on broad definition of cardiovascular disease)
Cohort1[35,1] <- Cohort1_model9$Estimate_CM_DISEASE_STAT1 
Cohort1[35,2] <- Cohort1_model9$Std.Error_CM_DISEASE_STAT1
Cohort1[36,1] <- Cohort1_model9$Estimate_CM_DISEASE_STAT2 
Cohort1[36,2] <- Cohort1_model9$Std.Error_CM_DISEASE_STAT2  
Cohort1[37,1] <- Cohort1_model9$Estimate_CM_DISEASE_STAT3 
Cohort1[37,2] <- Cohort1_model9$Std.Error_CM_DISEASE_STAT3 

# Model 10 - Association between childhood maltreatment and disease status (with medication intake as additional diagnosis criterion)
Cohort1[38,1] <- Cohort1_model10$Estimate_CM_DISEASE_STAT1 
Cohort1[38,2] <- Cohort1_model10$Std.Error_CM_DISEASE_STAT1
Cohort1[39,1] <- Cohort1_model10$Estimate_CM_DISEASE_STAT2 
Cohort1[39,2] <- Cohort1_model10$Std.Error_CM_DISEASE_STAT2   
Cohort1[40,1] <- Cohort1_model10$Estimate_CM_DISEASE_STAT3 
Cohort1[40,2] <- Cohort1_model10$Std.Error_CM_DISEASE_STAT3  

# Set column and row names
colnames(Cohort1) <- c('log(OR)','SE')
rownames(Cohort1) <-c ('model1',
                     'model2',
                     'model2a',
                     'model2b',
                     'model3_dep',
                     'model3_cmd',
                     'model3_comorb',
                     'model4_dep',
                     'model4_cmd',
                     'model4_comorb',
                     'model5a_dep',
                     'model5a_cmd',
                     'model5a_comorb',
                     'model5b_dep',
                     'model5b_cmd',
                     'model5b_comorb',
                     'model6_dep_pa',
                     'model6_cmd_pa',
                     'model6_comorb_pa',
                     'model6_dep_ea',
                     'model6_cmd_ea',
                     'model6_comorb_ea',
                     'model6_dep_sa',
                     'model6_cmd_sa',
                     'model6_comorb_sa',
                     'model7_sev1_dep',
                     'model7_sev1_cmd',
                     'model7_sev1_comorb',
                     'model7_sev2_dep',
                     'model7_sev2_cmd',
                     'model7_sev2_comorb',
                     'model8_dep',
                     'model8_cmd',
                     'model8_comorb',
                     'model9_dep',
                     'model9_cmd',
                     'model9_comorb',
                     'model10_dep',
                     'model10_cmd',
                     'model10_comorb')

# Repeat this for all 13 cohorts

#################### 4. Create data frames of effect sizes (log(OR) and SE) per model outcome with all cohorts ####################
Model1 <- as.data.frame(rbind(Cohort1 = c(Cohort1['model1','log(OR)'], Cohort1['model1','SE']), 
                              Cohort2 = c(Cohort2['model1','log(OR)'], Cohort2['model1','SE']),  
                              Cohort3 = c(Cohort3['model1','log(OR)'], Cohort3['model1','SE']), 
                              Cohort4 = c(Cohort4['model1','log(OR)'], Cohort4['model1','SE']), 
                              Cohort5 = c(Cohort5['model1','log(OR)'], Cohort5['model1','SE']), 
                              Cohort6 = c(Cohort6['model1','log(OR)'], Cohort6['model1','SE']), 
                              Cohort7 = c(Cohort7['model1','log(OR)'], Cohort7['model1','SE']), 
                              Cohort8 = c(Cohort8['model1','log(OR)'], Cohort8['model1','SE']), 
                              Cohort9 = c(Cohort9['model1','log(OR)'], Cohort9['model1','SE']), 
                              Cohort10 = c(Cohort10['model1','log(OR)'], Cohort10['model1','SE']), 
                              Cohort11 = c(Cohort11['model1','log(OR)'], Cohort11['model1','SE']),
                              Cohort12 = c(Cohort12['model1','log(OR)'], Cohort12['model1','SE']),
                              Cohort13 = c(Cohort13['model1','log(OR)'], Cohort13['model1','SE'])))
colnames(Model1) <- c('log(OR)', 'SE')

Model2 <- as.data.frame(rbind(Cohort1 = c(Cohort1['model2',1], Cohort1['model2',2]), 
                              Cohort2 = c(Cohort2['model2',1], Cohort2['model2',2]),  
                              Cohort3 = c(Cohort3['model2',1], Cohort3['model2',2]), 
                              Cohort4 = c(Cohort4['model2',1], Cohort4['model2',2]), 
                              Cohort5 = c(Cohort5['model2',1], Cohort5['model2',2]), 
                              Cohort6 = c(Cohort6['model2',1], Cohort6['model2',2]), 
                              Cohort7 = c(Cohort7['model2',1], Cohort7['model2',2]), 
                              Cohort8 = c(Cohort8['model2',1], Cohort8['model2',2]), 
                              Cohort9 = c(Cohort9['model2',1], Cohort9['model2',2]), 
                              Cohort10 = c(Cohort10['model2',1], Cohort10['model2',2]), 
                              Cohort11 = c(Cohort11['model2',1], Cohort11['model2',2]),
                              Cohort12 = c(Cohort12['model2',1], Cohort12['model2',2]),
                              Cohort13 = c(Cohort13['model2',1], Cohort13['model2',2])))
colnames(Model2) <- c('log(OR)', 'SE')

Model2a <- as.data.frame(rbind(Cohort1 = c(Cohort1['model2a',1], Cohort1['model2a',2]), 
                              Cohort2 = c(Cohort2['model2a',1], Cohort2['model2a',2]),  
                              Cohort3 = c(Cohort3['model2a',1], Cohort3['model2a',2]), 
                              Cohort4 = c(Cohort4['model2a',1], Cohort4['model2a',2]), 
                              Cohort5 = c(Cohort5['model2a',1], Cohort5['model2a',2]), 
                              Cohort6 = c(Cohort6['model2a',1], Cohort6['model2a',2]), 
                              Cohort7 = c(Cohort7['model2a',1], Cohort7['model2a',2]), 
                              Cohort8 = c(Cohort8['model2a',1], Cohort8['model2a',2]), 
                              Cohort9 = c(Cohort9['model2a',1], Cohort9['model2a',2]), 
                              Cohort10 = c(Cohort10['model2a',1], Cohort10['model2a',2]), 
                              Cohort11 = c(Cohort11['model2a',1], Cohort11['model2a',2]),
                              Cohort12 = c(Cohort12['model2a',1], Cohort12['model2a',2]),
                              Cohort13 = c(Cohort13['model2a',1], Cohort13['model2a',2])))
colnames(Model2a) <- c('log(OR)', 'SE')

Model2b <- as.data.frame(rbind(Cohort1 = c(Cohort1['model2b',1], Cohort1['model2b',2]), 
                              Cohort2 = c(Cohort2['model2b',1], Cohort2['model2b',2]),  
                              Cohort3 = c(Cohort3['model2b',1], Cohort3['model2b',2]), 
                              Cohort4 = c(Cohort4['model2b',1], Cohort4['model2b',2]), 
                              Cohort5 = c(Cohort5['model2b',1], Cohort5['model2b',2]), 
                              Cohort6 = c(Cohort6['model2b',1], Cohort6['model2b',2]), 
                              Cohort7 = c(Cohort7['model2b',1], Cohort7['model2b',2]), 
                              Cohort8 = c(Cohort8['model2b',1], Cohort8['model2b',2]), 
                              Cohort9 = c(Cohort9['model2b',1], Cohort9['model2b',2]), 
                              Cohort10 = c(Cohort10['model2b',1], Cohort10['model2b',2]), 
                              Cohort11 = c(Cohort11['model2b',1], Cohort11['model2b',2]),
                              Cohort12 = c(Cohort12['model2b',1], Cohort12['model2b',2]),
                              Cohort13 = c(Cohort13['model2b',1], Cohort13['model2b',2])))
colnames(Model2b) <- c('log(OR)', 'SE')

Model3_dep <- as.data.frame(rbind(Cohort1 = c(Cohort1['model3_dep',1], Cohort1['model3_dep',2]), 
                               Cohort2 = c(Cohort2['model3_dep',1], Cohort2['model3_dep',2]),  
                               Cohort3 = c(Cohort3['model3_dep',1], Cohort3['model3_dep',2]), 
                               Cohort4 = c(Cohort4['model3_dep',1], Cohort4['model3_dep',2]), 
                               Cohort5 = c(Cohort5['model3_dep',1], Cohort5['model3_dep',2]), 
                               Cohort6 = c(Cohort6['model3_dep',1], Cohort6['model3_dep',2]), 
                               Cohort7 = c(Cohort7['model3_dep',1], Cohort7['model3_dep',2]), 
                               Cohort8 = c(Cohort8['model3_dep',1], Cohort8['model3_dep',2]), 
                               Cohort9 = c(Cohort9['model3_dep',1], Cohort9['model3_dep',2]), 
                               Cohort10 = c(Cohort10['model3_dep',1], Cohort10['model3_dep',2]), 
                               Cohort11 = c(Cohort11['model3_dep',1], Cohort11['model3_dep',2]),
                               Cohort12 = c(Cohort12['model3_dep',1], Cohort12['model3_dep',2]),
                               Cohort13 = c(Cohort13['model3_dep',1], Cohort13['model3_dep',2])))
colnames(Model3_dep) <- c('log(OR)', 'SE')

Model3_cmd <- as.data.frame(rbind(Cohort1 = c(Cohort1['model3_cmd',1], Cohort1['model3_cmd',2]), 
                                  Cohort2 = c(Cohort2['model3_cmd',1], Cohort2['model3_cmd',2]),  
                                  Cohort3 = c(Cohort3['model3_cmd',1], Cohort3['model3_cmd',2]), 
                                  Cohort4 = c(Cohort4['model3_cmd',1], Cohort4['model3_cmd',2]), 
                                  Cohort5 = c(Cohort5['model3_cmd',1], Cohort5['model3_cmd',2]), 
                                  Cohort6 = c(Cohort6['model3_cmd',1], Cohort6['model3_cmd',2]), 
                                  Cohort7 = c(Cohort7['model3_cmd',1], Cohort7['model3_cmd',2]), 
                                  Cohort8 = c(Cohort8['model3_cmd',1], Cohort8['model3_cmd',2]), 
                                  Cohort9 = c(Cohort9['model3_cmd',1], Cohort9['model3_cmd',2]), 
                                  Cohort10 = c(Cohort10['model3_cmd',1], Cohort10['model3_cmd',2]), 
                                  Cohort11 = c(Cohort11['model3_cmd',1], Cohort11['model3_cmd',2]),
                                  Cohort12 = c(Cohort12['model3_cmd',1], Cohort12['model3_cmd',2]),
                                  Cohort13 = c(Cohort13['model3_cmd',1], Cohort13['model3_cmd',2])))
colnames(Model3_cmd) <- c('log(OR)', 'SE')

Model3_comorb <- as.data.frame(rbind(Cohort1 = c(Cohort1['model3_comorb',1], Cohort1['model3_comorb',2]), 
                                  Cohort2 = c(Cohort2['model3_comorb',1], Cohort2['model3_comorb',2]),  
                                  Cohort3 = c(Cohort3['model3_comorb',1], Cohort3['model3_comorb',2]), 
                                  Cohort4 = c(Cohort4['model3_comorb',1], Cohort4['model3_comorb',2]), 
                                  Cohort5 = c(Cohort5['model3_comorb',1], Cohort5['model3_comorb',2]), 
                                  Cohort6 = c(Cohort6['model3_comorb',1], Cohort6['model3_comorb',2]), 
                                  Cohort7 = c(Cohort7['model3_comorb',1], Cohort7['model3_comorb',2]), 
                                  Cohort8 = c(Cohort8['model3_comorb',1], Cohort8['model3_comorb',2]), 
                                  Cohort9 = c(Cohort9['model3_comorb',1], Cohort9['model3_comorb',2]), 
                                  Cohort10 = c(Cohort10['model3_comorb',1], Cohort10['model3_comorb',2]), 
                                  Cohort11 = c(Cohort11['model3_comorb',1], Cohort11['model3_comorb',2]),
                                  Cohort12 = c(Cohort12['model3_comorb',1], Cohort12['model3_comorb',2]),
                                  Cohort13 = c(Cohort13['model3_comorb',1], Cohort13['model3_comorb',2])))
colnames(Model3_comorb) <- c('log(OR)', 'SE')

Model4_dep <- as.data.frame(rbind(Cohort1 = c(Cohort1['model4_dep',1], Cohort1['model4_dep',2]), 
                                  Cohort2 = c(Cohort2['model4_dep',1], Cohort2['model4_dep',2]),  
                                  Cohort3 = c(Cohort3['model4_dep',1], Cohort3['model4_dep',2]), 
                                  Cohort4 = c(Cohort4['model4_dep',1], Cohort4['model4_dep',2]), 
                                  Cohort5 = c(Cohort5['model4_dep',1], Cohort5['model4_dep',2]), 
                                  Cohort6 = c(Cohort6['model4_dep',1], Cohort6['model4_dep',2]), 
                                  Cohort7 = c(Cohort7['model4_dep',1], Cohort7['model4_dep',2]), 
                                  Cohort8 = c(Cohort8['model4_dep',1], Cohort8['model4_dep',2]), 
                                  Cohort9 = c(Cohort9['model4_dep',1], Cohort9['model4_dep',2]), 
                                  Cohort10 = c(Cohort10['model4_dep',1], Cohort10['model4_dep',2]), 
                                  Cohort11 = c(Cohort11['model4_dep',1], Cohort11['model4_dep',2]),
                                  Cohort12 = c(Cohort12['model4_dep',1], Cohort12['model4_dep',2]),
                                  Cohort13 = c(Cohort13['model4_dep',1], Cohort13['model4_dep',2])))
colnames(Model4_dep) <- c('log(OR)', 'SE')

Model4_cmd <- as.data.frame(rbind(Cohort1 = c(Cohort1['model4_cmd',1], Cohort1['model4_cmd',2]), 
                                  Cohort2 = c(Cohort2['model4_cmd',1], Cohort2['model4_cmd',2]),  
                                  Cohort3 = c(Cohort3['model4_cmd',1], Cohort3['model4_cmd',2]), 
                                  Cohort4 = c(Cohort4['model4_cmd',1], Cohort4['model4_cmd',2]), 
                                  Cohort5 = c(Cohort5['model4_cmd',1], Cohort5['model4_cmd',2]), 
                                  Cohort6 = c(Cohort6['model4_cmd',1], Cohort6['model4_cmd',2]), 
                                  Cohort7 = c(Cohort7['model4_cmd',1], Cohort7['model4_cmd',2]), 
                                  Cohort8 = c(Cohort8['model4_cmd',1], Cohort8['model4_cmd',2]), 
                                  Cohort9 = c(Cohort9['model4_cmd',1], Cohort9['model4_cmd',2]), 
                                  Cohort10 = c(Cohort10['model4_cmd',1], Cohort10['model4_cmd',2]), 
                                  Cohort11 = c(Cohort11['model4_cmd',1], Cohort11['model4_cmd',2]),
                                  Cohort12 = c(Cohort12['model4_cmd',1], Cohort12['model4_cmd',2]),
                                  Cohort13 = c(Cohort13['model4_cmd',1], Cohort13['model4_cmd',2])))
colnames(Model4_cmd) <- c('log(OR)', 'SE')

Model4_comorb <- as.data.frame(rbind(Cohort1 = c(Cohort1['model4_comorb',1], Cohort1['model4_comorb',2]), 
                                     Cohort2 = c(Cohort2['model4_comorb',1], Cohort2['model4_comorb',2]),  
                                     Cohort3 = c(Cohort3['model4_comorb',1], Cohort3['model4_comorb',2]), 
                                     Cohort4 = c(Cohort4['model4_comorb',1], Cohort4['model4_comorb',2]), 
                                     Cohort5 = c(Cohort5['model4_comorb',1], Cohort5['model4_comorb',2]), 
                                     Cohort6 = c(Cohort6['model4_comorb',1], Cohort6['model4_comorb',2]), 
                                     Cohort7 = c(Cohort7['model4_comorb',1], Cohort7['model4_comorb',2]), 
                                     Cohort8 = c(Cohort8['model4_comorb',1], Cohort8['model4_comorb',2]), 
                                     Cohort9 = c(Cohort9['model4_comorb',1], Cohort9['model4_comorb',2]), 
                                     Cohort10 = c(Cohort10['model4_comorb',1], Cohort10['model4_comorb',2]), 
                                     Cohort11 = c(Cohort11['model4_comorb',1], Cohort11['model4_comorb',2]),
                                     Cohort12 = c(Cohort12['model4_comorb',1], Cohort12['model4_comorb',2]),
                                     Cohort13 = c(Cohort13['model4_comorb',1], Cohort13['model4_comorb',2])))
colnames(Model4_comorb) <- c('log(OR)', 'SE')

Model5a_dep <- as.data.frame(rbind(Cohort1 = c(Cohort1['model5a_dep',1], Cohort1['model5a_dep',2]), 
                                     Cohort2 = c(Cohort2['model5a_dep',1], Cohort2['model5a_dep',2]),  
                                     Cohort3 = c(Cohort3['model5a_dep',1], Cohort3['model5a_dep',2]), 
                                     Cohort4 = c(Cohort4['model5a_dep',1], Cohort4['model5a_dep',2]), 
                                     Cohort5 = c(Cohort5['model5a_dep',1], Cohort5['model5a_dep',2]), 
                                     Cohort6 = c(Cohort6['model5a_dep',1], Cohort6['model5a_dep',2]), 
                                     Cohort7 = c(Cohort7['model5a_dep',1], Cohort7['model5a_dep',2]), 
                                     Cohort8 = c(Cohort8['model5a_dep',1], Cohort8['model5a_dep',2]), 
                                     Cohort9 = c(Cohort9['model5a_dep',1], Cohort9['model5a_dep',2]), 
                                     Cohort10 = c(Cohort10['model5a_dep',1], Cohort10['model5a_dep',2]), 
                                     Cohort11 = c(Cohort11['model5a_dep',1], Cohort11['model5a_dep',2]),
                                     Cohort12 = c(Cohort12['model5a_dep',1], Cohort12['model5a_dep',2]),
                                     Cohort13 = c(Cohort13['model5a_dep',1], Cohort13['model5a_dep',2])))
colnames(Model5a_dep) <- c('log(OR)', 'SE')

Model5a_cmd <- as.data.frame(rbind(Cohort1 = c(Cohort1['model5a_cmd',1], Cohort1['model5a_cmd',2]), 
                                    Cohort2 = c(Cohort2['model5a_cmd',1], Cohort2['model5a_cmd',2]),  
                                    Cohort3 = c(Cohort3['model5a_cmd',1], Cohort3['model5a_cmd',2]), 
                                    Cohort4 = c(Cohort4['model5a_cmd',1], Cohort4['model5a_cmd',2]), 
                                    Cohort5 = c(Cohort5['model5a_cmd',1], Cohort5['model5a_cmd',2]), 
                                    Cohort6 = c(Cohort6['model5a_cmd',1], Cohort6['model5a_cmd',2]), 
                                    Cohort7 = c(Cohort7['model5a_cmd',1], Cohort7['model5a_cmd',2]), 
                                    Cohort8 = c(Cohort8['model5a_cmd',1], Cohort8['model5a_cmd',2]), 
                                    Cohort9 = c(Cohort9['model5a_cmd',1], Cohort9['model5a_cmd',2]), 
                                    Cohort10 = c(Cohort10['model5a_cmd',1], Cohort10['model5a_cmd',2]), 
                                    Cohort11 = c(Cohort11['model5a_cmd',1], Cohort11['model5a_cmd',2]),
                                    Cohort12 = c(Cohort12['model5a_cmd',1], Cohort12['model5a_cmd',2]),
                                    Cohort13 = c(Cohort13['model5a_cmd',1], Cohort13['model5a_cmd',2])))
colnames(Model5a_cmd) <- c('log(OR)', 'SE')

Model5a_comorb <- as.data.frame(rbind(Cohort1 = c(Cohort1['model5a_comorb',1], Cohort1['model5a_comorb',2]), 
                                   Cohort2 = c(Cohort2['model5a_comorb',1], Cohort2['model5a_comorb',2]),  
                                   Cohort3 = c(Cohort3['model5a_comorb',1], Cohort3['model5a_comorb',2]), 
                                   Cohort4 = c(Cohort4['model5a_comorb',1], Cohort4['model5a_comorb',2]), 
                                   Cohort5 = c(Cohort5['model5a_comorb',1], Cohort5['model5a_comorb',2]), 
                                   Cohort6 = c(Cohort6['model5a_comorb',1], Cohort6['model5a_comorb',2]), 
                                   Cohort7 = c(Cohort7['model5a_comorb',1], Cohort7['model5a_comorb',2]), 
                                   Cohort8 = c(Cohort8['model5a_comorb',1], Cohort8['model5a_comorb',2]), 
                                   Cohort9 = c(Cohort9['model5a_comorb',1], Cohort9['model5a_comorb',2]), 
                                   Cohort10 = c(Cohort10['model5a_comorb',1], Cohort10['model5a_comorb',2]), 
                                   Cohort11 = c(Cohort11['model5a_comorb',1], Cohort11['model5a_comorb',2]),
                                   Cohort12 = c(Cohort12['model5a_comorb',1], Cohort12['model5a_comorb',2]),
                                   Cohort13 = c(Cohort13['model5a_comorb',1], Cohort13['model5a_comorb',2])))
colnames(Model5a_comorb) <- c('log(OR)', 'SE')

Model5b_dep <- as.data.frame(rbind(Cohort1 = c(Cohort1['model5b_dep',1], Cohort1['model5b_dep',2]), 
                                      Cohort2 = c(Cohort2['model5b_dep',1], Cohort2['model5b_dep',2]),  
                                      Cohort3 = c(Cohort3['model5b_dep',1], Cohort3['model5b_dep',2]), 
                                      Cohort4 = c(Cohort4['model5b_dep',1], Cohort4['model5b_dep',2]), 
                                      Cohort5 = c(Cohort5['model5b_dep',1], Cohort5['model5b_dep',2]), 
                                      Cohort6 = c(Cohort6['model5b_dep',1], Cohort6['model5b_dep',2]), 
                                      Cohort7 = c(Cohort7['model5b_dep',1], Cohort7['model5b_dep',2]), 
                                      Cohort8 = c(Cohort8['model5b_dep',1], Cohort8['model5b_dep',2]), 
                                      Cohort9 = c(Cohort9['model5b_dep',1], Cohort9['model5b_dep',2]), 
                                      Cohort10 = c(Cohort10['model5b_dep',1], Cohort10['model5b_dep',2]), 
                                      Cohort11 = c(Cohort11['model5b_dep',1], Cohort11['model5b_dep',2]),
                                      Cohort12 = c(Cohort12['model5b_dep',1], Cohort12['model5b_dep',2]),
                                      Cohort13 = c(Cohort13['model5b_dep',1], Cohort13['model5b_dep',2])))
colnames(Model5b_dep) <- c('log(OR)', 'SE')

Model5b_cmd <- as.data.frame(rbind(Cohort1 = c(Cohort1['model5b_cmd',1], Cohort1['model5b_cmd',2]), 
                                   Cohort2 = c(Cohort2['model5b_cmd',1], Cohort2['model5b_cmd',2]),  
                                   Cohort3 = c(Cohort3['model5b_cmd',1], Cohort3['model5b_cmd',2]), 
                                   Cohort4 = c(Cohort4['model5b_cmd',1], Cohort4['model5b_cmd',2]), 
                                   Cohort5 = c(Cohort5['model5b_cmd',1], Cohort5['model5b_cmd',2]), 
                                   Cohort6 = c(Cohort6['model5b_cmd',1], Cohort6['model5b_cmd',2]), 
                                   Cohort7 = c(Cohort7['model5b_cmd',1], Cohort7['model5b_cmd',2]), 
                                   Cohort8 = c(Cohort8['model5b_cmd',1], Cohort8['model5b_cmd',2]), 
                                   Cohort9 = c(Cohort9['model5b_cmd',1], Cohort9['model5b_cmd',2]), 
                                   Cohort10 = c(Cohort10['model5b_cmd',1], Cohort10['model5b_cmd',2]), 
                                   Cohort11 = c(Cohort11['model5b_cmd',1], Cohort11['model5b_cmd',2]),
                                   Cohort12 = c(Cohort12['model5b_cmd',1], Cohort12['model5b_cmd',2]),
                                   Cohort13 = c(Cohort13['model5b_cmd',1], Cohort13['model5b_cmd',2])))
colnames(Model5b_cmd) <- c('log(OR)', 'SE')

Model5b_comorb <- as.data.frame(rbind(Cohort1 = c(Cohort1['model5b_comorbd',1], Cohort1['model5b_comorbd',2]), 
                                   Cohort2 = c(Cohort2['model5b_comorbd',1], Cohort2['model5b_comorbd',2]),  
                                   Cohort3 = c(Cohort3['model5b_comorbd',1], Cohort3['model5b_comorbd',2]), 
                                   Cohort4 = c(Cohort4['model5b_comorbd',1], Cohort4['model5b_comorbd',2]), 
                                   Cohort5 = c(Cohort5['model5b_comorbd',1], Cohort5['model5b_comorbd',2]), 
                                   Cohort6 = c(Cohort6['model5b_comorbd',1], Cohort6['model5b_comorbd',2]), 
                                   Cohort7 = c(Cohort7['model5b_comorbd',1], Cohort7['model5b_comorbd',2]), 
                                   Cohort8 = c(Cohort8['model5b_comorbd',1], Cohort8['model5b_comorbd',2]), 
                                   Cohort9 = c(Cohort9['model5b_comorbd',1], Cohort9['model5b_comorbd',2]), 
                                   Cohort10 = c(Cohort10['model5b_comorbd',1], Cohort10['model5b_comorbd',2]), 
                                   Cohort11 = c(Cohort11['model5b_comorbd',1], Cohort11['model5b_comorbd',2]),
                                   Cohort12 = c(Cohort12['model5b_comorbd',1], Cohort12['model5b_comorbd',2]),
                                   Cohort13 = c(Cohort13['model5b_comorbd',1], Cohort13['model5b_comorbd',2])))
colnames(Model5b_comorb) <- c('log(OR)', 'SE')

Model6_dep_pa <- as.data.frame(rbind(Cohort1 = c(Cohort1['model6_dep_pa',1], Cohort1['model6_dep_pa',2]), 
                                      Cohort2 = c(Cohort2['model6_dep_pa',1], Cohort2['model6_dep_pa',2]),  
                                      Cohort3 = c(Cohort3['model6_dep_pa',1], Cohort3['model6_dep_pa',2]), 
                                      Cohort4 = c(Cohort4['model6_dep_pa',1], Cohort4['model6_dep_pa',2]), 
                                      Cohort5 = c(Cohort5['model6_dep_pa',1], Cohort5['model6_dep_pa',2]), 
                                      Cohort6 = c(Cohort6['model6_dep_pa',1], Cohort6['model6_dep_pa',2]), 
                                      Cohort7 = c(Cohort7['model6_dep_pa',1], Cohort7['model6_dep_pa',2]), 
                                      Cohort8 = c(Cohort8['model6_dep_pa',1], Cohort8['model6_dep_pa',2]), 
                                      Cohort9 = c(Cohort9['model6_dep_pa',1], Cohort9['model6_dep_pa',2]), 
                                      Cohort10 = c(Cohort10['model6_dep_pa',1], Cohort10['model6_dep_pa',2]), 
                                      Cohort11 = c(Cohort11['model6_dep_pa',1], Cohort11['model6_dep_pa',2]),
                                      Cohort12 = c(Cohort12['model6_dep_pa',1], Cohort12['model6_dep_pa',2]),
                                      Cohort13 = c(Cohort13['model6_dep_pa',1], Cohort13['model6_dep_pa',2])))
colnames(Model6_dep_pa) <- c('log(OR)', 'SE')

Model6_cmd_pa <- as.data.frame(rbind(Cohort1 = c(Cohort1['model6_cmd_pa',1], Cohort1['model6_cmd_pa',2]), 
                                     Cohort2 = c(Cohort2['model6_cmd_pa',1], Cohort2['model6_cmd_pa',2]),  
                                     Cohort3 = c(Cohort3['model6_cmd_pa',1], Cohort3['model6_cmd_pa',2]), 
                                     Cohort4 = c(Cohort4['model6_cmd_pa',1], Cohort4['model6_cmd_pa',2]), 
                                     Cohort5 = c(Cohort5['model6_cmd_pa',1], Cohort5['model6_cmd_pa',2]), 
                                     Cohort6 = c(Cohort6['model6_cmd_pa',1], Cohort6['model6_cmd_pa',2]), 
                                     Cohort7 = c(Cohort7['model6_cmd_pa',1], Cohort7['model6_cmd_pa',2]), 
                                     Cohort8 = c(Cohort8['model6_cmd_pa',1], Cohort8['model6_cmd_pa',2]), 
                                     Cohort9 = c(Cohort9['model6_cmd_pa',1], Cohort9['model6_cmd_pa',2]), 
                                     Cohort10 = c(Cohort10['model6_cmd_pa',1], Cohort10['model6_cmd_pa',2]), 
                                     Cohort11 = c(Cohort11['model6_cmd_pa',1], Cohort11['model6_cmd_pa',2]),
                                     Cohort12 = c(Cohort12['model6_cmd_pa',1], Cohort12['model6_cmd_pa',2]),
                                     Cohort13 = c(Cohort13['model6_cmd_pa',1], Cohort13['model6_cmd_pa',2])))
colnames(Model6_cmd_pa) <- c('log(OR)', 'SE')

Model6_comorb_pa <- as.data.frame(rbind(Cohort1 = c(Cohort1['model6_comorb_pa',1], Cohort1['model6_comorb_pa',2]), 
                                     Cohort2 = c(Cohort2['model6_comorb_pa',1], Cohort2['model6_comorb_pa',2]),  
                                     Cohort3 = c(Cohort3['model6_comorb_pa',1], Cohort3['model6_comorb_pa',2]), 
                                     Cohort4 = c(Cohort4['model6_comorb_pa',1], Cohort4['model6_comorb_pa',2]), 
                                     Cohort5 = c(Cohort5['model6_comorb_pa',1], Cohort5['model6_comorb_pa',2]), 
                                     Cohort6 = c(Cohort6['model6_comorb_pa',1], Cohort6['model6_comorb_pa',2]), 
                                     Cohort7 = c(Cohort7['model6_comorb_pa',1], Cohort7['model6_comorb_pa',2]), 
                                     Cohort8 = c(Cohort8['model6_comorb_pa',1], Cohort8['model6_comorb_pa',2]), 
                                     Cohort9 = c(Cohort9['model6_comorb_pa',1], Cohort9['model6_comorb_pa',2]), 
                                     Cohort10 = c(Cohort10['model6_comorb_pa',1], Cohort10['model6_comorb_pa',2]), 
                                     Cohort11 = c(Cohort11['model6_comorb_pa',1], Cohort11['model6_comorb_pa',2]),
                                     Cohort12 = c(Cohort12['model6_comorb_pa',1], Cohort12['model6_comorb_pa',2]),
                                     Cohort13 = c(Cohort13['model6_comorb_pa',1], Cohort13['model6_comorb_pa',2])))
colnames(Model6_comorb_pa) <- c('log(OR)', 'SE')

Model6_dep_ea <- as.data.frame(rbind(Cohort1 = c(Cohort1['model6_dep_ea',1], Cohort1['model6_dep_ea',2]), 
                                        Cohort2 = c(Cohort2['model6_dep_ea',1], Cohort2['model6_dep_ea',2]),  
                                        Cohort3 = c(Cohort3['model6_dep_ea',1], Cohort3['model6_dep_ea',2]), 
                                        Cohort4 = c(Cohort4['model6_dep_ea',1], Cohort4['model6_dep_ea',2]), 
                                        Cohort5 = c(Cohort5['model6_dep_ea',1], Cohort5['model6_dep_ea',2]), 
                                        Cohort6 = c(Cohort6['model6_dep_ea',1], Cohort6['model6_dep_ea',2]), 
                                        Cohort7 = c(Cohort7['model6_dep_ea',1], Cohort7['model6_dep_ea',2]), 
                                        Cohort8 = c(Cohort8['model6_dep_ea',1], Cohort8['model6_dep_ea',2]), 
                                        Cohort9 = c(Cohort9['model6_dep_ea',1], Cohort9['model6_dep_ea',2]), 
                                        Cohort10 = c(Cohort10['model6_dep_ea',1], Cohort10['model6_dep_ea',2]), 
                                        Cohort11 = c(Cohort11['model6_dep_ea',1], Cohort11['model6_dep_ea',2]),
                                        Cohort12 = c(Cohort12['model6_dep_ea',1], Cohort12['model6_dep_ea',2]),
                                        Cohort13 = c(Cohort13['model6_dep_ea',1], Cohort13['model6_dep_ea',2])))
colnames(Model6_dep_ea) <- c('log(OR)', 'SE')

Model6_cmd_ea <- as.data.frame(rbind(Cohort1 = c(Cohort1['model6_cmd_ea',1], Cohort1['model6_cmd_ea',2]), 
                                     Cohort2 = c(Cohort2['model6_cmd_ea',1], Cohort2['model6_cmd_ea',2]),  
                                     Cohort3 = c(Cohort3['model6_cmd_ea',1], Cohort3['model6_cmd_ea',2]), 
                                     Cohort4 = c(Cohort4['model6_cmd_ea',1], Cohort4['model6_cmd_ea',2]), 
                                     Cohort5 = c(Cohort5['model6_cmd_ea',1], Cohort5['model6_cmd_ea',2]), 
                                     Cohort6 = c(Cohort6['model6_cmd_ea',1], Cohort6['model6_cmd_ea',2]), 
                                     Cohort7 = c(Cohort7['model6_cmd_ea',1], Cohort7['model6_cmd_ea',2]), 
                                     Cohort8 = c(Cohort8['model6_cmd_ea',1], Cohort8['model6_cmd_ea',2]), 
                                     Cohort9 = c(Cohort9['model6_cmd_ea',1], Cohort9['model6_cmd_ea',2]), 
                                     Cohort10 = c(Cohort10['model6_cmd_ea',1], Cohort10['model6_cmd_ea',2]), 
                                     Cohort11 = c(Cohort11['model6_cmd_ea',1], Cohort11['model6_cmd_ea',2]),
                                     Cohort12 = c(Cohort12['model6_cmd_ea',1], Cohort12['model6_cmd_ea',2]),
                                     Cohort13 = c(Cohort13['model6_cmd_ea',1], Cohort13['model6_cmd_ea',2])))
colnames(Model6_cmd_ea) <- c('log(OR)', 'SE')

Model6_comorb_ea <- as.data.frame(rbind(Cohort1 = c(Cohort1['model6_comorb_ea',1], Cohort1['model6_comorb_ea',2]), 
                                     Cohort2 = c(Cohort2['model6_comorb_ea',1], Cohort2['model6_comorb_ea',2]),  
                                     Cohort3 = c(Cohort3['model6_comorb_ea',1], Cohort3['model6_comorb_ea',2]), 
                                     Cohort4 = c(Cohort4['model6_comorb_ea',1], Cohort4['model6_comorb_ea',2]), 
                                     Cohort5 = c(Cohort5['model6_comorb_ea',1], Cohort5['model6_comorb_ea',2]), 
                                     Cohort6 = c(Cohort6['model6_comorb_ea',1], Cohort6['model6_comorb_ea',2]), 
                                     Cohort7 = c(Cohort7['model6_comorb_ea',1], Cohort7['model6_comorb_ea',2]), 
                                     Cohort8 = c(Cohort8['model6_comorb_ea',1], Cohort8['model6_comorb_ea',2]), 
                                     Cohort9 = c(Cohort9['model6_comorb_ea',1], Cohort9['model6_comorb_ea',2]), 
                                     Cohort10 = c(Cohort10['model6_comorb_ea',1], Cohort10['model6_comorb_ea',2]), 
                                     Cohort11 = c(Cohort11['model6_comorb_ea',1], Cohort11['model6_comorb_ea',2]),
                                     Cohort12 = c(Cohort12['model6_comorb_ea',1], Cohort12['model6_comorb_ea',2]),
                                     Cohort13 = c(Cohort13['model6_comorb_ea',1], Cohort13['model6_comorb_ea',2])))
colnames(Model6_comorb_ea) <- c('log(OR)', 'SE')

Model6_dep_sa <- as.data.frame(rbind(Cohort1 = c(Cohort1['model6_dep_sa',1], Cohort1['model6_dep_sa',2]), 
                                        Cohort2 = c(Cohort2['model6_dep_sa',1], Cohort2['model6_dep_sa',2]),  
                                        Cohort3 = c(Cohort3['model6_dep_sa',1], Cohort3['model6_dep_sa',2]), 
                                        Cohort4 = c(Cohort4['model6_dep_sa',1], Cohort4['model6_dep_sa',2]), 
                                        Cohort5 = c(Cohort5['model6_dep_sa',1], Cohort5['model6_dep_sa',2]), 
                                        Cohort6 = c(Cohort6['model6_dep_sa',1], Cohort6['model6_dep_sa',2]), 
                                        Cohort7 = c(Cohort7['model6_dep_sa',1], Cohort7['model6_dep_sa',2]), 
                                        Cohort8 = c(Cohort8['model6_dep_sa',1], Cohort8['model6_dep_sa',2]), 
                                        Cohort9 = c(Cohort9['model6_dep_sa',1], Cohort9['model6_dep_sa',2]), 
                                        Cohort10 = c(Cohort10['model6_dep_sa',1], Cohort10['model6_dep_sa',2]), 
                                        Cohort11 = c(Cohort11['model6_dep_sa',1], Cohort11['model6_dep_sa',2]),
                                        Cohort12 = c(Cohort12['model6_dep_sa',1], Cohort12['model6_dep_sa',2]),
                                        Cohort13 = c(Cohort13['model6_dep_sa',1], Cohort13['model6_dep_sa',2])))
colnames(Model6_dep_sa) <- c('log(OR)', 'SE')

Model6_cmd_sa <- as.data.frame(rbind(Cohort1 = c(Cohort1['model6_cmd_sa',1], Cohort1['model6_cmd_sa',2]), 
                                     Cohort2 = c(Cohort2['model6_cmd_sa',1], Cohort2['model6_cmd_sa',2]),  
                                     Cohort3 = c(Cohort3['model6_cmd_sa',1], Cohort3['model6_cmd_sa',2]), 
                                     Cohort4 = c(Cohort4['model6_cmd_sa',1], Cohort4['model6_cmd_sa',2]), 
                                     Cohort5 = c(Cohort5['model6_cmd_sa',1], Cohort5['model6_cmd_sa',2]), 
                                     Cohort6 = c(Cohort6['model6_cmd_sa',1], Cohort6['model6_cmd_sa',2]), 
                                     Cohort7 = c(Cohort7['model6_cmd_sa',1], Cohort7['model6_cmd_sa',2]), 
                                     Cohort8 = c(Cohort8['model6_cmd_sa',1], Cohort8['model6_cmd_sa',2]), 
                                     Cohort9 = c(Cohort9['model6_cmd_sa',1], Cohort9['model6_cmd_sa',2]), 
                                     Cohort10 = c(Cohort10['model6_cmd_sa',1], Cohort10['model6_cmd_sa',2]), 
                                     Cohort11 = c(Cohort11['model6_cmd_sa',1], Cohort11['model6_cmd_sa',2]),
                                     Cohort12 = c(Cohort12['model6_cmd_sa',1], Cohort12['model6_cmd_sa',2]),
                                     Cohort13 = c(Cohort13['model6_cmd_sa',1], Cohort13['model6_cmd_sa',2])))
colnames(Model6_cmd_sa) <- c('log(OR)', 'SE')

Model6_comorb_sa <- as.data.frame(rbind(Cohort1 = c(Cohort1['model6_comorb_sa',1], Cohort1['model6_comorb_sa',2]), 
                                     Cohort2 = c(Cohort2['model6_comorb_sa',1], Cohort2['model6_comorb_sa',2]),  
                                     Cohort3 = c(Cohort3['model6_comorb_sa',1], Cohort3['model6_comorb_sa',2]), 
                                     Cohort4 = c(Cohort4['model6_comorb_sa',1], Cohort4['model6_comorb_sa',2]), 
                                     Cohort5 = c(Cohort5['model6_comorb_sa',1], Cohort5['model6_comorb_sa',2]), 
                                     Cohort6 = c(Cohort6['model6_comorb_sa',1], Cohort6['model6_comorb_sa',2]), 
                                     Cohort7 = c(Cohort7['model6_comorb_sa',1], Cohort7['model6_comorb_sa',2]), 
                                     Cohort8 = c(Cohort8['model6_comorb_sa',1], Cohort8['model6_comorb_sa',2]), 
                                     Cohort9 = c(Cohort9['model6_comorb_sa',1], Cohort9['model6_comorb_sa',2]), 
                                     Cohort10 = c(Cohort10['model6_comorb_sa',1], Cohort10['model6_comorb_sa',2]), 
                                     Cohort11 = c(Cohort11['model6_comorb_sa',1], Cohort11['model6_comorb_sa',2]),
                                     Cohort12 = c(Cohort12['model6_comorb_sa',1], Cohort12['model6_comorb_sa',2]),
                                     Cohort13 = c(Cohort13['model6_comorb_sa',1], Cohort13['model6_comorb_sa',2])))
colnames(Model6_comorb_sa) <- c('log(OR)', 'SE')

Model7_sev1_dep <- as.data.frame(rbind(Cohort1 = c(Cohort1['model7_sev1_dep',1], Cohort1['model7_sev1_dep',2]), 
                                       Cohort2 = c(Cohort2['model7_sev1_dep',1], Cohort2['model7_sev1_dep',2]),  
                                       Cohort3 = c(Cohort3['model7_sev1_dep',1], Cohort3['model7_sev1_dep',2]), 
                                       Cohort4 = c(Cohort4['model7_sev1_dep',1], Cohort4['model7_sev1_dep',2]), 
                                       Cohort5 = c(Cohort5['model7_sev1_dep',1], Cohort5['model7_sev1_dep',2]), 
                                       Cohort6 = c(Cohort6['model7_sev1_dep',1], Cohort6['model7_sev1_dep',2]), 
                                       Cohort7 = c(Cohort7['model7_sev1_dep',1], Cohort7['model7_sev1_dep',2]), 
                                       Cohort8 = c(Cohort8['model7_sev1_dep',1], Cohort8['model7_sev1_dep',2]), 
                                       Cohort9 = c(Cohort9['model7_sev1_dep',1], Cohort9['model7_sev1_dep',2]), 
                                       Cohort10 = c(Cohort10['model7_sev1_dep',1], Cohort10['model7_sev1_dep',2]), 
                                       Cohort11 = c(Cohort11['model7_sev1_dep',1], Cohort11['model7_sev1_dep',2]),
                                       Cohort12 = c(Cohort12['model7_sev1_dep',1], Cohort12['model7_sev1_dep',2]),
                                       Cohort13 = c(Cohort13['model7_sev1_dep',1], Cohort13['model7_sev1_dep',2])))
colnames(Model7_sev1_dep) <- c('log(OR)', 'SE')

Model7_sev1_cmd <- as.data.frame(rbind(Cohort1 = c(Cohort1['model7_sev1_cmd',1], Cohort1['model7_sev1_cmd',2]), 
                                       Cohort2 = c(Cohort2['model7_sev1_cmd',1], Cohort2['model7_sev1_cmd',2]),  
                                       Cohort3 = c(Cohort3['model7_sev1_cmd',1], Cohort3['model7_sev1_cmd',2]), 
                                       Cohort4 = c(Cohort4['model7_sev1_cmd',1], Cohort4['model7_sev1_cmd',2]), 
                                       Cohort5 = c(Cohort5['model7_sev1_cmd',1], Cohort5['model7_sev1_cmd',2]), 
                                       Cohort6 = c(Cohort6['model7_sev1_cmd',1], Cohort6['model7_sev1_cmd',2]), 
                                       Cohort7 = c(Cohort7['model7_sev1_cmd',1], Cohort7['model7_sev1_cmd',2]), 
                                       Cohort8 = c(Cohort8['model7_sev1_cmd',1], Cohort8['model7_sev1_cmd',2]), 
                                       Cohort9 = c(Cohort9['model7_sev1_cmd',1], Cohort9['model7_sev1_cmd',2]), 
                                       Cohort10 = c(Cohort10['model7_sev1_cmd',1], Cohort10['model7_sev1_cmd',2]), 
                                       Cohort11 = c(Cohort11['model7_sev1_cmd',1], Cohort11['model7_sev1_cmd',2]),
                                       Cohort12 = c(Cohort12['model7_sev1_cmd',1], Cohort12['model7_sev1_cmd',2]),
                                       Cohort13 = c(Cohort13['model7_sev1_cmd',1], Cohort13['model7_sev1_cmd',2])))
colnames(Model7_sev1_cmd) <- c('log(OR)', 'SE')

Model7_sev1_comorb <- as.data.frame(rbind(Cohort1 = c(Cohort1['model7_sev1_comorb',1], Cohort1['model7_sev1_comorb',2]), 
                                       Cohort2 = c(Cohort2['model7_sev1_comorb',1], Cohort2['model7_sev1_comorb',2]),  
                                       Cohort3 = c(Cohort3['model7_sev1_comorb',1], Cohort3['model7_sev1_comorb',2]), 
                                       Cohort4 = c(Cohort4['model7_sev1_comorb',1], Cohort4['model7_sev1_comorb',2]), 
                                       Cohort5 = c(Cohort5['model7_sev1_comorb',1], Cohort5['model7_sev1_comorb',2]), 
                                       Cohort6 = c(Cohort6['model7_sev1_comorb',1], Cohort6['model7_sev1_comorb',2]), 
                                       Cohort7 = c(Cohort7['model7_sev1_comorb',1], Cohort7['model7_sev1_comorb',2]), 
                                       Cohort8 = c(Cohort8['model7_sev1_comorb',1], Cohort8['model7_sev1_comorb',2]), 
                                       Cohort9 = c(Cohort9['model7_sev1_comorb',1], Cohort9['model7_sev1_comorb',2]), 
                                       Cohort10 = c(Cohort10['model7_sev1_comorb',1], Cohort10['model7_sev1_comorb',2]), 
                                       Cohort11 = c(Cohort11['model7_sev1_comorb',1], Cohort11['model7_sev1_comorb',2]),
                                       Cohort12 = c(Cohort12['model7_sev1_comorb',1], Cohort12['model7_sev1_comorb',2]),
                                       Cohort13 = c(Cohort13['model7_sev1_comorb',1], Cohort13['model7_sev1_comorb',2])))
colnames(Model7_sev1_comorb) <- c('log(OR)', 'SE')

Model7_sev2_dep <- as.data.frame(rbind(Cohort1 = c(Cohort1['model7_sev2_dep',1], Cohort1['model7_sev2_dep',2]), 
                                       Cohort2 = c(Cohort2['model7_sev2_dep',1], Cohort2['model7_sev2_dep',2]),  
                                       Cohort3 = c(Cohort3['model7_sev2_dep',1], Cohort3['model7_sev2_dep',2]), 
                                       Cohort4 = c(Cohort4['model7_sev2_dep',1], Cohort4['model7_sev2_dep',2]), 
                                       Cohort5 = c(Cohort5['model7_sev2_dep',1], Cohort5['model7_sev2_dep',2]), 
                                       Cohort6 = c(Cohort6['model7_sev2_dep',1], Cohort6['model7_sev2_dep',2]), 
                                       Cohort7 = c(Cohort7['model7_sev2_dep',1], Cohort7['model7_sev2_dep',2]), 
                                       Cohort8 = c(Cohort8['model7_sev2_dep',1], Cohort8['model7_sev2_dep',2]), 
                                       Cohort9 = c(Cohort9['model7_sev2_dep',1], Cohort9['model7_sev2_dep',2]), 
                                       Cohort10 = c(Cohort10['model7_sev2_dep',1], Cohort10['model7_sev2_dep',2]), 
                                       Cohort11 = c(Cohort11['model7_sev2_dep',1], Cohort11['model7_sev2_dep',2]),
                                       Cohort12 = c(Cohort12['model7_sev2_dep',1], Cohort12['model7_sev2_dep',2]),
                                       Cohort13 = c(Cohort13['model7_sev2_dep',1], Cohort13['model7_sev2_dep',2])))
colnames(Model7_sev2_dep) <- c('log(OR)', 'SE')

Model7_sev2_cmd <- as.data.frame(rbind(Cohort1 = c(Cohort1['model7_sev2_cmd',1], Cohort1['model7_sev2_cmd',2]), 
                                       Cohort2 = c(Cohort2['model7_sev2_cmd',1], Cohort2['model7_sev2_cmd',2]),  
                                       Cohort3 = c(Cohort3['model7_sev2_cmd',1], Cohort3['model7_sev2_cmd',2]), 
                                       Cohort4 = c(Cohort4['model7_sev2_cmd',1], Cohort4['model7_sev2_cmd',2]), 
                                       Cohort5 = c(Cohort5['model7_sev2_cmd',1], Cohort5['model7_sev2_cmd',2]), 
                                       Cohort6 = c(Cohort6['model7_sev2_cmd',1], Cohort6['model7_sev2_cmd',2]), 
                                       Cohort7 = c(Cohort7['model7_sev2_cmd',1], Cohort7['model7_sev2_cmd',2]), 
                                       Cohort8 = c(Cohort8['model7_sev2_cmd',1], Cohort8['model7_sev2_cmd',2]), 
                                       Cohort9 = c(Cohort9['model7_sev2_cmd',1], Cohort9['model7_sev2_cmd',2]), 
                                       Cohort10 = c(Cohort10['model7_sev2_cmd',1], Cohort10['model7_sev2_cmd',2]), 
                                       Cohort11 = c(Cohort11['model7_sev2_cmd',1], Cohort11['model7_sev2_cmd',2]),
                                       Cohort12 = c(Cohort12['model7_sev2_cmd',1], Cohort12['model7_sev2_cmd',2]),
                                       Cohort13 = c(Cohort13['model7_sev2_cmd',1], Cohort13['model7_sev2_cmd',2])))
colnames(Model7_sev2_cmd) <- c('log(OR)', 'SE')

Model7_sev2_comorb <- as.data.frame(rbind(Cohort1 = c(Cohort1['model7_sev2_comorb',1], Cohort1['model7_sev2_comorb',2]), 
                                          Cohort2 = c(Cohort2['model7_sev2_comorb',1], Cohort2['model7_sev2_comorb',2]),  
                                          Cohort3 = c(Cohort3['model7_sev2_comorb',1], Cohort3['model7_sev2_comorb',2]), 
                                          Cohort4 = c(Cohort4['model7_sev2_comorb',1], Cohort4['model7_sev2_comorb',2]), 
                                          Cohort5 = c(Cohort5['model7_sev2_comorb',1], Cohort5['model7_sev2_comorb',2]), 
                                          Cohort6 = c(Cohort6['model7_sev2_comorb',1], Cohort6['model7_sev2_comorb',2]), 
                                          Cohort7 = c(Cohort7['model7_sev2_comorb',1], Cohort7['model7_sev2_comorb',2]), 
                                          Cohort8 = c(Cohort8['model7_sev2_comorb',1], Cohort8['model7_sev2_comorb',2]), 
                                          Cohort9 = c(Cohort9['model7_sev2_comorb',1], Cohort9['model7_sev2_comorb',2]), 
                                          Cohort10 = c(Cohort10['model7_sev2_comorb',1], Cohort10['model7_sev2_comorb',2]), 
                                          Cohort11 = c(Cohort11['model7_sev2_comorb',1], Cohort11['model7_sev2_comorb',2]),
                                          Cohort12 = c(Cohort12['model7_sev2_comorb',1], Cohort12['model7_sev2_comorb',2]),
                                          Cohort13 = c(Cohort13['model7_sev2_comorb',1], Cohort13['model7_sev2_comorb',2])))
colnames(Model7_sev2_comorb) <- c('log(OR)', 'SE')

Model8_dep <- as.data.frame(rbind(Cohort1 = c(Cohort1['model8_dep',1], Cohort1['model8_dep',2]), 
                                        Cohort2 = c(Cohort2['model8_dep',1], Cohort2['model8_dep',2]),  
                                        Cohort3 = c(Cohort3['model8_dep',1], Cohort3['model8_dep',2]), 
                                        Cohort4 = c(Cohort4['model8_dep',1], Cohort4['model8_dep',2]), 
                                        Cohort5 = c(Cohort5['model8_dep',1], Cohort5['model8_dep',2]), 
                                        Cohort6 = c(Cohort6['model8_dep',1], Cohort6['model8_dep',2]), 
                                        Cohort7 = c(Cohort7['model8_dep',1], Cohort7['model8_dep',2]), 
                                        Cohort8 = c(Cohort8['model8_dep',1], Cohort8['model8_dep',2]), 
                                        Cohort9 = c(Cohort9['model8_dep',1], Cohort9['model8_dep',2]), 
                                        Cohort10 = c(Cohort10['model8_dep',1], Cohort10['model8_dep',2]), 
                                        Cohort11 = c(Cohort11['model8_dep',1], Cohort11['model8_dep',2]),
                                        Cohort12 = c(Cohort12['model8_dep',1], Cohort12['model8_dep',2]),
                                        Cohort13 = c(Cohort13['model8_dep',1], Cohort13['model8_dep',2])))
colnames(Model8_dep) <- c('log(OR)', 'SE')

Model8_cmd <- as.data.frame(rbind(Cohort1 = c(Cohort1['model8_cmd',1], Cohort1['model8_cmd',2]), 
                                  Cohort2 = c(Cohort2['model8_cmd',1], Cohort2['model8_cmd',2]),  
                                  Cohort3 = c(Cohort3['model8_cmd',1], Cohort3['model8_cmd',2]), 
                                  Cohort4 = c(Cohort4['model8_cmd',1], Cohort4['model8_cmd',2]), 
                                  Cohort5 = c(Cohort5['model8_cmd',1], Cohort5['model8_cmd',2]), 
                                  Cohort6 = c(Cohort6['model8_cmd',1], Cohort6['model8_cmd',2]), 
                                  Cohort7 = c(Cohort7['model8_cmd',1], Cohort7['model8_cmd',2]), 
                                  Cohort8 = c(Cohort8['model8_cmd',1], Cohort8['model8_cmd',2]), 
                                  Cohort9 = c(Cohort9['model8_cmd',1], Cohort9['model8_cmd',2]), 
                                  Cohort10 = c(Cohort10['model8_cmd',1], Cohort10['model8_cmd',2]), 
                                  Cohort11 = c(Cohort11['model8_cmd',1], Cohort11['model8_cmd',2]),
                                  Cohort12 = c(Cohort12['model8_cmd',1], Cohort12['model8_cmd',2]),
                                  Cohort13 = c(Cohort13['model8_cmd',1], Cohort13['model8_cmd',2])))
colnames(Model8_cmd) <- c('log(OR)', 'SE')

Model8_comorb <- as.data.frame(rbind(Cohort1 = c(Cohort1['model8_comorb',1], Cohort1['model8_comorb',2]), 
                                  Cohort2 = c(Cohort2['model8_comorb',1], Cohort2['model8_comorb',2]),  
                                  Cohort3 = c(Cohort3['model8_comorb',1], Cohort3['model8_comorb',2]), 
                                  Cohort4 = c(Cohort4['model8_comorb',1], Cohort4['model8_comorb',2]), 
                                  Cohort5 = c(Cohort5['model8_comorb',1], Cohort5['model8_comorb',2]), 
                                  Cohort6 = c(Cohort6['model8_comorb',1], Cohort6['model8_comorb',2]), 
                                  Cohort7 = c(Cohort7['model8_comorb',1], Cohort7['model8_comorb',2]), 
                                  Cohort8 = c(Cohort8['model8_comorb',1], Cohort8['model8_comorb',2]), 
                                  Cohort9 = c(Cohort9['model8_comorb',1], Cohort9['model8_comorb',2]), 
                                  Cohort10 = c(Cohort10['model8_comorb',1], Cohort10['model8_comorb',2]), 
                                  Cohort11 = c(Cohort11['model8_comorb',1], Cohort11['model8_comorb',2]),
                                  Cohort12 = c(Cohort12['model8_comorb',1], Cohort12['model8_comorb',2]),
                                  Cohort13 = c(Cohort13['model8_comorb',1], Cohort13['model8_comorb',2])))
colnames(Model8_comorb) <- c('log(OR)', 'SE')

Model9_dep <- as.data.frame(rbind(Cohort1 = c(Cohort1['model9_dep',1], Cohort1['model9_dep',2]), 
                                     Cohort2 = c(Cohort2['model9_dep',1], Cohort2['model9_dep',2]),  
                                     Cohort3 = c(Cohort3['model9_dep',1], Cohort3['model9_dep',2]), 
                                     Cohort4 = c(Cohort4['model9_dep',1], Cohort4['model9_dep',2]), 
                                     Cohort5 = c(Cohort5['model9_dep',1], Cohort5['model9_dep',2]), 
                                     Cohort6 = c(Cohort6['model9_dep',1], Cohort6['model9_dep',2]), 
                                     Cohort7 = c(Cohort7['model9_dep',1], Cohort7['model9_dep',2]), 
                                     Cohort8 = c(Cohort8['model9_dep',1], Cohort8['model9_dep',2]), 
                                     Cohort9 = c(Cohort9['model9_dep',1], Cohort9['model9_dep',2]), 
                                     Cohort10 = c(Cohort10['model9_dep',1], Cohort10['model9_dep',2]), 
                                     Cohort11 = c(Cohort11['model9_dep',1], Cohort11['model9_dep',2]),
                                     Cohort12 = c(Cohort12['model9_dep',1], Cohort12['model9_dep',2]),
                                     Cohort13 = c(Cohort13['model9_dep',1], Cohort13['model9_dep',2])))
colnames(Model9_dep) <- c('log(OR)', 'SE')

Model9_cmd <- as.data.frame(rbind(Cohort1 = c(Cohort1['model9_cmd',1], Cohort1['model9_cmd',2]), 
                                  Cohort2 = c(Cohort2['model9_cmd',1], Cohort2['model9_cmd',2]),  
                                  Cohort3 = c(Cohort3['model9_cmd',1], Cohort3['model9_cmd',2]), 
                                  Cohort4 = c(Cohort4['model9_cmd',1], Cohort4['model9_cmd',2]), 
                                  Cohort5 = c(Cohort5['model9_cmd',1], Cohort5['model9_cmd',2]), 
                                  Cohort6 = c(Cohort6['model9_cmd',1], Cohort6['model9_cmd',2]), 
                                  Cohort7 = c(Cohort7['model9_cmd',1], Cohort7['model9_cmd',2]), 
                                  Cohort8 = c(Cohort8['model9_cmd',1], Cohort8['model9_cmd',2]), 
                                  Cohort9 = c(Cohort9['model9_cmd',1], Cohort9['model9_cmd',2]), 
                                  Cohort10 = c(Cohort10['model9_cmd',1], Cohort10['model9_cmd',2]), 
                                  Cohort11 = c(Cohort11['model9_cmd',1], Cohort11['model9_cmd',2]),
                                  Cohort12 = c(Cohort12['model9_cmd',1], Cohort12['model9_cmd',2]),
                                  Cohort13 = c(Cohort13['model9_cmd',1], Cohort13['model9_cmd',2])))
colnames(Model9_cmd) <- c('log(OR)', 'SE')

Model9_comorb <- as.data.frame(rbind(Cohort1 = c(Cohort1['model9_comorb',1], Cohort1['model9_comorb',2]), 
                                  Cohort2 = c(Cohort2['model9_comorb',1], Cohort2['model9_comorb',2]),  
                                  Cohort3 = c(Cohort3['model9_comorb',1], Cohort3['model9_comorb',2]), 
                                  Cohort4 = c(Cohort4['model9_comorb',1], Cohort4['model9_comorb',2]), 
                                  Cohort5 = c(Cohort5['model9_comorb',1], Cohort5['model9_comorb',2]), 
                                  Cohort6 = c(Cohort6['model9_comorb',1], Cohort6['model9_comorb',2]), 
                                  Cohort7 = c(Cohort7['model9_comorb',1], Cohort7['model9_comorb',2]), 
                                  Cohort8 = c(Cohort8['model9_comorb',1], Cohort8['model9_comorb',2]), 
                                  Cohort9 = c(Cohort9['model9_comorb',1], Cohort9['model9_comorb',2]), 
                                  Cohort10 = c(Cohort10['model9_comorb',1], Cohort10['model9_comorb',2]), 
                                  Cohort11 = c(Cohort11['model9_comorb',1], Cohort11['model9_comorb',2]),
                                  Cohort12 = c(Cohort12['model9_comorb',1], Cohort12['model9_comorb',2]),
                                  Cohort13 = c(Cohort13['model9_comorb',1], Cohort13['model9_comorb',2])))
colnames(Model9_comorb) <- c('log(OR)', 'SE')

Model10_dep <- as.data.frame(rbind(Cohort1 = c(Cohort1['model10_dep',1], Cohort1['model10_dep',2]), 
                                     Cohort2 = c(Cohort2['model10_dep',1], Cohort2['model10_dep',2]),  
                                     Cohort3 = c(Cohort3['model10_dep',1], Cohort3['model10_dep',2]), 
                                     Cohort4 = c(Cohort4['model10_dep',1], Cohort4['model10_dep',2]), 
                                     Cohort5 = c(Cohort5['model10_dep',1], Cohort5['model10_dep',2]), 
                                     Cohort6 = c(Cohort6['model10_dep',1], Cohort6['model10_dep',2]), 
                                     Cohort7 = c(Cohort7['model10_dep',1], Cohort7['model10_dep',2]), 
                                     Cohort8 = c(Cohort8['model10_dep',1], Cohort8['model10_dep',2]), 
                                     Cohort9 = c(Cohort9['model10_dep',1], Cohort9['model10_dep',2]), 
                                     Cohort10 = c(Cohort10['model10_dep',1], Cohort10['model10_dep',2]), 
                                     Cohort11 = c(Cohort11['model10_dep',1], Cohort11['model10_dep',2]),
                                     Cohort12 = c(Cohort12['model10_dep',1], Cohort12['model10_dep',2]),
                                     Cohort13 = c(Cohort13['model10_dep',1], Cohort13['model10_dep',2])))
colnames(Model10_dep) <- c('log(OR)', 'SE')

Model10_cmd <- as.data.frame(rbind(Cohort1 = c(Cohort1['model10_cmd',1], Cohort1['model10_cmd',2]), 
                                  Cohort2 = c(Cohort2['model10_cmd',1], Cohort2['model10_cmd',2]),  
                                  Cohort3 = c(Cohort3['model10_cmd',1], Cohort3['model10_cmd',2]), 
                                  Cohort4 = c(Cohort4['model10_cmd',1], Cohort4['model10_cmd',2]), 
                                  Cohort5 = c(Cohort5['model10_cmd',1], Cohort5['model10_cmd',2]), 
                                  Cohort6 = c(Cohort6['model10_cmd',1], Cohort6['model10_cmd',2]), 
                                  Cohort7 = c(Cohort7['model10_cmd',1], Cohort7['model10_cmd',2]), 
                                  Cohort8 = c(Cohort8['model10_cmd',1], Cohort8['model10_cmd',2]), 
                                  Cohort9 = c(Cohort9['model10_cmd',1], Cohort9['model10_cmd',2]), 
                                  Cohort10 = c(Cohort10['model10_cmd',1], Cohort10['model10_cmd',2]), 
                                  Cohort11 = c(Cohort11['model10_cmd',1], Cohort11['model10_cmd',2]),
                                  Cohort12 = c(Cohort12['model10_cmd',1], Cohort12['model10_cmd',2]),
                                  Cohort13 = c(Cohort13['model10_cmd',1], Cohort13['model10_cmd',2])))
colnames(Model10_cmd) <- c('log(OR)', 'SE')

Model10_comorb <- as.data.frame(rbind(Cohort1 = c(Cohort1['model10_comorb',1], Cohort1['model10_comorb',2]), 
                                  Cohort2 = c(Cohort2['model10_comorb',1], Cohort2['model10_comorb',2]),  
                                  Cohort3 = c(Cohort3['model10_comorb',1], Cohort3['model10_comorb',2]), 
                                  Cohort4 = c(Cohort4['model10_comorb',1], Cohort4['model10_comorb',2]), 
                                  Cohort5 = c(Cohort5['model10_comorb',1], Cohort5['model10_comorb',2]), 
                                  Cohort6 = c(Cohort6['model10_comorb',1], Cohort6['model10_comorb',2]), 
                                  Cohort7 = c(Cohort7['model10_comorb',1], Cohort7['model10_comorb',2]), 
                                  Cohort8 = c(Cohort8['model10_comorb',1], Cohort8['model10_comorb',2]), 
                                  Cohort9 = c(Cohort9['model10_comorb',1], Cohort9['model10_comorb',2]), 
                                  Cohort10 = c(Cohort10['model10_comorb',1], Cohort10['model10_comorb',2]), 
                                  Cohort11 = c(Cohort11['model10_comorb',1], Cohort11['model10_comorb',2]),
                                  Cohort12 = c(Cohort12['model10_comorb',1], Cohort12['model10_comorb',2]),
                                  Cohort13 = c(Cohort13['model10_comorb',1], Cohort13['model10_comorb',2])))
colnames(Model10_comorb) <- c('log(OR)', 'SE')

#################### 5. Check for each model whether cohorts have at least 5 cases per exposure x outcome crosstab cell ####################
# Use the following function for binomial logistic models (models 1, 2, 2a, and 2b)
crosstablogistic_function <- function(Cohort1_modelX,
                                      Cohort2_modelX,
                                      Cohort3_modelX,
                                      Cohort4_modelX, 
                                      Cohort5_modelX,
                                      Cohort6_modelX,
                                      Cohort7_modelX,
                                      Cohort8_modelX,
                                      Cohort9_modelX,
                                      Cohort10_modelX,
                                      Cohort11_modelX,
                                      Cohort12_modelX,
                                      Cohort13_modelX){
  ifelse(is.data.frame(Cohort1_modelX) == T, 
         crosstab_Cohort1_modelX <- as.data.frame(matrix(data = c('Cohort1_modelX',
                                                                  'No CM',
                                                                  Cohort1_modelX$n_noCM_DISEASE_STAT0,
                                                                  Cohort1_modelX$n_noCM_DISEASE_STAT1,
                                                                  sum(Cohort1_modelX$n_noCM_DISEASE_STAT0,
                                                                      Cohort1_modelX$n_noCM_DISEASE_STAT1),
                                                                  '',
                                                                  '',
                                                                  'CM',
                                                                  Cohort1_modelX$n_CM_DISEASE_STAT0,
                                                                  Cohort1_modelX$n_CM_DISEASE_STAT1,
                                                                  sum(Cohort1_modelX$n_CM_DISEASE_STAT0,
                                                                      Cohort1_modelX$n_CM_DISEASE_STAT1),
                                                                  Cohort1_modelX$N),
                                                         nrow = 2,
                                                         ncol = 6,
                                                         byrow = T)), crosstab_Cohort1_modelX <- as.data.frame(matrix(NA, nrow = 2, ncol = 6, byrow = T)))
  ifelse(is.data.frame(Cohort2_modelX) == T, 
         crosstab_Cohort2_modelX <- as.data.frame(matrix(data = c('Cohort2_modelX',
                                                                  'No CM',
                                                                  Cohort2_modelX$n_noCM_DISEASE_STAT0,
                                                                  Cohort2_modelX$n_noCM_DISEASE_STAT1,
                                                                  sum(Cohort2_modelX$n_noCM_DISEASE_STAT0,
                                                                      Cohort2_modelX$n_noCM_DISEASE_STAT1),
                                                                  '',
                                                                  '',
                                                                  'CM',
                                                                  Cohort2_modelX$n_CM_DISEASE_STAT0,
                                                                  Cohort2_modelX$n_CM_DISEASE_STAT1,
                                                                  sum(Cohort2_modelX$n_CM_DISEASE_STAT0,
                                                                      Cohort2_modelX$n_CM_DISEASE_STAT1),
                                                                  Cohort2_modelX$N),
                                                         nrow = 2,
                                                         ncol = 6,
                                                         byrow = T)), crosstab_Cohort2_modelX <- as.data.frame(matrix(NA, nrow = 2, ncol = 6, byrow = T)))
  ifelse(is.data.frame(Cohort3_modelX) == T, 
         crosstab_Cohort3_modelX <- as.data.frame(matrix(data = c('Cohort3_modelX',
                                                                  'No CM',
                                                                  Cohort3_modelX$n_noCM_DISEASE_STAT0,
                                                                  Cohort3_modelX$n_noCM_DISEASE_STAT1,
                                                                  sum(Cohort3_modelX$n_noCM_DISEASE_STAT0,
                                                                      Cohort3_modelX$n_noCM_DISEASE_STAT1),
                                                                  '',
                                                                  '',
                                                                  'CM',
                                                                  Cohort3_modelX$n_CM_DISEASE_STAT0,
                                                                  Cohort3_modelX$n_CM_DISEASE_STAT1,
                                                                  sum(Cohort3_modelX$n_CM_DISEASE_STAT0,
                                                                      Cohort3_modelX$n_CM_DISEASE_STAT1),
                                                                  Cohort3_modelX$N),
                                                         nrow = 2,
                                                         ncol = 6,
                                                         byrow = T)), crosstab_Cohort3_modelX <- as.data.frame(matrix(NA, nrow = 2, ncol = 6, byrow = T)))
  ifelse(is.data.frame(Cohort4_modelX) == T, 
         crosstab_Cohort4_modelX <- as.data.frame(matrix(data = c('Cohort4_modelX',
                                                                  'No CM',
                                                                  Cohort4_modelX$n_noCM_DISEASE_STAT0,
                                                                  Cohort4_modelX$n_noCM_DISEASE_STAT1,
                                                                  sum(Cohort4_modelX$n_noCM_DISEASE_STAT0,
                                                                      Cohort4_modelX$n_noCM_DISEASE_STAT1),
                                                                  '',
                                                                  '',
                                                                  'CM',
                                                                  Cohort4_modelX$n_CM_DISEASE_STAT0,
                                                                  Cohort4_modelX$n_CM_DISEASE_STAT1,
                                                                  sum(Cohort4_modelX$n_CM_DISEASE_STAT0,
                                                                      Cohort4_modelX$n_CM_DISEASE_STAT1),
                                                                  Cohort4_modelX$N),
                                                         nrow = 2,
                                                         ncol = 6,
                                                         byrow = T)), crosstab_Cohort4_modelX <- as.data.frame(matrix(NA, nrow = 2, ncol = 6, byrow = T)))
  ifelse(is.data.frame(Cohort5_modelX) == T, 
         crosstab_Cohort5_modelX <- as.data.frame(matrix(data = c('Cohort5_modelX',
                                                                  'No CM',
                                                                  Cohort5_modelX$n_noCM_DISEASE_STAT0,
                                                                  Cohort5_modelX$n_noCM_DISEASE_STAT1,
                                                                  sum(Cohort5_modelX$n_noCM_DISEASE_STAT0,
                                                                      Cohort5_modelX$n_noCM_DISEASE_STAT1),
                                                                  '',
                                                                  '',
                                                                  'CM',
                                                                  Cohort5_modelX$n_CM_DISEASE_STAT0,
                                                                  Cohort5_modelX$n_CM_DISEASE_STAT1,
                                                                  sum(Cohort5_modelX$n_CM_DISEASE_STAT0,
                                                                      Cohort5_modelX$n_CM_DISEASE_STAT1),
                                                                  Cohort5_modelX$N),
                                                         nrow = 2,
                                                         ncol = 6,
                                                         byrow = T)), crosstab_Cohort5_modelX <- as.data.frame(matrix(NA, nrow = 2, ncol = 6, byrow = T)))
  ifelse(is.data.frame(Cohort6_modelX) == T, 
         crosstab_Cohort6_modelX <- as.data.frame(matrix(data = c('Cohort6_modelX',
                                                                  'No CM',
                                                                  Cohort6_modelX$n_noCM_DISEASE_STAT0,
                                                                  Cohort6_modelX$n_noCM_DISEASE_STAT1,
                                                                  sum(Cohort6_modelX$n_noCM_DISEASE_STAT0,
                                                                      Cohort6_modelX$n_noCM_DISEASE_STAT1),
                                                                  '',
                                                                  '',
                                                                  'CM',
                                                                  Cohort6_modelX$n_CM_DISEASE_STAT0,
                                                                  Cohort6_modelX$n_CM_DISEASE_STAT1,
                                                                  sum(Cohort6_modelX$n_CM_DISEASE_STAT0,
                                                                      Cohort6_modelX$n_CM_DISEASE_STAT1),
                                                                  Cohort6_modelX$N),
                                                         nrow = 2,
                                                         ncol = 6,
                                                         byrow = T)), crosstab_Cohort6_modelX <- as.data.frame(matrix(NA, nrow = 2, ncol = 6, byrow = T)))
  ifelse(is.data.frame(Cohort7_modelX) == T, 
         crosstab_Cohort7_modelX <- as.data.frame(matrix(data = c('Cohort7_modelX',
                                                                  'No CM',
                                                                  Cohort7_modelX$n_noCM_DISEASE_STAT0,
                                                                  Cohort7_modelX$n_noCM_DISEASE_STAT1,
                                                                  sum(Cohort7_modelX$n_noCM_DISEASE_STAT0,
                                                                      Cohort7_modelX$n_noCM_DISEASE_STAT1),
                                                                  '',
                                                                  '',
                                                                  'CM',
                                                                  Cohort7_modelX$n_CM_DISEASE_STAT0,
                                                                  Cohort7_modelX$n_CM_DISEASE_STAT1,
                                                                  sum(Cohort7_modelX$n_CM_DISEASE_STAT0,
                                                                      Cohort7_modelX$n_CM_DISEASE_STAT1),
                                                                  Cohort7_modelX$N),
                                                         nrow = 2,
                                                         ncol = 6,
                                                         byrow = T)), crosstab_Cohort7_modelX <- as.data.frame(matrix(NA, nrow = 2, ncol = 6, byrow = T)))
  ifelse(is.data.frame(Cohort8_modelX) == T, 
         crosstab_Cohort8_modelX <- as.data.frame(matrix(data = c('Cohort8_modelX',
                                                                  'No CM',
                                                                  Cohort8_modelX$n_noCM_DISEASE_STAT0,
                                                                  Cohort8_modelX$n_noCM_DISEASE_STAT1,
                                                                  sum(Cohort8_modelX$n_noCM_DISEASE_STAT0,
                                                                      Cohort8_modelX$n_noCM_DISEASE_STAT1),
                                                                  '',
                                                                  '',
                                                                  'CM',
                                                                  Cohort8_modelX$n_CM_DISEASE_STAT0,
                                                                  Cohort8_modelX$n_CM_DISEASE_STAT1,
                                                                  sum(Cohort8_modelX$n_CM_DISEASE_STAT0,
                                                                      Cohort8_modelX$n_CM_DISEASE_STAT1),
                                                                  Cohort8_modelX$N),
                                                         nrow = 2,
                                                         ncol = 6,
                                                         byrow = T)), crosstab_Cohort8_modelX <- as.data.frame(matrix(NA, nrow = 2, ncol = 6, byrow = T)))
  ifelse(is.data.frame(Cohort9_modelX) == T, 
         crosstab_Cohort9_modelX <- as.data.frame(matrix(data = c('Cohort9_modelX',
                                                                  'No CM',
                                                                  Cohort9_modelX$n_noCM_DISEASE_STAT0,
                                                                  Cohort9_modelX$n_noCM_DISEASE_STAT1,
                                                                  sum(Cohort9_modelX$n_noCM_DISEASE_STAT0,
                                                                      Cohort9_modelX$n_noCM_DISEASE_STAT1),
                                                                  '',
                                                                  '',
                                                                  'CM',
                                                                  Cohort9_modelX$n_CM_DISEASE_STAT0,
                                                                  Cohort9_modelX$n_CM_DISEASE_STAT1,
                                                                  sum(Cohort9_modelX$n_CM_DISEASE_STAT0,
                                                                      Cohort9_modelX$n_CM_DISEASE_STAT1),
                                                                  Cohort9_modelX$N),
                                                         nrow = 2,
                                                         ncol = 6,
                                                         byrow = T)), crosstab_Cohort9_modelX <- as.data.frame(matrix(NA, nrow = 2, ncol = 6, byrow = T)))
  ifelse(is.data.frame(Cohort10_modelX) == T, 
         crosstab_Cohort10_modelX <- as.data.frame(matrix(data = c('Cohort10_modelX',
                                                                  'No CM',
                                                                  Cohort10_modelX$n_noCM_DISEASE_STAT0,
                                                                  Cohort10_modelX$n_noCM_DISEASE_STAT1,
                                                                  sum(Cohort10_modelX$n_noCM_DISEASE_STAT0,
                                                                      Cohort10_modelX$n_noCM_DISEASE_STAT1),
                                                                  '',
                                                                  '',
                                                                  'CM',
                                                                  Cohort10_modelX$n_CM_DISEASE_STAT0,
                                                                  Cohort10_modelX$n_CM_DISEASE_STAT1,
                                                                  sum(Cohort10_modelX$n_CM_DISEASE_STAT0,
                                                                      Cohort10_modelX$n_CM_DISEASE_STAT1),
                                                                  Cohort10_modelX$N),
                                                         nrow = 2,
                                                         ncol = 6,
                                                         byrow = T)), crosstab_Cohort10_modelX <- as.data.frame(matrix(NA, nrow = 2, ncol = 6, byrow = T)))
  ifelse(is.data.frame(Cohort11_modelX) == T, 
         crosstab_Cohort11_modelX <- as.data.frame(matrix(data = c('Cohort11_modelX',
                                                                  'No CM',
                                                                  Cohort11_modelX$n_noCM_DISEASE_STAT0,
                                                                  Cohort11_modelX$n_noCM_DISEASE_STAT1,
                                                                  sum(Cohort11_modelX$n_noCM_DISEASE_STAT0,
                                                                      Cohort11_modelX$n_noCM_DISEASE_STAT1),
                                                                  '',
                                                                  '',
                                                                  'CM',
                                                                  Cohort11_modelX$n_CM_DISEASE_STAT0,
                                                                  Cohort11_modelX$n_CM_DISEASE_STAT1,
                                                                  sum(Cohort11_modelX$n_CM_DISEASE_STAT0,
                                                                      Cohort11_modelX$n_CM_DISEASE_STAT1),
                                                                  Cohort7_modelX$N),
                                                         nrow = 2,
                                                         ncol = 6,
                                                         byrow = T)), crosstab_Cohort11_modelX <- as.data.frame(matrix(NA, nrow = 2, ncol = 6, byrow = T)))
  ifelse(is.data.frame(Cohort12_modelX) == T, 
         crosstab_Cohort12_modelX <- as.data.frame(matrix(data = c('Cohort12_modelX',
                                                                  'No CM',
                                                                  Cohort12_modelX$n_noCM_DISEASE_STAT0,
                                                                  Cohort12_modelX$n_noCM_DISEASE_STAT1,
                                                                  sum(Cohort12_modelX$n_noCM_DISEASE_STAT0,
                                                                      Cohort12_modelX$n_noCM_DISEASE_STAT1),
                                                                  '',
                                                                  '',
                                                                  'CM',
                                                                  Cohort12_modelX$n_CM_DISEASE_STAT0,
                                                                  Cohort12_modelX$n_CM_DISEASE_STAT1,
                                                                  sum(Cohort12_modelX$n_CM_DISEASE_STAT0,
                                                                      Cohort12_modelX$n_CM_DISEASE_STAT1),
                                                                  Cohort12_modelX$N),
                                                         nrow = 2,
                                                         ncol = 6,
                                                         byrow = T)), crosstab_Cohort12_modelX <- as.data.frame(matrix(NA, nrow = 2, ncol = 6, byrow = T)))
  ifelse(is.data.frame(Cohort13_modelX) == T, 
         crosstab_Cohort13_modelX <- as.data.frame(matrix(data = c('Cohort13_modelX',
                                                                  'No CM',
                                                                  Cohort13_modelX$n_noCM_DISEASE_STAT0,
                                                                  Cohort13_modelX$n_noCM_DISEASE_STAT1,
                                                                  sum(Cohort13_modelX$n_noCM_DISEASE_STAT0,
                                                                      Cohort13_modelX$n_noCM_DISEASE_STAT1),
                                                                  '',
                                                                  '',
                                                                  'CM',
                                                                  Cohort13_modelX$n_CM_DISEASE_STAT0,
                                                                  Cohort13_modelX$n_CM_DISEASE_STAT1,
                                                                  sum(Cohort13_modelX$n_CM_DISEASE_STAT0,
                                                                      Cohort13_modelX$n_CM_DISEASE_STAT1),
                                                                  Cohort13_modelX$N),
                                                         nrow = 2,
                                                         ncol = 6,
                                                         byrow = T)), crosstab_Cohort13_modelX <- as.data.frame(matrix(NA, nrow = 2, ncol = 6, byrow = T)))
  
  crosstab_model <- rbind(crosstab_Cohort1_modelX,
                          crosstab_Cohort2_modelX, 
                          crosstab_Cohort3_modelX,
                          crosstab_Cohort4_modelX,
                          crosstab_Cohort5_modelX,
                          crosstab_Cohort6_modelX,
                          crosstab_Cohort7_modelX,
                          crosstab_Cohort8_modelX,
                          crosstab_Cohort9_modelX,
                          crosstab_Cohort10_modelX,
                          crosstab_Cohort11_modelX,
                          crosstab_Cohort12_modelX,
                          crosstab_Cohort13_modelX, 
                          make.row.names = F)
  colnames(crosstab_model) <- c('','','Healthy','Outcome', 'N per exposure', 'Total N')
  
  return(crosstab_model)
}

# Use the following function for multinomial logistic models with a single-level single predictor of interest (models 3, 4, 5a, 5b, 8, 9, and 10)
crosstabsmultinomial_function <- function(Cohort1_modelX, 
                                          Cohort2_modelX,
                                          Cohort3_modelX,
                                          Cohort4_modelX, 
                                          Cohort5_modelX,
                                          Cohort6_modelX,
                                          Cohort7_modelX, 
                                          Cohort8_modelX,
                                          Cohort9_modelX,
                                          Cohort10_modelX,
                                          Cohort11_modelX, 
                                          Cohort12_modelX,
                                          Cohort13_modelX){
  ifelse(is.data.frame(Cohort1_modelX)==T,
         crosstab_Cohort1_modelX <- as.data.frame(matrix(data = c('Cohort1_modelX',
                                                                  'No CM',
                                                                  Cohort1_modelX$n_noCM_DISEASE_STAT0,
                                                                  Cohort1_modelX$n_noCM_DISEASE_STAT1,
                                                                  Cohort1_modelX$n_noCM_DISEASE_STAT2,
                                                                  Cohort1_modelX$n_noCM_DISEASE_STAT3,
                                                                  sum(Cohort1_modelX$n_noCM_DISEASE_STAT0,
                                                                      Cohort1_modelX$n_noCM_DISEASE_STAT1,
                                                                      Cohort1_modelX$n_noCM_DISEASE_STAT2,
                                                                      Cohort1_modelX$n_noCM_DISEASE_STAT3),
                                                                  '',
                                                                  '',
                                                                  'CM',
                                                                  Cohort1_modelX$n_CM_DISEASE_STAT0,
                                                                  Cohort1_modelX$n_CM_DISEASE_STAT1,
                                                                  Cohort1_modelX$n_CM_DISEASE_STAT2,
                                                                  Cohort1_modelX$n_CM_DISEASE_STAT3,
                                                                  sum(Cohort1_modelX$n_CM_DISEASE_STAT0,
                                                                      Cohort1_modelX$n_CM_DISEASE_STAT1,
                                                                      Cohort1_modelX$n_CM_DISEASE_STAT2,
                                                                      Cohort1_modelX$n_CM_DISEASE_STAT3),
                                                                  Cohort1_modelX$N),
                                                         nrow = 2,
                                                         ncol = 8,
                                                         byrow = T)), crosstab_Cohort1_modelX <- as.data.frame(matrix(NA, nrow = 2, ncol = 8, byrow = T)))
  ifelse(is.data.frame(Cohort2_modelX)==T,
         crosstab_Cohort2_modelX <- as.data.frame(matrix(data = c('Cohort2_modelX',
                                                                  'No CM',
                                                                  Cohort2_modelX$n_noCM_DISEASE_STAT0,
                                                                  Cohort2_modelX$n_noCM_DISEASE_STAT1,
                                                                  Cohort2_modelX$n_noCM_DISEASE_STAT2,
                                                                  Cohort2_modelX$n_noCM_DISEASE_STAT3,
                                                                  sum(Cohort2_modelX$n_noCM_DISEASE_STAT0,
                                                                      Cohort2_modelX$n_noCM_DISEASE_STAT1,
                                                                      Cohort2_modelX$n_noCM_DISEASE_STAT2,
                                                                      Cohort2_modelX$n_noCM_DISEASE_STAT3),
                                                                  '',
                                                                  '',
                                                                  'CM',
                                                                  Cohort2_modelX$n_CM_DISEASE_STAT0,
                                                                  Cohort2_modelX$n_CM_DISEASE_STAT1,
                                                                  Cohort2_modelX$n_CM_DISEASE_STAT2,
                                                                  Cohort2_modelX$n_CM_DISEASE_STAT3,
                                                                  sum(Cohort2_modelX$n_CM_DISEASE_STAT0,
                                                                      Cohort2_modelX$n_CM_DISEASE_STAT1,
                                                                      Cohort2_modelX$n_CM_DISEASE_STAT2,
                                                                      Cohort2_modelX$n_CM_DISEASE_STAT3),
                                                                  Cohort2_modelX$N),
                                                         nrow = 2,
                                                         ncol = 8,
                                                         byrow = T)), crosstab_Cohort2_modelX <- as.data.frame(matrix(NA, nrow = 2, ncol = 8, byrow = T)))
  ifelse(is.data.frame(Cohort3_modelX)==T,
         crosstab_Cohort3_modelX <- as.data.frame(matrix(data = c('Cohort3_modelX',
                                                                  'No CM',
                                                                  Cohort3_modelX$n_noCM_DISEASE_STAT0,
                                                                  Cohort3_modelX$n_noCM_DISEASE_STAT1,
                                                                  Cohort3_modelX$n_noCM_DISEASE_STAT2,
                                                                  Cohort3_modelX$n_noCM_DISEASE_STAT3,
                                                                  sum(Cohort3_modelX$n_noCM_DISEASE_STAT0,
                                                                      Cohort3_modelX$n_noCM_DISEASE_STAT1,
                                                                      Cohort3_modelX$n_noCM_DISEASE_STAT2,
                                                                      Cohort3_modelX$n_noCM_DISEASE_STAT3),
                                                                  '',
                                                                  '',
                                                                  'CM',
                                                                  Cohort3_modelX$n_CM_DISEASE_STAT0,
                                                                  Cohort3_modelX$n_CM_DISEASE_STAT1,
                                                                  Cohort3_modelX$n_CM_DISEASE_STAT2,
                                                                  Cohort3_modelX$n_CM_DISEASE_STAT3,
                                                                  sum(Cohort3_modelX$n_CM_DISEASE_STAT0,
                                                                      Cohort3_modelX$n_CM_DISEASE_STAT1,
                                                                      Cohort3_modelX$n_CM_DISEASE_STAT2,
                                                                      Cohort3_modelX$n_CM_DISEASE_STAT3),
                                                                  Cohort3_modelX$N),
                                                         nrow = 2,
                                                         ncol = 8,
                                                         byrow = T)), crosstab_Cohort3_modelX <- as.data.frame(matrix(NA, nrow = 2, ncol = 8, byrow = T)))
  ifelse(is.data.frame(Cohort4_modelX)==T,
         crosstab_Cohort4_modelX <- as.data.frame(matrix(data = c('Cohort4_modelX',
                                                                  'No CM',
                                                                  Cohort4_modelX$n_noCM_DISEASE_STAT0,
                                                                  Cohort4_modelX$n_noCM_DISEASE_STAT1,
                                                                  Cohort4_modelX$n_noCM_DISEASE_STAT2,
                                                                  Cohort4_modelX$n_noCM_DISEASE_STAT3,
                                                                  sum(Cohort4_modelX$n_noCM_DISEASE_STAT0,
                                                                      Cohort4_modelX$n_noCM_DISEASE_STAT1,
                                                                      Cohort4_modelX$n_noCM_DISEASE_STAT2,
                                                                      Cohort4_modelX$n_noCM_DISEASE_STAT3),
                                                                  '',
                                                                  '',
                                                                  'CM',
                                                                  Cohort4_modelX$n_CM_DISEASE_STAT0,
                                                                  Cohort4_modelX$n_CM_DISEASE_STAT1,
                                                                  Cohort4_modelX$n_CM_DISEASE_STAT2,
                                                                  Cohort4_modelX$n_CM_DISEASE_STAT3,
                                                                  sum(Cohort4_modelX$n_CM_DISEASE_STAT0,
                                                                      Cohort4_modelX$n_CM_DISEASE_STAT1,
                                                                      Cohort4_modelX$n_CM_DISEASE_STAT2,
                                                                      Cohort4_modelX$n_CM_DISEASE_STAT3),
                                                                  Cohort4_modelX$N),
                                                         nrow = 2,
                                                         ncol = 8,
                                                         byrow = T)), crosstab_Cohort4_modelX <- as.data.frame(matrix(NA, nrow = 2, ncol = 8, byrow = T)))
  ifelse(is.data.frame(Cohort5_modelX)==T,
         crosstab_Cohort5_modelX <- as.data.frame(matrix(data = c('Cohort5_modelX',
                                                                  'No CM',
                                                                  Cohort5_modelX$n_noCM_DISEASE_STAT0,
                                                                  Cohort5_modelX$n_noCM_DISEASE_STAT1,
                                                                  Cohort5_modelX$n_noCM_DISEASE_STAT2,
                                                                  Cohort5_modelX$n_noCM_DISEASE_STAT3,
                                                                  sum(Cohort5_modelX$n_noCM_DISEASE_STAT0,
                                                                      Cohort5_modelX$n_noCM_DISEASE_STAT1,
                                                                      Cohort5_modelX$n_noCM_DISEASE_STAT2,
                                                                      Cohort5_modelX$n_noCM_DISEASE_STAT3),
                                                                  '',
                                                                  '',
                                                                  'CM',
                                                                  Cohort5_modelX$n_CM_DISEASE_STAT0,
                                                                  Cohort5_modelX$n_CM_DISEASE_STAT1,
                                                                  Cohort5_modelX$n_CM_DISEASE_STAT2,
                                                                  Cohort5_modelX$n_CM_DISEASE_STAT3,
                                                                  sum(Cohort5_modelX$n_CM_DISEASE_STAT0,
                                                                      Cohort5_modelX$n_CM_DISEASE_STAT1,
                                                                      Cohort5_modelX$n_CM_DISEASE_STAT2,
                                                                      Cohort5_modelX$n_CM_DISEASE_STAT3),
                                                                  Cohort5_modelX$N),
                                                         nrow = 2,
                                                         ncol = 8,
                                                         byrow = T)), crosstab_Cohort5_modelX <- as.data.frame(matrix(NA, nrow = 2, ncol = 8, byrow = T)))
  ifelse(is.data.frame(Cohort6_modelX)==T,
         crosstab_Cohort6_modelX <- as.data.frame(matrix(data = c('Cohort6_modelX',
                                                                  'No CM',
                                                                  Cohort6_modelX$n_noCM_DISEASE_STAT0,
                                                                  Cohort6_modelX$n_noCM_DISEASE_STAT1,
                                                                  Cohort6_modelX$n_noCM_DISEASE_STAT2,
                                                                  Cohort6_modelX$n_noCM_DISEASE_STAT3,
                                                                  sum(Cohort6_modelX$n_noCM_DISEASE_STAT0,
                                                                      Cohort6_modelX$n_noCM_DISEASE_STAT1,
                                                                      Cohort6_modelX$n_noCM_DISEASE_STAT2,
                                                                      Cohort6_modelX$n_noCM_DISEASE_STAT3),
                                                                  '',
                                                                  '',
                                                                  'CM',
                                                                  Cohort6_modelX$n_CM_DISEASE_STAT0,
                                                                  Cohort6_modelX$n_CM_DISEASE_STAT1,
                                                                  Cohort6_modelX$n_CM_DISEASE_STAT2,
                                                                  Cohort6_modelX$n_CM_DISEASE_STAT3,
                                                                  sum(Cohort6_modelX$n_CM_DISEASE_STAT0,
                                                                      Cohort6_modelX$n_CM_DISEASE_STAT1,
                                                                      Cohort6_modelX$n_CM_DISEASE_STAT2,
                                                                      Cohort6_modelX$n_CM_DISEASE_STAT3),
                                                                  Cohort6_modelX$N),
                                                         nrow = 2,
                                                         ncol = 8,
                                                         byrow = T)), crosstab_Cohort6_modelX <- as.data.frame(matrix(NA, nrow = 2, ncol = 8, byrow = T)))
  ifelse(is.data.frame(Cohort7_modelX)==T,
         crosstab_Cohort7_modelX <- as.data.frame(matrix(data = c('Cohort7_modelX',
                                                                  'No CM',
                                                                  Cohort7_modelX$n_noCM_DISEASE_STAT0,
                                                                  Cohort7_modelX$n_noCM_DISEASE_STAT1,
                                                                  Cohort7_modelX$n_noCM_DISEASE_STAT2,
                                                                  Cohort7_modelX$n_noCM_DISEASE_STAT3,
                                                                  sum(Cohort7_modelX$n_noCM_DISEASE_STAT0,
                                                                      Cohort7_modelX$n_noCM_DISEASE_STAT1,
                                                                      Cohort7_modelX$n_noCM_DISEASE_STAT2,
                                                                      Cohort7_modelX$n_noCM_DISEASE_STAT3),
                                                                  '',
                                                                  '',
                                                                  'CM',
                                                                  Cohort7_modelX$n_CM_DISEASE_STAT0,
                                                                  Cohort7_modelX$n_CM_DISEASE_STAT1,
                                                                  Cohort7_modelX$n_CM_DISEASE_STAT2,
                                                                  Cohort7_modelX$n_CM_DISEASE_STAT3,
                                                                  sum(Cohort7_modelX$n_CM_DISEASE_STAT0,
                                                                      Cohort7_modelX$n_CM_DISEASE_STAT1,
                                                                      Cohort7_modelX$n_CM_DISEASE_STAT2,
                                                                      Cohort7_modelX$n_CM_DISEASE_STAT3),
                                                                  Cohort7_modelX$N),
                                                         nrow = 2,
                                                         ncol = 8,
                                                         byrow = T)), crosstab_Cohort7_modelX <- as.data.frame(matrix(NA, nrow = 2, ncol = 8, byrow = T)))
  ifelse(is.data.frame(Cohort8_modelX)==T,
         crosstab_Cohort8_modelX <- as.data.frame(matrix(data = c('Cohort8_modelX',
                                                                  'No CM',
                                                                  Cohort8_modelX$n_noCM_DISEASE_STAT0,
                                                                  Cohort8_modelX$n_noCM_DISEASE_STAT1,
                                                                  Cohort8_modelX$n_noCM_DISEASE_STAT2,
                                                                  Cohort8_modelX$n_noCM_DISEASE_STAT3,
                                                                  sum(Cohort8_modelX$n_noCM_DISEASE_STAT0,
                                                                      Cohort8_modelX$n_noCM_DISEASE_STAT1,
                                                                      Cohort8_modelX$n_noCM_DISEASE_STAT2,
                                                                      Cohort8_modelX$n_noCM_DISEASE_STAT3),
                                                                  '',
                                                                  '',
                                                                  'CM',
                                                                  Cohort8_modelX$n_CM_DISEASE_STAT0,
                                                                  Cohort8_modelX$n_CM_DISEASE_STAT1,
                                                                  Cohort8_modelX$n_CM_DISEASE_STAT2,
                                                                  Cohort8_modelX$n_CM_DISEASE_STAT3,
                                                                  sum(Cohort8_modelX$n_CM_DISEASE_STAT0,
                                                                      Cohort8_modelX$n_CM_DISEASE_STAT1,
                                                                      Cohort8_modelX$n_CM_DISEASE_STAT2,
                                                                      Cohort8_modelX$n_CM_DISEASE_STAT3),
                                                                  Cohort8_modelX$N),
                                                         nrow = 2,
                                                         ncol = 8,
                                                         byrow = T)), crosstab_Cohort8_modelX <- as.data.frame(matrix(NA, nrow = 2, ncol = 8, byrow = T)))
  ifelse(is.data.frame(Cohort9_modelX)==T,
         crosstab_Cohort9_modelX <- as.data.frame(matrix(data = c('Cohort9_modelX',
                                                                  'No CM',
                                                                  Cohort9_modelX$n_noCM_DISEASE_STAT0,
                                                                  Cohort9_modelX$n_noCM_DISEASE_STAT1,
                                                                  Cohort9_modelX$n_noCM_DISEASE_STAT2,
                                                                  Cohort9_modelX$n_noCM_DISEASE_STAT3,
                                                                  sum(Cohort9_modelX$n_noCM_DISEASE_STAT0,
                                                                      Cohort9_modelX$n_noCM_DISEASE_STAT1,
                                                                      Cohort9_modelX$n_noCM_DISEASE_STAT2,
                                                                      Cohort9_modelX$n_noCM_DISEASE_STAT3),
                                                                  '',
                                                                  '',
                                                                  'CM',
                                                                  Cohort9_modelX$n_CM_DISEASE_STAT0,
                                                                  Cohort9_modelX$n_CM_DISEASE_STAT1,
                                                                  Cohort9_modelX$n_CM_DISEASE_STAT2,
                                                                  Cohort9_modelX$n_CM_DISEASE_STAT3,
                                                                  sum(Cohort9_modelX$n_CM_DISEASE_STAT0,
                                                                      Cohort9_modelX$n_CM_DISEASE_STAT1,
                                                                      Cohort9_modelX$n_CM_DISEASE_STAT2,
                                                                      Cohort9_modelX$n_CM_DISEASE_STAT3),
                                                                  Cohort9_modelX$N),
                                                         nrow = 2,
                                                         ncol = 8,
                                                         byrow = T)), crosstab_Cohort9_modelX <- as.data.frame(matrix(NA, nrow = 2, ncol = 8, byrow = T)))
  ifelse(is.data.frame(Cohort10_modelX)==T,
         crosstab_Cohort10_modelX <- as.data.frame(matrix(data = c('Cohort10_modelX',
                                                                  'No CM',
                                                                  Cohort10_modelX$n_noCM_DISEASE_STAT0,
                                                                  Cohort10_modelX$n_noCM_DISEASE_STAT1,
                                                                  Cohort10_modelX$n_noCM_DISEASE_STAT2,
                                                                  Cohort10_modelX$n_noCM_DISEASE_STAT3,
                                                                  sum(Cohort10_modelX$n_noCM_DISEASE_STAT0,
                                                                      Cohort10_modelX$n_noCM_DISEASE_STAT1,
                                                                      Cohort10_modelX$n_noCM_DISEASE_STAT2,
                                                                      Cohort10_modelX$n_noCM_DISEASE_STAT3),
                                                                  '',
                                                                  '',
                                                                  'CM',
                                                                  Cohort10_modelX$n_CM_DISEASE_STAT0,
                                                                  Cohort10_modelX$n_CM_DISEASE_STAT1,
                                                                  Cohort10_modelX$n_CM_DISEASE_STAT2,
                                                                  Cohort10_modelX$n_CM_DISEASE_STAT3,
                                                                  sum(Cohort10_modelX$n_CM_DISEASE_STAT0,
                                                                      Cohort10_modelX$n_CM_DISEASE_STAT1,
                                                                      Cohort10_modelX$n_CM_DISEASE_STAT2,
                                                                      Cohort10_modelX$n_CM_DISEASE_STAT3),
                                                                  Cohort10_modelX$N),
                                                         nrow = 2,
                                                         ncol = 8,
                                                         byrow = T)), crosstab_Cohort10_modelX <- as.data.frame(matrix(NA, nrow = 2, ncol = 8, byrow = T)))
  ifelse(is.data.frame(Cohort11_modelX)==T,
         crosstab_Cohort11_modelX <- as.data.frame(matrix(data = c('Cohort11_modelX',
                                                                   'No CM',
                                                                   Cohort11_modelX$n_noCM_DISEASE_STAT0,
                                                                   Cohort11_modelX$n_noCM_DISEASE_STAT1,
                                                                   Cohort11_modelX$n_noCM_DISEASE_STAT2,
                                                                   Cohort11_modelX$n_noCM_DISEASE_STAT3,
                                                                   sum(Cohort11_modelX$n_noCM_DISEASE_STAT0,
                                                                       Cohort11_modelX$n_noCM_DISEASE_STAT1,
                                                                       Cohort11_modelX$n_noCM_DISEASE_STAT2,
                                                                       Cohort11_modelX$n_noCM_DISEASE_STAT3),
                                                                   '',
                                                                   '',
                                                                   'CM',
                                                                   Cohort11_modelX$n_CM_DISEASE_STAT0,
                                                                   Cohort11_modelX$n_CM_DISEASE_STAT1,
                                                                   Cohort11_modelX$n_CM_DISEASE_STAT2,
                                                                   Cohort11_modelX$n_CM_DISEASE_STAT3,
                                                                   sum(Cohort11_modelX$n_CM_DISEASE_STAT0,
                                                                       Cohort11_modelX$n_CM_DISEASE_STAT1,
                                                                       Cohort11_modelX$n_CM_DISEASE_STAT2,
                                                                       Cohort11_modelX$n_CM_DISEASE_STAT3),
                                                                   Cohort11_modelX$N),
                                                          nrow = 2,
                                                          ncol = 8,
                                                          byrow = T)), crosstab_Cohort11_modelX <- as.data.frame(matrix(NA, nrow = 2, ncol = 8, byrow = T)))
  ifelse(is.data.frame(Cohort12_modelX)==T,
         crosstab_Cohort12_modelX <- as.data.frame(matrix(data = c('Cohort12_modelX',
                                                                   'No CM',
                                                                   Cohort12_modelX$n_noCM_DISEASE_STAT0,
                                                                   Cohort12_modelX$n_noCM_DISEASE_STAT1,
                                                                   Cohort12_modelX$n_noCM_DISEASE_STAT2,
                                                                   Cohort12_modelX$n_noCM_DISEASE_STAT3,
                                                                   sum(Cohort12_modelX$n_noCM_DISEASE_STAT0,
                                                                       Cohort12_modelX$n_noCM_DISEASE_STAT1,
                                                                       Cohort12_modelX$n_noCM_DISEASE_STAT2,
                                                                       Cohort12_modelX$n_noCM_DISEASE_STAT3),
                                                                   '',
                                                                   '',
                                                                   'CM',
                                                                   Cohort12_modelX$n_CM_DISEASE_STAT0,
                                                                   Cohort12_modelX$n_CM_DISEASE_STAT1,
                                                                   Cohort12_modelX$n_CM_DISEASE_STAT2,
                                                                   Cohort12_modelX$n_CM_DISEASE_STAT3,
                                                                   sum(Cohort12_modelX$n_CM_DISEASE_STAT0,
                                                                       Cohort12_modelX$n_CM_DISEASE_STAT1,
                                                                       Cohort12_modelX$n_CM_DISEASE_STAT2,
                                                                       Cohort12_modelX$n_CM_DISEASE_STAT3),
                                                                   Cohort12_modelX$N),
                                                          nrow = 2,
                                                          ncol = 8,
                                                          byrow = T)), crosstab_Cohort12_modelX <- as.data.frame(matrix(NA, nrow = 2, ncol = 8, byrow = T)))
  ifelse(is.data.frame(Cohort13_modelX)==T,
         crosstab_Cohort13_modelX <- as.data.frame(matrix(data = c('Cohort13_modelX',
                                                                   'No CM',
                                                                   Cohort13_modelX$n_noCM_DISEASE_STAT0,
                                                                   Cohort13_modelX$n_noCM_DISEASE_STAT1,
                                                                   Cohort13_modelX$n_noCM_DISEASE_STAT2,
                                                                   Cohort13_modelX$n_noCM_DISEASE_STAT3,
                                                                   sum(Cohort13_modelX$n_noCM_DISEASE_STAT0,
                                                                       Cohort13_modelX$n_noCM_DISEASE_STAT1,
                                                                       Cohort13_modelX$n_noCM_DISEASE_STAT2,
                                                                       Cohort13_modelX$n_noCM_DISEASE_STAT3),
                                                                   '',
                                                                   '',
                                                                   'CM',
                                                                   Cohort13_modelX$n_CM_DISEASE_STAT0,
                                                                   Cohort13_modelX$n_CM_DISEASE_STAT1,
                                                                   Cohort13_modelX$n_CM_DISEASE_STAT2,
                                                                   Cohort13_modelX$n_CM_DISEASE_STAT3,
                                                                   sum(Cohort13_modelX$n_CM_DISEASE_STAT0,
                                                                       Cohort13_modelX$n_CM_DISEASE_STAT1,
                                                                       Cohort13_modelX$n_CM_DISEASE_STAT2,
                                                                       Cohort13_modelX$n_CM_DISEASE_STAT3),
                                                                   Cohort13_modelX$N),
                                                          nrow = 2,
                                                          ncol = 8,
                                                          byrow = T)), crosstab_Cohort13_modelX <- as.data.frame(matrix(NA, nrow = 2, ncol = 8, byrow = T)))
  
  crosstab_model <- rbind(crosstab_Cohort1_modelX,
                          crosstab_Cohort2_modelX,
                          crosstab_Cohort3_modelX,
                          crosstab_Cohort4_modelX,
                          crosstab_Cohort5_modelX,
                          crosstab_Cohort6_modelX,
                          crosstab_Cohort7_modelX,
                          crosstab_Cohort8_modelX,
                          crosstab_Cohort9_modelX,
                          crosstab_Cohort10_modelX,
                          crosstab_Cohort11_modelX,
                          crosstab_Cohort12_modelX,
                          crosstab_Cohort13_modelX, make.row.names = F)
  colnames(crosstab_model) <- c('','','Healthy','Dep only', 'Cmd disease only','Comorbidity', 'N per exposure','Total N')
  
  return(crosstab_model)
}

# Use the following function for multinomial logistic models with several predictors of interest (model 6)
crosstabsmultinomial_multippredictors_function <- function(Cohort1_modelX, 
                                                           Cohort2_modelX,
                                                           Cohort3_modelX,
                                                           Cohort4_modelX, 
                                                           Cohort5_modelX,
                                                           Cohort6_modelX,
                                                           Cohort7_modelX, 
                                                           Cohort8_modelX,
                                                           Cohort9_modelX,
                                                           Cohort10_modelX,
                                                           Cohort11_modelX, 
                                                           Cohort12_modelX,
                                                           Cohort13_modelX){
  ifelse(is.data.frame(Cohort1_modelX) == T, 
         crosstab_Cohort1_modelX <- as.data.frame(matrix(data = c('Cohort1_modelX',
                                                                  'No CM_pa',
                                                                  Cohort1_modelX$n_noCM_pa_DISEASE_STAT0,
                                                                  Cohort1_modelX$n_noCM_pa_DISEASE_STAT1,
                                                                  Cohort1_modelX$n_noCM_pa_DISEASE_STAT2,
                                                                  Cohort1_modelX$n_noCM_pa_DISEASE_STAT3,
                                                                  sum(Cohort1_modelX$n_noCM_pa_DISEASE_STAT0,
                                                                      Cohort1_modelX$n_noCM_pa_DISEASE_STAT1,
                                                                      Cohort1_modelX$n_noCM_pa_DISEASE_STAT2,
                                                                      Cohort1_modelX$n_noCM_pa_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_pa',
                                                                  Cohort1_modelX$n_CM_pa_DISEASE_STAT0,
                                                                  Cohort1_modelX$n_CM_pa_DISEASE_STAT1,
                                                                  Cohort1_modelX$n_CM_pa_DISEASE_STAT2,
                                                                  Cohort1_modelX$n_CM_pa_DISEASE_STAT3,
                                                                  sum(Cohort1_modelX$n_CM_pa_DISEASE_STAT0,
                                                                      Cohort1_modelX$n_CM_pa_DISEASE_STAT1,
                                                                      Cohort1_modelX$n_CM_pa_DISEASE_STAT2,
                                                                      Cohort1_modelX$n_CM_pa_DISEASE_STAT3),
                                                                  '',
                                                                  'No CM_ea',
                                                                  Cohort1_modelX$n_noCM_ea_DISEASE_STAT0,
                                                                  Cohort1_modelX$n_noCM_ea_DISEASE_STAT1,
                                                                  Cohort1_modelX$n_noCM_ea_DISEASE_STAT2,
                                                                  Cohort1_modelX$n_noCM_ea_DISEASE_STAT3,
                                                                  sum(Cohort1_modelX$n_noCM_ea_DISEASE_STAT0,
                                                                      Cohort1_modelX$n_noCM_ea_DISEASE_STAT1,
                                                                      Cohort1_modelX$n_noCM_ea_DISEASE_STAT2,
                                                                      Cohort1_modelX$n_noCM_ea_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_ea',
                                                                  Cohort1_modelX$n_CM_ea_DISEASE_STAT0,
                                                                  Cohort1_modelX$n_CM_ea_DISEASE_STAT1,
                                                                  Cohort1_modelX$n_CM_ea_DISEASE_STAT2,
                                                                  Cohort1_modelX$n_CM_ea_DISEASE_STAT3,
                                                                  sum(Cohort1_modelX$n_CM_ea_DISEASE_STAT0,
                                                                      Cohort1_modelX$n_CM_ea_DISEASE_STAT1,
                                                                      Cohort1_modelX$n_CM_ea_DISEASE_STAT2,
                                                                      Cohort1_modelX$n_CM_ea_DISEASE_STAT3),
                                                                  '',
                                                                  'No CM_sa',
                                                                  Cohort1_modelX$n_noCM_sa_DISEASE_STAT0,
                                                                  Cohort1_modelX$n_noCM_sa_DISEASE_STAT1,
                                                                  Cohort1_modelX$n_noCM_sa_DISEASE_STAT2,
                                                                  Cohort1_modelX$n_noCM_sa_DISEASE_STAT3,
                                                                  sum(Cohort1_modelX$n_noCM_sa_DISEASE_STAT0,
                                                                      Cohort1_modelX$n_noCM_sa_DISEASE_STAT1,
                                                                      Cohort1_modelX$n_noCM_sa_DISEASE_STAT2,
                                                                      Cohort1_modelX$n_noCM_sa_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sa',
                                                                  Cohort1_modelX$n_CM_sa_DISEASE_STAT0,
                                                                  Cohort1_modelX$n_CM_sa_DISEASE_STAT1,
                                                                  Cohort1_modelX$n_CM_sa_DISEASE_STAT2,
                                                                  Cohort1_modelX$n_CM_sa_DISEASE_STAT3,
                                                                  sum(Cohort1_modelX$n_CM_sa_DISEASE_STAT0,
                                                                      Cohort1_modelX$n_CM_sa_DISEASE_STAT1,
                                                                      Cohort1_modelX$n_CM_sa_DISEASE_STAT2,
                                                                      Cohort1_modelX$n_CM_sa_DISEASE_STAT3)),
                                                         nrow = 6,
                                                         ncol = 7,
                                                         byrow = T)), crosstab_Cohort1_modelX <- as.data.frame(matrix(NA, nrow = 6, ncol = 7, byrow = T)))
  ifelse(is.data.frame(Cohort2_modelX) == T, 
         crosstab_Cohort2_modelX <- as.data.frame(matrix(data = c('Cohort2_modelX',
                                                                  'No CM_pa',
                                                                  Cohort2_modelX$n_noCM_pa_DISEASE_STAT0,
                                                                  Cohort2_modelX$n_noCM_pa_DISEASE_STAT1,
                                                                  Cohort2_modelX$n_noCM_pa_DISEASE_STAT2,
                                                                  Cohort2_modelX$n_noCM_pa_DISEASE_STAT3,
                                                                  sum(Cohort2_modelX$n_noCM_pa_DISEASE_STAT0,
                                                                      Cohort2_modelX$n_noCM_pa_DISEASE_STAT1,
                                                                      Cohort2_modelX$n_noCM_pa_DISEASE_STAT2,
                                                                      Cohort2_modelX$n_noCM_pa_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_pa',
                                                                  Cohort2_modelX$n_CM_pa_DISEASE_STAT0,
                                                                  Cohort2_modelX$n_CM_pa_DISEASE_STAT1,
                                                                  Cohort2_modelX$n_CM_pa_DISEASE_STAT2,
                                                                  Cohort2_modelX$n_CM_pa_DISEASE_STAT3,
                                                                  sum(Cohort2_modelX$n_CM_pa_DISEASE_STAT0,
                                                                      Cohort2_modelX$n_CM_pa_DISEASE_STAT1,
                                                                      Cohort2_modelX$n_CM_pa_DISEASE_STAT2,
                                                                      Cohort2_modelX$n_CM_pa_DISEASE_STAT3),
                                                                  '',
                                                                  'No CM_ea',
                                                                  Cohort2_modelX$n_noCM_ea_DISEASE_STAT0,
                                                                  Cohort2_modelX$n_noCM_ea_DISEASE_STAT1,
                                                                  Cohort2_modelX$n_noCM_ea_DISEASE_STAT2,
                                                                  Cohort2_modelX$n_noCM_ea_DISEASE_STAT3,
                                                                  sum(Cohort2_modelX$n_noCM_ea_DISEASE_STAT0,
                                                                      Cohort2_modelX$n_noCM_ea_DISEASE_STAT1,
                                                                      Cohort2_modelX$n_noCM_ea_DISEASE_STAT2,
                                                                      Cohort2_modelX$n_noCM_ea_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_ea',
                                                                  Cohort2_modelX$n_CM_ea_DISEASE_STAT0,
                                                                  Cohort2_modelX$n_CM_ea_DISEASE_STAT1,
                                                                  Cohort2_modelX$n_CM_ea_DISEASE_STAT2,
                                                                  Cohort2_modelX$n_CM_ea_DISEASE_STAT3,
                                                                  sum(Cohort2_modelX$n_CM_ea_DISEASE_STAT0,
                                                                      Cohort2_modelX$n_CM_ea_DISEASE_STAT1,
                                                                      Cohort2_modelX$n_CM_ea_DISEASE_STAT2,
                                                                      Cohort2_modelX$n_CM_ea_DISEASE_STAT3),
                                                                  '',
                                                                  'No CM_sa',
                                                                  Cohort2_modelX$n_noCM_sa_DISEASE_STAT0,
                                                                  Cohort2_modelX$n_noCM_sa_DISEASE_STAT1,
                                                                  Cohort2_modelX$n_noCM_sa_DISEASE_STAT2,
                                                                  Cohort2_modelX$n_noCM_sa_DISEASE_STAT3,
                                                                  sum(Cohort2_modelX$n_noCM_sa_DISEASE_STAT0,
                                                                      Cohort2_modelX$n_noCM_sa_DISEASE_STAT1,
                                                                      Cohort2_modelX$n_noCM_sa_DISEASE_STAT2,
                                                                      Cohort2_modelX$n_noCM_sa_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sa',
                                                                  Cohort2_modelX$n_CM_sa_DISEASE_STAT0,
                                                                  Cohort2_modelX$n_CM_sa_DISEASE_STAT1,
                                                                  Cohort2_modelX$n_CM_sa_DISEASE_STAT2,
                                                                  Cohort2_modelX$n_CM_sa_DISEASE_STAT3,
                                                                  sum(Cohort2_modelX$n_CM_sa_DISEASE_STAT0,
                                                                      Cohort2_modelX$n_CM_sa_DISEASE_STAT1,
                                                                      Cohort2_modelX$n_CM_sa_DISEASE_STAT2,
                                                                      Cohort2_modelX$n_CM_sa_DISEASE_STAT3)),
                                                         nrow = 6,
                                                         ncol = 7,
                                                         byrow = T)), crosstab_Cohort2_modelX <- as.data.frame(matrix(NA, nrow = 6, ncol = 7, byrow = T)))
  ifelse(is.data.frame(Cohort3_modelX) == T, 
         crosstab_Cohort3_modelX <- as.data.frame(matrix(data = c('Cohort3_modelX',
                                                                  'No CM_pa',
                                                                  Cohort3_modelX$n_noCM_pa_DISEASE_STAT0,
                                                                  Cohort3_modelX$n_noCM_pa_DISEASE_STAT1,
                                                                  Cohort3_modelX$n_noCM_pa_DISEASE_STAT2,
                                                                  Cohort3_modelX$n_noCM_pa_DISEASE_STAT3,
                                                                  sum(Cohort3_modelX$n_noCM_pa_DISEASE_STAT0,
                                                                      Cohort3_modelX$n_noCM_pa_DISEASE_STAT1,
                                                                      Cohort3_modelX$n_noCM_pa_DISEASE_STAT2,
                                                                      Cohort3_modelX$n_noCM_pa_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_pa',
                                                                  Cohort3_modelX$n_CM_pa_DISEASE_STAT0,
                                                                  Cohort3_modelX$n_CM_pa_DISEASE_STAT1,
                                                                  Cohort3_modelX$n_CM_pa_DISEASE_STAT2,
                                                                  Cohort3_modelX$n_CM_pa_DISEASE_STAT3,
                                                                  sum(Cohort3_modelX$n_CM_pa_DISEASE_STAT0,
                                                                      Cohort3_modelX$n_CM_pa_DISEASE_STAT1,
                                                                      Cohort3_modelX$n_CM_pa_DISEASE_STAT2,
                                                                      Cohort3_modelX$n_CM_pa_DISEASE_STAT3),
                                                                  '',
                                                                  'No CM_ea',
                                                                  Cohort3_modelX$n_noCM_ea_DISEASE_STAT0,
                                                                  Cohort3_modelX$n_noCM_ea_DISEASE_STAT1,
                                                                  Cohort3_modelX$n_noCM_ea_DISEASE_STAT2,
                                                                  Cohort3_modelX$n_noCM_ea_DISEASE_STAT3,
                                                                  sum(Cohort3_modelX$n_noCM_ea_DISEASE_STAT0,
                                                                      Cohort3_modelX$n_noCM_ea_DISEASE_STAT1,
                                                                      Cohort3_modelX$n_noCM_ea_DISEASE_STAT2,
                                                                      Cohort3_modelX$n_noCM_ea_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_ea',
                                                                  Cohort3_modelX$n_CM_ea_DISEASE_STAT0,
                                                                  Cohort3_modelX$n_CM_ea_DISEASE_STAT1,
                                                                  Cohort3_modelX$n_CM_ea_DISEASE_STAT2,
                                                                  Cohort3_modelX$n_CM_ea_DISEASE_STAT3,
                                                                  sum(Cohort3_modelX$n_CM_ea_DISEASE_STAT0,
                                                                      Cohort3_modelX$n_CM_ea_DISEASE_STAT1,
                                                                      Cohort3_modelX$n_CM_ea_DISEASE_STAT2,
                                                                      Cohort3_modelX$n_CM_ea_DISEASE_STAT3),
                                                                  '',
                                                                  'No CM_sa',
                                                                  Cohort3_modelX$n_noCM_sa_DISEASE_STAT0,
                                                                  Cohort3_modelX$n_noCM_sa_DISEASE_STAT1,
                                                                  Cohort3_modelX$n_noCM_sa_DISEASE_STAT2,
                                                                  Cohort3_modelX$n_noCM_sa_DISEASE_STAT3,
                                                                  sum(Cohort3_modelX$n_noCM_sa_DISEASE_STAT0,
                                                                      Cohort3_modelX$n_noCM_sa_DISEASE_STAT1,
                                                                      Cohort3_modelX$n_noCM_sa_DISEASE_STAT2,
                                                                      Cohort3_modelX$n_noCM_sa_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sa',
                                                                  Cohort3_modelX$n_CM_sa_DISEASE_STAT0,
                                                                  Cohort3_modelX$n_CM_sa_DISEASE_STAT1,
                                                                  Cohort3_modelX$n_CM_sa_DISEASE_STAT2,
                                                                  Cohort3_modelX$n_CM_sa_DISEASE_STAT3,
                                                                  sum(Cohort3_modelX$n_CM_sa_DISEASE_STAT0,
                                                                      Cohort3_modelX$n_CM_sa_DISEASE_STAT1,
                                                                      Cohort3_modelX$n_CM_sa_DISEASE_STAT2,
                                                                      Cohort3_modelX$n_CM_sa_DISEASE_STAT3)),
                                                         nrow = 6,
                                                         ncol = 7,
                                                         byrow = T)), crosstab_Cohort3_modelX <- as.data.frame(matrix(NA, nrow = 6, ncol = 7, byrow = T)))
  ifelse(is.data.frame(Cohort4_modelX) == T, 
         crosstab_Cohort4_modelX <- as.data.frame(matrix(data = c('Cohort4_modelX',
                                                                  'No CM_pa',
                                                                  Cohort4_modelX$n_noCM_pa_DISEASE_STAT0,
                                                                  Cohort4_modelX$n_noCM_pa_DISEASE_STAT1,
                                                                  Cohort4_modelX$n_noCM_pa_DISEASE_STAT2,
                                                                  Cohort4_modelX$n_noCM_pa_DISEASE_STAT3,
                                                                  sum(Cohort4_modelX$n_noCM_pa_DISEASE_STAT0,
                                                                      Cohort4_modelX$n_noCM_pa_DISEASE_STAT1,
                                                                      Cohort4_modelX$n_noCM_pa_DISEASE_STAT2,
                                                                      Cohort4_modelX$n_noCM_pa_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_pa',
                                                                  Cohort4_modelX$n_CM_pa_DISEASE_STAT0,
                                                                  Cohort4_modelX$n_CM_pa_DISEASE_STAT1,
                                                                  Cohort4_modelX$n_CM_pa_DISEASE_STAT2,
                                                                  Cohort4_modelX$n_CM_pa_DISEASE_STAT3,
                                                                  sum(Cohort4_modelX$n_CM_pa_DISEASE_STAT0,
                                                                      Cohort4_modelX$n_CM_pa_DISEASE_STAT1,
                                                                      Cohort4_modelX$n_CM_pa_DISEASE_STAT2,
                                                                      Cohort4_modelX$n_CM_pa_DISEASE_STAT3),
                                                                  '',
                                                                  'No CM_ea',
                                                                  Cohort4_modelX$n_noCM_ea_DISEASE_STAT0,
                                                                  Cohort4_modelX$n_noCM_ea_DISEASE_STAT1,
                                                                  Cohort4_modelX$n_noCM_ea_DISEASE_STAT2,
                                                                  Cohort4_modelX$n_noCM_ea_DISEASE_STAT3,
                                                                  sum(Cohort4_modelX$n_noCM_ea_DISEASE_STAT0,
                                                                      Cohort4_modelX$n_noCM_ea_DISEASE_STAT1,
                                                                      Cohort4_modelX$n_noCM_ea_DISEASE_STAT2,
                                                                      Cohort4_modelX$n_noCM_ea_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_ea',
                                                                  Cohort4_modelX$n_CM_ea_DISEASE_STAT0,
                                                                  Cohort4_modelX$n_CM_ea_DISEASE_STAT1,
                                                                  Cohort4_modelX$n_CM_ea_DISEASE_STAT2,
                                                                  Cohort4_modelX$n_CM_ea_DISEASE_STAT3,
                                                                  sum(Cohort4_modelX$n_CM_ea_DISEASE_STAT0,
                                                                      Cohort4_modelX$n_CM_ea_DISEASE_STAT1,
                                                                      Cohort4_modelX$n_CM_ea_DISEASE_STAT2,
                                                                      Cohort4_modelX$n_CM_ea_DISEASE_STAT3),
                                                                  '',
                                                                  'No CM_sa',
                                                                  Cohort4_modelX$n_noCM_sa_DISEASE_STAT0,
                                                                  Cohort4_modelX$n_noCM_sa_DISEASE_STAT1,
                                                                  Cohort4_modelX$n_noCM_sa_DISEASE_STAT2,
                                                                  Cohort4_modelX$n_noCM_sa_DISEASE_STAT3,
                                                                  sum(Cohort4_modelX$n_noCM_sa_DISEASE_STAT0,
                                                                      Cohort4_modelX$n_noCM_sa_DISEASE_STAT1,
                                                                      Cohort4_modelX$n_noCM_sa_DISEASE_STAT2,
                                                                      Cohort4_modelX$n_noCM_sa_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sa',
                                                                  Cohort4_modelX$n_CM_sa_DISEASE_STAT0,
                                                                  Cohort4_modelX$n_CM_sa_DISEASE_STAT1,
                                                                  Cohort4_modelX$n_CM_sa_DISEASE_STAT2,
                                                                  Cohort4_modelX$n_CM_sa_DISEASE_STAT3,
                                                                  sum(Cohort4_modelX$n_CM_sa_DISEASE_STAT0,
                                                                      Cohort4_modelX$n_CM_sa_DISEASE_STAT1,
                                                                      Cohort4_modelX$n_CM_sa_DISEASE_STAT2,
                                                                      Cohort4_modelX$n_CM_sa_DISEASE_STAT3)),
                                                         nrow = 6,
                                                         ncol = 7,
                                                         byrow = T)), crosstab_Cohort4_modelX <- as.data.frame(matrix(NA, nrow = 6, ncol = 7, byrow = T)))
  ifelse(is.data.frame(Cohort5_modelX) == T, 
         crosstab_Cohort5_modelX <- as.data.frame(matrix(data = c('Cohort5_modelX',
                                                                  'No CM_pa',
                                                                  Cohort5_modelX$n_noCM_pa_DISEASE_STAT0,
                                                                  Cohort5_modelX$n_noCM_pa_DISEASE_STAT1,
                                                                  Cohort5_modelX$n_noCM_pa_DISEASE_STAT2,
                                                                  Cohort5_modelX$n_noCM_pa_DISEASE_STAT3,
                                                                  sum(Cohort5_modelX$n_noCM_pa_DISEASE_STAT0,
                                                                      Cohort5_modelX$n_noCM_pa_DISEASE_STAT1,
                                                                      Cohort5_modelX$n_noCM_pa_DISEASE_STAT2,
                                                                      Cohort5_modelX$n_noCM_pa_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_pa',
                                                                  Cohort5_modelX$n_CM_pa_DISEASE_STAT0,
                                                                  Cohort5_modelX$n_CM_pa_DISEASE_STAT1,
                                                                  Cohort5_modelX$n_CM_pa_DISEASE_STAT2,
                                                                  Cohort5_modelX$n_CM_pa_DISEASE_STAT3,
                                                                  sum(Cohort5_modelX$n_CM_pa_DISEASE_STAT0,
                                                                      Cohort5_modelX$n_CM_pa_DISEASE_STAT1,
                                                                      Cohort5_modelX$n_CM_pa_DISEASE_STAT2,
                                                                      Cohort5_modelX$n_CM_pa_DISEASE_STAT3),
                                                                  '',
                                                                  'No CM_ea',
                                                                  Cohort5_modelX$n_noCM_ea_DISEASE_STAT0,
                                                                  Cohort5_modelX$n_noCM_ea_DISEASE_STAT1,
                                                                  Cohort5_modelX$n_noCM_ea_DISEASE_STAT2,
                                                                  Cohort5_modelX$n_noCM_ea_DISEASE_STAT3,
                                                                  sum(Cohort5_modelX$n_noCM_ea_DISEASE_STAT0,
                                                                      Cohort5_modelX$n_noCM_ea_DISEASE_STAT1,
                                                                      Cohort5_modelX$n_noCM_ea_DISEASE_STAT2,
                                                                      Cohort5_modelX$n_noCM_ea_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_ea',
                                                                  Cohort5_modelX$n_CM_ea_DISEASE_STAT0,
                                                                  Cohort5_modelX$n_CM_ea_DISEASE_STAT1,
                                                                  Cohort5_modelX$n_CM_ea_DISEASE_STAT2,
                                                                  Cohort5_modelX$n_CM_ea_DISEASE_STAT3,
                                                                  sum(Cohort5_modelX$n_CM_ea_DISEASE_STAT0,
                                                                      Cohort5_modelX$n_CM_ea_DISEASE_STAT1,
                                                                      Cohort5_modelX$n_CM_ea_DISEASE_STAT2,
                                                                      Cohort5_modelX$n_CM_ea_DISEASE_STAT3),
                                                                  '',
                                                                  'No CM_sa',
                                                                  Cohort5_modelX$n_noCM_sa_DISEASE_STAT0,
                                                                  Cohort5_modelX$n_noCM_sa_DISEASE_STAT1,
                                                                  Cohort5_modelX$n_noCM_sa_DISEASE_STAT2,
                                                                  Cohort5_modelX$n_noCM_sa_DISEASE_STAT3,
                                                                  sum(Cohort5_modelX$n_noCM_sa_DISEASE_STAT0,
                                                                      Cohort5_modelX$n_noCM_sa_DISEASE_STAT1,
                                                                      Cohort5_modelX$n_noCM_sa_DISEASE_STAT2,
                                                                      Cohort5_modelX$n_noCM_sa_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sa',
                                                                  Cohort5_modelX$n_CM_sa_DISEASE_STAT0,
                                                                  Cohort5_modelX$n_CM_sa_DISEASE_STAT1,
                                                                  Cohort5_modelX$n_CM_sa_DISEASE_STAT2,
                                                                  Cohort5_modelX$n_CM_sa_DISEASE_STAT3,
                                                                  sum(Cohort5_modelX$n_CM_sa_DISEASE_STAT0,
                                                                      Cohort5_modelX$n_CM_sa_DISEASE_STAT1,
                                                                      Cohort5_modelX$n_CM_sa_DISEASE_STAT2,
                                                                      Cohort5_modelX$n_CM_sa_DISEASE_STAT3)),
                                                         nrow = 6,
                                                         ncol = 7,
                                                         byrow = T)), crosstab_Cohort5_modelX <- as.data.frame(matrix(NA, nrow = 6, ncol = 7, byrow = T)))
  ifelse(is.data.frame(Cohort6_modelX) == T, 
         crosstab_Cohort6_modelX <- as.data.frame(matrix(data = c('Cohort6_modelX',
                                                                  'No CM_pa',
                                                                  Cohort6_modelX$n_noCM_pa_DISEASE_STAT0,
                                                                  Cohort6_modelX$n_noCM_pa_DISEASE_STAT1,
                                                                  Cohort6_modelX$n_noCM_pa_DISEASE_STAT2,
                                                                  Cohort6_modelX$n_noCM_pa_DISEASE_STAT3,
                                                                  sum(Cohort6_modelX$n_noCM_pa_DISEASE_STAT0,
                                                                      Cohort6_modelX$n_noCM_pa_DISEASE_STAT1,
                                                                      Cohort6_modelX$n_noCM_pa_DISEASE_STAT2,
                                                                      Cohort6_modelX$n_noCM_pa_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_pa',
                                                                  Cohort6_modelX$n_CM_pa_DISEASE_STAT0,
                                                                  Cohort6_modelX$n_CM_pa_DISEASE_STAT1,
                                                                  Cohort6_modelX$n_CM_pa_DISEASE_STAT2,
                                                                  Cohort6_modelX$n_CM_pa_DISEASE_STAT3,
                                                                  sum(Cohort6_modelX$n_CM_pa_DISEASE_STAT0,
                                                                      Cohort6_modelX$n_CM_pa_DISEASE_STAT1,
                                                                      Cohort6_modelX$n_CM_pa_DISEASE_STAT2,
                                                                      Cohort6_modelX$n_CM_pa_DISEASE_STAT3),
                                                                  '',
                                                                  'No CM_ea',
                                                                  Cohort6_modelX$n_noCM_ea_DISEASE_STAT0,
                                                                  Cohort6_modelX$n_noCM_ea_DISEASE_STAT1,
                                                                  Cohort6_modelX$n_noCM_ea_DISEASE_STAT2,
                                                                  Cohort6_modelX$n_noCM_ea_DISEASE_STAT3,
                                                                  sum(Cohort6_modelX$n_noCM_ea_DISEASE_STAT0,
                                                                      Cohort6_modelX$n_noCM_ea_DISEASE_STAT1,
                                                                      Cohort6_modelX$n_noCM_ea_DISEASE_STAT2,
                                                                      Cohort6_modelX$n_noCM_ea_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_ea',
                                                                  Cohort6_modelX$n_CM_ea_DISEASE_STAT0,
                                                                  Cohort6_modelX$n_CM_ea_DISEASE_STAT1,
                                                                  Cohort6_modelX$n_CM_ea_DISEASE_STAT2,
                                                                  Cohort6_modelX$n_CM_ea_DISEASE_STAT3,
                                                                  sum(Cohort6_modelX$n_CM_ea_DISEASE_STAT0,
                                                                      Cohort6_modelX$n_CM_ea_DISEASE_STAT1,
                                                                      Cohort6_modelX$n_CM_ea_DISEASE_STAT2,
                                                                      Cohort6_modelX$n_CM_ea_DISEASE_STAT3),
                                                                  '',
                                                                  'No CM_sa',
                                                                  Cohort6_modelX$n_noCM_sa_DISEASE_STAT0,
                                                                  Cohort6_modelX$n_noCM_sa_DISEASE_STAT1,
                                                                  Cohort6_modelX$n_noCM_sa_DISEASE_STAT2,
                                                                  Cohort6_modelX$n_noCM_sa_DISEASE_STAT3,
                                                                  sum(Cohort6_modelX$n_noCM_sa_DISEASE_STAT0,
                                                                      Cohort6_modelX$n_noCM_sa_DISEASE_STAT1,
                                                                      Cohort6_modelX$n_noCM_sa_DISEASE_STAT2,
                                                                      Cohort6_modelX$n_noCM_sa_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sa',
                                                                  Cohort6_modelX$n_CM_sa_DISEASE_STAT0,
                                                                  Cohort6_modelX$n_CM_sa_DISEASE_STAT1,
                                                                  Cohort6_modelX$n_CM_sa_DISEASE_STAT2,
                                                                  Cohort6_modelX$n_CM_sa_DISEASE_STAT3,
                                                                  sum(Cohort6_modelX$n_CM_sa_DISEASE_STAT0,
                                                                      Cohort6_modelX$n_CM_sa_DISEASE_STAT1,
                                                                      Cohort6_modelX$n_CM_sa_DISEASE_STAT2,
                                                                      Cohort6_modelX$n_CM_sa_DISEASE_STAT3)),
                                                         nrow = 6,
                                                         ncol = 7,
                                                         byrow = T)), crosstab_Cohort6_modelX <- as.data.frame(matrix(NA, nrow = 6, ncol = 7, byrow = T)))
  ifelse(is.data.frame(Cohort7_modelX) == T, 
         crosstab_Cohort7_modelX <- as.data.frame(matrix(data = c('Cohort7_modelX',
                                                                  'No CM_pa',
                                                                  Cohort7_modelX$n_noCM_pa_DISEASE_STAT0,
                                                                  Cohort7_modelX$n_noCM_pa_DISEASE_STAT1,
                                                                  Cohort7_modelX$n_noCM_pa_DISEASE_STAT2,
                                                                  Cohort7_modelX$n_noCM_pa_DISEASE_STAT3,
                                                                  sum(Cohort7_modelX$n_noCM_pa_DISEASE_STAT0,
                                                                      Cohort7_modelX$n_noCM_pa_DISEASE_STAT1,
                                                                      Cohort7_modelX$n_noCM_pa_DISEASE_STAT2,
                                                                      Cohort7_modelX$n_noCM_pa_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_pa',
                                                                  Cohort7_modelX$n_CM_pa_DISEASE_STAT0,
                                                                  Cohort7_modelX$n_CM_pa_DISEASE_STAT1,
                                                                  Cohort7_modelX$n_CM_pa_DISEASE_STAT2,
                                                                  Cohort7_modelX$n_CM_pa_DISEASE_STAT3,
                                                                  sum(Cohort7_modelX$n_CM_pa_DISEASE_STAT0,
                                                                      Cohort7_modelX$n_CM_pa_DISEASE_STAT1,
                                                                      Cohort7_modelX$n_CM_pa_DISEASE_STAT2,
                                                                      Cohort7_modelX$n_CM_pa_DISEASE_STAT3),
                                                                  '',
                                                                  'No CM_ea',
                                                                  Cohort7_modelX$n_noCM_ea_DISEASE_STAT0,
                                                                  Cohort7_modelX$n_noCM_ea_DISEASE_STAT1,
                                                                  Cohort7_modelX$n_noCM_ea_DISEASE_STAT2,
                                                                  Cohort7_modelX$n_noCM_ea_DISEASE_STAT3,
                                                                  sum(Cohort7_modelX$n_noCM_ea_DISEASE_STAT0,
                                                                      Cohort7_modelX$n_noCM_ea_DISEASE_STAT1,
                                                                      Cohort7_modelX$n_noCM_ea_DISEASE_STAT2,
                                                                      Cohort7_modelX$n_noCM_ea_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_ea',
                                                                  Cohort7_modelX$n_CM_ea_DISEASE_STAT0,
                                                                  Cohort7_modelX$n_CM_ea_DISEASE_STAT1,
                                                                  Cohort7_modelX$n_CM_ea_DISEASE_STAT2,
                                                                  Cohort7_modelX$n_CM_ea_DISEASE_STAT3,
                                                                  sum(Cohort7_modelX$n_CM_ea_DISEASE_STAT0,
                                                                      Cohort7_modelX$n_CM_ea_DISEASE_STAT1,
                                                                      Cohort7_modelX$n_CM_ea_DISEASE_STAT2,
                                                                      Cohort7_modelX$n_CM_ea_DISEASE_STAT3),
                                                                  '',
                                                                  'No CM_sa',
                                                                  Cohort7_modelX$n_noCM_sa_DISEASE_STAT0,
                                                                  Cohort7_modelX$n_noCM_sa_DISEASE_STAT1,
                                                                  Cohort7_modelX$n_noCM_sa_DISEASE_STAT2,
                                                                  Cohort7_modelX$n_noCM_sa_DISEASE_STAT3,
                                                                  sum(Cohort7_modelX$n_noCM_sa_DISEASE_STAT0,
                                                                      Cohort7_modelX$n_noCM_sa_DISEASE_STAT1,
                                                                      Cohort7_modelX$n_noCM_sa_DISEASE_STAT2,
                                                                      Cohort7_modelX$n_noCM_sa_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sa',
                                                                  Cohort7_modelX$n_CM_sa_DISEASE_STAT0,
                                                                  Cohort7_modelX$n_CM_sa_DISEASE_STAT1,
                                                                  Cohort7_modelX$n_CM_sa_DISEASE_STAT2,
                                                                  Cohort7_modelX$n_CM_sa_DISEASE_STAT3,
                                                                  sum(Cohort7_modelX$n_CM_sa_DISEASE_STAT0,
                                                                      Cohort7_modelX$n_CM_sa_DISEASE_STAT1,
                                                                      Cohort7_modelX$n_CM_sa_DISEASE_STAT2,
                                                                      Cohort7_modelX$n_CM_sa_DISEASE_STAT3)),
                                                         nrow = 6,
                                                         ncol = 7,
                                                         byrow = T)), crosstab_Cohort7_modelX <- as.data.frame(matrix(NA, nrow = 6, ncol = 7, byrow = T)))
  ifelse(is.data.frame(Cohort8_modelX) == T, 
         crosstab_Cohort8_modelX <- as.data.frame(matrix(data = c('Cohort8_modelX',
                                                                  'No CM_pa',
                                                                  Cohort8_modelX$n_noCM_pa_DISEASE_STAT0,
                                                                  Cohort8_modelX$n_noCM_pa_DISEASE_STAT1,
                                                                  Cohort8_modelX$n_noCM_pa_DISEASE_STAT2,
                                                                  Cohort8_modelX$n_noCM_pa_DISEASE_STAT3,
                                                                  sum(Cohort8_modelX$n_noCM_pa_DISEASE_STAT0,
                                                                      Cohort8_modelX$n_noCM_pa_DISEASE_STAT1,
                                                                      Cohort8_modelX$n_noCM_pa_DISEASE_STAT2,
                                                                      Cohort8_modelX$n_noCM_pa_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_pa',
                                                                  Cohort8_modelX$n_CM_pa_DISEASE_STAT0,
                                                                  Cohort8_modelX$n_CM_pa_DISEASE_STAT1,
                                                                  Cohort8_modelX$n_CM_pa_DISEASE_STAT2,
                                                                  Cohort8_modelX$n_CM_pa_DISEASE_STAT3,
                                                                  sum(Cohort8_modelX$n_CM_pa_DISEASE_STAT0,
                                                                      Cohort8_modelX$n_CM_pa_DISEASE_STAT1,
                                                                      Cohort8_modelX$n_CM_pa_DISEASE_STAT2,
                                                                      Cohort8_modelX$n_CM_pa_DISEASE_STAT3),
                                                                  '',
                                                                  'No CM_ea',
                                                                  Cohort8_modelX$n_noCM_ea_DISEASE_STAT0,
                                                                  Cohort8_modelX$n_noCM_ea_DISEASE_STAT1,
                                                                  Cohort8_modelX$n_noCM_ea_DISEASE_STAT2,
                                                                  Cohort8_modelX$n_noCM_ea_DISEASE_STAT3,
                                                                  sum(Cohort8_modelX$n_noCM_ea_DISEASE_STAT0,
                                                                      Cohort8_modelX$n_noCM_ea_DISEASE_STAT1,
                                                                      Cohort8_modelX$n_noCM_ea_DISEASE_STAT2,
                                                                      Cohort8_modelX$n_noCM_ea_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_ea',
                                                                  Cohort8_modelX$n_CM_ea_DISEASE_STAT0,
                                                                  Cohort8_modelX$n_CM_ea_DISEASE_STAT1,
                                                                  Cohort8_modelX$n_CM_ea_DISEASE_STAT2,
                                                                  Cohort8_modelX$n_CM_ea_DISEASE_STAT3,
                                                                  sum(Cohort8_modelX$n_CM_ea_DISEASE_STAT0,
                                                                      Cohort8_modelX$n_CM_ea_DISEASE_STAT1,
                                                                      Cohort8_modelX$n_CM_ea_DISEASE_STAT2,
                                                                      Cohort8_modelX$n_CM_ea_DISEASE_STAT3),
                                                                  '',
                                                                  'No CM_sa',
                                                                  Cohort8_modelX$n_noCM_sa_DISEASE_STAT0,
                                                                  Cohort8_modelX$n_noCM_sa_DISEASE_STAT1,
                                                                  Cohort8_modelX$n_noCM_sa_DISEASE_STAT2,
                                                                  Cohort8_modelX$n_noCM_sa_DISEASE_STAT3,
                                                                  sum(Cohort8_modelX$n_noCM_sa_DISEASE_STAT0,
                                                                      Cohort8_modelX$n_noCM_sa_DISEASE_STAT1,
                                                                      Cohort8_modelX$n_noCM_sa_DISEASE_STAT2,
                                                                      Cohort8_modelX$n_noCM_sa_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sa',
                                                                  Cohort8_modelX$n_CM_sa_DISEASE_STAT0,
                                                                  Cohort8_modelX$n_CM_sa_DISEASE_STAT1,
                                                                  Cohort8_modelX$n_CM_sa_DISEASE_STAT2,
                                                                  Cohort8_modelX$n_CM_sa_DISEASE_STAT3,
                                                                  sum(Cohort8_modelX$n_CM_sa_DISEASE_STAT0,
                                                                      Cohort8_modelX$n_CM_sa_DISEASE_STAT1,
                                                                      Cohort8_modelX$n_CM_sa_DISEASE_STAT2,
                                                                      Cohort8_modelX$n_CM_sa_DISEASE_STAT3)),
                                                         nrow = 6,
                                                         ncol = 7,
                                                         byrow = T)), crosstab_Cohort8_modelX <- as.data.frame(matrix(NA, nrow = 6, ncol = 7, byrow = T)))
  ifelse(is.data.frame(Cohort9_modelX) == T, 
         crosstab_Cohort9_modelX <- as.data.frame(matrix(data = c('Cohort9_modelX',
                                                                  'No CM_pa',
                                                                  Cohort9_modelX$n_noCM_pa_DISEASE_STAT0,
                                                                  Cohort9_modelX$n_noCM_pa_DISEASE_STAT1,
                                                                  Cohort9_modelX$n_noCM_pa_DISEASE_STAT2,
                                                                  Cohort9_modelX$n_noCM_pa_DISEASE_STAT3,
                                                                  sum(Cohort9_modelX$n_noCM_pa_DISEASE_STAT0,
                                                                      Cohort9_modelX$n_noCM_pa_DISEASE_STAT1,
                                                                      Cohort9_modelX$n_noCM_pa_DISEASE_STAT2,
                                                                      Cohort9_modelX$n_noCM_pa_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_pa',
                                                                  Cohort9_modelX$n_CM_pa_DISEASE_STAT0,
                                                                  Cohort9_modelX$n_CM_pa_DISEASE_STAT1,
                                                                  Cohort9_modelX$n_CM_pa_DISEASE_STAT2,
                                                                  Cohort9_modelX$n_CM_pa_DISEASE_STAT3,
                                                                  sum(Cohort9_modelX$n_CM_pa_DISEASE_STAT0,
                                                                      Cohort9_modelX$n_CM_pa_DISEASE_STAT1,
                                                                      Cohort9_modelX$n_CM_pa_DISEASE_STAT2,
                                                                      Cohort9_modelX$n_CM_pa_DISEASE_STAT3),
                                                                  '',
                                                                  'No CM_ea',
                                                                  Cohort9_modelX$n_noCM_ea_DISEASE_STAT0,
                                                                  Cohort9_modelX$n_noCM_ea_DISEASE_STAT1,
                                                                  Cohort9_modelX$n_noCM_ea_DISEASE_STAT2,
                                                                  Cohort9_modelX$n_noCM_ea_DISEASE_STAT3,
                                                                  sum(Cohort9_modelX$n_noCM_ea_DISEASE_STAT0,
                                                                      Cohort9_modelX$n_noCM_ea_DISEASE_STAT1,
                                                                      Cohort9_modelX$n_noCM_ea_DISEASE_STAT2,
                                                                      Cohort9_modelX$n_noCM_ea_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_ea',
                                                                  Cohort9_modelX$n_CM_ea_DISEASE_STAT0,
                                                                  Cohort9_modelX$n_CM_ea_DISEASE_STAT1,
                                                                  Cohort9_modelX$n_CM_ea_DISEASE_STAT2,
                                                                  Cohort9_modelX$n_CM_ea_DISEASE_STAT3,
                                                                  sum(Cohort9_modelX$n_CM_ea_DISEASE_STAT0,
                                                                      Cohort9_modelX$n_CM_ea_DISEASE_STAT1,
                                                                      Cohort9_modelX$n_CM_ea_DISEASE_STAT2,
                                                                      Cohort9_modelX$n_CM_ea_DISEASE_STAT3),
                                                                  '',
                                                                  'No CM_sa',
                                                                  Cohort9_modelX$n_noCM_sa_DISEASE_STAT0,
                                                                  Cohort9_modelX$n_noCM_sa_DISEASE_STAT1,
                                                                  Cohort9_modelX$n_noCM_sa_DISEASE_STAT2,
                                                                  Cohort9_modelX$n_noCM_sa_DISEASE_STAT3,
                                                                  sum(Cohort9_modelX$n_noCM_sa_DISEASE_STAT0,
                                                                      Cohort9_modelX$n_noCM_sa_DISEASE_STAT1,
                                                                      Cohort9_modelX$n_noCM_sa_DISEASE_STAT2,
                                                                      Cohort9_modelX$n_noCM_sa_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sa',
                                                                  Cohort9_modelX$n_CM_sa_DISEASE_STAT0,
                                                                  Cohort9_modelX$n_CM_sa_DISEASE_STAT1,
                                                                  Cohort9_modelX$n_CM_sa_DISEASE_STAT2,
                                                                  Cohort9_modelX$n_CM_sa_DISEASE_STAT3,
                                                                  sum(Cohort9_modelX$n_CM_sa_DISEASE_STAT0,
                                                                      Cohort9_modelX$n_CM_sa_DISEASE_STAT1,
                                                                      Cohort9_modelX$n_CM_sa_DISEASE_STAT2,
                                                                      Cohort9_modelX$n_CM_sa_DISEASE_STAT3)),
                                                         nrow = 6,
                                                         ncol = 7,
                                                         byrow = T)), crosstab_Cohort9_modelX <- as.data.frame(matrix(NA, nrow = 6, ncol = 7, byrow = T)))
  ifelse(is.data.frame(Cohort10_modelX) == T, 
         crosstab_Cohort10_modelX <- as.data.frame(matrix(data = c('Cohort10_modelX',
                                                                  'No CM_pa',
                                                                  Cohort10_modelX$n_noCM_pa_DISEASE_STAT0,
                                                                  Cohort10_modelX$n_noCM_pa_DISEASE_STAT1,
                                                                  Cohort10_modelX$n_noCM_pa_DISEASE_STAT2,
                                                                  Cohort10_modelX$n_noCM_pa_DISEASE_STAT3,
                                                                  sum(Cohort10_modelX$n_noCM_pa_DISEASE_STAT0,
                                                                      Cohort10_modelX$n_noCM_pa_DISEASE_STAT1,
                                                                      Cohort10_modelX$n_noCM_pa_DISEASE_STAT2,
                                                                      Cohort10_modelX$n_noCM_pa_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_pa',
                                                                  Cohort10_modelX$n_CM_pa_DISEASE_STAT0,
                                                                  Cohort10_modelX$n_CM_pa_DISEASE_STAT1,
                                                                  Cohort10_modelX$n_CM_pa_DISEASE_STAT2,
                                                                  Cohort10_modelX$n_CM_pa_DISEASE_STAT3,
                                                                  sum(Cohort10_modelX$n_CM_pa_DISEASE_STAT0,
                                                                      Cohort10_modelX$n_CM_pa_DISEASE_STAT1,
                                                                      Cohort10_modelX$n_CM_pa_DISEASE_STAT2,
                                                                      Cohort10_modelX$n_CM_pa_DISEASE_STAT3),
                                                                  '',
                                                                  'No CM_ea',
                                                                  Cohort10_modelX$n_noCM_ea_DISEASE_STAT0,
                                                                  Cohort10_modelX$n_noCM_ea_DISEASE_STAT1,
                                                                  Cohort10_modelX$n_noCM_ea_DISEASE_STAT2,
                                                                  Cohort10_modelX$n_noCM_ea_DISEASE_STAT3,
                                                                  sum(Cohort10_modelX$n_noCM_ea_DISEASE_STAT0,
                                                                      Cohort10_modelX$n_noCM_ea_DISEASE_STAT1,
                                                                      Cohort10_modelX$n_noCM_ea_DISEASE_STAT2,
                                                                      Cohort10_modelX$n_noCM_ea_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_ea',
                                                                  Cohort10_modelX$n_CM_ea_DISEASE_STAT0,
                                                                  Cohort10_modelX$n_CM_ea_DISEASE_STAT1,
                                                                  Cohort10_modelX$n_CM_ea_DISEASE_STAT2,
                                                                  Cohort10_modelX$n_CM_ea_DISEASE_STAT3,
                                                                  sum(Cohort10_modelX$n_CM_ea_DISEASE_STAT0,
                                                                      Cohort10_modelX$n_CM_ea_DISEASE_STAT1,
                                                                      Cohort10_modelX$n_CM_ea_DISEASE_STAT2,
                                                                      Cohort10_modelX$n_CM_ea_DISEASE_STAT3),
                                                                  '',
                                                                  'No CM_sa',
                                                                  Cohort10_modelX$n_noCM_sa_DISEASE_STAT0,
                                                                  Cohort10_modelX$n_noCM_sa_DISEASE_STAT1,
                                                                  Cohort10_modelX$n_noCM_sa_DISEASE_STAT2,
                                                                  Cohort10_modelX$n_noCM_sa_DISEASE_STAT3,
                                                                  sum(Cohort10_modelX$n_noCM_sa_DISEASE_STAT0,
                                                                      Cohort10_modelX$n_noCM_sa_DISEASE_STAT1,
                                                                      Cohort10_modelX$n_noCM_sa_DISEASE_STAT2,
                                                                      Cohort10_modelX$n_noCM_sa_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sa',
                                                                  Cohort10_modelX$n_CM_sa_DISEASE_STAT0,
                                                                  Cohort10_modelX$n_CM_sa_DISEASE_STAT1,
                                                                  Cohort10_modelX$n_CM_sa_DISEASE_STAT2,
                                                                  Cohort10_modelX$n_CM_sa_DISEASE_STAT3,
                                                                  sum(Cohort10_modelX$n_CM_sa_DISEASE_STAT0,
                                                                      Cohort10_modelX$n_CM_sa_DISEASE_STAT1,
                                                                      Cohort10_modelX$n_CM_sa_DISEASE_STAT2,
                                                                      Cohort10_modelX$n_CM_sa_DISEASE_STAT3)),
                                                         nrow = 6,
                                                         ncol = 7,
                                                         byrow = T)), crosstab_Cohort10_modelX <- as.data.frame(matrix(NA, nrow = 6, ncol = 7, byrow = T)))
  ifelse(is.data.frame(Cohort11_modelX) == T, 
         crosstab_Cohort11_modelX <- as.data.frame(matrix(data = c('Cohort11_modelX',
                                                                   'No CM_pa',
                                                                   Cohort11_modelX$n_noCM_pa_DISEASE_STAT0,
                                                                   Cohort11_modelX$n_noCM_pa_DISEASE_STAT1,
                                                                   Cohort11_modelX$n_noCM_pa_DISEASE_STAT2,
                                                                   Cohort11_modelX$n_noCM_pa_DISEASE_STAT3,
                                                                   sum(Cohort11_modelX$n_noCM_pa_DISEASE_STAT0,
                                                                       Cohort11_modelX$n_noCM_pa_DISEASE_STAT1,
                                                                       Cohort11_modelX$n_noCM_pa_DISEASE_STAT2,
                                                                       Cohort11_modelX$n_noCM_pa_DISEASE_STAT3),
                                                                   '',
                                                                   'CM_pa',
                                                                   Cohort11_modelX$n_CM_pa_DISEASE_STAT0,
                                                                   Cohort11_modelX$n_CM_pa_DISEASE_STAT1,
                                                                   Cohort11_modelX$n_CM_pa_DISEASE_STAT2,
                                                                   Cohort11_modelX$n_CM_pa_DISEASE_STAT3,
                                                                   sum(Cohort11_modelX$n_CM_pa_DISEASE_STAT0,
                                                                       Cohort11_modelX$n_CM_pa_DISEASE_STAT1,
                                                                       Cohort11_modelX$n_CM_pa_DISEASE_STAT2,
                                                                       Cohort11_modelX$n_CM_pa_DISEASE_STAT3),
                                                                   '',
                                                                   'No CM_ea',
                                                                   Cohort11_modelX$n_noCM_ea_DISEASE_STAT0,
                                                                   Cohort11_modelX$n_noCM_ea_DISEASE_STAT1,
                                                                   Cohort11_modelX$n_noCM_ea_DISEASE_STAT2,
                                                                   Cohort11_modelX$n_noCM_ea_DISEASE_STAT3,
                                                                   sum(Cohort11_modelX$n_noCM_ea_DISEASE_STAT0,
                                                                       Cohort11_modelX$n_noCM_ea_DISEASE_STAT1,
                                                                       Cohort11_modelX$n_noCM_ea_DISEASE_STAT2,
                                                                       Cohort11_modelX$n_noCM_ea_DISEASE_STAT3),
                                                                   '',
                                                                   'CM_ea',
                                                                   Cohort11_modelX$n_CM_ea_DISEASE_STAT0,
                                                                   Cohort11_modelX$n_CM_ea_DISEASE_STAT1,
                                                                   Cohort11_modelX$n_CM_ea_DISEASE_STAT2,
                                                                   Cohort11_modelX$n_CM_ea_DISEASE_STAT3,
                                                                   sum(Cohort11_modelX$n_CM_ea_DISEASE_STAT0,
                                                                       Cohort11_modelX$n_CM_ea_DISEASE_STAT1,
                                                                       Cohort11_modelX$n_CM_ea_DISEASE_STAT2,
                                                                       Cohort11_modelX$n_CM_ea_DISEASE_STAT3),
                                                                   '',
                                                                   'No CM_sa',
                                                                   Cohort11_modelX$n_noCM_sa_DISEASE_STAT0,
                                                                   Cohort11_modelX$n_noCM_sa_DISEASE_STAT1,
                                                                   Cohort11_modelX$n_noCM_sa_DISEASE_STAT2,
                                                                   Cohort11_modelX$n_noCM_sa_DISEASE_STAT3,
                                                                   sum(Cohort11_modelX$n_noCM_sa_DISEASE_STAT0,
                                                                       Cohort11_modelX$n_noCM_sa_DISEASE_STAT1,
                                                                       Cohort11_modelX$n_noCM_sa_DISEASE_STAT2,
                                                                       Cohort11_modelX$n_noCM_sa_DISEASE_STAT3),
                                                                   '',
                                                                   'CM_sa',
                                                                   Cohort11_modelX$n_CM_sa_DISEASE_STAT0,
                                                                   Cohort11_modelX$n_CM_sa_DISEASE_STAT1,
                                                                   Cohort11_modelX$n_CM_sa_DISEASE_STAT2,
                                                                   Cohort11_modelX$n_CM_sa_DISEASE_STAT3,
                                                                   sum(Cohort11_modelX$n_CM_sa_DISEASE_STAT0,
                                                                       Cohort11_modelX$n_CM_sa_DISEASE_STAT1,
                                                                       Cohort11_modelX$n_CM_sa_DISEASE_STAT2,
                                                                       Cohort11_modelX$n_CM_sa_DISEASE_STAT3)),
                                                          nrow = 6,
                                                          ncol = 7,
                                                          byrow = T)), crosstab_Cohort11_modelX <- as.data.frame(matrix(NA, nrow = 6, ncol = 7, byrow = T)))
  ifelse(is.data.frame(Cohort12_modelX) == T, 
         crosstab_Cohort12_modelX <- as.data.frame(matrix(data = c('Cohort12_modelX',
                                                                   'No CM_pa',
                                                                   Cohort12_modelX$n_noCM_pa_DISEASE_STAT0,
                                                                   Cohort12_modelX$n_noCM_pa_DISEASE_STAT1,
                                                                   Cohort12_modelX$n_noCM_pa_DISEASE_STAT2,
                                                                   Cohort12_modelX$n_noCM_pa_DISEASE_STAT3,
                                                                   sum(Cohort12_modelX$n_noCM_pa_DISEASE_STAT0,
                                                                       Cohort12_modelX$n_noCM_pa_DISEASE_STAT1,
                                                                       Cohort12_modelX$n_noCM_pa_DISEASE_STAT2,
                                                                       Cohort12_modelX$n_noCM_pa_DISEASE_STAT3),
                                                                   '',
                                                                   'CM_pa',
                                                                   Cohort12_modelX$n_CM_pa_DISEASE_STAT0,
                                                                   Cohort12_modelX$n_CM_pa_DISEASE_STAT1,
                                                                   Cohort12_modelX$n_CM_pa_DISEASE_STAT2,
                                                                   Cohort12_modelX$n_CM_pa_DISEASE_STAT3,
                                                                   sum(Cohort12_modelX$n_CM_pa_DISEASE_STAT0,
                                                                       Cohort12_modelX$n_CM_pa_DISEASE_STAT1,
                                                                       Cohort12_modelX$n_CM_pa_DISEASE_STAT2,
                                                                       Cohort12_modelX$n_CM_pa_DISEASE_STAT3),
                                                                   '',
                                                                   'No CM_ea',
                                                                   Cohort12_modelX$n_noCM_ea_DISEASE_STAT0,
                                                                   Cohort12_modelX$n_noCM_ea_DISEASE_STAT1,
                                                                   Cohort12_modelX$n_noCM_ea_DISEASE_STAT2,
                                                                   Cohort12_modelX$n_noCM_ea_DISEASE_STAT3,
                                                                   sum(Cohort12_modelX$n_noCM_ea_DISEASE_STAT0,
                                                                       Cohort12_modelX$n_noCM_ea_DISEASE_STAT1,
                                                                       Cohort12_modelX$n_noCM_ea_DISEASE_STAT2,
                                                                       Cohort12_modelX$n_noCM_ea_DISEASE_STAT3),
                                                                   '',
                                                                   'CM_ea',
                                                                   Cohort12_modelX$n_CM_ea_DISEASE_STAT0,
                                                                   Cohort12_modelX$n_CM_ea_DISEASE_STAT1,
                                                                   Cohort12_modelX$n_CM_ea_DISEASE_STAT2,
                                                                   Cohort12_modelX$n_CM_ea_DISEASE_STAT3,
                                                                   sum(Cohort12_modelX$n_CM_ea_DISEASE_STAT0,
                                                                       Cohort12_modelX$n_CM_ea_DISEASE_STAT1,
                                                                       Cohort12_modelX$n_CM_ea_DISEASE_STAT2,
                                                                       Cohort12_modelX$n_CM_ea_DISEASE_STAT3),
                                                                   '',
                                                                   'No CM_sa',
                                                                   Cohort12_modelX$n_noCM_sa_DISEASE_STAT0,
                                                                   Cohort12_modelX$n_noCM_sa_DISEASE_STAT1,
                                                                   Cohort12_modelX$n_noCM_sa_DISEASE_STAT2,
                                                                   Cohort12_modelX$n_noCM_sa_DISEASE_STAT3,
                                                                   sum(Cohort12_modelX$n_noCM_sa_DISEASE_STAT0,
                                                                       Cohort12_modelX$n_noCM_sa_DISEASE_STAT1,
                                                                       Cohort12_modelX$n_noCM_sa_DISEASE_STAT2,
                                                                       Cohort12_modelX$n_noCM_sa_DISEASE_STAT3),
                                                                   '',
                                                                   'CM_sa',
                                                                   Cohort12_modelX$n_CM_sa_DISEASE_STAT0,
                                                                   Cohort12_modelX$n_CM_sa_DISEASE_STAT1,
                                                                   Cohort12_modelX$n_CM_sa_DISEASE_STAT2,
                                                                   Cohort12_modelX$n_CM_sa_DISEASE_STAT3,
                                                                   sum(Cohort12_modelX$n_CM_sa_DISEASE_STAT0,
                                                                       Cohort12_modelX$n_CM_sa_DISEASE_STAT1,
                                                                       Cohort12_modelX$n_CM_sa_DISEASE_STAT2,
                                                                       Cohort12_modelX$n_CM_sa_DISEASE_STAT3)),
                                                          nrow = 6,
                                                          ncol = 7,
                                                          byrow = T)), crosstab_Cohort12_modelX <- as.data.frame(matrix(NA, nrow = 6, ncol = 7, byrow = T)))
  ifelse(is.data.frame(Cohort13_modelX) == T, 
         crosstab_Cohort13_modelX <- as.data.frame(matrix(data = c('Cohort13_modelX',
                                                                   'No CM_pa',
                                                                   Cohort13_modelX$n_noCM_pa_DISEASE_STAT0,
                                                                   Cohort13_modelX$n_noCM_pa_DISEASE_STAT1,
                                                                   Cohort13_modelX$n_noCM_pa_DISEASE_STAT2,
                                                                   Cohort13_modelX$n_noCM_pa_DISEASE_STAT3,
                                                                   sum(Cohort13_modelX$n_noCM_pa_DISEASE_STAT0,
                                                                       Cohort13_modelX$n_noCM_pa_DISEASE_STAT1,
                                                                       Cohort13_modelX$n_noCM_pa_DISEASE_STAT2,
                                                                       Cohort13_modelX$n_noCM_pa_DISEASE_STAT3),
                                                                   '',
                                                                   'CM_pa',
                                                                   Cohort13_modelX$n_CM_pa_DISEASE_STAT0,
                                                                   Cohort13_modelX$n_CM_pa_DISEASE_STAT1,
                                                                   Cohort13_modelX$n_CM_pa_DISEASE_STAT2,
                                                                   Cohort13_modelX$n_CM_pa_DISEASE_STAT3,
                                                                   sum(Cohort13_modelX$n_CM_pa_DISEASE_STAT0,
                                                                       Cohort13_modelX$n_CM_pa_DISEASE_STAT1,
                                                                       Cohort13_modelX$n_CM_pa_DISEASE_STAT2,
                                                                       Cohort13_modelX$n_CM_pa_DISEASE_STAT3),
                                                                   '',
                                                                   'No CM_ea',
                                                                   Cohort13_modelX$n_noCM_ea_DISEASE_STAT0,
                                                                   Cohort13_modelX$n_noCM_ea_DISEASE_STAT1,
                                                                   Cohort13_modelX$n_noCM_ea_DISEASE_STAT2,
                                                                   Cohort13_modelX$n_noCM_ea_DISEASE_STAT3,
                                                                   sum(Cohort13_modelX$n_noCM_ea_DISEASE_STAT0,
                                                                       Cohort13_modelX$n_noCM_ea_DISEASE_STAT1,
                                                                       Cohort13_modelX$n_noCM_ea_DISEASE_STAT2,
                                                                       Cohort13_modelX$n_noCM_ea_DISEASE_STAT3),
                                                                   '',
                                                                   'CM_ea',
                                                                   Cohort13_modelX$n_CM_ea_DISEASE_STAT0,
                                                                   Cohort13_modelX$n_CM_ea_DISEASE_STAT1,
                                                                   Cohort13_modelX$n_CM_ea_DISEASE_STAT2,
                                                                   Cohort13_modelX$n_CM_ea_DISEASE_STAT3,
                                                                   sum(Cohort13_modelX$n_CM_ea_DISEASE_STAT0,
                                                                       Cohort13_modelX$n_CM_ea_DISEASE_STAT1,
                                                                       Cohort13_modelX$n_CM_ea_DISEASE_STAT2,
                                                                       Cohort13_modelX$n_CM_ea_DISEASE_STAT3),
                                                                   '',
                                                                   'No CM_sa',
                                                                   Cohort13_modelX$n_noCM_sa_DISEASE_STAT0,
                                                                   Cohort13_modelX$n_noCM_sa_DISEASE_STAT1,
                                                                   Cohort13_modelX$n_noCM_sa_DISEASE_STAT2,
                                                                   Cohort13_modelX$n_noCM_sa_DISEASE_STAT3,
                                                                   sum(Cohort13_modelX$n_noCM_sa_DISEASE_STAT0,
                                                                       Cohort13_modelX$n_noCM_sa_DISEASE_STAT1,
                                                                       Cohort13_modelX$n_noCM_sa_DISEASE_STAT2,
                                                                       Cohort13_modelX$n_noCM_sa_DISEASE_STAT3),
                                                                   '',
                                                                   'CM_sa',
                                                                   Cohort13_modelX$n_CM_sa_DISEASE_STAT0,
                                                                   Cohort13_modelX$n_CM_sa_DISEASE_STAT1,
                                                                   Cohort13_modelX$n_CM_sa_DISEASE_STAT2,
                                                                   Cohort13_modelX$n_CM_sa_DISEASE_STAT3,
                                                                   sum(Cohort13_modelX$n_CM_sa_DISEASE_STAT0,
                                                                       Cohort13_modelX$n_CM_sa_DISEASE_STAT1,
                                                                       Cohort13_modelX$n_CM_sa_DISEASE_STAT2,
                                                                       Cohort13_modelX$n_CM_sa_DISEASE_STAT3)),
                                                          nrow = 6,
                                                          ncol = 7,
                                                          byrow = T)), crosstab_Cohort13_modelX <- as.data.frame(matrix(NA, nrow = 6, ncol = 7, byrow = T)))
  
  crosstab_model <- rbind(crosstab_Cohort1_modelX,
                          crosstab_Cohort2_modelX,
                          crosstab_Cohort3_modelX,
                          crosstab_Cohort4_modelX,
                          crosstab_Cohort5_modelX,
                          crosstab_Cohort6_modelX,
                          crosstab_Cohort7_modelX,
                          crosstab_Cohort8_modelX,
                          crosstab_Cohort9_modelX,
                          crosstab_Cohort10_modelX,
                          crosstab_Cohort11_modelX,
                          crosstab_Cohort12_modelX,
                          crosstab_Cohort13_modelX, make.row.names = F)
  colnames(crosstab_model) <- c('','','Healthy','Dep only', 'Cmd disease only','Comorbidity', 'Total')
  
  return(crosstab_model)
}

# Use the following function for multinomial logistic models with a predictor of interest with three levels (model 7)

crosstabsmultinomial_multilevelpredictor_function <- function(Cohort1_modelX, 
                                                              Cohort2_modelX,
                                                              Cohort3_modelX,
                                                              Cohort4_modelX, 
                                                              Cohort5_modelX,
                                                              Cohort6_modelX,
                                                              Cohort7_modelX, 
                                                              Cohort8_modelX,
                                                              Cohort9_modelX,
                                                              Cohort10_modelX,
                                                              Cohort11_modelX, 
                                                              Cohort12_modelX,
                                                              Cohort13_modelX){
  ifelse(is.data.frame(Cohort1_modelX) == T, 
         crosstab_Cohort1_modelX <- as.data.frame(matrix(data = c('Cohort1_modelX',
                                                                  'No CM',
                                                                  Cohort1_modelX$n_CM_sev0_DISEASE_STAT0,
                                                                  Cohort1_modelX$n_CM_sev0_DISEASE_STAT1,
                                                                  Cohort1_modelX$n_CM_sev0_DISEASE_STAT2,
                                                                  Cohort1_modelX$n_CM_sev0_DISEASE_STAT3,
                                                                  sum(Cohort1_modelX$n_CM_sev0_DISEASE_STAT0,
                                                                      Cohort1_modelX$n_CM_sev0_DISEASE_STAT1,
                                                                      Cohort1_modelX$n_CM_sev0_DISEASE_STAT2,
                                                                      Cohort1_modelX$n_CM_sev0_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sev1',
                                                                  Cohort1_modelX$n_CM_sev1_DISEASE_STAT0,
                                                                  Cohort1_modelX$n_CM_sev1_DISEASE_STAT1,
                                                                  Cohort1_modelX$n_CM_sev1_DISEASE_STAT2,
                                                                  Cohort1_modelX$n_CM_sev1_DISEASE_STAT3,
                                                                  sum(Cohort1_modelX$n_CM_sev1_DISEASE_STAT0,
                                                                      Cohort1_modelX$n_CM_sev1_DISEASE_STAT1,
                                                                      Cohort1_modelX$n_CM_sev1_DISEASE_STAT2,
                                                                      Cohort1_modelX$n_CM_sev1_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sev2',
                                                                  Cohort1_modelX$n_CM_sev2_DISEASE_STAT0,
                                                                  Cohort1_modelX$n_CM_sev2_DISEASE_STAT1,
                                                                  Cohort1_modelX$n_CM_sev2_DISEASE_STAT2,
                                                                  Cohort1_modelX$n_CM_sev2_DISEASE_STAT3,
                                                                  sum(Cohort1_modelX$n_CM_sev2_DISEASE_STAT0,
                                                                      Cohort1_modelX$n_CM_sev2_DISEASE_STAT1,
                                                                      Cohort1_modelX$n_CM_sev2_DISEASE_STAT2,
                                                                      Cohort1_modelX$n_CM_sev2_DISEASE_STAT3)),
                                                         nrow = 3,
                                                         ncol = 7,
                                                         byrow = T)), crosstab_Cohort1_modelX <- as.data.frame(matrix(NA, nrow = 3, ncol = 7, byrow = T)))
  ifelse(is.data.frame(Cohort2_modelX) == T, 
         crosstab_Cohort2_modelX <- as.data.frame(matrix(data = c('Cohort2_modelX',
                                                                  'No CM',
                                                                  Cohort2_modelX$n_CM_sev0_DISEASE_STAT0,
                                                                  Cohort2_modelX$n_CM_sev0_DISEASE_STAT1,
                                                                  Cohort2_modelX$n_CM_sev0_DISEASE_STAT2,
                                                                  Cohort2_modelX$n_CM_sev0_DISEASE_STAT3,
                                                                  sum(Cohort2_modelX$n_CM_sev0_DISEASE_STAT0,
                                                                      Cohort2_modelX$n_CM_sev0_DISEASE_STAT1,
                                                                      Cohort2_modelX$n_CM_sev0_DISEASE_STAT2,
                                                                      Cohort2_modelX$n_CM_sev0_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sev1',
                                                                  Cohort2_modelX$n_CM_sev1_DISEASE_STAT0,
                                                                  Cohort2_modelX$n_CM_sev1_DISEASE_STAT1,
                                                                  Cohort2_modelX$n_CM_sev1_DISEASE_STAT2,
                                                                  Cohort2_modelX$n_CM_sev1_DISEASE_STAT3,
                                                                  sum(Cohort2_modelX$n_CM_sev1_DISEASE_STAT0,
                                                                      Cohort2_modelX$n_CM_sev1_DISEASE_STAT1,
                                                                      Cohort2_modelX$n_CM_sev1_DISEASE_STAT2,
                                                                      Cohort2_modelX$n_CM_sev1_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sev2',
                                                                  Cohort2_modelX$n_CM_sev2_DISEASE_STAT0,
                                                                  Cohort2_modelX$n_CM_sev2_DISEASE_STAT1,
                                                                  Cohort2_modelX$n_CM_sev2_DISEASE_STAT2,
                                                                  Cohort2_modelX$n_CM_sev2_DISEASE_STAT3,
                                                                  sum(Cohort2_modelX$n_CM_sev2_DISEASE_STAT0,
                                                                      Cohort2_modelX$n_CM_sev2_DISEASE_STAT1,
                                                                      Cohort2_modelX$n_CM_sev2_DISEASE_STAT2,
                                                                      Cohort2_modelX$n_CM_sev2_DISEASE_STAT3)),
                                                         nrow = 3,
                                                         ncol = 7,
                                                         byrow = T)), crosstab_Cohort2_modelX <- as.data.frame(matrix(NA, nrow = 3, ncol = 7, byrow = T))) 
  ifelse(is.data.frame(Cohort3_modelX) == T, 
         crosstab_Cohort3_modelX <- as.data.frame(matrix(data = c('Cohort3_modelX',
                                                                  'No CM',
                                                                  Cohort3_modelX$n_CM_sev0_DISEASE_STAT0,
                                                                  Cohort3_modelX$n_CM_sev0_DISEASE_STAT1,
                                                                  Cohort3_modelX$n_CM_sev0_DISEASE_STAT2,
                                                                  Cohort3_modelX$n_CM_sev0_DISEASE_STAT3,
                                                                  sum(Cohort3_modelX$n_CM_sev0_DISEASE_STAT0,
                                                                      Cohort3_modelX$n_CM_sev0_DISEASE_STAT1,
                                                                      Cohort3_modelX$n_CM_sev0_DISEASE_STAT2,
                                                                      Cohort3_modelX$n_CM_sev0_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sev1',
                                                                  Cohort3_modelX$n_CM_sev1_DISEASE_STAT0,
                                                                  Cohort3_modelX$n_CM_sev1_DISEASE_STAT1,
                                                                  Cohort3_modelX$n_CM_sev1_DISEASE_STAT2,
                                                                  Cohort3_modelX$n_CM_sev1_DISEASE_STAT3,
                                                                  sum(Cohort3_modelX$n_CM_sev1_DISEASE_STAT0,
                                                                      Cohort3_modelX$n_CM_sev1_DISEASE_STAT1,
                                                                      Cohort3_modelX$n_CM_sev1_DISEASE_STAT2,
                                                                      Cohort3_modelX$n_CM_sev1_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sev2',
                                                                  Cohort3_modelX$n_CM_sev2_DISEASE_STAT0,
                                                                  Cohort3_modelX$n_CM_sev2_DISEASE_STAT1,
                                                                  Cohort3_modelX$n_CM_sev2_DISEASE_STAT2,
                                                                  Cohort3_modelX$n_CM_sev2_DISEASE_STAT3,
                                                                  sum(Cohort3_modelX$n_CM_sev2_DISEASE_STAT0,
                                                                      Cohort3_modelX$n_CM_sev2_DISEASE_STAT1,
                                                                      Cohort3_modelX$n_CM_sev2_DISEASE_STAT2,
                                                                      Cohort3_modelX$n_CM_sev2_DISEASE_STAT3)),
                                                         nrow = 3,
                                                         ncol = 7,
                                                         byrow = T)), crosstab_Cohort3_modelX <- as.data.frame(matrix(NA, nrow = 3, ncol = 7, byrow = T))) 
  ifelse(is.data.frame(Cohort4_modelX) == T, 
         crosstab_Cohort4_modelX <- as.data.frame(matrix(data = c('Cohort4_modelX',
                                                                  'No CM',
                                                                  Cohort4_modelX$n_CM_sev0_DISEASE_STAT0,
                                                                  Cohort4_modelX$n_CM_sev0_DISEASE_STAT1,
                                                                  Cohort4_modelX$n_CM_sev0_DISEASE_STAT2,
                                                                  Cohort4_modelX$n_CM_sev0_DISEASE_STAT3,
                                                                  sum(Cohort4_modelX$n_CM_sev0_DISEASE_STAT0,
                                                                      Cohort4_modelX$n_CM_sev0_DISEASE_STAT1,
                                                                      Cohort4_modelX$n_CM_sev0_DISEASE_STAT2,
                                                                      Cohort4_modelX$n_CM_sev0_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sev1',
                                                                  Cohort4_modelX$n_CM_sev1_DISEASE_STAT0,
                                                                  Cohort4_modelX$n_CM_sev1_DISEASE_STAT1,
                                                                  Cohort4_modelX$n_CM_sev1_DISEASE_STAT2,
                                                                  Cohort4_modelX$n_CM_sev1_DISEASE_STAT3,
                                                                  sum(Cohort4_modelX$n_CM_sev1_DISEASE_STAT0,
                                                                      Cohort4_modelX$n_CM_sev1_DISEASE_STAT1,
                                                                      Cohort4_modelX$n_CM_sev1_DISEASE_STAT2,
                                                                      Cohort4_modelX$n_CM_sev1_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sev2',
                                                                  Cohort4_modelX$n_CM_sev2_DISEASE_STAT0,
                                                                  Cohort4_modelX$n_CM_sev2_DISEASE_STAT1,
                                                                  Cohort4_modelX$n_CM_sev2_DISEASE_STAT2,
                                                                  Cohort4_modelX$n_CM_sev2_DISEASE_STAT3,
                                                                  sum(Cohort4_modelX$n_CM_sev2_DISEASE_STAT0,
                                                                      Cohort4_modelX$n_CM_sev2_DISEASE_STAT1,
                                                                      Cohort4_modelX$n_CM_sev2_DISEASE_STAT2,
                                                                      Cohort4_modelX$n_CM_sev2_DISEASE_STAT3)),
                                                         nrow = 3,
                                                         ncol = 7,
                                                         byrow = T)), crosstab_Cohort4_modelX <- as.data.frame(matrix(NA, nrow = 3, ncol = 7, byrow = T))) 
  ifelse(is.data.frame(Cohort5_modelX) == T, 
         crosstab_Cohort5_modelX <- as.data.frame(matrix(data = c('Cohort5_modelX',
                                                                  'No CM',
                                                                  Cohort5_modelX$n_CM_sev0_DISEASE_STAT0,
                                                                  Cohort5_modelX$n_CM_sev0_DISEASE_STAT1,
                                                                  Cohort5_modelX$n_CM_sev0_DISEASE_STAT2,
                                                                  Cohort5_modelX$n_CM_sev0_DISEASE_STAT3,
                                                                  sum(Cohort5_modelX$n_CM_sev0_DISEASE_STAT0,
                                                                      Cohort5_modelX$n_CM_sev0_DISEASE_STAT1,
                                                                      Cohort5_modelX$n_CM_sev0_DISEASE_STAT2,
                                                                      Cohort5_modelX$n_CM_sev0_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sev1',
                                                                  Cohort5_modelX$n_CM_sev1_DISEASE_STAT0,
                                                                  Cohort5_modelX$n_CM_sev1_DISEASE_STAT1,
                                                                  Cohort5_modelX$n_CM_sev1_DISEASE_STAT2,
                                                                  Cohort5_modelX$n_CM_sev1_DISEASE_STAT3,
                                                                  sum(Cohort5_modelX$n_CM_sev1_DISEASE_STAT0,
                                                                      Cohort5_modelX$n_CM_sev1_DISEASE_STAT1,
                                                                      Cohort5_modelX$n_CM_sev1_DISEASE_STAT2,
                                                                      Cohort5_modelX$n_CM_sev1_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sev2',
                                                                  Cohort5_modelX$n_CM_sev2_DISEASE_STAT0,
                                                                  Cohort5_modelX$n_CM_sev2_DISEASE_STAT1,
                                                                  Cohort5_modelX$n_CM_sev2_DISEASE_STAT2,
                                                                  Cohort5_modelX$n_CM_sev2_DISEASE_STAT3,
                                                                  sum(Cohort5_modelX$n_CM_sev2_DISEASE_STAT0,
                                                                      Cohort5_modelX$n_CM_sev2_DISEASE_STAT1,
                                                                      Cohort5_modelX$n_CM_sev2_DISEASE_STAT2,
                                                                      Cohort5_modelX$n_CM_sev2_DISEASE_STAT3)),
                                                         nrow = 3,
                                                         ncol = 7,
                                                         byrow = T)), crosstab_Cohort5_modelX <- as.data.frame(matrix(NA, nrow = 3, ncol = 7, byrow = T))) 
  ifelse(is.data.frame(Cohort6_modelX) == T, 
         crosstab_Cohort6_modelX <- as.data.frame(matrix(data = c('Cohort6_modelX',
                                                                  'No CM',
                                                                  Cohort6_modelX$n_CM_sev0_DISEASE_STAT0,
                                                                  Cohort6_modelX$n_CM_sev0_DISEASE_STAT1,
                                                                  Cohort6_modelX$n_CM_sev0_DISEASE_STAT2,
                                                                  Cohort6_modelX$n_CM_sev0_DISEASE_STAT3,
                                                                  sum(Cohort6_modelX$n_CM_sev0_DISEASE_STAT0,
                                                                      Cohort6_modelX$n_CM_sev0_DISEASE_STAT1,
                                                                      Cohort6_modelX$n_CM_sev0_DISEASE_STAT2,
                                                                      Cohort6_modelX$n_CM_sev0_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sev1',
                                                                  Cohort6_modelX$n_CM_sev1_DISEASE_STAT0,
                                                                  Cohort6_modelX$n_CM_sev1_DISEASE_STAT1,
                                                                  Cohort6_modelX$n_CM_sev1_DISEASE_STAT2,
                                                                  Cohort6_modelX$n_CM_sev1_DISEASE_STAT3,
                                                                  sum(Cohort6_modelX$n_CM_sev1_DISEASE_STAT0,
                                                                      Cohort6_modelX$n_CM_sev1_DISEASE_STAT1,
                                                                      Cohort6_modelX$n_CM_sev1_DISEASE_STAT2,
                                                                      Cohort6_modelX$n_CM_sev1_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sev2',
                                                                  Cohort6_modelX$n_CM_sev2_DISEASE_STAT0,
                                                                  Cohort6_modelX$n_CM_sev2_DISEASE_STAT1,
                                                                  Cohort6_modelX$n_CM_sev2_DISEASE_STAT2,
                                                                  Cohort6_modelX$n_CM_sev2_DISEASE_STAT3,
                                                                  sum(Cohort6_modelX$n_CM_sev2_DISEASE_STAT0,
                                                                      Cohort6_modelX$n_CM_sev2_DISEASE_STAT1,
                                                                      Cohort6_modelX$n_CM_sev2_DISEASE_STAT2,
                                                                      Cohort6_modelX$n_CM_sev2_DISEASE_STAT3)),
                                                         nrow = 3,
                                                         ncol = 7,
                                                         byrow = T)), crosstab_Cohort6_modelX <- as.data.frame(matrix(NA, nrow = 3, ncol = 7, byrow = T))) 
  ifelse(is.data.frame(Cohort7_modelX) == T, 
         crosstab_Cohort7_modelX <- as.data.frame(matrix(data = c('Cohort7_modelX',
                                                                  'No CM',
                                                                  Cohort7_modelX$n_CM_sev0_DISEASE_STAT0,
                                                                  Cohort7_modelX$n_CM_sev0_DISEASE_STAT1,
                                                                  Cohort7_modelX$n_CM_sev0_DISEASE_STAT2,
                                                                  Cohort7_modelX$n_CM_sev0_DISEASE_STAT3,
                                                                  sum(Cohort7_modelX$n_CM_sev0_DISEASE_STAT0,
                                                                      Cohort7_modelX$n_CM_sev0_DISEASE_STAT1,
                                                                      Cohort7_modelX$n_CM_sev0_DISEASE_STAT2,
                                                                      Cohort7_modelX$n_CM_sev0_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sev1',
                                                                  Cohort7_modelX$n_CM_sev1_DISEASE_STAT0,
                                                                  Cohort7_modelX$n_CM_sev1_DISEASE_STAT1,
                                                                  Cohort7_modelX$n_CM_sev1_DISEASE_STAT2,
                                                                  Cohort7_modelX$n_CM_sev1_DISEASE_STAT3,
                                                                  sum(Cohort7_modelX$n_CM_sev1_DISEASE_STAT0,
                                                                      Cohort7_modelX$n_CM_sev1_DISEASE_STAT1,
                                                                      Cohort7_modelX$n_CM_sev1_DISEASE_STAT2,
                                                                      Cohort7_modelX$n_CM_sev1_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sev2',
                                                                  Cohort7_modelX$n_CM_sev2_DISEASE_STAT0,
                                                                  Cohort7_modelX$n_CM_sev2_DISEASE_STAT1,
                                                                  Cohort7_modelX$n_CM_sev2_DISEASE_STAT2,
                                                                  Cohort7_modelX$n_CM_sev2_DISEASE_STAT3,
                                                                  sum(Cohort7_modelX$n_CM_sev2_DISEASE_STAT0,
                                                                      Cohort7_modelX$n_CM_sev2_DISEASE_STAT1,
                                                                      Cohort7_modelX$n_CM_sev2_DISEASE_STAT2,
                                                                      Cohort7_modelX$n_CM_sev2_DISEASE_STAT3)),
                                                         nrow = 3,
                                                         ncol = 7,
                                                         byrow = T)), crosstab_Cohort7_modelX <- as.data.frame(matrix(NA, nrow = 3, ncol = 7, byrow = T))) 
  ifelse(is.data.frame(Cohort8_modelX) == T, 
         crosstab_Cohort8_modelX <- as.data.frame(matrix(data = c('Cohort8_modelX',
                                                                  'No CM',
                                                                  Cohort8_modelX$n_CM_sev0_DISEASE_STAT0,
                                                                  Cohort8_modelX$n_CM_sev0_DISEASE_STAT1,
                                                                  Cohort8_modelX$n_CM_sev0_DISEASE_STAT2,
                                                                  Cohort8_modelX$n_CM_sev0_DISEASE_STAT3,
                                                                  sum(Cohort8_modelX$n_CM_sev0_DISEASE_STAT0,
                                                                      Cohort8_modelX$n_CM_sev0_DISEASE_STAT1,
                                                                      Cohort8_modelX$n_CM_sev0_DISEASE_STAT2,
                                                                      Cohort8_modelX$n_CM_sev0_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sev1',
                                                                  Cohort8_modelX$n_CM_sev1_DISEASE_STAT0,
                                                                  Cohort8_modelX$n_CM_sev1_DISEASE_STAT1,
                                                                  Cohort8_modelX$n_CM_sev1_DISEASE_STAT2,
                                                                  Cohort8_modelX$n_CM_sev1_DISEASE_STAT3,
                                                                  sum(Cohort8_modelX$n_CM_sev1_DISEASE_STAT0,
                                                                      Cohort8_modelX$n_CM_sev1_DISEASE_STAT1,
                                                                      Cohort8_modelX$n_CM_sev1_DISEASE_STAT2,
                                                                      Cohort8_modelX$n_CM_sev1_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sev2',
                                                                  Cohort8_modelX$n_CM_sev2_DISEASE_STAT0,
                                                                  Cohort8_modelX$n_CM_sev2_DISEASE_STAT1,
                                                                  Cohort8_modelX$n_CM_sev2_DISEASE_STAT2,
                                                                  Cohort8_modelX$n_CM_sev2_DISEASE_STAT3,
                                                                  sum(Cohort8_modelX$n_CM_sev2_DISEASE_STAT0,
                                                                      Cohort8_modelX$n_CM_sev2_DISEASE_STAT1,
                                                                      Cohort8_modelX$n_CM_sev2_DISEASE_STAT2,
                                                                      Cohort8_modelX$n_CM_sev2_DISEASE_STAT3)),
                                                         nrow = 3,
                                                         ncol = 7,
                                                         byrow = T)), crosstab_Cohort8_modelX <- as.data.frame(matrix(NA, nrow = 3, ncol = 7, byrow = T))) 
  ifelse(is.data.frame(Cohort9_modelX) == T, 
         crosstab_Cohort9_modelX <- as.data.frame(matrix(data = c('Cohort9_modelX',
                                                                  'No CM',
                                                                  Cohort9_modelX$n_CM_sev0_DISEASE_STAT0,
                                                                  Cohort9_modelX$n_CM_sev0_DISEASE_STAT1,
                                                                  Cohort9_modelX$n_CM_sev0_DISEASE_STAT2,
                                                                  Cohort9_modelX$n_CM_sev0_DISEASE_STAT3,
                                                                  sum(Cohort9_modelX$n_CM_sev0_DISEASE_STAT0,
                                                                      Cohort9_modelX$n_CM_sev0_DISEASE_STAT1,
                                                                      Cohort9_modelX$n_CM_sev0_DISEASE_STAT2,
                                                                      Cohort9_modelX$n_CM_sev0_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sev1',
                                                                  Cohort9_modelX$n_CM_sev1_DISEASE_STAT0,
                                                                  Cohort9_modelX$n_CM_sev1_DISEASE_STAT1,
                                                                  Cohort9_modelX$n_CM_sev1_DISEASE_STAT2,
                                                                  Cohort9_modelX$n_CM_sev1_DISEASE_STAT3,
                                                                  sum(Cohort9_modelX$n_CM_sev1_DISEASE_STAT0,
                                                                      Cohort9_modelX$n_CM_sev1_DISEASE_STAT1,
                                                                      Cohort9_modelX$n_CM_sev1_DISEASE_STAT2,
                                                                      Cohort9_modelX$n_CM_sev1_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sev2',
                                                                  Cohort9_modelX$n_CM_sev2_DISEASE_STAT0,
                                                                  Cohort9_modelX$n_CM_sev2_DISEASE_STAT1,
                                                                  Cohort9_modelX$n_CM_sev2_DISEASE_STAT2,
                                                                  Cohort9_modelX$n_CM_sev2_DISEASE_STAT3,
                                                                  sum(Cohort9_modelX$n_CM_sev2_DISEASE_STAT0,
                                                                      Cohort9_modelX$n_CM_sev2_DISEASE_STAT1,
                                                                      Cohort9_modelX$n_CM_sev2_DISEASE_STAT2,
                                                                      Cohort9_modelX$n_CM_sev2_DISEASE_STAT3)),
                                                         nrow = 3,
                                                         ncol = 7,
                                                         byrow = T)), crosstab_Cohort9_modelX <- as.data.frame(matrix(NA, nrow = 3, ncol = 7, byrow = T))) 
  ifelse(is.data.frame(Cohort10_modelX) == T, 
         crosstab_Cohort10_modelX <- as.data.frame(matrix(data = c('Cohort10_modelX',
                                                                  'No CM',
                                                                  Cohort10_modelX$n_CM_sev0_DISEASE_STAT0,
                                                                  Cohort10_modelX$n_CM_sev0_DISEASE_STAT1,
                                                                  Cohort10_modelX$n_CM_sev0_DISEASE_STAT2,
                                                                  Cohort10_modelX$n_CM_sev0_DISEASE_STAT3,
                                                                  sum(Cohort10_modelX$n_CM_sev0_DISEASE_STAT0,
                                                                      Cohort10_modelX$n_CM_sev0_DISEASE_STAT1,
                                                                      Cohort10_modelX$n_CM_sev0_DISEASE_STAT2,
                                                                      Cohort10_modelX$n_CM_sev0_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sev1',
                                                                  Cohort10_modelX$n_CM_sev1_DISEASE_STAT0,
                                                                  Cohort10_modelX$n_CM_sev1_DISEASE_STAT1,
                                                                  Cohort10_modelX$n_CM_sev1_DISEASE_STAT2,
                                                                  Cohort10_modelX$n_CM_sev1_DISEASE_STAT3,
                                                                  sum(Cohort10_modelX$n_CM_sev1_DISEASE_STAT0,
                                                                      Cohort10_modelX$n_CM_sev1_DISEASE_STAT1,
                                                                      Cohort10_modelX$n_CM_sev1_DISEASE_STAT2,
                                                                      Cohort10_modelX$n_CM_sev1_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sev2',
                                                                  Cohort10_modelX$n_CM_sev2_DISEASE_STAT0,
                                                                  Cohort10_modelX$n_CM_sev2_DISEASE_STAT1,
                                                                  Cohort10_modelX$n_CM_sev2_DISEASE_STAT2,
                                                                  Cohort10_modelX$n_CM_sev2_DISEASE_STAT3,
                                                                  sum(Cohort10_modelX$n_CM_sev2_DISEASE_STAT0,
                                                                      Cohort10_modelX$n_CM_sev2_DISEASE_STAT1,
                                                                      Cohort10_modelX$n_CM_sev2_DISEASE_STAT2,
                                                                      Cohort10_modelX$n_CM_sev2_DISEASE_STAT3)),
                                                         nrow = 3,
                                                         ncol = 7,
                                                         byrow = T)), crosstab_Cohort10_modelX <- as.data.frame(matrix(NA, nrow = 3, ncol = 7, byrow = T))) 
  ifelse(is.data.frame(Cohort11_modelX) == T, 
         crosstab_Cohort11_modelX <- as.data.frame(matrix(data = c('Cohort11_modelX',
                                                                  'No CM',
                                                                  Cohort11_modelX$n_CM_sev0_DISEASE_STAT0,
                                                                  Cohort11_modelX$n_CM_sev0_DISEASE_STAT1,
                                                                  Cohort11_modelX$n_CM_sev0_DISEASE_STAT2,
                                                                  Cohort11_modelX$n_CM_sev0_DISEASE_STAT3,
                                                                  sum(Cohort11_modelX$n_CM_sev0_DISEASE_STAT0,
                                                                      Cohort11_modelX$n_CM_sev0_DISEASE_STAT1,
                                                                      Cohort11_modelX$n_CM_sev0_DISEASE_STAT2,
                                                                      Cohort11_modelX$n_CM_sev0_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sev1',
                                                                  Cohort11_modelX$n_CM_sev1_DISEASE_STAT0,
                                                                  Cohort11_modelX$n_CM_sev1_DISEASE_STAT1,
                                                                  Cohort11_modelX$n_CM_sev1_DISEASE_STAT2,
                                                                  Cohort11_modelX$n_CM_sev1_DISEASE_STAT3,
                                                                  sum(Cohort11_modelX$n_CM_sev1_DISEASE_STAT0,
                                                                      Cohort11_modelX$n_CM_sev1_DISEASE_STAT1,
                                                                      Cohort11_modelX$n_CM_sev1_DISEASE_STAT2,
                                                                      Cohort11_modelX$n_CM_sev1_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sev2',
                                                                  Cohort11_modelX$n_CM_sev2_DISEASE_STAT0,
                                                                  Cohort11_modelX$n_CM_sev2_DISEASE_STAT1,
                                                                  Cohort11_modelX$n_CM_sev2_DISEASE_STAT2,
                                                                  Cohort11_modelX$n_CM_sev2_DISEASE_STAT3,
                                                                  sum(Cohort11_modelX$n_CM_sev2_DISEASE_STAT0,
                                                                      Cohort11_modelX$n_CM_sev2_DISEASE_STAT1,
                                                                      Cohort11_modelX$n_CM_sev2_DISEASE_STAT2,
                                                                      Cohort11_modelX$n_CM_sev2_DISEASE_STAT3)),
                                                         nrow = 3,
                                                         ncol = 7,
                                                         byrow = T)), crosstab_Cohort11_modelX <- as.data.frame(matrix(NA, nrow = 3, ncol = 7, byrow = T))) 
  ifelse(is.data.frame(Cohort12_modelX) == T, 
         crosstab_Cohort12_modelX <- as.data.frame(matrix(data = c('Cohort12_modelX',
                                                                  'No CM',
                                                                  Cohort12_modelX$n_CM_sev0_DISEASE_STAT0,
                                                                  Cohort12_modelX$n_CM_sev0_DISEASE_STAT1,
                                                                  Cohort12_modelX$n_CM_sev0_DISEASE_STAT2,
                                                                  Cohort12_modelX$n_CM_sev0_DISEASE_STAT3,
                                                                  sum(Cohort12_modelX$n_CM_sev0_DISEASE_STAT0,
                                                                      Cohort12_modelX$n_CM_sev0_DISEASE_STAT1,
                                                                      Cohort12_modelX$n_CM_sev0_DISEASE_STAT2,
                                                                      Cohort12_modelX$n_CM_sev0_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sev1',
                                                                  Cohort12_modelX$n_CM_sev1_DISEASE_STAT0,
                                                                  Cohort12_modelX$n_CM_sev1_DISEASE_STAT1,
                                                                  Cohort12_modelX$n_CM_sev1_DISEASE_STAT2,
                                                                  Cohort12_modelX$n_CM_sev1_DISEASE_STAT3,
                                                                  sum(Cohort12_modelX$n_CM_sev1_DISEASE_STAT0,
                                                                      Cohort12_modelX$n_CM_sev1_DISEASE_STAT1,
                                                                      Cohort12_modelX$n_CM_sev1_DISEASE_STAT2,
                                                                      Cohort12_modelX$n_CM_sev1_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sev2',
                                                                  Cohort12_modelX$n_CM_sev2_DISEASE_STAT0,
                                                                  Cohort12_modelX$n_CM_sev2_DISEASE_STAT1,
                                                                  Cohort12_modelX$n_CM_sev2_DISEASE_STAT2,
                                                                  Cohort12_modelX$n_CM_sev2_DISEASE_STAT3,
                                                                  sum(Cohort12_modelX$n_CM_sev2_DISEASE_STAT0,
                                                                      Cohort12_modelX$n_CM_sev2_DISEASE_STAT1,
                                                                      Cohort12_modelX$n_CM_sev2_DISEASE_STAT2,
                                                                      Cohort12_modelX$n_CM_sev2_DISEASE_STAT3)),
                                                         nrow = 3,
                                                         ncol = 7,
                                                         byrow = T)), crosstab_Cohort12_modelX <- as.data.frame(matrix(NA, nrow = 3, ncol = 7, byrow = T))) 
  ifelse(is.data.frame(Cohort13_modelX) == T, 
         crosstab_Cohort13_modelX <- as.data.frame(matrix(data = c('Cohort13_modelX',
                                                                  'No CM',
                                                                  Cohort13_modelX$n_CM_sev0_DISEASE_STAT0,
                                                                  Cohort13_modelX$n_CM_sev0_DISEASE_STAT1,
                                                                  Cohort13_modelX$n_CM_sev0_DISEASE_STAT2,
                                                                  Cohort13_modelX$n_CM_sev0_DISEASE_STAT3,
                                                                  sum(Cohort13_modelX$n_CM_sev0_DISEASE_STAT0,
                                                                      Cohort13_modelX$n_CM_sev0_DISEASE_STAT1,
                                                                      Cohort13_modelX$n_CM_sev0_DISEASE_STAT2,
                                                                      Cohort13_modelX$n_CM_sev0_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sev1',
                                                                  Cohort13_modelX$n_CM_sev1_DISEASE_STAT0,
                                                                  Cohort13_modelX$n_CM_sev1_DISEASE_STAT1,
                                                                  Cohort13_modelX$n_CM_sev1_DISEASE_STAT2,
                                                                  Cohort13_modelX$n_CM_sev1_DISEASE_STAT3,
                                                                  sum(Cohort13_modelX$n_CM_sev1_DISEASE_STAT0,
                                                                      Cohort13_modelX$n_CM_sev1_DISEASE_STAT1,
                                                                      Cohort13_modelX$n_CM_sev1_DISEASE_STAT2,
                                                                      Cohort13_modelX$n_CM_sev1_DISEASE_STAT3),
                                                                  '',
                                                                  'CM_sev2',
                                                                  Cohort13_modelX$n_CM_sev2_DISEASE_STAT0,
                                                                  Cohort13_modelX$n_CM_sev2_DISEASE_STAT1,
                                                                  Cohort13_modelX$n_CM_sev2_DISEASE_STAT2,
                                                                  Cohort13_modelX$n_CM_sev2_DISEASE_STAT3,
                                                                  sum(Cohort13_modelX$n_CM_sev2_DISEASE_STAT0,
                                                                      Cohort13_modelX$n_CM_sev2_DISEASE_STAT1,
                                                                      Cohort13_modelX$n_CM_sev2_DISEASE_STAT2,
                                                                      Cohort13_modelX$n_CM_sev2_DISEASE_STAT3)),
                                                         nrow = 3,
                                                         ncol = 7,
                                                         byrow = T)), crosstab_Cohort13_modelX <- as.data.frame(matrix(NA, nrow = 3, ncol = 7, byrow = T))) 
  crosstab_model <- rbind(crosstab_Cohort1_modelX,
                          crosstab_Cohort2_modelX,
                          crosstab_Cohort3_modelX,
                          crosstab_Cohort4_modelX,
                          crosstab_Cohort5_modelX,
                          crosstab_Cohort6_modelX,
                          crosstab_Cohort7_modelX,
                          crosstab_Cohort8_modelX,
                          crosstab_Cohort9_modelX,
                          crosstab_Cohort10_modelX,
                          crosstab_Cohort11_modelX,
                          crosstab_Cohort12_modelX,
                          crosstab_Cohort13_modelX, make.row.names = F)
  colnames(crosstab_model) <- c('','','Healthy','Dep only', 'Cmd disease only','Comorbidity', 'N per exposure','Total N')
  
  return(crosstab_model)
}

#################### 6. Carry out random-effects meta-analysis models ####################
# Model 1: MDD ~ CM + Age + Sex + Educ
# Check for each cohort whether they have 5 or more cases in each exposure x outcome crosstab cell
crosstablogistic_function(Cohort1_model1,
                          Cohort2_model1,
                          Cohort3_model1,
                          Cohort4_model1,
                          Cohort5_model1,
                          Cohort6_model1,
                          Cohort7_model1,
                          Cohort8_model1,
                          Cohort9_model1,
                          Cohort10_model1,
                          Cohort11_model1,
                          Cohort12_model1,
                          Cohort13_model1)
# Run model, compute odds ratios and calculate weights of each cohort in meta-analysis
RE_model1 <- rma(yi = 'log(OR)', sei = SE, data = Model1)
summary(RE_model1)
OR_model1 <- predict(RE_model1, transf=exp, digits=3)
weights_model1 <- paste0(formatC(weights(RE_model1), format="f", digits=1, width=4), "%")

# Model 2: CMD ~ CM + Age + Sex + Educ
# Check for each cohort whether they have 5 or more cases in each exposure x outcome crosstab cell
crosstablogistic_function(Cohort1_model2,
                          Cohort2_model2,
                          Cohort3_model2,
                          Cohort4_model2,
                          Cohort5_model2,
                          Cohort6_model2,
                          Cohort7_model2,
                          Cohort8_model2,
                          Cohort9_model2,
                          Cohort10_model2,
                          Cohort11_model2,
                          Cohort12_model2,
                          Cohort13_model2)

# Run model, compute odds ratios and calculate weights of each cohort in meta-analysis
RE_model2 <- rma(yi = 'log(OR)', sei = SE, data = Model2)
summary(RE_model2)
OR_model2 <- predict(RE_model2, transf=exp, digits=3)
weights_model2 <- paste0(formatC(weights(RE_model2), format="f", digits=1, width=4), "%")

# Model 2a: DM ~ CM + Age + Sex + Educ
# Check for each cohort whether they have 5 or more cases in each exposure x outcome crosstab cell
crosstablogistic_function(Cohort1_model2a,
                          Cohort2_model2a,
                          Cohort3_model2a,
                          Cohort4_model2a,
                          Cohort5_model2a,
                          Cohort6_model2a,
                          Cohort7_model2a,
                          Cohort8_model2a,
                          Cohort9_model2a,
                          Cohort10_model2a,
                          Cohort11_model2a,
                          Cohort12_model2a,
                          Cohort13_model2a) #One cohort (GenR mothers) has to be excluded because crosstab cell < 5

# Run model, compute odds ratios and calculate weights of each cohort in meta-analysis
RE_model2a <- rma(yi = 'log(OR)', sei = SE, data = Model2a[-which(rownames(Model2a)%in%"GenR"),])
summary(RE_model2a)
OR_model2a <- predict(RE_model2a, transf=exp, digits=3)
weights_model2a <- paste0(formatC(weights(RE_model2a), format="f", digits=1, width=4), "%")

# Model 2b: CVD1 ~ CM + Age + Sex + Educ
# Check for each cohort whether they have 5 or more cases in each exposure x outcome crosstab cell
crosstablogistic_function(Cohort1_model2b,
                          Cohort2_model2b,
                          Cohort3_model2b,
                          Cohort4_model2b,
                          Cohort5_model2b,
                          Cohort6_model2b,
                          Cohort7_model2b,
                          Cohort8_model2b,
                          Cohort9_model2b,
                          Cohort10_model2b,
                          Cohort11_model2b,
                          Cohort12_model2b,
                          Cohort13_model2b) #Two cohorts (ALSPAC partners and MACS) have to be excluded because crosstab cell < 5

# Run model, compute odds ratios and calculate weights of each cohort in meta-analysis
RE_model2b <- rma(yi = 'log(OR)', sei = SE, data = Model2b[-which(rownames(Model2b)%in%c("ALSPACpartners","MACS")),])
summary(RE_model2b)
OR_model2b <- predict(RE_model2b, transf=exp, digits=3)
weights_model2b <- paste0(formatC(weights(RE_model2b), format="f", digits=1, width=4), "%")

# Model 3: DISEASE_STAT ~ CM + Age + Sex + Educ
# Check for each cohort whether they have 5 or more cases in each exposure x outcome crosstab cell
crosstabmultinomial_function(Cohort1_model3,
                             Cohort2_model3,
                             Cohort3_model3,
                             Cohort4_model3,
                             Cohort5_model3,
                             Cohort6_model3,
                             Cohort7_model3,
                             Cohort8_model3,
                             Cohort9_model3,
                             Cohort10_model3,
                             Cohort11_model3,
                             Cohort12_model3,
                             Cohort13_model3) #Four cohorts (ALSPAC partners, GenR mothers, MACS, and NESDO) have to be excluded because crosstab cell < 5

# Run model, compute odds ratios and calculate weights of each cohort in meta-analysis
# Outcome level = depression
RE_model3_dep <- rma(yi = 'log(OR)', sei = SE, data = Model3_dep[-which(rownames(Model3_dep)%in%c("ALSPACpartners","GenR","MACS","NESDO")),])
summary(RE_model3_dep)
OR_model3_dep <- predict(RE_model3_dep, transf=exp, digits=3)
weights_model3_dep <- paste0(formatC(weights(RE_model3_dep), format="f", digits=1, width=4), "%")

# Outcome level = cardiometabolic disease
RE_model3_cmd <- rma(yi = 'log(OR)', sei = SE, data = Model3_cmd[-which(rownames(Model3_cmd)%in%c("ALSPACpartners","GenR","MACS","NESDO")),])
summary(RE_model3_cmd)
OR_model3_cmd <- predict(RE_model3_cmd, transf=exp, digits=3)
weights_model3_cmd <- paste0(formatC(weights(RE_model3_cmd), format="f", digits=1, width=4), "%")

# Outcome level = comorbidity
RE_model3_comorb <- rma(yi = 'log(OR)', sei = SE, data = Model3_comorb[-which(rownames(Model3_comorb)%in%c("ALSPACpartners","GenR","MACS","NESDO")),])
summary(RE_model3_comorb)
OR_model3_comorb <- predict(RE_model3_comorb, transf=exp, digits=3)
weights_model3_comorb <- paste0(formatC(weights(RE_model3_comorb), format="f", digits=1, width=4), "%")

# Subgroup analysis - Fit random-effects model in the two subgroups (case-control vs. population-based)
RE_model3_dep_cas <- rma(yi = 'log(OR)', sei = SE, subset=(Cohort_type=="Case-control"), data=Model4_dep[-which(rownames(Model4_dep)==c("ALSPACpartners","GenR","MACS","NESDO")),])
RE_model3_cmd_cas <- rma(yi = 'log(OR)', sei = SE, subset=(Cohort_type=="Case-control"), data=Model4_cmd[-which(rownames(Model4_cmd)==c("ALSPACpartners","GenR","MACS","NESDO")),])
RE_model3_comorb_cas <- rma(yi = 'log(OR)', sei = SE, subset=(Cohort_type=="Case-control"), data=Model4_comorb[-which(rownames(Model4_comorb)==c("ALSPACpartners","GenR","MACS","NESDO")),])
RE_model3_dep_pop <- rma(yi = 'log(OR)', sei = SE, subset=(Cohort_type=="Population-based"), data=Model4_dep[-which(rownames(Model4_dep)==c("ALSPACpartners","GenR","MACS","NESDO")),])
RE_model3_cmd_pop <- rma(yi = 'log(OR)', sei = SE, subset=(Cohort_type=="Population-based"), data=Model4_cmd[-which(rownames(Model4_cmd)==c("ALSPACpartners","GenR","MACS","NESDO")),])
RE_model3_comorb_pop <- rma(yi = 'log(OR)', sei = SE, subset=(Cohort_type=="Population-based"), data=Model4_comorb[-which(rownames(Model4_comorb)==c("ALSPACpartners","GenR","MACS","NESDO")),])

# Model 4: DISEASE_STAT ~ CM + Age + Sex + Educ + Smoke + Alcohol + PhyAct
# Check for each cohort whether they have 5 or more cases in each exposure x outcome crosstab cell
crosstabmultinomial_function(Cohort1_model4,
                             Cohort2_model4,
                             Cohort3_model4,
                             Cohort4_model4,
                             Cohort5_model4,
                             Cohort6_model4,
                             Cohort7_model4,
                             Cohort8_model4,
                             Cohort9_model4,
                             Cohort10_model4,
                             Cohort11_model4,
                             Cohort12_model4,
                             Cohort13_model4) #Four cohorts (ALSPAC partners, GenR mothers, MACS, and NESDO) have to be excluded because crosstab cell < 5

# Run model, compute odds ratios and calculate weights of each cohort in meta-analysis
# Outcome level = depression
RE_model4_dep <- rma(yi = 'log(OR)', sei = SE, data = Model4_dep[-which(rownames(Model4_dep)%in%c("ALSPACpartners","GenR","MACS","NESDO")),])
summary(RE_model4_dep)
OR_model4_dep <- predict(RE_model4_dep, transf=exp, digits=3)
weights_model4_dep <- paste0(formatC(weights(RE_model4_dep), format="f", digits=1, width=4), "%")

# Outcome level = cardiometabolic disease
RE_model4_cmd <- rma(yi = 'log(OR)', sei = SE, data = Model4_cmd[-which(rownames(Model4_cmd)%in%c("ALSPACpartners","GenR","MACS","NESDO")),])
summary(RE_model4_cmd)
OR_model4_cmd <- predict(RE_model4_cmd, transf=exp, digits=3)
weights_model4_cmd <- paste0(formatC(weights(RE_model4_cmd), format="f", digits=1, width=4), "%")

# Outcome level = comorbidity
RE_model4_comorb <- rma(yi = 'log(OR)', sei = SE, data = Model4_comorb[-which(rownames(Model4_comorb)%in%c("ALSPACpartners","GenR","MACS","NESDO")),])
summary(RE_model4_comorb)
OR_model4_comorb <- predict(RE_model4_comorb, transf=exp, digits=3)
weights_model4_comorb <- paste0(formatC(weights(RE_model4_comorb), format="f", digits=1, width=4), "%")

# Model 5a (in males only): DISEASE_STAT ~ CM + Age + Sex + Educ
# Check for each cohort whether they have 5 or more cases in each exposure x outcome crosstab cell
crosstabmultinomial_function(Cohort1_model5a,
                             Cohort2_model5a,
                             Cohort3_model5a,
                             Cohort4_model5a,
                             Cohort5_model5a,
                             Cohort6_model5a,
                             Cohort7_model5a,
                             Cohort8_model5a,
                             Cohort9_model5a,
                             Cohort10_model5a,
                             Cohort11_model5a,
                             Cohort12_model5a,
                             Cohort13_model5a) #Four cohorts (ALSPAC partners, MACS, NESDO, and SHIP-Legend) have to be excluded because crosstab cell < 5

# Run model, compute odds ratios and calculate weights of each cohort in meta-analysis
# Outcome level = depression
RE_model5a_dep <- rma(yi = 'log(OR)', sei = SE, data = Model5a_dep[-which(rownames(Model5a_dep)%in%c("ALSPAC_mothers","ALSPAC_partners","GenR","MACS","NESDO","SHIP_Legend")),]) #ALSPAC mothers and GenR mothers are also excluded because they do not have any male
summary(RE_model5a_dep)
OR_model5a_dep <- predict(RE_model5a_dep, transf=exp, digits=3)
weights_model5a_dep <- paste0(formatC(weights(RE_model5a_dep), format="f", digits=1, width=4), "%")

# Outcome level = cardiometabolic disease
RE_model5a_cmd <- rma(yi = 'log(OR)', sei = SE, data = Model5a_cmd[-which(rownames(Model5a_cmd)%in%c("ALSPAC_mothers","ALSPAC_partners","GenR","MACS","NESDO","SHIP_Legend")),]) #ALSPAC mothers and GenR mothers are also excluded because they do not have any male
summary(RE_model5a_cmd)
OR_model5a_cmd <- predict(RE_model5a_cmd, transf=exp, digits=3)
weights_model5a_cmd <- paste0(formatC(weights(RE_model5a_cmd), format="f", digits=1, width=4), "%")

# Outcome level = comorbidity
RE_model5a_comorbd <- rma(yi = 'log(OR)', sei = SE, data = Model5a_comorb[-which(rownames(Model5a_comorb)%in%c("ALSPAC_mothers","ALSPAC_partners","GenR","MACS","NESDO","SHIP_Legend")),]) #ALSPAC mothers and GenR mothers are also excluded because they do not have any male
summary(RE_model5a_comorbd)
OR_model5a_comorb <- predict(RE_model5a_comorbd, transf=exp, digits=3)
weights_model5a_comorb <- paste0(formatC(weights(RE_model5a_comorbd), format="f", digits=1, width=4), "%")

# Model 5b (in females only): DISEASE_STAT ~ CM + Age + Sex + Educ
# Check for each cohort whether they have 5 or more cases in each exposure x outcome crosstab cell
crosstabmultinomial_function(Cohort1_model5b,
                             Cohort2_model5b,
                             Cohort3_model5b,
                             Cohort4_model5b,
                             Cohort5_model5b,
                             Cohort6_model5b,
                             Cohort7_model5b,
                             Cohort8_model5b,
                             Cohort9_model5b,
                             Cohort10_model5b,
                             Cohort11_model5b,
                             Cohort12_model5b,
                             Cohort13_model5b) #Four cohorts (GenR mothers, MACS, NEMESIS-1, and NESDO) have to be excluded because crosstab cell < 5

# Run model, compute odds ratios and calculate weights of each cohort in meta-analysis
# Outcome level = depression
RE_model5b_dep <- rma(yi = 'log(OR)', sei = SE, data = Model5b_dep[-which(rownames(Model5b_dep)%in%c("ALSPAC_partners","GenR","MACS","NEMESIS-1","NESDO")),]) #ALSPAC partners are also excluded because they do not have any female
summary(RE_model5b_dep)
OR_model5b_dep <- predict(RE_model5b_dep, transf=exp, digits=3)
weights_model5b_dep <- paste0(formatC(weights(RE_model5b_dep), format="f", digits=1, width=4), "%")

# Outcome level = cardiometabolic disease
RE_model5b_cmd <- rma(yi = 'log(OR)', sei = SE, data = Model5b_cmd[-which(rownames(Model5b_cmd)%in%c("ALSPAC_partners","GenR","MACS","NEMESIS-1","NESDO")),]) #ALSPAC mothers and GenR mothers are also excluded because they do not have any female
summary(RE_model5b_cmd)
OR_model5b_cmd <- predict(RE_model5b_cmd, transf=exp, digits=3)
weights_model5b_cmd <- paste0(formatC(weights(RE_model5b_cmd), format="f", digits=1, width=4), "%")

# Outcome level = comorbidity
RE_model5b_comorbd <- rma(yi = 'log(OR)', sei = SE, data = Model5b_comorb[-which(rownames(Model5b_comorb)%in%c("ALSPAC_partners","GenR","MACS","NEMESIS-1","NESDO")),]) #ALSPAC mothers and GenR mothers are also excluded because they do not have any female
summary(RE_model5b_comorbd)
OR_model5b_comorb <- predict(RE_model5b_comorbd, transf=exp, digits=3)
weights_model5b_comorb <- paste0(formatC(weights(RE_model5b_comorbd), format="f", digits=1, width=4), "%")

# Model 6: DISEASE_STAT ~ CM_pa + CM_ea + CM_sa + Age + Sex + Educ
# Check for each cohort whether they have 5 or more cases in each exposure x outcome crosstab cell
crosstabmultinomial_multippredictors_function(Cohort1_model6,
                                              Cohort2_model6,
                                              Cohort3_model6,
                                              Cohort4_model6,
                                              Cohort5_model6,
                                              Cohort6_model6,
                                              Cohort7_model6,
                                              Cohort8_model6,
                                              Cohort9_model6,
                                              Cohort10_model6,
                                              Cohort11_model6,
                                              Cohort12_model6,
                                              Cohort13_model6) #Six cohorts (ALSPAC mothers, ALSPAC partners, GenR mothers, MACS, NEMESIS-1, and NESDO) have to be excluded because crosstab cell < 5

# Run model, compute odds ratios and calculate weights of each cohort in meta-analysis
# Predictor = physical abuse, outcome level = depression
RE_model6_dep_pa <- rma(yi = 'log(OR)', sei = SE, data = Model6_dep_pa[-which(rownames(Model6_dep_pa)%in%c("ALSPAC_mothers","ALSPAC_partners","GenR","MACS","MIDUS","NEMESIS-1","NESDO")),]) #MIDUS is also excluded because it does not have data on sexual abuse
summary(RE_model6_dep_pa)
OR_model6_dep_pa <- predict(RE_model6_dep_pa, transf=exp, digits=3)
weights_model6_dep_pa <- paste0(formatC(weights(RE_model6_dep_pa), format="f", digits=1, width=4), "%")

# Predictor = physical abuse, outcome level = cardiometabolic disease
RE_model6_cmd_pa <- rma(yi = 'log(OR)', sei = SE, data = Model6_cmd_pa[-which(rownames(Model6_cmd_pa)%in%c("ALSPAC_mothers","ALSPAC_partners","GenR","MACS","MIDUS","NEMESIS-1","NESDO")),]) #MIDUS is also excluded because it does not have data on sexual abuse
summary(RE_model6_cmd_pa)
OR_model6_cmd_pa <- predict(RE_model6_cmd_pa, transf=exp, digits=3)
weights_model6_cmd_pa <- paste0(formatC(weights(RE_model6_cmd_pa), format="f", digits=1, width=4), "%")

# Predictor = physical abuse, outcome level = comorbidity
RE_model6_comorb_pa <- rma(yi = 'log(OR)', sei = SE, data = Model6_comorb_pa[-which(rownames(Model6_comorb_pa)%in%c("ALSPAC_mothers","ALSPAC_partners","GenR","MACS","MIDUS","NEMESIS-1","NESDO")),]) #MIDUS is also excluded because it does not have data on sexual abuse
summary(RE_model6_comorb_pa)
OR_model6_comorb_pa <- predict(RE_model6_comorb_pa, transf=exp, digits=3)
weights_model6_comorb_pa <- paste0(formatC(weights(RE_model6_comorb_pa), format="f", digits=1, width=4), "%")

# Predictor = emotional abuse, outcome level = depression
RE_model6_dep_ea <- rma(yi = 'log(OR)', sei = SE, data = Model6_dep_ea[-which(rownames(Model6_dep_ea)%in%c("ALSPAC_mothers","ALSPAC_partners","GenR","MACS","MIDUS","NEMESIS-1","NESDO")),]) #MIDUS is also excluded because it does not have data on sexual abuse
summary(RE_model6_dep_ea)
OR_model6_dep_ea <- predict(RE_model6_dep_ea, transf=exp, digits=3)
weights_model6_dep_ea <- paste0(formatC(weights(RE_model6_dep_ea), format="f", digits=1, width=4), "%")

# Predictor = emotional abuse, outcome level = cardiometabolic disease
RE_model6_cmd_ea <- rma(yi = 'log(OR)', sei = SE, data = Model6_cmd_ea[-which(rownames(Model6_cmd_ea)%in%c("ALSPAC_mothers","ALSPAC_partners","GenR","MACS","MIDUS","NEMESIS-1","NESDO")),]) #MIDUS is also excluded because it does not have data on sexual abuse
summary(RE_model6_cmd_ea)
OR_model6_cmd_ea <- predict(RE_model6_cmd_ea, transf=exp, digits=3)
weights_model6_cmd_ea <- paste0(formatC(weights(RE_model6_cmd_ea), format="f", digits=1, width=4), "%")

# Predictor = emotional abuse, outcome level = comorbidity
RE_model6_comorb_ea <- rma(yi = 'log(OR)', sei = SE, data = Model6_comorb_ea[-which(rownames(Model6_comorb_ea)%in%c("ALSPAC_mothers","ALSPAC_partners","GenR","MACS","MIDUS","NEMESIS-1","NESDO")),]) #MIDUS is also excluded because it does not have data on sexual abuse
summary(RE_model6_comorb_ea)
OR_model6_comorb_ea <- predict(RE_model6_comorb_ea, transf=exp, digits=3)
weights_model6_comorb_ea <- paste0(formatC(weights(RE_model6_comorb_ea), format="f", digits=1, width=4), "%")

# Predictor = sexual abuse, outcome level = depression
RE_model6_dep_sa <- rma(yi = 'log(OR)', sei = SE, data = Model6_dep_sa[-which(rownames(Model6_dep_sa)%in%c("ALSPAC_mothers","ALSPAC_partners","GenR","MACS","MIDUS","NEMESIS-1","NESDO")),]) #MIDUS is also excluded because it does not have data on sexual abuse
summary(RE_model6_dep_sa)
OR_model6_dep_sa <- predict(RE_model6_dep_sa, transf=exp, digits=3)
weights_model6_dep_sa <- paste0(formatC(weights(RE_model6_dep_sa), format="f", digits=1, width=4), "%")

# Predictor = sexual abuse, outcome level = cardiometabolic disease
RE_model6_cmd_sa <- rma(yi = 'log(OR)', sei = SE, data = Model6_cmd_sa[-which(rownames(Model6_cmd_sa)%in%c("ALSPAC_mothers","ALSPAC_partners","GenR","MACS","MIDUS","NEMESIS-1","NESDO")),]) #MIDUS is also excluded because it does not have data on sexual abuse
summary(RE_model6_cmd_sa)
OR_model6_cmd_sa <- predict(RE_model6_cmd_sa, transf=exp, digits=3)
weights_model6_cmd_sa <- paste0(formatC(weights(RE_model6_cmd_sa), format="f", digits=1, width=4), "%")

# Predictor = sexual abuse, outcome level = comorbidity
RE_model6_comorb_sa <- rma(yi = 'log(OR)', sei = SE, data = Model6_comorb_sa[-which(rownames(Model6_comorb_sa)%in%c("ALSPAC_mothers","ALSPAC_partners","GenR","MACS","MIDUS","NEMESIS-1","NESDO")),]) #MIDUS is also excluded because it does not have data on sexual abuse
summary(RE_model6_comorb_sa)
OR_model6_comorb_sa <- predict(RE_model6_comorb_sa, transf=exp, digits=3)
weights_model6_comorb_sa <- paste0(formatC(weights(RE_model6_comorb_sa), format="f", digits=1, width=4), "%")

# Model 7: DISEASE_STAT ~ CM_sev + Age + Sex + Educ
# Check for each cohort whether they have 5 or more cases in each exposure x outcome crosstab cell
crosstabmultinomial_multilevelpredictor_function(Cohort1_model7,
                                                 Cohort2_model7,
                                                 Cohort3_model7,
                                                 Cohort4_model7,
                                                 Cohort5_model7,
                                                 Cohort6_model7,
                                                 Cohort7_model7,
                                                 Cohort8_model7,
                                                 Cohort9_model7,
                                                 Cohort10_model7,
                                                 Cohort11_model7,
                                                 Cohort12_model7,
                                                 Cohort13_model7) #Six cohorts (ALSPAC mothers, ALSPAC partners, GenR mothers, MACS, NEMESIS-1, and NESDO) have to be excluded because crosstab cell < 5

# Run model, compute odds ratios and calculate weights of each cohort in meta-analysis
# Predictor level = sev1, outcome level = depression
RE_model7_sev1_dep <- rma(yi = 'log(OR)', sei = SE, data = Model7_sev1_dep[-which(rownames(Model7_sev1_dep)%in%c("ALSPAC_mothers","ALSPAC_partners","GenR","MACS","MIDUS","NEMESIS-1","NESDO")),]) #MIDUS is also excluded because it does not have data on sexual abuse
summary(RE_model7_sev1_dep)
OR_model7_sev1_dep <- predict(RE_model7_sev1_dep, transf=exp, digits=3)
weights_model7_sev1_dep <- paste0(formatC(weights(RE_model7_sev1_dep), format="f", digits=1, width=4), "%")

# Predictor level = sev1, outcome level = cardiometabolic disease
RE_model7_sev1_cmd <- rma(yi = 'log(OR)', sei = SE, data = Model7_sev1_cmd[-which(rownames(Model7_sev1_cmd)%in%c("ALSPAC_mothers","ALSPAC_partners","GenR","MACS","MIDUS","NEMESIS-1","NESDO")),]) #MIDUS is also excluded because it does not have data on sexual abuse
summary(RE_model7_sev1_cmd)
OR_model7_sev1_cmd <- predict(RE_model7_sev1_cmd, transf=exp, digits=3)
weights_model7_sev1_cmd <- paste0(formatC(weights(RE_model7_sev1_cmd), format="f", digits=1, width=4), "%")

# Predictor level = sev1, outcome level = comorbidity
RE_model7_sev1_comorb <- rma(yi = 'log(OR)', sei = SE, data = Model7_sev1_comorb[-which(rownames(Model7_sev1_comorb)%in%c("ALSPAC_mothers","ALSPAC_partners","GenR","MACS","MIDUS","NEMESIS-1","NESDO")),]) #MIDUS is also excluded because it does not have data on sexual abuse
summary(RE_model7_sev1_comorb)
OR_model7_sev1_comorb <- predict(RE_model7_sev1_comorb, transf=exp, digits=3)
weights_model7_sev1_comorb <- paste0(formatC(weights(RE_model7_sev1_comorb), format="f", digits=1, width=4), "%")

# Predictor level = sev2, outcome level = depression
RE_model7_sev2_dep <- rma(yi = 'log(OR)', sei = SE, data = Model7_sev2_dep[-which(rownames(Model7_sev2_dep)%in%c("ALSPAC_mothers","ALSPAC_partners","GenR","MACS","MIDUS","NEMESIS-1","NESDO")),]) #MIDUS is also excluded because it does not have data on sexual abuse
summary(RE_model7_sev2_dep)
OR_model7_sev2_dep <- predict(RE_model7_sev2_dep, transf=exp, digits=3)
weights_model7_sev2_dep <- paste0(formatC(weights(RE_model7_sev2_dep), format="f", digits=1, width=4), "%")

# Predictor level = sev2, outcome level = cardiometabolic disease
RE_model7_sev2_cmd <- rma(yi = 'log(OR)', sei = SE, data = Model7_sev2_cmd[-which(rownames(Model7_sev2_cmd)%in%c("ALSPAC_mothers","ALSPAC_partners","GenR","MACS","MIDUS","NEMESIS-1","NESDO")),]) #MIDUS is also excluded because it does not have data on sexual abuse
summary(RE_model7_sev2_cmd)
OR_model7_sev2_cmd <- predict(RE_model7_sev2_cmd, transf=exp, digits=3)
weights_model7_sev2_cmd <- paste0(formatC(weights(RE_model7_sev2_cmd), format="f", digits=1, width=4), "%")

# Predictor level = sev2, outcome level = comorbidity
RE_model7_sev2_comorb <- rma(yi = 'log(OR)', sei = SE, data = Model7_sev2_comorb[-which(rownames(Model7_sev2_comorb)%in%c("ALSPAC_mothers","ALSPAC_partners","GenR","MACS","MIDUS","NEMESIS-1","NESDO")),]) #MIDUS is also excluded because it does not have data on sexual abuse
summary(RE_model7_sev2_comorb)
OR_model7_sev2_comorb <- predict(RE_model7_sev2_comorb, transf=exp, digits=3)
weights_model7_sev2_comorb <- paste0(formatC(weights(RE_model7_sev2_comorb), format="f", digits=1, width=4), "%")

# Model 8: DISEASE_STAT_CD ~ CM + Age + Sex + Educ
# Check for each cohort whether they have 5 or more cases in each exposure x outcome crosstab cell
crosstabmultinomial_function(Cohort1_model8,
                             Cohort2_model8,
                             Cohort3_model8,
                             Cohort4_model8,
                             Cohort5_model8,
                             Cohort6_model8,
                             Cohort7_model8,
                             Cohort8_model8,
                             Cohort9_model8,
                             Cohort10_model8,
                             Cohort11_model8,
                             Cohort12_model8,
                             Cohort13_model8) #Four cohorts (ALSPAC partners, GenR mothers, MACS, and NESDO) have to be excluded because crosstab cell < 5

# Run model, compute odds ratios and calculate weights of each cohort in meta-analysis
# Outcome level = depression
RE_model8_dep <- rma(yi = 'log(OR)', sei = SE, data = Model8_dep[-which(rownames(Model8_dep)%in%c("ALSPAC_partners","GenR","MACS","NESDO")),])
summary(RE_model8_dep)
OR_model8_dep <- predict(RE_model8_dep, transf=exp, digits=3)
weights_model8_dep <- paste0(formatC(weights(RE_model8_dep), format="f", digits=1, width=4), "%")

# Outcome level = cardiometabolic disease
RE_model8_cmd <- rma(yi = 'log(OR)', sei = SE, data = Model8_cmd[-which(rownames(Model8_cmd)%in%c("ALSPAC_partners","GenR","MACS","NESDO")),])
summary(RE_model8_cmd)
OR_model8_cmd <- predict(RE_model8_cmd, transf=exp, digits=3)
weights_model8_cmd <- paste0(formatC(weights(RE_model8_cmd), format="f", digits=1, width=4), "%")

# Outcome level = comorbidity
RE_model8_comorb <- rma(yi = 'log(OR)', sei = SE, data = Model8_comorb[-which(rownames(Model8_comorb)%in%c("ALSPAC_partners","GenR","MACS","NESDO")),])
summary(RE_model8_comorb)
OR_model8_comorb <- predict(RE_model8_comorb, transf=exp, digits=3)
weights_model8_comorb <- paste0(formatC(weights(RE_model8_comorb), format="f", digits=1, width=4), "%")

# Model 9: DISEASE_STAT_CVD2 ~ CM + Age + Sex + Educ
# Check for each cohort whether they have 5 or more cases in each exposure x outcome crosstab cell
crosstabmultinomial_function(Cohort1_model9,
                             Cohort2_model9,
                             Cohort3_model9,
                             Cohort4_model9,
                             Cohort5_model9,
                             Cohort6_model9,
                             Cohort7_model9,
                             Cohort8_model9,
                             Cohort9_model9,
                             Cohort10_model9,
                             Cohort11_model9,
                             Cohort12_model9,
                             Cohort13_model9) #One cohort (GenR mothers) has to be excluded because crosstab cell < 5

# Run model, compute odds ratios and calculate weights of each cohort in meta-analysis
# Outcome level = depression
RE_model9_dep <- rma(yi = 'log(OR)', sei = SE, data = Model9_dep[-which(rownames(Model9_dep)%in%c("ALSPAC_partners","GenR")),]) #ALSPAC partners are also excluded because it does not have data on broad definition of cardiovascular disease
summary(RE_model9_dep)
OR_model9_dep <- predict(RE_model9_dep, transf=exp, digits=3)
weights_model9_dep <- paste0(formatC(weights(RE_model9_dep), format="f", digits=1, width=4), "%")

# Outcome level = cardiometabolic disease
RE_model9_cmd <- rma(yi = 'log(OR)', sei = SE, data = Model9_cmd[-which(rownames(Model9_cmd)%in%c("ALSPAC_partners","GenR")),]) #ALSPAC partners are also excluded because it does not have data on broad definition of cardiovascular disease
summary(RE_model9_cmd)
OR_model9_cmd <- predict(RE_model9_cmd, transf=exp, digits=3)
weights_model9_cmd <- paste0(formatC(weights(RE_model9_cmd), format="f", digits=1, width=4), "%")

# Outcome level = comorbidity
RE_model9_comorb <- rma(yi = 'log(OR)', sei = SE, data = Model9_comorb[-which(rownames(Model9_comorb)%in%c("ALSPAC_partners","GenR")),]) #ALSPAC partners are also excluded because it does not have data on broad definition of cardiovascular disease
summary(RE_model9_comorb)
OR_model9_comorb <- predict(RE_model9_comorb, transf=exp, digits=3)
weights_model9_comorb <- paste0(formatC(weights(RE_model9_comorb), format="f", digits=1, width=4), "%")

# Model 10: DISEASE_STAT_med ~ CM + Age + Sex + Educ
# Check for each cohort whether they have 5 or more cases in each exposure x outcome crosstab cell
crosstabmultinomial_function(Cohort1_model10,
                             Cohort2_model10,
                             Cohort3_model10,
                             Cohort4_model10,
                             Cohort5_model10,
                             Cohort6_model10,
                             Cohort7_model10,
                             Cohort8_model10,
                             Cohort9_model10,
                             Cohort10_model10,
                             Cohort11_model10,
                             Cohort12_model10,
                             Cohort13_model10) #Two cohorts (GenR mothers and NESDO) were excluded because crosstab cell < 5

# Run model, compute odds ratios and calculate weights of each cohort in meta-analysis
# Outcome level = depression
RE_model10_dep <- rma(yi = 'log(OR)', sei = SE, data = Model10_dep[-which(rownames(Model10_dep)%in%c("ALSPAC_mothers","ALSPAC_partners","GenR","NEMESIS-1","NEMESIS-2","NESDO")),]) #ALSPAC mothers and partners as well as NEMESIS-1 and -2 are also excluded because they do not have necessary data on medication
summary(RE_model10_dep)
OR_model10_dep <- predict(RE_model10_dep, transf=exp, digits=3)
weights_model10_dep <- paste0(formatC(weights(RE_model10_dep), format="f", digits=1, width=4), "%")

# Outcome level = cardiometabolic disease
RE_model10_cmd <- rma(yi = 'log(OR)', sei = SE, data = Model10_cmd[-which(rownames(Model10_cmd)%in%c("ALSPAC_mothers","ALSPAC_partners","GenR","NEMESIS-1","NEMESIS-2","NESDO")),]) #ALSPAC mothers and partners as well as NEMESIS-1 and -2 are also excluded because they do not have necessary data on medication
summary(RE_model10_cmd)
OR_model10_cmd <- predict(RE_model10_cmd, transf=exp, digits=3)
weights_model10_cmd <- paste0(formatC(weights(RE_model10_cmd), format="f", digits=1, width=4), "%")

# Outcome level = comorbidity
RE_model10_comorb <- rma(yi = 'log(OR)', sei = SE, data = Model10_comorb[-which(rownames(Model10_comorb)%in%c("ALSPAC_mothers","ALSPAC_partners","GenR","NEMESIS-1","NEMESIS-2","NESDO")),]) #ALSPAC mothers and partners as well as NEMESIS-1 and -2 are also excluded because they do not have necessary data on medication
summary(RE_model10_comorb)
OR_model10_comorb <- predict(RE_model10_comorb, transf=exp, digits=3)
weights_model10_comorb <- paste0(formatC(weights(RE_model10_comorb), format="f", digits=1, width=4), "%")

#################### END ######################
