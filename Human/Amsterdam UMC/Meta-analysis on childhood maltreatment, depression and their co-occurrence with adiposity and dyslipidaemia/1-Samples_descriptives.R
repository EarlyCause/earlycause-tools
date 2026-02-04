############################# Cohort-level descriptive statistics, as part of the investigation of the relationship of childhood maltreatment and depression co-occurrence with metabolic outcomes #############################

# Note1: This document contains R code that was used to load on cohort's data and create samples 
# Note2: This code was used as an example to process the data of other cohorts involved in the cross-cohort data synthesis
# Note3: To follow the example below, cohort data needs to be uploaded in wide format with one case per row and the different variables in columns



#################### 1. General settings ####################

# Upload packages
library('foreign')
library('plyr')

# Set working directory, adjust with your own pathway
setwd("")



#################### 2. Load data #################### 

Data <- read.spss('Data.sav',
                  use.value.labels=FALSE,
                  to.data.frame=TRUE,
                  max.value.labels=Inf)



#################### 3. Create various samples and save descriptive statistics #################### 

# 3.1. Create main sample - participants with available data on physical abuse (cm_pa), emotional abuse (cm_ea), depression (mdd), and on at least one of the main metabolic outcomes, and excluding pregnant women
Sample_main <- Data[(!is.na(Data$cm_pa) & !is.na(Data$cm_ea))                           # Available data on physical and emotional abuse
                    & !is.na(Data$mdd)                                                  # Available data on depression
                    & (is.na(Data$preg) | Data$preg == 0)                               # Non-pregnant
                    & (!is.na(Data$bmi) |                                               # Available data on at least one of the main metabolic outcomes (bmi, waist/hip ratio, ldl/hdl ratio, and/or triglycerides)
                         !is.na(Data$waisthip_rat) | 
                         !is.na(Data$ldlhdl_rat) | 
                         !is.na(Data$trig)),]

# 3.2. Save descriptive statistics of the main sample
descriptives <- function(sample){
  out <- data.frame(n = nrow(sample),                                                                 # total number of individuals in sample
                    n_males = nrow(sample[sample$sex==0 & !is.na(sample$sex),]),                      # number of males
                    n_females = nrow(sample[sample$sex==1 & !is.na(sample$sex),]),                    # number of females
                    n_nocm = nrow(sample[sample$cm==0 & !is.na(sample$cm),]),                         # number of individuals without childhood maltreatment
                    n_cm = nrow(sample[sample$cm==1 & !is.na(sample$cm),]),                           # number of individuals with childhood maltreatment
                    n_nocmpa = nrow(sample[sample$cm_pa == 0 & !is.na(sample$cm_pa),]),               # number of individuals without physical abuse
                    n_cmpa = nrow(sample[sample$cm_pa == 1 & !is.na(sample$cm_pa),]),                 # number of individuals with physical abuse
                    n_nocmea = nrow(sample[sample$cm_ea == 0 & !is.na(sample$cm_ea),]),               # number of individuals without emotional abuse
                    n_cmea = nrow(sample[sample$cm_ea == 1 & !is.na(sample$cm_ea),]),                 # number of individuals with emotional abuse
                    n_nocmsa = nrow(sample[sample$cm_sa == 0 & !is.na(sample$cm_sa),]),               # number of individuals without sexual abuse 
                    n_cmsa = nrow(sample[sample$cm_sa == 1 & !is.na(sample$cm_sa),]),                 # number of individuals with sexual abuse
                    n_nomdd = nrow(sample[sample$mdd==0 & !is.na(sample$mdd),]),                      # number of individuals without depression
                    n_mdd = nrow(sample[sample$mdd==1 & !is.na(sample$mdd),]),                        # number of individuals with depression
                    n_noantidep = nrow(sample[sample$antidep==0 & !is.na(sample$antidep),]),          # number of individuals using no antidepressants
                    n_antidep = nrow(sample[sample$antidep==1 & !is.na(sample$antidep),]),            # number of individuals using antidepressants
                    n_cooccur0 = nrow(sample[sample$cooccur==0 & !is.na(sample$cooccur),]),           # cooccur0 is the factor level where both childhood maltreatment and depression are absent
                    n_cooccur1 = nrow(sample[sample$cooccur==1 & !is.na(sample$cooccur),]),           # cooccur1 is the factor level where childhood maltreatment is present and depression is absent
                    n_cooccur2 = nrow(sample[sample$cooccur==2 & !is.na(sample$cooccur),]),           # cooccur2 is the factor level where childhood maltreatment is absent and depression is present
                    n_cooccur3 = nrow(sample[sample$cooccur==3 & !is.na(sample$cooccur),]),           # cooccur3 is the factor level where both childhood maltreatment and depression are present
                    n_nolipid_meds = nrow(sample[sample$lipid_meds==0 & !is.na(sample$lipid_meds),]), # number of individuals taking no lipid-modifying medication
                    n_lipid_meds = nrow(sample[sample$lipid_meds==1 & !is.na(sample$lipid_meds),]),   # number of individuals taking lipid-modifying medication
                    n_bmi_withoutliers = sum(!is.na(sample$bmi_withoutliers)),                        # number of individuals with bmi data, including outliers
                    n_bmi = sum(!is.na(sample$bmi)),                                                  # number of individuals with bmi data, excluding outliers
                    n_lowbmi = nrow(sample[sample$bmi_cat==1 & !is.na(sample$bmi_cat),]),             # number of individuals with underweight
                    n_normalbmi = nrow(sample[sample$bmi_cat==2 & !is.na(sample$bmi_cat),]),          # number of individuals with normal bmi
                    n_overweightbmi = nrow(sample[sample$bmi_cat==3 & !is.na(sample$bmi_cat),]),      # number of individuals with overweight
                    n_obesebmi = nrow(sample[sample$bmi_cat==4 & !is.na(sample$bmi_cat),]),           # number of individuals with obesity
                    mean_bmi = mean(sample$bmi, na.rm=T),                                             # average bmi
                    median_bmi = median(sample$bmi, na.rm=T),                                         # median bmi
                    sd_bmi = sd(sample$bmi, na.rm=T),                                                 # bmi standard deviation
                    n_waist_cir_withoutliers = sum(!is.na(sample$waist_cir_withoutliers)),            # number of individuals with data on waist circumference, including outliers
                    n_waist_cir = sum(!is.na(sample$waist_cir)),                                      # number of individuals with data on waist circumference, excluding outliers
                    mean_waistcir = mean(sample$waist_cir, na.rm=T),                                  # average waist circumference
                    median_waistcir = median(sample$waist_cir, na.rm=T),                              # median waist circumference
                    sd_waistcir = sd(sample$waist_cir, na.rm=T),                                      # waist circumference standard deviation
                    n_hip_cir_withoutliers = sum(!is.na(sample$hip_cir_withoutliers)),                # number of individuals with data on hip circumference, including outliers
                    n_hip_cir = sum(!is.na(sample$hip_cir)),                                          # number of individuals with data on hip circumference, excluding outliers
                    mean_hipcir = mean(sample$hip_cir, na.rm=T),                                      # average hip circumference
                    median_hipcir = median(sample$hip_cir, na.rm=T),                                  # median hip circumference
                    sd_hipcir = sd(sample$hip_cir, na.rm=T),                                          # hip circumference standard deviation
                    n_waisthip_rat_withoutliers = sum(!is.na(sample$waisthip_rat_withoutliers)),      # number of individuals with wait-to-hip ratio data, including outliers
                    n_waisthip_rat = sum(!is.na(sample$waisthip_rat)),                                # number of individuals with waist-to-hip ratio data, excluding outliers
                    n_lowwaisthiprat = nrow(sample[sample$waisthip_rat_dic==0 & !is.na(sample$waisthip_rat_dic),]), # number of individuals with low waist-to-hip ratio
                    n_highwaisthiprat = nrow(sample[sample$waisthip_rat_dic==1 & !is.na(sample$waisthip_rat_dic),]), # number of individuals with high waist-to-hip ratio
                    mean_waisthiprat = mean(sample$waisthip_rat, na.rm=T),                            # average waist-to-hip ratio
                    median_waisthiprat = median(sample$waisthip_rat, na.rm=T),                        # median waist-to-hip ratio
                    sd_waisthiprat = sd(sample$waisthip_rat, na.rm=T),                                # waist-to-hip ratio standard deviation
                    n_hdl_withoutliers = sum(!is.na(sample$hdl_withoutliers)),                        # number of individuals with hdl data, including outliers
                    n_hdl = sum(!is.na(sample$hdl)),                                                  # number of individuals with hdl data, excluding outliers
                    mean_hdl = mean(sample$hdl, na.rm=T),                                             # average hdl levels
                    median_hdl = median(sample$hdl, na.rm=T),                                         # median hdl levels
                    sd_hdl = sd(sample$hdl, na.rm=T),                                                 # hdl standard deviation
                    n_ldl_withoutliers = sum(!is.na(sample$ldl_withoutliers)),                        # number of individuals with ldl data, including outliers
                    n_ldl = sum(!is.na(sample$ldl)),                                                  # number of individuals with ldl data, excluding outliers
                    mean_ldl = mean(sample$ldl, na.rm=T),                                             # average ldl levels
                    median_ldl = median(sample$ldl, na.rm=T),                                         # median ldl levels
                    sd_ldl = sd(sample$ldl, na.rm=T),                                                 # ldl standard deviation
                    n_ldlhdl_rat_withoutliers = sum(!is.na(sample$ldlhdl_rat_withoutliers)),          # number of individuals with ldl/hdl ratio, including outliers
                    n_ldlhdl_rat = sum(!is.na(sample$ldlhdl_rat)),                                    # number of individuals with ldl/hdl ratio, excluding outliers
                    n_lowldlhdl_rat = nrow(sample[sample$ldlhdl_rat_dic==0 & !is.na(sample$ldlhdl_rat_dic),]), # number of individuals with low ldl/hdl ratio
                    n_highldlhdl_rat = nrow(sample[sample$ldlhdl_rat_dic==1 & !is.na(sample$ldlhdl_rat_dic),]), # number of individuals with high ldl/hdl ratio
                    mean_ldlhdl_rat = mean(sample$ldlhdl_rat, na.rm=T),                               # average ldl/hdl ratio
                    median_ldlhdl_rat = median(sample$ldlhdl_rat, na.rm=T),                           # median ldl/hdl ratio
                    sd_ldlhdl_rat = sd(sample$ldlhdl_rat, na.rm=T),                                   # ldl/hdl ratio standard deviation
                    n_trig_withoutliers = sum(!is.na(sample$trig_withoutliers)),                      # number of individuals with triglycerides data, including outliers
                    n_trig = sum(!is.na(sample$trig)),                                                # number of individuals with triglycerides data, excluding outliers
                    n_lowtrig = nrow(sample[sample$trig_dic==0 & !is.na(sample$trig_dic),]),          # number of individuals with low triglycerides levels
                    n_hightrig = nrow(sample[sample$trig_dic==1 & !is.na(sample$trig_dic),]),         # number of individuals with high triglycerides levels
                    mean_trig = mean(sample$trig, na.rm=T),                                           # average triglycerides levels
                    median_trig = median(sample$trig, na.rm=T),                                       # median triglycerides levels
                    sd_trig = sd(sample$trig, na.rm=T),                                               # triglycerides standard deviation
                    mean_age = mean(sample$age, na.rm=T),                                             # average age
                    median_age = median(sample$age, na.rm=T),                                         # median age
                    sd_age = sd(sample$age, na.rm=T),                                                 # age standard deviation
                    n_educ0 = nrow(sample[sample$educ==0 & !is.na(sample$educ),]),                    # number of individuals with low education
                    n_educ1 = nrow(sample[sample$educ==1 & !is.na(sample$educ),]),                    # number of individuals with medium education
                    n_educ2 = nrow(sample[sample$educ==2 & !is.na(sample$educ),]),                    # number of individuals with high education
                    n_nosmoke = nrow(sample[sample$smoke==0 & !is.na(sample$smoke),]),                # number of non-smokers
                    n_smoke = nrow(sample[sample$smoke==1 & !is.na(sample$smoke),]),                  # number of smokers
                    mean_phyact = mean(sample$phyact, na.rm=T),                                       # average level of physical activity
                    median_phyact = median(sample$phyact, na.rm=T),                                   # median level of physical activity
                    sd_phyact = sd(sample$phyact, na.rm=T),                                           # Physical activity level standard deviation
                    mean_alcohol = mean(sample$alcohol, na.rm=T),                                     # average alcohol consumption
                    median_alcohol = median(sample$alcohol, na.rm=T),                                 # median alcohol consumption
                    sd_alcohol = sd(sample$alcohol, na.rm=T),                                         # alcohol consumption standard deviation
                    n_ethn0 = nrow(sample[sample$ethn==0 & !is.na(sample$ethn),]),                    # European ancestry
                    n_ethn1 = nrow(sample[sample$ethn==1 & !is.na(sample$ethn),]))                    # non-European ancestry
  return(out)
}

Sample_main_characteristics <- descriptives(Sample_main)

# 3.3. Create sample of males
Sample_males <- Data[(!is.na(Data$cm_pa) & !is.na(Data$cm_ea)) & !is.na(Data$mdd)
                   & (!is.na(Data$bmi) | !is.na(Data$waisthip_rat) | !is.na(Data$ldlhdl_rat) | !is.na(Data$trig))
                   & Data$sex==0,]                                                        # Selection of males only

Sample_males_characteristics <- descriptives(Sample_males)  #save descriptives

# 3.4. Create sample of females
Sample_females <- Data[(!is.na(Data$cm_pa) & !is.na(Data$cm_ea)) & !is.na(Data$mdd)
                     & (is.na(Data$preg) | Data$preg==0)
                     & (!is.na(Data$bmi) | !is.na(Data$waisthip_rat) | !is.na(Data$ldlhdl_rat) | !is.na(Data$trig))
                     & Data$sex==1,]                                                     # Selection of females only
Sample_females_characteristics <- descriptives(Sample_females) #save descriptives

# 3.5. Create sample of participants not taking antidepressants and lipid-modifying medication, and sample of participants taking either
Sample_nomed <- Data[(!is.na(Data$cm_pa) & !is.na(Data$cm_ea))
                     & !is.na(Data$mdd)
                     & (is.na(Data$preg) | Data$preg==0)
                     & (!is.na(Data$bmi) | !is.na(Data$waisthip_rat) | !is.na(Data$ldlhdl_rat) | !is.na(Data$trig))
                     & ((!is.na(Data$antidep) & Data$antidep == 0)                         #Excluding antidepressant users
                        & (!is.na(Data$lipid_meds) & Data$lipid_meds == 0)),]              #Excluding lipid-modifying medication users

Sample_eithermed <- Data[(!is.na(Data$cm_pa) & !is.na(Data$cm_ea))
                         & !is.na(Data$mdd)
                         & (is.na(Data$preg) | Data$preg==0)
                         & (!is.na(Data$bmi) | !is.na(Data$waisthip_rat) | !is.na(Data$ldlhdl_rat) | !is.na(Data$trig))
                         & ((!is.na(Data$antidep) & Data$antidep == 1)                         #Including antidepressants or lipid-modifying agents users
                            | (!is.na(Data$lipid_meds) & Data$lipid_meds == 1)),]

Sample_nomed_characteristics <- descriptives(Sample_nomed)  #save descriptives
Sample_eithermed_characteristics <- descriptives(Sample_eithermed)  #save descriptives



#################### 4. Create correlation table of all metabolic outcomes #################### 
df_adiposity <- data.frame(matrix(c(Sample_main$bmi, 
                                    Sample_main$waist_cir, 
                                    Sample_main$waisthip_rat,
                                    Sample_main$ldl, 
                                    Sample_main$hdl,
                                    Sample_main$ldlhdl_rat, 
                                    Sample_main$trig),
                                  ncol=7))
colnames(df_adiposity) <- c('BMI','Waist cir.','Waist/hip','LDL','HDL','LDL/HDL','Trig.')

mcor <- round(cor(data.frame(matrix(c(Sample_main$bmi, 
                                      Sample_main$waist_cir, 
                                      Sample_main$waisthip_rat,
                                      Sample_main$ldl, 
                                      Sample_main$hdl,
                                      Sample_main$ldlhdl_rat, 
                                      Sample_main$trig), 
                                    ncol=7)), 
                  use = "pairwise.complete.obs"),
              3)

rownames(mcor) <- colnames(df_adiposity)
colnames(mcor) <- colnames(df_adiposity)

upper <- mcor
upper[upper.tri(mcor)] <- ""
upper <- as.data.frame(upper)

# This is the end of the code to create samples and calculate descriptive statistics
# To continue with the analyses and run the models, follow the code example that was used in "2-IPD_models.R"
