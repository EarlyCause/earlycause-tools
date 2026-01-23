############################# Data synthesis across cohorts, on the relationship of childhood maltreatment and depression co-occurrence with metabolic outcomes #############################



#################### 0. General settings ####################
# Upload packages
library('ggplot2')
library('foreign')
library("metafor")
library('gridExtra')
library('dplyr')
library('tibble')



#################### 1. Load cohorts summary statistics #################### 

# Function to extract summary statistics
results <- function(cohort_models) {
  num_models <- length(cohort_models)
  num_columns <- 89
  
  cohort <- as.data.frame(matrix(NA, ncol = num_columns, nrow = num_models))
  
  for (i in 1:num_models) {
    if (!is.null(cohort_models[[i]])) {
      row_idx <- i  # Calculate row index based on model number
      
      if ("emm.contr" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 1] <- cohort_models[[i]]$emm.contr
      }
      if ("cohensd" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 2] <- cohort_models[[i]]$cohensd
      }
      if ("cohensd.SE" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 3] <- cohort_models[[i]]$cohensd.SE
      }
      
      
      
      if ("cohensd.contr1" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 4] <- cohort_models[[i]]$cohensd.contr1
      }
      if ("cohensd.contr1.est" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 5] <- cohort_models[[i]]$cohensd.contr1.est
      }
      if ("cohensd.contr1.SE" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 6] <- cohort_models[[i]]$cohensd.contr1.SE
      }
      
      if ("cohensd.contr2" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 7] <- cohort_models[[i]]$cohensd.contr2
      }
      if ("cohensd.contr2.est" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 8] <- cohort_models[[i]]$cohensd.contr2.est
      }
      if ("cohensd.contr2.SE" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 9] <- cohort_models[[i]]$cohensd.contr2.SE
      }
      
      if ("cohensd.contr3" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 10] <- cohort_models[[i]]$cohensd.contr3
      }
      if ("cohensd.contr3.est" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 11] <- cohort_models[[i]]$cohensd.contr3.est
      }
      if ("cohensd.contr3.SE" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 12] <- cohort_models[[i]]$cohensd.contr3.SE
      }
      
      if ("cohensd.contr4" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 13] <- cohort_models[[i]]$cohensd.contr4
      }
      if ("cohensd.contr4.est" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 14] <- cohort_models[[i]]$cohensd.contr4.est
      }
      if ("cohensd.contr4.SE" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 15] <- cohort_models[[i]]$cohensd.contr4.SE
      }
      
      if ("cohensd.contr5" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 16] <- cohort_models[[i]]$cohensd.contr5
      }
      if ("cohensd.contr5.est" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 17] <- cohort_models[[i]]$cohensd.contr5.est
      }
      if ("cohensd.contr5.SE" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 18] <- cohort_models[[i]]$cohensd.contr5.SE
      }
      
      if ("cohensd.contr6" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 19] <- cohort_models[[i]]$cohensd.contr6
      }
      if ("cohensd.contr6.est" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 20] <- cohort_models[[i]]$cohensd.contr6.est
      }
      if ("cohensd.contr6.SE" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 21] <- cohort_models[[i]]$cohensd.contr6.SE
      }
      
      
      if ("emm_cooccur0" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 22] <- cohort_models[[i]]$emm_cooccur0
      }
      if ("emm.SE_cooccur0" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 23] <- cohort_models[[i]]$emm.SE_cooccur0
      }
      if ("emm_cooccur1" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 24] <- cohort_models[[i]]$emm_cooccur1
      }
      if ("emm.SE_cooccur1" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 25] <- cohort_models[[i]]$emm.SE_cooccur1
      }
      if ("emm_cooccur2" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 26] <- cohort_models[[i]]$emm_cooccur2
      }
      if ("emm.SE_cooccur2" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 27] <- cohort_models[[i]]$emm.SE_cooccur2
      }
      if ("emm_cooccur3" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 28] <- cohort_models[[i]]$emm_cooccur3
      }
      if ("emm.SE_cooccur3" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 29] <- cohort_models[[i]]$emm.SE_cooccur3
      }
      
      
      if ("estimate_cooccur1_bmi_cat1" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 30] <- cohort_models[[i]]$estimate_cooccur1_bmi_cat1
      }
      if ("SE_cooccur1_bmi_cat1" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 31] <- cohort_models[[i]]$SE_cooccur1_bmi_cat1
      }
      if ("CI_LL_logOR_cooccur1_bmi_cat1" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 32] <- cohort_models[[i]]$CI_LL_logOR_cooccur1_bmi_cat1
      }
      if ("CI_UL_logOR_cooccur1_bmi_cat1" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 33] <- cohort_models[[i]]$CI_UL_logOR_cooccur1_bmi_cat1
      }
      
      if ("estimate_cooccur1_bmi_cat3" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 34] <- cohort_models[[i]]$estimate_cooccur1_bmi_cat3
      }
      if ("SE_cooccur1_bmi_cat3" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 35] <- cohort_models[[i]]$SE_cooccur1_bmi_cat3
      }
      if ("CI_LL_logOR_cooccur1_bmi_cat3" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 36] <- cohort_models[[i]]$CI_LL_logOR_cooccur1_bmi_cat3
      }
      if ("CI_UL_logOR_cooccur1_bmi_cat3" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 37] <- cohort_models[[i]]$CI_UL_logOR_cooccur1_bmi_cat3
      }
      
      if ("estimate_cooccur1_bmi_cat4" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 38] <- cohort_models[[i]]$estimate_cooccur1_bmi_cat4
      }
      if ("SE_cooccur1_bmi_cat4" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 39] <- cohort_models[[i]]$SE_cooccur1_bmi_cat4
      }
      if ("CI_LL_logOR_cooccur1_bmi_cat4" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 40] <- cohort_models[[i]]$CI_LL_logOR_cooccur1_bmi_cat4
      }
      if ("CI_UL_logOR_cooccur1_bmi_cat4" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 41] <- cohort_models[[i]]$CI_UL_logOR_cooccur1_bmi_cat4
      }
      
      
      if ("estimate_cooccur2_bmi_cat1" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 42] <- cohort_models[[i]]$estimate_cooccur2_bmi_cat1
      }
      if ("SE_cooccur2_bmi_cat1" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 43] <- cohort_models[[i]]$SE_cooccur2_bmi_cat1
      }
      if ("CI_LL_logOR_cooccur2_bmi_cat1" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 44] <- cohort_models[[i]]$CI_LL_logOR_cooccur2_bmi_cat1
      }
      if ("CI_UL_logOR_cooccur2_bmi_cat1" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 45] <- cohort_models[[i]]$CI_UL_logOR_cooccur2_bmi_cat1
      }
      
      if ("estimate_cooccur2_bmi_cat3" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 46] <- cohort_models[[i]]$estimate_cooccur2_bmi_cat3
      }
      if ("SE_cooccur2_bmi_cat3" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 47] <- cohort_models[[i]]$SE_cooccur2_bmi_cat3
      }
      if ("CI_LL_logOR_cooccur2_bmi_cat3" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 48] <- cohort_models[[i]]$CI_LL_logOR_cooccur2_bmi_cat3
      }
      if ("CI_UL_logOR_cooccur2_bmi_cat3" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 49] <- cohort_models[[i]]$CI_UL_logOR_cooccur2_bmi_cat3
      }
      
      if ("estimate_cooccur2_bmi_cat4" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 50] <- cohort_models[[i]]$estimate_cooccur2_bmi_cat4
      }
      if ("SE_cooccur2_bmi_cat4" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 51] <- cohort_models[[i]]$SE_cooccur2_bmi_cat4
      }
      if ("CI_LL_logOR_cooccur2_bmi_cat4" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 52] <- cohort_models[[i]]$CI_LL_logOR_cooccur2_bmi_cat4
      }
      if ("CI_UL_logOR_cooccur2_bmi_cat4" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 53] <- cohort_models[[i]]$CI_UL_logOR_cooccur2_bmi_cat4
      }
      
      
      if ("estimate_cooccur3_bmi_cat1" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 54] <- cohort_models[[i]]$estimate_cooccur3_bmi_cat1
      }
      if ("SE_cooccur3_bmi_cat1" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 55] <- cohort_models[[i]]$SE_cooccur3_bmi_cat1
      }
      if ("CI_LL_logOR_cooccur3_bmi_cat1" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 56] <- cohort_models[[i]]$CI_LL_logOR_cooccur3_bmi_cat1
      }
      if ("CI_UL_logOR_cooccur3_bmi_cat1" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 57] <- cohort_models[[i]]$CI_UL_logOR_cooccur3_bmi_cat1
      }
      
      if ("estimate_cooccur3_bmi_cat3" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 58] <- cohort_models[[i]]$estimate_cooccur3_bmi_cat3
      }
      if ("SE_cooccur3_bmi_cat3" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 59] <- cohort_models[[i]]$SE_cooccur3_bmi_cat3
      }
      if ("CI_LL_logOR_cooccur3_bmi_cat3" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 60] <- cohort_models[[i]]$CI_LL_logOR_cooccur3_bmi_cat3
      }
      if ("CI_UL_logOR_cooccur3_bmi_cat3" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 61] <- cohort_models[[i]]$CI_UL_logOR_cooccur3_bmi_cat3
      }
      
      if ("estimate_cooccur3_bmi_cat4" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 62] <- cohort_models[[i]]$estimate_cooccur3_bmi_cat4
      }
      if ("SE_cooccur3_bmi_cat4" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 63] <- cohort_models[[i]]$SE_cooccur3_bmi_cat4
      }
      if ("CI_LL_logOR_cooccur3_bmi_cat4" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 64] <- cohort_models[[i]]$CI_LL_logOR_cooccur3_bmi_cat4
      }
      if ("CI_UL_logOR_cooccur3_bmi_cat4" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 65] <- cohort_models[[i]]$CI_UL_logOR_cooccur3_bmi_cat4
      }
      
      
      
      if ("estimate_cooccur1_outcome_dic" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 66] <- cohort_models[[i]]$estimate_cooccur1_outcome_dic
      }
      if ("SE_cooccur1_outcome_dic" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 67] <- cohort_models[[i]]$SE_cooccur1_outcome_dic
      }
      if ("CI_LL_logOR_cooccur1_outcome_dic" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 68] <- cohort_models[[i]]$CI_LL_logOR_cooccur1_outcome_dic
      }
      if ("CI_UL_logOR_cooccur1_outcome_dic" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 69] <- cohort_models[[i]]$CI_UL_logOR_cooccur1_outcome_dic
      }
      
      
      if ("estimate_cooccur2_outcome_dic" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 70] <- cohort_models[[i]]$estimate_cooccur2_outcome_dic
      }
      if ("SE_cooccur2_outcome_dic" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 71] <- cohort_models[[i]]$SE_cooccur2_outcome_dic
      }
      if ("CI_LL_logOR_cooccur2_outcome_dic" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 72] <- cohort_models[[i]]$CI_LL_logOR_cooccur2_outcome_dic
      }
      if ("CI_UL_logOR_cooccur2_outcome_dic" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 73] <- cohort_models[[i]]$CI_UL_logOR_cooccur2_outcome_dic
      }
      
      if ("estimate_cooccur3_outcome_dic" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 74] <- cohort_models[[i]]$estimate_cooccur3_outcome_dic
      }
      if ("SE_cooccur3_outcome_dic" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 75] <- cohort_models[[i]]$SE_cooccur3_outcome_dic
      }
      if ("CI_LL_logOR_cooccur3_outcome_dic" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 76] <- cohort_models[[i]]$CI_LL_logOR_cooccur3_outcome_dic
      }
      if ("CI_UL_logOR_cooccur3_outcome_dic" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 77] <- cohort_models[[i]]$CI_UL_logOR_cooccur3_outcome_dic
      }
      
      if ("anova_df" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 78] <- cohort_models[[i]]$anova_df
      }
      if ("anova_F" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 79] <- cohort_models[[i]]$anova_F
      }
      if ("anova_p" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 80] <- cohort_models[[i]]$anova_p
      }
      
      if ("N" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 81] <- cohort_models[[i]]$N
      }
      if ("n_cm0" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 82] <- cohort_models[[i]]$n_cm0
      }
      if ("n_cm1" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 83] <- cohort_models[[i]]$n_cm1
      }
      if ("n_mdd0" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 84] <- cohort_models[[i]]$n_mdd0
      }
      if ("n_mdd1" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 85] <- cohort_models[[i]]$n_mdd1
      }
      if ("n_cooccur0" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 86] <- cohort_models[[i]]$n_cooccur0
      }
      if ("n_cooccur1" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 87] <- cohort_models[[i]]$n_cooccur1
      }
      if ("n_cooccur2" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 88] <- cohort_models[[i]]$n_cooccur2
      }
      if ("n_cooccur3" %in% names(cohort_models[[i]])) {
        cohort[row_idx, 89] <- cohort_models[[i]]$n_cooccur3
      }
    }
  }
  
  rownames(cohort) <- paste0("model", 1:num_models)
  colnames(cohort) <- c('contrast','cohensd','SE',
                        'contrast1','cohensd1','SE1',
                        'contrast2','cohensd2','SE2',
                        'contrast3','cohensd3','SE3',
                        'contrast4','cohensd4','SE4',
                        'contrast5','cohensd5','SE5',
                        'contrast6','cohensd6','SE6',
                        'emm_cooccur0','emm.SE_cooccur0',
                        'emm_cooccur1','emm.SE_cooccur1',
                        'emm_cooccur2','emm.SE_cooccur2',
                        'emm_cooccur3','emm.SE_cooccur3',
                        'logOR_cooccur1_bmi_cat1', 'SE_cooccur1_bmi_cat1','CI_LL_logOR_cooccur1_bmi_cat1', 'CI_UL_logOR_cooccur1_bmi_cat1',
                        'logOR_cooccur1_bmi_cat3', 'SE_cooccur1_bmi_cat3','CI_LL_logOR_cooccur1_bmi_cat3', 'CI_UL_logOR_cooccur1_bmi_cat3',
                        'logOR_cooccur1_bmi_cat4', 'SE_cooccur1_bmi_cat4','CI_LL_logOR_cooccur1_bmi_cat4', 'CI_UL_logOR_cooccur1_bmi_cat4',
                        'logOR_cooccur2_bmi_cat1', 'SE_cooccur2_bmi_cat1','CI_LL_logOR_cooccur2_bmi_cat1', 'CI_UL_logOR_cooccur2_bmi_cat1',
                        'logOR_cooccur2_bmi_cat3', 'SE_cooccur2_bmi_cat3','CI_LL_logOR_cooccur2_bmi_cat3', 'CI_UL_logOR_cooccur2_bmi_cat3',
                        'logOR_cooccur2_bmi_cat4', 'SE_cooccur2_bmi_cat4','CI_LL_logOR_cooccur2_bmi_cat4', 'CI_UL_logOR_cooccur2_bmi_cat4',
                        'logOR_cooccur3_bmi_cat1', 'SE_cooccur3_bmi_cat1','CI_LL_logOR_cooccur3_bmi_cat1', 'CI_UL_logOR_cooccur3_bmi_cat1',
                        'logOR_cooccur3_bmi_cat3', 'SE_cooccur3_bmi_cat3','CI_LL_logOR_cooccur3_bmi_cat3', 'CI_UL_logOR_cooccur3_bmi_cat3',
                        'logOR_cooccur3_bmi_cat4', 'SE_cooccur3_bmi_cat4','CI_LL_logOR_cooccur3_bmi_cat4', 'CI_UL_logOR_cooccur3_bmi_cat4',
                        'logOR_cooccur1_outcome_dic','SE_cooccur1_outcome_dic','CI_LL_logOR_cooccur1_outcome_dic', 'CI_UL_logOR_cooccur1_outcome_dic',
                        'logOR_cooccur2_outcome_dic','SE_cooccur2_outcome_dic','CI_LL_logOR_cooccur2_outcome_dic', 'CI_UL_logOR_cooccur2_outcome_dic',
                        'logOR_cooccur3_outcome_dic','SE_cooccur3_outcome_dic','CI_LL_logOR_cooccur3_outcome_dic', 'CI_UL_logOR_cooccur3_outcome_dic',
                        'anova_df','anova_F','anova_p',
                        'n',
                        'n_cm0', 'n_cm1', 'n_mdd0', 'n_mdd1', 'n_cooccur0', 'n_cooccur1', 'n_cooccur2', 'n_cooccur3')
  
  return(cohort)
}


# Cohort 1 summary statistics
setwd("N:/Documenten/MA_Fat/MA_Fat/CohortOutputs/cohort1")
cohort1_models <- list()
cohort1_models[[1]] <- read.table(file = "cohort1_model1_date.csv", header = T, sep = ",")
cohort1_models[[2]] <-  read.table(file = "cohort1_model2_date.csv", header = T, sep = ",")
cohort1_models[[3]] <- read.table(file = "cohort1_model3_date.csv", header = T, sep = ",")
cohort1_models[[4]] <- read.table(file = "cohort1_model4_date.csv", header = T, sep = ",")
cohort1_models[[5]] <- read.table(file = "cohort1_model5_date.csv", header = T, sep = ",")
cohort1_models[[6]] <- read.table(file = "cohort1_model6_date.csv", header = T, sep = ",")
cohort1_models[[7]] <- read.table(file = "cohort1_model7_date.csv", header = T, sep = ",")
cohort1_models[[8]] <- read.table(file = "cohort1_model8_date.csv", header = T, sep = ",")
cohort1_models[[9]] <- read.table(file = "cohort1_model9_date.csv", header = T, sep = ",")
cohort1_models[[10]] <- read.table(file = "cohort1_model6_date.csv", header = T, sep = ",")
cohort1_models[[11]] <- read.table(file = "cohort1_model11_date.csv", header = T, sep = ",")
cohort1_models[[12]] <- read.table(file = "cohort1_model12_date.csv", header = T, sep = ",")
cohort1_models[[13]] <- read.table(file = "cohort1_model13_date.csv", header = T, sep = ",")
cohort1_models[[14]] <- read.table(file = "cohort1_model14_date.csv", header = T, sep = ",")
cohort1_models[[15]] <- read.table(file = "cohort1_model15_date.csv", header = T, sep = ",")
cohort1_models[[16]] <- read.table(file = "cohort1_model16_date.csv", header = T, sep = ",")
cohort1_models[[17]] <- read.table(file = "cohort1_model17_date.csv", header = T, sep = ",")
cohort1_models[[18]] <- read.table(file = "cohort1_model18_date.csv", header = T, sep = ",")
cohort1_models[[19]] <- read.table(file = "cohort1_model19_date.csv", header = T, sep = ",")
cohort1_models[[20]] <- read.table(file = "cohort1_model20_date.csv", header = T, sep = ",")
cohort1_models[[21]] <- read.table(file = "cohort1_model18_date.csv", header = T, sep = ",")
cohort1_models[[22]] <- read.table(file = "cohort1_model22_date.csv", header = T, sep = ",")
cohort1_models[[23]] <- read.table(file = "cohort1_model23_date.csv", header = T, sep = ",")
cohort1_models[[24]] <- read.table(file = "cohort1_model24_date.csv", header = T, sep = ",")
cohort1_models[[25]] <- read.table(file = "cohort1_model25_date.csv", header = T, sep = ",")
cohort1_models[[26]] <- read.table(file = "cohort1_model26_date.csv", header = T, sep = ",")
cohort1_models[[27]] <- read.table(file = "cohort1_model27_date.csv", header = T, sep = ",")
cohort1_models[[28]] <- read.table(file = "cohort1_model28_date.csv", header = T, sep = ",")
cohort1_models[[29]] <- read.table(file = "cohort1_model29_date.csv", header = T, sep = ",")
cohort1_models[[30]] <- read.table(file = "cohort1_model30_date.csv", header = T, sep = ",")
cohort1_models[[31]] <- read.table(file = "cohort1_model31_date.csv", header = T, sep = ",")
cohort1_models[[32]] <- read.table(file = "cohort1_model32_date.csv", header = T, sep = ",")

cohort1 <- results(cohort1_models)
  
save(cohort1, file="~/cohort1_Effect_sizes.RData")

# Load and extract summary statistics similarly for all other cohorts



#################### 2. Descriptive statistics across cohorts #################### 
Descriptives_cohort1 <- read.table(file = "~/cohort1_Descriptivesmain_date.csv", header = T, sep = ",")
Descriptives_cohort2 <- read.table(file = "~/cohort2_Descriptivesmain_date.csv", header = T, sep = ",")
#continue for all cohorts

Descriptives <- rbind(Descriptives_cohort1, 
                      Descriptives_cohort2,
                      Descriptives_cohort3,
                      Descriptives_cohort4,
                      Descriptives_cohort6,
                      Descriptives_cohort6,
                      Descriptives_cohort7,
                      Descriptives_cohort8,
                      Descriptives_cohort9,
                      Descriptives_cohort10,
                      Descriptives_cohort11)

# Create Table 1
Table1 <- data.frame(cohort = c(Descriptives$cohorts, "Total"),                                                               # cohort names and pooled sample
                     n = c(Descriptives$n,sum(Descriptives$n)),                                                               # sample sizes
                     females_n = c(Descriptives$n_females, sum(Descriptives$n_females)),                                      # female sample size
                     females_percentage = round(c(Descriptives$n_females*100/(Descriptives$n_females + Descriptives$n_males), # female percentage
                                                  weighted.mean(Descriptives$n_females*100/(Descriptives$n_females + Descriptives$n_males), Descriptives$n)), digits = 0),
                     age_mean = round(c(Descriptives$mean_age, weighted.mean(Descriptives$mean_age, Descriptives$n)), digits = 2), # average age
                     age_sd = round(c(Descriptives$sd_age, weighted.mean(Descriptives$sd_age, Descriptives$n)), digits = 2),  # age standard deviation
                     low_education_n = c(Descriptives$n_educ0, sum(Descriptives$n_educ0)),                                    # number of participants with low education level
                     low_education_percentage = round(c(Descriptives$n_educ0*100/(Descriptives$n_educ0 + Descriptives$n_educ1 + Descriptives$n_educ2), # percentage of participants with low education level
                                                        weighted.mean(Descriptives$n_educ0*100/(Descriptives$n_educ0 + Descriptives$n_educ1 + Descriptives$n_educ2), 
                                                                      Descriptives$n)), digits = 0),
                     middle_education_n = c(Descriptives$n_educ1, sum(Descriptives$n_educ1)),                                 # number of participants with middle education level
                     middle_education_percentage = round(c(Descriptives$n_educ1*100/(Descriptives$n_educ0 + Descriptives$n_educ1 + Descriptives$n_educ2),  # percentage of participants with middle education level
                                                           weighted.mean(Descriptives$n_educ1*100/(Descriptives$n_educ0 + Descriptives$n_educ1 + Descriptives$n_educ2), 
                                                                         Descriptives$n)), digits = 0),
                     high_education_n = c(Descriptives$n_educ2, sum(Descriptives$n_educ2)),                                   # number of participants with high education
                     high_education_percentage = round(c(Descriptives$n_educ2*100/(Descriptives$n_educ0 + Descriptives$n_educ1 + Descriptives$n_educ2), # percentage of participants with high education level
                                                         weighted.mean(Descriptives$n_educ2*100/(Descriptives$n_educ0 + Descriptives$n_educ1 + Descriptives$n_educ2), 
                                                                       Descriptives$n)), digits = 0),
                     european_n = c(Descriptives$n_ethn0, sum(Descriptives$n_ethn0, na.rm=T)),                                # number of participants with European ancestry
                     european_ancestry_percentage = round(c(Descriptives$n_ethn0*100/(Descriptives$n_ethn0 + Descriptives$n_ethn1), # percentage of participants with European ancestry 
                                                            weighted.mean(Descriptives$n_ethn0*100/(Descriptives$n_ethn0 + Descriptives$n_ethn1), Descriptives$n)), 
                                                          digits = 0),
                     no_maltreatment_no_mdd_n = c(Descriptives$n_cooccur0, sum(Descriptives$n_cooccur0)),                     # number of participants with neither childhood maltreatment nor depression
                     no_maltreatment_no_mdd_percentage = round(c(Descriptives$n_cooccur0*100/(Descriptives$n_cooccur0 + Descriptives$n_cooccur1 + Descriptives$n_cooccur2 + Descriptives$n_cooccur3), # percentage of participants with neither childhood maltreatment nor depression
                                                                 weighted.mean(Descriptives$n_cooccur0*100/(Descriptives$n_cooccur0 + Descriptives$n_cooccur1 + Descriptives$n_cooccur2 + Descriptives$n_cooccur3), Descriptives$n)), 
                                                               digits = 0),
                     maltreatment_only_n = c(Descriptives$n_cooccur1, sum(Descriptives$n_cooccur1)),                          # number of participants with childhood maltreatment but no depression
                     maltreatment_only_percentage = round(c(Descriptives$n_cooccur1*100/(Descriptives$n_cooccur0 + Descriptives$n_cooccur1 + Descriptives$n_cooccur2 + Descriptives$n_cooccur3), # percentage of participants with childhood maltreatment but no depression
                                                            weighted.mean(Descriptives$n_cooccur1*100/(Descriptives$n_cooccur0 + Descriptives$n_cooccur1 + Descriptives$n_cooccur2 + Descriptives$n_cooccur3), Descriptives$n)), 
                                                          digits = 0),
                     mdd_only_n = c(Descriptives$n_cooccur2, sum(Descriptives$n_cooccur2)),                                   # number of participants with depression but no childhood maltreatment
                     mdd_only_percentage = round(c(Descriptives$n_cooccur2*100/(Descriptives$n_cooccur0 + Descriptives$n_cooccur1 + Descriptives$n_cooccur2 + Descriptives$n_cooccur3), # percentage of participants with depression but no childhood maltreatment
                                                   weighted.mean(Descriptives$n_cooccur2*100/(Descriptives$n_cooccur0 + Descriptives$n_cooccur1 + Descriptives$n_cooccur2 + Descriptives$n_cooccur3), Descriptives$n)), 
                                                 digits = 0),
                     cm_and_mdd_n = c(Descriptives$n_cooccur3, sum(Descriptives$n_cooccur3)),                                 # number of participants with both childhood maltreatment and depresion
                     cm_and_mdd_percentage = round(c(Descriptives$n_cooccur3*100/(Descriptives$n_cooccur0 + Descriptives$n_cooccur1 + Descriptives$n_cooccur2 + Descriptives$n_cooccur3), # percentage of participants with both childhood maltreatment and depression
                                                     weighted.mean(Descriptives$n_cooccur3*100/(Descriptives$n_cooccur0 + Descriptives$n_cooccur1 + Descriptives$n_cooccur2 + Descriptives$n_cooccur3), Descriptives$n)), 
                                                   digits = 0),
                     BMI_mean = round(c(Descriptives$mean_bmi, weighted.mean(Descriptives$mean_bmi, Descriptives$n)), digits = 2), # average bmi
                     BMI_sd = round(c(Descriptives$sd_bmi, weighted.mean(Descriptives$sd_bmi, Descriptives$n)), digits = 2),  # bmi standard deviation
                     BMI_underweight_n = c(Descriptives$n_lowbmi, sum(Descriptives$n_lowbmi)),                                # number of participants with underweight bmi
                     BMI_underweight_percentage = round(c(Descriptives$n_lowbmi*100/(Descriptives$n_lowbmi + Descriptives$n_normalbmi + Descriptives$n_overweightbmi + Descriptives$n_obesebmi), # percentage of participants with underweight bmi
                                                          weighted.mean(Descriptives$n_lowbmi*100/(Descriptives$n_lowbmi + Descriptives$n_normalbmi + Descriptives$n_overweightbmi + Descriptives$n_obesebmi), Descriptives$n)), 
                                                        digits = 0),
                     BMI_normal_n = c(Descriptives$n_normalbmi, sum(Descriptives$n_normalbmi)),                               # number of participants with normal bmi
                     BMI_normal_percentage = round(c(Descriptives$n_normalbmi*100/(Descriptives$n_lowbmi + Descriptives$n_normalbmi + Descriptives$n_overweightbmi + Descriptives$n_obesebmi), # percentage of participants with normal bmi
                                                     weighted.mean(Descriptives$n_normalbmi*100/(Descriptives$n_lowbmi + Descriptives$n_normalbmi + Descriptives$n_overweightbmi + Descriptives$n_obesebmi), Descriptives$n)), 
                                                   digits = 0),
                     BMI_overweight_n = c(Descriptives$n_overweightbmi, sum(Descriptives$n_overweightbmi)),                   # number of participants with overweight bmi
                     BMI_overweight_percentage = round(c(Descriptives$n_overweightbmi*100/(Descriptives$n_lowbmi + Descriptives$n_normalbmi + Descriptives$n_overweightbmi + Descriptives$n_obesebmi), # percentage of participants with overweight bmi
                                                         weighted.mean(Descriptives$n_overweightbmi*100/(Descriptives$n_lowbmi + Descriptives$n_normalbmi + Descriptives$n_overweightbmi + Descriptives$n_obesebmi), Descriptives$n)), 
                                                       digits = 0),
                     BMI_obese_n = c(Descriptives$n_obesebmi, sum(Descriptives$n_obesebmi)),                                  # number of participants with obese bmi
                     BMI_obese_percentage = round(c(Descriptives$n_obesebmi*100/(Descriptives$n_lowbmi + Descriptives$n_normalbmi + Descriptives$n_overweightbmi + Descriptives$n_obesebmi), # percentage of participants with obese bmi
                                                    weighted.mean(Descriptives$n_obesebmi*100/(Descriptives$n_lowbmi + Descriptives$n_normalbmi + Descriptives$n_overweightbmi + Descriptives$n_obesebmi), Descriptives$n)), 
                                                  digits = 0),
                     WHR_mean = round(c(Descriptives$mean_waisthiprat, weighted.mean(Descriptives$mean_waisthiprat, Descriptives$n)), # average waist-to-hip ratio
                                      digits = 2),
                     WHR_sd = round(c(Descriptives$sd_waisthiprat, weighted.mean(Descriptives$sd_waisthiprat, Descriptives$n)), # waist-to-hip ration standard deviation
                                    digits = 2),
                     WHR_healthy_n = c(Descriptives$n_lowwaisthiprat, sum(Descriptives$n_lowwaisthiprat)),                   # number of participants with low waist-to-hip ratio
                     WHR_healthy_percentage = round(c(Descriptives$n_lowwaisthiprat*100/(Descriptives$n_lowwaisthiprat + Descriptives$n_highwaisthiprat), # percentage of participants with low waist-to-hip ratio
                                                      weighted.mean(Descriptives$n_lowwaisthiprat*100/(Descriptives$n_lowwaisthiprat + Descriptives$n_highwaisthiprat), Descriptives$n)), 
                                                    digits = 0),
                     WHR_unhealthy_n = c(Descriptives$n_highwaisthiprat, sum(Descriptives$n_highwaisthiprat)),               # number of participants with high waist-to-hip ratio
                     WHR_unhealthy_percentage = round(c(Descriptives$n_highwaisthiprat*100/(Descriptives$n_lowwaisthiprat + Descriptives$n_highwaisthiprat), # percentage of participants with high waist-to-hip ratio
                                                        weighted.mean(Descriptives$n_highwaisthiprat*100/(Descriptives$n_lowwaisthiprat + Descriptives$n_highwaisthiprat), Descriptives$n)), 
                                                      digits = 0),
                     Triglycerides_mean = round(c(Descriptives$mean_trig, weighted.mean(Descriptives$mean_trig, Descriptives$n)), # average triglycerides levels
                                                digits = 2),
                     Triglycerides_sd = round(c(Descriptives$sd_trig, weighted.mean(Descriptives$sd_trig, Descriptives$n)),  # triglycerides standard deviation
                                              digits = 2),
                     Triglycerides_healthy_n = c(Descriptives$n_lowtrig, sum(Descriptives$n_lowtrig)),                       # number of participants with low triglycerides levels
                     Triglycerides_healthy_percentage = round(c(Descriptives$n_lowtrig*100/(Descriptives$n_lowtrig + Descriptives$n_hightrig), # percentage of participants with low triglycerides leveles
                                                                weighted.mean(Descriptives$n_lowtrig*100/(Descriptives$n_lowtrig + Descriptives$n_hightrig), Descriptives$n)), digits = 0),
                     Triglycerides_unhealthy_n = c(Descriptives$n_hightrig, sum(Descriptives$n_hightrig)),                   # number of participants with high triglycerides levels
                     Triglycerides_unhealthy_percentage = round(c(Descriptives$n_hightrig*100/(Descriptives$n_lowtrig + Descriptives$n_hightrig), # percentage of participants with high triglycerides levles
                                                                  weighted.mean(Descriptives$n_hightrig*100/(Descriptives$n_lowtrig + Descriptives$n_hightrig), Descriptives$n)), digits = 0),
                     LDLHDL_mean = round(c(Descriptives$mean_ldlhdl_rat, weighted.mean(Descriptives$mean_ldlhdl_rat, Descriptives$n)), # average ldl/hdl ratio
                                         digits = 2),
                     LDLHDL_sd = round(c(Descriptives$sd_ldlhdl_rat, weighted.mean(Descriptives$sd_ldlhdl_rat, Descriptives$n)), # ldl/hdl ratio standard deviation
                                       digits = 2),
                     LDLHDL_healthy_n = c(Descriptives$n_lowldlhdl_rat, sum(Descriptives$n_lowldlhdl_rat)),                  # number of participants with a low ldl/hdl level
                     LDLHDL_healthy_percentage = round(c(Descriptives$n_lowldlhdl_rat*100/(Descriptives$n_lowldlhdl_rat + Descriptives$n_highldlhdl_rat), # percentage of participants with low ldl/hdl levels
                                                         weighted.mean(Descriptives$n_lowldlhdl_rat*100/(Descriptives$n_lowldlhdl_rat + Descriptives$n_highldlhdl_rat), Descriptives$n)), digits = 0),
                     LDLHDL_unhealthy_n = c(Descriptives$n_highldlhdl_rat, sum(Descriptives$n_highldlhdl_rat)),              # number of participants with a high level of ldl/hdl ratio
                     LDLHDL_unhealthy_percentage = round(c(Descriptives$n_highldlhdl_rat*100/(Descriptives$n_lowldlhdl_rat + Descriptives$n_highldlhdl_rat), # percentage of participants with high levels of ldl/hdl levels 
                                                           weighted.mean(Descriptives$n_highldlhdl_rat*100/(Descriptives$n_lowldlhdl_rat + Descriptives$n_highldlhdl_rat), Descriptives$n)), digits = 0),
                     smoke_n = c(Descriptives$n_smoke, sum(Descriptives$n_smoke)),                                           # number of smokers
                     smoke_percentage = round(c(Descriptives$n_smoke*100/(Descriptives$n_smoke + Descriptives$n_nosmoke),    # percentage of smokers
                                                weighted.mean(Descriptives$n_smoke*100/(Descriptives$n_smoke + Descriptives$n_nosmoke), Descriptives$n)), 
                                              digits = 0),
                     physical_activity_mean_hours_per_week = round(c(Descriptives$mean_phyact, weighted.mean(Descriptives$mean_phyact, Descriptives$n)), # average hours per week of physical activity
                                                                   digits = 2),
                     physical_activity_sd_hours_per_week = round(c(Descriptives$sd_phyact, weighted.mean(Descriptives$sd_phyact, Descriptives$n)), # standard deviation of hours per week of physical activity
                                                                 digits = 2),
                     alcohol_mean_drinks_per_week = round(c(Descriptives$mean_alcohol, weighted.mean(Descriptives$mean_alcohol, Descriptives$n)), # average number of alcoholic rinks per week
                                                          digits = 2),
                     alcohol_sd_drinks_per_week = round(c(Descriptives$sd_alcohol, weighted.mean(Descriptives$sd_alcohol, Descriptives$n)), # standard deviation of number of alcoholic drinks per week
                                                        digits = 2))



#################### 3. Models 1, 2, 3, and 4 - Effect sizes of association of childhood maltreatment with metabolic outcomes #################### 

Model1_cm <- as.data.frame(rbind(cohort1 = c(cohort1['model1', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]), 
                                 cohort2 = c(cohort2['model1', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]), 
                                 cohort3 = c(cohort3['model1', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort4 = c(cohort4['model1', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort5 = c(cohort5['model1', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort6 = c(cohort6['model1', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort7 = c(cohort7['model1', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort8 = c(cohort8['model1', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort9 = c(cohort9['model1', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort10 = c(cohort10['model1', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort11 = c(cohort11['model1', c('cohensd', 'SE', 'n_cm0', 'n_cm1')])))

colnames(Model1_cm) <- c('cohensd', 'SE', 'n_cm0', 'n_cm1')

Model2_cm <- as.data.frame(rbind(cohort1 = c(cohort1['model2', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]), 
                                 cohort2 = c(cohort2['model2', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]), 
                                 cohort3 = c(cohort3['model2', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort4 = c(cohort4['model2', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort5 = c(cohort5['model2', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort6 = c(cohort6['model2', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort7 = c(cohort7['model2', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort8 = c(cohort8['model2', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort9 = c(cohort9['model2', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort10 = c(cohort10['model2', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort11 = c(cohort11['model2', c('cohensd', 'SE', 'n_cm0', 'n_cm1')])))

colnames(Model2_cm) <- c('cohensd', 'SE', 'n_cm0', 'n_cm1')

Model3_cm <- as.data.frame(rbind(cohort1 = c(cohort1['model3', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]), 
                                 cohort2 = c(cohort2['model3', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]), 
                                 cohort3 = c(cohort3['model3', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort4 = c(cohort4['model3', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort5 = c(cohort5['model3', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort6 = c(cohort6['model3', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort7 = c(cohort7['model3', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort8 = c(cohort8['model3', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort9 = c(cohort9['model3', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort10 = c(cohort10['model3', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort11 = c(cohort11['model3', c('cohensd', 'SE', 'n_cm0', 'n_cm1')])))

colnames(Model3_cm) <- c('cohensd', 'SE', 'n_cm0', 'n_cm1')

Model4_cm <- as.data.frame(rbind(cohort1 = c(cohort1['model4', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]), 
                                 cohort2 = c(cohort2['model4', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]), 
                                 cohort3 = c(cohort3['model4', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort4 = c(cohort4['model4', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort5 = c(cohort5['model4', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort6 = c(cohort6['model4', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort7 = c(cohort7['model4', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort8 = c(cohort8['model4', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort9 = c(cohort9['model4', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort10 = c(cohort10['model4', c('cohensd', 'SE', 'n_cm0', 'n_cm1')]),
                                 cohort11 = c(cohort11['model4', c('cohensd', 'SE', 'n_cm0', 'n_cm1')])))

colnames(Model4_cm) <- c('cohensd', 'SE', 'n_cm0', 'n_cm1')



#################### 4. Models 5, 6, 7, and 8  - Effect sizes of association of depression with metabolic outcomes #################### 
Model5_mdd <- as.data.frame(rbind(cohort1 = c(cohort1['model5', c('cohensd','SE','n_mdd0','n_mdd1')]), 
                                 cohort2 = c(cohort2['model5', c('cohensd','SE','n_mdd0','n_mdd1')]), 
                                 cohort3 = c(cohort3['model5', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                 cohort4 = c(cohort4['model5', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                 cohort5 = c(cohort5['model5', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                 cohort6 = c(cohort6['model5', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                 cohort7 = c(cohort7['model5', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                 cohort8 = c(cohort8['model5', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                 cohort9 = c(cohort9['model5', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                 cohort10 = c(cohort10['model5', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                 cohort11 = c(cohort11['model5', c('cohensd','SE','n_mdd0','n_mdd1')])))

colnames(Model5_mdd) <- c('cohensd', 'SE','n_mdd0', 'n_mdd1')

Model6_mdd <- as.data.frame(rbind(cohort1 = c(cohort1['model6', c('cohensd','SE','n_mdd0','n_mdd1')]), 
                                   cohort2 = c(cohort2['model6', c('cohensd','SE','n_mdd0','n_mdd1')]), 
                                   cohort3 = c(cohort3['model6', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                   cohort4 = c(cohort4['model6', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                   cohort5 = c(cohort5['model6', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                   cohort6 = c(cohort6['model6', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                   cohort7 = c(cohort7['model6', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                   cohort8 = c(cohort8['model6', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                   cohort9 = c(cohort9['model6', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                   cohort10 = c(cohort10['model6', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                   cohort11 = c(cohort11['model6', c('cohensd','SE','n_mdd0','n_mdd1')])))

colnames(Model6_mdd) <- c('cohensd', 'SE','n_mdd0', 'n_mdd1')

Model7_mdd <- as.data.frame(rbind(cohort1 = c(cohort1['model7', c('cohensd','SE','n_mdd0','n_mdd1')]), 
                                   cohort2 = c(cohort2['model7', c('cohensd','SE','n_mdd0','n_mdd1')]), 
                                   cohort3 = c(cohort3['model7', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                   cohort4 = c(cohort4['model7', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                   cohort5 = c(cohort5['model7', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                   cohort6 = c(cohort6['model7', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                   cohort7 = c(cohort7['model7', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                   cohort8 = c(cohort8['model7', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                   cohort9 = c(cohort9['model7', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                   cohort10 = c(cohort10['model7', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                   cohort11 = c(cohort11['model7', c('cohensd','SE','n_mdd0','n_mdd1')])))

colnames(Model7_mdd) <- c('cohensd', 'SE','n_mdd0', 'n_mdd1')

Model8_mdd <- as.data.frame(rbind(cohort1 = c(cohort1['model8', c('cohensd','SE','n_mdd0','n_mdd1')]), 
                                   cohort2 = c(cohort2['model8', c('cohensd','SE','n_mdd0','n_mdd1')]), 
                                   cohort3 = c(cohort3['model8', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                   cohort4 = c(cohort4['model8', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                   cohort5 = c(cohort5['model8', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                   cohort6 = c(cohort6['model8', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                   cohort7 = c(cohort7['model8', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                   cohort8 = c(cohort8['model8', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                   cohort9 = c(cohort9['model8', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                   cohort10 = c(cohort10['model8', c('cohensd','SE','n_mdd0','n_mdd1')]),
                                   cohort11 = c(cohort11['model8', c('cohensd','SE','n_mdd0','n_mdd1')])))

colnames(Model8_mdd) <- c('cohensd', 'SE','n_mdd0', 'n_mdd1')



#################### 5. Models 9, 10, 11, and 12 - Effect sizes of association of cooccurrence of childhood maltreatment and depression with metabolic outcomes #################### 
Model9_cooccur1_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model9','cohensd1'], cohort1['model9','SE1'], cohort1['model9', c('n', 'n_cooccur1')]), 
                              cohort2 = c(cohort2['model9','cohensd1'], cohort2['model9','SE1'], cohort2['model9', c('n', 'n_cooccur1')]), 
                              cohort3 = c(cohort3['model9','cohensd1'], cohort3['model9','SE1'], cohort3['model9', c('n', 'n_cooccur1')]),
                              cohort4 = c(cohort4['model9', 'cohensd1'], cohort4['model9','SE1'], cohort4['model9', c('n', 'n_cooccur1')]),
                              cohort5 = c(cohort5['model9', 'cohensd1'], cohort5['model9','SE1'], cohort5['model9', c('n', 'n_cooccur1')]),
                              cohort6 = c(cohort6['model9', 'cohensd1'], cohort6['model9','SE1'], cohort6['model9', c('n', 'n_cooccur1')]),
                              cohort7 = c(cohort7['model9', 'cohensd1'], cohort7['model9','SE1'], cohort7['model9', c('n', 'n_cooccur1')]),
                              cohort8 = c(cohort8['model9', 'cohensd1'], cohort8['model9','SE1'], cohort8['model9', c('n', 'n_cooccur1')]),
                              cohort9 = c(cohort9['model9', 'cohensd1'], cohort9['model9','SE1'], cohort9['model9', c('n', 'n_cooccur1')]),
                              cohort10 = c(cohort10['model9', 'cohensd1'], cohort10['model9','SE1'], cohort10['model9', c('n', 'n_cooccur1')]),
                              cohort11 = c(cohort11['model9', 'cohensd1'], cohort11['model9','SE1'], cohort11['model9', c('n', 'n_cooccur1')])))
colnames(Model9_cooccur1_vs_cooccur0) <- c('cohensd', 'SE', 'n', 'n_cooccur1')

Model9_cooccur2_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model9','cohensd2'], cohort1['model9','SE2'], cohort1['model9', c('n', 'n_cooccur2')]), 
                                                    cohort2 = c(cohort2['model9','cohensd2'], cohort2['model9','SE2'], cohort2['model9', c('n', 'n_cooccur2')]), 
                                                    cohort3 = c(cohort3['model9','cohensd2'], cohort3['model9','SE2'], cohort3['model9', c('n', 'n_cooccur2')]),
                                                    cohort4 = c(cohort4['model9', 'cohensd2'], cohort4['model9','SE2'], cohort4['model9', c('n', 'n_cooccur2')]),
                                                    cohort5 = c(cohort5['model9', 'cohensd2'], cohort5['model9','SE2'], cohort5['model9', c('n', 'n_cooccur2')]),
                                                    cohort6 = c(cohort6['model9', 'cohensd2'], cohort6['model9','SE2'], cohort6['model9', c('n', 'n_cooccur2')]),
                                                    cohort7 = c(cohort7['model9', 'cohensd2'], cohort7['model9','SE2'], cohort7['model9', c('n', 'n_cooccur2')]),
                                                    cohort8 = c(cohort8['model9', 'cohensd2'], cohort8['model9','SE2'], cohort8['model9', c('n', 'n_cooccur2')]),
                                                    cohort9 = c(cohort9['model9', 'cohensd2'], cohort9['model9','SE2'], cohort9['model9', c('n', 'n_cooccur2')]),
                                                    cohort10 = c(cohort10['model9', 'cohensd2'], cohort10['model9','SE2'], cohort10['model9', c('n', 'n_cooccur2')]),
                                                    cohort11 = c(cohort11['model9', 'cohensd2'], cohort11['model9','SE2'], cohort11['model9', c('n', 'n_cooccur2')])))

colnames(Model9_cooccur2_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur2')

Model9_cooccur3_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model9','cohensd3'], cohort1['model9','SE3'], cohort1['model9', c('n', 'n_cooccur3')]), 
                                                    cohort2 = c(cohort2['model9','cohensd3'], cohort2['model9','SE3'], cohort2['model9', c('n', 'n_cooccur3')]), 
                                                    cohort3 = c(cohort3['model9','cohensd3'], cohort3['model9','SE3'], cohort3['model9', c('n', 'n_cooccur3')]),
                                                    cohort4 = c(cohort4['model9', 'cohensd3'], cohort4['model9','SE3'], cohort4['model9', c('n', 'n_cooccur3')]),
                                                    cohort5 = c(cohort5['model9', 'cohensd3'], cohort5['model9','SE3'], cohort5['model9', c('n', 'n_cooccur3')]),
                                                    cohort6 = c(cohort6['model9', 'cohensd3'], cohort6['model9','SE3'], cohort6['model9', c('n', 'n_cooccur3')]),
                                                    cohort7 = c(cohort7['model9', 'cohensd3'], cohort7['model9','SE3'], cohort7['model9', c('n', 'n_cooccur3')]),
                                                    cohort8 = c(cohort8['model9', 'cohensd3'], cohort8['model9','SE3'], cohort8['model9', c('n', 'n_cooccur3')]),
                                                    cohort9 = c(cohort9['model9', 'cohensd3'], cohort9['model9','SE3'], cohort9['model9', c('n', 'n_cooccur3')]),
                                                    cohort10 = c(cohort10['model9', 'cohensd3'], cohort10['model9','SE3'], cohort10['model9', c('n', 'n_cooccur3')]),
                                                    cohort11 = c(cohort11['model9', 'cohensd3'], cohort11['model9','SE3'], cohort11['model9', c('n', 'n_cooccur3')])))
colnames(Model9_cooccur3_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur3')

Model9_cooccur2_vs_cooccur1 <- as.data.frame(rbind(cohort1 = c(cohort1['model9','cohensd4'], cohort1['model9','SE4']), 
                                                    cohort2 = c(cohort2['model9','cohensd4'], cohort2['model9','SE4']), 
                                                    cohort3 = c(cohort3['model9','cohensd4'], cohort3['model9','SE4']),
                                                    cohort4 = c(cohort4['model9', 'cohensd4'], cohort4['model9','SE4']),
                                                    cohort5 = c(cohort5['model9', 'cohensd4'], cohort5['model9','SE4']),
                                                    cohort6 = c(cohort6['model9', 'cohensd4'], cohort6['model9','SE4']),
                                                    cohort7 = c(cohort7['model9', 'cohensd4'], cohort7['model9','SE4']),
                                                    cohort8 = c(cohort8['model9', 'cohensd4'], cohort8['model9','SE4']),
                                                    cohort9 = c(cohort9['model9', 'cohensd4'], cohort9['model9','SE4']),
                                                    cohort10 = c(cohort10['model9', 'cohensd4'], cohort10['model9','SE4']),
                                                    cohort11 = c(cohort11['model9', 'cohensd4'], cohort11['model9','SE4'])))
colnames(Model9_cooccur2_vs_cooccur1) <- c('cohensd', 'SE')

Model9_cooccur3_vs_cooccur1 <- as.data.frame(rbind(cohort1 = c(cohort1['model9','cohensd5'], cohort1['model9','SE5']), 
                                                    cohort2 = c(cohort2['model9','cohensd5'], cohort2['model9','SE5']), 
                                                    cohort3 = c(cohort3['model9','cohensd5'], cohort3['model9','SE5']),
                                                    cohort4 = c(cohort4['model9', 'cohensd5'], cohort4['model9','SE5']),
                                                    cohort5 = c(cohort5['model9', 'cohensd5'], cohort5['model9','SE5']),
                                                    cohort6 = c(cohort6['model9', 'cohensd5'], cohort6['model9','SE5']),
                                                    cohort7 = c(cohort7['model9', 'cohensd5'], cohort7['model9','SE5']),
                                                    cohort8 = c(cohort8['model9', 'cohensd5'], cohort8['model9','SE5']),
                                                    cohort9 = c(cohort9['model9', 'cohensd5'], cohort9['model9','SE5']),
                                                    cohort10 = c(cohort10['model9', 'cohensd5'], cohort10['model9','SE5']),
                                                    cohort11 = c(cohort11['model9', 'cohensd5'], cohort11['model9','SE5'])))
colnames(Model9_cooccur3_vs_cooccur1) <- c('cohensd', 'SE')

Model9_cooccur3_vs_cooccur2 <- as.data.frame(rbind(cohort1 = c(cohort1['model9','cohensd6'], cohort1['model9','SE6']), 
                                                    cohort2 = c(cohort2['model9','cohensd6'], cohort2['model9','SE6']), 
                                                    cohort3 = c(cohort3['model9','cohensd6'], cohort3['model9','SE6']),
                                                    cohort4 = c(cohort4['model9', 'cohensd6'], cohort4['model9','SE6']),
                                                    cohort5 = c(cohort5['model9', 'cohensd6'], cohort5['model9','SE6']),
                                                    cohort6 = c(cohort6['model9', 'cohensd6'], cohort6['model9','SE6']),
                                                    cohort7 = c(cohort7['model9', 'cohensd6'], cohort7['model9','SE6']),
                                                    cohort8 = c(cohort8['model9', 'cohensd6'], cohort8['model9','SE6']),
                                                    cohort9 = c(cohort9['model9', 'cohensd6'], cohort9['model9','SE6']),
                                                    cohort10 = c(cohort10['model9', 'cohensd6'], cohort10['model9','SE6']),
                                                    cohort11 = c(cohort11['model9', 'cohensd6'], cohort11['model9','SE6'])))
colnames(Model9_cooccur3_vs_cooccur2) <- c('cohensd', 'SE')

Model10_cooccur1_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model10','cohensd1'], cohort1['model10','SE1'], cohort1['model10', c('n', 'n_cooccur1')]), 
                                                    cohort2 = c(cohort2['model10','cohensd1'], cohort2['model10','SE1'], cohort2['model10', c('n', 'n_cooccur1')]), 
                                                    cohort3 = c(cohort3['model10','cohensd1'], cohort3['model10','SE1'], cohort3['model10', c('n', 'n_cooccur1')]),
                                                    cohort4 = c(cohort4['model10', 'cohensd1'], cohort4['model10','SE1'], cohort4['model10', c('n', 'n_cooccur1')]),
                                                    cohort5 = c(cohort5['model10', 'cohensd1'], cohort5['model10','SE1'], cohort5['model10', c('n', 'n_cooccur1')]),
                                                    cohort6 = c(cohort6['model10', 'cohensd1'], cohort6['model10','SE1'], cohort6['model10', c('n', 'n_cooccur1')]),
                                                    cohort7 = c(cohort7['model10', 'cohensd1'], cohort7['model10','SE1'], cohort7['model10', c('n', 'n_cooccur1')]),
                                                    cohort8 = c(cohort8['model10', 'cohensd1'], cohort8['model10','SE1'], cohort8['model10', c('n', 'n_cooccur1')]),
                                                    cohort9 = c(cohort9['model10', 'cohensd1'], cohort9['model10','SE1'], cohort9['model10', c('n', 'n_cooccur1')]),
                                                    cohort10 = c(cohort10['model10', 'cohensd1'], cohort10['model10','SE1'], cohort10['model10', c('n', 'n_cooccur1')]),
                                                    cohort11 = c(cohort11['model10', 'cohensd1'], cohort11['model10','SE1'], cohort11['model10', c('n', 'n_cooccur1')])))
colnames(Model10_cooccur1_vs_cooccur0) <- c('cohensd', 'SE', 'n', 'n_cooccur1')

Model10_cooccur2_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model10','cohensd2'], cohort1['model10','SE2'], cohort1['model10', c('n', 'n_cooccur2')]), 
                                                    cohort2 = c(cohort2['model10','cohensd2'], cohort2['model10','SE2'], cohort2['model10', c('n', 'n_cooccur2')]), 
                                                    cohort3 = c(cohort3['model10','cohensd2'], cohort3['model10','SE2'], cohort3['model10', c('n', 'n_cooccur2')]),
                                                    cohort4 = c(cohort4['model10', 'cohensd2'], cohort4['model10','SE2'], cohort4['model10', c('n', 'n_cooccur2')]),
                                                    cohort5 = c(cohort5['model10', 'cohensd2'], cohort5['model10','SE2'], cohort5['model10', c('n', 'n_cooccur2')]),
                                                    cohort6 = c(cohort6['model10', 'cohensd2'], cohort6['model10','SE2'], cohort6['model10', c('n', 'n_cooccur2')]),
                                                    cohort7 = c(cohort7['model10', 'cohensd2'], cohort7['model10','SE2'], cohort7['model10', c('n', 'n_cooccur2')]),
                                                    cohort8 = c(cohort8['model10', 'cohensd2'], cohort8['model10','SE2'], cohort8['model10', c('n', 'n_cooccur2')]),
                                                    cohort9 = c(cohort9['model10', 'cohensd2'], cohort9['model10','SE2'], cohort9['model10', c('n', 'n_cooccur2')]),
                                                    cohort10 = c(cohort10['model10', 'cohensd2'], cohort10['model10','SE2'], cohort10['model10', c('n', 'n_cooccur2')]),
                                                    cohort11 = c(cohort11['model10', 'cohensd2'], cohort11['model10','SE2'], cohort11['model10', c('n', 'n_cooccur2')])))

colnames(Model10_cooccur2_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur2')

Model10_cooccur3_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model10','cohensd3'], cohort1['model10','SE3'], cohort1['model10', c('n', 'n_cooccur3')]), 
                                                    cohort2 = c(cohort2['model10','cohensd3'], cohort2['model10','SE3'], cohort2['model10', c('n', 'n_cooccur3')]), 
                                                    cohort3 = c(cohort3['model10','cohensd3'], cohort3['model10','SE3'], cohort3['model10', c('n', 'n_cooccur3')]),
                                                    cohort4 = c(cohort4['model10', 'cohensd3'], cohort4['model10','SE3'], cohort4['model10', c('n', 'n_cooccur3')]),
                                                    cohort5 = c(cohort5['model10', 'cohensd3'], cohort5['model10','SE3'], cohort5['model10', c('n', 'n_cooccur3')]),
                                                    cohort6 = c(cohort6['model10', 'cohensd3'], cohort6['model10','SE3'], cohort6['model10', c('n', 'n_cooccur3')]),
                                                    cohort7 = c(cohort7['model10', 'cohensd3'], cohort7['model10','SE3'], cohort7['model10', c('n', 'n_cooccur3')]),
                                                    cohort8 = c(cohort8['model10', 'cohensd3'], cohort8['model10','SE3'], cohort8['model10', c('n', 'n_cooccur3')]),
                                                    cohort9 = c(cohort9['model10', 'cohensd3'], cohort9['model10','SE3'], cohort9['model10', c('n', 'n_cooccur3')]),
                                                    cohort10 = c(cohort10['model10', 'cohensd3'], cohort10['model10','SE3'], cohort10['model10', c('n', 'n_cooccur3')]),
                                                    cohort11 = c(cohort11['model10', 'cohensd3'], cohort11['model10','SE3'], cohort11['model10', c('n', 'n_cooccur3')])))
colnames(Model10_cooccur3_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur3')

Model10_cooccur2_vs_cooccur1 <- as.data.frame(rbind(cohort1 = c(cohort1['model10','cohensd4'], cohort1['model10','SE4']), 
                                                    cohort2 = c(cohort2['model10','cohensd4'], cohort2['model10','SE4']), 
                                                    cohort3 = c(cohort3['model10','cohensd4'], cohort3['model10','SE4']),
                                                    cohort4 = c(cohort4['model10', 'cohensd4'], cohort4['model10','SE4']),
                                                    cohort5 = c(cohort5['model10', 'cohensd4'], cohort5['model10','SE4']),
                                                    cohort6 = c(cohort6['model10', 'cohensd4'], cohort6['model10','SE4']),
                                                    cohort7 = c(cohort7['model10', 'cohensd4'], cohort7['model10','SE4']),
                                                    cohort8 = c(cohort8['model10', 'cohensd4'], cohort8['model10','SE4']),
                                                    cohort9 = c(cohort9['model10', 'cohensd4'], cohort9['model10','SE4']),
                                                    cohort10 = c(cohort10['model10', 'cohensd4'], cohort10['model10','SE4']),
                                                    cohort11 = c(cohort11['model10', 'cohensd4'], cohort11['model10','SE4'])))
colnames(Model10_cooccur2_vs_cooccur1) <- c('cohensd', 'SE')

Model10_cooccur3_vs_cooccur1 <- as.data.frame(rbind(cohort1 = c(cohort1['model10','cohensd5'], cohort1['model10','SE5']), 
                                                    cohort2 = c(cohort2['model10','cohensd5'], cohort2['model10','SE5']), 
                                                    cohort3 = c(cohort3['model10','cohensd5'], cohort3['model10','SE5']),
                                                    cohort4 = c(cohort4['model10', 'cohensd5'], cohort4['model10','SE5']),
                                                    cohort5 = c(cohort5['model10', 'cohensd5'], cohort5['model10','SE5']),
                                                    cohort6 = c(cohort6['model10', 'cohensd5'], cohort6['model10','SE5']),
                                                    cohort7 = c(cohort7['model10', 'cohensd5'], cohort7['model10','SE5']),
                                                    cohort8 = c(cohort8['model10', 'cohensd5'], cohort8['model10','SE5']),
                                                    cohort9 = c(cohort9['model10', 'cohensd5'], cohort9['model10','SE5']),
                                                    cohort10 = c(cohort10['model10', 'cohensd5'], cohort10['model10','SE5']),
                                                    cohort11 = c(cohort11['model10', 'cohensd5'], cohort11['model10','SE5'])))
colnames(Model10_cooccur3_vs_cooccur1) <- c('cohensd', 'SE')

Model10_cooccur3_vs_cooccur2 <- as.data.frame(rbind(cohort1 = c(cohort1['model10','cohensd6'], cohort1['model10','SE6']), 
                                                    cohort2 = c(cohort2['model10','cohensd6'], cohort2['model10','SE6']), 
                                                    cohort3 = c(cohort3['model10','cohensd6'], cohort3['model10','SE6']),
                                                    cohort4 = c(cohort4['model10', 'cohensd6'], cohort4['model10','SE6']),
                                                    cohort5 = c(cohort5['model10', 'cohensd6'], cohort5['model10','SE6']),
                                                    cohort6 = c(cohort6['model10', 'cohensd6'], cohort6['model10','SE6']),
                                                    cohort7 = c(cohort7['model10', 'cohensd6'], cohort7['model10','SE6']),
                                                    cohort8 = c(cohort8['model10', 'cohensd6'], cohort8['model10','SE6']),
                                                    cohort9 = c(cohort9['model10', 'cohensd6'], cohort9['model10','SE6']),
                                                    cohort10 = c(cohort10['model10', 'cohensd6'], cohort10['model10','SE6']),
                                                    cohort11 = c(cohort11['model10', 'cohensd6'], cohort11['model10','SE6'])))
colnames(Model10_cooccur3_vs_cooccur2) <- c('cohensd', 'SE')

Model11_cooccur1_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model11','cohensd1'], cohort1['model11','SE1'], cohort1['model11', c('n', 'n_cooccur1')]), 
                                                    cohort2 = c(cohort2['model11','cohensd1'], cohort2['model11','SE1'], cohort2['model11', c('n', 'n_cooccur1')]), 
                                                    cohort3 = c(cohort3['model11','cohensd1'], cohort3['model11','SE1'], cohort3['model11', c('n', 'n_cooccur1')]),
                                                    cohort4 = c(cohort4['model11', 'cohensd1'], cohort4['model11','SE1'], cohort4['model11', c('n', 'n_cooccur1')]),
                                                    cohort5 = c(cohort5['model11', 'cohensd1'], cohort5['model11','SE1'], cohort5['model11', c('n', 'n_cooccur1')]),
                                                    cohort6 = c(cohort6['model11', 'cohensd1'], cohort6['model11','SE1'], cohort6['model11', c('n', 'n_cooccur1')]),
                                                    cohort7 = c(cohort7['model11', 'cohensd1'], cohort7['model11','SE1'], cohort7['model11', c('n', 'n_cooccur1')]),
                                                    cohort8 = c(cohort8['model11', 'cohensd1'], cohort8['model11','SE1'], cohort8['model11', c('n', 'n_cooccur1')]),
                                                    cohort9 = c(cohort9['model11', 'cohensd1'], cohort9['model11','SE1'], cohort9['model11', c('n', 'n_cooccur1')]),
                                                    cohort10 = c(cohort10['model11', 'cohensd1'], cohort10['model11','SE1'], cohort10['model11', c('n', 'n_cooccur1')]),
                                                    cohort11 = c(cohort11['model11', 'cohensd1'], cohort11['model11','SE1'], cohort11['model11', c('n', 'n_cooccur1')])))
colnames(Model11_cooccur1_vs_cooccur0) <- c('cohensd', 'SE', 'n', 'n_cooccur1')

Model11_cooccur2_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model11','cohensd2'], cohort1['model11','SE2'], cohort1['model11', c('n', 'n_cooccur2')]), 
                                                    cohort2 = c(cohort2['model11','cohensd2'], cohort2['model11','SE2'], cohort2['model11', c('n', 'n_cooccur2')]), 
                                                    cohort3 = c(cohort3['model11','cohensd2'], cohort3['model11','SE2'], cohort3['model11', c('n', 'n_cooccur2')]),
                                                    cohort4 = c(cohort4['model11', 'cohensd2'], cohort4['model11','SE2'], cohort4['model11', c('n', 'n_cooccur2')]),
                                                    cohort5 = c(cohort5['model11', 'cohensd2'], cohort5['model11','SE2'], cohort5['model11', c('n', 'n_cooccur2')]),
                                                    cohort6 = c(cohort6['model11', 'cohensd2'], cohort6['model11','SE2'], cohort6['model11', c('n', 'n_cooccur2')]),
                                                    cohort7 = c(cohort7['model11', 'cohensd2'], cohort7['model11','SE2'], cohort7['model11', c('n', 'n_cooccur2')]),
                                                    cohort8 = c(cohort8['model11', 'cohensd2'], cohort8['model11','SE2'], cohort8['model11', c('n', 'n_cooccur2')]),
                                                    cohort9 = c(cohort9['model11', 'cohensd2'], cohort9['model11','SE2'], cohort9['model11', c('n', 'n_cooccur2')]),
                                                    cohort10 = c(cohort10['model11', 'cohensd2'], cohort10['model11','SE2'], cohort10['model11', c('n', 'n_cooccur2')]),
                                                    cohort11 = c(cohort11['model11', 'cohensd2'], cohort11['model11','SE2'], cohort11['model11', c('n', 'n_cooccur2')])))

colnames(Model11_cooccur2_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur2')

Model11_cooccur3_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model11','cohensd3'], cohort1['model11','SE3'], cohort1['model11', c('n', 'n_cooccur3')]), 
                                                    cohort2 = c(cohort2['model11','cohensd3'], cohort2['model11','SE3'], cohort2['model11', c('n', 'n_cooccur3')]), 
                                                    cohort3 = c(cohort3['model11','cohensd3'], cohort3['model11','SE3'], cohort3['model11', c('n', 'n_cooccur3')]),
                                                    cohort4 = c(cohort4['model11', 'cohensd3'], cohort4['model11','SE3'], cohort4['model11', c('n', 'n_cooccur3')]),
                                                    cohort5 = c(cohort5['model11', 'cohensd3'], cohort5['model11','SE3'], cohort5['model11', c('n', 'n_cooccur3')]),
                                                    cohort6 = c(cohort6['model11', 'cohensd3'], cohort6['model11','SE3'], cohort6['model11', c('n', 'n_cooccur3')]),
                                                    cohort7 = c(cohort7['model11', 'cohensd3'], cohort7['model11','SE3'], cohort7['model11', c('n', 'n_cooccur3')]),
                                                    cohort8 = c(cohort8['model11', 'cohensd3'], cohort8['model11','SE3'], cohort8['model11', c('n', 'n_cooccur3')]),
                                                    cohort9 = c(cohort9['model11', 'cohensd3'], cohort9['model11','SE3'], cohort9['model11', c('n', 'n_cooccur3')]),
                                                    cohort10 = c(cohort10['model11', 'cohensd3'], cohort10['model11','SE3'], cohort10['model11', c('n', 'n_cooccur3')]),
                                                    cohort11 = c(cohort11['model11', 'cohensd3'], cohort11['model11','SE3'], cohort11['model11', c('n', 'n_cooccur3')])))
colnames(Model11_cooccur3_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur3')

Model11_cooccur2_vs_cooccur1 <- as.data.frame(rbind(cohort1 = c(cohort1['model11','cohensd4'], cohort1['model11','SE4']), 
                                                    cohort2 = c(cohort2['model11','cohensd4'], cohort2['model11','SE4']), 
                                                    cohort3 = c(cohort3['model11','cohensd4'], cohort3['model11','SE4']),
                                                    cohort4 = c(cohort4['model11', 'cohensd4'], cohort4['model11','SE4']),
                                                    cohort5 = c(cohort5['model11', 'cohensd4'], cohort5['model11','SE4']),
                                                    cohort6 = c(cohort6['model11', 'cohensd4'], cohort6['model11','SE4']),
                                                    cohort7 = c(cohort7['model11', 'cohensd4'], cohort7['model11','SE4']),
                                                    cohort8 = c(cohort8['model11', 'cohensd4'], cohort8['model11','SE4']),
                                                    cohort9 = c(cohort9['model11', 'cohensd4'], cohort9['model11','SE4']),
                                                    cohort10 = c(cohort10['model11', 'cohensd4'], cohort10['model11','SE4']),
                                                    cohort11 = c(cohort11['model11', 'cohensd4'], cohort11['model11','SE4'])))
colnames(Model11_cooccur2_vs_cooccur1) <- c('cohensd', 'SE')

Model11_cooccur3_vs_cooccur1 <- as.data.frame(rbind(cohort1 = c(cohort1['model11','cohensd5'], cohort1['model11','SE5']), 
                                                    cohort2 = c(cohort2['model11','cohensd5'], cohort2['model11','SE5']), 
                                                    cohort3 = c(cohort3['model11','cohensd5'], cohort3['model11','SE5']),
                                                    cohort4 = c(cohort4['model11', 'cohensd5'], cohort4['model11','SE5']),
                                                    cohort5 = c(cohort5['model11', 'cohensd5'], cohort5['model11','SE5']),
                                                    cohort6 = c(cohort6['model11', 'cohensd5'], cohort6['model11','SE5']),
                                                    cohort7 = c(cohort7['model11', 'cohensd5'], cohort7['model11','SE5']),
                                                    cohort8 = c(cohort8['model11', 'cohensd5'], cohort8['model11','SE5']),
                                                    cohort9 = c(cohort9['model11', 'cohensd5'], cohort9['model11','SE5']),
                                                    cohort10 = c(cohort10['model11', 'cohensd5'], cohort10['model11','SE5']),
                                                    cohort11 = c(cohort11['model11', 'cohensd5'], cohort11['model11','SE5'])))
colnames(Model11_cooccur3_vs_cooccur1) <- c('cohensd', 'SE')

Model11_cooccur3_vs_cooccur2 <- as.data.frame(rbind(cohort1 = c(cohort1['model11','cohensd6'], cohort1['model11','SE6']), 
                                                    cohort2 = c(cohort2['model11','cohensd6'], cohort2['model11','SE6']), 
                                                    cohort3 = c(cohort3['model11','cohensd6'], cohort3['model11','SE6']),
                                                    cohort4 = c(cohort4['model11', 'cohensd6'], cohort4['model11','SE6']),
                                                    cohort5 = c(cohort5['model11', 'cohensd6'], cohort5['model11','SE6']),
                                                    cohort6 = c(cohort6['model11', 'cohensd6'], cohort6['model11','SE6']),
                                                    cohort7 = c(cohort7['model11', 'cohensd6'], cohort7['model11','SE6']),
                                                    cohort8 = c(cohort8['model11', 'cohensd6'], cohort8['model11','SE6']),
                                                    cohort9 = c(cohort9['model11', 'cohensd6'], cohort9['model11','SE6']),
                                                    cohort10 = c(cohort10['model11', 'cohensd6'], cohort10['model11','SE6']),
                                                    cohort11 = c(cohort11['model11', 'cohensd6'], cohort11['model11','SE6'])))
colnames(Model11_cooccur3_vs_cooccur2) <- c('cohensd', 'SE')

Model12_cooccur1_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model12','cohensd1'], cohort1['model12','SE1'], cohort1['model12', c('n', 'n_cooccur1')]), 
                                                    cohort2 = c(cohort2['model12','cohensd1'], cohort2['model12','SE1'], cohort2['model12', c('n', 'n_cooccur1')]), 
                                                    cohort3 = c(cohort3['model12','cohensd1'], cohort3['model12','SE1'], cohort3['model12', c('n', 'n_cooccur1')]),
                                                    cohort4 = c(cohort4['model12', 'cohensd1'], cohort4['model12','SE1'], cohort4['model12', c('n', 'n_cooccur1')]),
                                                    cohort5 = c(cohort5['model12', 'cohensd1'], cohort5['model12','SE1'], cohort5['model12', c('n', 'n_cooccur1')]),
                                                    cohort6 = c(cohort6['model12', 'cohensd1'], cohort6['model12','SE1'], cohort6['model12', c('n', 'n_cooccur1')]),
                                                    cohort7 = c(cohort7['model12', 'cohensd1'], cohort7['model12','SE1'], cohort7['model12', c('n', 'n_cooccur1')]),
                                                    cohort8 = c(cohort8['model12', 'cohensd1'], cohort8['model12','SE1'], cohort8['model12', c('n', 'n_cooccur1')]),
                                                    cohort9 = c(cohort9['model12', 'cohensd1'], cohort9['model12','SE1'], cohort9['model12', c('n', 'n_cooccur1')]),
                                                    cohort10 = c(cohort10['model12', 'cohensd1'], cohort10['model12','SE1'], cohort10['model12', c('n', 'n_cooccur1')]),
                                                    cohort11 = c(cohort11['model12', 'cohensd1'], cohort11['model12','SE1'], cohort11['model12', c('n', 'n_cooccur1')])))
colnames(Model12_cooccur1_vs_cooccur0) <- c('cohensd', 'SE', 'n', 'n_cooccur1')

Model12_cooccur2_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model12','cohensd2'], cohort1['model12','SE2'], cohort1['model12', c('n', 'n_cooccur2')]), 
                                                    cohort2 = c(cohort2['model12','cohensd2'], cohort2['model12','SE2'], cohort2['model12', c('n', 'n_cooccur2')]), 
                                                    cohort3 = c(cohort3['model12','cohensd2'], cohort3['model12','SE2'], cohort3['model12', c('n', 'n_cooccur2')]),
                                                    cohort4 = c(cohort4['model12', 'cohensd2'], cohort4['model12','SE2'], cohort4['model12', c('n', 'n_cooccur2')]),
                                                    cohort5 = c(cohort5['model12', 'cohensd2'], cohort5['model12','SE2'], cohort5['model12', c('n', 'n_cooccur2')]),
                                                    cohort6 = c(cohort6['model12', 'cohensd2'], cohort6['model12','SE2'], cohort6['model12', c('n', 'n_cooccur2')]),
                                                    cohort7 = c(cohort7['model12', 'cohensd2'], cohort7['model12','SE2'], cohort7['model12', c('n', 'n_cooccur2')]),
                                                    cohort8 = c(cohort8['model12', 'cohensd2'], cohort8['model12','SE2'], cohort8['model12', c('n', 'n_cooccur2')]),
                                                    cohort9 = c(cohort9['model12', 'cohensd2'], cohort9['model12','SE2'], cohort9['model12', c('n', 'n_cooccur2')]),
                                                    cohort10 = c(cohort10['model12', 'cohensd2'], cohort10['model12','SE2'], cohort10['model12', c('n', 'n_cooccur2')]),
                                                    cohort11 = c(cohort11['model12', 'cohensd2'], cohort11['model12','SE2'], cohort11['model12', c('n', 'n_cooccur2')])))

colnames(Model12_cooccur2_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur2')

Model12_cooccur3_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model12','cohensd3'], cohort1['model12','SE3'], cohort1['model12', c('n', 'n_cooccur3')]), 
                                                    cohort2 = c(cohort2['model12','cohensd3'], cohort2['model12','SE3'], cohort2['model12', c('n', 'n_cooccur3')]), 
                                                    cohort3 = c(cohort3['model12','cohensd3'], cohort3['model12','SE3'], cohort3['model12', c('n', 'n_cooccur3')]),
                                                    cohort4 = c(cohort4['model12', 'cohensd3'], cohort4['model12','SE3'], cohort4['model12', c('n', 'n_cooccur3')]),
                                                    cohort5 = c(cohort5['model12', 'cohensd3'], cohort5['model12','SE3'], cohort5['model12', c('n', 'n_cooccur3')]),
                                                    cohort6 = c(cohort6['model12', 'cohensd3'], cohort6['model12','SE3'], cohort6['model12', c('n', 'n_cooccur3')]),
                                                    cohort7 = c(cohort7['model12', 'cohensd3'], cohort7['model12','SE3'], cohort7['model12', c('n', 'n_cooccur3')]),
                                                    cohort8 = c(cohort8['model12', 'cohensd3'], cohort8['model12','SE3'], cohort8['model12', c('n', 'n_cooccur3')]),
                                                    cohort9 = c(cohort9['model12', 'cohensd3'], cohort9['model12','SE3'], cohort9['model12', c('n', 'n_cooccur3')]),
                                                    cohort10 = c(cohort10['model12', 'cohensd3'], cohort10['model12','SE3'], cohort10['model12', c('n', 'n_cooccur3')]),
                                                    cohort11 = c(cohort11['model12', 'cohensd3'], cohort11['model12','SE3'], cohort11['model12', c('n', 'n_cooccur3')])))
colnames(Model12_cooccur3_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur3')

cohort3['model12','n_cooccur0']

Model12_cooccur2_vs_cooccur1 <- as.data.frame(rbind(cohort1 = c(cohort1['model12','cohensd4'], cohort1['model12','SE4']), 
                                                    cohort2 = c(cohort2['model12','cohensd4'], cohort2['model12','SE4']), 
                                                    cohort3 = c(cohort3['model12','cohensd4'], cohort3['model12','SE4']),
                                                    cohort4 = c(cohort4['model12', 'cohensd4'], cohort4['model12','SE4']),
                                                    cohort5 = c(cohort5['model12', 'cohensd4'], cohort5['model12','SE4']),
                                                    cohort6 = c(cohort6['model12', 'cohensd4'], cohort6['model12','SE4']),
                                                    cohort7 = c(cohort7['model12', 'cohensd4'], cohort7['model12','SE4']),
                                                    cohort8 = c(cohort8['model12', 'cohensd4'], cohort8['model12','SE4']),
                                                    cohort9 = c(cohort9['model12', 'cohensd4'], cohort9['model12','SE4']),
                                                    cohort10 = c(cohort10['model12', 'cohensd4'], cohort10['model12','SE4']),
                                                    cohort11 = c(cohort11['model12', 'cohensd4'], cohort11['model12','SE4'])))
colnames(Model12_cooccur2_vs_cooccur1) <- c('cohensd', 'SE')

Model12_cooccur3_vs_cooccur1 <- as.data.frame(rbind(cohort1 = c(cohort1['model12','cohensd5'], cohort1['model12','SE5']), 
                                                    cohort2 = c(cohort2['model12','cohensd5'], cohort2['model12','SE5']), 
                                                    cohort3 = c(cohort3['model12','cohensd5'], cohort3['model12','SE5']),
                                                    cohort4 = c(cohort4['model12', 'cohensd5'], cohort4['model12','SE5']),
                                                    cohort5 = c(cohort5['model12', 'cohensd5'], cohort5['model12','SE5']),
                                                    cohort6 = c(cohort6['model12', 'cohensd5'], cohort6['model12','SE5']),
                                                    cohort7 = c(cohort7['model12', 'cohensd5'], cohort7['model12','SE5']),
                                                    cohort8 = c(cohort8['model12', 'cohensd5'], cohort8['model12','SE5']),
                                                    cohort9 = c(cohort9['model12', 'cohensd5'], cohort9['model12','SE5']),
                                                    cohort10 = c(cohort10['model12', 'cohensd5'], cohort10['model12','SE5']),
                                                    cohort11 = c(cohort11['model12', 'cohensd5'], cohort11['model12','SE5'])))
colnames(Model12_cooccur3_vs_cooccur1) <- c('cohensd', 'SE')

Model12_cooccur3_vs_cooccur2 <- as.data.frame(rbind(cohort1 = c(cohort1['model12','cohensd6'], cohort1['model12','SE6']), 
                                                    cohort2 = c(cohort2['model12','cohensd6'], cohort2['model12','SE6']), 
                                                    cohort3 = c(cohort3['model12','cohensd6'], cohort3['model12','SE6']),
                                                    cohort4 = c(cohort4['model12', 'cohensd6'], cohort4['model12','SE6']),
                                                    cohort5 = c(cohort5['model12', 'cohensd6'], cohort5['model12','SE6']),
                                                    cohort6 = c(cohort6['model12', 'cohensd6'], cohort6['model12','SE6']),
                                                    cohort7 = c(cohort7['model12', 'cohensd6'], cohort7['model12','SE6']),
                                                    cohort8 = c(cohort8['model12', 'cohensd6'], cohort8['model12','SE6']),
                                                    cohort9 = c(cohort9['model12', 'cohensd6'], cohort9['model12','SE6']),
                                                    cohort10 = c(cohort10['model12', 'cohensd6'], cohort10['model12','SE6']),
                                                    cohort11 = c(cohort11['model12', 'cohensd6'], cohort11['model12','SE6'])))
colnames(Model12_cooccur3_vs_cooccur2) <- c('cohensd', 'SE')



#################### 6. Models 13, 14, 15, and 16 - Effect sizes of association of cooccurrence of childhood maltreatment and depression with categorical metabolic outcomes#################### 
# Note: In the data synthesis, we only included the cohorts with >=5 cases per crosstab cell for models with categorical dependent variables. 
# For model 13, the only cohorts with 5 or more cases per crosstab cell were cohorts 1, 3, 7 and 10.

Model13_cooccur1_vs_cooccur0_bmi_cat1 <- as.data.frame(rbind(cohort1 = c(cohort1['model13','logOR_cooccur1_bmi_cat1'], cohort1['model13','SE_cooccur1_bmi_cat1'], cohort1['model13','CI_LL_logOR_cooccur1_bmi_cat1'],cohort1['model13','CI_UL_logOR_cooccur1_bmi_cat1'], cohort1_models[[17]][,c('n_cooccur0_bmi_cat1', 'n_cooccur0_bmi_cat2', 'n_cooccur0_bmi_cat3', 'n_cooccur0_bmi_cat4', 'n_cooccur1_bmi_cat1', 'n_cooccur1_bmi_cat2', 'n_cooccur1_bmi_cat3', 'n_cooccur1_bmi_cat4', 'n_cooccur2_bmi_cat1', 'n_cooccur2_bmi_cat2', 'n_cooccur2_bmi_cat3', 'n_cooccur2_bmi_cat4', 'n_cooccur3_bmi_cat1', 'n_cooccur3_bmi_cat2', 'n_cooccur3_bmi_cat3', 'n_cooccur3_bmi_cat4')]), 
                                                    cohort3 = c(cohort3['model13','logOR_cooccur1_bmi_cat1'], cohort3['model13','SE_cooccur1_bmi_cat1'], cohort3['model13','CI_LL_logOR_cooccur1_bmi_cat1'],cohort3['model13','CI_UL_logOR_cooccur1_bmi_cat1'], cohort3_models[[17]][, c('n_cooccur0_bmi_cat1', 'n_cooccur0_bmi_cat2', 'n_cooccur0_bmi_cat3', 'n_cooccur0_bmi_cat4', 'n_cooccur1_bmi_cat1', 'n_cooccur1_bmi_cat2', 'n_cooccur1_bmi_cat3', 'n_cooccur1_bmi_cat4', 'n_cooccur2_bmi_cat1', 'n_cooccur2_bmi_cat2', 'n_cooccur2_bmi_cat3', 'n_cooccur2_bmi_cat4', 'n_cooccur3_bmi_cat1', 'n_cooccur3_bmi_cat2', 'n_cooccur3_bmi_cat3', 'n_cooccur3_bmi_cat4')]),
                                                    cohort7 = c(cohort7['model13', 'logOR_cooccur1_bmi_cat1'], cohort7['model13','SE_cooccur1_bmi_cat1'], cohort7['model13','CI_LL_logOR_cooccur1_bmi_cat1'],cohort7['model13','CI_UL_logOR_cooccur1_bmi_cat1'], cohort7_models[[17]][, c('n_cooccur0_bmi_cat1', 'n_cooccur0_bmi_cat2', 'n_cooccur0_bmi_cat3', 'n_cooccur0_bmi_cat4', 'n_cooccur1_bmi_cat1', 'n_cooccur1_bmi_cat2', 'n_cooccur1_bmi_cat3', 'n_cooccur1_bmi_cat4', 'n_cooccur2_bmi_cat1', 'n_cooccur2_bmi_cat2', 'n_cooccur2_bmi_cat3', 'n_cooccur2_bmi_cat4', 'n_cooccur3_bmi_cat1', 'n_cooccur3_bmi_cat2', 'n_cooccur3_bmi_cat3', 'n_cooccur3_bmi_cat4')]),
                                                    cohort10 = c(cohort10['model13', 'logOR_cooccur1_bmi_cat1'], cohort10['model13','SE_cooccur1_bmi_cat1'], cohort10['model13','CI_LL_logOR_cooccur1_bmi_cat1'],cohort10['model13','CI_UL_logOR_cooccur1_bmi_cat1'], cohort10_models[[17]][, c('n_cooccur0_bmi_cat1', 'n_cooccur0_bmi_cat2', 'n_cooccur0_bmi_cat3', 'n_cooccur0_bmi_cat4', 'n_cooccur1_bmi_cat1', 'n_cooccur1_bmi_cat2', 'n_cooccur1_bmi_cat3', 'n_cooccur1_bmi_cat4', 'n_cooccur2_bmi_cat1', 'n_cooccur2_bmi_cat2', 'n_cooccur2_bmi_cat3', 'n_cooccur2_bmi_cat4', 'n_cooccur3_bmi_cat1', 'n_cooccur3_bmi_cat2', 'n_cooccur3_bmi_cat3', 'n_cooccur3_bmi_cat4')])))
colnames(Model13_cooccur1_vs_cooccur0_bmi_cat1) <- c('log_OR', 'SE', 'LL_log_OR', 'UL_log_OR', 'n_cooccur0_bmi_cat1', 'n_cooccur0_bmi_cat2', 'n_cooccur0_bmi_cat3', 'n_cooccur0_bmi_cat4', 'n_cooccur1_bmi_cat1', 'n_cooccur1_bmi_cat2', 'n_cooccur1_bmi_cat3', 'n_cooccur1_bmi_cat4', 'n_cooccur2_bmi_cat1', 'n_cooccur2_bmi_cat2', 'n_cooccur2_bmi_cat3', 'n_cooccur2_bmi_cat4', 'n_cooccur3_bmi_cat1', 'n_cooccur3_bmi_cat2', 'n_cooccur3_bmi_cat3', 'n_cooccur3_bmi_cat4')

Model13_cooccur2_vs_cooccur0_bmi_cat1 <- as.data.frame(rbind(cohort1 = c(cohort1['model13','logOR_cooccur2_bmi_cat1'], cohort1['model13','SE_cooccur2_bmi_cat1'], cohort1['model13','CI_LL_logOR_cooccur2_bmi_cat1'],cohort1['model13','CI_UL_logOR_cooccur2_bmi_cat1']), 
                                                            cohort3 = c(cohort3['model13','logOR_cooccur2_bmi_cat1'], cohort3['model13','SE_cooccur2_bmi_cat1'], cohort3['model13','CI_LL_logOR_cooccur2_bmi_cat1'],cohort3['model13','CI_UL_logOR_cooccur2_bmi_cat1']),
                                                            cohort7 = c(cohort7['model13', 'logOR_cooccur2_bmi_cat1'], cohort7['model13','SE_cooccur2_bmi_cat1'], cohort7['model13','CI_LL_logOR_cooccur2_bmi_cat1'],cohort7['model13','CI_UL_logOR_cooccur2_bmi_cat1']),
                                                            cohort10 = c(cohort10['model13', 'logOR_cooccur2_bmi_cat1'], cohort10['model13','SE_cooccur2_bmi_cat1'], cohort10['model13','CI_LL_logOR_cooccur2_bmi_cat1'],cohort10['model13','CI_UL_logOR_cooccur2_bmi_cat1'])))
colnames(Model13_cooccur2_vs_cooccur0_bmi_cat1) <- c('log_OR', 'SE', 'LL_log_OR', 'UL_log_OR')

Model13_cooccur3_vs_cooccur0_bmi_cat1 <- as.data.frame(rbind(cohort1 = c(cohort1['model13','logOR_cooccur3_bmi_cat1'], cohort1['model13','SE_cooccur3_bmi_cat1'], cohort1['model13','CI_LL_logOR_cooccur3_bmi_cat1'],cohort1['model13','CI_UL_logOR_cooccur3_bmi_cat1']), 
                                                            cohort3 = c(cohort3['model13','logOR_cooccur3_bmi_cat1'], cohort3['model13','SE_cooccur3_bmi_cat1'], cohort3['model13','CI_LL_logOR_cooccur3_bmi_cat1'],cohort3['model13','CI_UL_logOR_cooccur3_bmi_cat1']),
                                                            cohort7 = c(cohort7['model13', 'logOR_cooccur3_bmi_cat1'], cohort7['model13','SE_cooccur3_bmi_cat1'], cohort7['model13','CI_LL_logOR_cooccur3_bmi_cat1'],cohort7['model13','CI_UL_logOR_cooccur3_bmi_cat1']),
                                                            cohort10 = c(cohort10['model13', 'logOR_cooccur3_bmi_cat1'], cohort10['model13','SE_cooccur3_bmi_cat1'], cohort10['model13','CI_LL_logOR_cooccur3_bmi_cat1'],cohort10['model13','CI_UL_logOR_cooccur3_bmi_cat1'])))
colnames(Model13_cooccur3_vs_cooccur0_bmi_cat1) <- c('log_OR', 'SE', 'LL_log_OR', 'UL_log_OR')

Model13_cooccur1_vs_cooccur0_bmi_cat3 <- as.data.frame(rbind(cohort1 = c(cohort1['model13','logOR_cooccur1_bmi_cat3'], cohort1['model13','SE_cooccur1_bmi_cat3'], cohort1['model13','CI_LL_logOR_cooccur1_bmi_cat3'],cohort1['model13','CI_UL_logOR_cooccur1_bmi_cat3']), 
                                                            cohort3 = c(cohort3['model13','logOR_cooccur1_bmi_cat3'], cohort3['model13','SE_cooccur1_bmi_cat3'], cohort3['model13','CI_LL_logOR_cooccur1_bmi_cat3'],cohort3['model13','CI_UL_logOR_cooccur1_bmi_cat3']),
                                                            cohort7 = c(cohort7['model13', 'logOR_cooccur1_bmi_cat3'], cohort7['model13','SE_cooccur1_bmi_cat3'], cohort7['model13','CI_LL_logOR_cooccur1_bmi_cat3'],cohort7['model13','CI_UL_logOR_cooccur1_bmi_cat3']),
                                                            cohort10 = c(cohort10['model13', 'logOR_cooccur1_bmi_cat3'], cohort10['model13','SE_cooccur1_bmi_cat3'], cohort10['model13','CI_LL_logOR_cooccur1_bmi_cat3'],cohort10['model13','CI_UL_logOR_cooccur1_bmi_cat3'])))
colnames(Model13_cooccur1_vs_cooccur0_bmi_cat3) <- c('log_OR', 'SE', 'LL_log_OR', 'UL_log_OR')

Model13_cooccur2_vs_cooccur0_bmi_cat3 <- as.data.frame(rbind(cohort1 = c(cohort1['model13','logOR_cooccur2_bmi_cat3'], cohort1['model13','SE_cooccur2_bmi_cat3'], cohort1['model13','CI_LL_logOR_cooccur2_bmi_cat3'],cohort1['model13','CI_UL_logOR_cooccur2_bmi_cat3']), 
                                                            cohort3 = c(cohort3['model13','logOR_cooccur2_bmi_cat3'], cohort3['model13','SE_cooccur2_bmi_cat3'], cohort3['model13','CI_LL_logOR_cooccur2_bmi_cat3'],cohort3['model13','CI_UL_logOR_cooccur2_bmi_cat3']),
                                                            cohort7 = c(cohort7['model13', 'logOR_cooccur2_bmi_cat3'], cohort7['model13','SE_cooccur2_bmi_cat3'], cohort7['model13','CI_LL_logOR_cooccur2_bmi_cat3'],cohort7['model13','CI_UL_logOR_cooccur2_bmi_cat3']),
                                                            cohort10 = c(cohort10['model13', 'logOR_cooccur2_bmi_cat3'], cohort10['model13','SE_cooccur2_bmi_cat3'], cohort10['model13','CI_LL_logOR_cooccur2_bmi_cat3'],cohort10['model13','CI_UL_logOR_cooccur2_bmi_cat3'])))
colnames(Model13_cooccur2_vs_cooccur0_bmi_cat3) <- c('log_OR', 'SE', 'LL_log_OR', 'UL_log_OR')

Model13_cooccur3_vs_cooccur0_bmi_cat3 <- as.data.frame(rbind(cohort1 = c(cohort1['model13','logOR_cooccur3_bmi_cat3'], cohort1['model13','SE_cooccur3_bmi_cat3'], cohort1['model13','CI_LL_logOR_cooccur3_bmi_cat3'],cohort1['model13','CI_UL_logOR_cooccur3_bmi_cat3']), 
                                                            cohort3 = c(cohort3['model13','logOR_cooccur3_bmi_cat3'], cohort3['model13','SE_cooccur3_bmi_cat3'], cohort3['model13','CI_LL_logOR_cooccur3_bmi_cat3'],cohort3['model13','CI_UL_logOR_cooccur3_bmi_cat3']),
                                                            cohort7 = c(cohort7['model13', 'logOR_cooccur3_bmi_cat3'], cohort7['model13','SE_cooccur3_bmi_cat3'], cohort7['model13','CI_LL_logOR_cooccur3_bmi_cat3'],cohort7['model13','CI_UL_logOR_cooccur3_bmi_cat3']),
                                                            cohort10 = c(cohort10['model13', 'logOR_cooccur3_bmi_cat3'], cohort10['model13','SE_cooccur3_bmi_cat3'], cohort10['model13','CI_LL_logOR_cooccur3_bmi_cat3'],cohort10['model13','CI_UL_logOR_cooccur3_bmi_cat3'])))
colnames(Model13_cooccur3_vs_cooccur0_bmi_cat3) <- c('log_OR', 'SE', 'LL_log_OR', 'UL_log_OR')

Model13_cooccur1_vs_cooccur0_bmi_cat4 <- as.data.frame(rbind(cohort1 = c(cohort1['model13','logOR_cooccur1_bmi_cat4'], cohort1['model13','SE_cooccur1_bmi_cat4'], cohort1['model13','CI_LL_logOR_cooccur1_bmi_cat4'],cohort1['model13','CI_UL_logOR_cooccur1_bmi_cat4']), 
                                                            cohort3 = c(cohort3['model13','logOR_cooccur1_bmi_cat4'], cohort3['model13','SE_cooccur1_bmi_cat4'], cohort3['model13','CI_LL_logOR_cooccur1_bmi_cat4'],cohort3['model13','CI_UL_logOR_cooccur1_bmi_cat4']),
                                                            cohort7 = c(cohort7['model13', 'logOR_cooccur1_bmi_cat4'], cohort7['model13','SE_cooccur1_bmi_cat4'], cohort7['model13','CI_LL_logOR_cooccur1_bmi_cat4'],cohort7['model13','CI_UL_logOR_cooccur1_bmi_cat4']),
                                                            cohort10 = c(cohort10['model13', 'logOR_cooccur1_bmi_cat4'], cohort10['model13','SE_cooccur1_bmi_cat4'], cohort10['model13','CI_LL_logOR_cooccur1_bmi_cat4'],cohort10['model13','CI_UL_logOR_cooccur1_bmi_cat4'])))
colnames(Model13_cooccur1_vs_cooccur0_bmi_cat4) <- c('log_OR', 'SE', 'LL_log_OR', 'UL_log_OR')

Model13_cooccur2_vs_cooccur0_bmi_cat4 <- as.data.frame(rbind(cohort1 = c(cohort1['model13','logOR_cooccur2_bmi_cat4'], cohort1['model13','SE_cooccur2_bmi_cat4'], cohort1['model13','CI_LL_logOR_cooccur2_bmi_cat4'],cohort1['model13','CI_UL_logOR_cooccur2_bmi_cat4']), 
                                                            cohort3 = c(cohort3['model13','logOR_cooccur2_bmi_cat4'], cohort3['model13','SE_cooccur2_bmi_cat4'], cohort3['model13','CI_LL_logOR_cooccur2_bmi_cat4'],cohort3['model13','CI_UL_logOR_cooccur2_bmi_cat4']),
                                                            cohort7 = c(cohort7['model13', 'logOR_cooccur2_bmi_cat4'], cohort7['model13','SE_cooccur2_bmi_cat4'], cohort7['model13','CI_LL_logOR_cooccur2_bmi_cat4'],cohort7['model13','CI_UL_logOR_cooccur2_bmi_cat4']),
                                                            cohort10 = c(cohort10['model13', 'logOR_cooccur2_bmi_cat4'], cohort10['model13','SE_cooccur2_bmi_cat4'], cohort10['model13','CI_LL_logOR_cooccur2_bmi_cat4'],cohort10['model13','CI_UL_logOR_cooccur2_bmi_cat4'])))
colnames(Model13_cooccur2_vs_cooccur0_bmi_cat4) <- c('log_OR', 'SE', 'LL_log_OR', 'UL_log_OR')

Model13_cooccur3_vs_cooccur0_bmi_cat4 <- as.data.frame(rbind(cohort1 = c(cohort1['model13','logOR_cooccur3_bmi_cat4'], cohort1['model13','SE_cooccur3_bmi_cat4'], cohort1['model13','CI_LL_logOR_cooccur3_bmi_cat4'],cohort1['model13','CI_UL_logOR_cooccur3_bmi_cat4']), 
                                                            cohort3 = c(cohort3['model13','logOR_cooccur3_bmi_cat4'], cohort3['model13','SE_cooccur3_bmi_cat4'], cohort3['model13','CI_LL_logOR_cooccur3_bmi_cat4'],cohort3['model13','CI_UL_logOR_cooccur3_bmi_cat4']),
                                                            cohort7 = c(cohort7['model13', 'logOR_cooccur3_bmi_cat4'], cohort7['model13','SE_cooccur3_bmi_cat4'], cohort7['model13','CI_LL_logOR_cooccur3_bmi_cat4'],cohort7['model13','CI_UL_logOR_cooccur3_bmi_cat4']),
                                                            cohort10 = c(cohort10['model13', 'logOR_cooccur3_bmi_cat4'], cohort10['model13','SE_cooccur3_bmi_cat4'], cohort10['model13','CI_LL_logOR_cooccur3_bmi_cat4'],cohort10['model13','CI_UL_logOR_cooccur3_bmi_cat4'])))
colnames(Model13_cooccur3_vs_cooccur0_bmi_cat4) <- c('log_OR', 'SE', 'LL_log_OR', 'UL_log_OR')

# Note: In the data synthesis, we only included the cohorts with >=5 cases per crosstab cell for models with categorical dependent variables. 
# For model 14, cohorts 6 and 9 were therefore excluded.
Model14_cooccur1_vs_cooccur0_outcome_dic <- as.data.frame(rbind(cohort1 = c(cohort1['model14','logOR_cooccur1_outcome_dic'], cohort1['model14','SE_cooccur1_outcome_dic'], cohort1['model14','CI_LL_logOR_cooccur1_outcome_dic'],cohort1['model14','CI_UL_logOR_cooccur1_outcome_dic'], cohort1_models[[18]][,c('n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')]), 
                                                            cohort2 = c(cohort2['model14','logOR_cooccur1_outcome_dic'], cohort2['model14','SE_cooccur1_outcome_dic'], cohort2['model14','CI_LL_logOR_cooccur1_outcome_dic'],cohort2['model14','CI_UL_logOR_cooccur1_outcome_dic'], cohort2_models[[18]][,c('n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')]), 
                                                            cohort3 = c(cohort3['model14','logOR_cooccur1_outcome_dic'], cohort3['model14','SE_cooccur1_outcome_dic'], cohort3['model14','CI_LL_logOR_cooccur1_outcome_dic'],cohort3['model14','CI_UL_logOR_cooccur1_outcome_dic'], cohort3_models[[18]][,c('n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')]),
                                                            cohort4 = c(cohort4['model14', 'logOR_cooccur1_outcome_dic'], cohort4['model14','SE_cooccur1_outcome_dic'], cohort4['model14','CI_LL_logOR_cooccur1_outcome_dic'],cohort4['model14','CI_UL_logOR_cooccur1_outcome_dic'], cohort4_models[[18]][,c('n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')]),
                                                            cohort5 = c(cohort5['model14', 'logOR_cooccur1_outcome_dic'], cohort5['model14','SE_cooccur1_outcome_dic'], cohort5['model14','CI_LL_logOR_cooccur1_outcome_dic'],cohort5['model14','CI_UL_logOR_cooccur1_outcome_dic'], cohort5_models[[18]][,c('n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')]),
                                                            cohort7 = c(cohort7['model14', 'logOR_cooccur1_outcome_dic'], cohort7['model14','SE_cooccur1_outcome_dic'], cohort7['model14','CI_LL_logOR_cooccur1_outcome_dic'], cohort7['model14','CI_UL_logOR_cooccur1_outcome_dic'], cohort7_models[[18]][,c('n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')]),
                                                            cohort8 = c(cohort8['model14', 'logOR_cooccur1_outcome_dic'], cohort8['model14','SE_cooccur1_outcome_dic'], cohort8['model14','CI_LL_logOR_cooccur1_outcome_dic'], cohort8['model14','CI_UL_logOR_cooccur1_outcome_dic'], cohort8_models[[18]][,c('n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')]),
                                                            cohort10 = c(cohort10['model14', 'logOR_cooccur1_outcome_dic'], cohort10['model14','SE_cooccur1_outcome_dic'], cohort10['model14','CI_LL_logOR_cooccur1_outcome_dic'],cohort10['model14','CI_UL_logOR_cooccur1_outcome_dic'], cohort10_models[[18]][,c('n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')]),
                                                            cohort11 = c(cohort11['model14', 'logOR_cooccur1_outcome_dic'], cohort11['model14','SE_cooccur1_outcome_dic'], cohort11['model14','CI_LL_logOR_cooccur1_outcome_dic'],cohort11['model14','CI_UL_logOR_cooccur1_outcome_dic'], cohort11_models[[18]][,c('n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')])))

colnames(Model14_cooccur1_vs_cooccur0_outcome_dic) <- c('log_OR', 'SE', 'LL_log_OR', 'UL_log_OR', 'n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')

Model14_cooccur2_vs_cooccur0_outcome_dic <- as.data.frame(rbind(cohort1 = c(cohort1['model14','logOR_cooccur2_outcome_dic'], cohort1['model14','SE_cooccur2_outcome_dic'], cohort1['model14','CI_LL_logOR_cooccur2_outcome_dic'],cohort1['model14','CI_UL_logOR_cooccur2_outcome_dic'], cohort1['model14', c('n', 'n_cooccur2')]), 
                                                                cohort2 = c(cohort2['model14','logOR_cooccur2_outcome_dic'], cohort2['model14','SE_cooccur2_outcome_dic'], cohort2['model14','CI_LL_logOR_cooccur2_outcome_dic'],cohort2['model14','CI_UL_logOR_cooccur2_outcome_dic'], cohort2['model14', c('n', 'n_cooccur2')]), 
                                                                cohort3 = c(cohort3['model14','logOR_cooccur2_outcome_dic'], cohort3['model14','SE_cooccur2_outcome_dic'], cohort3['model14','CI_LL_logOR_cooccur2_outcome_dic'],cohort3['model14','CI_UL_logOR_cooccur2_outcome_dic'], cohort3['model14', c('n', 'n_cooccur2')]),
                                                                cohort4 = c(cohort4['model14', 'logOR_cooccur2_outcome_dic'], cohort4['model14','SE_cooccur2_outcome_dic'], cohort4['model14','CI_LL_logOR_cooccur2_outcome_dic'],cohort4['model14','CI_UL_logOR_cooccur2_outcome_dic'], cohort4['model14', c('n', 'n_cooccur2')]),
                                                                cohort5 = c(cohort5['model14', 'logOR_cooccur2_outcome_dic'], cohort5['model14','SE_cooccur2_outcome_dic'], cohort5['model14','CI_LL_logOR_cooccur2_outcome_dic'],cohort5['model14','CI_UL_logOR_cooccur2_outcome_dic'], cohort5['model14', c('n', 'n_cooccur2')]),
                                                                cohort7 = c(cohort7['model14', 'logOR_cooccur2_outcome_dic'], cohort7['model14','SE_cooccur2_outcome_dic'], cohort7['model14','CI_LL_logOR_cooccur2_outcome_dic'], cohort7['model14','CI_UL_logOR_cooccur2_outcome_dic'], cohort7['model14', c('n', 'n_cooccur2')]),
                                                                cohort8 = c(cohort8['model14', 'logOR_cooccur2_outcome_dic'], cohort8['model14','SE_cooccur2_outcome_dic'], cohort8['model14','CI_LL_logOR_cooccur2_outcome_dic'], cohort8['model14','CI_UL_logOR_cooccur2_outcome_dic'], cohort8['model14', c('n', 'n_cooccur2')]),
                                                                cohort10 = c(cohort10['model14', 'logOR_cooccur2_outcome_dic'], cohort10['model14','SE_cooccur2_outcome_dic'], cohort10['model14','CI_LL_logOR_cooccur2_outcome_dic'],cohort10['model14','CI_UL_logOR_cooccur2_outcome_dic'], cohort10['model14', c('n', 'n_cooccur2')]),
                                                                cohort11 = c(cohort11['model14', 'logOR_cooccur2_outcome_dic'], cohort11['model14','SE_cooccur2_outcome_dic'], cohort11['model14','CI_LL_logOR_cooccur2_outcome_dic'],cohort11['model14','CI_UL_logOR_cooccur2_outcome_dic'], cohort11['model14', c('n', 'n_cooccur2')])))

colnames(Model14_cooccur2_vs_cooccur0_outcome_dic) <- c('log_OR', 'SE', 'LL_log_OR', 'UL_log_OR', 'n', 'n_cooccur2')

Model14_cooccur3_vs_cooccur0_outcome_dic <- as.data.frame(rbind(cohort1 = c(cohort1['model14','logOR_cooccur3_outcome_dic'], cohort1['model14','SE_cooccur3_outcome_dic'], cohort1['model14','CI_LL_logOR_cooccur3_outcome_dic'],cohort1['model14','CI_UL_logOR_cooccur3_outcome_dic'], cohort1['model14', c('n', 'n_cooccur3')]), 
                                                                cohort2 = c(cohort2['model14','logOR_cooccur3_outcome_dic'], cohort2['model14','SE_cooccur3_outcome_dic'], cohort2['model14','CI_LL_logOR_cooccur3_outcome_dic'],cohort2['model14','CI_UL_logOR_cooccur3_outcome_dic'], cohort2['model14', c('n', 'n_cooccur3')]), 
                                                                cohort3 = c(cohort3['model14','logOR_cooccur3_outcome_dic'], cohort3['model14','SE_cooccur3_outcome_dic'], cohort3['model14','CI_LL_logOR_cooccur3_outcome_dic'],cohort3['model14','CI_UL_logOR_cooccur3_outcome_dic'], cohort3['model14', c('n', 'n_cooccur3')]),
                                                                cohort4 = c(cohort4['model14', 'logOR_cooccur3_outcome_dic'], cohort4['model14','SE_cooccur3_outcome_dic'], cohort4['model14','CI_LL_logOR_cooccur3_outcome_dic'],cohort4['model14','CI_UL_logOR_cooccur3_outcome_dic'], cohort4['model14', c('n', 'n_cooccur3')]),
                                                                cohort5 = c(cohort5['model14', 'logOR_cooccur3_outcome_dic'], cohort5['model14','SE_cooccur3_outcome_dic'], cohort5['model14','CI_LL_logOR_cooccur3_outcome_dic'],cohort5['model14','CI_UL_logOR_cooccur3_outcome_dic'], cohort5['model14', c('n', 'n_cooccur3')]),
                                                                cohort7 = c(cohort7['model14', 'logOR_cooccur3_outcome_dic'], cohort7['model14','SE_cooccur3_outcome_dic'], cohort7['model14','CI_LL_logOR_cooccur3_outcome_dic'], cohort7['model14','CI_UL_logOR_cooccur3_outcome_dic'], cohort7['model14', c('n', 'n_cooccur3')]),
                                                                cohort8 = c(cohort8['model14', 'logOR_cooccur3_outcome_dic'], cohort8['model14','SE_cooccur3_outcome_dic'], cohort8['model14','CI_LL_logOR_cooccur3_outcome_dic'], cohort8['model14','CI_UL_logOR_cooccur3_outcome_dic'], cohort8['model14', c('n', 'n_cooccur3')]),
                                                                cohort10 = c(cohort10['model14', 'logOR_cooccur3_outcome_dic'], cohort10['model14','SE_cooccur3_outcome_dic'], cohort10['model14','CI_LL_logOR_cooccur3_outcome_dic'],cohort10['model14','CI_UL_logOR_cooccur3_outcome_dic'], cohort10['model14', c('n', 'n_cooccur3')]),
                                                                cohort11 = c(cohort11['model14', 'logOR_cooccur3_outcome_dic'], cohort11['model14','SE_cooccur3_outcome_dic'], cohort11['model14','CI_LL_logOR_cooccur3_outcome_dic'],cohort11['model14','CI_UL_logOR_cooccur3_outcome_dic'], cohort11['model14', c('n', 'n_cooccur3')])))

colnames(Model14_cooccur3_vs_cooccur0_outcome_dic) <- c('log_OR', 'SE', 'LL_log_OR', 'UL_log_OR', 'n', 'n_cooccur3')

# Note: In the data synthesis, we only included the cohorts with >=5 cases per crosstab cell for models with categorical dependent variables. 
# For model 15, cohorts 2 and 6 were therefore excluded.
Model15_cooccur1_vs_cooccur0_outcome_dic <- as.data.frame(rbind(cohort1 = c(cohort1['model15','logOR_cooccur1_outcome_dic'], cohort1['model15','SE_cooccur1_outcome_dic'], cohort1['model15','CI_LL_logOR_cooccur1_outcome_dic'],cohort1['model15','CI_UL_logOR_cooccur1_outcome_dic'], cohort1_models[[19]][,c('n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')]), 
                                                                cohort3 = c(cohort3['model15','logOR_cooccur1_outcome_dic'], cohort3['model15','SE_cooccur1_outcome_dic'], cohort3['model15','CI_LL_logOR_cooccur1_outcome_dic'],cohort3['model15','CI_UL_logOR_cooccur1_outcome_dic'], cohort3_models[[19]][,c('n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')]),
                                                                cohort4 = c(cohort4['model15', 'logOR_cooccur1_outcome_dic'], cohort4['model15','SE_cooccur1_outcome_dic'], cohort4['model15','CI_LL_logOR_cooccur1_outcome_dic'],cohort4['model15','CI_UL_logOR_cooccur1_outcome_dic'], cohort4_models[[19]][,c('n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')]),
                                                                cohort5 = c(cohort5['model15', 'logOR_cooccur1_outcome_dic'], cohort5['model15','SE_cooccur1_outcome_dic'], cohort5['model15','CI_LL_logOR_cooccur1_outcome_dic'],cohort5['model15','CI_UL_logOR_cooccur1_outcome_dic'], cohort5_models[[19]][,c('n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')]),
                                                                cohort7 = c(cohort7['model15', 'logOR_cooccur1_outcome_dic'], cohort7['model15','SE_cooccur1_outcome_dic'], cohort7['model15','CI_LL_logOR_cooccur1_outcome_dic'], cohort7['model15','CI_UL_logOR_cooccur1_outcome_dic'], cohort7_models[[19]][,c('n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')]),
                                                                cohort8 = c(cohort8['model15', 'logOR_cooccur1_outcome_dic'], cohort8['model15','SE_cooccur1_outcome_dic'], cohort8['model15','CI_LL_logOR_cooccur1_outcome_dic'], cohort8['model15','CI_UL_logOR_cooccur1_outcome_dic'], cohort8_models[[19]][,c('n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')]),
                                                                cohort9 = c(cohort9['model15', 'logOR_cooccur1_outcome_dic'], cohort9['model15','SE_cooccur1_outcome_dic'], cohort9['model15','CI_LL_logOR_cooccur1_outcome_dic'], cohort9['model15','CI_UL_logOR_cooccur1_outcome_dic'], cohort9_models[[19]][,c('n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')]),
                                                                cohort10 = c(cohort10['model15', 'logOR_cooccur1_outcome_dic'], cohort10['model15','SE_cooccur1_outcome_dic'], cohort10['model15','CI_LL_logOR_cooccur1_outcome_dic'],cohort10['model15','CI_UL_logOR_cooccur1_outcome_dic'], cohort10_models[[19]][,c('n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')]),
                                                                cohort11 = c(cohort11['model15', 'logOR_cooccur1_outcome_dic'], cohort11['model15','SE_cooccur1_outcome_dic'], cohort11['model15','CI_LL_logOR_cooccur1_outcome_dic'],cohort11['model15','CI_UL_logOR_cooccur1_outcome_dic'], cohort11_models[[19]][,c('n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')])))

colnames(Model15_cooccur1_vs_cooccur0_outcome_dic) <- c('log_OR', 'SE', 'LL_log_OR', 'UL_log_OR', 'n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')

Model15_cooccur2_vs_cooccur0_outcome_dic <- as.data.frame(rbind(cohort1 = c(cohort1['model15','logOR_cooccur2_outcome_dic'], cohort1['model15','SE_cooccur2_outcome_dic'], cohort1['model15','CI_LL_logOR_cooccur2_outcome_dic'],cohort1['model15','CI_UL_logOR_cooccur2_outcome_dic'], cohort1['model15', c('n', 'n_cooccur2')]), 
                                                                cohort3 = c(cohort3['model15','logOR_cooccur2_outcome_dic'], cohort3['model15','SE_cooccur2_outcome_dic'], cohort3['model15','CI_LL_logOR_cooccur2_outcome_dic'],cohort3['model15','CI_UL_logOR_cooccur2_outcome_dic'], cohort3['model15', c('n', 'n_cooccur2')]),
                                                                cohort4 = c(cohort4['model15', 'logOR_cooccur2_outcome_dic'], cohort4['model15','SE_cooccur2_outcome_dic'], cohort4['model15','CI_LL_logOR_cooccur2_outcome_dic'],cohort4['model15','CI_UL_logOR_cooccur2_outcome_dic'], cohort4['model15', c('n', 'n_cooccur2')]),
                                                                cohort5 = c(cohort5['model15', 'logOR_cooccur2_outcome_dic'], cohort5['model15','SE_cooccur2_outcome_dic'], cohort5['model15','CI_LL_logOR_cooccur2_outcome_dic'],cohort5['model15','CI_UL_logOR_cooccur2_outcome_dic'], cohort5['model15', c('n', 'n_cooccur2')]),
                                                                cohort7 = c(cohort7['model15', 'logOR_cooccur2_outcome_dic'], cohort7['model15','SE_cooccur2_outcome_dic'], cohort7['model15','CI_LL_logOR_cooccur2_outcome_dic'], cohort7['model15','CI_UL_logOR_cooccur2_outcome_dic'], cohort7['model15', c('n', 'n_cooccur2')]),
                                                                cohort8 = c(cohort8['model15', 'logOR_cooccur2_outcome_dic'], cohort8['model15','SE_cooccur2_outcome_dic'], cohort8['model15','CI_LL_logOR_cooccur2_outcome_dic'], cohort8['model15','CI_UL_logOR_cooccur2_outcome_dic'], cohort8['model15', c('n', 'n_cooccur2')]),
                                                                cohort9 = c(cohort9['model15', 'logOR_cooccur2_outcome_dic'], cohort9['model15','SE_cooccur2_outcome_dic'], cohort9['model15','CI_LL_logOR_cooccur2_outcome_dic'], cohort9['model15','CI_UL_logOR_cooccur2_outcome_dic'], cohort9['model15', c('n', 'n_cooccur2')]),
                                                                cohort10 = c(cohort10['model15', 'logOR_cooccur2_outcome_dic'], cohort10['model15','SE_cooccur2_outcome_dic'], cohort10['model15','CI_LL_logOR_cooccur2_outcome_dic'],cohort10['model15','CI_UL_logOR_cooccur2_outcome_dic'], cohort10['model15', c('n', 'n_cooccur2')]),
                                                                cohort11 = c(cohort11['model15', 'logOR_cooccur2_outcome_dic'], cohort11['model15','SE_cooccur2_outcome_dic'], cohort11['model15','CI_LL_logOR_cooccur2_outcome_dic'],cohort11['model15','CI_UL_logOR_cooccur2_outcome_dic'], cohort11['model15', c('n', 'n_cooccur2')])))

colnames(Model15_cooccur2_vs_cooccur0_outcome_dic) <- c('log_OR', 'SE', 'LL_log_OR', 'UL_log_OR', 'n', 'n_cooccur2')

Model15_cooccur3_vs_cooccur0_outcome_dic <- as.data.frame(rbind(cohort1 = c(cohort1['model15','logOR_cooccur3_outcome_dic'], cohort1['model15','SE_cooccur3_outcome_dic'], cohort1['model15','CI_LL_logOR_cooccur3_outcome_dic'],cohort1['model15','CI_UL_logOR_cooccur3_outcome_dic'], cohort1['model15', c('n', 'n_cooccur3')]), 
                                                                cohort3 = c(cohort3['model15','logOR_cooccur3_outcome_dic'], cohort3['model15','SE_cooccur3_outcome_dic'], cohort3['model15','CI_LL_logOR_cooccur3_outcome_dic'],cohort3['model15','CI_UL_logOR_cooccur3_outcome_dic'], cohort3['model15', c('n', 'n_cooccur3')]),
                                                                cohort4 = c(cohort4['model15', 'logOR_cooccur3_outcome_dic'], cohort4['model15','SE_cooccur3_outcome_dic'], cohort4['model15','CI_LL_logOR_cooccur3_outcome_dic'],cohort4['model15','CI_UL_logOR_cooccur3_outcome_dic'], cohort4['model15', c('n', 'n_cooccur3')]),
                                                                cohort5 = c(cohort5['model15', 'logOR_cooccur3_outcome_dic'], cohort5['model15','SE_cooccur3_outcome_dic'], cohort5['model15','CI_LL_logOR_cooccur3_outcome_dic'],cohort5['model15','CI_UL_logOR_cooccur3_outcome_dic'], cohort5['model15', c('n', 'n_cooccur3')]),
                                                                cohort7 = c(cohort7['model15', 'logOR_cooccur3_outcome_dic'], cohort7['model15','SE_cooccur3_outcome_dic'], cohort7['model15','CI_LL_logOR_cooccur3_outcome_dic'], cohort7['model15','CI_UL_logOR_cooccur3_outcome_dic'], cohort7['model15', c('n', 'n_cooccur3')]),
                                                                cohort8 = c(cohort8['model15', 'logOR_cooccur3_outcome_dic'], cohort8['model15','SE_cooccur3_outcome_dic'], cohort8['model15','CI_LL_logOR_cooccur3_outcome_dic'], cohort8['model15','CI_UL_logOR_cooccur3_outcome_dic'], cohort8['model15', c('n', 'n_cooccur3')]),
                                                                cohort9 = c(cohort9['model15', 'logOR_cooccur3_outcome_dic'], cohort9['model15','SE_cooccur3_outcome_dic'], cohort9['model15','CI_LL_logOR_cooccur3_outcome_dic'], cohort9['model15','CI_UL_logOR_cooccur3_outcome_dic'], cohort9['model15', c('n', 'n_cooccur3')]),
                                                                cohort10 = c(cohort10['model15', 'logOR_cooccur3_outcome_dic'], cohort10['model15','SE_cooccur3_outcome_dic'], cohort10['model15','CI_LL_logOR_cooccur3_outcome_dic'],cohort10['model15','CI_UL_logOR_cooccur3_outcome_dic'], cohort10['model15', c('n', 'n_cooccur3')]),
                                                                cohort11 = c(cohort11['model15', 'logOR_cooccur3_outcome_dic'], cohort11['model15','SE_cooccur3_outcome_dic'], cohort11['model15','CI_LL_logOR_cooccur3_outcome_dic'],cohort11['model15','CI_UL_logOR_cooccur3_outcome_dic'], cohort11['model15', c('n', 'n_cooccur3')])))

colnames(Model15_cooccur3_vs_cooccur0_outcome_dic) <- c('log_OR', 'SE', 'LL_log_OR', 'UL_log_OR', 'n', 'n_cooccur3')

# Note: In the data synthesis, we only included the cohorts with >=5 cases per crosstab cell for models with categorical dependent variables. 
# For model 16, cohorts 2, 6, 7, 9 and 11 were therefore excluded.

Model16_cooccur1_vs_cooccur0_outcome_dic <- as.data.frame(rbind(cohort1 = c(cohort1['model16','logOR_cooccur1_outcome_dic'], cohort1['model16','SE_cooccur1_outcome_dic'], cohort1['model16','CI_LL_logOR_cooccur1_outcome_dic'],cohort1['model16','CI_UL_logOR_cooccur1_outcome_dic'], cohort1_models[[20]][,c('n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')]), 
                                                                cohort3 = c(cohort3['model16','logOR_cooccur1_outcome_dic'], cohort3['model16','SE_cooccur1_outcome_dic'], cohort3['model16','CI_LL_logOR_cooccur1_outcome_dic'],cohort3['model16','CI_UL_logOR_cooccur1_outcome_dic'], cohort3_models[[20]][,c('n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')]),
                                                                cohort4 = c(cohort4['model16', 'logOR_cooccur1_outcome_dic'], cohort4['model16','SE_cooccur1_outcome_dic'], cohort4['model16','CI_LL_logOR_cooccur1_outcome_dic'],cohort4['model16','CI_UL_logOR_cooccur1_outcome_dic'], cohort4_models[[20]][,c('n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')]),
                                                                cohort5 = c(cohort5['model16', 'logOR_cooccur1_outcome_dic'], cohort5['model16','SE_cooccur1_outcome_dic'], cohort5['model16','CI_LL_logOR_cooccur1_outcome_dic'],cohort5['model16','CI_UL_logOR_cooccur1_outcome_dic'], cohort5_models[[20]][,c('n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')]),
                                                                cohort8 = c(cohort8['model16', 'logOR_cooccur1_outcome_dic'], cohort8['model16','SE_cooccur1_outcome_dic'], cohort8['model16','CI_LL_logOR_cooccur1_outcome_dic'], cohort8['model16','CI_UL_logOR_cooccur1_outcome_dic'], cohort8_models[[20]][,c('n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')]),
                                                                cohort10 = c(cohort10['model16', 'logOR_cooccur1_outcome_dic'], cohort10['model16','SE_cooccur1_outcome_dic'], cohort10['model16','CI_LL_logOR_cooccur1_outcome_dic'],cohort10['model16','CI_UL_logOR_cooccur1_outcome_dic'], cohort10_models[[20]][,c('n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')])))
colnames(Model16_cooccur1_vs_cooccur0_outcome_dic) <- c('log_OR', 'SE', 'LL_log_OR', 'UL_log_OR', 'n_cooccur0_outcome_dic0', 'n_cooccur0_outcome_dic1', 'n_cooccur1_outcome_dic0', 'n_cooccur1_outcome_dic1', 'n_cooccur2_outcome_dic0', 'n_cooccur2_outcome_dic1', 'n_cooccur3_outcome_dic0', 'n_cooccur3_outcome_dic1')

Model16_cooccur2_vs_cooccur0_outcome_dic <- as.data.frame(rbind(cohort1 = c(cohort1['model16','logOR_cooccur2_outcome_dic'], cohort1['model16','SE_cooccur2_outcome_dic'], cohort1['model16','CI_LL_logOR_cooccur2_outcome_dic'],cohort1['model16','CI_UL_logOR_cooccur2_outcome_dic'], cohort1['model16', c('n', 'n_cooccur2')]), 
                                                                cohort3 = c(cohort3['model16','logOR_cooccur2_outcome_dic'], cohort3['model16','SE_cooccur2_outcome_dic'], cohort3['model16','CI_LL_logOR_cooccur2_outcome_dic'],cohort3['model16','CI_UL_logOR_cooccur2_outcome_dic'], cohort3['model16', c('n', 'n_cooccur2')]),
                                                                cohort4 = c(cohort4['model16', 'logOR_cooccur2_outcome_dic'], cohort4['model16','SE_cooccur2_outcome_dic'], cohort4['model16','CI_LL_logOR_cooccur2_outcome_dic'],cohort4['model16','CI_UL_logOR_cooccur2_outcome_dic'], cohort4['model16', c('n', 'n_cooccur2')]),
                                                                cohort5 = c(cohort5['model16', 'logOR_cooccur2_outcome_dic'], cohort5['model16','SE_cooccur2_outcome_dic'], cohort5['model16','CI_LL_logOR_cooccur2_outcome_dic'],cohort5['model16','CI_UL_logOR_cooccur2_outcome_dic'], cohort5['model16', c('n', 'n_cooccur2')]),
                                                                cohort8 = c(cohort8['model16', 'logOR_cooccur2_outcome_dic'], cohort8['model16','SE_cooccur2_outcome_dic'], cohort8['model16','CI_LL_logOR_cooccur2_outcome_dic'], cohort8['model16','CI_UL_logOR_cooccur2_outcome_dic'], cohort8['model16', c('n', 'n_cooccur2')]),
                                                                cohort10 = c(cohort10['model16', 'logOR_cooccur2_outcome_dic'], cohort10['model16','SE_cooccur2_outcome_dic'], cohort10['model16','CI_LL_logOR_cooccur2_outcome_dic'],cohort10['model16','CI_UL_logOR_cooccur2_outcome_dic'], cohort10['model16', c('n', 'n_cooccur2')])))
colnames(Model16_cooccur2_vs_cooccur0_outcome_dic) <- c('log_OR', 'SE', 'LL_log_OR', 'UL_log_OR', 'n', 'n_cooccur2')

Model16_cooccur3_vs_cooccur0_outcome_dic <- as.data.frame(rbind(cohort1 = c(cohort1['model16','logOR_cooccur3_outcome_dic'], cohort1['model16','SE_cooccur3_outcome_dic'], cohort1['model16','CI_LL_logOR_cooccur3_outcome_dic'],cohort1['model16','CI_UL_logOR_cooccur3_outcome_dic'], cohort1['model16', c('n', 'n_cooccur3')]), 
                                                                cohort3 = c(cohort3['model16','logOR_cooccur3_outcome_dic'], cohort3['model16','SE_cooccur3_outcome_dic'], cohort3['model16','CI_LL_logOR_cooccur3_outcome_dic'],cohort3['model16','CI_UL_logOR_cooccur3_outcome_dic'], cohort3['model16', c('n', 'n_cooccur3')]),
                                                                cohort4 = c(cohort4['model16', 'logOR_cooccur3_outcome_dic'], cohort4['model16','SE_cooccur3_outcome_dic'], cohort4['model16','CI_LL_logOR_cooccur3_outcome_dic'],cohort4['model16','CI_UL_logOR_cooccur3_outcome_dic'], cohort4['model16', c('n', 'n_cooccur3')]),
                                                                cohort5 = c(cohort5['model16', 'logOR_cooccur3_outcome_dic'], cohort5['model16','SE_cooccur3_outcome_dic'], cohort5['model16','CI_LL_logOR_cooccur3_outcome_dic'],cohort5['model16','CI_UL_logOR_cooccur3_outcome_dic'], cohort5['model16', c('n', 'n_cooccur3')]),
                                                                cohort8 = c(cohort8['model16', 'logOR_cooccur3_outcome_dic'], cohort8['model16','SE_cooccur3_outcome_dic'], cohort8['model16','CI_LL_logOR_cooccur3_outcome_dic'], cohort8['model16','CI_UL_logOR_cooccur3_outcome_dic'], cohort8['model16', c('n', 'n_cooccur3')]),
                                                                cohort10 = c(cohort10['model16', 'logOR_cooccur3_outcome_dic'], cohort10['model16','SE_cooccur3_outcome_dic'], cohort10['model16','CI_LL_logOR_cooccur3_outcome_dic'],cohort10['model16','CI_UL_logOR_cooccur3_outcome_dic'], cohort10['model16', c('n', 'n_cooccur3')])))
colnames(Model16_cooccur3_vs_cooccur0_outcome_dic) <- c('log_OR', 'SE', 'LL_log_OR', 'UL_log_OR', 'n', 'n_cooccur3')



#################### 7. Models 17, 18, 19, and 20 - Effect sizes of association of cooccurrence of childhood maltreatment and depression with metabolic outcomes, adjusting for lifestyle covariates ###### 

Model17_cooccur1_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model17','cohensd1'], cohort1['model17','SE1'], cohort1['model17', c('n', 'n_cooccur1')]), 
                                                    cohort2 = c(cohort2['model17','cohensd1'], cohort2['model17','SE1'], cohort2['model17', c('n', 'n_cooccur1')]), 
                                                    cohort3 = c(cohort3['model17','cohensd1'], cohort3['model17','SE1'], cohort3['model17', c('n', 'n_cooccur1')]),
                                                    cohort4 = c(cohort4['model17', 'cohensd1'], cohort4['model17','SE1'], cohort4['model17', c('n', 'n_cooccur1')]),
                                                    cohort5 = c(cohort5['model17', 'cohensd1'], cohort5['model17','SE1'], cohort5['model17', c('n', 'n_cooccur1')]),
                                                    cohort6 = c(cohort6['model17', 'cohensd1'], cohort6['model17','SE1'], cohort6['model17', c('N', 'n_cooccur1')]),
                                                    cohort7 = c(cohort7['model17', 'cohensd1'], cohort7['model17','SE1'], cohort7['model17', c('n', 'n_cooccur1')]),
                                                    cohort8 = c(cohort8['model17', 'cohensd1'], cohort8['model17','SE1'], cohort8['model17', c('n', 'n_cooccur1')]),
                                                    cohort9 = c(cohort9['model17', 'cohensd1'], cohort9['model17','SE1'], cohort9['model17', c('n', 'n_cooccur1')]),
                                                    cohort10 = c(cohort10['model17', 'cohensd1'], cohort10['model17','SE1'], cohort10['model17', c('n', 'n_cooccur1')]),
                                                    cohort11 = c(cohort11['model17', 'cohensd1'], cohort11['model17','SE1'], cohort11['model17', c('n', 'n_cooccur1')])))
colnames(Model17_cooccur1_vs_cooccur0) <- c('cohensd', 'SE', 'n', 'n_cooccur1')

Model17_cooccur2_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model17','cohensd2'], cohort1['model17','SE2'], cohort1['model17', c('n', 'n_cooccur2')]), 
                                                    cohort2 = c(cohort2['model17','cohensd2'], cohort2['model17','SE2'], cohort2['model17', c('n', 'n_cooccur2')]), 
                                                    cohort3 = c(cohort3['model17','cohensd2'], cohort3['model17','SE2'], cohort3['model17', c('n', 'n_cooccur2')]),
                                                    cohort4 = c(cohort4['model17', 'cohensd2'], cohort4['model17','SE2'], cohort4['model17', c('n', 'n_cooccur2')]),
                                                    cohort5 = c(cohort5['model17', 'cohensd2'], cohort5['model17','SE2'], cohort5['model17', c('n', 'n_cooccur2')]),
                                                    cohort6 = c(cohort6['model17', 'cohensd2'], cohort6['model17','SE2'], cohort6['model17', c('N', 'n_cooccur2')]),
                                                    cohort7 = c(cohort7['model17', 'cohensd2'], cohort7['model17','SE2'], cohort7['model17', c('n', 'n_cooccur2')]),
                                                    cohort8 = c(cohort8['model17', 'cohensd2'], cohort8['model17','SE2'], cohort8['model17', c('n', 'n_cooccur2')]),
                                                    cohort9 = c(cohort9['model17', 'cohensd2'], cohort9['model17','SE2'], cohort9['model17', c('n', 'n_cooccur2')]),
                                                    cohort10 = c(cohort10['model17', 'cohensd2'], cohort10['model17','SE2'], cohort10['model17', c('n', 'n_cooccur2')]),
                                                    cohort11 = c(cohort11['model17', 'cohensd2'], cohort11['model17','SE2'], cohort11['model17', c('n', 'n_cooccur2')])))
colnames(Model17_cooccur2_vs_cooccur0) <- c('cohensd', 'SE', 'n', 'n_cooccur2')

Model17_cooccur3_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model17','cohensd3'], cohort1['model17','SE3'], cohort1['model17', c('n', 'n_cooccur3')]), 
                                                    cohort2 = c(cohort2['model17','cohensd3'], cohort2['model17','SE3'], cohort2['model17', c('n', 'n_cooccur3')]), 
                                                    cohort3 = c(cohort3['model17','cohensd3'], cohort3['model17','SE3'], cohort3['model17', c('n', 'n_cooccur3')]),
                                                    cohort4 = c(cohort4['model17', 'cohensd3'], cohort4['model17','SE3'], cohort4['model17', c('n', 'n_cooccur3')]),
                                                    cohort5 = c(cohort5['model17', 'cohensd3'], cohort5['model17','SE3'], cohort5['model17', c('n', 'n_cooccur3')]),
                                                    cohort6 = c(cohort6['model17', 'cohensd3'], cohort6['model17','SE3'], cohort6['model17', c('N', 'n_cooccur3')]),
                                                    cohort7 = c(cohort7['model17', 'cohensd3'], cohort7['model17','SE3'], cohort7['model17', c('n', 'n_cooccur3')]),
                                                    cohort8 = c(cohort8['model17', 'cohensd3'], cohort8['model17','SE3'], cohort8['model17', c('n', 'n_cooccur3')]),
                                                    cohort9 = c(cohort9['model17', 'cohensd3'], cohort9['model17','SE3'], cohort9['model17', c('n', 'n_cooccur3')]),
                                                    cohort10 = c(cohort10['model17', 'cohensd3'], cohort10['model17','SE3'], cohort10['model17', c('n', 'n_cooccur3')]),
                                                    cohort11 = c(cohort11['model17', 'cohensd3'], cohort11['model17','SE3'], cohort11['model17', c('n', 'n_cooccur3')])))
colnames(Model17_cooccur3_vs_cooccur0) <- c('cohensd', 'SE', 'n', 'n_cooccur3')

Model18_cooccur1_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model18','cohensd1'], cohort1['model18','SE1'], cohort1['model18', c('n', 'n_cooccur1')]), 
                                                    cohort2 = c(cohort2['model18','cohensd1'], cohort2['model18','SE1'], cohort2['model18', c('n', 'n_cooccur1')]), 
                                                    cohort3 = c(cohort3['model18','cohensd1'], cohort3['model18','SE1'], cohort3['model18', c('n', 'n_cooccur1')]),
                                                    cohort4 = c(cohort4['model18', 'cohensd1'], cohort4['model18','SE1'], cohort4['model18', c('n', 'n_cooccur1')]),
                                                    cohort5 = c(cohort5['model18', 'cohensd1'], cohort5['model18','SE1'], cohort5['model18', c('n', 'n_cooccur1')]),
                                                    cohort6 = c(cohort6['model18', 'cohensd1'], cohort6['model18','SE1'], cohort6['model18', c('n', 'n_cooccur1')]),
                                                    cohort7 = c(cohort7['model18', 'cohensd1'], cohort7['model18','SE1'], cohort7['model18', c('n', 'n_cooccur1')]),
                                                    cohort8 = c(cohort8['model18', 'cohensd1'], cohort8['model18','SE1'], cohort8['model18', c('n', 'n_cooccur1')]),
                                                    cohort9 = c(cohort9['model18', 'cohensd1'], cohort9['model18','SE1'], cohort9['model18', c('n', 'n_cooccur1')]),
                                                    cohort10 = c(cohort10['model18', 'cohensd1'], cohort10['model18','SE1'], cohort10['model18', c('n', 'n_cooccur1')]),
                                                    cohort11 = c(cohort11['model18', 'cohensd1'], cohort11['model18','SE1'], cohort11['model18', c('n', 'n_cooccur1')])))

colnames(Model18_cooccur1_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur1')

Model18_cooccur2_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model18','cohensd2'], cohort1['model18','SE2'], cohort1['model18', c('n', 'n_cooccur2')]), 
                                                    cohort2 = c(cohort2['model18','cohensd2'], cohort2['model18','SE2'], cohort2['model18', c('n', 'n_cooccur2')]), 
                                                    cohort3 = c(cohort3['model18','cohensd2'], cohort3['model18','SE2'], cohort3['model18', c('n', 'n_cooccur2')]),
                                                    cohort4 = c(cohort4['model18', 'cohensd2'], cohort4['model18','SE2'], cohort4['model18', c('n', 'n_cooccur2')]),
                                                    cohort5 = c(cohort5['model18', 'cohensd2'], cohort5['model18','SE2'], cohort5['model18', c('n', 'n_cooccur2')]),
                                                    cohort6 = c(cohort6['model18', 'cohensd2'], cohort6['model18','SE2'], cohort6['model18', c('n', 'n_cooccur2')]),
                                                    cohort7 = c(cohort7['model18', 'cohensd2'], cohort7['model18','SE2'], cohort7['model18', c('n', 'n_cooccur2')]),
                                                    cohort8 = c(cohort8['model18', 'cohensd2'], cohort8['model18','SE2'], cohort8['model18', c('n', 'n_cooccur2')]),
                                                    cohort9 = c(cohort9['model18', 'cohensd2'], cohort9['model18','SE2'], cohort9['model18', c('n', 'n_cooccur2')]),
                                                    cohort10 = c(cohort10['model18', 'cohensd2'], cohort10['model18','SE2'], cohort10['model18', c('n', 'n_cooccur2')]),
                                                    cohort11 = c(cohort11['model18', 'cohensd2'], cohort11['model18','SE2'], cohort11['model18', c('n', 'n_cooccur2')])))

colnames(Model18_cooccur2_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur2')

Model18_cooccur3_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model18','cohensd3'], cohort1['model18','SE3'], cohort1['model18', c('n', 'n_cooccur3')]), 
                                                    cohort2 = c(cohort2['model18','cohensd3'], cohort2['model18','SE3'], cohort2['model18', c('n', 'n_cooccur3')]), 
                                                    cohort3 = c(cohort3['model18','cohensd3'], cohort3['model18','SE3'], cohort3['model18', c('n', 'n_cooccur3')]),
                                                    cohort4 = c(cohort4['model18', 'cohensd3'], cohort4['model18','SE3'], cohort4['model18', c('n', 'n_cooccur3')]),
                                                    cohort5 = c(cohort5['model18', 'cohensd3'], cohort5['model18','SE3'], cohort5['model18', c('n', 'n_cooccur3')]),
                                                    cohort6 = c(cohort6['model18', 'cohensd3'], cohort6['model18','SE3'], cohort6['model18', c('n', 'n_cooccur3')]),
                                                    cohort7 = c(cohort7['model18', 'cohensd3'], cohort7['model18','SE3'], cohort7['model18', c('n', 'n_cooccur3')]),
                                                    cohort8 = c(cohort8['model18', 'cohensd3'], cohort8['model18','SE3'], cohort8['model18', c('n', 'n_cooccur3')]),
                                                    cohort9 = c(cohort9['model18', 'cohensd3'], cohort9['model18','SE3'], cohort9['model18', c('n', 'n_cooccur3')]),
                                                    cohort10 = c(cohort10['model18', 'cohensd3'], cohort10['model18','SE3'], cohort10['model18', c('n', 'n_cooccur3')]),
                                                    cohort11 = c(cohort11['model18', 'cohensd3'], cohort11['model18','SE3'], cohort11['model18', c('n', 'n_cooccur3')])))
colnames(Model18_cooccur3_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur3')

Model19_cooccur1_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model19','cohensd1'], cohort1['model19','SE1'], cohort1['model19', c('n', 'n_cooccur1')]), 
                                                    cohort2 = c(cohort2['model19','cohensd1'], cohort2['model19','SE1'], cohort2['model19', c('n', 'n_cooccur1')]), 
                                                    cohort3 = c(cohort3['model19','cohensd1'], cohort3['model19','SE1'], cohort3['model19', c('n', 'n_cooccur1')]),
                                                    cohort4 = c(cohort4['model19', 'cohensd1'], cohort4['model19','SE1'], cohort4['model19', c('n', 'n_cooccur1')]),
                                                    cohort5 = c(cohort5['model19', 'cohensd1'], cohort5['model19','SE1'], cohort5['model19', c('n', 'n_cooccur1')]),
                                                    cohort6 = c(cohort6['model19', 'cohensd1'], cohort6['model19','SE1'], cohort6['model19', c('n', 'n_cooccur1')]),
                                                    cohort7 = c(cohort7['model19', 'cohensd1'], cohort7['model19','SE1'], cohort7['model19', c('n', 'n_cooccur1')]),
                                                    cohort8 = c(cohort8['model19', 'cohensd1'], cohort8['model19','SE1'], cohort8['model19', c('n', 'n_cooccur1')]),
                                                    cohort9 = c(cohort9['model19', 'cohensd1'], cohort9['model19','SE1'], cohort9['model19', c('n', 'n_cooccur1')]),
                                                    cohort10 = c(cohort10['model19', 'cohensd1'], cohort10['model19','SE1'], cohort10['model19', c('n', 'n_cooccur1')]),
                                                    cohort11 = c(cohort11['model19', 'cohensd1'], cohort11['model19','SE1'], cohort11['model19', c('n', 'n_cooccur1')])))
colnames(Model19_cooccur1_vs_cooccur0) <- c('cohensd', 'SE', 'n', 'n_cooccur1')

Model19_cooccur2_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model19','cohensd2'], cohort1['model19','SE2'], cohort1['model19', c('n', 'n_cooccur2')]), 
                                                    cohort2 = c(cohort2['model19','cohensd2'], cohort2['model19','SE2'], cohort2['model19', c('n', 'n_cooccur2')]), 
                                                    cohort3 = c(cohort3['model19','cohensd2'], cohort3['model19','SE2'], cohort3['model19', c('n', 'n_cooccur2')]),
                                                    cohort4 = c(cohort4['model19', 'cohensd2'], cohort4['model19','SE2'], cohort4['model19', c('n', 'n_cooccur2')]),
                                                    cohort5 = c(cohort5['model19', 'cohensd2'], cohort5['model19','SE2'], cohort5['model19', c('n', 'n_cooccur2')]),
                                                    cohort6 = c(cohort6['model19', 'cohensd2'], cohort6['model19','SE2'], cohort6['model19', c('n', 'n_cooccur2')]),
                                                    cohort7 = c(cohort7['model19', 'cohensd2'], cohort7['model19','SE2'], cohort7['model19', c('n', 'n_cooccur2')]),
                                                    cohort8 = c(cohort8['model19', 'cohensd2'], cohort8['model19','SE2'], cohort8['model19', c('n', 'n_cooccur2')]),
                                                    cohort9 = c(cohort9['model19', 'cohensd2'], cohort9['model19','SE2'], cohort9['model19', c('n', 'n_cooccur2')]),
                                                    cohort10 = c(cohort10['model19', 'cohensd2'], cohort10['model19','SE2'], cohort10['model19', c('n', 'n_cooccur2')]),
                                                    cohort11 = c(cohort11['model19', 'cohensd2'], cohort11['model19','SE2'], cohort11['model19', c('n', 'n_cooccur2')])))

colnames(Model19_cooccur2_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur2')

Model19_cooccur3_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model19','cohensd3'], cohort1['model19','SE3'], cohort1['model19', c('n', 'n_cooccur3')]), 
                                                    cohort2 = c(cohort2['model19','cohensd3'], cohort2['model19','SE3'], cohort2['model19', c('n', 'n_cooccur3')]), 
                                                    cohort3 = c(cohort3['model19','cohensd3'], cohort3['model19','SE3'], cohort3['model19', c('n', 'n_cooccur3')]),
                                                    cohort4 = c(cohort4['model19', 'cohensd3'], cohort4['model19','SE3'], cohort4['model19', c('n', 'n_cooccur3')]),
                                                    cohort5 = c(cohort5['model19', 'cohensd3'], cohort5['model19','SE3'], cohort5['model19', c('n', 'n_cooccur3')]),
                                                    cohort6 = c(cohort6['model19', 'cohensd3'], cohort6['model19','SE3'], cohort6['model19', c('n', 'n_cooccur3')]),
                                                    cohort7 = c(cohort7['model19', 'cohensd3'], cohort7['model19','SE3'], cohort7['model19', c('n', 'n_cooccur3')]),
                                                    cohort8 = c(cohort8['model19', 'cohensd3'], cohort8['model19','SE3'], cohort8['model19', c('n', 'n_cooccur3')]),
                                                    cohort9 = c(cohort9['model19', 'cohensd3'], cohort9['model19','SE3'], cohort9['model19', c('n', 'n_cooccur3')]),
                                                    cohort10 = c(cohort10['model19', 'cohensd3'], cohort10['model19','SE3'], cohort10['model19', c('n', 'n_cooccur3')]),
                                                    cohort11 = c(cohort11['model19', 'cohensd3'], cohort11['model19','SE3'], cohort11['model19', c('n', 'n_cooccur3')])))
colnames(Model19_cooccur3_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur3')

Model20_cooccur1_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model20','cohensd1'], cohort1['model20','SE1'], cohort1['model20', c('n', 'n_cooccur1')]), 
                                                    cohort2 = c(cohort2['model20','cohensd1'], cohort2['model20','SE1'], cohort2['model20', c('n', 'n_cooccur1')]), 
                                                    cohort3 = c(cohort3['model20','cohensd1'], cohort3['model20','SE1'], cohort3['model20', c('n', 'n_cooccur1')]),
                                                    cohort4 = c(cohort4['model20', 'cohensd1'], cohort4['model20','SE1'], cohort4['model20', c('n', 'n_cooccur1')]),
                                                    cohort5 = c(cohort5['model20', 'cohensd1'], cohort5['model20','SE1'], cohort5['model20', c('n', 'n_cooccur1')]),
                                                    cohort6 = c(cohort6['model20', 'cohensd1'], cohort6['model20','SE1'], cohort6['model20', c('n', 'n_cooccur1')]),
                                                    cohort7 = c(cohort7['model20', 'cohensd1'], cohort7['model20','SE1'], cohort7['model20', c('n', 'n_cooccur1')]),
                                                    cohort8 = c(cohort8['model20', 'cohensd1'], cohort8['model20','SE1'], cohort8['model20', c('n', 'n_cooccur1')]),
                                                    cohort9 = c(cohort9['model20', 'cohensd1'], cohort9['model20','SE1'], cohort9['model20', c('n', 'n_cooccur1')]),
                                                    cohort10 = c(cohort10['model20', 'cohensd1'], cohort10['model20','SE1'], cohort10['model20', c('n', 'n_cooccur1')]),
                                                    cohort11 = c(cohort11['model20', 'cohensd1'], cohort11['model20','SE1'], cohort11['model20', c('n', 'n_cooccur1')])))
colnames(Model20_cooccur1_vs_cooccur0) <- c('cohensd', 'SE', 'n', 'n_cooccur1')

Model20_cooccur2_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model20','cohensd2'], cohort1['model20','SE2'], cohort1['model20', c('n', 'n_cooccur2')]), 
                                                    cohort2 = c(cohort2['model20','cohensd2'], cohort2['model20','SE2'], cohort2['model20', c('n', 'n_cooccur2')]), 
                                                    cohort3 = c(cohort3['model20','cohensd2'], cohort3['model20','SE2'], cohort3['model20', c('n', 'n_cooccur2')]),
                                                    cohort4 = c(cohort4['model20', 'cohensd2'], cohort4['model20','SE2'], cohort4['model20', c('n', 'n_cooccur2')]),
                                                    cohort5 = c(cohort5['model20', 'cohensd2'], cohort5['model20','SE2'], cohort5['model20', c('n', 'n_cooccur2')]),
                                                    cohort6 = c(cohort6['model20', 'cohensd2'], cohort6['model20','SE2'], cohort6['model20', c('n', 'n_cooccur2')]),
                                                    cohort7 = c(cohort7['model20', 'cohensd2'], cohort7['model20','SE2'], cohort7['model20', c('n', 'n_cooccur2')]),
                                                    cohort8 = c(cohort8['model20', 'cohensd2'], cohort8['model20','SE2'], cohort8['model20', c('n', 'n_cooccur2')]),
                                                    cohort9 = c(cohort9['model20', 'cohensd2'], cohort9['model20','SE2'], cohort9['model20', c('n', 'n_cooccur2')]),
                                                    cohort10 = c(cohort10['model20', 'cohensd2'], cohort10['model20','SE2'], cohort10['model20', c('n', 'n_cooccur2')]),
                                                    cohort11 = c(cohort11['model20', 'cohensd2'], cohort11['model20','SE2'], cohort11['model20', c('n', 'n_cooccur2')])))

colnames(Model20_cooccur2_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur2')

Model20_cooccur3_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model20','cohensd3'], cohort1['model20','SE3'], cohort1['model20', c('n', 'n_cooccur3')]), 
                                                    cohort2 = c(cohort2['model20','cohensd3'], cohort2['model20','SE3'], cohort2['model20', c('n', 'n_cooccur3')]), 
                                                    cohort3 = c(cohort3['model20','cohensd3'], cohort3['model20','SE3'], cohort3['model20', c('n', 'n_cooccur3')]),
                                                    cohort4 = c(cohort4['model20', 'cohensd3'], cohort4['model20','SE3'], cohort4['model20', c('n', 'n_cooccur3')]),
                                                    cohort5 = c(cohort5['model20', 'cohensd3'], cohort5['model20','SE3'], cohort5['model20', c('n', 'n_cooccur3')]),
                                                    cohort6 = c(cohort6['model20', 'cohensd3'], cohort6['model20','SE3'], cohort6['model20', c('n', 'n_cooccur3')]),
                                                    cohort7 = c(cohort7['model20', 'cohensd3'], cohort7['model20','SE3'], cohort7['model20', c('n', 'n_cooccur3')]),
                                                    cohort8 = c(cohort8['model20', 'cohensd3'], cohort8['model20','SE3'], cohort8['model20', c('n', 'n_cooccur3')]),
                                                    cohort9 = c(cohort9['model20', 'cohensd3'], cohort9['model20','SE3'], cohort9['model20', c('n', 'n_cooccur3')]),
                                                    cohort10 = c(cohort10['model20', 'cohensd3'], cohort10['model20','SE3'], cohort10['model20', c('n', 'n_cooccur3')]),
                                                    cohort11 = c(cohort11['model20', 'cohensd3'], cohort11['model20','SE3'], cohort11['model20', c('n', 'n_cooccur3')])))
colnames(Model20_cooccur3_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur3')



#################### 8. Models 21, 22, 23, and 24 - Effect sizes of association of cooccurrence of childhood maltreatment and depression with metabolic outcomes, excluding lipid-modifying medication and antidepressant users #################### 
Model21_cooccur1_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model21','cohensd1'], cohort1['model21','SE1'], cohort1['model21', c('n', 'n_cooccur1')]), 
                                                    cohort2 = c(cohort2['model21','cohensd1'], cohort2['model21','SE1'], cohort2['model21', c('n', 'n_cooccur1')]), 
                                                    cohort3 = c(cohort3['model21','cohensd1'], cohort3['model21','SE1'], cohort3['model21', c('n', 'n_cooccur1')]),
                                                    cohort4 = c(cohort4['model21', 'cohensd1'], cohort4['model21','SE1'], cohort4['model21', c('n', 'n_cooccur1')]),
                                                    cohort5 = c(cohort5['model21', 'cohensd1'], cohort5['model21','SE1'], cohort5['model21', c('n', 'n_cooccur1')]),
                                                    cohort6 = c(cohort6['model21', 'cohensd1'], cohort6['model21','SE1'], cohort6['model21', c('n', 'n_cooccur1')]),
                                                    cohort7 = c(cohort7['model21', 'cohensd1'], cohort7['model21','SE1'], cohort7['model21', c('n', 'n_cooccur1')]),
                                                    cohort8 = c(cohort8['model21', 'cohensd1'], cohort8['model21','SE1'], cohort8['model21', c('n', 'n_cooccur1')]),
                                                    cohort9 = c(cohort9['model21', 'cohensd1'], cohort9['model21','SE1'], cohort9['model21', c('n', 'n_cooccur1')]),
                                                    cohort10 = c(cohort10['model21', 'cohensd1'], cohort10['model21','SE1'], cohort10['model21', c('n', 'n_cooccur1')]),
                                                    cohort11 = c(cohort11['model21', 'cohensd1'], cohort11['model21','SE1'], cohort11['model21', c('n', 'n_cooccur1')])))
colnames(Model21_cooccur1_vs_cooccur0) <- c('cohensd', 'SE', 'n', 'n_cooccur1')

Model21_cooccur2_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model21','cohensd2'], cohort1['model21','SE2'], cohort1['model21', c('n', 'n_cooccur2')]), 
                                                    cohort2 = c(cohort2['model21','cohensd2'], cohort2['model21','SE2'], cohort2['model21', c('n', 'n_cooccur2')]), 
                                                    cohort3 = c(cohort3['model21','cohensd2'], cohort3['model21','SE2'], cohort3['model21', c('n', 'n_cooccur2')]),
                                                    cohort4 = c(cohort4['model21', 'cohensd2'], cohort4['model21','SE2'], cohort4['model21', c('n', 'n_cooccur2')]),
                                                    cohort5 = c(cohort5['model21', 'cohensd2'], cohort5['model21','SE2'], cohort5['model21', c('n', 'n_cooccur2')]),
                                                    cohort6 = c(cohort6['model21', 'cohensd2'], cohort6['model21','SE2'], cohort6['model21', c('n', 'n_cooccur2')]),
                                                    cohort7 = c(cohort7['model21', 'cohensd2'], cohort7['model21','SE2'], cohort7['model21', c('n', 'n_cooccur2')]),
                                                    cohort8 = c(cohort8['model21', 'cohensd2'], cohort8['model21','SE2'], cohort8['model21', c('n', 'n_cooccur2')]),
                                                    cohort9 = c(cohort9['model21', 'cohensd2'], cohort9['model21','SE2'], cohort9['model21', c('n', 'n_cooccur2')]),
                                                    cohort10 = c(cohort10['model21', 'cohensd2'], cohort10['model21','SE2'], cohort10['model21', c('n', 'n_cooccur2')]),
                                                    cohort11 = c(cohort11['model21', 'cohensd2'], cohort11['model21','SE2'], cohort11['model21', c('n', 'n_cooccur2')])))

colnames(Model21_cooccur2_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur2')

Model21_cooccur3_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model21','cohensd3'], cohort1['model21','SE3'], cohort1['model21', c('n', 'n_cooccur3')]), 
                                                    cohort2 = c(cohort2['model21','cohensd3'], cohort2['model21','SE3'], cohort2['model21', c('n', 'n_cooccur3')]), 
                                                    cohort3 = c(cohort3['model21','cohensd3'], cohort3['model21','SE3'], cohort3['model21', c('n', 'n_cooccur3')]),
                                                    cohort4 = c(cohort4['model21', 'cohensd3'], cohort4['model21','SE3'], cohort4['model21', c('n', 'n_cooccur3')]),
                                                    cohort5 = c(cohort5['model21', 'cohensd3'], cohort5['model21','SE3'], cohort5['model21', c('n', 'n_cooccur3')]),
                                                    cohort6 = c(cohort6['model21', 'cohensd3'], cohort6['model21','SE3'], cohort6['model21', c('n', 'n_cooccur3')]),
                                                    cohort7 = c(cohort7['model21', 'cohensd3'], cohort7['model21','SE3'], cohort7['model21', c('n', 'n_cooccur3')]),
                                                    cohort8 = c(cohort8['model21', 'cohensd3'], cohort8['model21','SE3'], cohort8['model21', c('n', 'n_cooccur3')]),
                                                    cohort9 = c(cohort9['model21', 'cohensd3'], cohort9['model21','SE3'], cohort9['model21', c('n', 'n_cooccur3')]),
                                                    cohort10 = c(cohort10['model21', 'cohensd3'], cohort10['model21','SE3'], cohort10['model21', c('n', 'n_cooccur3')]),
                                                    cohort11 = c(cohort11['model21', 'cohensd3'], cohort11['model21','SE3'], cohort11['model21', c('n', 'n_cooccur3')])))
colnames(Model21_cooccur3_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur3')

Model22_cooccur1_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model22','cohensd1'], cohort1['model22','SE1'], cohort1['model22', c('n', 'n_cooccur1')]), 
                                                    cohort2 = c(cohort2['model22','cohensd1'], cohort2['model22','SE1'], cohort2['model22', c('n', 'n_cooccur1')]), 
                                                    cohort3 = c(cohort3['model22','cohensd1'], cohort3['model22','SE1'], cohort3['model22', c('n', 'n_cooccur1')]),
                                                    cohort4 = c(cohort4['model22', 'cohensd1'], cohort4['model22','SE1'], cohort4['model22', c('n', 'n_cooccur1')]),
                                                    cohort5 = c(cohort5['model22', 'cohensd1'], cohort5['model22','SE1'], cohort5['model22', c('n', 'n_cooccur1')]),
                                                    cohort6 = c(cohort6['model22', 'cohensd1'], cohort6['model22','SE1'], cohort6['model22', c('n', 'n_cooccur1')]),
                                                    cohort7 = c(cohort7['model22', 'cohensd1'], cohort7['model22','SE1'], cohort7['model22', c('n', 'n_cooccur1')]),
                                                    cohort8 = c(cohort8['model22', 'cohensd1'], cohort8['model22','SE1'], cohort8['model22', c('n', 'n_cooccur1')]),
                                                    cohort9 = c(cohort9['model22', 'cohensd1'], cohort9['model22','SE1'], cohort9['model22', c('n', 'n_cooccur1')]),
                                                    cohort10 = c(cohort10['model22', 'cohensd1'], cohort10['model22','SE1'], cohort10['model22', c('n', 'n_cooccur1')]),
                                                    cohort11 = c(cohort11['model22', 'cohensd1'], cohort11['model22','SE1'], cohort11['model22', c('n', 'n_cooccur1')])))
colnames(Model22_cooccur1_vs_cooccur0) <- c('cohensd', 'SE', 'n', 'n_cooccur1')

Model22_cooccur2_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model22','cohensd2'], cohort1['model22','SE2'], cohort1['model22', c('n', 'n_cooccur2')]), 
                                                    cohort2 = c(cohort2['model22','cohensd2'], cohort2['model22','SE2'], cohort2['model22', c('n', 'n_cooccur2')]), 
                                                    cohort3 = c(cohort3['model22','cohensd2'], cohort3['model22','SE2'], cohort3['model22', c('n', 'n_cooccur2')]),
                                                    cohort4 = c(cohort4['model22', 'cohensd2'], cohort4['model22','SE2'], cohort4['model22', c('n', 'n_cooccur2')]),
                                                    cohort5 = c(cohort5['model22', 'cohensd2'], cohort5['model22','SE2'], cohort5['model22', c('n', 'n_cooccur2')]),
                                                    cohort6 = c(cohort6['model22', 'cohensd2'], cohort6['model22','SE2'], cohort6['model22', c('n', 'n_cooccur2')]),
                                                    cohort7 = c(cohort7['model22', 'cohensd2'], cohort7['model22','SE2'], cohort7['model22', c('n', 'n_cooccur2')]),
                                                    cohort8 = c(cohort8['model22', 'cohensd2'], cohort8['model22','SE2'], cohort8['model22', c('n', 'n_cooccur2')]),
                                                    cohort9 = c(cohort9['model22', 'cohensd2'], cohort9['model22','SE2'], cohort9['model22', c('n', 'n_cooccur2')]),
                                                    cohort10 = c(cohort10['model22', 'cohensd2'], cohort10['model22','SE2'], cohort10['model22', c('n', 'n_cooccur2')]),
                                                    cohort11 = c(cohort11['model22', 'cohensd2'], cohort11['model22','SE2'], cohort11['model22', c('n', 'n_cooccur2')])))

colnames(Model22_cooccur2_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur2')

Model22_cooccur3_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model22','cohensd3'], cohort1['model22','SE3'], cohort1['model22', c('n', 'n_cooccur3')]), 
                                                    cohort2 = c(cohort2['model22','cohensd3'], cohort2['model22','SE3'], cohort2['model22', c('n', 'n_cooccur3')]), 
                                                    cohort3 = c(cohort3['model22','cohensd3'], cohort3['model22','SE3'], cohort3['model22', c('n', 'n_cooccur3')]),
                                                    cohort4 = c(cohort4['model22', 'cohensd3'], cohort4['model22','SE3'], cohort4['model22', c('n', 'n_cooccur3')]),
                                                    cohort5 = c(cohort5['model22', 'cohensd3'], cohort5['model22','SE3'], cohort5['model22', c('n', 'n_cooccur3')]),
                                                    cohort6 = c(cohort6['model22', 'cohensd3'], cohort6['model22','SE3'], cohort6['model22', c('n', 'n_cooccur3')]),
                                                    cohort7 = c(cohort7['model22', 'cohensd3'], cohort7['model22','SE3'], cohort7['model22', c('n', 'n_cooccur3')]),
                                                    cohort8 = c(cohort8['model22', 'cohensd3'], cohort8['model22','SE3'], cohort8['model22', c('n', 'n_cooccur3')]),
                                                    cohort9 = c(cohort9['model22', 'cohensd3'], cohort9['model22','SE3'], cohort9['model22', c('n', 'n_cooccur3')]),
                                                    cohort10 = c(cohort10['model22', 'cohensd3'], cohort10['model22','SE3'], cohort10['model22', c('n', 'n_cooccur3')]),
                                                    cohort11 = c(cohort11['model22', 'cohensd3'], cohort11['model22','SE3'], cohort11['model22', c('n', 'n_cooccur3')])))
colnames(Model22_cooccur3_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur3')

Model23_cooccur1_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model23','cohensd1'], cohort1['model23','SE1'], cohort1['model23', c('n', 'n_cooccur1')]), 
                                                    cohort2 = c(cohort2['model23','cohensd1'], cohort2['model23','SE1'], cohort2['model23', c('n', 'n_cooccur1')]), 
                                                    cohort3 = c(cohort3['model23','cohensd1'], cohort3['model23','SE1'], cohort3['model23', c('n', 'n_cooccur1')]),
                                                    cohort4 = c(cohort4['model23', 'cohensd1'], cohort4['model23','SE1'], cohort4['model23', c('n', 'n_cooccur1')]),
                                                    cohort5 = c(cohort5['model23', 'cohensd1'], cohort5['model23','SE1'], cohort5['model23', c('n', 'n_cooccur1')]),
                                                    cohort6 = c(cohort6['model23', 'cohensd1'], cohort6['model23','SE1'], cohort6['model23', c('n', 'n_cooccur1')]),
                                                    cohort7 = c(cohort7['model23', 'cohensd1'], cohort7['model23','SE1'], cohort7['model23', c('n', 'n_cooccur1')]),
                                                    cohort8 = c(cohort8['model23', 'cohensd1'], cohort8['model23','SE1'], cohort8['model23', c('n', 'n_cooccur1')]),
                                                    cohort9 = c(cohort9['model23', 'cohensd1'], cohort9['model23','SE1'], cohort9['model23', c('n', 'n_cooccur1')]),
                                                    cohort10 = c(cohort10['model23', 'cohensd1'], cohort10['model23','SE1'], cohort10['model23', c('n', 'n_cooccur1')]),
                                                    cohort11 = c(cohort11['model23', 'cohensd1'], cohort11['model23','SE1'], cohort11['model23', c('n', 'n_cooccur1')])))
colnames(Model23_cooccur1_vs_cooccur0) <- c('cohensd', 'SE', 'n', 'n_cooccur1')

Model23_cooccur2_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model23','cohensd2'], cohort1['model23','SE2'], cohort1['model23', c('n', 'n_cooccur2')]), 
                                                    cohort2 = c(cohort2['model23','cohensd2'], cohort2['model23','SE2'], cohort2['model23', c('n', 'n_cooccur2')]), 
                                                    cohort3 = c(cohort3['model23','cohensd2'], cohort3['model23','SE2'], cohort3['model23', c('n', 'n_cooccur2')]),
                                                    cohort4 = c(cohort4['model23', 'cohensd2'], cohort4['model23','SE2'], cohort4['model23', c('n', 'n_cooccur2')]),
                                                    cohort5 = c(cohort5['model23', 'cohensd2'], cohort5['model23','SE2'], cohort5['model23', c('n', 'n_cooccur2')]),
                                                    cohort6 = c(cohort6['model23', 'cohensd2'], cohort6['model23','SE2'], cohort6['model23', c('n', 'n_cooccur2')]),
                                                    cohort7 = c(cohort7['model23', 'cohensd2'], cohort7['model23','SE2'], cohort7['model23', c('n', 'n_cooccur2')]),
                                                    cohort8 = c(cohort8['model23', 'cohensd2'], cohort8['model23','SE2'], cohort8['model23', c('n', 'n_cooccur2')]),
                                                    cohort9 = c(cohort9['model23', 'cohensd2'], cohort9['model23','SE2'], cohort9['model23', c('n', 'n_cooccur2')]),
                                                    cohort10 = c(cohort10['model23', 'cohensd2'], cohort10['model23','SE2'], cohort10['model23', c('n', 'n_cooccur2')]),
                                                    cohort11 = c(cohort11['model23', 'cohensd2'], cohort11['model23','SE2'], cohort11['model23', c('n', 'n_cooccur2')])))

colnames(Model23_cooccur2_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur2')

Model23_cooccur3_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model23','cohensd3'], cohort1['model23','SE3'], cohort1['model23', c('n', 'n_cooccur3')]), 
                                                    cohort2 = c(cohort2['model23','cohensd3'], cohort2['model23','SE3'], cohort2['model23', c('n', 'n_cooccur3')]), 
                                                    cohort3 = c(cohort3['model23','cohensd3'], cohort3['model23','SE3'], cohort3['model23', c('n', 'n_cooccur3')]),
                                                    cohort4 = c(cohort4['model23', 'cohensd3'], cohort4['model23','SE3'], cohort4['model23', c('n', 'n_cooccur3')]),
                                                    cohort5 = c(cohort5['model23', 'cohensd3'], cohort5['model23','SE3'], cohort5['model23', c('n', 'n_cooccur3')]),
                                                    cohort6 = c(cohort6['model23', 'cohensd3'], cohort6['model23','SE3'], cohort6['model23', c('n', 'n_cooccur3')]),
                                                    cohort7 = c(cohort7['model23', 'cohensd3'], cohort7['model23','SE3'], cohort7['model23', c('n', 'n_cooccur3')]),
                                                    cohort8 = c(cohort8['model23', 'cohensd3'], cohort8['model23','SE3'], cohort8['model23', c('n', 'n_cooccur3')]),
                                                    cohort9 = c(cohort9['model23', 'cohensd3'], cohort9['model23','SE3'], cohort9['model23', c('n', 'n_cooccur3')]),
                                                    cohort10 = c(cohort10['model23', 'cohensd3'], cohort10['model23','SE3'], cohort10['model23', c('n', 'n_cooccur3')]),
                                                    cohort11 = c(cohort11['model23', 'cohensd3'], cohort11['model23','SE3'], cohort11['model23', c('n', 'n_cooccur3')])))
colnames(Model23_cooccur3_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur3')

Model24_cooccur1_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model24','cohensd1'], cohort1['model24','SE1'], cohort1['model24', c('n', 'n_cooccur1')]), 
                                                    cohort2 = c(cohort2['model24','cohensd1'], cohort2['model24','SE1'], cohort2['model24', c('n', 'n_cooccur1')]), 
                                                    cohort3 = c(cohort3['model24','cohensd1'], cohort3['model24','SE1'], cohort3['model24', c('n', 'n_cooccur1')]),
                                                    cohort4 = c(cohort4['model24', 'cohensd1'], cohort4['model24','SE1'], cohort4['model24', c('n', 'n_cooccur1')]),
                                                    cohort5 = c(cohort5['model24', 'cohensd1'], cohort5['model24','SE1'], cohort5['model24', c('n', 'n_cooccur1')]),
                                                    cohort6 = c(cohort6['model24', 'cohensd1'], cohort6['model24','SE1'], cohort6['model24', c('n', 'n_cooccur1')]),
                                                    cohort7 = c(cohort7['model24', 'cohensd1'], cohort7['model24','SE1'], cohort7['model24', c('n', 'n_cooccur1')]),
                                                    cohort8 = c(cohort8['model24', 'cohensd1'], cohort8['model24','SE1'], cohort8['model24', c('n', 'n_cooccur1')]),
                                                    cohort9 = c(cohort9['model24', 'cohensd1'], cohort9['model24','SE1'], cohort9['model24', c('n', 'n_cooccur1')]),
                                                    cohort10 = c(cohort10['model24', 'cohensd1'], cohort10['model24','SE1'], cohort10['model24', c('n', 'n_cooccur1')]),
                                                    cohort11 = c(cohort11['model24', 'cohensd1'], cohort11['model24','SE1'], cohort11['model24', c('n', 'n_cooccur1')])))
colnames(Model24_cooccur1_vs_cooccur0) <- c('cohensd', 'SE', 'n', 'n_cooccur1')

Model24_cooccur2_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model24','cohensd2'], cohort1['model24','SE2'], cohort1['model24', c('n', 'n_cooccur2')]), 
                                                    cohort2 = c(cohort2['model24','cohensd2'], cohort2['model24','SE2'], cohort2['model24', c('n', 'n_cooccur2')]), 
                                                    cohort3 = c(cohort3['model24','cohensd2'], cohort3['model24','SE2'], cohort3['model24', c('n', 'n_cooccur2')]),
                                                    cohort4 = c(cohort4['model24', 'cohensd2'], cohort4['model24','SE2'], cohort4['model24', c('n', 'n_cooccur2')]),
                                                    cohort5 = c(cohort5['model24', 'cohensd2'], cohort5['model24','SE2'], cohort5['model24', c('n', 'n_cooccur2')]),
                                                    cohort6 = c(cohort6['model24', 'cohensd2'], cohort6['model24','SE2'], cohort6['model24', c('n', 'n_cooccur2')]),
                                                    cohort7 = c(cohort7['model24', 'cohensd2'], cohort7['model24','SE2'], cohort7['model24', c('n', 'n_cooccur2')]),
                                                    cohort8 = c(cohort8['model24', 'cohensd2'], cohort8['model24','SE2'], cohort8['model24', c('n', 'n_cooccur2')]),
                                                    cohort9 = c(cohort9['model24', 'cohensd2'], cohort9['model24','SE2'], cohort9['model24', c('n', 'n_cooccur2')]),
                                                    cohort10 = c(cohort10['model24', 'cohensd2'], cohort10['model24','SE2'], cohort10['model24', c('n', 'n_cooccur2')]),
                                                    cohort11 = c(cohort11['model24', 'cohensd2'], cohort11['model24','SE2'], cohort11['model24', c('n', 'n_cooccur2')])))

colnames(Model24_cooccur2_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur2')

Model24_cooccur3_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model24','cohensd3'], cohort1['model24','SE3'], cohort1['model24', c('n', 'n_cooccur3')]), 
                                                    cohort2 = c(cohort2['model24','cohensd3'], cohort2['model24','SE3'], cohort2['model24', c('n', 'n_cooccur3')]), 
                                                    cohort3 = c(cohort3['model24','cohensd3'], cohort3['model24','SE3'], cohort3['model24', c('n', 'n_cooccur3')]),
                                                    cohort4 = c(cohort4['model24', 'cohensd3'], cohort4['model24','SE3'], cohort4['model24', c('n', 'n_cooccur3')]),
                                                    cohort5 = c(cohort5['model24', 'cohensd3'], cohort5['model24','SE3'], cohort5['model24', c('n', 'n_cooccur3')]),
                                                    cohort6 = c(cohort6['model24', 'cohensd3'], cohort6['model24','SE3'], cohort6['model24', c('n', 'n_cooccur3')]),
                                                    cohort7 = c(cohort7['model24', 'cohensd3'], cohort7['model24','SE3'], cohort7['model24', c('n', 'n_cooccur3')]),
                                                    cohort8 = c(cohort8['model24', 'cohensd3'], cohort8['model24','SE3'], cohort8['model24', c('n', 'n_cooccur3')]),
                                                    cohort9 = c(cohort9['model24', 'cohensd3'], cohort9['model24','SE3'], cohort9['model24', c('n', 'n_cooccur3')]),
                                                    cohort10 = c(cohort10['model24', 'cohensd3'], cohort10['model24','SE3'], cohort10['model24', c('n', 'n_cooccur3')]),
                                                    cohort11 = c(cohort11['model24', 'cohensd3'], cohort11['model24','SE3'], cohort11['model24', c('n', 'n_cooccur3')])))
colnames(Model24_cooccur3_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur3')



#################### 9. Models 25, 26, 27, and 28 - Effect sizes of associations of cooccurrence of childhood maltreatment and depression with metabolic outcomes, in males only #################### 
Model25_cooccur1_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model25','cohensd1'], cohort1['model25','SE1'], cohort1['model25', c('n', 'n_cooccur1')]), 
                                                    cohort2 = c(cohort2['model25','cohensd1'], cohort2['model25','SE1'], cohort2['model25', c('n', 'n_cooccur1')]), 
                                                    cohort3 = c(cohort3['model25','cohensd1'], cohort3['model25','SE1'], cohort3['model25', c('n', 'n_cooccur1')]),
                                                    cohort4 = c(cohort4['model25', 'cohensd1'], cohort4['model25','SE1'], cohort4['model25', c('n', 'n_cooccur1')]),
                                                    cohort5 = c(cohort5['model25', 'cohensd1'], cohort5['model25','SE1'], cohort5['model25', c('n', 'n_cooccur1')]),
                                                    cohort6 = c(cohort6['model25', 'cohensd1'], cohort6['model25','SE1'], cohort6['model25', c('n', 'n_cooccur1')]),
                                                    cohort7 = c(cohort7['model25', 'cohensd1'], cohort7['model25','SE1'], cohort7['model25', c('n', 'n_cooccur1')]),
                                                    cohort8 = c(cohort8['model25', 'cohensd1'], cohort8['model25','SE1'], cohort8['model25', c('n', 'n_cooccur1')]),
                                                    cohort9 = c(cohort9['model25', 'cohensd1'], cohort9['model25','SE1'], cohort9['model25', c('n', 'n_cooccur1')]),
                                                    cohort10 = c(cohort10['model25', 'cohensd1'], cohort10['model25','SE1'], cohort10['model25', c('n', 'n_cooccur1')]),
                                                    cohort11 = c(cohort11['model25', 'cohensd1'], cohort11['model25','SE1'], cohort11['model25', c('n', 'n_cooccur1')])))
colnames(Model25_cooccur1_vs_cooccur0) <- c('cohensd', 'SE', 'n', 'n_cooccur1')

Model25_cooccur2_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model25','cohensd2'], cohort1['model25','SE2'], cohort1['model25', c('n', 'n_cooccur2')]), 
                                                    cohort2 = c(cohort2['model25','cohensd2'], cohort2['model25','SE2'], cohort2['model25', c('n', 'n_cooccur2')]), 
                                                    cohort3 = c(cohort3['model25','cohensd2'], cohort3['model25','SE2'], cohort3['model25', c('n', 'n_cooccur2')]),
                                                    cohort4 = c(cohort4['model25', 'cohensd2'], cohort4['model25','SE2'], cohort4['model25', c('n', 'n_cooccur2')]),
                                                    cohort5 = c(cohort5['model25', 'cohensd2'], cohort5['model25','SE2'], cohort5['model25', c('n', 'n_cooccur2')]),
                                                    cohort6 = c(cohort6['model25', 'cohensd2'], cohort6['model25','SE2'], cohort6['model25', c('n', 'n_cooccur2')]),
                                                    cohort7 = c(cohort7['model25', 'cohensd2'], cohort7['model25','SE2'], cohort7['model25', c('n', 'n_cooccur2')]),
                                                    cohort8 = c(cohort8['model25', 'cohensd2'], cohort8['model25','SE2'], cohort8['model25', c('n', 'n_cooccur2')]),
                                                    cohort9 = c(cohort9['model25', 'cohensd2'], cohort9['model25','SE2'], cohort9['model25', c('n', 'n_cooccur2')]),
                                                    cohort10 = c(cohort10['model25', 'cohensd2'], cohort10['model25','SE2'], cohort10['model25', c('n', 'n_cooccur2')]),
                                                    cohort11 = c(cohort11['model25', 'cohensd2'], cohort11['model25','SE2'], cohort11['model25', c('n', 'n_cooccur2')])))

colnames(Model25_cooccur2_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur2')

Model25_cooccur3_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model25','cohensd3'], cohort1['model25','SE3'], cohort1['model25', c('n', 'n_cooccur3')]), 
                                                    cohort2 = c(cohort2['model25','cohensd3'], cohort2['model25','SE3'], cohort2['model25', c('n', 'n_cooccur3')]), 
                                                    cohort3 = c(cohort3['model25','cohensd3'], cohort3['model25','SE3'], cohort3['model25', c('n', 'n_cooccur3')]),
                                                    cohort4 = c(cohort4['model25', 'cohensd3'], cohort4['model25','SE3'], cohort4['model25', c('n', 'n_cooccur3')]),
                                                    cohort5 = c(cohort5['model25', 'cohensd3'], cohort5['model25','SE3'], cohort5['model25', c('n', 'n_cooccur3')]),
                                                    cohort6 = c(cohort6['model25', 'cohensd3'], cohort6['model25','SE3'], cohort6['model25', c('n', 'n_cooccur3')]),
                                                    cohort7 = c(cohort7['model25', 'cohensd3'], cohort7['model25','SE3'], cohort7['model25', c('n', 'n_cooccur3')]),
                                                    cohort8 = c(cohort8['model25', 'cohensd3'], cohort8['model25','SE3'], cohort8['model25', c('n', 'n_cooccur3')]),
                                                    cohort9 = c(cohort9['model25', 'cohensd3'], cohort9['model25','SE3'], cohort9['model25', c('n', 'n_cooccur3')]),
                                                    cohort10 = c(cohort10['model25', 'cohensd3'], cohort10['model25','SE3'], cohort10['model25', c('n', 'n_cooccur3')]),
                                                    cohort11 = c(cohort11['model25', 'cohensd3'], cohort11['model25','SE3'], cohort11['model25', c('n', 'n_cooccur3')])))
colnames(Model25_cooccur3_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur3')

Model26_cooccur1_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model26','cohensd1'], cohort1['model26','SE1'], cohort1['model26', c('n', 'n_cooccur1')]), 
                                                    cohort2 = c(cohort2['model26','cohensd1'], cohort2['model26','SE1'], cohort2['model26', c('n', 'n_cooccur1')]), 
                                                    cohort3 = c(cohort3['model26','cohensd1'], cohort3['model26','SE1'], cohort3['model26', c('n', 'n_cooccur1')]),
                                                    cohort4 = c(cohort4['model26', 'cohensd1'], cohort4['model26','SE1'], cohort4['model26', c('n', 'n_cooccur1')]),
                                                    cohort5 = c(cohort5['model26', 'cohensd1'], cohort5['model26','SE1'], cohort5['model26', c('n', 'n_cooccur1')]),
                                                    cohort6 = c(cohort6['model26', 'cohensd1'], cohort6['model26','SE1'], cohort6['model26', c('n', 'n_cooccur1')]),
                                                    cohort7 = c(cohort7['model26', 'cohensd1'], cohort7['model26','SE1'], cohort7['model26', c('n', 'n_cooccur1')]),
                                                    cohort8 = c(cohort8['model26', 'cohensd1'], cohort8['model26','SE1'], cohort8['model26', c('n', 'n_cooccur1')]),
                                                    cohort9 = c(cohort9['model26', 'cohensd1'], cohort9['model26','SE1'], cohort9['model26', c('n', 'n_cooccur1')]),
                                                    cohort10 = c(cohort10['model26', 'cohensd1'], cohort10['model26','SE1'], cohort10['model26', c('n', 'n_cooccur1')]),
                                                    cohort11 = c(cohort11['model26', 'cohensd1'], cohort11['model26','SE1'], cohort11['model26', c('n', 'n_cooccur1')])))
colnames(Model26_cooccur1_vs_cooccur0) <- c('cohensd', 'SE', 'n', 'n_cooccur1')

Model26_cooccur2_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model26','cohensd2'], cohort1['model26','SE2'], cohort1['model26', c('n', 'n_cooccur2')]), 
                                                    cohort2 = c(cohort2['model26','cohensd2'], cohort2['model26','SE2'], cohort2['model26', c('n', 'n_cooccur2')]), 
                                                    cohort3 = c(cohort3['model26','cohensd2'], cohort3['model26','SE2'], cohort3['model26', c('n', 'n_cooccur2')]),
                                                    cohort4 = c(cohort4['model26', 'cohensd2'], cohort4['model26','SE2'], cohort4['model26', c('n', 'n_cooccur2')]),
                                                    cohort5 = c(cohort5['model26', 'cohensd2'], cohort5['model26','SE2'], cohort5['model26', c('n', 'n_cooccur2')]),
                                                    cohort6 = c(cohort6['model26', 'cohensd2'], cohort6['model26','SE2'], cohort6['model26', c('n', 'n_cooccur2')]),
                                                    cohort7 = c(cohort7['model26', 'cohensd2'], cohort7['model26','SE2'], cohort7['model26', c('n', 'n_cooccur2')]),
                                                    cohort8 = c(cohort8['model26', 'cohensd2'], cohort8['model26','SE2'], cohort8['model26', c('n', 'n_cooccur2')]),
                                                    cohort9 = c(cohort9['model26', 'cohensd2'], cohort9['model26','SE2'], cohort9['model26', c('n', 'n_cooccur2')]),
                                                    cohort10 = c(cohort10['model26', 'cohensd2'], cohort10['model26','SE2'], cohort10['model26', c('n', 'n_cooccur2')]),
                                                    cohort11 = c(cohort11['model26', 'cohensd2'], cohort11['model26','SE2'], cohort11['model26', c('n', 'n_cooccur2')])))

colnames(Model26_cooccur2_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur2')

Model26_cooccur3_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model26','cohensd3'], cohort1['model26','SE3'], cohort1['model26', c('n', 'n_cooccur3')]), 
                                                    cohort2 = c(cohort2['model26','cohensd3'], cohort2['model26','SE3'], cohort2['model26', c('n', 'n_cooccur3')]), 
                                                    cohort3 = c(cohort3['model26','cohensd3'], cohort3['model26','SE3'], cohort3['model26', c('n', 'n_cooccur3')]),
                                                    cohort4 = c(cohort4['model26', 'cohensd3'], cohort4['model26','SE3'], cohort4['model26', c('n', 'n_cooccur3')]),
                                                    cohort5 = c(cohort5['model26', 'cohensd3'], cohort5['model26','SE3'], cohort5['model26', c('n', 'n_cooccur3')]),
                                                    cohort6 = c(cohort6['model26', 'cohensd3'], cohort6['model26','SE3'], cohort6['model26', c('n', 'n_cooccur3')]),
                                                    cohort7 = c(cohort7['model26', 'cohensd3'], cohort7['model26','SE3'], cohort7['model26', c('n', 'n_cooccur3')]),
                                                    cohort8 = c(cohort8['model26', 'cohensd3'], cohort8['model26','SE3'], cohort8['model26', c('n', 'n_cooccur3')]),
                                                    cohort9 = c(cohort9['model26', 'cohensd3'], cohort9['model26','SE3'], cohort9['model26', c('n', 'n_cooccur3')]),
                                                    cohort10 = c(cohort10['model26', 'cohensd3'], cohort10['model26','SE3'], cohort10['model26', c('n', 'n_cooccur3')]),
                                                    cohort11 = c(cohort11['model26', 'cohensd3'], cohort11['model26','SE3'], cohort11['model26', c('n', 'n_cooccur3')])))
colnames(Model26_cooccur3_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur3')

Model27_cooccur1_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model27','cohensd1'], cohort1['model27','SE1'], cohort1['model27', c('n', 'n_cooccur1')]), 
                                                    cohort2 = c(cohort2['model27','cohensd1'], cohort2['model27','SE1'], cohort2['model27', c('n', 'n_cooccur1')]), 
                                                    cohort3 = c(cohort3['model27','cohensd1'], cohort3['model27','SE1'], cohort3['model27', c('n', 'n_cooccur1')]),
                                                    cohort4 = c(cohort4['model27', 'cohensd1'], cohort4['model27','SE1'], cohort4['model27', c('n', 'n_cooccur1')]),
                                                    cohort5 = c(cohort5['model27', 'cohensd1'], cohort5['model27','SE1'], cohort5['model27', c('n', 'n_cooccur1')]),
                                                    cohort6 = c(cohort6['model27', 'cohensd1'], cohort6['model27','SE1'], cohort6['model27', c('n', 'n_cooccur1')]),
                                                    cohort7 = c(cohort7['model27', 'cohensd1'], cohort7['model27','SE1'], cohort7['model27', c('n', 'n_cooccur1')]),
                                                    cohort8 = c(cohort8['model27', 'cohensd1'], cohort8['model27','SE1'], cohort8['model27', c('n', 'n_cooccur1')]),
                                                    cohort9 = c(cohort9['model27', 'cohensd1'], cohort9['model27','SE1'], cohort9['model27', c('n', 'n_cooccur1')]),
                                                    cohort10 = c(cohort10['model27', 'cohensd1'], cohort10['model27','SE1'], cohort10['model27', c('n', 'n_cooccur1')]),
                                                    cohort11 = c(cohort11['model27', 'cohensd1'], cohort11['model27','SE1'], cohort11['model27', c('n', 'n_cooccur1')])))
colnames(Model27_cooccur1_vs_cooccur0) <- c('cohensd', 'SE', 'n', 'n_cooccur1')

Model27_cooccur2_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model27','cohensd2'], cohort1['model27','SE2'], cohort1['model27', c('n', 'n_cooccur2')]), 
                                                    cohort2 = c(cohort2['model27','cohensd2'], cohort2['model27','SE2'], cohort2['model27', c('n', 'n_cooccur2')]), 
                                                    cohort3 = c(cohort3['model27','cohensd2'], cohort3['model27','SE2'], cohort3['model27', c('n', 'n_cooccur2')]),
                                                    cohort4 = c(cohort4['model27', 'cohensd2'], cohort4['model27','SE2'], cohort4['model27', c('n', 'n_cooccur2')]),
                                                    cohort5 = c(cohort5['model27', 'cohensd2'], cohort5['model27','SE2'], cohort5['model27', c('n', 'n_cooccur2')]),
                                                    cohort6 = c(cohort6['model27', 'cohensd2'], cohort6['model27','SE2'], cohort6['model27', c('n', 'n_cooccur2')]),
                                                    cohort7 = c(cohort7['model27', 'cohensd2'], cohort7['model27','SE2'], cohort7['model27', c('n', 'n_cooccur2')]),
                                                    cohort8 = c(cohort8['model27', 'cohensd2'], cohort8['model27','SE2'], cohort8['model27', c('n', 'n_cooccur2')]),
                                                    cohort9 = c(cohort9['model27', 'cohensd2'], cohort9['model27','SE2'], cohort9['model27', c('n', 'n_cooccur2')]),
                                                    cohort10 = c(cohort10['model27', 'cohensd2'], cohort10['model27','SE2'], cohort10['model27', c('n', 'n_cooccur2')]),
                                                    cohort11 = c(cohort11['model27', 'cohensd2'], cohort11['model27','SE2'], cohort11['model27', c('n', 'n_cooccur2')])))

colnames(Model27_cooccur2_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur2')

Model27_cooccur3_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model27','cohensd3'], cohort1['model27','SE3'], cohort1['model27', c('n', 'n_cooccur3')]), 
                                                    cohort2 = c(cohort2['model27','cohensd3'], cohort2['model27','SE3'], cohort2['model27', c('n', 'n_cooccur3')]), 
                                                    cohort3 = c(cohort3['model27','cohensd3'], cohort3['model27','SE3'], cohort3['model27', c('n', 'n_cooccur3')]),
                                                    cohort4 = c(cohort4['model27', 'cohensd3'], cohort4['model27','SE3'], cohort4['model27', c('n', 'n_cooccur3')]),
                                                    cohort5 = c(cohort5['model27', 'cohensd3'], cohort5['model27','SE3'], cohort5['model27', c('n', 'n_cooccur3')]),
                                                    cohort6 = c(cohort6['model27', 'cohensd3'], cohort6['model27','SE3'], cohort6['model27', c('n', 'n_cooccur3')]),
                                                    cohort7 = c(cohort7['model27', 'cohensd3'], cohort7['model27','SE3'], cohort7['model27', c('n', 'n_cooccur3')]),
                                                    cohort8 = c(cohort8['model27', 'cohensd3'], cohort8['model27','SE3'], cohort8['model27', c('n', 'n_cooccur3')]),
                                                    cohort9 = c(cohort9['model27', 'cohensd3'], cohort9['model27','SE3'], cohort9['model27', c('n', 'n_cooccur3')]),
                                                    cohort10 = c(cohort10['model27', 'cohensd3'], cohort10['model27','SE3'], cohort10['model27', c('n', 'n_cooccur3')]),
                                                    cohort11 = c(cohort11['model27', 'cohensd3'], cohort11['model27','SE3'], cohort11['model27', c('n', 'n_cooccur3')])))
colnames(Model27_cooccur3_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur3')

Model28_cooccur1_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model28','cohensd1'], cohort1['model28','SE1'], cohort1['model28', c('n', 'n_cooccur1')]), 
                                                    cohort2 = c(cohort2['model28','cohensd1'], cohort2['model28','SE1'], cohort2['model28', c('n', 'n_cooccur1')]), 
                                                    cohort3 = c(cohort3['model28','cohensd1'], cohort3['model28','SE1'], cohort3['model28', c('n', 'n_cooccur1')]),
                                                    cohort4 = c(cohort4['model28', 'cohensd1'], cohort4['model28','SE1'], cohort4['model28', c('n', 'n_cooccur1')]),
                                                    cohort5 = c(cohort5['model28', 'cohensd1'], cohort5['model28','SE1'], cohort5['model28', c('n', 'n_cooccur1')]),
                                                    cohort6 = c(cohort6['model28', 'cohensd1'], cohort6['model28','SE1'], cohort6['model28', c('n', 'n_cooccur1')]),
                                                    cohort7 = c(cohort7['model28', 'cohensd1'], cohort7['model28','SE1'], cohort7['model28', c('n', 'n_cooccur1')]),
                                                    cohort8 = c(cohort8['model28', 'cohensd1'], cohort8['model28','SE1'], cohort8['model28', c('n', 'n_cooccur1')]),
                                                    cohort9 = c(cohort9['model28', 'cohensd1'], cohort9['model28','SE1'], cohort9['model28', c('n', 'n_cooccur1')]),
                                                    cohort10 = c(cohort10['model28', 'cohensd1'], cohort10['model28','SE1'], cohort10['model28', c('n', 'n_cooccur1')]),
                                                    cohort11 = c(cohort11['model28', 'cohensd1'], cohort11['model28','SE1'], cohort11['model28', c('n', 'n_cooccur1')])))
colnames(Model28_cooccur1_vs_cooccur0) <- c('cohensd', 'SE', 'n', 'n_cooccur1')

Model28_cooccur2_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model28','cohensd2'], cohort1['model28','SE2'], cohort1['model28', c('n', 'n_cooccur2')]), 
                                                    cohort2 = c(cohort2['model28','cohensd2'], cohort2['model28','SE2'], cohort2['model28', c('n', 'n_cooccur2')]), 
                                                    cohort3 = c(cohort3['model28','cohensd2'], cohort3['model28','SE2'], cohort3['model28', c('n', 'n_cooccur2')]),
                                                    cohort4 = c(cohort4['model28', 'cohensd2'], cohort4['model28','SE2'], cohort4['model28', c('n', 'n_cooccur2')]),
                                                    cohort5 = c(cohort5['model28', 'cohensd2'], cohort5['model28','SE2'], cohort5['model28', c('n', 'n_cooccur2')]),
                                                    cohort6 = c(cohort6['model28', 'cohensd2'], cohort6['model28','SE2'], cohort6['model28', c('n', 'n_cooccur2')]),
                                                    cohort7 = c(cohort7['model28', 'cohensd2'], cohort7['model28','SE2'], cohort7['model28', c('n', 'n_cooccur2')]),
                                                    cohort8 = c(cohort8['model28', 'cohensd2'], cohort8['model28','SE2'], cohort8['model28', c('n', 'n_cooccur2')]),
                                                    cohort9 = c(cohort9['model28', 'cohensd2'], cohort9['model28','SE2'], cohort9['model28', c('n', 'n_cooccur2')]),
                                                    cohort10 = c(cohort10['model28', 'cohensd2'], cohort10['model28','SE2'], cohort10['model28', c('n', 'n_cooccur2')]),
                                                    cohort11 = c(cohort11['model28', 'cohensd2'], cohort11['model28','SE2'], cohort11['model28', c('n', 'n_cooccur2')])))

colnames(Model28_cooccur2_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur2')

Model28_cooccur3_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model28','cohensd3'], cohort1['model28','SE3'], cohort1['model28', c('n', 'n_cooccur3')]), 
                                                    cohort2 = c(cohort2['model28','cohensd3'], cohort2['model28','SE3'], cohort2['model28', c('n', 'n_cooccur3')]), 
                                                    cohort3 = c(cohort3['model28','cohensd3'], cohort3['model28','SE3'], cohort3['model28', c('n', 'n_cooccur3')]),
                                                    cohort4 = c(cohort4['model28', 'cohensd3'], cohort4['model28','SE3'], cohort4['model28', c('n', 'n_cooccur3')]),
                                                    cohort5 = c(cohort5['model28', 'cohensd3'], cohort5['model28','SE3'], cohort5['model28', c('n', 'n_cooccur3')]),
                                                    cohort6 = c(cohort6['model28', 'cohensd3'], cohort6['model28','SE3'], cohort6['model28', c('n', 'n_cooccur3')]),
                                                    cohort7 = c(cohort7['model28', 'cohensd3'], cohort7['model28','SE3'], cohort7['model28', c('n', 'n_cooccur3')]),
                                                    cohort8 = c(cohort8['model28', 'cohensd3'], cohort8['model28','SE3'], cohort8['model28', c('n', 'n_cooccur3')]),
                                                    cohort9 = c(cohort9['model28', 'cohensd3'], cohort9['model28','SE3'], cohort9['model28', c('n', 'n_cooccur3')]),
                                                    cohort10 = c(cohort10['model28', 'cohensd3'], cohort10['model28','SE3'], cohort10['model28', c('n', 'n_cooccur3')]),
                                                    cohort11 = c(cohort11['model28', 'cohensd3'], cohort11['model28','SE3'], cohort11['model28', c('n', 'n_cooccur3')])))
colnames(Model28_cooccur3_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur3')



#################### 10. Models 29, 30, 31, and 32 - Effect sizes of associations of cooccurrence of childhood maltreatment and depression with metabolic outcomes, in females only ###### 
Model29_cooccur1_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model29','cohensd1'], cohort1['model29','SE1'], cohort1['model29', c('n', 'n_cooccur1')]), 
                                                    cohort2 = c(cohort2['model29','cohensd1'], cohort2['model29','SE1'], cohort2['model29', c('n', 'n_cooccur1')]), 
                                                    cohort3 = c(cohort3['model29','cohensd1'], cohort3['model29','SE1'], cohort3['model29', c('n', 'n_cooccur1')]),
                                                    cohort4 = c(cohort4['model29', 'cohensd1'], cohort4['model29','SE1'], cohort4['model29', c('n', 'n_cooccur1')]),
                                                    cohort5 = c(cohort5['model29', 'cohensd1'], cohort5['model29','SE1'], cohort5['model29', c('n', 'n_cooccur1')]),
                                                    cohort6 = c(cohort6['model29', 'cohensd1'], cohort6['model29','SE1'], cohort6['model29', c('n', 'n_cooccur1')]),
                                                    cohort7 = c(cohort7['model29', 'cohensd1'], cohort7['model29','SE1'], cohort7['model29', c('n', 'n_cooccur1')]),
                                                    cohort8 = c(cohort8['model29', 'cohensd1'], cohort8['model29','SE1'], cohort8['model29', c('n', 'n_cooccur1')]),
                                                    cohort9 = c(cohort9['model29', 'cohensd1'], cohort9['model29','SE1'], cohort9['model29', c('n', 'n_cooccur1')]),
                                                    cohort10 = c(cohort10['model29', 'cohensd1'], cohort10['model29','SE1'], cohort10['model29', c('n', 'n_cooccur1')]),
                                                    cohort11 = c(cohort11['model29', 'cohensd1'], cohort11['model29','SE1'], cohort11['model29', c('n', 'n_cooccur1')])))
colnames(Model29_cooccur1_vs_cooccur0) <- c('cohensd', 'SE', 'n', 'n_cooccur1')

Model29_cooccur2_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model29','cohensd2'], cohort1['model29','SE2'], cohort1['model29', c('n', 'n_cooccur2')]), 
                                                    cohort2 = c(cohort2['model29','cohensd2'], cohort2['model29','SE2'], cohort2['model29', c('n', 'n_cooccur2')]), 
                                                    cohort3 = c(cohort3['model29','cohensd2'], cohort3['model29','SE2'], cohort3['model29', c('n', 'n_cooccur2')]),
                                                    cohort4 = c(cohort4['model29', 'cohensd2'], cohort4['model29','SE2'], cohort4['model29', c('n', 'n_cooccur2')]),
                                                    cohort5 = c(cohort5['model29', 'cohensd2'], cohort5['model29','SE2'], cohort5['model29', c('n', 'n_cooccur2')]),
                                                    cohort6 = c(cohort6['model29', 'cohensd2'], cohort6['model29','SE2'], cohort6['model29', c('n', 'n_cooccur2')]),
                                                    cohort7 = c(cohort7['model29', 'cohensd2'], cohort7['model29','SE2'], cohort7['model29', c('n', 'n_cooccur2')]),
                                                    cohort8 = c(cohort8['model29', 'cohensd2'], cohort8['model29','SE2'], cohort8['model29', c('n', 'n_cooccur2')]),
                                                    cohort9 = c(cohort9['model29', 'cohensd2'], cohort9['model29','SE2'], cohort9['model29', c('n', 'n_cooccur2')]),
                                                    cohort10 = c(cohort10['model29', 'cohensd2'], cohort10['model29','SE2'], cohort10['model29', c('n', 'n_cooccur2')]),
                                                    cohort11 = c(cohort11['model29', 'cohensd2'], cohort11['model29','SE2'], cohort11['model29', c('n', 'n_cooccur2')])))

colnames(Model29_cooccur2_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur2')

Model29_cooccur3_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model29','cohensd3'], cohort1['model29','SE3'], cohort1['model29', c('n', 'n_cooccur3')]), 
                                                    cohort2 = c(cohort2['model29','cohensd3'], cohort2['model29','SE3'], cohort2['model29', c('n', 'n_cooccur3')]), 
                                                    cohort3 = c(cohort3['model29','cohensd3'], cohort3['model29','SE3'], cohort3['model29', c('n', 'n_cooccur3')]),
                                                    cohort4 = c(cohort4['model29', 'cohensd3'], cohort4['model29','SE3'], cohort4['model29', c('n', 'n_cooccur3')]),
                                                    cohort5 = c(cohort5['model29', 'cohensd3'], cohort5['model29','SE3'], cohort5['model29', c('n', 'n_cooccur3')]),
                                                    cohort6 = c(cohort6['model29', 'cohensd3'], cohort6['model29','SE3'], cohort6['model29', c('n', 'n_cooccur3')]),
                                                    cohort7 = c(cohort7['model29', 'cohensd3'], cohort7['model29','SE3'], cohort7['model29', c('n', 'n_cooccur3')]),
                                                    cohort8 = c(cohort8['model29', 'cohensd3'], cohort8['model29','SE3'], cohort8['model29', c('n', 'n_cooccur3')]),
                                                    cohort9 = c(cohort9['model29', 'cohensd3'], cohort9['model29','SE3'], cohort9['model29', c('n', 'n_cooccur3')]),
                                                    cohort10 = c(cohort10['model29', 'cohensd3'], cohort10['model29','SE3'], cohort10['model29', c('n', 'n_cooccur3')]),
                                                    cohort11 = c(cohort11['model29', 'cohensd3'], cohort11['model29','SE3'], cohort11['model29', c('n', 'n_cooccur3')])))
colnames(Model29_cooccur3_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur3')

Model30_cooccur1_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model30','cohensd1'], cohort1['model30','SE1'], cohort1['model30', c('n', 'n_cooccur1')]), 
                                                    cohort2 = c(cohort2['model30','cohensd1'], cohort2['model30','SE1'], cohort2['model30', c('n', 'n_cooccur1')]), 
                                                    cohort3 = c(cohort3['model30','cohensd1'], cohort3['model30','SE1'], cohort3['model30', c('n', 'n_cooccur1')]),
                                                    cohort4 = c(cohort4['model30', 'cohensd1'], cohort4['model30','SE1'], cohort4['model30', c('n', 'n_cooccur1')]),
                                                    cohort5 = c(cohort5['model30', 'cohensd1'], cohort5['model30','SE1'], cohort5['model30', c('n', 'n_cooccur1')]),
                                                    cohort6 = c(cohort6['model30', 'cohensd1'], cohort6['model30','SE1'], cohort6['model30', c('n', 'n_cooccur1')]),
                                                    cohort7 = c(cohort7['model30', 'cohensd1'], cohort7['model30','SE1'], cohort7['model30', c('n', 'n_cooccur1')]),
                                                    cohort8 = c(cohort8['model30', 'cohensd1'], cohort8['model30','SE1'], cohort8['model30', c('n', 'n_cooccur1')]),
                                                    cohort9 = c(cohort9['model30', 'cohensd1'], cohort9['model30','SE1'], cohort9['model30', c('n', 'n_cooccur1')]),
                                                    cohort10 = c(cohort10['model30', 'cohensd1'], cohort10['model30','SE1'], cohort10['model30', c('n', 'n_cooccur1')]),
                                                    cohort11 = c(cohort11['model30', 'cohensd1'], cohort11['model30','SE1'], cohort11['model30', c('n', 'n_cooccur1')])))
colnames(Model30_cooccur1_vs_cooccur0) <- c('cohensd', 'SE', 'n', 'n_cooccur1')

Model30_cooccur2_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model30','cohensd2'], cohort1['model30','SE2'], cohort1['model30', c('n', 'n_cooccur2')]), 
                                                    cohort2 = c(cohort2['model30','cohensd2'], cohort2['model30','SE2'], cohort2['model30', c('n', 'n_cooccur2')]), 
                                                    cohort3 = c(cohort3['model30','cohensd2'], cohort3['model30','SE2'], cohort3['model30', c('n', 'n_cooccur2')]),
                                                    cohort4 = c(cohort4['model30', 'cohensd2'], cohort4['model30','SE2'], cohort4['model30', c('n', 'n_cooccur2')]),
                                                    cohort5 = c(cohort5['model30', 'cohensd2'], cohort5['model30','SE2'], cohort5['model30', c('n', 'n_cooccur2')]),
                                                    cohort6 = c(cohort6['model30', 'cohensd2'], cohort6['model30','SE2'], cohort6['model30', c('n', 'n_cooccur2')]),
                                                    cohort7 = c(cohort7['model30', 'cohensd2'], cohort7['model30','SE2'], cohort7['model30', c('n', 'n_cooccur2')]),
                                                    cohort8 = c(cohort8['model30', 'cohensd2'], cohort8['model30','SE2'], cohort8['model30', c('n', 'n_cooccur2')]),
                                                    cohort9 = c(cohort9['model30', 'cohensd2'], cohort9['model30','SE2'], cohort9['model30', c('n', 'n_cooccur2')]),
                                                    cohort10 = c(cohort10['model30', 'cohensd2'], cohort10['model30','SE2'], cohort10['model30', c('n', 'n_cooccur2')]),
                                                    cohort11 = c(cohort11['model30', 'cohensd2'], cohort11['model30','SE2'], cohort11['model30', c('n', 'n_cooccur2')])))

colnames(Model30_cooccur2_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur2')

Model30_cooccur3_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model30','cohensd3'], cohort1['model30','SE3'], cohort1['model30', c('n', 'n_cooccur3')]), 
                                                    cohort2 = c(cohort2['model30','cohensd3'], cohort2['model30','SE3'], cohort2['model30', c('n', 'n_cooccur3')]), 
                                                    cohort3 = c(cohort3['model30','cohensd3'], cohort3['model30','SE3'], cohort3['model30', c('n', 'n_cooccur3')]),
                                                    cohort4 = c(cohort4['model30', 'cohensd3'], cohort4['model30','SE3'], cohort4['model30', c('n', 'n_cooccur3')]),
                                                    cohort5 = c(cohort5['model30', 'cohensd3'], cohort5['model30','SE3'], cohort5['model30', c('n', 'n_cooccur3')]),
                                                    cohort6 = c(cohort6['model30', 'cohensd3'], cohort6['model30','SE3'], cohort6['model30', c('n', 'n_cooccur3')]),
                                                    cohort7 = c(cohort7['model30', 'cohensd3'], cohort7['model30','SE3'], cohort7['model30', c('n', 'n_cooccur3')]),
                                                    cohort8 = c(cohort8['model30', 'cohensd3'], cohort8['model30','SE3'], cohort8['model30', c('n', 'n_cooccur3')]),
                                                    cohort9 = c(cohort9['model30', 'cohensd3'], cohort9['model30','SE3'], cohort9['model30', c('n', 'n_cooccur3')]),
                                                    cohort10 = c(cohort10['model30', 'cohensd3'], cohort10['model30','SE3'], cohort10['model30', c('n', 'n_cooccur3')]),
                                                    cohort11 = c(cohort11['model30', 'cohensd3'], cohort11['model30','SE3'], cohort11['model30', c('n', 'n_cooccur3')])))
colnames(Model30_cooccur3_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur3')

Model31_cooccur1_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model31','cohensd1'], cohort1['model31','SE1'], cohort1['model31', c('n', 'n_cooccur1')]), 
                                                    cohort2 = c(cohort2['model31','cohensd1'], cohort2['model31','SE1'], cohort2['model31', c('n', 'n_cooccur1')]), 
                                                    cohort3 = c(cohort3['model31','cohensd1'], cohort3['model31','SE1'], cohort3['model31', c('n', 'n_cooccur1')]),
                                                    cohort4 = c(cohort4['model31', 'cohensd1'], cohort4['model31','SE1'], cohort4['model31', c('n', 'n_cooccur1')]),
                                                    cohort5 = c(cohort5['model31', 'cohensd1'], cohort5['model31','SE1'], cohort5['model31', c('n', 'n_cooccur1')]),
                                                    cohort6 = c(cohort6['model31', 'cohensd1'], cohort6['model31','SE1'], cohort6['model31', c('n', 'n_cooccur1')]),
                                                    cohort7 = c(cohort7['model31', 'cohensd1'], cohort7['model31','SE1'], cohort7['model31', c('n', 'n_cooccur1')]),
                                                    cohort8 = c(cohort8['model31', 'cohensd1'], cohort8['model31','SE1'], cohort8['model31', c('n', 'n_cooccur1')]),
                                                    cohort9 = c(cohort9['model31', 'cohensd1'], cohort9['model31','SE1'], cohort9['model31', c('n', 'n_cooccur1')]),
                                                    cohort10 = c(cohort10['model31', 'cohensd1'], cohort10['model31','SE1'], cohort10['model31', c('n', 'n_cooccur1')]),
                                                    cohort11 = c(cohort11['model31', 'cohensd1'], cohort11['model31','SE1'], cohort11['model31', c('n', 'n_cooccur1')])))
colnames(Model31_cooccur1_vs_cooccur0) <- c('cohensd', 'SE', 'n', 'n_cooccur1')

Model31_cooccur2_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model31','cohensd2'], cohort1['model31','SE2'], cohort1['model31', c('n', 'n_cooccur2')]), 
                                                    cohort2 = c(cohort2['model31','cohensd2'], cohort2['model31','SE2'], cohort2['model31', c('n', 'n_cooccur2')]), 
                                                    cohort3 = c(cohort3['model31','cohensd2'], cohort3['model31','SE2'], cohort3['model31', c('n', 'n_cooccur2')]),
                                                    cohort4 = c(cohort4['model31', 'cohensd2'], cohort4['model31','SE2'], cohort4['model31', c('n', 'n_cooccur2')]),
                                                    cohort5 = c(cohort5['model31', 'cohensd2'], cohort5['model31','SE2'], cohort5['model31', c('n', 'n_cooccur2')]),
                                                    cohort6 = c(cohort6['model31', 'cohensd2'], cohort6['model31','SE2'], cohort6['model31', c('n', 'n_cooccur2')]),
                                                    cohort7 = c(cohort7['model31', 'cohensd2'], cohort7['model31','SE2'], cohort7['model31', c('n', 'n_cooccur2')]),
                                                    cohort8 = c(cohort8['model31', 'cohensd2'], cohort8['model31','SE2'], cohort8['model31', c('n', 'n_cooccur2')]),
                                                    cohort9 = c(cohort9['model31', 'cohensd2'], cohort9['model31','SE2'], cohort9['model31', c('n', 'n_cooccur2')]),
                                                    cohort10 = c(cohort10['model31', 'cohensd2'], cohort10['model31','SE2'], cohort10['model31', c('n', 'n_cooccur2')]),
                                                    cohort11 = c(cohort11['model31', 'cohensd2'], cohort11['model31','SE2'], cohort11['model31', c('n', 'n_cooccur2')])))

colnames(Model31_cooccur2_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur2')

Model31_cooccur3_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model31','cohensd3'], cohort1['model31','SE3'], cohort1['model31', c('n', 'n_cooccur3')]), 
                                                    cohort2 = c(cohort2['model31','cohensd3'], cohort2['model31','SE3'], cohort2['model31', c('n', 'n_cooccur3')]), 
                                                    cohort3 = c(cohort3['model31','cohensd3'], cohort3['model31','SE3'], cohort3['model31', c('n', 'n_cooccur3')]),
                                                    cohort4 = c(cohort4['model31', 'cohensd3'], cohort4['model31','SE3'], cohort4['model31', c('n', 'n_cooccur3')]),
                                                    cohort5 = c(cohort5['model31', 'cohensd3'], cohort5['model31','SE3'], cohort5['model31', c('n', 'n_cooccur3')]),
                                                    cohort6 = c(cohort6['model31', 'cohensd3'], cohort6['model31','SE3'], cohort6['model31', c('n', 'n_cooccur3')]),
                                                    cohort7 = c(cohort7['model31', 'cohensd3'], cohort7['model31','SE3'], cohort7['model31', c('n', 'n_cooccur3')]),
                                                    cohort8 = c(cohort8['model31', 'cohensd3'], cohort8['model31','SE3'], cohort8['model31', c('n', 'n_cooccur3')]),
                                                    cohort9 = c(cohort9['model31', 'cohensd3'], cohort9['model31','SE3'], cohort9['model31', c('n', 'n_cooccur3')]),
                                                    cohort10 = c(cohort10['model31', 'cohensd3'], cohort10['model31','SE3'], cohort10['model31', c('n', 'n_cooccur3')]),
                                                    cohort11 = c(cohort11['model31', 'cohensd3'], cohort11['model31','SE3'], cohort11['model31', c('n', 'n_cooccur3')])))
colnames(Model31_cooccur3_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur3')

Model32_cooccur1_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model32','cohensd1'], cohort1['model32','SE1'], cohort1['model32', c('n', 'n_cooccur1')]), 
                                                    cohort2 = c(cohort2['model32','cohensd1'], cohort2['model32','SE1'], cohort2['model32', c('n', 'n_cooccur1')]), 
                                                    cohort3 = c(cohort3['model32','cohensd1'], cohort3['model32','SE1'], cohort3['model32', c('n', 'n_cooccur1')]),
                                                    cohort4 = c(cohort4['model32', 'cohensd1'], cohort4['model32','SE1'], cohort4['model32', c('n', 'n_cooccur1')]),
                                                    cohort5 = c(cohort5['model32', 'cohensd1'], cohort5['model32','SE1'], cohort5['model32', c('n', 'n_cooccur1')]),
                                                    cohort6 = c(cohort6['model32', 'cohensd1'], cohort6['model32','SE1'], cohort6['model32', c('n', 'n_cooccur1')]),
                                                    cohort7 = c(cohort7['model32', 'cohensd1'], cohort7['model32','SE1'], cohort7['model32', c('n', 'n_cooccur1')]),
                                                    cohort8 = c(cohort8['model32', 'cohensd1'], cohort8['model32','SE1'], cohort8['model32', c('n', 'n_cooccur1')]),
                                                    cohort9 = c(cohort9['model32', 'cohensd1'], cohort9['model32','SE1'], cohort9['model32', c('n', 'n_cooccur1')]),
                                                    cohort10 = c(cohort10['model32', 'cohensd1'], cohort10['model32','SE1'], cohort10['model32', c('n', 'n_cooccur1')]),
                                                    cohort11 = c(cohort11['model32', 'cohensd1'], cohort11['model32','SE1'], cohort11['model32', c('n', 'n_cooccur1')])))
colnames(Model32_cooccur1_vs_cooccur0) <- c('cohensd', 'SE', 'n', 'n_cooccur1')

Model32_cooccur2_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model32','cohensd2'], cohort1['model32','SE2'], cohort1['model32', c('n', 'n_cooccur2')]), 
                                                    cohort2 = c(cohort2['model32','cohensd2'], cohort2['model32','SE2'], cohort2['model32', c('n', 'n_cooccur2')]), 
                                                    cohort3 = c(cohort3['model32','cohensd2'], cohort3['model32','SE2'], cohort3['model32', c('n', 'n_cooccur2')]),
                                                    cohort4 = c(cohort4['model32', 'cohensd2'], cohort4['model32','SE2'], cohort4['model32', c('n', 'n_cooccur2')]),
                                                    cohort5 = c(cohort5['model32', 'cohensd2'], cohort5['model32','SE2'], cohort5['model32', c('n', 'n_cooccur2')]),
                                                    cohort6 = c(cohort6['model32', 'cohensd2'], cohort6['model32','SE2'], cohort6['model32', c('n', 'n_cooccur2')]),
                                                    cohort7 = c(cohort7['model32', 'cohensd2'], cohort7['model32','SE2'], cohort7['model32', c('n', 'n_cooccur2')]),
                                                    cohort8 = c(cohort8['model32', 'cohensd2'], cohort8['model32','SE2'], cohort8['model32', c('n', 'n_cooccur2')]),
                                                    cohort9 = c(cohort9['model32', 'cohensd2'], cohort9['model32','SE2'], cohort9['model32', c('n', 'n_cooccur2')]),
                                                    cohort10 = c(cohort10['model32', 'cohensd2'], cohort10['model32','SE2'], cohort10['model32', c('n', 'n_cooccur2')]),
                                                    cohort11 = c(cohort11['model32', 'cohensd2'], cohort11['model32','SE2'], cohort11['model32', c('n', 'n_cooccur2')])))

colnames(Model32_cooccur2_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur2')

Model32_cooccur3_vs_cooccur0 <- as.data.frame(rbind(cohort1 = c(cohort1['model32','cohensd3'], cohort1['model32','SE3'], cohort1['model32', c('n', 'n_cooccur3')]), 
                                                    cohort2 = c(cohort2['model32','cohensd3'], cohort2['model32','SE3'], cohort2['model32', c('n', 'n_cooccur3')]), 
                                                    cohort3 = c(cohort3['model32','cohensd3'], cohort3['model32','SE3'], cohort3['model32', c('n', 'n_cooccur3')]),
                                                    cohort4 = c(cohort4['model32', 'cohensd3'], cohort4['model32','SE3'], cohort4['model32', c('n', 'n_cooccur3')]),
                                                    cohort5 = c(cohort5['model32', 'cohensd3'], cohort5['model32','SE3'], cohort5['model32', c('n', 'n_cooccur3')]),
                                                    cohort6 = c(cohort6['model32', 'cohensd3'], cohort6['model32','SE3'], cohort6['model32', c('n', 'n_cooccur3')]),
                                                    cohort7 = c(cohort7['model32', 'cohensd3'], cohort7['model32','SE3'], cohort7['model32', c('n', 'n_cooccur3')]),
                                                    cohort8 = c(cohort8['model32', 'cohensd3'], cohort8['model32','SE3'], cohort8['model32', c('n', 'n_cooccur3')]),
                                                    cohort9 = c(cohort9['model32', 'cohensd3'], cohort9['model32','SE3'], cohort9['model32', c('n', 'n_cooccur3')]),
                                                    cohort10 = c(cohort10['model32', 'cohensd3'], cohort10['model32','SE3'], cohort10['model32', c('n', 'n_cooccur3')]),
                                                    cohort11 = c(cohort11['model32', 'cohensd3'], cohort11['model32','SE3'], cohort11['model32', c('n', 'n_cooccur3')])))
colnames(Model32_cooccur3_vs_cooccur0) <- c('cohensd', 'SE','n','n_cooccur3')



#################### 11. Estimated means and SE of groups (categorical dependent variables) from models 13, 14, 15, and 16 #################### 
Model13_emm <- as.data.frame(rbind(cohort1_cooccur0 = c(cohort1['model13','emm_cooccur0'], cohort1['model13','emm.SE_cooccur0'], cohort1['model13','emm_cooccur0']-1.96*cohort1['model13','emm.SE_cooccur0'], cohort1['model13','emm_cooccur0']+1.96*cohort1['model13','emm.SE_cooccur0'], 'cohort1', 'no_cm_no_mdd', cohort1['model13', 'n_cooccur0'], NA, NA),
                               cohort1_cooccur1 = c(cohort1['model13','emm_cooccur1'], cohort1['model13','emm.SE_cooccur1'], cohort1['model13','emm_cooccur1']-1.96*cohort1['model13','emm.SE_cooccur1'], cohort1['model13','emm_cooccur1']+1.96*cohort1['model13','emm.SE_cooccur1'], 'cohort1', 'cm_only', cohort1['model13', 'n_cooccur1'], cohort1['model13', c('cohensd1', 'SE1')]),
                               cohort1_cooccur2 = c(cohort1['model13','emm_cooccur2'], cohort1['model13','emm.SE_cooccur2'], cohort1['model13','emm_cooccur2']-1.96*cohort1['model13','emm.SE_cooccur2'], cohort1['model13','emm_cooccur2']+1.96*cohort1['model13','emm.SE_cooccur2'], 'cohort1', 'mdd_only', cohort1['model13', 'n_cooccur2'], cohort1['model13', c('cohensd2', 'SE2')]),
                               cohort1_cooccur3 = c(cohort1['model13','emm_cooccur3'], cohort1['model13','emm.SE_cooccur3'], cohort1['model13','emm_cooccur3']-1.96*cohort1['model13','emm.SE_cooccur3'], cohort1['model13','emm_cooccur3']+1.96*cohort1['model13','emm.SE_cooccur3'], 'cohort1', 'cm_and_mdd', cohort1['model13', 'n_cooccur3'], cohort1['model13', c('cohensd3', 'SE3')]),
                               
                               cohort2_cooccur0 = c(cohort2['model13','emm_cooccur0'], cohort2['model13','emm.SE_cooccur0'], cohort2['model13','emm_cooccur0']-1.96*cohort2['model13','emm.SE_cooccur0'], cohort2['model13','emm_cooccur0']+1.96*cohort2['model13','emm.SE_cooccur0'], 'cohort2', 'no_cm_no_mdd', cohort2['model13', 'n_cooccur0'], NA, NA),
                               cohort2_cooccur1 = c(cohort2['model13','emm_cooccur1'], cohort2['model13','emm.SE_cooccur1'], cohort2['model13','emm_cooccur1']-1.96*cohort2['model13','emm.SE_cooccur1'], cohort2['model13','emm_cooccur1']+1.96*cohort2['model13','emm.SE_cooccur1'], 'cohort2', 'cm_only', cohort2['model13', 'n_cooccur1'], cohort2['model13', c('cohensd1', 'SE1')]),
                               cohort2_cooccur2 = c(cohort2['model13','emm_cooccur2'], cohort2['model13','emm.SE_cooccur2'], cohort2['model13','emm_cooccur2']-1.96*cohort2['model13','emm.SE_cooccur2'], cohort2['model13','emm_cooccur2']+1.96*cohort2['model13','emm.SE_cooccur2'], 'cohort2', 'mdd_only', cohort2['model13', 'n_cooccur2'], cohort2['model13', c('cohensd2', 'SE2')]),
                               cohort2_cooccur3 = c(cohort2['model13','emm_cooccur3'], cohort2['model13','emm.SE_cooccur3'], cohort2['model13','emm_cooccur3']-1.96*cohort2['model13','emm.SE_cooccur3'], cohort2['model13','emm_cooccur3']+1.96*cohort2['model13','emm.SE_cooccur3'], 'cohort2', 'cm_and_mdd', cohort2['model13', 'n_cooccur3'], cohort2['model13', c('cohensd3', 'SE3')]),
                               
                               cohort3_cooccur0 = c(cohort3['model13','emm_cooccur0'], cohort3['model13','emm.SE_cooccur0'], cohort3['model13','emm_cooccur0']-1.96*cohort3['model13','emm.SE_cooccur0'], cohort3['model13','emm_cooccur0']+1.96*cohort3['model13','emm.SE_cooccur0'], 'cohort3', 'no_cm_no_mdd', cohort3['model13', 'n_cooccur0'], NA, NA),
                               cohort3_cooccur1 = c(cohort3['model13','emm_cooccur1'], cohort3['model13','emm.SE_cooccur1'], cohort3['model13','emm_cooccur1']-1.96*cohort3['model13','emm.SE_cooccur1'], cohort3['model13','emm_cooccur1']+1.96*cohort3['model13','emm.SE_cooccur1'], 'cohort3', 'cm_only', cohort3['model13', 'n_cooccur1'], cohort3['model13', c('cohensd1', 'SE1')]),
                               cohort3_cooccur2 = c(cohort3['model13','emm_cooccur2'], cohort3['model13','emm.SE_cooccur2'], cohort3['model13','emm_cooccur2']-1.96*cohort3['model13','emm.SE_cooccur2'], cohort3['model13','emm_cooccur2']+1.96*cohort3['model13','emm.SE_cooccur2'], 'cohort3', 'mdd_only', cohort3['model13', 'n_cooccur2'], cohort3['model13', c('cohensd2', 'SE2')]),
                               cohort3_cooccur3 = c(cohort3['model13','emm_cooccur3'], cohort3['model13','emm.SE_cooccur3'], cohort3['model13','emm_cooccur3']-1.96*cohort3['model13','emm.SE_cooccur3'], cohort3['model13','emm_cooccur3']+1.96*cohort3['model13','emm.SE_cooccur3'], 'cohort3', 'cm_and_mdd', cohort3['model13', 'n_cooccur3'], cohort3['model13', c('cohensd3', 'SE3')]),
                               
                               cohort4_cooccur0 = c(cohort4['model13','emm_cooccur0'], cohort4['model13','emm.SE_cooccur0'], cohort4['model13','emm_cooccur0']-1.96*cohort4['model13','emm.SE_cooccur0'], cohort4['model13','emm_cooccur0']+1.96*cohort4['model13','emm.SE_cooccur0'], 'cohort4', 'no_cm_no_mdd', cohort4['model13', 'n_cooccur0'], NA, NA),
                               cohort4_cooccur1 = c(cohort4['model13','emm_cooccur1'], cohort4['model13','emm.SE_cooccur1'], cohort4['model13','emm_cooccur1']-1.96*cohort4['model13','emm.SE_cooccur1'], cohort4['model13','emm_cooccur1']+1.96*cohort4['model13','emm.SE_cooccur1'], 'cohort4', 'cm_only', cohort4['model13', 'n_cooccur1'], cohort4['model13', c('cohensd1', 'SE1')]),
                               cohort4_cooccur2 = c(cohort4['model13','emm_cooccur2'], cohort4['model13','emm.SE_cooccur2'], cohort4['model13','emm_cooccur2']-1.96*cohort4['model13','emm.SE_cooccur2'], cohort4['model13','emm_cooccur2']+1.96*cohort4['model13','emm.SE_cooccur2'], 'cohort4', 'mdd_only', cohort4['model13', 'n_cooccur2'], cohort4['model13', c('cohensd2', 'SE2')]),
                               cohort4_cooccur3 = c(cohort4['model13','emm_cooccur3'], cohort4['model13','emm.SE_cooccur3'], cohort4['model13','emm_cooccur3']-1.96*cohort4['model13','emm.SE_cooccur3'], cohort4['model13','emm_cooccur3']+1.96*cohort4['model13','emm.SE_cooccur3'], 'cohort4', 'cm_and_mdd', cohort4['model13', 'n_cooccur3'], cohort4['model13', c('cohensd3', 'SE3')]),
                               
                               cohort5_cooccur0 = c(cohort5['model13','emm_cooccur0'], cohort5['model13','emm.SE_cooccur0'], cohort5['model13','emm_cooccur0']-1.96*cohort5['model13','emm.SE_cooccur0'], cohort5['model13','emm_cooccur0']+1.96*cohort5['model13','emm.SE_cooccur0'], 'cohort5', 'no_cm_no_mdd', cohort5['model13', 'n_cooccur0'], NA, NA),
                               cohort5_cooccur1 = c(cohort5['model13','emm_cooccur1'], cohort5['model13','emm.SE_cooccur1'], cohort5['model13','emm_cooccur1']-1.96*cohort5['model13','emm.SE_cooccur1'], cohort5['model13','emm_cooccur1']+1.96*cohort5['model13','emm.SE_cooccur1'], 'cohort5', 'cm_only', cohort5['model13', 'n_cooccur1'], cohort5['model13', c('cohensd1', 'SE1')]),
                               cohort5_cooccur2 = c(cohort5['model13','emm_cooccur2'], cohort5['model13','emm.SE_cooccur2'], cohort5['model13','emm_cooccur2']-1.96*cohort5['model13','emm.SE_cooccur2'], cohort5['model13','emm_cooccur2']+1.96*cohort5['model13','emm.SE_cooccur2'], 'cohort5', 'mdd_only', cohort5['model13', 'n_cooccur2'], cohort5['model13', c('cohensd2', 'SE2')]),
                               cohort5_cooccur3 = c(cohort5['model13','emm_cooccur3'], cohort5['model13','emm.SE_cooccur3'], cohort5['model13','emm_cooccur3']-1.96*cohort5['model13','emm.SE_cooccur3'], cohort5['model13','emm_cooccur3']+1.96*cohort5['model13','emm.SE_cooccur3'], 'cohort5', 'cm_and_mdd', cohort5['model13', 'n_cooccur3'], cohort5['model13', c('cohensd3', 'SE3')]),

                               cohort6_cooccur0 = c(cohort6['model13','emm_cooccur0'], cohort6['model13','emm.SE_cooccur0'], cohort6['model13','emm_cooccur0']-1.96*cohort6['model13','emm.SE_cooccur0'], cohort6['model13','emm_cooccur0']+1.96*cohort6['model13','emm.SE_cooccur0'], 'cohort6', 'no_cm_no_mdd', cohort6['model13', 'n_cooccur0'], NA, NA),
                               cohort6_cooccur1 = c(cohort6['model13','emm_cooccur1'], cohort6['model13','emm.SE_cooccur1'], cohort6['model13','emm_cooccur1']-1.96*cohort6['model13','emm.SE_cooccur1'], cohort6['model13','emm_cooccur1']+1.96*cohort6['model13','emm.SE_cooccur1'], 'cohort6', 'cm_only', cohort6['model13', 'n_cooccur1'], cohort6['model13', c('cohensd1', 'SE1')]),
                               cohort6_cooccur2 = c(cohort6['model13','emm_cooccur2'], cohort6['model13','emm.SE_cooccur2'], cohort6['model13','emm_cooccur2']-1.96*cohort6['model13','emm.SE_cooccur2'], cohort6['model13','emm_cooccur2']+1.96*cohort6['model13','emm.SE_cooccur2'], 'cohort6', 'mdd_only', cohort6['model13', 'n_cooccur2'], cohort6['model13', c('cohensd2', 'SE2')]),
                               cohort6_cooccur3 = c(cohort6['model13','emm_cooccur3'], cohort6['model13','emm.SE_cooccur3'], cohort6['model13','emm_cooccur3']-1.96*cohort6['model13','emm.SE_cooccur3'], cohort6['model13','emm_cooccur3']+1.96*cohort6['model13','emm.SE_cooccur3'], 'cohort6', 'cm_and_mdd', cohort6['model13', 'n_cooccur3'], cohort6['model13', c('cohensd3', 'SE3')]),

                               cohort7_cooccur0 = c(cohort7['model13','emm_cooccur0'], cohort7['model13','emm.SE_cooccur0'], cohort7['model13','emm_cooccur0']-1.96*cohort7['model13','emm.SE_cooccur0'], cohort7['model13','emm_cooccur0']+1.96*cohort7['model13','emm.SE_cooccur0'], 'cohort7', 'no_cm_no_mdd', cohort7['model13', 'n_cooccur0'], NA, NA),
                               cohort7_cooccur1 = c(cohort7['model13','emm_cooccur1'], cohort7['model13','emm.SE_cooccur1'], cohort7['model13','emm_cooccur1']-1.96*cohort7['model13','emm.SE_cooccur1'], cohort7['model13','emm_cooccur1']+1.96*cohort7['model13','emm.SE_cooccur1'], 'cohort7', 'cm_only', cohort7['model13', 'n_cooccur1'], cohort7['model13', c('cohensd1', 'SE1')]),
                               cohort7_cooccur2 = c(cohort7['model13','emm_cooccur2'], cohort7['model13','emm.SE_cooccur2'], cohort7['model13','emm_cooccur2']-1.96*cohort7['model13','emm.SE_cooccur2'], cohort7['model13','emm_cooccur2']+1.96*cohort7['model13','emm.SE_cooccur2'], 'cohort7', 'mdd_only', cohort7['model13', 'n_cooccur2'], cohort7['model13', c('cohensd2', 'SE2')]),
                               cohort7_cooccur3 = c(cohort7['model13','emm_cooccur3'], cohort7['model13','emm.SE_cooccur3'], cohort7['model13','emm_cooccur3']-1.96*cohort7['model13','emm.SE_cooccur3'], cohort7['model13','emm_cooccur3']+1.96*cohort7['model13','emm.SE_cooccur3'], 'cohort7', 'cm_and_mdd', cohort7['model13', 'n_cooccur3'], cohort7['model13', c('cohensd3', 'SE3')]),

                               cohort8_cooccur0 = c(cohort8['model13','emm_cooccur0'], cohort8['model13','emm.SE_cooccur0'], cohort8['model13','emm_cooccur0']-1.96*cohort8['model13','emm.SE_cooccur0'], cohort8['model13','emm_cooccur0']+1.96*cohort8['model13','emm.SE_cooccur0'], 'cohort8', 'no_cm_no_mdd', cohort8['model13', 'n_cooccur0'], NA, NA),
                               cohort8_cooccur1 = c(cohort8['model13','emm_cooccur1'], cohort8['model13','emm.SE_cooccur1'], cohort8['model13','emm_cooccur1']-1.96*cohort8['model13','emm.SE_cooccur1'], cohort8['model13','emm_cooccur1']+1.96*cohort8['model13','emm.SE_cooccur1'], 'cohort8', 'cm_only', cohort8['model13', 'n_cooccur1'], cohort8['model13', c('cohensd1', 'SE1')]),
                               cohort8_cooccur2 = c(cohort8['model13','emm_cooccur2'], cohort8['model13','emm.SE_cooccur2'], cohort8['model13','emm_cooccur2']-1.96*cohort8['model13','emm.SE_cooccur2'], cohort8['model13','emm_cooccur2']+1.96*cohort8['model13','emm.SE_cooccur2'], 'cohort8', 'mdd_only', cohort8['model13', 'n_cooccur2'], cohort8['model13', c('cohensd2', 'SE2')]),
                               cohort8_cooccur3 = c(cohort8['model13','emm_cooccur3'], cohort8['model13','emm.SE_cooccur3'], cohort8['model13','emm_cooccur3']-1.96*cohort8['model13','emm.SE_cooccur3'], cohort8['model13','emm_cooccur3']+1.96*cohort8['model13','emm.SE_cooccur3'], 'cohort8', 'cm_and_mdd', cohort8['model13', 'n_cooccur3'], cohort8['model13', c('cohensd3', 'SE3')]),

                               cohort9_cooccur0 = c(cohort9['model13','emm_cooccur0'], cohort9['model13','emm.SE_cooccur0'], cohort9['model13','emm_cooccur0']-1.96*cohort9['model13','emm.SE_cooccur0'], cohort9['model13','emm_cooccur0']+1.96*cohort9['model13','emm.SE_cooccur0'], 'cohort9', 'no_cm_no_mdd', cohort9['model13', 'n_cooccur0'], NA, NA),
                               cohort9_cooccur1 = c(cohort9['model13','emm_cooccur1'], cohort9['model13','emm.SE_cooccur1'], cohort9['model13','emm_cooccur1']-1.96*cohort9['model13','emm.SE_cooccur1'], cohort9['model13','emm_cooccur1']+1.96*cohort9['model13','emm.SE_cooccur1'], 'cohort9', 'cm_only', cohort9['model13', 'n_cooccur1'], cohort9['model13', c('cohensd1', 'SE1')]),
                               cohort9_cooccur2 = c(cohort9['model13','emm_cooccur2'], cohort9['model13','emm.SE_cooccur2'], cohort9['model13','emm_cooccur2']-1.96*cohort9['model13','emm.SE_cooccur2'], cohort9['model13','emm_cooccur2']+1.96*cohort9['model13','emm.SE_cooccur2'], 'cohort9', 'mdd_only', cohort9['model13', 'n_cooccur2'], cohort9['model13', c('cohensd2', 'SE2')]),
                               cohort9_cooccur3 = c(cohort9['model13','emm_cooccur3'], cohort9['model13','emm.SE_cooccur3'], cohort9['model13','emm_cooccur3']-1.96*cohort9['model13','emm.SE_cooccur3'], cohort9['model13','emm_cooccur3']+1.96*cohort9['model13','emm.SE_cooccur3'], 'cohort9', 'cm_and_mdd', cohort9['model13', 'n_cooccur3'], cohort9['model13', c('cohensd3', 'SE3')]),

                               cohort10_cooccur0 = c(cohort10['model13','emm_cooccur0'], cohort10['model13','emm.SE_cooccur0'], cohort10['model13','emm_cooccur0']-1.96*cohort10['model13','emm.SE_cooccur0'], cohort10['model13','emm_cooccur0']+1.96*cohort10['model13','emm.SE_cooccur0'], 'cohort10', 'no_cm_no_mdd', cohort10['model13', 'n_cooccur0'], NA, NA),
                               cohort10_cooccur1 = c(cohort10['model13','emm_cooccur1'], cohort10['model13','emm.SE_cooccur1'], cohort10['model13','emm_cooccur1']-1.96*cohort10['model13','emm.SE_cooccur1'], cohort10['model13','emm_cooccur1']+1.96*cohort10['model13','emm.SE_cooccur1'], 'cohort10', 'cm_only', cohort10['model13', 'n_cooccur1'], cohort10['model13', c('cohensd1', 'SE1')]),
                               cohort10_cooccur2 = c(cohort10['model13','emm_cooccur2'], cohort10['model13','emm.SE_cooccur2'], cohort10['model13','emm_cooccur2']-1.96*cohort10['model13','emm.SE_cooccur2'], cohort10['model13','emm_cooccur2']+1.96*cohort10['model13','emm.SE_cooccur2'], 'cohort10', 'mdd_only', cohort10['model13', 'n_cooccur2'], cohort10['model13', c('cohensd2', 'SE2')]),
                               cohort10_cooccur3 = c(cohort10['model13','emm_cooccur3'], cohort10['model13','emm.SE_cooccur3'], cohort10['model13','emm_cooccur3']-1.96*cohort10['model13','emm.SE_cooccur3'], cohort10['model13','emm_cooccur3']+1.96*cohort10['model13','emm.SE_cooccur3'], 'cohort10', 'cm_and_mdd', cohort10['model13', 'n_cooccur3'], cohort10['model13', c('cohensd3', 'SE3')]),

                               cohort11_cooccur0 = c(cohort11['model13','emm_cooccur0'], cohort11['model13','emm.SE_cooccur0'], cohort11['model13','emm_cooccur0']-1.96*cohort11['model13','emm.SE_cooccur0'], cohort11['model13','emm_cooccur0']+1.96*cohort11['model13','emm.SE_cooccur0'], 'cohort11', 'no_cm_no_mdd', cohort11['model13', 'n_cooccur0'], NA, NA),
                               cohort11_cooccur1 = c(cohort11['model13','emm_cooccur1'], cohort11['model13','emm.SE_cooccur1'], cohort11['model13','emm_cooccur1']-1.96*cohort11['model13','emm.SE_cooccur1'], cohort11['model13','emm_cooccur1']+1.96*cohort11['model13','emm.SE_cooccur1'], 'cohort11', 'cm_only', cohort11['model13', 'n_cooccur0'], cohort11['model13', c('cohensd1', 'SE1')]),
                               cohort11_cooccur2 = c(cohort11['model13','emm_cooccur2'], cohort11['model13','emm.SE_cooccur2'], cohort11['model13','emm_cooccur2']-1.96*cohort11['model13','emm.SE_cooccur2'], cohort11['model13','emm_cooccur2']+1.96*cohort11['model13','emm.SE_cooccur2'], 'cohort11', 'mdd_only', cohort11['model13', 'n_cooccur0'], cohort11['model13', c('cohensd2', 'SE2')]),
                               cohort11_cooccur3 = c(cohort11['model13','emm_cooccur3'], cohort11['model13','emm.SE_cooccur3'], cohort11['model13','emm_cooccur3']-1.96*cohort11['model13','emm.SE_cooccur3'], cohort11['model13','emm_cooccur3']+1.96*cohort11['model13','emm.SE_cooccur3'], 'cohort11', 'cm_and_mdd', cohort11['model13', 'n_cooccur0'], cohort11['model13', c('cohensd3', 'SE3')])))

colnames(Model13_emm) <- c('emm', 'SE','CI_lb','CI_ub','cohort','group', 'n', 'cohensd', 'SE_cohensd')

Model13_emm$group <- factor(Model13_emm$group, levels = c("no_cm_no_mdd","cm_only","mdd_only","cm_and_mdd"))


Model14_emm <- as.data.frame(rbind(cohort1_cooccur0 = c(cohort1['model14','emm_cooccur0'], cohort1['model14','emm.SE_cooccur0'], cohort1['model14','emm_cooccur0']-1.96*cohort1['model14','emm.SE_cooccur0'], cohort1['model14','emm_cooccur0']+1.96*cohort1['model14','emm.SE_cooccur0'], 'cohort1', 'no_cm_no_mdd', cohort1['model14', 'n_cooccur0'], NA, NA),
                                   cohort1_cooccur1 = c(cohort1['model14','emm_cooccur1'], cohort1['model14','emm.SE_cooccur1'], cohort1['model14','emm_cooccur1']-1.96*cohort1['model14','emm.SE_cooccur1'], cohort1['model14','emm_cooccur1']+1.96*cohort1['model14','emm.SE_cooccur1'], 'cohort1', 'cm_only', cohort1['model14', 'n_cooccur1'], cohort1['model14', c('cohensd1', 'SE1')]),
                                   cohort1_cooccur2 = c(cohort1['model14','emm_cooccur2'], cohort1['model14','emm.SE_cooccur2'], cohort1['model14','emm_cooccur2']-1.96*cohort1['model14','emm.SE_cooccur2'], cohort1['model14','emm_cooccur2']+1.96*cohort1['model14','emm.SE_cooccur2'], 'cohort1', 'mdd_only', cohort1['model14', 'n_cooccur2'], cohort1['model14', c('cohensd2', 'SE2')]),
                                   cohort1_cooccur3 = c(cohort1['model14','emm_cooccur3'], cohort1['model14','emm.SE_cooccur3'], cohort1['model14','emm_cooccur3']-1.96*cohort1['model14','emm.SE_cooccur3'], cohort1['model14','emm_cooccur3']+1.96*cohort1['model14','emm.SE_cooccur3'], 'cohort1', 'cm_and_mdd', cohort1['model14', 'n_cooccur3'], cohort1['model14', c('cohensd3', 'SE3')]),
                                   
                                   cohort2_cooccur0 = c(cohort2['model14','emm_cooccur0'], cohort2['model14','emm.SE_cooccur0'], cohort2['model14','emm_cooccur0']-1.96*cohort2['model14','emm.SE_cooccur0'], cohort2['model14','emm_cooccur0']+1.96*cohort2['model14','emm.SE_cooccur0'], 'cohort2', 'no_cm_no_mdd', cohort2['model14', 'n_cooccur0'], NA, NA),
                                   cohort2_cooccur1 = c(cohort2['model14','emm_cooccur1'], cohort2['model14','emm.SE_cooccur1'], cohort2['model14','emm_cooccur1']-1.96*cohort2['model14','emm.SE_cooccur1'], cohort2['model14','emm_cooccur1']+1.96*cohort2['model14','emm.SE_cooccur1'], 'cohort2', 'cm_only', cohort2['model14', 'n_cooccur1'], cohort2['model14', c('cohensd1', 'SE1')]),
                                   cohort2_cooccur2 = c(cohort2['model14','emm_cooccur2'], cohort2['model14','emm.SE_cooccur2'], cohort2['model14','emm_cooccur2']-1.96*cohort2['model14','emm.SE_cooccur2'], cohort2['model14','emm_cooccur2']+1.96*cohort2['model14','emm.SE_cooccur2'], 'cohort2', 'mdd_only', cohort2['model14', 'n_cooccur2'], cohort2['model14', c('cohensd2', 'SE2')]),
                                   cohort2_cooccur3 = c(cohort2['model14','emm_cooccur3'], cohort2['model14','emm.SE_cooccur3'], cohort2['model14','emm_cooccur3']-1.96*cohort2['model14','emm.SE_cooccur3'], cohort2['model14','emm_cooccur3']+1.96*cohort2['model14','emm.SE_cooccur3'], 'cohort2', 'cm_and_mdd', cohort2['model14', 'n_cooccur3'], cohort2['model14', c('cohensd3', 'SE3')]),
                                   
                                   cohort3_cooccur0 = c(cohort3['model14','emm_cooccur0'], cohort3['model14','emm.SE_cooccur0'], cohort3['model14','emm_cooccur0']-1.96*cohort3['model14','emm.SE_cooccur0'], cohort3['model14','emm_cooccur0']+1.96*cohort3['model14','emm.SE_cooccur0'], 'cohort3', 'no_cm_no_mdd', cohort3['model14', 'n_cooccur0'], NA, NA),
                                   cohort3_cooccur1 = c(cohort3['model14','emm_cooccur1'], cohort3['model14','emm.SE_cooccur1'], cohort3['model14','emm_cooccur1']-1.96*cohort3['model14','emm.SE_cooccur1'], cohort3['model14','emm_cooccur1']+1.96*cohort3['model14','emm.SE_cooccur1'], 'cohort3', 'cm_only', cohort3['model14', 'n_cooccur1'], cohort3['model14', c('cohensd1', 'SE1')]),
                                   cohort3_cooccur2 = c(cohort3['model14','emm_cooccur2'], cohort3['model14','emm.SE_cooccur2'], cohort3['model14','emm_cooccur2']-1.96*cohort3['model14','emm.SE_cooccur2'], cohort3['model14','emm_cooccur2']+1.96*cohort3['model14','emm.SE_cooccur2'], 'cohort3', 'mdd_only', cohort3['model14', 'n_cooccur2'], cohort3['model14', c('cohensd2', 'SE2')]),
                                   cohort3_cooccur3 = c(cohort3['model14','emm_cooccur3'], cohort3['model14','emm.SE_cooccur3'], cohort3['model14','emm_cooccur3']-1.96*cohort3['model14','emm.SE_cooccur3'], cohort3['model14','emm_cooccur3']+1.96*cohort3['model14','emm.SE_cooccur3'], 'cohort3', 'cm_and_mdd', cohort3['model14', 'n_cooccur3'], cohort3['model14', c('cohensd3', 'SE3')]),
                                   
                                   cohort4_cooccur0 = c(cohort4['model14','emm_cooccur0'], cohort4['model14','emm.SE_cooccur0'], cohort4['model14','emm_cooccur0']-1.96*cohort4['model14','emm.SE_cooccur0'], cohort4['model14','emm_cooccur0']+1.96*cohort4['model14','emm.SE_cooccur0'], 'cohort4', 'no_cm_no_mdd', cohort4['model14', 'n_cooccur0'], NA, NA),
                                   cohort4_cooccur1 = c(cohort4['model14','emm_cooccur1'], cohort4['model14','emm.SE_cooccur1'], cohort4['model14','emm_cooccur1']-1.96*cohort4['model14','emm.SE_cooccur1'], cohort4['model14','emm_cooccur1']+1.96*cohort4['model14','emm.SE_cooccur1'], 'cohort4', 'cm_only', cohort4['model14', 'n_cooccur1'], cohort4['model14', c('cohensd1', 'SE1')]),
                                   cohort4_cooccur2 = c(cohort4['model14','emm_cooccur2'], cohort4['model14','emm.SE_cooccur2'], cohort4['model14','emm_cooccur2']-1.96*cohort4['model14','emm.SE_cooccur2'], cohort4['model14','emm_cooccur2']+1.96*cohort4['model14','emm.SE_cooccur2'], 'cohort4', 'mdd_only', cohort4['model14', 'n_cooccur2'], cohort4['model14', c('cohensd2', 'SE2')]),
                                   cohort4_cooccur3 = c(cohort4['model14','emm_cooccur3'], cohort4['model14','emm.SE_cooccur3'], cohort4['model14','emm_cooccur3']-1.96*cohort4['model14','emm.SE_cooccur3'], cohort4['model14','emm_cooccur3']+1.96*cohort4['model14','emm.SE_cooccur3'], 'cohort4', 'cm_and_mdd', cohort4['model14', 'n_cooccur3'], cohort4['model14', c('cohensd3', 'SE3')]),
                                   
                                   cohort5_cooccur0 = c(cohort5['model14','emm_cooccur0'], cohort5['model14','emm.SE_cooccur0'], cohort5['model14','emm_cooccur0']-1.96*cohort5['model14','emm.SE_cooccur0'], cohort5['model14','emm_cooccur0']+1.96*cohort5['model14','emm.SE_cooccur0'], 'cohort5', 'no_cm_no_mdd', cohort5['model14', 'n_cooccur0'], NA, NA),
                                   cohort5_cooccur1 = c(cohort5['model14','emm_cooccur1'], cohort5['model14','emm.SE_cooccur1'], cohort5['model14','emm_cooccur1']-1.96*cohort5['model14','emm.SE_cooccur1'], cohort5['model14','emm_cooccur1']+1.96*cohort5['model14','emm.SE_cooccur1'], 'cohort5', 'cm_only', cohort5['model14', 'n_cooccur1'], cohort5['model14', c('cohensd1', 'SE1')]),
                                   cohort5_cooccur2 = c(cohort5['model14','emm_cooccur2'], cohort5['model14','emm.SE_cooccur2'], cohort5['model14','emm_cooccur2']-1.96*cohort5['model14','emm.SE_cooccur2'], cohort5['model14','emm_cooccur2']+1.96*cohort5['model14','emm.SE_cooccur2'], 'cohort5', 'mdd_only', cohort5['model14', 'n_cooccur2'], cohort5['model14', c('cohensd2', 'SE2')]),
                                   cohort5_cooccur3 = c(cohort5['model14','emm_cooccur3'], cohort5['model14','emm.SE_cooccur3'], cohort5['model14','emm_cooccur3']-1.96*cohort5['model14','emm.SE_cooccur3'], cohort5['model14','emm_cooccur3']+1.96*cohort5['model14','emm.SE_cooccur3'], 'cohort5', 'cm_and_mdd', cohort5['model14', 'n_cooccur3'], cohort5['model14', c('cohensd3', 'SE3')]),
                                   
                                   cohort6_cooccur0 = c(cohort6['model14','emm_cooccur0'], cohort6['model14','emm.SE_cooccur0'], cohort6['model14','emm_cooccur0']-1.96*cohort6['model14','emm.SE_cooccur0'], cohort6['model14','emm_cooccur0']+1.96*cohort6['model14','emm.SE_cooccur0'], 'cohort6', 'no_cm_no_mdd', cohort6['model14', 'n_cooccur0'], NA, NA),
                                   cohort6_cooccur1 = c(cohort6['model14','emm_cooccur1'], cohort6['model14','emm.SE_cooccur1'], cohort6['model14','emm_cooccur1']-1.96*cohort6['model14','emm.SE_cooccur1'], cohort6['model14','emm_cooccur1']+1.96*cohort6['model14','emm.SE_cooccur1'], 'cohort6', 'cm_only', cohort6['model14', 'n_cooccur1'], cohort6['model14', c('cohensd1', 'SE1')]),
                                   cohort6_cooccur2 = c(cohort6['model14','emm_cooccur2'], cohort6['model14','emm.SE_cooccur2'], cohort6['model14','emm_cooccur2']-1.96*cohort6['model14','emm.SE_cooccur2'], cohort6['model14','emm_cooccur2']+1.96*cohort6['model14','emm.SE_cooccur2'], 'cohort6', 'mdd_only', cohort6['model14', 'n_cooccur2'], cohort6['model14', c('cohensd2', 'SE2')]),
                                   cohort6_cooccur3 = c(cohort6['model14','emm_cooccur3'], cohort6['model14','emm.SE_cooccur3'], cohort6['model14','emm_cooccur3']-1.96*cohort6['model14','emm.SE_cooccur3'], cohort6['model14','emm_cooccur3']+1.96*cohort6['model14','emm.SE_cooccur3'], 'cohort6', 'cm_and_mdd', cohort6['model14', 'n_cooccur3'], cohort6['model14', c('cohensd3', 'SE3')]),
                                   
                                   cohort7_cooccur0 = c(cohort7['model14','emm_cooccur0'], cohort7['model14','emm.SE_cooccur0'], cohort7['model14','emm_cooccur0']-1.96*cohort7['model14','emm.SE_cooccur0'], cohort7['model14','emm_cooccur0']+1.96*cohort7['model14','emm.SE_cooccur0'], 'cohort7', 'no_cm_no_mdd', cohort7['model14', 'n_cooccur0'], NA, NA),
                                   cohort7_cooccur1 = c(cohort7['model14','emm_cooccur1'], cohort7['model14','emm.SE_cooccur1'], cohort7['model14','emm_cooccur1']-1.96*cohort7['model14','emm.SE_cooccur1'], cohort7['model14','emm_cooccur1']+1.96*cohort7['model14','emm.SE_cooccur1'], 'cohort7', 'cm_only', cohort7['model14', 'n_cooccur1'], cohort7['model14', c('cohensd1', 'SE1')]),
                                   cohort7_cooccur2 = c(cohort7['model14','emm_cooccur2'], cohort7['model14','emm.SE_cooccur2'], cohort7['model14','emm_cooccur2']-1.96*cohort7['model14','emm.SE_cooccur2'], cohort7['model14','emm_cooccur2']+1.96*cohort7['model14','emm.SE_cooccur2'], 'cohort7', 'mdd_only', cohort7['model14', 'n_cooccur2'], cohort7['model14', c('cohensd2', 'SE2')]),
                                   cohort7_cooccur3 = c(cohort7['model14','emm_cooccur3'], cohort7['model14','emm.SE_cooccur3'], cohort7['model14','emm_cooccur3']-1.96*cohort7['model14','emm.SE_cooccur3'], cohort7['model14','emm_cooccur3']+1.96*cohort7['model14','emm.SE_cooccur3'], 'cohort7', 'cm_and_mdd', cohort7['model14', 'n_cooccur3'], cohort7['model14', c('cohensd3', 'SE3')]),
                                   
                                   cohort8_cooccur0 = c(cohort8['model14','emm_cooccur0'], cohort8['model14','emm.SE_cooccur0'], cohort8['model14','emm_cooccur0']-1.96*cohort8['model14','emm.SE_cooccur0'], cohort8['model14','emm_cooccur0']+1.96*cohort8['model14','emm.SE_cooccur0'], 'cohort8', 'no_cm_no_mdd', cohort8['model14', 'n_cooccur0'], NA, NA),
                                   cohort8_cooccur1 = c(cohort8['model14','emm_cooccur1'], cohort8['model14','emm.SE_cooccur1'], cohort8['model14','emm_cooccur1']-1.96*cohort8['model14','emm.SE_cooccur1'], cohort8['model14','emm_cooccur1']+1.96*cohort8['model14','emm.SE_cooccur1'], 'cohort8', 'cm_only', cohort8['model14', 'n_cooccur1'], cohort8['model14', c('cohensd1', 'SE1')]),
                                   cohort8_cooccur2 = c(cohort8['model14','emm_cooccur2'], cohort8['model14','emm.SE_cooccur2'], cohort8['model14','emm_cooccur2']-1.96*cohort8['model14','emm.SE_cooccur2'], cohort8['model14','emm_cooccur2']+1.96*cohort8['model14','emm.SE_cooccur2'], 'cohort8', 'mdd_only', cohort8['model14', 'n_cooccur2'], cohort8['model14', c('cohensd2', 'SE2')]),
                                   cohort8_cooccur3 = c(cohort8['model14','emm_cooccur3'], cohort8['model14','emm.SE_cooccur3'], cohort8['model14','emm_cooccur3']-1.96*cohort8['model14','emm.SE_cooccur3'], cohort8['model14','emm_cooccur3']+1.96*cohort8['model14','emm.SE_cooccur3'], 'cohort8', 'cm_and_mdd', cohort8['model14', 'n_cooccur3'], cohort8['model14', c('cohensd3', 'SE3')]),
                                   
                                   cohort9_cooccur0 = c(cohort9['model14','emm_cooccur0'], cohort9['model14','emm.SE_cooccur0'], cohort9['model14','emm_cooccur0']-1.96*cohort9['model14','emm.SE_cooccur0'], cohort9['model14','emm_cooccur0']+1.96*cohort9['model14','emm.SE_cooccur0'], 'cohort9', 'no_cm_no_mdd', cohort9['model14', 'n_cooccur0'], NA, NA),
                                   cohort9_cooccur1 = c(cohort9['model14','emm_cooccur1'], cohort9['model14','emm.SE_cooccur1'], cohort9['model14','emm_cooccur1']-1.96*cohort9['model14','emm.SE_cooccur1'], cohort9['model14','emm_cooccur1']+1.96*cohort9['model14','emm.SE_cooccur1'], 'cohort9', 'cm_only', cohort9['model14', 'n_cooccur1'], cohort9['model14', c('cohensd1', 'SE1')]),
                                   cohort9_cooccur2 = c(cohort9['model14','emm_cooccur2'], cohort9['model14','emm.SE_cooccur2'], cohort9['model14','emm_cooccur2']-1.96*cohort9['model14','emm.SE_cooccur2'], cohort9['model14','emm_cooccur2']+1.96*cohort9['model14','emm.SE_cooccur2'], 'cohort9', 'mdd_only', cohort9['model14', 'n_cooccur2'], cohort9['model14', c('cohensd2', 'SE2')]),
                                   cohort9_cooccur3 = c(cohort9['model14','emm_cooccur3'], cohort9['model14','emm.SE_cooccur3'], cohort9['model14','emm_cooccur3']-1.96*cohort9['model14','emm.SE_cooccur3'], cohort9['model14','emm_cooccur3']+1.96*cohort9['model14','emm.SE_cooccur3'], 'cohort9', 'cm_and_mdd', cohort9['model14', 'n_cooccur3'], cohort9['model14', c('cohensd3', 'SE3')]),
                                   
                                   cohort10_cooccur0 = c(cohort10['model14','emm_cooccur0'], cohort10['model14','emm.SE_cooccur0'], cohort10['model14','emm_cooccur0']-1.96*cohort10['model14','emm.SE_cooccur0'], cohort10['model14','emm_cooccur0']+1.96*cohort10['model14','emm.SE_cooccur0'], 'cohort10', 'no_cm_no_mdd', cohort10['model14', 'n_cooccur0'], NA, NA),
                                   cohort10_cooccur1 = c(cohort10['model14','emm_cooccur1'], cohort10['model14','emm.SE_cooccur1'], cohort10['model14','emm_cooccur1']-1.96*cohort10['model14','emm.SE_cooccur1'], cohort10['model14','emm_cooccur1']+1.96*cohort10['model14','emm.SE_cooccur1'], 'cohort10', 'cm_only', cohort10['model14', 'n_cooccur1'], cohort10['model14', c('cohensd1', 'SE1')]),
                                   cohort10_cooccur2 = c(cohort10['model14','emm_cooccur2'], cohort10['model14','emm.SE_cooccur2'], cohort10['model14','emm_cooccur2']-1.96*cohort10['model14','emm.SE_cooccur2'], cohort10['model14','emm_cooccur2']+1.96*cohort10['model14','emm.SE_cooccur2'], 'cohort10', 'mdd_only', cohort10['model14', 'n_cooccur2'], cohort10['model14', c('cohensd2', 'SE2')]),
                                   cohort10_cooccur3 = c(cohort10['model14','emm_cooccur3'], cohort10['model14','emm.SE_cooccur3'], cohort10['model14','emm_cooccur3']-1.96*cohort10['model14','emm.SE_cooccur3'], cohort10['model14','emm_cooccur3']+1.96*cohort10['model14','emm.SE_cooccur3'], 'cohort10', 'cm_and_mdd', cohort10['model14', 'n_cooccur3'], cohort10['model14', c('cohensd3', 'SE3')]),
                                   
                                   cohort11_cooccur0 = c(cohort11['model14','emm_cooccur0'], cohort11['model14','emm.SE_cooccur0'], cohort11['model14','emm_cooccur0']-1.96*cohort11['model14','emm.SE_cooccur0'], cohort11['model14','emm_cooccur0']+1.96*cohort11['model14','emm.SE_cooccur0'], 'cohort11', 'no_cm_no_mdd', cohort11['model14', 'n_cooccur0'], NA, NA),
                                   cohort11_cooccur1 = c(cohort11['model14','emm_cooccur1'], cohort11['model14','emm.SE_cooccur1'], cohort11['model14','emm_cooccur1']-1.96*cohort11['model14','emm.SE_cooccur1'], cohort11['model14','emm_cooccur1']+1.96*cohort11['model14','emm.SE_cooccur1'], 'cohort11', 'cm_only', cohort11['model14', 'n_cooccur0'], cohort11['model14', c('cohensd1', 'SE1')]),
                                   cohort11_cooccur2 = c(cohort11['model14','emm_cooccur2'], cohort11['model14','emm.SE_cooccur2'], cohort11['model14','emm_cooccur2']-1.96*cohort11['model14','emm.SE_cooccur2'], cohort11['model14','emm_cooccur2']+1.96*cohort11['model14','emm.SE_cooccur2'], 'cohort11', 'mdd_only', cohort11['model14', 'n_cooccur0'], cohort11['model14', c('cohensd2', 'SE2')]),
                                   cohort11_cooccur3 = c(cohort11['model14','emm_cooccur3'], cohort11['model14','emm.SE_cooccur3'], cohort11['model14','emm_cooccur3']-1.96*cohort11['model14','emm.SE_cooccur3'], cohort11['model14','emm_cooccur3']+1.96*cohort11['model14','emm.SE_cooccur3'], 'cohort11', 'cm_and_mdd', cohort11['model14', 'n_cooccur0'], cohort11['model14', c('cohensd3', 'SE3')])))

colnames(Model14_emm) <- c('emm', 'SE','CI_lb','CI_ub','cohort','group', 'n', 'cohensd', 'SE_cohensd')

Model14_emm$group <- factor(Model14_emm$group, levels = c("no_cm_no_mdd","cm_only","mdd_only","cm_and_mdd"))


Model15_emm <- as.data.frame(rbind(cohort1_cooccur0 = c(cohort1['model15','emm_cooccur0'], cohort1['model15','emm.SE_cooccur0'], cohort1['model15','emm_cooccur0']-1.96*cohort1['model15','emm.SE_cooccur0'], cohort1['model15','emm_cooccur0']+1.96*cohort1['model15','emm.SE_cooccur0'], 'cohort1', 'no_cm_no_mdd', cohort1['model15', 'n_cooccur0'], NA, NA),
                                   cohort1_cooccur1 = c(cohort1['model15','emm_cooccur1'], cohort1['model15','emm.SE_cooccur1'], cohort1['model15','emm_cooccur1']-1.96*cohort1['model15','emm.SE_cooccur1'], cohort1['model15','emm_cooccur1']+1.96*cohort1['model15','emm.SE_cooccur1'], 'cohort1', 'cm_only', cohort1['model15', 'n_cooccur1'], cohort1['model15', c('cohensd1', 'SE1')]),
                                   cohort1_cooccur2 = c(cohort1['model15','emm_cooccur2'], cohort1['model15','emm.SE_cooccur2'], cohort1['model15','emm_cooccur2']-1.96*cohort1['model15','emm.SE_cooccur2'], cohort1['model15','emm_cooccur2']+1.96*cohort1['model15','emm.SE_cooccur2'], 'cohort1', 'mdd_only', cohort1['model15', 'n_cooccur2'], cohort1['model15', c('cohensd2', 'SE2')]),
                                   cohort1_cooccur3 = c(cohort1['model15','emm_cooccur3'], cohort1['model15','emm.SE_cooccur3'], cohort1['model15','emm_cooccur3']-1.96*cohort1['model15','emm.SE_cooccur3'], cohort1['model15','emm_cooccur3']+1.96*cohort1['model15','emm.SE_cooccur3'], 'cohort1', 'cm_and_mdd', cohort1['model15', 'n_cooccur3'], cohort1['model15', c('cohensd3', 'SE3')]),
                                   
                                   cohort2_cooccur0 = c(cohort2['model15','emm_cooccur0'], cohort2['model15','emm.SE_cooccur0'], cohort2['model15','emm_cooccur0']-1.96*cohort2['model15','emm.SE_cooccur0'], cohort2['model15','emm_cooccur0']+1.96*cohort2['model15','emm.SE_cooccur0'], 'cohort2', 'no_cm_no_mdd', cohort2['model15', 'n_cooccur0'], NA, NA),
                                   cohort2_cooccur1 = c(cohort2['model15','emm_cooccur1'], cohort2['model15','emm.SE_cooccur1'], cohort2['model15','emm_cooccur1']-1.96*cohort2['model15','emm.SE_cooccur1'], cohort2['model15','emm_cooccur1']+1.96*cohort2['model15','emm.SE_cooccur1'], 'cohort2', 'cm_only', cohort2['model15', 'n_cooccur1'], cohort2['model15', c('cohensd1', 'SE1')]),
                                   cohort2_cooccur2 = c(cohort2['model15','emm_cooccur2'], cohort2['model15','emm.SE_cooccur2'], cohort2['model15','emm_cooccur2']-1.96*cohort2['model15','emm.SE_cooccur2'], cohort2['model15','emm_cooccur2']+1.96*cohort2['model15','emm.SE_cooccur2'], 'cohort2', 'mdd_only', cohort2['model15', 'n_cooccur2'], cohort2['model15', c('cohensd2', 'SE2')]),
                                   cohort2_cooccur3 = c(cohort2['model15','emm_cooccur3'], cohort2['model15','emm.SE_cooccur3'], cohort2['model15','emm_cooccur3']-1.96*cohort2['model15','emm.SE_cooccur3'], cohort2['model15','emm_cooccur3']+1.96*cohort2['model15','emm.SE_cooccur3'], 'cohort2', 'cm_and_mdd', cohort2['model15', 'n_cooccur3'], cohort2['model15', c('cohensd3', 'SE3')]),
                                   
                                   cohort3_cooccur0 = c(cohort3['model15','emm_cooccur0'], cohort3['model15','emm.SE_cooccur0'], cohort3['model15','emm_cooccur0']-1.96*cohort3['model15','emm.SE_cooccur0'], cohort3['model15','emm_cooccur0']+1.96*cohort3['model15','emm.SE_cooccur0'], 'cohort3', 'no_cm_no_mdd', cohort3['model15', 'n_cooccur0'], NA, NA),
                                   cohort3_cooccur1 = c(cohort3['model15','emm_cooccur1'], cohort3['model15','emm.SE_cooccur1'], cohort3['model15','emm_cooccur1']-1.96*cohort3['model15','emm.SE_cooccur1'], cohort3['model15','emm_cooccur1']+1.96*cohort3['model15','emm.SE_cooccur1'], 'cohort3', 'cm_only', cohort3['model15', 'n_cooccur1'], cohort3['model15', c('cohensd1', 'SE1')]),
                                   cohort3_cooccur2 = c(cohort3['model15','emm_cooccur2'], cohort3['model15','emm.SE_cooccur2'], cohort3['model15','emm_cooccur2']-1.96*cohort3['model15','emm.SE_cooccur2'], cohort3['model15','emm_cooccur2']+1.96*cohort3['model15','emm.SE_cooccur2'], 'cohort3', 'mdd_only', cohort3['model15', 'n_cooccur2'], cohort3['model15', c('cohensd2', 'SE2')]),
                                   cohort3_cooccur3 = c(cohort3['model15','emm_cooccur3'], cohort3['model15','emm.SE_cooccur3'], cohort3['model15','emm_cooccur3']-1.96*cohort3['model15','emm.SE_cooccur3'], cohort3['model15','emm_cooccur3']+1.96*cohort3['model15','emm.SE_cooccur3'], 'cohort3', 'cm_and_mdd', cohort3['model15', 'n_cooccur3'], cohort3['model15', c('cohensd3', 'SE3')]),
                                   
                                   cohort4_cooccur0 = c(cohort4['model15','emm_cooccur0'], cohort4['model15','emm.SE_cooccur0'], cohort4['model15','emm_cooccur0']-1.96*cohort4['model15','emm.SE_cooccur0'], cohort4['model15','emm_cooccur0']+1.96*cohort4['model15','emm.SE_cooccur0'], 'cohort4', 'no_cm_no_mdd', cohort4['model15', 'n_cooccur0'], NA, NA),
                                   cohort4_cooccur1 = c(cohort4['model15','emm_cooccur1'], cohort4['model15','emm.SE_cooccur1'], cohort4['model15','emm_cooccur1']-1.96*cohort4['model15','emm.SE_cooccur1'], cohort4['model15','emm_cooccur1']+1.96*cohort4['model15','emm.SE_cooccur1'], 'cohort4', 'cm_only', cohort4['model15', 'n_cooccur1'], cohort4['model15', c('cohensd1', 'SE1')]),
                                   cohort4_cooccur2 = c(cohort4['model15','emm_cooccur2'], cohort4['model15','emm.SE_cooccur2'], cohort4['model15','emm_cooccur2']-1.96*cohort4['model15','emm.SE_cooccur2'], cohort4['model15','emm_cooccur2']+1.96*cohort4['model15','emm.SE_cooccur2'], 'cohort4', 'mdd_only', cohort4['model15', 'n_cooccur2'], cohort4['model15', c('cohensd2', 'SE2')]),
                                   cohort4_cooccur3 = c(cohort4['model15','emm_cooccur3'], cohort4['model15','emm.SE_cooccur3'], cohort4['model15','emm_cooccur3']-1.96*cohort4['model15','emm.SE_cooccur3'], cohort4['model15','emm_cooccur3']+1.96*cohort4['model15','emm.SE_cooccur3'], 'cohort4', 'cm_and_mdd', cohort4['model15', 'n_cooccur3'], cohort4['model15', c('cohensd3', 'SE3')]),
                                   
                                   cohort5_cooccur0 = c(cohort5['model15','emm_cooccur0'], cohort5['model15','emm.SE_cooccur0'], cohort5['model15','emm_cooccur0']-1.96*cohort5['model15','emm.SE_cooccur0'], cohort5['model15','emm_cooccur0']+1.96*cohort5['model15','emm.SE_cooccur0'], 'cohort5', 'no_cm_no_mdd', cohort5['model15', 'n_cooccur0'], NA, NA),
                                   cohort5_cooccur1 = c(cohort5['model15','emm_cooccur1'], cohort5['model15','emm.SE_cooccur1'], cohort5['model15','emm_cooccur1']-1.96*cohort5['model15','emm.SE_cooccur1'], cohort5['model15','emm_cooccur1']+1.96*cohort5['model15','emm.SE_cooccur1'], 'cohort5', 'cm_only', cohort5['model15', 'n_cooccur1'], cohort5['model15', c('cohensd1', 'SE1')]),
                                   cohort5_cooccur2 = c(cohort5['model15','emm_cooccur2'], cohort5['model15','emm.SE_cooccur2'], cohort5['model15','emm_cooccur2']-1.96*cohort5['model15','emm.SE_cooccur2'], cohort5['model15','emm_cooccur2']+1.96*cohort5['model15','emm.SE_cooccur2'], 'cohort5', 'mdd_only', cohort5['model15', 'n_cooccur2'], cohort5['model15', c('cohensd2', 'SE2')]),
                                   cohort5_cooccur3 = c(cohort5['model15','emm_cooccur3'], cohort5['model15','emm.SE_cooccur3'], cohort5['model15','emm_cooccur3']-1.96*cohort5['model15','emm.SE_cooccur3'], cohort5['model15','emm_cooccur3']+1.96*cohort5['model15','emm.SE_cooccur3'], 'cohort5', 'cm_and_mdd', cohort5['model15', 'n_cooccur3'], cohort5['model15', c('cohensd3', 'SE3')]),
                                   
                                   cohort6_cooccur0 = c(cohort6['model15','emm_cooccur0'], cohort6['model15','emm.SE_cooccur0'], cohort6['model15','emm_cooccur0']-1.96*cohort6['model15','emm.SE_cooccur0'], cohort6['model15','emm_cooccur0']+1.96*cohort6['model15','emm.SE_cooccur0'], 'cohort6', 'no_cm_no_mdd', cohort6['model15', 'n_cooccur0'], NA, NA),
                                   cohort6_cooccur1 = c(cohort6['model15','emm_cooccur1'], cohort6['model15','emm.SE_cooccur1'], cohort6['model15','emm_cooccur1']-1.96*cohort6['model15','emm.SE_cooccur1'], cohort6['model15','emm_cooccur1']+1.96*cohort6['model15','emm.SE_cooccur1'], 'cohort6', 'cm_only', cohort6['model15', 'n_cooccur1'], cohort6['model15', c('cohensd1', 'SE1')]),
                                   cohort6_cooccur2 = c(cohort6['model15','emm_cooccur2'], cohort6['model15','emm.SE_cooccur2'], cohort6['model15','emm_cooccur2']-1.96*cohort6['model15','emm.SE_cooccur2'], cohort6['model15','emm_cooccur2']+1.96*cohort6['model15','emm.SE_cooccur2'], 'cohort6', 'mdd_only', cohort6['model15', 'n_cooccur2'], cohort6['model15', c('cohensd2', 'SE2')]),
                                   cohort6_cooccur3 = c(cohort6['model15','emm_cooccur3'], cohort6['model15','emm.SE_cooccur3'], cohort6['model15','emm_cooccur3']-1.96*cohort6['model15','emm.SE_cooccur3'], cohort6['model15','emm_cooccur3']+1.96*cohort6['model15','emm.SE_cooccur3'], 'cohort6', 'cm_and_mdd', cohort6['model15', 'n_cooccur3'], cohort6['model15', c('cohensd3', 'SE3')]),
                                   
                                   cohort7_cooccur0 = c(cohort7['model15','emm_cooccur0'], cohort7['model15','emm.SE_cooccur0'], cohort7['model15','emm_cooccur0']-1.96*cohort7['model15','emm.SE_cooccur0'], cohort7['model15','emm_cooccur0']+1.96*cohort7['model15','emm.SE_cooccur0'], 'cohort7', 'no_cm_no_mdd', cohort7['model15', 'n_cooccur0'], NA, NA),
                                   cohort7_cooccur1 = c(cohort7['model15','emm_cooccur1'], cohort7['model15','emm.SE_cooccur1'], cohort7['model15','emm_cooccur1']-1.96*cohort7['model15','emm.SE_cooccur1'], cohort7['model15','emm_cooccur1']+1.96*cohort7['model15','emm.SE_cooccur1'], 'cohort7', 'cm_only', cohort7['model15', 'n_cooccur1'], cohort7['model15', c('cohensd1', 'SE1')]),
                                   cohort7_cooccur2 = c(cohort7['model15','emm_cooccur2'], cohort7['model15','emm.SE_cooccur2'], cohort7['model15','emm_cooccur2']-1.96*cohort7['model15','emm.SE_cooccur2'], cohort7['model15','emm_cooccur2']+1.96*cohort7['model15','emm.SE_cooccur2'], 'cohort7', 'mdd_only', cohort7['model15', 'n_cooccur2'], cohort7['model15', c('cohensd2', 'SE2')]),
                                   cohort7_cooccur3 = c(cohort7['model15','emm_cooccur3'], cohort7['model15','emm.SE_cooccur3'], cohort7['model15','emm_cooccur3']-1.96*cohort7['model15','emm.SE_cooccur3'], cohort7['model15','emm_cooccur3']+1.96*cohort7['model15','emm.SE_cooccur3'], 'cohort7', 'cm_and_mdd', cohort7['model15', 'n_cooccur3'], cohort7['model15', c('cohensd3', 'SE3')]),
                                   
                                   cohort8_cooccur0 = c(cohort8['model15','emm_cooccur0'], cohort8['model15','emm.SE_cooccur0'], cohort8['model15','emm_cooccur0']-1.96*cohort8['model15','emm.SE_cooccur0'], cohort8['model15','emm_cooccur0']+1.96*cohort8['model15','emm.SE_cooccur0'], 'cohort8', 'no_cm_no_mdd', cohort8['model15', 'n_cooccur0'], NA, NA),
                                   cohort8_cooccur1 = c(cohort8['model15','emm_cooccur1'], cohort8['model15','emm.SE_cooccur1'], cohort8['model15','emm_cooccur1']-1.96*cohort8['model15','emm.SE_cooccur1'], cohort8['model15','emm_cooccur1']+1.96*cohort8['model15','emm.SE_cooccur1'], 'cohort8', 'cm_only', cohort8['model15', 'n_cooccur1'], cohort8['model15', c('cohensd1', 'SE1')]),
                                   cohort8_cooccur2 = c(cohort8['model15','emm_cooccur2'], cohort8['model15','emm.SE_cooccur2'], cohort8['model15','emm_cooccur2']-1.96*cohort8['model15','emm.SE_cooccur2'], cohort8['model15','emm_cooccur2']+1.96*cohort8['model15','emm.SE_cooccur2'], 'cohort8', 'mdd_only', cohort8['model15', 'n_cooccur2'], cohort8['model15', c('cohensd2', 'SE2')]),
                                   cohort8_cooccur3 = c(cohort8['model15','emm_cooccur3'], cohort8['model15','emm.SE_cooccur3'], cohort8['model15','emm_cooccur3']-1.96*cohort8['model15','emm.SE_cooccur3'], cohort8['model15','emm_cooccur3']+1.96*cohort8['model15','emm.SE_cooccur3'], 'cohort8', 'cm_and_mdd', cohort8['model15', 'n_cooccur3'], cohort8['model15', c('cohensd3', 'SE3')]),
                                   
                                   cohort9_cooccur0 = c(cohort9['model15','emm_cooccur0'], cohort9['model15','emm.SE_cooccur0'], cohort9['model15','emm_cooccur0']-1.96*cohort9['model15','emm.SE_cooccur0'], cohort9['model15','emm_cooccur0']+1.96*cohort9['model15','emm.SE_cooccur0'], 'cohort9', 'no_cm_no_mdd', cohort9['model15', 'n_cooccur0'], NA, NA),
                                   cohort9_cooccur1 = c(cohort9['model15','emm_cooccur1'], cohort9['model15','emm.SE_cooccur1'], cohort9['model15','emm_cooccur1']-1.96*cohort9['model15','emm.SE_cooccur1'], cohort9['model15','emm_cooccur1']+1.96*cohort9['model15','emm.SE_cooccur1'], 'cohort9', 'cm_only', cohort9['model15', 'n_cooccur1'], cohort9['model15', c('cohensd1', 'SE1')]),
                                   cohort9_cooccur2 = c(cohort9['model15','emm_cooccur2'], cohort9['model15','emm.SE_cooccur2'], cohort9['model15','emm_cooccur2']-1.96*cohort9['model15','emm.SE_cooccur2'], cohort9['model15','emm_cooccur2']+1.96*cohort9['model15','emm.SE_cooccur2'], 'cohort9', 'mdd_only', cohort9['model15', 'n_cooccur2'], cohort9['model15', c('cohensd2', 'SE2')]),
                                   cohort9_cooccur3 = c(cohort9['model15','emm_cooccur3'], cohort9['model15','emm.SE_cooccur3'], cohort9['model15','emm_cooccur3']-1.96*cohort9['model15','emm.SE_cooccur3'], cohort9['model15','emm_cooccur3']+1.96*cohort9['model15','emm.SE_cooccur3'], 'cohort9', 'cm_and_mdd', cohort9['model15', 'n_cooccur3'], cohort9['model15', c('cohensd3', 'SE3')]),
                                   
                                   cohort10_cooccur0 = c(cohort10['model15','emm_cooccur0'], cohort10['model15','emm.SE_cooccur0'], cohort10['model15','emm_cooccur0']-1.96*cohort10['model15','emm.SE_cooccur0'], cohort10['model15','emm_cooccur0']+1.96*cohort10['model15','emm.SE_cooccur0'], 'cohort10', 'no_cm_no_mdd', cohort10['model15', 'n_cooccur0'], NA, NA),
                                   cohort10_cooccur1 = c(cohort10['model15','emm_cooccur1'], cohort10['model15','emm.SE_cooccur1'], cohort10['model15','emm_cooccur1']-1.96*cohort10['model15','emm.SE_cooccur1'], cohort10['model15','emm_cooccur1']+1.96*cohort10['model15','emm.SE_cooccur1'], 'cohort10', 'cm_only', cohort10['model15', 'n_cooccur1'], cohort10['model15', c('cohensd1', 'SE1')]),
                                   cohort10_cooccur2 = c(cohort10['model15','emm_cooccur2'], cohort10['model15','emm.SE_cooccur2'], cohort10['model15','emm_cooccur2']-1.96*cohort10['model15','emm.SE_cooccur2'], cohort10['model15','emm_cooccur2']+1.96*cohort10['model15','emm.SE_cooccur2'], 'cohort10', 'mdd_only', cohort10['model15', 'n_cooccur2'], cohort10['model15', c('cohensd2', 'SE2')]),
                                   cohort10_cooccur3 = c(cohort10['model15','emm_cooccur3'], cohort10['model15','emm.SE_cooccur3'], cohort10['model15','emm_cooccur3']-1.96*cohort10['model15','emm.SE_cooccur3'], cohort10['model15','emm_cooccur3']+1.96*cohort10['model15','emm.SE_cooccur3'], 'cohort10', 'cm_and_mdd', cohort10['model15', 'n_cooccur3'], cohort10['model15', c('cohensd3', 'SE3')]),
                                   
                                   cohort11_cooccur0 = c(cohort11['model15','emm_cooccur0'], cohort11['model15','emm.SE_cooccur0'], cohort11['model15','emm_cooccur0']-1.96*cohort11['model15','emm.SE_cooccur0'], cohort11['model15','emm_cooccur0']+1.96*cohort11['model15','emm.SE_cooccur0'], 'cohort11', 'no_cm_no_mdd', cohort11['model15', 'n_cooccur0'], NA, NA),
                                   cohort11_cooccur1 = c(cohort11['model15','emm_cooccur1'], cohort11['model15','emm.SE_cooccur1'], cohort11['model15','emm_cooccur1']-1.96*cohort11['model15','emm.SE_cooccur1'], cohort11['model15','emm_cooccur1']+1.96*cohort11['model15','emm.SE_cooccur1'], 'cohort11', 'cm_only', cohort11['model15', 'n_cooccur0'], cohort11['model15', c('cohensd1', 'SE1')]),
                                   cohort11_cooccur2 = c(cohort11['model15','emm_cooccur2'], cohort11['model15','emm.SE_cooccur2'], cohort11['model15','emm_cooccur2']-1.96*cohort11['model15','emm.SE_cooccur2'], cohort11['model15','emm_cooccur2']+1.96*cohort11['model15','emm.SE_cooccur2'], 'cohort11', 'mdd_only', cohort11['model15', 'n_cooccur0'], cohort11['model15', c('cohensd2', 'SE2')]),
                                   cohort11_cooccur3 = c(cohort11['model15','emm_cooccur3'], cohort11['model15','emm.SE_cooccur3'], cohort11['model15','emm_cooccur3']-1.96*cohort11['model15','emm.SE_cooccur3'], cohort11['model15','emm_cooccur3']+1.96*cohort11['model15','emm.SE_cooccur3'], 'cohort11', 'cm_and_mdd', cohort11['model15', 'n_cooccur0'], cohort11['model15', c('cohensd3', 'SE3')])))

colnames(Model15_emm) <- c('emm', 'SE','CI_lb','CI_ub','cohort','group', 'n', 'cohensd', 'SE_cohensd')

Model15_emm$group <- factor(Model15_emm$group, levels = c("no_cm_no_mdd","cm_only","mdd_only","cm_and_mdd"))


Model16_emm <- as.data.frame(rbind(cohort1_cooccur0 = c(cohort1['model16','emm_cooccur0'], cohort1['model16','emm.SE_cooccur0'], cohort1['model16','emm_cooccur0']-1.96*cohort1['model16','emm.SE_cooccur0'], cohort1['model16','emm_cooccur0']+1.96*cohort1['model16','emm.SE_cooccur0'], 'cohort1', 'no_cm_no_mdd', cohort1['model16', 'n_cooccur0'], NA, NA),
                                   cohort1_cooccur1 = c(cohort1['model16','emm_cooccur1'], cohort1['model16','emm.SE_cooccur1'], cohort1['model16','emm_cooccur1']-1.96*cohort1['model16','emm.SE_cooccur1'], cohort1['model16','emm_cooccur1']+1.96*cohort1['model16','emm.SE_cooccur1'], 'cohort1', 'cm_only', cohort1['model16', 'n_cooccur1'], cohort1['model16', c('cohensd1', 'SE1')]),
                                   cohort1_cooccur2 = c(cohort1['model16','emm_cooccur2'], cohort1['model16','emm.SE_cooccur2'], cohort1['model16','emm_cooccur2']-1.96*cohort1['model16','emm.SE_cooccur2'], cohort1['model16','emm_cooccur2']+1.96*cohort1['model16','emm.SE_cooccur2'], 'cohort1', 'mdd_only', cohort1['model16', 'n_cooccur2'], cohort1['model16', c('cohensd2', 'SE2')]),
                                   cohort1_cooccur3 = c(cohort1['model16','emm_cooccur3'], cohort1['model16','emm.SE_cooccur3'], cohort1['model16','emm_cooccur3']-1.96*cohort1['model16','emm.SE_cooccur3'], cohort1['model16','emm_cooccur3']+1.96*cohort1['model16','emm.SE_cooccur3'], 'cohort1', 'cm_and_mdd', cohort1['model16', 'n_cooccur3'], cohort1['model16', c('cohensd3', 'SE3')]),
                                   
                                   cohort2_cooccur0 = c(cohort2['model16','emm_cooccur0'], cohort2['model16','emm.SE_cooccur0'], cohort2['model16','emm_cooccur0']-1.96*cohort2['model16','emm.SE_cooccur0'], cohort2['model16','emm_cooccur0']+1.96*cohort2['model16','emm.SE_cooccur0'], 'cohort2', 'no_cm_no_mdd', cohort2['model16', 'n_cooccur0'], NA, NA),
                                   cohort2_cooccur1 = c(cohort2['model16','emm_cooccur1'], cohort2['model16','emm.SE_cooccur1'], cohort2['model16','emm_cooccur1']-1.96*cohort2['model16','emm.SE_cooccur1'], cohort2['model16','emm_cooccur1']+1.96*cohort2['model16','emm.SE_cooccur1'], 'cohort2', 'cm_only', cohort2['model16', 'n_cooccur1'], cohort2['model16', c('cohensd1', 'SE1')]),
                                   cohort2_cooccur2 = c(cohort2['model16','emm_cooccur2'], cohort2['model16','emm.SE_cooccur2'], cohort2['model16','emm_cooccur2']-1.96*cohort2['model16','emm.SE_cooccur2'], cohort2['model16','emm_cooccur2']+1.96*cohort2['model16','emm.SE_cooccur2'], 'cohort2', 'mdd_only', cohort2['model16', 'n_cooccur2'], cohort2['model16', c('cohensd2', 'SE2')]),
                                   cohort2_cooccur3 = c(cohort2['model16','emm_cooccur3'], cohort2['model16','emm.SE_cooccur3'], cohort2['model16','emm_cooccur3']-1.96*cohort2['model16','emm.SE_cooccur3'], cohort2['model16','emm_cooccur3']+1.96*cohort2['model16','emm.SE_cooccur3'], 'cohort2', 'cm_and_mdd', cohort2['model16', 'n_cooccur3'], cohort2['model16', c('cohensd3', 'SE3')]),
                                   
                                   cohort3_cooccur0 = c(cohort3['model16','emm_cooccur0'], cohort3['model16','emm.SE_cooccur0'], cohort3['model16','emm_cooccur0']-1.96*cohort3['model16','emm.SE_cooccur0'], cohort3['model16','emm_cooccur0']+1.96*cohort3['model16','emm.SE_cooccur0'], 'cohort3', 'no_cm_no_mdd', cohort3['model16', 'n_cooccur0'], NA, NA),
                                   cohort3_cooccur1 = c(cohort3['model16','emm_cooccur1'], cohort3['model16','emm.SE_cooccur1'], cohort3['model16','emm_cooccur1']-1.96*cohort3['model16','emm.SE_cooccur1'], cohort3['model16','emm_cooccur1']+1.96*cohort3['model16','emm.SE_cooccur1'], 'cohort3', 'cm_only', cohort3['model16', 'n_cooccur1'], cohort3['model16', c('cohensd1', 'SE1')]),
                                   cohort3_cooccur2 = c(cohort3['model16','emm_cooccur2'], cohort3['model16','emm.SE_cooccur2'], cohort3['model16','emm_cooccur2']-1.96*cohort3['model16','emm.SE_cooccur2'], cohort3['model16','emm_cooccur2']+1.96*cohort3['model16','emm.SE_cooccur2'], 'cohort3', 'mdd_only', cohort3['model16', 'n_cooccur2'], cohort3['model16', c('cohensd2', 'SE2')]),
                                   cohort3_cooccur3 = c(cohort3['model16','emm_cooccur3'], cohort3['model16','emm.SE_cooccur3'], cohort3['model16','emm_cooccur3']-1.96*cohort3['model16','emm.SE_cooccur3'], cohort3['model16','emm_cooccur3']+1.96*cohort3['model16','emm.SE_cooccur3'], 'cohort3', 'cm_and_mdd', cohort3['model16', 'n_cooccur3'], cohort3['model16', c('cohensd3', 'SE3')]),
                                   
                                   cohort4_cooccur0 = c(cohort4['model16','emm_cooccur0'], cohort4['model16','emm.SE_cooccur0'], cohort4['model16','emm_cooccur0']-1.96*cohort4['model16','emm.SE_cooccur0'], cohort4['model16','emm_cooccur0']+1.96*cohort4['model16','emm.SE_cooccur0'], 'cohort4', 'no_cm_no_mdd', cohort4['model16', 'n_cooccur0'], NA, NA),
                                   cohort4_cooccur1 = c(cohort4['model16','emm_cooccur1'], cohort4['model16','emm.SE_cooccur1'], cohort4['model16','emm_cooccur1']-1.96*cohort4['model16','emm.SE_cooccur1'], cohort4['model16','emm_cooccur1']+1.96*cohort4['model16','emm.SE_cooccur1'], 'cohort4', 'cm_only', cohort4['model16', 'n_cooccur1'], cohort4['model16', c('cohensd1', 'SE1')]),
                                   cohort4_cooccur2 = c(cohort4['model16','emm_cooccur2'], cohort4['model16','emm.SE_cooccur2'], cohort4['model16','emm_cooccur2']-1.96*cohort4['model16','emm.SE_cooccur2'], cohort4['model16','emm_cooccur2']+1.96*cohort4['model16','emm.SE_cooccur2'], 'cohort4', 'mdd_only', cohort4['model16', 'n_cooccur2'], cohort4['model16', c('cohensd2', 'SE2')]),
                                   cohort4_cooccur3 = c(cohort4['model16','emm_cooccur3'], cohort4['model16','emm.SE_cooccur3'], cohort4['model16','emm_cooccur3']-1.96*cohort4['model16','emm.SE_cooccur3'], cohort4['model16','emm_cooccur3']+1.96*cohort4['model16','emm.SE_cooccur3'], 'cohort4', 'cm_and_mdd', cohort4['model16', 'n_cooccur3'], cohort4['model16', c('cohensd3', 'SE3')]),
                                   
                                   cohort5_cooccur0 = c(cohort5['model16','emm_cooccur0'], cohort5['model16','emm.SE_cooccur0'], cohort5['model16','emm_cooccur0']-1.96*cohort5['model16','emm.SE_cooccur0'], cohort5['model16','emm_cooccur0']+1.96*cohort5['model16','emm.SE_cooccur0'], 'cohort5', 'no_cm_no_mdd', cohort5['model16', 'n_cooccur0'], NA, NA),
                                   cohort5_cooccur1 = c(cohort5['model16','emm_cooccur1'], cohort5['model16','emm.SE_cooccur1'], cohort5['model16','emm_cooccur1']-1.96*cohort5['model16','emm.SE_cooccur1'], cohort5['model16','emm_cooccur1']+1.96*cohort5['model16','emm.SE_cooccur1'], 'cohort5', 'cm_only', cohort5['model16', 'n_cooccur1'], cohort5['model16', c('cohensd1', 'SE1')]),
                                   cohort5_cooccur2 = c(cohort5['model16','emm_cooccur2'], cohort5['model16','emm.SE_cooccur2'], cohort5['model16','emm_cooccur2']-1.96*cohort5['model16','emm.SE_cooccur2'], cohort5['model16','emm_cooccur2']+1.96*cohort5['model16','emm.SE_cooccur2'], 'cohort5', 'mdd_only', cohort5['model16', 'n_cooccur2'], cohort5['model16', c('cohensd2', 'SE2')]),
                                   cohort5_cooccur3 = c(cohort5['model16','emm_cooccur3'], cohort5['model16','emm.SE_cooccur3'], cohort5['model16','emm_cooccur3']-1.96*cohort5['model16','emm.SE_cooccur3'], cohort5['model16','emm_cooccur3']+1.96*cohort5['model16','emm.SE_cooccur3'], 'cohort5', 'cm_and_mdd', cohort5['model16', 'n_cooccur3'], cohort5['model16', c('cohensd3', 'SE3')]),
                                   
                                   cohort6_cooccur0 = c(cohort6['model16','emm_cooccur0'], cohort6['model16','emm.SE_cooccur0'], cohort6['model16','emm_cooccur0']-1.96*cohort6['model16','emm.SE_cooccur0'], cohort6['model16','emm_cooccur0']+1.96*cohort6['model16','emm.SE_cooccur0'], 'cohort6', 'no_cm_no_mdd', cohort6['model16', 'n_cooccur0'], NA, NA),
                                   cohort6_cooccur1 = c(cohort6['model16','emm_cooccur1'], cohort6['model16','emm.SE_cooccur1'], cohort6['model16','emm_cooccur1']-1.96*cohort6['model16','emm.SE_cooccur1'], cohort6['model16','emm_cooccur1']+1.96*cohort6['model16','emm.SE_cooccur1'], 'cohort6', 'cm_only', cohort6['model16', 'n_cooccur1'], cohort6['model16', c('cohensd1', 'SE1')]),
                                   cohort6_cooccur2 = c(cohort6['model16','emm_cooccur2'], cohort6['model16','emm.SE_cooccur2'], cohort6['model16','emm_cooccur2']-1.96*cohort6['model16','emm.SE_cooccur2'], cohort6['model16','emm_cooccur2']+1.96*cohort6['model16','emm.SE_cooccur2'], 'cohort6', 'mdd_only', cohort6['model16', 'n_cooccur2'], cohort6['model16', c('cohensd2', 'SE2')]),
                                   cohort6_cooccur3 = c(cohort6['model16','emm_cooccur3'], cohort6['model16','emm.SE_cooccur3'], cohort6['model16','emm_cooccur3']-1.96*cohort6['model16','emm.SE_cooccur3'], cohort6['model16','emm_cooccur3']+1.96*cohort6['model16','emm.SE_cooccur3'], 'cohort6', 'cm_and_mdd', cohort6['model16', 'n_cooccur3'], cohort6['model16', c('cohensd3', 'SE3')]),
                                   
                                   cohort7_cooccur0 = c(cohort7['model16','emm_cooccur0'], cohort7['model16','emm.SE_cooccur0'], cohort7['model16','emm_cooccur0']-1.96*cohort7['model16','emm.SE_cooccur0'], cohort7['model16','emm_cooccur0']+1.96*cohort7['model16','emm.SE_cooccur0'], 'cohort7', 'no_cm_no_mdd', cohort7['model16', 'n_cooccur0'], NA, NA),
                                   cohort7_cooccur1 = c(cohort7['model16','emm_cooccur1'], cohort7['model16','emm.SE_cooccur1'], cohort7['model16','emm_cooccur1']-1.96*cohort7['model16','emm.SE_cooccur1'], cohort7['model16','emm_cooccur1']+1.96*cohort7['model16','emm.SE_cooccur1'], 'cohort7', 'cm_only', cohort7['model16', 'n_cooccur1'], cohort7['model16', c('cohensd1', 'SE1')]),
                                   cohort7_cooccur2 = c(cohort7['model16','emm_cooccur2'], cohort7['model16','emm.SE_cooccur2'], cohort7['model16','emm_cooccur2']-1.96*cohort7['model16','emm.SE_cooccur2'], cohort7['model16','emm_cooccur2']+1.96*cohort7['model16','emm.SE_cooccur2'], 'cohort7', 'mdd_only', cohort7['model16', 'n_cooccur2'], cohort7['model16', c('cohensd2', 'SE2')]),
                                   cohort7_cooccur3 = c(cohort7['model16','emm_cooccur3'], cohort7['model16','emm.SE_cooccur3'], cohort7['model16','emm_cooccur3']-1.96*cohort7['model16','emm.SE_cooccur3'], cohort7['model16','emm_cooccur3']+1.96*cohort7['model16','emm.SE_cooccur3'], 'cohort7', 'cm_and_mdd', cohort7['model16', 'n_cooccur3'], cohort7['model16', c('cohensd3', 'SE3')]),
                                   
                                   cohort8_cooccur0 = c(cohort8['model16','emm_cooccur0'], cohort8['model16','emm.SE_cooccur0'], cohort8['model16','emm_cooccur0']-1.96*cohort8['model16','emm.SE_cooccur0'], cohort8['model16','emm_cooccur0']+1.96*cohort8['model16','emm.SE_cooccur0'], 'cohort8', 'no_cm_no_mdd', cohort8['model16', 'n_cooccur0'], NA, NA),
                                   cohort8_cooccur1 = c(cohort8['model16','emm_cooccur1'], cohort8['model16','emm.SE_cooccur1'], cohort8['model16','emm_cooccur1']-1.96*cohort8['model16','emm.SE_cooccur1'], cohort8['model16','emm_cooccur1']+1.96*cohort8['model16','emm.SE_cooccur1'], 'cohort8', 'cm_only', cohort8['model16', 'n_cooccur1'], cohort8['model16', c('cohensd1', 'SE1')]),
                                   cohort8_cooccur2 = c(cohort8['model16','emm_cooccur2'], cohort8['model16','emm.SE_cooccur2'], cohort8['model16','emm_cooccur2']-1.96*cohort8['model16','emm.SE_cooccur2'], cohort8['model16','emm_cooccur2']+1.96*cohort8['model16','emm.SE_cooccur2'], 'cohort8', 'mdd_only', cohort8['model16', 'n_cooccur2'], cohort8['model16', c('cohensd2', 'SE2')]),
                                   cohort8_cooccur3 = c(cohort8['model16','emm_cooccur3'], cohort8['model16','emm.SE_cooccur3'], cohort8['model16','emm_cooccur3']-1.96*cohort8['model16','emm.SE_cooccur3'], cohort8['model16','emm_cooccur3']+1.96*cohort8['model16','emm.SE_cooccur3'], 'cohort8', 'cm_and_mdd', cohort8['model16', 'n_cooccur3'], cohort8['model16', c('cohensd3', 'SE3')]),
                                   
                                   cohort9_cooccur0 = c(cohort9['model16','emm_cooccur0'], cohort9['model16','emm.SE_cooccur0'], cohort9['model16','emm_cooccur0']-1.96*cohort9['model16','emm.SE_cooccur0'], cohort9['model16','emm_cooccur0']+1.96*cohort9['model16','emm.SE_cooccur0'], 'cohort9', 'no_cm_no_mdd', cohort9['model16', 'n_cooccur0'], NA, NA),
                                   cohort9_cooccur1 = c(cohort9['model16','emm_cooccur1'], cohort9['model16','emm.SE_cooccur1'], cohort9['model16','emm_cooccur1']-1.96*cohort9['model16','emm.SE_cooccur1'], cohort9['model16','emm_cooccur1']+1.96*cohort9['model16','emm.SE_cooccur1'], 'cohort9', 'cm_only', cohort9['model16', 'n_cooccur1'], cohort9['model16', c('cohensd1', 'SE1')]),
                                   cohort9_cooccur2 = c(cohort9['model16','emm_cooccur2'], cohort9['model16','emm.SE_cooccur2'], cohort9['model16','emm_cooccur2']-1.96*cohort9['model16','emm.SE_cooccur2'], cohort9['model16','emm_cooccur2']+1.96*cohort9['model16','emm.SE_cooccur2'], 'cohort9', 'mdd_only', cohort9['model16', 'n_cooccur2'], cohort9['model16', c('cohensd2', 'SE2')]),
                                   cohort9_cooccur3 = c(cohort9['model16','emm_cooccur3'], cohort9['model16','emm.SE_cooccur3'], cohort9['model16','emm_cooccur3']-1.96*cohort9['model16','emm.SE_cooccur3'], cohort9['model16','emm_cooccur3']+1.96*cohort9['model16','emm.SE_cooccur3'], 'cohort9', 'cm_and_mdd', cohort9['model16', 'n_cooccur3'], cohort9['model16', c('cohensd3', 'SE3')]),
                                   
                                   cohort10_cooccur0 = c(cohort10['model16','emm_cooccur0'], cohort10['model16','emm.SE_cooccur0'], cohort10['model16','emm_cooccur0']-1.96*cohort10['model16','emm.SE_cooccur0'], cohort10['model16','emm_cooccur0']+1.96*cohort10['model16','emm.SE_cooccur0'], 'cohort10', 'no_cm_no_mdd', cohort10['model16', 'n_cooccur0'], NA, NA),
                                   cohort10_cooccur1 = c(cohort10['model16','emm_cooccur1'], cohort10['model16','emm.SE_cooccur1'], cohort10['model16','emm_cooccur1']-1.96*cohort10['model16','emm.SE_cooccur1'], cohort10['model16','emm_cooccur1']+1.96*cohort10['model16','emm.SE_cooccur1'], 'cohort10', 'cm_only', cohort10['model16', 'n_cooccur1'], cohort10['model16', c('cohensd1', 'SE1')]),
                                   cohort10_cooccur2 = c(cohort10['model16','emm_cooccur2'], cohort10['model16','emm.SE_cooccur2'], cohort10['model16','emm_cooccur2']-1.96*cohort10['model16','emm.SE_cooccur2'], cohort10['model16','emm_cooccur2']+1.96*cohort10['model16','emm.SE_cooccur2'], 'cohort10', 'mdd_only', cohort10['model16', 'n_cooccur2'], cohort10['model16', c('cohensd2', 'SE2')]),
                                   cohort10_cooccur3 = c(cohort10['model16','emm_cooccur3'], cohort10['model16','emm.SE_cooccur3'], cohort10['model16','emm_cooccur3']-1.96*cohort10['model16','emm.SE_cooccur3'], cohort10['model16','emm_cooccur3']+1.96*cohort10['model16','emm.SE_cooccur3'], 'cohort10', 'cm_and_mdd', cohort10['model16', 'n_cooccur3'], cohort10['model16', c('cohensd3', 'SE3')]),
                                   
                                   cohort11_cooccur0 = c(cohort11['model16','emm_cooccur0'], cohort11['model16','emm.SE_cooccur0'], cohort11['model16','emm_cooccur0']-1.96*cohort11['model16','emm.SE_cooccur0'], cohort11['model16','emm_cooccur0']+1.96*cohort11['model16','emm.SE_cooccur0'], 'cohort11', 'no_cm_no_mdd', cohort11['model16', 'n_cooccur0'], NA, NA),
                                   cohort11_cooccur1 = c(cohort11['model16','emm_cooccur1'], cohort11['model16','emm.SE_cooccur1'], cohort11['model16','emm_cooccur1']-1.96*cohort11['model16','emm.SE_cooccur1'], cohort11['model16','emm_cooccur1']+1.96*cohort11['model16','emm.SE_cooccur1'], 'cohort11', 'cm_only', cohort11['model16', 'n_cooccur0'], cohort11['model16', c('cohensd1', 'SE1')]),
                                   cohort11_cooccur2 = c(cohort11['model16','emm_cooccur2'], cohort11['model16','emm.SE_cooccur2'], cohort11['model16','emm_cooccur2']-1.96*cohort11['model16','emm.SE_cooccur2'], cohort11['model16','emm_cooccur2']+1.96*cohort11['model16','emm.SE_cooccur2'], 'cohort11', 'mdd_only', cohort11['model16', 'n_cooccur0'], cohort11['model16', c('cohensd2', 'SE2')]),
                                   cohort11_cooccur3 = c(cohort11['model16','emm_cooccur3'], cohort11['model16','emm.SE_cooccur3'], cohort11['model16','emm_cooccur3']-1.96*cohort11['model16','emm.SE_cooccur3'], cohort11['model16','emm_cooccur3']+1.96*cohort11['model16','emm.SE_cooccur3'], 'cohort11', 'cm_and_mdd', cohort11['model16', 'n_cooccur0'], cohort11['model16', c('cohensd3', 'SE3')])))

colnames(Model16_emm) <- c('emm', 'SE','CI_lb','CI_ub','cohort','group', 'n', 'cohensd', 'SE_cohensd')

Model16_emm$group <- factor(Model16_emm$group, levels = c("no_cm_no_mdd","cm_only","mdd_only","cm_and_mdd"))



#################### 12. Random-effect meta-analysis of models 1, 2, 3, and 4 #################### 

# Pooled analyses of metabolic outcomes with childhood maltreatment (vs. no childhood maltreatment)
RE_model1 <- rma(yi = unlist(cohensd), sei = unlist(SE), data = Model1_cm)
summary(RE_model1)

RE_model2 <- rma(yi = unlist(cohensd), sei = unlist(SE), data = Model2_cm)
summary(RE_model2)

RE_model3 <- rma(yi = unlist(cohensd), sei = unlist(SE), data = Model3_cm)
summary(RE_model3)

RE_model4 <- rma(yi = unlist(cohensd), sei = unlist(SE), data = Model4_cm)
summary(RE_model4)



#################### 13. Random-effect meta-analysis of models 5, 6, 7, and 8 #################### 

# Pooled analyses of metabolic outcomes with depression (vs. no depression)
RE_model5 <- rma(yi = unlist(cohensd), sei = unlist(SE), data = Model5_mdd)
summary(RE_model5)

RE_model6 <- rma(yi = unlist(cohensd), sei = unlist(SE), data = Model6_mdd)
summary(RE_model6)

RE_model7 <- rma(yi = unlist(cohensd), sei = unlist(SE), data = Model7_mdd)
summary(RE_model7)

RE_model8 <- rma(yi = unlist(cohensd), sei = unlist(SE), data = Model8_mdd)
summary(RE_model8)



#################### 14. Random-effect meta-analysis of models 9, 10, 11, and 12 #################### 

##### Model 9, cooccur 1 - pooled analysis of bmi associations with childhood maltreatment only (vs. no childhood maltreatment and no depression)
RE_model9_cooccur1_vs_cooccur0 <- rma(yi = unlist(cohensd), sei = unlist(SE), data = Model9_cooccur1_vs_cooccur0)
summary(RE_model9_cooccur1_vs_cooccur0)

#Subgroup analyses: case-control vs. population-based studies; and self-reports vs. clinical interviews
RE_model9_cooccur1_vs_cooccur0_cas <- rma(yi = unlist(cohensd), sei = unlist(SE), data=Model9_cooccur1_vs_cooccur0[c('cohort1','cohort2'),])
summary(RE_model9_cooccur1_vs_cooccur0_cas)
RE_model9_cooccur1_vs_cooccur0_pop <- rma(yi = unlist(cohensd), sei = unlist(SE), data=Model9_cooccur1_vs_cooccur0[-c(1,2),])
summary(RE_model9_cooccur1_vs_cooccur0_pop)

RE_model9_cooccur1_vs_cooccur0_SR <- rma(yi = unlist(cohensd), sei = unlist(SE), data=Model9_cooccur1_vs_cooccur0[c('cohort6','cohort7','cohort8','cohort9','cohort10'),])
summary(RE_model9_cooccur1_vs_cooccur0_SR)
RE_model9_cooccur1_vs_cooccur0_IV <- rma(yi = unlist(cohensd), sei = unlist(SE), data=Model9_cooccur1_vs_cooccur0[-c(6,7,8,9,10),])
summary(RE_model9_cooccur1_vs_cooccur0_IV)

#Leave-one out analysis - cohort2
RE_model9_cooccur1_vs_cooccur0_nocohort2 <- rma(yi = unlist(cohensd), sei = unlist(SE), data=Model9_cooccur1_vs_cooccur0[-2,])
summary(RE_model9_cooccur1_vs_cooccur0_nocohort2)
#Leave-one out analysis - cohort7
RE_model9_cooccur1_vs_cooccur0_nocohort7 <- rma(yi = unlist(cohensd), sei = unlist(SE), data=Model9_cooccur1_vs_cooccur0[-7,])
summary(RE_model9_cooccur1_vs_cooccur0_nocohort7)
#Leave-one out analysis - cohort11
RE_model9_cooccur1_vs_cooccur0_nocohort11 <- rma(yi = unlist(cohensd), sei = unlist(SE), data=Model9_cooccur1_vs_cooccur0[-11,])
summary(RE_model9_cooccur1_vs_cooccur0_nocohort11)


##### Model 9, cooccur 2 - pooled analysis of bmi associations with depression only (vs. no childhood maltreatment and no depression)
RE_model9_cooccur2_vs_cooccur0 <- rma(yi = unlist(cohensd), sei = unlist(SE), data = Model9_cooccur2_vs_cooccur0)
summary(RE_model9_cooccur2_vs_cooccur0)

#Subgroup analyses: case-control vs. population-based studies; and self-reports vs. clinical interviews
RE_model9_cooccur2_vs_cooccur0_cas <- rma(yi = unlist(cohensd), sei = unlist(SE), data=Model9_cooccur2_vs_cooccur0[c('cohort1','cohort2'),])
summary(RE_model9_cooccur2_vs_cooccur0_cas)
RE_model9_cooccur2_vs_cooccur0_pop <- rma(yi = unlist(cohensd), sei = unlist(SE), data=Model9_cooccur2_vs_cooccur0[-c(1,2),])
summary(RE_model9_cooccur2_vs_cooccur0_pop)

RE_model9_cooccur2_vs_cooccur0_SR <- rma(yi = unlist(cohensd), sei = unlist(SE), data=Model9_cooccur2_vs_cooccur0[c('cohort6','cohort7','cohort8','cohort9','cohort10'),])
summary(RE_model9_cooccur2_vs_cooccur0_SR)
RE_model9_cooccur2_vs_cooccur0_IV <- rma(yi = unlist(cohensd), sei = unlist(SE), data=Model9_cooccur2_vs_cooccur0[-c(6,7,8,9,10),])
summary(RE_model9_cooccur2_vs_cooccur0_IV)

#Leave-one out analysis - cohort2
RE_model9_cooccur2_vs_cooccur0_nocohort2 <- rma(yi = unlist(cohensd), sei = unlist(SE), data=Model9_cooccur2_vs_cooccur0[-2,])
summary(RE_model9_cooccur2_vs_cooccur0_nocohort2)
#Leave-one out analysis - cohort7
RE_model9_cooccur2_vs_cooccur0_nocohort7 <- rma(yi = unlist(cohensd), sei = unlist(SE), data=Model9_cooccur2_vs_cooccur0[-7,])
summary(RE_model9_cooccur2_vs_cooccur0_nocohort7)
#Leave-one out analysis - cohort11
RE_model9_cooccur2_vs_cooccur0_nocohort11 <- rma(yi = unlist(cohensd), sei = unlist(SE), data=Model9_cooccur2_vs_cooccur0[-11,])
summary(RE_model9_cooccur2_vs_cooccur0_nocohort11)

##### Model 9, cooccur 3 - pooled analysis of bmi associations with co-occurring childhood maltreatment and depression (vs. no childhood maltreatment and no depression)
RE_model9_cooccur3_vs_cooccur0 <- rma(yi = unlist(cohensd), sei = unlist(SE), data = Model9_cooccur3_vs_cooccur0)
summary(RE_model9_cooccur3_vs_cooccur0)

#Subgroup analyses: case-control vs. population-based studies; and self-reports vs. clinical interviews
RE_model9_cooccur3_vs_cooccur0_cas <- rma(yi = unlist(cohensd), sei = unlist(SE), data=Model9_cooccur3_vs_cooccur0[c('cohort1','cohort2'),])
summary(RE_model9_cooccur3_vs_cooccur0_cas)
RE_model9_cooccur3_vs_cooccur0_pop <- rma(yi = unlist(cohensd), sei = unlist(SE), data=Model9_cooccur3_vs_cooccur0[-c(1,2),])
summary(RE_model9_cooccur3_vs_cooccur0_pop)

RE_model9_cooccur3_vs_cooccur0_SR <- rma(yi = unlist(cohensd), sei = unlist(SE), data=Model9_cooccur3_vs_cooccur0[c('cohort6','cohort7','cohort8','cohort9','cohort10'),])
summary(RE_model9_cooccur3_vs_cooccur0_SR)
RE_model9_cooccur3_vs_cooccur0_IV <- rma(yi = unlist(cohensd), sei = unlist(SE), data=Model9_cooccur3_vs_cooccur0[-c(6,7,8,9,10),])
summary(RE_model9_cooccur3_vs_cooccur0_IV)

#Leave-one out analysis - cohort2
RE_model9_cooccur3_vs_cooccur0_nocohort2 <- rma(yi = unlist(cohensd), sei = unlist(SE), data=Model9_cooccur3_vs_cooccur0[-2,])
summary(RE_model9_cooccur3_vs_cooccur0_nocohort2)
#Leave-one out analysis - cohort7
RE_model9_cooccur3_vs_cooccur0_nocohort7 <- rma(yi = unlist(cohensd), sei = unlist(SE), data=Model9_cooccur3_vs_cooccur0[-7,])
summary(RE_model9_cooccur3_vs_cooccur0_nocohort7)
#Leave-one out analysis - cohort11
RE_model9_cooccur3_vs_cooccur0_nocohort11 <- rma(yi = unlist(cohensd), sei = unlist(SE), data=Model9_cooccur3_vs_cooccur0[-11,])
summary(RE_model9_cooccur3_vs_cooccur0_nocohort11)


##### Model 10 - pooled analysis of waist-to-hip ratio associations with childhood maltreatment, depression, and their cooccurrence (vs. no childhood maltreatment and no depression)
RE_model10_cooccur1_vs_cooccur0 <- rma(yi = unlist(cohensd), sei = unlist(SE), data = Model10_cooccur1_vs_cooccur0)
summary(RE_model10_cooccur1_vs_cooccur0)

RE_model10_cooccur2_vs_cooccur0 <- rma(yi = unlist(cohensd), sei = unlist(SE), data = Model10_cooccur2_vs_cooccur0)
summary(RE_model10_cooccur2_vs_cooccur0)

RE_model10_cooccur3_vs_cooccur0 <- rma(yi = unlist(cohensd), sei = unlist(SE), data = Model10_cooccur3_vs_cooccur0)
summary(RE_model10_cooccur3_vs_cooccur0)


##### Model 11 - pooled analysis of association of LDL/HDL ratio with childhood maltreatment, depression, and their cooccurrence (vs. no childhood maltreatment and no depression)
RE_model11_cooccur1_vs_cooccur0 <- rma(yi = unlist(cohensd), sei = unlist(SE), data = Model11_cooccur1_vs_cooccur0)
summary(RE_model11_cooccur1_vs_cooccur0)

RE_model11_cooccur2_vs_cooccur0 <- rma(yi = unlist(cohensd), sei = unlist(SE), data = Model11_cooccur2_vs_cooccur0)
summary(RE_model11_cooccur2_vs_cooccur0)

RE_model11_cooccur3_vs_cooccur0 <- rma(yi = unlist(cohensd), sei = unlist(SE), data = Model11_cooccur3_vs_cooccur0)
summary(RE_model11_cooccur3_vs_cooccur0)


##### Model 12 - pooled analysis of association of triglycerides with childhood maltreatment, depression, and their cooccurrence (vs. no childhood maltreatment and no depression)
RE_model12_cooccur1_vs_cooccur0 <- rma(yi = unlist(cohensd), sei = unlist(SE), data = Model12_cooccur1_vs_cooccur0)
summary(RE_model12_cooccur1_vs_cooccur0)

RE_model12_cooccur2_vs_cooccur0 <- rma(yi = unlist(cohensd), sei = unlist(SE), data = Model12_cooccur2_vs_cooccur0)
summary(RE_model12_cooccur2_vs_cooccur0)

RE_model12_cooccur3_vs_cooccur0 <- rma(yi = unlist(cohensd), sei = unlist(SE), data = Model12_cooccur3_vs_cooccur0)
summary(RE_model12_cooccur3_vs_cooccur0)



#################### 15. Random-effect meta-analysis of models 13, 14, 15, and 16 ####################

##### Model 13, cooccur 1 - pooled analysis of bmi categorical associations with childhood maltreatment only (vs. no childhood maltreatment and no depression)
RE_model13_cooccur1_vs_cooccur0_bmi_cat1 <- rma(yi = unlist(log_OR), sei = unlist(SE), data = Model13_cooccur1_vs_cooccur0_bmi_cat1)
summary(RE_model13_cooccur1_vs_cooccur0_bmi_cat1)
OR_cm_underweight <- exp(RE_model13_cooccur1_vs_cooccur0_bmi_cat1$beta)
lb_OR_cm_underweight <- exp(RE_model13_cooccur1_vs_cooccur0_bmi_cat1$beta-1.96*RE_model13_cooccur1_vs_cooccur0_bmi_cat1$se)
ub_OR_cm_underweight <- exp(RE_model13_cooccur1_vs_cooccur0_bmi_cat1$beta+1.96*RE_model13_cooccur1_vs_cooccur0_bmi_cat1$se)

RE_model13_cooccur1_vs_cooccur0_bmi_cat3 <- rma(yi = unlist(log_OR), sei = unlist(SE), data = Model13_cooccur1_vs_cooccur0_bmi_cat3)
summary(RE_model13_cooccur1_vs_cooccur0_bmi_cat3)
OR_cm_overweight <- exp(RE_model13_cooccur1_vs_cooccur0_bmi_cat3$beta)
lb_OR_cm_overweight <- exp(RE_model13_cooccur1_vs_cooccur0_bmi_cat3$beta-1.96*RE_model13_cooccur1_vs_cooccur0_bmi_cat3$se)
ub_OR_cm_overweight <- exp(RE_model13_cooccur1_vs_cooccur0_bmi_cat3$beta+1.96*RE_model13_cooccur1_vs_cooccur0_bmi_cat3$se)

RE_model13_cooccur1_vs_cooccur0_bmi_cat4 <- rma(yi = unlist(log_OR), sei = unlist(SE), data = Model13_cooccur1_vs_cooccur0_bmi_cat4)
summary(RE_model13_cooccur1_vs_cooccur0_bmi_cat4)
OR_cm_obese <- exp(RE_model13_cooccur1_vs_cooccur0_bmi_cat4$beta)
lb_OR_cm_obese <- exp(RE_model13_cooccur1_vs_cooccur0_bmi_cat4$beta-1.96*RE_model13_cooccur1_vs_cooccur0_bmi_cat4$se)
ub_OR_cm_obese <- exp(RE_model13_cooccur1_vs_cooccur0_bmi_cat4$beta+1.96*RE_model13_cooccur1_vs_cooccur0_bmi_cat4$se)

##### Model 13, cooccur 2 - pooled analysis of bmi categorical associations with depression only (vs. no childhood maltreatment and no depression)
RE_model13_cooccur2_vs_cooccur0_bmi_cat1 <- rma(yi = unlist(log_OR), sei = unlist(SE), data = Model13_cooccur2_vs_cooccur0_bmi_cat1)
summary(RE_model13_cooccur2_vs_cooccur0_bmi_cat1)
OR_mdd_underweight <- exp(RE_model13_cooccur2_vs_cooccur0_bmi_cat1$beta)
lb_OR_mdd_underweight <- exp(RE_model13_cooccur2_vs_cooccur0_bmi_cat1$beta-1.96*RE_model13_cooccur2_vs_cooccur0_bmi_cat1$se)
ub_OR_mdd_underweight <- exp(RE_model13_cooccur2_vs_cooccur0_bmi_cat1$beta+1.96*RE_model13_cooccur2_vs_cooccur0_bmi_cat1$se)

RE_model13_cooccur2_vs_cooccur0_bmi_cat3 <- rma(yi = unlist(log_OR), sei = unlist(SE), data = Model13_cooccur2_vs_cooccur0_bmi_cat3)
summary(RE_model13_cooccur2_vs_cooccur0_bmi_cat3)
OR_mdd_overweight <- exp(RE_model13_cooccur2_vs_cooccur0_bmi_cat3$beta)
lb_OR_mdd_overweight <- exp(RE_model13_cooccur2_vs_cooccur0_bmi_cat3$beta-1.96*RE_model13_cooccur2_vs_cooccur0_bmi_cat3$se)
ub_OR_mdd_overweight <- exp(RE_model13_cooccur2_vs_cooccur0_bmi_cat3$beta+1.96*RE_model13_cooccur2_vs_cooccur0_bmi_cat3$se)

RE_model13_cooccur2_vs_cooccur0_bmi_cat4 <- rma(yi = unlist(log_OR), sei = unlist(SE), data = Model13_cooccur2_vs_cooccur0_bmi_cat4)
summary(RE_model13_cooccur2_vs_cooccur0_bmi_cat4)
OR_mdd_obese <- exp(RE_model13_cooccur2_vs_cooccur0_bmi_cat4$beta)
lb_OR_mdd_obese <- exp(RE_model13_cooccur2_vs_cooccur0_bmi_cat4$beta-1.96*RE_model13_cooccur2_vs_cooccur0_bmi_cat4$se)
ub_OR_mdd_obese <- exp(RE_model13_cooccur2_vs_cooccur0_bmi_cat4$beta+1.96*RE_model13_cooccur2_vs_cooccur0_bmi_cat4$se)

##### Model 13, cooccur 3 - pooled analysis of bmi categorical associations with cooccurring childhood maltreatment and depression (vs. no childhood maltreatment and no depression)
RE_model13_cooccur3_vs_cooccur0_bmi_cat1 <- rma(yi = unlist(log_OR), sei = unlist(SE), data = Model13_cooccur3_vs_cooccur0_bmi_cat1)
summary(RE_model13_cooccur3_vs_cooccur0_bmi_cat1)
OR_cm_and_mdd_underweight <- exp(RE_model13_cooccur3_vs_cooccur0_bmi_cat1$beta)
lb_OR_cm_and_mdd_underweight <- exp(RE_model13_cooccur3_vs_cooccur0_bmi_cat1$beta-1.96*RE_model13_cooccur3_vs_cooccur0_bmi_cat1$se)
ub_OR_cm_and_mdd_underweight <- exp(RE_model13_cooccur3_vs_cooccur0_bmi_cat1$beta+1.96*RE_model13_cooccur3_vs_cooccur0_bmi_cat1$se)

RE_model13_cooccur3_vs_cooccur0_bmi_cat3 <- rma(yi = unlist(log_OR), sei = unlist(SE), data = Model13_cooccur3_vs_cooccur0_bmi_cat3)
summary(RE_model13_cooccur3_vs_cooccur0_bmi_cat3)
OR_cm_and_mdd_overweight <- exp(RE_model13_cooccur3_vs_cooccur0_bmi_cat3$beta)
lb_OR_cm_and_mdd_overweight <- exp(RE_model13_cooccur3_vs_cooccur0_bmi_cat3$beta-1.96*RE_model13_cooccur3_vs_cooccur0_bmi_cat3$se)
ub_OR_cm_and_mdd_overweight <- exp(RE_model13_cooccur3_vs_cooccur0_bmi_cat3$beta+1.96*RE_model13_cooccur3_vs_cooccur0_bmi_cat3$se)

RE_model13_cooccur3_vs_cooccur0_bmi_cat4 <- rma(yi = unlist(log_OR), sei = unlist(SE), data = Model13_cooccur3_vs_cooccur0_bmi_cat4)
summary(RE_model13_cooccur3_vs_cooccur0_bmi_cat4)
OR_cm_and_mdd_obese <- exp(RE_model13_cooccur3_vs_cooccur0_bmi_cat4$beta)
lb_OR_cm_and_mdd_obese <- exp(RE_model13_cooccur3_vs_cooccur0_bmi_cat4$beta-1.96*RE_model13_cooccur3_vs_cooccur0_bmi_cat4$se)
ub_OR_cm_and_mdd_obese <- exp(RE_model13_cooccur3_vs_cooccur0_bmi_cat4$beta+1.96*RE_model13_cooccur3_vs_cooccur0_bmi_cat4$se)


##### Model 14 - pooled analysis of waist-to-hip ratio binomial associations with childhood maltreatment only, depression only, and cooccurring childhood maltreatment and depression (vs. no childhood maltreatment and no depression)
RE_model14_cooccur1_vs_cooccur0_outcome_dic <- rma(yi = unlist(log_OR), sei = unlist(SE), data = Model14_cooccur1_vs_cooccur0_outcome_dic)
summary(RE_model14_cooccur1_vs_cooccur0_outcome_dic)
exp(RE_model14_cooccur1_vs_cooccur0_outcome_dic$beta)
exp(RE_model14_cooccur1_vs_cooccur0_outcome_dic$beta-1.96*RE_model14_cooccur1_vs_cooccur0_outcome_dic$se)
exp(RE_model14_cooccur1_vs_cooccur0_outcome_dic$beta+1.96*RE_model14_cooccur1_vs_cooccur0_outcome_dic$se)

RE_model14_cooccur2_vs_cooccur0_outcome_dic <- rma(yi = unlist(log_OR), sei = unlist(SE), data = Model14_cooccur2_vs_cooccur0_outcome_dic)
summary(RE_model14_cooccur2_vs_cooccur0_outcome_dic)
exp(RE_model14_cooccur2_vs_cooccur0_outcome_dic$beta)
exp(RE_model14_cooccur2_vs_cooccur0_outcome_dic$beta-1.96*RE_model14_cooccur2_vs_cooccur0_outcome_dic$se)
exp(RE_model14_cooccur2_vs_cooccur0_outcome_dic$beta+1.96*RE_model14_cooccur2_vs_cooccur0_outcome_dic$se)

RE_model14_cooccur3_vs_cooccur0_outcome_dic <- rma(yi = unlist(log_OR), sei = unlist(SE), data = Model14_cooccur3_vs_cooccur0_outcome_dic)
summary(RE_model14_cooccur3_vs_cooccur0_outcome_dic)
exp(RE_model14_cooccur3_vs_cooccur0_outcome_dic$beta)
exp(RE_model14_cooccur3_vs_cooccur0_outcome_dic$beta-1.96*RE_model14_cooccur3_vs_cooccur0_outcome_dic$se)
exp(RE_model14_cooccur3_vs_cooccur0_outcome_dic$beta+1.96*RE_model14_cooccur3_vs_cooccur0_outcome_dic$se)

##### Model 15 - pooled analysis of triglycerides binomial associations with childhood maltreatment only, depression only, and cooccurring childhood maltreatment and depression (vs. no childhood maltreatment and no depression)
RE_model15_cooccur1_vs_cooccur0_outcome_dic <- rma(yi = unlist(log_OR), sei = unlist(SE), data = Model15_cooccur1_vs_cooccur0_outcome_dic)
summary(RE_model15_cooccur1_vs_cooccur0_outcome_dic)
exp(RE_model15_cooccur1_vs_cooccur0_outcome_dic$beta)
exp(RE_model15_cooccur1_vs_cooccur0_outcome_dic$beta-1.96*RE_model15_cooccur1_vs_cooccur0_outcome_dic$se)
exp(RE_model15_cooccur1_vs_cooccur0_outcome_dic$beta+1.96*RE_model15_cooccur1_vs_cooccur0_outcome_dic$se)

RE_model15_cooccur2_vs_cooccur0_outcome_dic <- rma(yi = unlist(log_OR), sei = unlist(SE), data = Model15_cooccur2_vs_cooccur0_outcome_dic)
summary(RE_model15_cooccur2_vs_cooccur0_outcome_dic)
exp(RE_model15_cooccur2_vs_cooccur0_outcome_dic$beta)
exp(RE_model15_cooccur2_vs_cooccur0_outcome_dic$beta-1.96*RE_model15_cooccur2_vs_cooccur0_outcome_dic$se)
exp(RE_model15_cooccur2_vs_cooccur0_outcome_dic$beta+1.96*RE_model15_cooccur2_vs_cooccur0_outcome_dic$se)

RE_model15_cooccur3_vs_cooccur0_outcome_dic <- rma(yi = unlist(log_OR), sei = unlist(SE), data = Model15_cooccur3_vs_cooccur0_outcome_dic)
summary(RE_model15_cooccur3_vs_cooccur0_outcome_dic)
exp(RE_model15_cooccur3_vs_cooccur0_outcome_dic$beta)
exp(RE_model15_cooccur3_vs_cooccur0_outcome_dic$beta-1.96*RE_model15_cooccur3_vs_cooccur0_outcome_dic$se)
exp(RE_model15_cooccur3_vs_cooccur0_outcome_dic$beta+1.96*RE_model15_cooccur3_vs_cooccur0_outcome_dic$se)

##### Model 16 - pooled analysis of ldl/hdl ratio binomial associations with childhood maltreatment only, depression only, and cooccurring childhood maltreatment and depression (vs. no childhood maltreatment and no depression)
RE_model16_cooccur1_vs_cooccur0_outcome_dic <- rma(yi = unlist(log_OR), sei = unlist(SE), data = Model16_cooccur1_vs_cooccur0_outcome_dic)
summary(RE_model16_cooccur1_vs_cooccur0_outcome_dic)
exp(RE_model16_cooccur1_vs_cooccur0_outcome_dic$beta)
exp(RE_model16_cooccur1_vs_cooccur0_outcome_dic$beta-1.96*RE_model16_cooccur1_vs_cooccur0_outcome_dic$se)
exp(RE_model16_cooccur1_vs_cooccur0_outcome_dic$beta+1.96*RE_model16_cooccur1_vs_cooccur0_outcome_dic$se)

RE_model16_cooccur2_vs_cooccur0_outcome_dic <- rma(yi = unlist(log_OR), sei = unlist(SE), data = Model16_cooccur2_vs_cooccur0_outcome_dic)
summary(RE_model16_cooccur2_vs_cooccur0_outcome_dic)
exp(RE_model16_cooccur2_vs_cooccur0_outcome_dic$beta)
exp(RE_model16_cooccur2_vs_cooccur0_outcome_dic$beta-1.96*RE_model16_cooccur2_vs_cooccur0_outcome_dic$se)
exp(RE_model16_cooccur2_vs_cooccur0_outcome_dic$beta+1.96*RE_model16_cooccur2_vs_cooccur0_outcome_dic$se)

RE_model16_cooccur3_vs_cooccur0_outcome_dic <- rma(yi = unlist(log_OR), sei = unlist(SE), data = Model16_cooccur3_vs_cooccur0_outcome_dic)
summary(RE_model16_cooccur3_vs_cooccur0_outcome_dic)
exp(RE_model16_cooccur3_vs_cooccur0_outcome_dic$beta)
exp(RE_model16_cooccur3_vs_cooccur0_outcome_dic$beta-1.96*RE_model16_cooccur3_vs_cooccur0_outcome_dic$se)
exp(RE_model16_cooccur3_vs_cooccur0_outcome_dic$beta+1.96*RE_model16_cooccur3_vs_cooccur0_outcome_dic$se)


#################### 16. Random-effect meta-analysis of models 17, 18, 19, and 20 ####################

##### Model 17 - pooled analysis of bmi associations with childhood maltreatment, depression and their cooccurrence, adjusting for lifestyle covariates
RE_model17_cooccur1_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model17_cooccur1_vs_cooccur0)
summary(RE_model17_cooccur1_vs_cooccur0)

RE_model17_cooccur2_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model17_cooccur2_vs_cooccur0)
summary(RE_model17_cooccur2_vs_cooccur0)

RE_model17_cooccur3_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = unlist(SE), data = Model17_cooccur3_vs_cooccur0)
summary(RE_model17_cooccur3_vs_cooccur0)

##### Model 18 - pooled analysis of waist/hip ratio associations with childhood maltreatment, depression and their cooccurrence, adjusting for lifestyle covariates
RE_model18_cooccur1_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model18_cooccur1_vs_cooccur0)
summary(RE_model18_cooccur1_vs_cooccur0)

RE_model18_cooccur2_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model18_cooccur2_vs_cooccur0)
summary(RE_model18_cooccur2_vs_cooccur0)

RE_model18_cooccur3_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = unlist(SE), data = Model18_cooccur3_vs_cooccur0)
summary(RE_model18_cooccur3_vs_cooccur0)

##### Model 19 - pooled analysis of triglycerides associations with childhood maltreatment, depression and their cooccurrence, adjusting for lifestyle covariates
RE_model19_cooccur1_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model19_cooccur1_vs_cooccur0)
summary(RE_model19_cooccur1_vs_cooccur0)

RE_model19_cooccur2_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model19_cooccur2_vs_cooccur0)
summary(RE_model19_cooccur2_vs_cooccur0)

RE_model19_cooccur3_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = unlist(SE), data = Model19_cooccur3_vs_cooccur0)
summary(RE_model19_cooccur3_vs_cooccur0)

##### Model 20 - pooled analysis of ldl/hdl ratio associations with childhood maltreatment, depression and their cooccurrence, adjusting for lifestyle covariates
RE_model20_cooccur1_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model20_cooccur1_vs_cooccur0)
summary(RE_model20_cooccur1_vs_cooccur0)

RE_model20_cooccur2_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model20_cooccur2_vs_cooccur0)
summary(RE_model20_cooccur2_vs_cooccur0)

RE_model20_cooccur3_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = unlist(SE), data = Model20_cooccur3_vs_cooccur0)
summary(RE_model20_cooccur3_vs_cooccur0)


#################### 17. Random-effect meta-analysis of models 21, 22, 23, and 24 ####################

##### Model 21 - pooled analysis of bmi associations with childhood maltreatment, depression and their cooccurrence, excluding lipid-modifying medication and antidepressant users
RE_model21_cooccur1_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model21_cooccur1_vs_cooccur0)
summary(RE_model21_cooccur1_vs_cooccur0)

RE_model21_cooccur2_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model21_cooccur2_vs_cooccur0)
summary(RE_model21_cooccur2_vs_cooccur0)

RE_model21_cooccur3_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = unlist(SE), data = Model21_cooccur3_vs_cooccur0)
summary(RE_model21_cooccur3_vs_cooccur0)

##### Model 22 - pooled analysis of waist/hip ratio associations with childhood maltreatment, depression and their cooccurrence, excluding lipid-modifying medication and antidepressant users
RE_model22_cooccur1_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model22_cooccur1_vs_cooccur0)
summary(RE_model22_cooccur1_vs_cooccur0)

RE_model22_cooccur2_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model22_cooccur2_vs_cooccur0)
summary(RE_model22_cooccur2_vs_cooccur0)

RE_model22_cooccur3_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = unlist(SE), data = Model22_cooccur3_vs_cooccur0)
summary(RE_model22_cooccur3_vs_cooccur0)

##### Model 23 - pooled analysis of triglycerides associations with childhood maltreatment, depression and their cooccurrence, excluding lipid-modifying medication and antidepressant users
RE_model23_cooccur1_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model23_cooccur1_vs_cooccur0)
summary(RE_model23_cooccur1_vs_cooccur0)

RE_model23_cooccur2_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model23_cooccur2_vs_cooccur0)
summary(RE_model23_cooccur2_vs_cooccur0)

RE_model23_cooccur3_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = unlist(SE), data = Model23_cooccur3_vs_cooccur0)
summary(RE_model23_cooccur3_vs_cooccur0)

##### Model 24 - pooled analysis of ldl/hdl ratio associations with childhood maltreatment, depression and their cooccurrence, excluding lipid-modifying medication and antidepressant users
RE_model24_cooccur1_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model24_cooccur1_vs_cooccur0)
summary(RE_model24_cooccur1_vs_cooccur0)

RE_model24_cooccur2_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model24_cooccur2_vs_cooccur0)
summary(RE_model24_cooccur2_vs_cooccur0)

RE_model24_cooccur3_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = unlist(SE), data = Model24_cooccur3_vs_cooccur0)
summary(RE_model24_cooccur3_vs_cooccur0)


#################### 18. Random-effect meta-analysis of models 25, 26, 27, and 28 ####################

##### Model 25 - pooled analysis of bmi associations with childhood maltreatment, depression and their cooccurrence, in males only
RE_model25_cooccur1_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model25_cooccur1_vs_cooccur0)
summary(RE_model25_cooccur1_vs_cooccur0)

RE_model25_cooccur2_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model25_cooccur2_vs_cooccur0)
summary(RE_model25_cooccur2_vs_cooccur0)

RE_model25_cooccur3_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = unlist(SE), data = Model25_cooccur3_vs_cooccur0)
summary(RE_model25_cooccur3_vs_cooccur0)

##### Model 26 - pooled analysis of waist/hip ratio associations with childhood maltreatment, depression and their cooccurrence, in males only
RE_model26_cooccur1_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model26_cooccur1_vs_cooccur0)
summary(RE_model26_cooccur1_vs_cooccur0)

RE_model26_cooccur2_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model26_cooccur2_vs_cooccur0)
summary(RE_model26_cooccur2_vs_cooccur0)

RE_model26_cooccur3_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = unlist(SE), data = Model26_cooccur3_vs_cooccur0)
summary(RE_model26_cooccur3_vs_cooccur0)

##### Model 27 - pooled analysis of triglycerides associations with childhood maltreatment, depression and their cooccurrence, in males only
RE_model27_cooccur1_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model27_cooccur1_vs_cooccur0)
summary(RE_model27_cooccur1_vs_cooccur0)

RE_model27_cooccur2_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model27_cooccur2_vs_cooccur0)
summary(RE_model27_cooccur2_vs_cooccur0)

RE_model27_cooccur3_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = unlist(SE), data = Model27_cooccur3_vs_cooccur0)
summary(RE_model27_cooccur3_vs_cooccur0)

##### Model 28 - pooled analysis of ldl/hdl ratio associations with childhood maltreatment, depression and their cooccurrence, in males only
RE_model28_cooccur1_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model28_cooccur1_vs_cooccur0)
summary(RE_model28_cooccur1_vs_cooccur0)

RE_model28_cooccur2_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model28_cooccur2_vs_cooccur0)
summary(RE_model28_cooccur2_vs_cooccur0)

RE_model28_cooccur3_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = unlist(SE), data = Model28_cooccur3_vs_cooccur0)
summary(RE_model28_cooccur3_vs_cooccur0)


#################### 19. Random-effect meta-analysis of models 29, 30, 31, and 32 ####################

##### Model 29 - pooled analysis of bmi associations with childhood maltreatment, depression and their cooccurrence, in females only
RE_model29_cooccur1_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model29_cooccur1_vs_cooccur0)
summary(RE_model29_cooccur1_vs_cooccur0)

RE_model29_cooccur2_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model29_cooccur2_vs_cooccur0)
summary(RE_model29_cooccur2_vs_cooccur0)

RE_model29_cooccur3_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = unlist(SE), data = Model29_cooccur3_vs_cooccur0)
summary(RE_model29_cooccur3_vs_cooccur0)

##### Model 30 - pooled analysis of waist/hip ratio associations with childhood maltreatment, depression and their cooccurrence, in females only
RE_model30_cooccur1_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model30_cooccur1_vs_cooccur0)
summary(RE_model30_cooccur1_vs_cooccur0)

RE_model30_cooccur2_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model30_cooccur2_vs_cooccur0)
summary(RE_model30_cooccur2_vs_cooccur0)

RE_model30_cooccur3_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = unlist(SE), data = Model30_cooccur3_vs_cooccur0)
summary(RE_model30_cooccur3_vs_cooccur0)

##### Model 31 - pooled analysis of triglycerides associations with childhood maltreatment, depression and their cooccurrence, in females only
RE_model31_cooccur1_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model31_cooccur1_vs_cooccur0)
summary(RE_model31_cooccur1_vs_cooccur0)

RE_model31_cooccur2_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model31_cooccur2_vs_cooccur0)
summary(RE_model31_cooccur2_vs_cooccur0)

RE_model31_cooccur3_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = unlist(SE), data = Model31_cooccur3_vs_cooccur0)
summary(RE_model31_cooccur3_vs_cooccur0)

##### Model 32 - pooled analysis of ldl/hdl ratio associations with childhood maltreatment, depression and their cooccurrence, in females only
RE_model32_cooccur1_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model32_cooccur1_vs_cooccur0)
summary(RE_model32_cooccur1_vs_cooccur0)

RE_model32_cooccur2_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = as.numeric(unlist(SE)), data = Model32_cooccur2_vs_cooccur0)
summary(RE_model32_cooccur2_vs_cooccur0)

RE_model32_cooccur3_vs_cooccur0 <- rma(yi = as.numeric(unlist(cohensd)), sei = unlist(SE), data = Model32_cooccur3_vs_cooccur0)
summary(RE_model32_cooccur3_vs_cooccur0)




# This the end of the code used to pool the data across cohorts
# For questions, please reach out to c.p.souama@amsterdamumc.nl

