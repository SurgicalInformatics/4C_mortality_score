# ISARIC WHO CCP-UK study: 4C Mortality Score
# Multiple imputation
# 03_mice.R
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2020

# 1. Missing data inspection, description and characterisation 
# 2. Basic MICE function

# --------------------------------------------------------------------

# Variables for mice (Multivariate Imputation by Chained Equations)
library(finalfit)
library(dplyr)
library(mice)
explanatory = c("age", 
                "sex", "ethnicity_4levels",
                
                "chrincard", "renal_mhyn", 
                "malignantneo_mhyn", "modliv", "obesity_mhyn", "chronicpul_mhyn",
                "diabetes_combined", 
                "no_comorbid",
                "no_comorbid.factor",
                
                "rr_vsorres", "oxy_vsorres", "sysbp_vsorres", "admission_diabp_vsorres", 
                "temp_vsorres", "hr_vsorres", "daily_gcs_vsorres",
                
                "daily_hb_lborres", "daily_wbc_lborres",
                "daily_neutro_lborres", "daily_lymp_lborres",
                "daily_plt_lborres", 
                "daily_sodium_lborres", "daily_potassium_lborres",
                "daily_bil_lborres", 
                "daily_alt_lborres",
                "daily_bun_lborres",  
                "daily_creat_lborres",
                "daily_crp_lborres")


# Missing data inspection
cs %>%
  missing_plot(explanatory)

# Missing data patterns
cs %>% 
  missing_pattern(explanatory)

## Explore subset with: 
cs %>% 
  missing_pairs()

# Missing data tabulations
cs %>% 
  missing_compare(explanatory) 

# Basic MICE function --------------------------------------------------------------

## Predictor matrix
cs_train %>% 
  select(time, status, death, explanatory) %>% 
  missing_predictorMatrix(
    drop_from_imputed = c("time", "status", "death"),
    drop_from_imputer = c("time", "status")
  ) -> predM

# Select included variables
data_mice_train = cs_train %>% 
  select(time, status, death, explanatory)

# Run for 10 imputed sets with 10 iterations
# Run in parallel over 5 cores. 
sets_train =  
  parlmice(data_mice_train, m = 10, predictorMatrix = predM, maxit = 10, 
           n.core = 2, n.imp.core = 5)

# Ensure convergence 
plot(sets_train)

# Run other diagnostics from here: 
# https://www.gerkovink.com/miceVignettes/

# Repeat above for validation datasets  

