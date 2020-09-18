# ISARIC WHO CCP-UK study: 4C Mortality Score
# XGBoost
# 09_xgboost.R
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2020

# 1. Variable definition
# 2. Derivation and validation matrices created
# 3. XGBoost training
# 4. Discrimination (AUROC)

# Packages ----------------------------------------------------------------------
library(plsmselect)
library(yardstick)
library(xgboost)
library(finalfit)
library(rsample)

# Define variable set -----------------------------------------------------------
explanatory =   c(
  "age", 
  "sex", 
  "ethnicity_4levels",
  
  "chrincard", 
  "renal_mhyn", 
  "malignantneo_mhyn", 
  "modliv", 
  "obesity_mhyn", 
  "chronicpul_mhyn",
  
  "diabetes_combined", 
  "no_comorbid.factor",
  
  "rr_vsorres", 
  "oxy_vsorres", 
  "sysbp_vsorres", 
  "admission_diabp_vsorres", 
  "temp_vsorres", 
  "hr_vsorres", 
  "daily_gcs_vsorres",

  "daily_hb_lborres",
  "daily_wbc_lborres",
  "daily_neutro_lborres", 
  "daily_lymp_lborres",
  "daily_plt_lborres", 
  "daily_sodium_lborres", 
  "daily_bil_lborres", 
  "daily_bun_lborres",  
  "daily_creat_lborres",
  "daily_crp_lborres")

# Define derivation and validation matrices -----------------------------------------------
data_xgboost_train_complete = cs_train %>%         
  drop_na(death) %>% 
  select(death, explanatory) %>% 
  recipe(death ~ .) %>% 
  step_normalize(all_numeric()) %>% 
  step_dummy(all_nominal(), -death) %>%
  prep() %>% 
  juice() %>% 
  mutate(y = death %>% as.numeric() %>% {. - 1})

data_xgboost_train_mice = sets_train %>%  
  mice::complete(1) %>% 
  drop_na(status) %>% 
  select(status, explanatory) %>% 
  mutate(status = factor(status)) %>% 
  recipe(status ~ .) %>% 
  step_normalize(all_numeric()) %>% 
  step_dummy(all_nominal(), -status) %>%
  prep() %>% 
  juice() %>% 
  mutate(y = status %>% as.numeric() %>% {. - 1})

data_xgboost_test_mice = sets_test %>%  
  mice::complete(1) %>% 
  drop_na(status) %>% 
  select(status, explanatory) %>% 
  mutate(status = factor(status)) %>% 
  recipe(status ~ .) %>% 
  step_normalize(all_numeric()) %>% 
  step_dummy(all_nominal(), -status) %>%
  prep() %>% 
  juice() %>% 
  mutate(y = status %>% as.numeric() %>% {. - 1})

data_xgboost_test_complete = cs_test %>%         
  drop_na(death) %>% 
  select(death, explanatory) %>% 
  recipe(death ~ .) %>% 
  step_normalize(all_numeric()) %>% 
  step_dummy(all_nominal(), -death) %>%
  prep() %>% 
  juice() %>% 
  mutate(y = death %>% as.numeric() %>% {. - 1})

# Indicative XGBoost run --------------------------------------------------------------

## Split test/training sets
set.seed(100)
train_test_split <- initial_split(data_xgboost_train_complete, prop = 0.8)
train_test_split

## Retrieve train and test sets
train_tbl <- training(train_test_split)
test_tbl  <- testing(train_test_split) 

## Train
xg_train_complete = train_tbl %>% 
  select(-death, -y, -starts_with("age.")) %>% 
  as.matrix() %>% 
  xgboost(label = train_tbl$y, 
          max_depth = 20, eta = 0.05, nthread = 24, nrounds = 1000, 
          eval_metric = "error", objective = "binary:logistic")

## AUC in test set  
roc_auc_vec(test_tbl$death, 
            predict(xg_train_complete, test_tbl %>% 
                      select(-death, -y, -starts_with("age.")) %>% 
                      as.matrix()))

roc(test_tbl$death, predict(xg_train_complete, test_tbl %>% 
                              select(-death, -y, -starts_with("age.")) %>% 
                              as.matrix())) %>% 
  pROC::ci(method = "bootstrap") %>% 
  round(3)
# 0.796 (0.786 – 0.807) 
# Reported as model performance in training set 

## AUC in validation
roc_auc_vec(data_xgboost_test_complete$death, 
            predict(xg_train_complete, data_xgboost_test_complete %>% 
                      select(-death, -y, -starts_with("age.")) %>% 
                      as.matrix()))
roc(data_xgboost_test_complete$death, predict(xg_train_complete, data_xgboost_test_complete %>% 
                                                select(-death, -y, -starts_with("age.")) %>% 
                                                as.matrix())) %>% 
  pROC::ci(method = "bootstrap") %>%
  round(3)
# 0.782 (0.776 – 0.788)
