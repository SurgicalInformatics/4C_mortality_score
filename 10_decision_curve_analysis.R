# ISARIC WHO CCP-UK study: 4C Mortality Score
# Decision curve analysis
# 10_decision_curve_analysis.R
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2020

# 1. Apply comparison scores in derivation and validation data. 
# 2. DCA: fit in derivation and predict in validation (recalibration of comparators). 
# 3. DCA: fit and predict in validation data (recalibration of all).

# Packages ----------------------------------------------------------------------
library(tidyverse)
library(rmda)

# Apply scores to derivation data ----------------------------------------------
dca_train = sets_train %>%
  mice::complete("all") %>%
  map(~ bind_cols(.x, subjid = cs_train$subjid)) %>% 
  map(~ isaric_lasso(., age = age, sex = sex, comorbid = no_comorbid, rr = rr_vsorres,
                     spo2 = oxy_vsorres, gcs = daily_gcs_vsorres, 
                     bun = daily_bun_lborres, crp = daily_crp_lborres,
                     output = c("df_vector"), na_to_zeros = na_decision) %>% 
        
        adrop(spo2 = oxy_vsorres, sbp = sysbp_vsorres, 
              gcs = daily_gcs_vsorres, age = age,
              bun = daily_bun_lborres, sex = sex,
              output = "df_vector",
              na_to_zeros = na_decision) %>% 
        
        curb65(rr = rr_vsorres, sbp = sysbp_vsorres, dbp = admission_diabp_vsorres, 
               gcs = daily_gcs_vsorres, bun = daily_bun_lborres, age = age,
               na_to_zeros = na_decision, output = c("df_vector")) %>%
        
        select(subjid, status, age, isaric_lasso, adrop, curb65)
      
      ) %>%  
  bind_rows() %>% 
  group_by(subjid) %>% 
  summarise_all(mean) %>% 
  mutate(isaric_lasso = isaric_lasso %>% round(0),
         adrop = adrop %>% round(0),
         curb65 = curb65 %>% round(0))

# Apply scores to validation data -----------------------------------------------
dca_train = sets_test %>%
  mice::complete("all") %>%
  map(~ bind_cols(.x, subjid = cs_train$subjid)) %>% 
  map(~ isaric_lasso(., age = age, sex = sex, comorbid = no_comorbid, rr = rr_vsorres,
                     spo2 = oxy_vsorres, gcs = daily_gcs_vsorres, 
                     bun = daily_bun_lborres, crp = daily_crp_lborres,
                     output = c("df_vector"), na_to_zeros = na_decision) %>% 
        
        adrop(spo2 = oxy_vsorres, sbp = sysbp_vsorres, 
              gcs = daily_gcs_vsorres, age = age,
              bun = daily_bun_lborres, sex = sex,
              output = "df_vector",
              na_to_zeros = na_decision) %>% 
        
        curb65(rr = rr_vsorres, sbp = sysbp_vsorres, dbp = admission_diabp_vsorres, 
               gcs = daily_gcs_vsorres, bun = daily_bun_lborres, age = age,
               na_to_zeros = na_decision, output = c("df_vector")) %>%
        
        select(subjid, status, age, isaric_lasso, adrop, curb65)
      
  ) %>%  
  bind_rows() %>% 
  group_by(subjid) %>% 
  summarise_all(mean) %>% 
  mutate(isaric_lasso = isaric_lasso %>% round(0),
         adrop = adrop %>% round(0),
         curb65 = curb65 %>% round(0))

# Fit in derivation and predict in validation ---------------------------------
fit_isaric = glm(status ~ isaric_lasso, data = dca_train, family = binomial)
dca_test$p_isaric = predict(fit_isaric, newdata = dca_test, type = "response")

fit_adrop = glm(status ~ adrop_lasso, data = dca_train, family = binomial)
dca_test$p_adrop = predict(fit_adrop, newdata = dca_test, type = "response")

fit_curb65 = glm(status ~ curb65_lasso, data = dca_train, family = binomial)
dca_test$p_curb65 = predict(fit_curb65, newdata = dca_test, type = "response")

# DCA
dca_isaric = decision_curve(status ~ p_isaric, data = dca_test, fitted.risk = TRUE)
dca_adrop = decision_curve(status ~ p_adrop, data = dca_test, fitted.risk = TRUE)
dca_curb65 = decision_curve(status ~ p_curb65, data = dca_test, fitted.risk = TRUE)
dca_age = decision_curve(status ~ splines::bs(age, df = 5), data = dca_test)

# Plot
palette_dca = c("#AD002AFF", "#00468BFF", "#42B540FF", "#0099B4FF", "grey", "black")

plot_decision_curve(list(dca_isaric, dca_adrop, dca_curb65, dca_age), cost.benefit.axis = FALSE,
                    confidence.intervals = FALSE,
                    curve.names = c("4C", "A-DROP", "CURB65", "Age"),
                    col = palette_dca)


# Alternative fit and predict in validation data (recalibration) ---------------------------------
fit_isaric = glm(status ~ isaric_lasso, data = dca_train, family = binomial)
dca_test$p_isaric = predict(fit_isaric, newdata = dca_test, type = "response")

fit_adrop = glm(status ~ adrop_lasso, data = dca_train, family = binomial)
dca_test$p_adrop = predict(fit_adrop, newdata = dca_test, type = "response")

fit_curb65 = glm(status ~ curb65_lasso, data = dca_train, family = binomial)
dca_test$p_curb65 = predict(fit_curb65, newdata = dca_test, type = "response")

# DCA
dca_isaric = decision_curve(status ~ isaric_lasso, data = dca_test)
dca_adrop = decision_curve(status ~ adrop, data = dca_test)
dca_curb65 = decision_curve(status ~ curb65, data = dca_test)
dca_age = decision_curve(status ~ splines::bs(age, df = 5), data = dca_test)

# Plot
palette_dca = c("#AD002AFF", "#00468BFF", "#42B540FF", "#0099B4FF", "grey", "black")

plot_decision_curve(list(dca_isaric, dca_adrop, dca_curb65, dca_age), cost.benefit.axis = FALSE,
                    confidence.intervals = FALSE,
                    curve.names = c("4C", "A-DROP", "CURB65", "Age"),
                    col = palette_dca)
