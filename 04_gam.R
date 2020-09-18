# ISARIC WHO CCP-UK study: 4C Mortality Score
# Generalised additive models
# 04_gam.R
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2020

# 1. GAM with complete data
# 2. GAM with mice data
# 3. purrr methods for metrics, combining by Rubin's rules. 

# Packages -----------------------------------------------
library(mgcv)
library(purrr)
library(pROC)
library(yardstick)

# GAM using complete dataset-------------------------------

## Indicative regression for complete data
gam_complete = cs_train %>% 
  select(death, sex, no_comorbid, 
         age, rr_vsorres, oxy_vsorres, daily_gcs_vsorres,
         daily_bun_lborres, daily_crp_lborres) %>% 
  drop_na() %>% 
  gam(death ~ 
        sex + 
        ethnicity_4levels + 
        s(age) +
        s(no_comorbid) + 
        s(rr_vsorres, k = 4) + 
        s(oxy_vsorres, k = 3) + 
        s(sysbp_vsorres) + 
        s(admission_diabp_vsorres) + 
        s(temp_vsorres) + 
        s(hr_vsorres) + 
        s(daily_gcs_vsorres, k = 4) + 
        s(daily_hb_lborres) + 
        s(daily_wbc_lborres) + 
        s(daily_neutro_lborres) + 
        s(daily_lymp_lborres) + 
        s(daily_plt_lborres) + 
        s(daily_sodium_lborres) + 
        s(daily_potassium_lborres) + 
        s(daily_bil_lborres) +  
        s(daily_creat_lborres) + 
        s(daily_bun_lborres) +
        s(daily_crp_lborres),
      data = ., family = binomial)
summary(gam_complete)
plot(gam_complete)

# AUROC
## Derivation
roc(cs_train$death, gam_complete %>% predict(cs_train)) %>% 
  pROC::ci(method = "bootstrap") %>%
  round(3)

## Validation
roc(cs_test$death, gam_complete %>% predict(cs_test)) %>% 
  pROC::ci(method = "bootstrap") %>%
  round(3)

# GAM with mice data ------------------------------------------------
# mids object from mice: `sets_train` and `sets_test`

## Indicative regression methods for mice
gam_mice = sets_train %>%
  with(
    gam(death ~ 
          sex + 
          ethnicity_4levels + 
          s(age) +
          s(no_comorbid) + 
          s(rr_vsorres, k = 4) + 
          s(oxy_vsorres, k = 3) + 
          s(sysbp_vsorres) + 
          s(admission_diabp_vsorres) + 
          s(temp_vsorres) + 
          s(hr_vsorres) + 
          s(daily_gcs_vsorres, k = 4) + 
          s(daily_hb_lborres) + 
          s(daily_wbc_lborres) + 
          s(daily_neutro_lborres) + 
          s(daily_lymp_lborres) + 
          s(daily_plt_lborres) + 
          s(daily_sodium_lborres) + 
          s(daily_potassium_lborres) + 
          s(daily_bil_lborres) +  
          s(daily_creat_lborres) + 
          s(daily_bun_lborres) +
          s(daily_crp_lborres),
        family = binomial)
  )

# Extract metrics and mean as per Rubin's rules
## Note purrr methods to combine 10 models. 
gam_mice %>%
  magrittr::extract2(4) %>% 
  map_df(function(.x){
    n = summary(.x) %$% n
    r.sq = summary(.x) %$% r.sq
    dev.expl = summary(.x) %$% dev.expl
    ubre = summary(.x) %$% sp.criterion
    tibble(n, r.sq, dev.expl, ubre)
  }) %>% 
  summarise_all(mean)


# AUROC in imputed derviation data
map2(.x = gam_mice %>% 
       magrittr::extract2(4),
     .y = sets_train %>% 
       mice::complete("all"), 
     ~ roc(.y$status, .x %>% predict(.y)) %>% 
       pROC::ci(method = "bootstrap") # Computationally intensive, use method = "delong" for testing
) %>% 
  enframe() %>% 
  unnest_wider(value) %>% 
  summarise_all(mean)

# AUROC in imputed validation data
map2(.x = gam_mice %>% 
       magrittr::extract2(4),
     .y = sets_test %>% 
       mice::complete("all"), 
     ~ roc(.y$status, .x %>% predict(.y)) %>% 
       pROC::ci(method = "delong")
) %>% 
  enframe() %>% 
  unnest_wider(value) %>% 
  summarise_all(mean)

# AUROC in complete derivation data
map(gam_mice %>% 
      magrittr::extract2(4),
    function(.x){
      roc_auc_vec(
        truth = cs_train$death,
        estimate = .x %>% predict(cs_train),
        event_level = "second")
    }
)

# AUROC in complete validation data
map(gam_mice %>% 
      magrittr::extract2(4),
    function(.x){
      roc_auc_vec(
        truth = cs_test$death,
        estimate = .x %>% predict(cs_test),
        event_level = "second")
    }
)
