# ISARIC WHO CCP-UK study: 4C Mortality Score
# 4c mortality score function
# 06_4c_mortality_score.R
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2020

# 1. 4C mortality score function
# 2. Prognostic index discrimination using mice data
# 3. Score distribution
# 4. Calibration

# 4C mortality score function -------------------------------------------------------------------
isaric_lasso <- function(.data, age, sex, comorbid, rr, spo2, gcs, crp, bun,
                         output = c("vector", "components", "df_vector", "df_components"),
                         na_to_zeros = TRUE, all_na_rm = TRUE){
  .age = enquo(age)
  .sex = enquo(sex)
  .comorbid = enquo(comorbid)
  .rr = enquo(rr)
  .spo2 = enquo(spo2)
  .gcs = enquo(gcs)
  .bun = enquo(bun)
  .crp = enquo(crp)
  
  out = .data %>%
    dplyr::mutate_at(vars(!! .age, !! .comorbid, !! .rr, !! .spo2, !! .gcs, !! .crp, !! .bun
    ), as.numeric) %>%
    dplyr::mutate_at(vars(!! .sex), ~ as.character(.) %>% tolower() %>% trimws()) %>%
    
    mutate(
      isaric_lasso_age = case_when(
        !! .age < 50 ~ 0,
        !! .age < 60 ~ 2,
        !! .age < 70 ~ 4,
        !! .age < 80 ~ 6,
        !! .age >= 80 ~ 7,
        TRUE ~ NA_real_),
      
      isaric_lasso_sex = case_when(
        !! .sex == "male" ~ 1, # Changed from 0
        !! .sex == "female" ~ 0,
        TRUE ~ NA_real_),
      
      isaric_lasso_comorbid = case_when(
        !! .comorbid == 0 ~ 0,
        !! .comorbid == 1 ~ 1,
        !! .comorbid >= 2 ~ 2,
        TRUE ~ NA_real_),
      
      isaric_lasso_rr = case_when(
        !! .rr < 20 ~ 0,
        !! .rr < 30 ~ 1,
        !! .rr >= 30 ~ 2,
        TRUE ~ NA_real_),
      
      isaric_lasso_spo2 = case_when(
        !! .spo2 < 92 ~ 2,
        !! .spo2 >= 92 ~ 0,
        TRUE ~ NA_real_),
      
      isaric_lasso_gcs = case_when(
        !! .gcs <  15 ~ 2,
        !! .gcs == 15 ~ 0,
        TRUE ~ NA_real_),
      
      isaric_lasso_bun = case_when(
        !! .bun <= 7 ~ 0,
        !! .bun <= 14 ~ 1,
        !! .bun >  14 ~ 3,
        TRUE ~ NA_real_),
      
      isaric_lasso_crp = case_when(
        !! .crp < 50 ~ 0,
        !! .crp < 100 ~ 1,
        !! .crp >=100 ~ 2,
        TRUE ~ NA_real_)
    ) %>%
    mutate(
      isaric_lasso = rowSums(dplyr::select(., dplyr::starts_with("isaric_lasso_")),
                             na.rm = na_to_zeros)
    ) %>%
    
    {if(all_na_rm){
      dplyr::mutate(., isaric_lasso = dplyr::if_else(
        is.na(isaric_lasso_age) &
          is.na(isaric_lasso_sex) &
          is.na(isaric_lasso_comorbid) &
          is.na(isaric_lasso_rr) &
          is.na(isaric_lasso_spo2) &
          is.na(isaric_lasso_gcs) &
          is.na(isaric_lasso_bun) &
          is.na(isaric_lasso_crp), NA_real_, isaric_lasso))
    } else {
      .
    }}
  
  if(output == "vector"){
    out %>%
      pull(isaric_lasso)
  } else if(output == "components"){
    out %>%
      select(starts_with("isaric_lasso"))
  } else if(output == "df_vector"){
    out %>%
      pull(isaric_lasso) %>%
      bind_cols(.data, isaric_lasso = .)
  } else if(output == "df_components"){
    out
  }
}

# Prognostic index discrimination in multiply imputed (mice) derivation data ----------------------------------------
na_decision = FALSE   # Change to TRUE to impute 0 for NA in scoring models

sets_train %>% 
  complete("all") %>% 
  map(~ isaric_lasso(., age = age, sex = sex,  comorbid = no_comorbid, rr = rr_vsorres,
                     spo2 = oxy_vsorres, gcs = daily_gcs_vsorres, 
                     bun = daily_bun_lborres, crp = daily_crp_lborres,
                     output = c("df_vector"), na_to_zeros = na_decision)
  ) %>% 
  map(~ roc(.$status, .$isaric_lasso) %>% 
        pROC::ci(method = "delong") # or bootstrap
  ) %>% 
  enframe() %>% 
  unnest_wider(value) %>% 
  # summarise_all(mean)
  
  # Prognostic index discrimination in multiply imputed (mice) validation data ----------------------------------------
sets_test %>% 
  complete("all") %>% 
  map(~ isaric_lasso(., age = age, sex = sex, comorbid = no_comorbid, rr = rr_vsorres,
                     spo2 = oxy_vsorres, gcs = daily_gcs_vsorres, 
                     bun = daily_bun_lborres, crp = daily_crp_lborres,
                     output = c("df_vector"), na_to_zeros = na_decision)
  ) %>% 
  map(~ roc(.$status, .$isaric_lasso) %>% 
        pROC::ci(method = "delong")
  ) %>% 
  enframe() %>% 
  unnest_wider(value) %>% 
  summarise_all(mean)

# Score distribution in each single imputed dataset ---------------------------------------------------
sets_test %>% 
  mice::complete(1) %>% 
  isaric_lasso(age = age, sex = sex, comorbid = no_comorbid, rr = rr_vsorres,
               spo2 = oxy_vsorres, gcs = daily_gcs_vsorres, 
               bun = daily_bun_lborres, crp = daily_crp_lborres,
               output = c("df_vector"), na_to_zeros = na_decision) %>% 
  ggplot_lancet(aes(x = isaric_lasso)) +
  geom_histogram(color="white", fill="#AD002AFF", binwidth = 1) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  xlab("Score value") +
  ylab("Patient count")

# Risk groups summary table from multiply imputed (mice) validation data------------------------------
sets_test %>%
  mice::complete("all") %>% 
  map(~ isaric_lasso(., age = age, sex = sex, comorbid = no_comorbid, rr = rr_vsorres,
                     spo2 = oxy_vsorres, gcs = daily_gcs_vsorres, 
                     bun = daily_bun_lborres, crp = daily_crp_lborres,
                     output = c("df_vector"), na_to_zeros = na_decision) %>% 
        mutate(
          isaric_lasso = case_when(
            isaric_lasso <= 3 ~ "0-3",
            isaric_lasso <= 8 ~ "4-8",
            isaric_lasso <= 14 ~ "9-14",
            is.na(isaric_lasso) ~ NA_character_,
            TRUE ~ "15+") %>% 
            factor(levels = c("0-3", "4-8", "9-14", "15+"))
        ) %>% 
        group_by(isaric_lasso) %>% 
        count(status) %>% 
        mutate(nn = sum(n)) %>% 
        filter(status == 1) %>% 
        drop_na() %>% 
        ungroup() %>% 
        mutate(
          total_n = sum(n),
          total_nn = sum(nn))) %>% 
  bind_rows() %>% 
  group_by(isaric_lasso) %>% 
  summarise_all(mean) %>% 
  group_by(isaric_lasso) %>% 
  summarise_all(ceiling) %>% 
  mutate(
    n_out = paste0(nn, " (", (100*nn/total_nn) %>% round_tidy(1), ")"),
    m_out = paste0(n, " (", (100*n/nn) %>% round_tidy(1), ")")
  )
# # A tibble: 4 x 8
# isaric_lasso status     n    nn total_n total_nn n_out        m_out      
# <fct>         <dbl> <int> <int>   <int>    <int> <chr>        <chr>      
# 1 0-3               1    20  1650    6729    22361 1650 (7.4)   20 (1.2)   
# 2 4-8               1   486  4889    6729    22361 4889 (21.9)  486 (9.9)  
# 3 9-14              1  3666 11664    6729    22361 11664 (52.2) 3666 (31.4)
# 4 15+               1  2557  4158    6729    22361 4158 (18.6)  2557 (61.5)

# Probability of death by score averaged across imputed sets -------------------------------------------------
sets_test %>%
  mice::complete("all") %>%
  map(~ bind_cols(.x, subjid = cs_test$subjid)) %>% 
  map(~ isaric_lasso(., age = age, sex = sex, comorbid = no_comorbid, rr = rr_vsorres,
                     spo2 = oxy_vsorres, gcs = daily_gcs_vsorres, 
                     bun = daily_bun_lborres, crp = daily_crp_lborres,
                     output = c("df_vector"), na_to_zeros = na_decision) %>% 
        select(subjid, status, isaric_lasso)) %>% 
  bind_rows() %>% 
  group_by(subjid) %>% 
  summarise_all(mean) %>% 
  mutate(isaric_lasso = isaric_lasso %>% round(0)) %>% 
  group_by(isaric_lasso) %>% 
  count(status) %>% 
  mutate(nn = sum(n),
         mort_perc = 100*n/nn) %>% 
  filter(status == 1)

# x          y
# 1   1  0.2976190
# 2   2  0.8064516
# 3   3  2.3112481
# 4   4  4.8051948
# 5   5  7.4742268
# 6   6  7.7837838
# 7   7 11.6953762
# 8   8 14.4486692
# 9   9 19.1646192
# 10 10 22.8999465
# 11 11 26.9156159
# 12 12 32.9079307
# 13 13 40.1318888
# 14 14 44.5749441
# 15 15 51.6393443
# 16 16 59.1031390
# 17 17 66.1224490
# 18 18 75.8099352
# 19 19 77.3913043
# 20 20 82.8828829
# 21 21 87.5000000

# Calibration (two methods detailed below) --------------------------------------------------------
## Mean 4C score across derivation multiply imputed sets. 
cal_train = sets_train %>%
  mice::complete("all") %>%
  map(~ bind_cols(.x, subjid = cs_train$subjid)) %>% 
  map(~ isaric_lasso(., age = age, sex = sex, comorbid = no_comorbid, rr = rr_vsorres,
                     spo2 = oxy_vsorres, gcs = daily_gcs_vsorres, 
                     bun = daily_bun_lborres, crp = daily_crp_lborres,
                     output = c("df_vector"), na_to_zeros = na_decision) %>% 
        
        select(subjid, status, isaric_lasso)) %>% 
  bind_rows() %>% 
  group_by(subjid) %>% 
  summarise_all(mean) %>% 
  mutate(isaric_lasso = isaric_lasso %>% round(0))

# Mean 4C score across validation multiply imputed sets.
cal_test = sets_test %>%
  mice::complete("all") %>%
  map(~ bind_cols(.x, subjid = cs_test$subjid)) %>% 
  map(~ isaric_lasso(., age = age, sex = sex, comorbid = no_comorbid, rr = rr_vsorres,
                     spo2 = oxy_vsorres, gcs = daily_gcs_vsorres, 
                     bun = daily_bun_lborres, crp = daily_crp_lborres,
                     output = c("df_vector"), na_to_zeros = na_decision) %>% 
        
        select(subjid, status, isaric_lasso)) %>% 
  bind_rows() %>% 
  group_by(subjid) %>% 
  summarise_all(mean) %>% 
  mutate(isaric_lasso = isaric_lasso %>% round(0))

# Derivation model
m1 <- glm(status ~ isaric_lasso, data = cal_train, family = binomial)

# Predicted probabilities in validation data
cal_test$m1_pred <- predict(m1, newdata = cal_test, type = "response")

## Calibration plot function (deciles) ------------------------------------------
cal_plot_data <- function(data, pred_var, ...){
  
  # Calibration plot
  mutate(data, bin = ntile(get(pred_var), 10)) %>%
    # Bin prediction into deciles
    group_by(bin) %>%
    mutate(n = n(), # Get ests and CIs
           bin_pred = mean(get(pred_var)),
           bin_prob = mean(status),
           se = sqrt((bin_prob * (1 - bin_prob)) / n), # Simple CIs for this
           ul = bin_prob + 1.96 * se,
           ll = bin_prob - 1.96 * se) %>%
    ungroup()
}

## Plot
cal_plot_data(cal_test, "m1_pred") %>% 
  ggplot(aes(x = bin_pred, y = bin_prob, ymin = ll, ymax = ul)) +
  geom_pointrange(size = 0.3, color = "#AD002AFF") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  coord_cartesian(expand = FALSE, ylim=c(0,1)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  geom_abline(lty = 2) + # 45 degree line indicating perfect calibration
  geom_line(col = "#AD002AFF") +
  xlab("Predicted probability") +
  ylab("Observed probability") +
  theme_classic()

## Calibration in the large ---------------------------------------------------------
linear_pred_m1 = predict(m1, newdata = cal_test)
fit_linear_pred_m1 = glm(status ~ 1, offset = linear_pred_m1, data=cal_test, family="binomial")

## Calibration slope -----------------------------------------------------------------
glm(status ~ linear_pred_m1, data=cal_test, family="binomial")

## Brier -----------------------------------------------------------------------------
rms::val.prob(cal_test$m1_pred, cal_test$status, 
              pl = FALSE) %>% round(3)

# ---------------------------------------------------------------------------------------

# Alternative approach to calibration fitting each MI derivation dataset separately, predicting in validation set------------------
# Then fitting a loess curve through predictions
# Note, this approach is poorly supported by current packages, and the loess fitting is computationally expensive in large datasets
# e.g. Warning message:
# Computation failed in `stat_smooth()`:
#   workspace required (75004328395) is too large probably because of setting 'se = TRUE'. 

# Fit model in 10 imputed derivation sets and pool
fit_train = sets_train %>% 
  complete("all") %>% 
  map(~ isaric_lasso(., age = age, sex = sex,  comorbid = no_comorbid, rr = rr_vsorres,
                     spo2 = oxy_vsorres, gcs = daily_gcs_vsorres, 
                     bun = daily_bun_lborres, crp = daily_crp_lborres,
                     output = c("df_vector"), na_to_zeros = na_decision) %>% 
        glm(status ~ isaric_lasso, data = ., family = binomial)
  ) %>% 
  mice::pool()

# No predcit functoin for pooled mice (mipo), therefore do by hand. 

# Design matrix for 10 imputed validation sets
X = sets_test %>% 
  mice::complete("all") %>%
  map(~  isaric_lasso(., age = age, sex = sex,  comorbid = no_comorbid, rr = rr_vsorres,
                      spo2 = oxy_vsorres, gcs = daily_gcs_vsorres, 
                      bun = daily_bun_lborres, crp = daily_crp_lborres,
                      output = c("df_vector"), na_to_zeros = na_decision) %>% 
        mutate(intercept = 1) %>% 
        select(intercept, isaric_lasso) %>% 
        as.matrix()
  )

# Pooled coefficients
beta = fit_train$pooled %>% 
  select(estimate) %>% 
  as.matrix()

plot_data = X %>% 
  map_dfc(function(x){
    x %*% beta %>% 
      boot::inv.logit() %>% 
      tibble(pred = .)
  }
  ) %>% 
  mutate(
    y = sets_test %>% 
      mice::complete(1) %>% 
      pull(status)
  ) %>% 
  gather("key", "value", -y) 

plot_data %>% 
  ggplot_lancet(aes(x = value, y = y)) +
  geom_smooth(method = "loess", se = FALSE) + # Comment in the next line for faster method
  # geom_smooth(method="loess", se=F,
  #             method.args=loess.control(statistics = "approximate", trace.hat = "approximate")) +
  geom_abline(lty = 2) +
  labs(x = "Predicted", y = "Observed") +
  ggtitle("4C Mortality Score calibration curve",
          "Score fitted in multiply imputed derviation data, pooled, predictions from 10 validation sets and loess")


