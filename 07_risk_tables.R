# ISARIC WHO CCP-UK study: 4C Mortality Score
# Risk tables
# 07_risk_tables.R
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2020

# 1. Prognos function.
# 2. Risk tables generation at specified cut-offs

# Prognos function for automatic table generation ---------------------------------------
prognos <- function(.data, estimate, truth){
  .estimate = sym(estimate)
  .truth = sym(truth) 
  .data %>% 
    drop_na(!! .estimate, !! .truth) %>% 
    summarise(
      N = n(),
      TP = sum(!! .truth == 1 & !! .estimate == 1),
      TN = sum(!! .truth == 0  & !! .estimate == 0),
      FP = sum(!! .truth == 0  & !! .estimate == 1),
      FN = sum(!! .truth == 1  & !! .estimate == 0),
      Sens = sens(., !! .truth, !! .estimate)$.estimate,
      Spec = spec(., !! .truth, !! .estimate)$.estimate,
      PPV = ppv(., !! .truth, !! .estimate)$.estimate,
      NPV = npv(., !! .truth, !! .estimate)$.estimate,
      above = paste0(TP + FP, " (", ((100*(TP + FP))/N) %>% round_tidy(1), ")"),
      below = paste0(TN + FN, " (", ((100*(TN + FN))/N) %>% round_tidy(1), ")")
    ) %>% 
    mutate_at(vars("Sens", "Spec", "PPV", "NPV"), ~ prod(., 100)) %>% 
    mutate(across(c(everything(), -above, -below), round, digits = 1))
}


# Rule out table ---------------------------------------------------------
## These tables could be combined
## Set cut-offs

tbl_ruleout = sets_test %>%
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
  mutate(
    cutoff2 = case_when(
      isaric_lasso <= 2 ~ 0,
      isaric_lasso > 2 ~ 1,
      TRUE ~ NA_real_) %>%
      factor(),
    cutoff3 = case_when(
      isaric_lasso <= 3 ~ 0,
      isaric_lasso > 3 ~ 1,
      TRUE ~ NA_real_) %>%
      factor(),
    cutoff4 = case_when(
      isaric_lasso <= 4 ~ 0,
      isaric_lasso > 4 ~ 1,
      TRUE ~ NA_real_) %>%
      factor(),
    cutoff5 = case_when(
      isaric_lasso <= 5 ~ 0,
      isaric_lasso > 5 ~ 1,
      TRUE ~ NA_real_) %>%
      factor(),
    cutoff6 = case_when(
      isaric_lasso <= 6 ~ 0,
      isaric_lasso > 6 ~ 1,
      TRUE ~ NA_real_) %>%
      factor(),
    cutoff7 = case_when(
      isaric_lasso <= 7 ~ 0,
      isaric_lasso > 7 ~ 1,
      TRUE ~ NA_real_) %>%
      factor(),
    cutoff8 = case_when(
      isaric_lasso <= 8 ~ 0,
      isaric_lasso > 8 ~ 1,
      TRUE ~ NA_real_) %>%
      factor(),
    cutoff9 = case_when(
      isaric_lasso <= 9 ~ 0,
      isaric_lasso > 9 ~ 1,
      TRUE ~ NA_real_) %>%
      factor(),     
    cutoff10 = case_when(
      isaric_lasso <= 10 ~ 0,
      isaric_lasso >10 ~ 1,
      TRUE ~ NA_real_) %>%
      factor()
  ) %>% 
  select(status, dplyr::starts_with("cutoff")) %>% 
  mutate(status  = factor(status))

map_df(list(
  "<=2" = "cutoff2",
  "<=3" = "cutoff3",
  "<=4" = "cutoff4",
  "<=5" = "cutoff5",
  "<=6" = "cutoff6",
  "<=7" = "cutoff7",
  "<=8" = "cutoff8",
  "<=9" = "cutoff9",
  "<=10" = "cutoff10"
),
~ prognos(tbl_ruleout, .x, "status"), .id= "4C score") %>%
  mytable(caption = "Sensitivity, specificity, postitive predicive value and negative predictive value for Isaric score cut-offs for mortality")


# Rule in table ---------------------------------------------------------
## Set cut-offs

tbl_rulein = sets_test %>%
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
  mutate(
    cutoff9 = case_when(
      isaric_lasso >= 9 ~ 1,
      isaric_lasso < 9 ~ 0,
      TRUE ~ NA_real_) %>%
      factor(),
    cutoff11 = case_when(
      isaric_lasso >= 11 ~ 1,
      isaric_lasso < 11 ~ 0,
      TRUE ~ NA_real_) %>%
      factor(),
    cutoff13 = case_when(
      isaric_lasso >= 13 ~ 1,
      isaric_lasso < 13 ~ 0,
      TRUE ~ NA_real_) %>%
      factor(),
    cutoff15 = case_when(
      isaric_lasso >= 15 ~ 1,
      isaric_lasso < 15 ~ 0,
      TRUE ~ NA_real_) %>%
      factor(),
    cutoff17 = case_when(
      isaric_lasso >= 17 ~ 1,
      isaric_lasso < 17 ~ 0,
      TRUE ~ NA_real_) %>%
      factor(),
    cutoff19 = case_when(
      isaric_lasso >= 19 ~ 1,
      isaric_lasso < 19 ~ 0,
      TRUE ~ NA_real_) %>%
      factor(),     
    cutoff21 = case_when(
      isaric_lasso >= 21 ~ 1,
      isaric_lasso < 21 ~ 0,
      TRUE ~ NA_real_) %>%
      factor()
  ) %>% 
  select(status, dplyr::starts_with("cutoff")) %>% 
  mutate(status  = factor(status))


map_df(list(
  ">=9" = "cutoff9",
  ">=11" = "cutoff11",
  ">=13" = "cutoff13",
  ">=15" = "cutoff15",
  ">=17" = "cutoff17",
  ">=19" = "cutoff19",
  ">=21" = "cutoff21"
),
~ prognos(tbl_rulein, .x, "status"), .id= "4C score") %>%
  mytable(caption = "Sensitivity, specificity, postitive predicive value and negative predictive value for 4C score cut-offs for mortality")

# 4C score     N   TP   TN    FP   FN Sens Spec  PPV  NPV        above        below
# 1    <=2 22361 6724  996 14636    5  6.4 99.9 99.5 31.5 21360 (95.5)   1001 (4.5)
# 2    <=3 22361 6709 1630 14002   20 10.4 99.7 98.8 32.4 20711 (92.6)   1650 (7.4)
# 3    <=4 22361 6672 2363 13269   57 15.1 99.2 97.6 33.5 19941 (89.2)  2420 (10.8)
# 4    <=5 22361 6614 3081 12551  115 19.7 98.3 96.4 34.5 19165 (85.7)  3196 (14.3)
# 5    <=6 22361 6542 3934 11698  187 25.2 97.2 95.5 35.9 18240 (81.6)  4121 (18.4)
# 6    <=7 22361 6413 4908 10724  316 31.4 95.3 94.0 37.4 17137 (76.6)  5224 (23.4)
# 7    <=8 22361 6223 6033  9599  506 38.6 92.5 92.3 39.3 15822 (70.8)  6539 (29.2)
# 8    <=9 22361 5911 7349  8283  818 47.0 87.8 90.0 41.6 14194 (63.5)  8167 (36.5)
# 9   <=10 22361 5483 8790  6842 1246 56.2 81.5 87.6 44.5 12325 (55.1) 10036 (44.9)




















