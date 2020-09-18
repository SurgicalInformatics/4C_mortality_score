# ISARIC WHO CCP-UK study: 4C Mortality Score
# Score comparisons
# 08_score_comparisons.R
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2020

# 1. Generate risk scores
# 2. ff_aucroc function
# 3. Apply ff_auroc to risk scores

# Apply comparison scores to data ----------------------------------------------------------
## Refer to paper for score details
cs_test_scores = cs_test %>% 
  
  isaric_lasso(., age = age, sex = sex,  
               comorbid = no_comorbid, rr = rr_vsorres,
               spo2 = oxy_vsorres, gcs = daily_gcs_vsorres, 
               bun = daily_bun_lborres, crp = daily_crp_lborres,
               output = c("df_vector"), na_to_zeros = na_decision) %>% 
  
  news(rr = rr_vsorres, spo2 = oxy_vsorres, o2_rx = o2_rx,
       hypoxic_target = hypoxic_target, sbp = sysbp_vsorres, hr = hr_vsorres, 
       temp = temp_vsorres, alt_conscious = alt_conscious, 
       output = "df_vector",  na_to_zeros = na_decision) %>%
  
  sofa(po2 = daily_pao2_lborres, fio2 = daily_fio2_lborres, 
       invasive_vent = daily_invasive_prtrt, plts = daily_plt_lborres, 
       bil = daily_bil_lborres, gcs = daily_gcs_vsorres, creat = daily_creat_lborres, 
       map = daily_meanart_vsorres, dopgr15 = daily_dopgr15_cmtrt, 
       dop5to15 = daily_dop5to15_cmtrt, dopless5 = daily_dopless5_cmtrt, 
       output = c("df_vector"), na_to_zeros = na_decision) %>% 
  
  qsofa(rr = rr_vsorres, sbp = sysbp_vsorres, gcs = daily_gcs_vsorres,
        na_to_zeros = na_decision, output = c("df_vector")) %>% 
  
  crb65(rr = rr_vsorres, sbp = sysbp_vsorres, dbp = admission_diabp_vsorres, 
        gcs = daily_gcs_vsorres, age = age,
        na_to_zeros = na_decision, output = c("df_vector")) %>% 
  
  curb65(rr = rr_vsorres, sbp = sysbp_vsorres, dbp = admission_diabp_vsorres, 
         gcs = daily_gcs_vsorres, bun = daily_bun_lborres, age = age,
         na_to_zeros = na_decision, output = c("df_vector")) %>% 
  
  ecurb65(rr = rr_vsorres, sbp = sysbp_vsorres, dbp = admission_diabp_vsorres,
          gcs = daily_gcs_vsorres, bun = daily_bun_lborres, age = age,
          ldh = daily_ldh_lborres, plts = daily_plt_lborres, 
          #alb      # albumin not available in ISARIC
          na_to_zeros = na_decision, output = c("df_vector")) %>% 
  
  dscrb65(rr = rr_vsorres,  sbp = sysbp_vsorres, dbp = admission_diabp_vsorres, 
          gcs = daily_gcs_vsorres, age = age,
          spo2 = oxy_vsorres, card_comrb = chrincard, renal_comrb = renal_mhyn, 
          cancer_comrb = malignantneo_mhyn, liver_comrb = modliv,
          output = "df_vector",
          na_to_zeros = na_decision) %>% 
  
  scap(rr = rr_vsorres,  sbp = sysbp_vsorres, 
       gcs = daily_gcs_vsorres, age = age,
       po2 = daily_pao2_lborres, fio2 = daily_fio2_lborres, 
       bun = daily_bun_lborres, ph = daily_ph_lborres, infiltrates = infiltrates_faorres,
       output = c("df_vector"),
       na_to_zeros = na_decision) %>% 
  
  tongji(age = age, ldh = daily_ldh_lborres, lymph = daily_lymp_lborres, spo2 = oxy_vsorres, 
         output = c("df_vector")) %>% 
  
  adrop(spo2 = oxy_vsorres, sbp = sysbp_vsorres, 
        gcs = daily_gcs_vsorres, age = age,
        bun = daily_bun_lborres, sex = sex,
        output = "df_vector",
        na_to_zeros = na_decision) %>% 
  
  flucp(rr = rr_vsorres, sbp = sysbp_vsorres, gcs = daily_gcs_vsorres, spo2 = oxy_vsorres, 
        infiltrates = infiltrates_faorres, 
        output = "df_vector", 
        na_to_zeros = na_decision, all_na_rm = TRUE) %>% 
  
  psi(rr = rr_vsorres, sbp = sysbp_vsorres, gcs = daily_gcs_vsorres, temp = temp_vsorres, 
      hr= hr_vsorres, age = age, sex = sex, 
      card_comrb = chrincard, renal_comrb = renal_mhyn, cancer_comrb = malignantneo_mhyn,
      liver_comrb = modliv, ph = daily_ph_lborres, po2 = daily_pao2_lborres, 
      spo2 = oxy_vsorres, bun = daily_bun_lborres,na = daily_sodium_lborres,
      glu = daily_glucose_lborres, haematocrit = daily_haematocrit_lborres, 
      infiltrates = infiltrates_faorres,
      output = c("df_vector"), na_to_zeros = na_decision) %>% 
  
  smartcop(age = age, rr = rr_vsorres, hr= hr_vsorres, gcs = daily_gcs_vsorres, 
           po2 = daily_pao2_lborres, spo2 = oxy_vsorres, fio2 = daily_fio2_lborres, 
           ph = daily_ph_lborres, sbp = sysbp_vsorres, infiltrates = infiltrates_faorres, 
           #alb,   # albumin not available in ISARIC
           output = c("df_vector"), na_to_zeros = na_decision) %>% 
  
  DLscore(age = age, sex = sex, neutro = daily_neutro_lborres, lymph = daily_lymp_lborres, 
          plts = daily_plt_lborres, crp = daily_crp_lborres, creat = daily_creat_lborres,
          output = c("df_vector")) %>% 
  
  surgisphere(comorbidSS = comorbid_SS, #mobility = clinical_frailty, 
              rr = rr_vsorres, spo2 = oxy_vsorres, dyspnoe = dyspnoe, gcs = daily_gcs_vsorres,
              temp = temp_vsorres, hr = hr_vsorres, sbp = sysbp_vsorres,
              output = c("df_vector"), na_to_zeros = na_decision) %>% 
  
  xie_score(age = age, ldh = daily_ldh_lborres, lymph = daily_lymp_lborres, sats = oxy_vsorres,
            output = c("df_vector")) %>% 
  
  covid_gram(age = age, gcs = daily_gcs_vsorres, comorbid_gram = comorbid_gram, nlr = NLR,
             ldh = daily_ldh_lborres, bil = daily_bil_lborres, infiltrates = infiltrates_faorres,
             haemoptysis = coughhb_ceoccur_v2, dyspnoea = shortbreath_ceoccur_v2,
             malignancy = malignantneo_mhyn, output = c("df_vector"))





# ff_aucroc function to generate AUCROCs + counts across multiple scores -------------------------- 

ff_aucroc <- function(.data, estimate, truth){
  .estimate = sym(estimate)
  .truth = sym(truth) 
  .data %>% 
    drop_na(!! .estimate, !! .truth) %>% 
    summarise(
      N = n(),
      auc = lst(
        roc(!! .truth, !! .estimate) %>% 
          pROC::ci(method = "delong") # Change to bootstrap after testing
      )
    ) %>% 
    unnest_wider(auc) %>% 
    dplyr::rename(
      L95 = 2,
      estimate = 3,
      U95 = 4) %>% 
    mutate(label = paste0(estimate %>% round_tidy(3), 
                          " (", 
                          L95 %>% round_tidy(3), 
                          "-", 
                          U95 %>% round_tidy(3), ")"))
}

# Apply ff_auroc to scores ---------------------------------------------------------------------

map_df(list("qSOFA" = "qsofa",
            "NEWS" = "news",
            "SMARTCOP" = "smartcop",
            "SCAP" = "scap",
            "CRB65"= "crb65",
            "DS-CRB65"= "dscrb65",
            "CURB65"= "curb65",
            "SOFA" = "sofa",
            "A-DROP" = "adrop",
            "E-CURB65" = "ecurb65",
            "PSI" = "psi",
            "DL score*" = "DLscore",
            "Surgisphere*" = "surgisphere",
            "4C score" = "isaric_lasso",
            "Xie score*" = "xie_score",
            "COVID-GRAM*" = "covid_gram"
),
~ ff_aucroc(cs_test_scores, .x, "status"), .id= "test") %>% 
  arrange(estimate) %>% 
  select(test, N, label) %>% 
  mytable(caption = "Discriminatory performance of scores for mortality")

#    test             N label              
#  1 SOFA           197 0.614 (0.530-0.698)
#  2 qSOFA        19363 0.623 (0.615-0.630)
#  3 Surgisphere* 18987 0.630 (0.622-0.639)
#  4 SMARTCOP       486 0.645 (0.593-0.697)
#  5 NEWS         19075 0.654 (0.645-0.662)
#  6 DL score*    16346 0.669 (0.660-0.678)
#  7 SCAP           370 0.675 (0.620-0.729)
#  8 CRB65        19362 0.683 (0.676-0.691)
#  9 COVID-GRAM*   1239 0.706 (0.675-0.736)
# 10 DS-CRB65     18718 0.718 (0.710-0.725)
# 11 CURB65       15560 0.720 (0.713-0.728)
# 12 Xie score*    1753 0.727 (0.701-0.753)
# 13 A-DROP       15572 0.736 (0.728-0.744)
# 14 PSI            360 0.736 (0.683-0.790)
# 15 E-CURB65      1553 0.764 (0.740-0.788)
# 16 4C score     14398 0.774 (0.767-0.782)
