# ISARIC WHO CCP-UK study: 4C Mortality Score
# Data preparation
# 01_data_prep.R
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2020

# 1. Packages.
# 2. Variable definitions.
# 3. Extreme values in continuous variables. 
# 4. Training and test definitions. 
# 5. Geographical split definitions

# -------------------------------------------------------------

# Packages ----
library(tidyverse)
library(magrittr)
library(knitr)
library(kableExtra)
library(finalfit)
library(ggplot2)
library(lubridate)
library(recipes)
library(pROC)
library(grid)
library(gridExtra)
library(yardstick)

# Variable definitions ----------------------------------------
## Generated object, cs

cs = surv_data %>% 
  # Filter children
  filter(age >= 18) %>% 
  
  mutate(
    death = fct_explicit_na(death, na_level = "No"),
    hypertension_mhyn = factor(hypertension_mhyn, levels = c(1, 2, 3), labels = c("YES", "NO", "Unknown"))
  ) %>% 
  mutate_at(vars(chrincard:malnutrition_mhyn, dehydration_vsorres, -diabetes_type_mhyn), fct_recode, 
            NULL = "Unknown",
            "No" = "NO",
            "Yes" = "YES") %>%
  mutate_at(vars(chrincard:malnutrition_mhyn, dehydration_vsorres, -diabetes_type_mhyn), fct_relevel, 
            "No") %>% 
  mutate(
    sex = fct_recode(sex, NULL= "Not specified") %>% 
      ff_label("Sex"),
    
    ethnicity_4levels = fct_collapse(ethnicity,
                                     "Other Ethnic Minority" = 
                                       c("Other", "Arab", "Latin American", 
                                         "Aboriginal/First Nations", "West Asian")) %>% 
      fct_relevel("White", "South Asian", "East Asian", "Black", "Other Ethnic Minority") %>% 
      ff_label("Ethnicity"),
    
    # For NEWS
    alt_conscious = ifelse(daily_gcs_vsorres < 15, "Yes", "No"),
    hypoxic_target = "no", # This and next line for COPD patients only so not used.
    o2_rx = "no",
    
    # Neutrophil / Lymphocyte Ration
    NLR = daily_neutro_lborres / daily_lymp_lborres,
    
    diabetes_combined = case_when(
      diabetes_mhyn == "Yes" | diabetescom_mhyn == "Yes" ~ "Yes",
      is.na(diabetes_mhyn) & is.na(diabetescom_mhyn) ~ NA_character_,
      TRUE ~ "No") %>% 
      factor() %>% 
      ff_label("Diabetes"),
    
    # Combine day one BPs from different variables
    ## Make sysbp_vsorres definitive
    sysbp_vsorres = case_when(
      is.na(sysbp_vsorres) & !is.na(systolic_vsorres) ~ systolic_vsorres,
      
      # Take lowest value of two
      !is.na(sysbp_vsorres) & !is.na(systolic_vsorres) &
        systolic_vsorres > 30 &
        sysbp_vsorres > 30 &
        (systolic_vsorres < sysbp_vsorres) ~ systolic_vsorres,
      
      TRUE ~ sysbp_vsorres
    ),
    
    admission_diabp_vsorres = case_when(
      is.na(admission_diabp_vsorres) & !is.na(diastolic_vsorres) ~ diastolic_vsorres,
      
      # Take lowest value of two
      !is.na(admission_diabp_vsorres) & !is.na(diastolic_vsorres) &
        diastolic_vsorres > 10 &
        admission_diabp_vsorres > 10 &
        (diastolic_vsorres < admission_diabp_vsorres) ~ diastolic_vsorres,
      
      TRUE ~ admission_diabp_vsorres
    ),
    
    # Combine admission oxygen saturations from different variables
    oxy_vsorres= case_when(
      is.na(oxy_vsorres) & !is.na(daily_sao2_lborres) ~ daily_sao2_lborres,
      
      # Take lowest value of two
      !is.na(oxy_vsorres) & !is.na(daily_sao2_lborres) &
        daily_sao2_lborres > 50 &
        oxy_vsorres > 50 &
        (daily_sao2_lborres < oxy_vsorres) ~ daily_sao2_lborres,
      
      TRUE ~ oxy_vsorres
    ),
    
    # Comorbidity count
    no_comorbid = select(., chrincard:malnutrition_mhyn, -diabetes_type_mhyn) %>% 
      {. == "Yes"} %>% 
      rowSums(na.rm = TRUE) %>% 
      ff_label("Number of comorbidities"),
    
    no_comorbid.factor = case_when(
      no_comorbid < 1 ~ "0",
      no_comorbid < 2 ~ "1", 
      is.na(no_comorbid) ~ NA_character_,
      TRUE ~ "2+") %>% 
      factor() %>% 
      ff_label("Number of comorbidities")
  ) %>% 
  ff_relabel_df(surv_data) %>% 
  
  mutate(infiltrates_faorres = fct_recode(infiltrates_faorres, 
                                          NULL = "N/A",
                                          "No" = "NO",
                                          "Yes" = "YES") %>% 
           fct_relevel("No") %>% 
           ff_label("CXR infiltrates"),
         age.factor = ff_label(age.factor, "Age (years)"),
         daily_ldh_lborres = ff_label(daily_ldh_lborres, "LDH"),
         chrincard = ff_label(chrincard, "Chronic cardiac disease"),
         obesity_mhyn = ff_label(obesity_mhyn, "Obesity"),
         daily_crp_lborres = ff_label(daily_crp_lborres, "C-reactive protein"),
         daily_gcs_vsorres = ff_label(daily_gcs_vsorres, "GCS"),
         daily_urine_lborres = ff_label(daily_urine_lborres, "Urine flow rate")
  )


# Remove extreme values --------------------------------------------------------
cs = cs %>% 
  mutate(
    age = case_when(
      age > 110 ~ NA_real_,
      TRUE ~ age
    ), 
    temp_vsorres = case_when(
      temp_vsorres > 3000 ~ temp_vsorres / 100,
      temp_vsorres > 300 ~ temp_vsorres / 10,
      temp_vsorres  > 200 ~ temp_vsorres - 200,
      temp_vsorres > 100 ~ temp_vsorres - 100,
      temp_vsorres > 45 ~ NA_real_,
      temp_vsorres < 0 ~ abs(temp_vsorres),
      temp_vsorres < 3 ~ NA_real_,
      temp_vsorres < 4 ~ temp_vsorres * 10,
      temp_vsorres < 25 ~ NA_real_,
      TRUE ~ temp_vsorres
    ), 
    hr_vsorres = case_when(
      hr_vsorres > 300 ~ NA_real_,
      hr_vsorres < 30 ~ NA_real_,
      TRUE ~ hr_vsorres
    ),
    rr_vsorres = case_when(
      rr_vsorres > 150 ~ NA_real_,
      TRUE ~ rr_vsorres
    ),
    daily_gcs_vsorres = case_when(
      daily_gcs_vsorres < 3 ~ 3,
      daily_gcs_vsorres > 15 ~ 15,
      TRUE ~ daily_gcs_vsorres
    ),
    oxy_vsorres  = case_when(
      oxy_vsorres < 0 ~ abs(oxy_vsorres), 
      oxy_vsorres < 50 ~ NA_real_,
      oxy_vsorres > 100 ~ NA_real_,
      TRUE ~ oxy_vsorres),
    daily_plt_lborres = ifelse(daily_plt_lborres > 2000, NA_real_,  daily_plt_lborres),
    NLR = case_when(
      NLR <= 0 ~ NA_real_,
      NLR > 200 ~ NA_real_,
      TRUE ~ NLR),
    admission_diabp_vsorres = case_when(
      admission_diabp_vsorres < 20 ~ NA_real_,
      admission_diabp_vsorres > 200 ~ NA_real_,
      TRUE ~ admission_diabp_vsorres),
    sysbp_vsorres = case_when(
      sysbp_vsorres < 10 ~ NA_real_,
      sysbp_vsorres > 300 ~ NA_real_,
      TRUE ~ sysbp_vsorres),
    daily_fio2_lborres = case_when(
      daily_fio2_lborres > 1 ~ daily_fio2_lborres / 100,
      TRUE ~ daily_fio2_lborres),
    daily_pao2_lborres = case_when(
      daily_pao2_lborres > 100 ~ NA_real_,
      TRUE ~ daily_pao2_lborres),
    daily_ph_lborres  = case_when(
      daily_ph_lborres < 6 ~ NA_real_,
      TRUE ~ daily_ph_lborres),
    daily_urine_lborres = case_when(
      daily_urine_lborres < 0 ~ abs(daily_urine_lborres),
      TRUE ~ daily_urine_lborres),
    daily_bun_lborres = case_when(
      daily_bun_lborres > 100 ~ 100,
      TRUE ~ daily_bun_lborres),
    daily_hb_lborres = case_when(
      daily_hb_lborres > 300 ~ NA_real_,
      daily_hb_lborres < 20 ~ NA_real_,
      TRUE ~ daily_hb_lborres),
    daily_neutro_lborres = case_when(
      daily_neutro_lborres >= 100 ~ NA_real_,
      TRUE ~ daily_neutro_lborres),
    daily_lymp_lborres = case_when(
      daily_lymp_lborres >= 100 ~ NA_real_,
      TRUE ~ daily_lymp_lborres),
    daily_pt_lborres_add_inr = case_when(
      daily_pt_lborres_add_inr > 150 ~ NA_real_,
      daily_pt_lborres_add_inr < 9 ~ NA_real_,
      TRUE ~ daily_pt_lborres_add_inr),
    daily_aptt_lborres = case_when(
      daily_aptt_lborres > 150 ~ NA_real_,
      daily_aptt_lborres < 4 ~ daily_aptt_lborres * 22,
      TRUE ~ daily_aptt_lborres),
    daily_sodium_lborres = case_when(
      daily_sodium_lborres > 180 ~ NA_real_,
      daily_sodium_lborres < -100 ~ abs(daily_sodium_lborres),
      daily_sodium_lborres < 100 ~ NA_real_,
      TRUE ~ daily_sodium_lborres),
    daily_potassium_lborres = case_when(
      daily_potassium_lborres < 0.55 ~ daily_potassium_lborres * 10,
      TRUE ~ daily_potassium_lborres),
    daily_crp_lborres = case_when(
      daily_crp_lborres < 0 ~ abs(daily_crp_lborres),
      daily_crp_lborres > 750 ~ 750,
      TRUE ~ daily_crp_lborres),
    daily_lactate_lborres= case_when(
      daily_lactate_lborres < 0 ~ abs(daily_lactate_lborres),
      TRUE ~ daily_lactate_lborres)
  ) %>% 
  ff_relabel_df(cs)

# Make training and test sets -------------------------------------------------
## Primary analysis performed with data available 21/05/2020

cs_train = cs %>% 
  filter(subjid %in% training_subjid) %>% 
  ff_relabel_df(cs)

cs_test = cs %>% 
  filter(subjid %in% testing_subjid)%>% 
  ff_relabel_df(cs)

# Define geographical spilt ----
south = c("addenbrookes_hospi", "ashford_and_st_pet", "barking_havering_a",
          "bristol_royal_infi", "epsom__st_helier_n", "livewell_southwest", "hillingdon_hospita",
          "london_north_west", "kingston_hospital", "musgrove_park_hosp", "dorset_county_hosp", 
          "isle_of_wight_nhs", "northwick_park", "royal_surrey_nhsft", "royal_free_hospita",
          "north_middlesex_un", "northern_devon_hea", "luton_and_dunstabl", "royal_papworth_hos",
          "basildon_and_thurr", "ipswich_hospital", "east_suffolk_and_n", "sussex_community_n",
          "dorset_healthcare",  "royal_bournemouth", "royal_cornwall", "barking_havering_a", 
          "solent_nhs_trust", "weston_area_health", "torbay_and_south_d", "royal_devon_and_ex",
          "university_college", "university_hospitag", "yeovil_district_ho", "west_suffolk_hospi",
          "university_hospitae", "portsmouth_hospita", "royal_berkshire_ho", "guys_and_st_thomas",
          "lewisham_and_green", "croydon_health_ser",  "st_georges_univers",  "cornwall_partnersh",
          "kings_college_hosp", "university_hospitad", "norfolk_and_norwic", "chelsea__westminst",
          "norfolk_and_suffol", "hampshire_hospital", "nihr_team_wessex", "the_royal_marsden",
          "surrey_and_sussex", "gloucestershire_he", "east_sussex_health", "western_sussex_hos",
          "gloucester_royal_h",  "east_kent_hospital", "brighton_and_susse", "royal_brompton_and",
          "cambridge_and_pete", "oxford_university", "southmead_hospital", "ashford_and_st_pet",
          "fairfield_general", "maidstone_and_tunb", "east_and_north_her", "hertfordshire_part",
          "buckinghamshire_he", "surrey_and_borders", "imperial_college_h", "mid_and_south_esse",
          "bedfordshire_hospi", "milton_keynes_univ", "the_queen_elizabet", "royal_united_hospi",
          "poole_hospital_nhs", "frimley_health_nhs", "james_paget_univer", "whittington_health",
          "great_western_nhs", "dartford_and_grave", "the_dudley_group_n", "medway_nhsft",
          "the_princess_alexa", "homerton_universit", "west_hertfordshire"
)

north = c("aberdeen_royal_inf", "aintree_university", "airedale_general_h", "airedale_general_h",
          "alder_hey_children", "aneurin_bevan_univ", "barnsley_hospital",
          "betsi_cadwaladr_un", "bradford_teaching", "cardiff_and_vale_u", "cwm_taf_morgannwg",
          "glan_clwyd_hospita", "glangwili_general", "glasgow_royal_infi",
          "manchester_univers", "harrogate_district", "leeds_and_york_par",
          "morriston_hospital", "nhs_lanarkshire", "doncaster_and_bass",  "lincolnshire_partn",
          "nhs_ayrshire_and_a", "borders_general_ho", "golden_jubilee_nat", "nhs_dumfries_and_g",
          "nhs_forth_valley",  "ninewells_hospital",  "bradford_district",   "sheffield_health",
          "queen_elizabeth_un", "royal_liverpool__b", "mid_cheshire_hospi", "the_christie_nhsft",
          "south_tyneside_and", "liverpool_womens_h", "nottinghamshire_he", "sheffield_teaching",
          "the_pennine_acute", "victoria_hospital", "university_hospitac", "university_hospital",
          "wrexham_maelor_hos", "wirral_university", "whiston_hospital", "york_teaching_hosp",
          "northern_lincolnsh", "macclesfield_distr", "countess_of_cheste", "nottingham_univers",
          "sherwood_forest_ho", "salford_royal_nhsf",  "bolton_nhst", "north_cumbria_inte",
          "west_cumberland_ho", "royal_preston_hosp", "gateshead_health_n",  "leeds_teaching_hos",
          "south_tees_hospita",  "the_newcastle_upon", "nhs_lothian", "warrington_and_hal",
          "tees_esk_and_wear", "mid_yorkshire_hosp",  "south_west_yorkshi", "cheshire_and_wirra",
          "cumbria_northumber", "northumbria_health", "lincolnshire_commu", "leicestershire_par",
          "merseycare_nhs_fou",  "lancashire__south", "blackpool_teaching", "humber_teaching_nh",
          "university_hospitai", "royal_albert_edwar", "calderdale_and_hud", "university_hospitab",
          "united_lincolnshir", "bronglais_general", "stockport_nhsft", "university_hospitak",
          "north_west_borough", "southport_and_orms", "university_hospitaj", "rotherham_doncaste",
          "university_hospita", "darlington_memoria", "east_lancashire_ho", "worcestershire_hea",
          "walsall_healthcare", "clatterbridge_canc", "the_rotherham_nhsf", "chesterfield_royal",
          "north_west_anglia", "shropshire_communi", "warwick_hospital", "university_hospitah",
          "robert_jones__agne", "the_royal_wolverha",  "hereford_hospital", "george_eliot_hospi",
          "north_staffordshir", "tameside_hospital", "northampton_genera", "kettering_general",
          "birmingham_womens", "midlands_partnersh", "the_royal_oldham_h", "university_hospitaf",
          "worcestershire_acu", "sandwell_and_west", "shrewsbury__telfor", "coventry__warwicks",
          "birmingham_communi", "black_country_heal", "prince_philip_hosp", "withybush_general"
)

cs_south = cs_test %>% 
  filter(redcap_data_access_group %in% south) %>% 
  ff_relabel_df(cs)

cs_north = cs_test %>% 
  filter(redcap_data_access_group %in% north) %>% 
  ff_relabel_df(cs)


subjid_south = tmp_south %>% 
  pull(subjid)

subjid_north = tmp_north %>% 
  pull(subjid)
