# ISARIC WHO CCP-UK study: 4C Mortality Score
# Functions
# Any bespoke function is placed here. 
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2020

# Functions require library(tidyverse), requires() nor :: not written in.  

# 1. ggplot templates / extraction functions
# 2. Table defaults
# 3. Prognostic scoring

# Lisa's lovely Lancet-style ggplot template (Lisa Norman) ---------------------------------------------
ggplot_lancet <- function(...)
  ggplot2::ggplot(...) +
  scale_fill_brewer(palette = "Blues") +
  scale_colour_brewer(palette = "Blues") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(
    axis.title.x       = element_text(margin = margin(10, 0, 0, 0, "pt")),
    axis.title.y       = element_text(margin = margin(0, 10, 0, 0, "pt")),
    axis.text          = element_text(size = 10),
    axis.ticks.length  = unit(5, "pt"),
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    strip.background   = element_blank(),
    strip.placement    = "outside",
    strip.text.x       = element_text(hjust = 0,
                                      face  = "bold",
                                      size  = 10),
    strip.text.y       = element_text(hjust = 0,
                                      size  = 10),
    text = element_text(
      family = "Merriweather",
      size = 10,
      face = "plain"
    ),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.direction = "horizontal",
    legend.key.size = unit(10, "pt")
  ) +
  guides(fill = guide_legend(title.position = "top"))


# Extract ggplot y-axis limit for placing text annotations ----------------------------------- 
## Doesn't work with facetted plots 
## Other parameters left for future need
get_ymax <- function(plot) {
  gb = ggplot_build(plot)
  # xmin = gb$layout$panel_params[[1]]$x.range[1]
  # xmax = gb$layout$panel_params[[1]]$x.range[2]
  # ymin = gb$layout$panel_params[[1]]$y.range[1]
  ymax = gb$layout$panel_params[[1]]$y.range[2]
  # list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  return(ymax)
}


# Table defaults ---------------------------------------------------------------------------
# This makes table resize or continue over multiple pages in all output types
# PDF powered by kableExtra, Word by flextable
mytable = function(x, caption = "", longtable = TRUE, ...){
  
  # if not latex or html then else is Word
  if (is_latex_output() | is_html_output()){
    knitr::kable(x, row.names = FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r", "r"), 
                 booktabs = TRUE, caption = caption, longtable = longtable,
                 linesep = "", ...) %>%
      kableExtra::kable_styling(latex_options = c("scale_down", "hold_position"))
  }else{
    flextable::flextable(x) %>% 
      flextable::autofit() %>% 
      flextable::width(j = 1, width = 1.5) %>% 
      flextable::height(i = 1, height = 0.5, part = "header")
  }
}


# Prognostic scores ---------------------------------------------------------------------------------
## All scores written in rlang. 
## These take non-quoted variable names in arguments. 
## Output can be a single vector, vector of components, or the original tibble/dataframe with vector/components
## NEWS was made using cut(), but dplyr::case_when() more flexible, easier to read and less error prone. 

# National early warning score  ---------------------------------------------------------------------
news <- function(.data, rr, spo2, o2_rx, hypoxic_target, sbp, hr, temp, alt_conscious, 
                 output = c("vector", "components", "df_vector", "df_components"), 
                 na_to_zeros = TRUE, all_na_rm = TRUE){

  .rr = enquo(rr)
  .spo2 = enquo(spo2)
  .o2_rx = enquo(o2_rx)
  .hypoxic_target = enquo(hypoxic_target)
  .sbp = enquo(sbp)
  .hr = enquo(hr)
  .temp = enquo(temp)
  .alt_conscious = enquo(alt_conscious)
  
  out <- .data %>%
    # Ensure correct class / format
    dplyr::mutate_at(vars(!! .rr, !! .spo2, !! .sbp, !! .hr, !! .temp), as.numeric) %>% 
    dplyr::mutate_at(vars(!! .o2_rx, !! .hypoxic_target, !! .alt_conscious), 
                     ~ as.character(.) %>% tolower() %>% trimws()) %>% 
    
    # Convert parameters to NEWS scores
    dplyr::mutate(
      
      news_spo2 = case_when(
      !! .hypoxic_target == "yes" & 
        !! .o2_rx == "yes"       ~ cut(!! .spo2, right = FALSE,
                                       breaks = c(-Inf, 84, 86, 88, 93, 95, 97, Inf),
                                       labels = c(3, 2, 1, 0, 1, 2, 3)),
      !! .hypoxic_target == "yes" & 
        !! .o2_rx == "no"        ~ cut(!! .spo2, right = FALSE,
                                       breaks = c(-Inf, 84, 86, 88, Inf),
                                       labels = c(3, 2, 1, 0)),
      TRUE                       ~ cut(!! .spo2, right = FALSE,
                                       breaks = c(-Inf, 92, 94, 96, Inf),
                                       labels = c(3, 2, 1, 0))
    ),
    news_alt_conscious = case_when(
      !! .alt_conscious %in% 
        c("1", "yes")                 ~ 3,
      !! .alt_conscious %in%
        c("0", "no")                  ~ 0,
      TRUE                            ~ NA_real_),
    news_o2_rx = case_when(
      !! .o2_rx %in% 
        c("1", "yes")                 ~ 2,
      !! .o2_rx %in%
        c("0", "no")                  ~ 0,
      TRUE                            ~ NA_real_),
    
    news_temp                         = cut(!! .temp, breaks = c(-Inf, 35, 36, 38, 39, Inf),
                                            labels = c(3,1,0,1,2)),
    news_sbp                          = cut(!! .sbp, breaks = c(-Inf, 90, 100, 110, 219, Inf),
                                            labels = c(3,2,1,0,3)),
    news_hr                           = cut(!! .hr, breaks = c(-Inf, 40, 50, 90, 110, 130, Inf),
                                            labels = c(3,1,0,1,2,3)),
    
    # That RR 8 to 11 has 2 patients patients, therefore drop here. 
    # news_rr                           = cut(!! .rr, breaks = c(-Inf, 8, 11, 20, 24, Inf),
    #                                         labels = c(3, 1, 0, 2, 3))) %>% 
    
    news_rr                           = cut(!! .rr, breaks = c(-Inf, 8, 20, 24, Inf),
                                            labels = c(3, 0, 2, 3))) %>% 
    
    dplyr::mutate_at(vars(news_spo2, news_temp, news_sbp, news_hr, news_rr), 
                     ~ as.character(.) %>% as.numeric()) %>% 
    
    # total of all "news" columns
    mutate(news = rowSums(dplyr::select(., dplyr::starts_with("news_")),
                          na.rm = na_to_zeros)) %>% 
    {if(all_na_rm){
      dplyr::mutate(., news = dplyr::if_else(is.na(news_alt_conscious) &
                                               is.na(news_temp) &
                                               is.na(news_sbp) & 
                                               is.na(news_hr) &
                                               is.na(news_rr), NA_real_, news))
    } else {
      .
    }} 
  if(output == "vector"){
    out %>% 
      pull(news)
  } else if(output == "components"){
    out %>% 
      select(starts_with("news"))
  } else if(output == "df_vector"){
    out %>% 
      pull(news) %>% 
      bind_cols(.data, news = .)
  } else if(output == "df_components"){
    out
  }
}
# Example----------------------
# example <- dplyr::bind_cols(resprate = sample(c(1:40, c(NA, NA, NA, NA)), 200, replace = T),
#                             spo2 = sample(70:100, 200, replace = T),
#                             o2_therapy = sample(c("No", "Yes", "No", NA, NA), 200, replace = T),
#                             hypox_yn = sample(c("No", "Yes", "No", "No", NA), 200, replace = T),
#                             sys_bp = sample(30:250, 200, replace = T),
#                             heartrate = sample(c(30:250, rep(NA, 50)), 200, replace = T),
#                             temp = sample(seq(33, 40, by = 0.1), 200, replace = T),
#                             alt_conscious = sample(c("No", "Yes", "No", NA), 200, replace = T))
# news(example,
#       rr = "resprate", spo2 = "spo2", o2_rx = "o2_therapy", hypoxic_target = "hypox_yn",
#       sbp = "sys_bp", hr = "heartrate", temp = "temp", alt_conscious = "alt_conscious", output = "df") %>%
#   select(starts_with("news"))
# example %>% tail()
# tmp %>% tail()
# example %>% tail()
# example = example %>% add_row(resprate = NA, spo2 = NA, o2_therapy = NA, hypox_yn = NA, 
#                     sys_bp = NA, heartrate = NA, temp = NA, alt_conscious = NA)
# news(example,
#      rr = "resprate", spo2 = "spo2", o2_rx = "o2_therapy", hypoxic_target = "hypox_yn",
#      sbp = "sys_bp", hr = "heartrate", temp = "temp", alt_conscious = "alt_conscious", output = "df") %>%
#   select(starts_with("news")) %>% 
#   tail()


# SOFA --------------------------------------------------
sofa <- function(.data, po2, fio2, invasive_vent, plts, bil, gcs, creat, map, 
                 dopgr15, dop5to15, dopless5, 
                 output = c("vector", "components", "df_vector", "df_components"), 
                 na_to_zeros = TRUE, all_na_rm = TRUE){
  
  .po2 = enquo(po2)
  .fio2 = enquo(fio2)
  .invasive_vent = enquo(invasive_vent)
  .plts = enquo(plts)
  .bil = enquo(bil)
  .gcs = enquo(gcs)
  .creat = enquo(creat)
  .map = enquo(map)
  .dopgr15 = enquo(dopgr15)
  .dop5to15 = enquo(dop5to15)
  .dopless5 = enquo(dopless5)
  
  out = .data %>% 
    dplyr::mutate_at(vars(!! .po2, !! .fio2, !! .plts, !! .bil, !! .gcs, !! .creat, !! .map), as.numeric) %>% 
    dplyr::mutate_at(vars(!! .invasive_vent, !! .dopgr15, !! .dop5to15, !! .dopless5), 
                     ~ as.character(.) %>% tolower() %>% trimws()) %>% 
    
    mutate(
      # PaO2/FiO2 ratio
      .pf_ratio = (!! .po2*7.5)/!! .fio2,
      
      # Respiration SOFA score
      sofa_respi = case_when(
        .pf_ratio < 100 &
          !! .invasive_vent == "yes" ~ 4, # and ventilated
        .pf_ratio < 200 &
          !! .invasive_vent == "yes" ~ 3, # and ventilated
        .pf_ratio < 300 ~ 2,
        .pf_ratio < 400 ~ 1,
        .pf_ratio >= 400 ~ 0,
        TRUE ~ NA_real_),
      
      # Coagulation  SOFA score
      sofa_coagu = case_when(
        !! .plts < 20 ~ 4,
        !! .plts < 50 ~ 3,
        !! .plts < 100 ~ 2,
        !! .plts < 150 ~ 1,
        !! .plts >= 150 ~ 0,
        TRUE ~ NA_real_),
      
      # Liver  SOFA score
      sofa_liver = case_when(
        !! .bil < 20 ~ 0,
        !! .bil < 33 ~ 1,
        !! .bil < 102 ~ 2,
        !! .bil < 204 ~ 3,
        !! .bil >= 204 ~ 4,
        TRUE ~ NA_real_),
      
      # Central nervous system SOFA score
      sofa_nervo = case_when(
        !! .gcs < 6 ~ 4,
        !! .gcs  <= 9 ~ 3,
        !! .gcs  <= 12  ~ 2,
        !! .gcs  <= 14  ~ 1,
        !! .gcs   > 14 ~ 0,
        TRUE ~ NA_real_),
      
      # Renal  SOFA score
      sofa_renal = case_when(
        !! .creat < 110 ~ 0,
        !! .creat < 171 ~ 1,
        !! .creat < 300 ~ 2,
        !! .creat < 441 ~ 3,
        !! .creat >= 441 ~ 4,
        TRUE ~ NA_real_),
      
      sofa_cardi = case_when(
        !! .dopgr15 == "yes" ~ 4,      
        !! .dop5to15 == "yes" ~ 3,      
        !! .dopless5 == "yes" ~ 2,
        !! .map < 70 ~ 1,
        !! .map >= 70 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>% 
    
    # total of all "news" columns
    mutate(sofa = rowSums(dplyr::select(., dplyr::starts_with("sofa_")),
                          na.rm = na_to_zeros)) %>% 
    {if(all_na_rm){
      dplyr::mutate(., sofa = dplyr::if_else(
        is.na(sofa_respi) &
          is.na(sofa_coagu) &
          is.na(sofa_liver) &
          is.na(sofa_nervo) &
          is.na(sofa_renal) &
          is.na(sofa_cardi), NA_real_, sofa))
    } else {
      .
    }}
  
  if(output == "vector"){
    out %>% 
      pull(sofa)
  } else if(output == "components"){
    out %>% 
      select(starts_with("sofa"))
  } else if(output == "df_vector"){
    out %>% 
      pull(sofa) %>% 
      bind_cols(.data, sofa = .)
  } else if(output == "df_components"){
    out
  }
}
# example = dplyr::bind_cols(po2 = sample(c(8:20, c(NA, NA, NA, NA)), 200, replace = T),
#                            fio2 = sample(0.2:0.8, 200, replace = T),
#                            invasive_vent = sample(c("No", "Yes", "No", NA, NA), 200, replace = T),
#                            plts = sample(15:200, 200, replace = T),
#                            bil = sample(10:150, 200, replace = T),
#                            gcs = sample(3:15, 200, replace = T),
#                            creat = sample(60:250, 200, replace = T),
#                            map = sample(40:80, 200, replace = T),
#                            dopgr15 = sample(c("No", "Yes", NA), 200, replace = TRUE),
#                            dop5to15 = sample(c("No", "Yes", NA), 200, replace = TRUE),
#                            dopless5 = sample(c("No", "Yes", NA), 200, replace = TRUE)) %>% 
#   add_row(po2 = NA, fio2 = NA, invasive_vent = NA, plts = NA, 
#           bil= NA, gcs = NA, creat = NA, map = NA, dopgr15 = NA, dop5to15 = NA, dopless5 = NA)
# 
# example %>%                            
#   sofa(po2 = po2, fio2 = fio2, invasive_vent = invasive_vent, plts = plts, 
#        bil= bil, gcs = gcs, creat = creat, map = map, dopgr15 = dopgr15, dop5to15 = dop5to15, dopless5 = dopless5,
#        output = "components", na_to_zeros = FALSE, all_na_rm = FALSE) %>% 
#   tail()


# qsofa -----------------------------------------------------
qsofa <- function(.data, rr, sbp, gcs,
                 output = c("vector", "components", "df_vector", "df_components"), 
                 na_to_zeros = TRUE, all_na_rm = TRUE){
  
  .rr = enquo(rr)
  .sbp = enquo(sbp)
  .gcs = enquo(gcs)

  out = .data %>% 
    dplyr::mutate_at(vars(!! .rr, !! .sbp, !! .gcs), as.numeric) %>% 
    # dplyr::mutate_at(vars(!! .avpu), 
    #                  ~ as.character(.) %>% tolower() %>% trimws()) %>% 
    
    mutate(
      # qSOFA score
      qsofa_rr = case_when(
        !! .rr >=  22 ~ 1, 
        !! .rr < 22 ~ 0,
        TRUE ~ NA_real_),
      
      qsofa_sbp = case_when(
        !! .sbp <=  100 ~ 1, 
        !! .sbp > 100 ~ 0,
        TRUE ~ NA_real_),
      
      qsofa_gcs = case_when(
        !! .gcs <=  14 ~ 1, 
        !! .gcs > 14 ~ 0,
        TRUE ~ NA_real_),
    
    ) %>% 
    mutate(
      qsofa = rowSums(dplyr::select(., dplyr::starts_with("qsofa_")),
                      na.rm = na_to_zeros)
    ) %>% 
    
    {if(all_na_rm){
      dplyr::mutate(., qsofa = dplyr::if_else(
          is.na(qsofa_rr) &
          is.na(qsofa_sbp) &
          is.na(qsofa_gcs), NA_real_, qsofa))
    } else {
      .
    }}
  
  if(output == "vector"){
    out %>% 
      pull(qsofa)
  } else if(output == "components"){
    out %>% 
      select(starts_with("qsofa"))
  } else if(output == "df_vector"){
    out %>% 
      pull(qsofa) %>% 
      bind_cols(.data, qsofa = .)
  } else if(output == "df_components"){
    out
  }
}


# Coronascore-------------------------------------------------------------------
coronascore <- function(.data, age, rr, spo2, sbp, gcs, card_comrb, resp_comrb, renal_comrb, cancer_comrb,
                  output = c("vector", "components", "df_vector", "df_components"), 
                  na_to_zeros = TRUE, all_na_rm = TRUE){
  
  .age = enquo(age)
  .rr = enquo(rr)
  .spo2 = enquo(spo2)
  .sbp = enquo(sbp)
  .gcs = enquo(gcs)
  .card_comrb = enquo(card_comrb)
  .resp_comrb = enquo(resp_comrb)
  .renal_comrb = enquo(renal_comrb)
  .cancer_comrb = enquo(cancer_comrb)
  
  out = .data %>% 
    dplyr::mutate_at(vars(!! .age, !! .rr, !! .spo2, !! .sbp, !! .gcs), as.numeric) %>% 
    dplyr::mutate_at(vars(!! .card_comrb, !! .resp_comrb, !! .renal_comrb, !! .cancer_comrb), 
                     ~ as.character(.) %>% tolower() %>% trimws()) %>% 
    
    mutate(
      # coronascore
      coronascore_age = case_when(
        !! .age < 50 ~ 1,
        !! .age < 60 ~ 2,
        !! .age < 70 ~ 3, 
        !! .age < 80 ~ 4,
        is.na(!! .age) ~ NA_real_,
        TRUE ~ 5),
      
      coronascore_rr = case_when(
        !! .rr >=  24 ~ 1, 
        !! .rr < 24 ~ 0,
        TRUE ~ NA_real_),
      
      coronascore_spo2 = case_when(
        !! .spo2 <=  90 ~ 1,   #changed from >= 90 (which didn't make sense)
        !! .spo2 > 90 ~ 0,
        TRUE ~ NA_real_),
      
      coronascore_sbp = case_when(
        !! .sbp <=  100 ~ 1, 
        !! .sbp > 100 ~ 0,
        TRUE ~ NA_real_),
      
      coronascore_gcs = case_when(
        !! .gcs <=  14 ~ 1, 
        !! .gcs > 14 ~ 0,
        TRUE ~ NA_real_),
      
      coronascore_card_comrb = case_when(
        !! .card_comrb == "yes" ~ 1, 
        !! .card_comrb == "no" ~ 0, 
        TRUE ~ NA_real_),
      
      coronascore_resp_comrb = case_when(
        !! .resp_comrb == "yes" ~ 1, 
        !! .resp_comrb == "no" ~ 0, 
        TRUE ~ NA_real_),
      
      coronascore_renal_comrb = case_when(
        !! .renal_comrb == "yes" ~ 1, 
        !! .renal_comrb == "no" ~ 0, 
        TRUE ~ NA_real_),
      
      coronascore_cancer_comrb = case_when(
        !! .cancer_comrb == "yes" ~ 1, 
        !! .cancer_comrb == "no" ~ 0, 
        TRUE ~ NA_real_),
      
    ) %>% 
    mutate(
      coronascore = rowSums(dplyr::select(., dplyr::starts_with("coronascore_")),
                      na.rm = na_to_zeros)
    ) %>% 
    
    {if(all_na_rm){
      dplyr::mutate(., coronascore = dplyr::if_else(
        (is.na(coronascore_rr) &
          is.na(coronascore_spo2) &
          is.na(coronascore_sbp) &
          is.na(coronascore_gcs)) | 
          (is.na(coronascore_card_comrb) &
          is.na(coronascore_resp_comrb) &
          is.na(coronascore_renal_comrb) &
          is.na(coronascore_cancer_comrb)), NA_real_, coronascore))
    } else {
      .
    }}
  
  if(output == "vector"){
    out %>% 
      pull(coronascore)
  } else if(output == "components"){
    out %>% 
      select(starts_with("coronascore"))
  } else if(output == "df_vector"){
    out %>% 
      pull(coronascore) %>% 
      bind_cols(.data, coronascore = .)
  } else if(output == "df_components"){
    out
  }
}


# curb65 -----------------------------------------------------
curb65 <- function(.data, rr, sbp, dbp, gcs, bun, age,
                  output = c("vector", "components", "df_vector", "df_components"), 
                  na_to_zeros = TRUE, all_na_rm = TRUE){
  
  .rr = enquo(rr)
  .sbp = enquo(sbp)
  .dbp = enquo(dbp)
  .gcs = enquo(gcs)
  .bun = enquo(bun)
  .age = enquo(age)
  
  out = .data %>% 
    dplyr::mutate_at(vars(!! .rr, !! .sbp, !! .dbp, !! .gcs, !! .bun, !! .age), as.numeric) %>% 
    # dplyr::mutate_at(vars(!! .avpu), 
    #                  ~ as.character(.) %>% tolower() %>% trimws()) %>% 
    
    mutate(
      # curb65 score
      curb65_rr = case_when(
        !! .rr >=  30 ~ 1, 
        !! .rr < 30 ~ 0,
        TRUE ~ NA_real_),
      
      curb65_bp = case_when(
        !! .sbp <  90 |
          !! .dbp <= 60 ~ 1, 
        is.na(!! .sbp) & is.na(!! .dbp) ~ NA_real_,
        TRUE ~ 0),
      
      curb65_gcs = case_when(
        !! .gcs <=  14 ~ 1, 
        !! .gcs > 14 ~ 0,
        TRUE ~ NA_real_),
      
      curb65_bun = case_when(
        !! .bun >  7 ~ 1, 
        !! .bun <= 7 ~ 0,
        TRUE ~ NA_real_),
      
      curb65_age = case_when(
        !! .age >= 65 ~ 1, 
        !! .age < 65 ~ 0,
        TRUE ~ NA_real_),
      
    ) %>% 
    mutate(
      curb65 = rowSums(dplyr::select(., dplyr::starts_with("curb65_")),
                      na.rm = na_to_zeros)
    ) %>% 
    
    {if(all_na_rm){
      dplyr::mutate(., curb65 = dplyr::if_else(
        is.na(curb65_rr) &
          is.na(curb65_bp) &
          is.na(curb65_gcs) &
          is.na(curb65_bun) &
          is.na(curb65_age), NA_real_, curb65))
    } else {
      .
    }}
  
  if(output == "vector"){
    out %>% 
      pull(curb65)
  } else if(output == "components"){
    out %>% 
      select(starts_with("curb65"))
  } else if(output == "df_vector"){
    out %>% 
      pull(curb65) %>% 
      bind_cols(.data, curb65 = .)
  } else if(output == "df_components"){
    out
  }
}


# crb65 -----------------------------------------------------
crb65 <- function(.data, rr, sbp, dbp, gcs, age,
                   output = c("vector", "components", "df_vector", "df_components"), 
                   na_to_zeros = TRUE, all_na_rm = TRUE){
  
  .rr = enquo(rr)
  .sbp = enquo(sbp)
  .dbp = enquo(dbp)
  .gcs = enquo(gcs)
  .age = enquo(age)
  
  out = .data %>% 
    dplyr::mutate_at(vars(!! .rr, !! .sbp, !! .dbp, !! .gcs, !! .age), as.numeric) %>% 
    # dplyr::mutate_at(vars(!! .avpu), 
    #                  ~ as.character(.) %>% tolower() %>% trimws()) %>% 
    
    mutate(
      # crb65 score
      crb65_rr = case_when(
        !! .rr >=  30 ~ 1, 
        !! .rr < 30 ~ 0,
        TRUE ~ NA_real_),
      
      crb65_bp = case_when(
        !! .sbp <  90 |
          !! .dbp <= 60 ~ 1, 
        is.na(!! .sbp) & is.na(!! .dbp) ~ NA_real_,
        TRUE ~ 0),
      
      crb65_gcs = case_when(
        !! .gcs <=  14 ~ 1, 
        !! .gcs > 14 ~ 0,
        TRUE ~ NA_real_),
      
      crb65_age = case_when(
        !! .age >= 65 ~ 1, 
        !! .age < 65 ~ 0,
        TRUE ~ NA_real_),
      
    ) %>% 
    mutate(
      crb65 = rowSums(dplyr::select(., dplyr::starts_with("crb65_")),
                       na.rm = na_to_zeros)
    ) %>% 
    
    {if(all_na_rm){
      dplyr::mutate(., crb65 = dplyr::if_else(
        is.na(crb65_rr) &
          is.na(crb65_bp) &
          is.na(crb65_gcs) &
          is.na(crb65_age), NA_real_, crb65))
    } else {
      .
    }}
  
  if(output == "vector"){
    out %>% 
      pull(crb65)
  } else if(output == "components"){
    out %>% 
      select(starts_with("crb65"))
  } else if(output == "df_vector"){
    out %>% 
      pull(crb65) %>% 
      bind_cols(.data, crb65 = .)
  } else if(output == "df_components"){
    out
  }
}


# ecurb65 -----------------------------------------------------
ecurb65 <- function(.data, rr, sbp, dbp, gcs, bun, age,
                    ldh, plts, #alb,
                   output = c("vector", "components", "df_vector", "df_components"), 
                   na_to_zeros = TRUE, all_na_rm = TRUE){
  
  .rr = enquo(rr)
  .sbp = enquo(sbp)
  .dbp = enquo(dbp)
  .gcs = enquo(gcs)
  .bun = enquo(bun)
  .age = enquo(age)
  .ldh = enquo(ldh)
  # .alb = enquo(alb) # Albumin not available in ISARIC
  .plts = enquo(plts)
  
  out = .data %>% 
    dplyr::mutate_at(vars(!! .rr, !! .sbp, !! .dbp, !! .gcs, !! .bun, !! .age, #!! .alb,
                          !! .ldh, !! .plts), as.numeric) %>% 
    # dplyr::mutate_at(vars(!! .avpu), 
    #                  ~ as.character(.) %>% tolower() %>% trimws()) %>% 
    
    mutate(
      # ecurb65 score
      ecurb65_rr = case_when(
        !! .rr >=  30 ~ 1, 
        !! .rr < 30 ~ 0,
        TRUE ~ NA_real_),
      
      ecurb65_bp = case_when(
        !! .sbp <  90 |
          !! .dbp <= 60 ~ 1, 
        is.na(!! .sbp) & is.na(!! .dbp) ~ NA_real_,
        TRUE ~ 0),
      
      ecurb65_gcs = case_when(
        !! .gcs <=  14 ~ 1, 
        !! .gcs > 14 ~ 0,
        TRUE ~ NA_real_),
      
      ecurb65_bun = case_when(
        !! .bun >  7 ~ 1, 
        !! .bun <= 7 ~ 0,
        TRUE ~ NA_real_),
      
      ecurb65_age = case_when(
        !! .age >= 65 ~ 1, 
        !! .age < 65 ~ 0,
        TRUE ~ NA_real_),
      
      ecurb65_ldh = case_when(
        !! .ldh > 230 ~ 1, 
        !! .ldh <= 230 ~ 0,
        TRUE ~ NA_real_),
      
      # ecurb65_alb = case_when(
      #   !! .alb < 35 ~ 1, 
      #   !! .alb >=35 ~ 0,
      #   TRUE ~ NA_real_),
      
      ecurb65_plts = case_when(
        !! .plts < 100 ~ 1, 
        !! .plts >= 100 ~ 0,
        TRUE ~ NA_real_),
      
    ) %>% 
    mutate(
      ecurb65 = rowSums(dplyr::select(., dplyr::starts_with("ecurb65_")),
                       na.rm = na_to_zeros)
    ) %>% 
    
    {if(all_na_rm){
      dplyr::mutate(., ecurb65 = dplyr::if_else(
        is.na(ecurb65_rr) &
          is.na(ecurb65_bp) &
          is.na(ecurb65_gcs) &
          is.na(ecurb65_bun) &
          is.na(ecurb65_age) &
          is.na(ecurb65_ldh) &
          # is.na(ecurb65_alb) &
          is.na(ecurb65_plts), NA_real_, ecurb65))
    } else {
      .
    }}
  
  if(output == "vector"){
    out %>% 
      pull(ecurb65)
  } else if(output == "components"){
    out %>% 
      select(starts_with("ecurb65"))
  } else if(output == "df_vector"){
    out %>% 
      pull(ecurb65) %>% 
      bind_cols(.data, ecurb65 = .)
  } else if(output == "df_components"){
    out
  }
}


# dscrb65 -----------------------------------------------------
dscrb65 <- function(.data, rr, sbp, dbp, gcs, age,
                    spo2, card_comrb, renal_comrb, cancer_comrb, liver_comrb, 
                    output = c("vector", "components", "df_vector", "df_components"), 
                    na_to_zeros = TRUE, all_na_rm = TRUE){
  
  .rr = enquo(rr)
  .sbp = enquo(sbp)
  .dbp = enquo(dbp)
  .gcs = enquo(gcs)
  .age = enquo(age)
  .spo2 = enquo(spo2)
  .card_comrb = enquo(card_comrb)
  .renal_comrb = enquo(renal_comrb)
  .cancer_comrb = enquo(cancer_comrb)
  .liver_comrb = enquo(liver_comrb)
  
  out = .data %>% 
    dplyr::mutate_at(vars(!! .rr, !! .sbp, !! .dbp, !! .gcs, !! .age, !! .spo2), as.numeric) %>% 
    dplyr::mutate_at(vars(!! .card_comrb, !! .renal_comrb, !! .cancer_comrb, !! .liver_comrb), 
                     ~ as.character(.) %>% tolower() %>% trimws()) %>% 
    
    mutate(
      # dscrb65 score
      dscrb65_rr = case_when(
        !! .rr >=  30 ~ 1, 
        !! .rr < 30 ~ 0,
        TRUE ~ NA_real_),
      
      dscrb65_bp = case_when(
        !! .sbp <  90 |
          !! .dbp <= 60 ~ 1, 
        is.na(!! .sbp) & is.na(!! .dbp) ~ NA_real_,
        TRUE ~ 0),
      
      dscrb65_gcs = case_when(
        !! .gcs <=  14 ~ 1, 
        !! .gcs > 14 ~ 0,
        TRUE ~ NA_real_),
      
      dscrb65_age = case_when(
        !! .age >= 65 ~ 1, 
        !! .age < 65 ~ 0,
        TRUE ~ NA_real_),
      
      dscrb65_spo2 = case_when(
        !! .spo2 < 90 ~ 1, 
        !! .spo2 >= 90 ~ 0,
        TRUE ~ NA_real_),
      
      dscrb65_comrb = case_when(
        !! .card_comrb == "yes" ~ 1,
        !! .renal_comrb == "yes" ~ 1,
        !! .cancer_comrb == "yes" ~ 1,
        !! .liver_comrb == "yes" ~ 1,
        is.na(!! .card_comrb) &
          is.na(!! .renal_comrb) &
          is.na(!! .cancer_comrb) &
          is.na(!! .liver_comrb) ~ NA_real_,
        TRUE ~ 0)
      
    ) %>% 
    mutate(
      dscrb65 = rowSums(dplyr::select(., dplyr::starts_with("dscrb65_")),
                        na.rm = na_to_zeros)
    ) %>% 
    
    {if(all_na_rm){
      dplyr::mutate(., dscrb65 = dplyr::if_else(
        is.na(dscrb65_rr) &
          is.na(dscrb65_bp) &
          is.na(dscrb65_gcs) &
          is.na(dscrb65_age) &
          is.na(dscrb65_spo2) &
          is.na(dscrb65_comrb), NA_real_, dscrb65))
    } else {
      .
    }}
  
  if(output == "vector"){
    out %>% 
      pull(dscrb65)
  } else if(output == "components"){
    out %>% 
      select(starts_with("dscrb65"))
  } else if(output == "df_vector"){
    out %>% 
      pull(dscrb65) %>% 
      bind_cols(.data, dscrb65 = .)
  } else if(output == "df_components"){
    out
  }
}


# scap / curbxo80 -----------------------------------------------------
scap <- function(.data, rr, sbp, gcs, age,
                 po2, fio2, bun, ph, infiltrates, 
                    output = c("vector", "components", "df_vector", "df_components"), 
                    na_to_zeros = TRUE, all_na_rm = TRUE){
  
  .rr = enquo(rr)
  .sbp = enquo(sbp)
  .gcs = enquo(gcs)
  .age = enquo(age)
  .po2 = enquo(po2)
  .fio2 = enquo(fio2)
  .bun = enquo(bun)
  .ph = enquo(ph)
  .infiltrates = enquo(infiltrates)
  
  out = .data %>% 
    dplyr::mutate_at(vars(!! .rr, !! .sbp, !! .gcs, !! .age, !! .po2, !! .fio2, !! .bun, !! .ph), as.numeric) %>% 
    dplyr::mutate_at(vars(!! .infiltrates), ~ as.character(.) %>% tolower() %>% trimws()) %>% 
    
    mutate(
      # scap score
      scap_rr = case_when(
        !! .rr >  30 ~ 9, 
        !! .rr <= 30 ~ 0,
        TRUE ~ NA_real_),
      
      scap_bp = case_when(
        !! .sbp <  90 ~ 11,
        !! .sbp >= 90 ~ 0,
        TRUE ~ NA_real_),
      
      scap_gcs = case_when(
        !! .gcs <= 14 ~ 5, 
        !! .gcs >  14 ~ 0,
        TRUE ~ NA_real_),
      
      scap_age = case_when(
        !! .age >= 80 ~ 5, 
        !! .age < 80 ~ 0,
        TRUE ~ NA_real_),
      
      # PaO2/FiO2 ratio
      .pf_ratio = (!! .po2*7.5)/!! .fio2,
      
      # Respiration SCAP score
      scap_respi = case_when(
        .pf_ratio < 250 ~ 6,
        (!! .po2*7.5) < 54 ~ 6,
        .pf_ratio >= 250 ~ 0,
        (!! .po2*7.5) >= 54 ~ 0,
        TRUE ~ NA_real_),
      
      scap_bun = case_when(
        !! .bun > 10.7 ~ 5, 
        !! .bun <= 10.7 ~ 0,
        TRUE ~ NA_real_),
      
      scap_ph = case_when(
        !! .ph < 7.3 ~ 13, 
        !! .bun >= 7.3 ~ 0,
        TRUE ~ NA_real_),
      
      scap_infiltrates = case_when(
        !! .infiltrates == "yes" ~ 5, 
        !! .infiltrates == "no" ~ 0,
        TRUE ~ NA_real_)

    ) %>% 
    mutate(
      scap = rowSums(dplyr::select(., dplyr::starts_with("scap_")),
                        na.rm = na_to_zeros)
    ) %>% 
    
    {if(all_na_rm){
      dplyr::mutate(., scap = dplyr::if_else(
        is.na(scap_rr) &
          is.na(scap_bp) &
          is.na(scap_gcs) &
          is.na(scap_age) &
          is.na(scap_respi) &
          is.na(scap_bun) &
          is.na(scap_ph) &
          is.na(scap_infiltrates), NA_real_, scap))
    } else {
      .
    }}
  
  if(output == "vector"){
    out %>% 
      pull(scap)
  } else if(output == "components"){
    out %>% 
      select(starts_with("scap"))
  } else if(output == "df_vector"){
    out %>% 
      pull(scap) %>% 
      bind_cols(.data, scap = .)
  } else if(output == "df_components"){
    out
  }
}


# Tongji score -------------------------------------------------------------------------------------------
tongji <- function(.data, age, ldh, lymph, spo2,
                        output = c("vector", "components", "df_vector", "df_components")){
  
  .age = enquo(age)
  .ldh = enquo(ldh)
  .lymph = enquo(lymph)
  .spo2 = enquo(spo2)
  
  out = .data %>% 
    dplyr::mutate_at(vars(!! .age, !! .ldh, !! .lymph, !! .spo2), as.numeric) %>%
    
    mutate(
      # tongji
      tongji_age = !! .age * 0.047,
      tongji_ldh = !! .ldh * 0.003, 
      tongji_lymph = log(!! .lymph) * -1.094,
      tongji_spo2 = !! .spo2 * -0.098, 
      tongji_link = 4.559 + tongji_age + tongji_ldh + tongji_lymph + tongji_spo2,
      tongji = boot::inv.logit((tongji_link))
    )
  
  if(output == "vector"){
    out %>% 
      pull(tongji)
  } else if(output == "components"){
    out %>% 
      select(starts_with("tongji"))
  } else if(output == "df_vector"){
    out %>% 
      pull(tongji) %>% 
      bind_cols(.data, tongji = .)
  } else if(output == "df_components"){
    out
  }
}

# Example from paper
# beta = c(4.559, 0.047, 0.003, -1.094, -0.098)
# x = c(1, 59, 482, log(0.64), 85)
# boot::inv.logit(beta %*% x)
# 
# tmp = tibble(
#   age = 59,
#   ldh = 482,
#   lymph = 0.64,
#   spo2 = 85)
# 
# tmp %>%
#   tongji(age = age, ldh = ldh, lymph = lymph, spo2 = spo2)


# adrop -----------------------------------------------------
adrop <- function(.data, spo2, sbp, gcs, bun, age, sex,
                   output = c("vector", "components", "df_vector", "df_components"), 
                   na_to_zeros = TRUE, all_na_rm = TRUE){
  
  .spo2 = enquo(spo2)
  .sbp = enquo(sbp)
  .gcs = enquo(gcs)
  .bun = enquo(bun)
  .age = enquo(age)
  .sex = enquo(sex)
  
  out = .data %>% 
    dplyr::mutate_at(vars(!! .spo2, !! .sbp, !! .gcs, !! .bun, !! .age), as.numeric) %>% 
    dplyr::mutate_at(vars(!! .sex),
                     ~ as.character(.) %>% tolower() %>% trimws()) %>%
    
    mutate(
      # adrop score
      adrop_spo2 = case_when(
        !! .spo2 <=  90 ~ 1, 
        !! .spo2 > 90 ~ 0,
        TRUE ~ NA_real_),
      
      adrop_sbp = case_when(
        !! .sbp <  90  ~ 1, 
        !! .sbp >=  90  ~ 0, 
        TRUE ~ NA_real_),
      
      adrop_gcs = case_when(
        !! .gcs <=  14 ~ 1, 
        !! .gcs > 14 ~ 0,
        TRUE ~ NA_real_),
      
      adrop_bun = case_when(
        !! .bun >=  7.5 ~ 1, 
        !! .bun < 7.5 ~ 0,
        TRUE ~ NA_real_),
      
      adrop_age = case_when(
        !! .sex == "male" & !! .age >= 70 ~ 1, 
        !! .sex == "female" & !! .age >= 75 ~ 1,
        is.na(!! .sex) | is.na(!! .age) ~ NA_real_,
        TRUE ~ 0),
      
    ) %>% 
    mutate(
      adrop = rowSums(dplyr::select(., dplyr::starts_with("adrop_")),
                       na.rm = na_to_zeros)
    ) %>% 
    
    {if(all_na_rm){
      dplyr::mutate(., adrop = dplyr::if_else(
        is.na(adrop_spo2) &
          is.na(adrop_sbp) &
          is.na(adrop_gcs) &
          is.na(adrop_bun) &
          is.na(adrop_age), NA_real_, adrop))
    } else {
      .
    }}
  
  if(output == "vector"){
    out %>% 
      pull(adrop)
  } else if(output == "components"){
    out %>% 
      select(starts_with("adrop"))
  } else if(output == "df_vector"){
    out %>% 
      pull(adrop) %>% 
      bind_cols(.data, adrop = .)
  } else if(output == "df_components"){
    out
  }
}


# psi -----------------------------------------------------
psi <- function(.data, rr, sbp, gcs, temp, hr, 
                age, sex, card_comrb, renal_comrb, cancer_comrb, liver_comrb,
                ph, po2, spo2, bun, na, glu, haematocrit, infiltrates,
                output = c("vector", "components", "df_vector", "df_components"), 
                na_to_zeros = TRUE, all_na_rm = TRUE){
  
  .rr = enquo(rr)
  .sbp = enquo(sbp)
  .gcs = enquo(gcs)
  .temp = enquo(temp)
  .hr = enquo(hr)
  
  .age = enquo(age)
  .sex = enquo(sex)
  .card_comrb = enquo(card_comrb)
  .renal_comrb = enquo(renal_comrb)
  .cancer_comrb = enquo(cancer_comrb)
  .liver_comrb = enquo(liver_comrb) 

  .ph = enquo(ph)
  .po2 = enquo(po2)
  .spo2 = enquo(spo2)
  .bun = enquo(bun)
  .na= enquo(na)
  .glu = enquo(glu)
  .haematocrit = enquo(haematocrit)
  
  .infiltrates = enquo(infiltrates)
  
  out = .data %>% 
    dplyr::mutate_at(vars(!! .rr, !! .sbp, !! .gcs, !! .temp, !! .hr, !! .age, !! .ph, 
                          !! .po2, !! .spo2, !! .bun, !! .na, !! .glu, !! .haematocrit), as.numeric) %>% 
    dplyr::mutate_at(vars(!! .sex, !! .card_comrb, !! .renal_comrb, !! .cancer_comrb, !! .liver_comrb, 
                          !! .infiltrates), ~ as.character(.) %>% tolower() %>% trimws()) %>% 
    
    mutate(
      # psi score
      psi_age = case_when(
        !! .sex == "male" ~ age,
        !! .sex == "female" ~ age -10,
        is.na(!! .sex) ~ NA_real_,
        TRUE ~ NA_real_),
      
      # Nursing home resisdent not included
      
      psi_cancer = case_when(
        !!  .cancer_comrb == "yes" ~ 30,
        !!  .cancer_comrb == "no" ~ 0,
        TRUE ~ NA_real_),
      
      psi_liver = case_when(
        !! .liver_comrb == "yes" ~ 20, 
        !! .liver_comrb == "no" ~ 0,
        TRUE ~ NA_real_),
      
      psi_card = case_when(
        !! .card_comrb == "yes" ~ 10, 
        !! .card_comrb == "no" ~ 0,
        TRUE ~ NA_real_),
      
      # CVA excluded as don't have
      
      psi_renal = case_when(
        !! .renal_comrb == "yes" ~ 10, 
        !! .renal_comrb == "no" ~ 0,
        TRUE ~ NA_real_),
      
      psi_gcs = case_when(
        !! .gcs <=  14 ~ 20, 
        !! .gcs > 14 ~ 0,
        TRUE ~ NA_real_),      
      
      psi_rr = case_when(
        !! .rr >  30 ~ 20, 
        !! .rr <= 30 ~ 0,
        TRUE ~ NA_real_),
      
      psi_sbp = case_when(
        !! .sbp <  90 ~ 20, 
        !! .sbp >= 90 ~ 0,
        TRUE ~ NA_real_),
      
      psi_temp = case_when(
        !! .temp < 35 ~ 15, 
        !! .temp >=40 ~ 15,
        is.na(!! .temp) ~ NA_real_,
        TRUE ~ 0),
      
      psi_hr = case_when(
        !! .hr > 125 ~ 10, 
        !! .hr <= 125 ~ 0,
        TRUE ~ NA_real_),
      
      psi_ph = case_when(
        !! .ph < 7.35 ~ 30, 
        !! .ph >= 7.35 ~ 0,
        TRUE ~ NA_real_),

      psi_spo2 = case_when(
        !! .spo2 >=  90 ~ 1,
        !! .spo2 < 90 ~ 0,
        TRUE ~ NA_real_),
      
      psi_bun = case_when(
        !! .bun > 10.7 ~ 20, 
        !! .bun <= 10.7 ~ 0,
        TRUE ~ NA_real_),

      psi_na = case_when(
        !! .na < 130 ~ 20, 
        !! .na >= 130 ~ 0,
        TRUE ~ NA_real_),
      
      psi_glu = case_when(
        !! .glu > 13.9 ~ 10, 
        !! .glu <= 13.9 ~ 0,
        TRUE ~ NA_real_),
      
      psi_haematocrit = case_when(
        !! .haematocrit < 30 ~ 10, 
        !! .haematocrit >= 30 ~ 0,
        TRUE ~ NA_real_),
      
      psi_po2 = case_when(
        !! .po2 < 8 | 
          !! .spo2 < 90 ~ 10,
        !! .po2 >= 8 | 
          !! .spo2 >= 90 ~ 0,
        TRUE ~ NA_real_),
      
      psi_infiltrates = case_when(
        !! .infiltrates == "yes" ~ 10, 
        !! .infiltrates == "no" ~ 0,
        TRUE ~ NA_real_)
      
    ) %>% 
    mutate(
      psi = rowSums(dplyr::select(., dplyr::starts_with("psi_")),
                    na.rm = na_to_zeros)
    ) %>% 
    
    {if(all_na_rm){
      dplyr::mutate(., psi = dplyr::if_else(
        is.na(psi_age) &
          is.na(psi_cancer) &
          is.na(psi_liver) &
          is.na(psi_card) &
          is.na(psi_renal) &
          is.na(psi_gcs) &
          is.na(psi_rr) &
          is.na(psi_sbp) &
          is.na(psi_temp) &
          is.na(psi_hr) &
          is.na(psi_ph) &
          is.na(psi_spo2) & 
          is.na(psi_bun) &
          is.na(psi_na) &
          is.na(psi_glu) &
          is.na(psi_haematocrit) &
          is.na(psi_po2) &
          is.na(psi_infiltrates), NA_real_, psi))
    } else {
      .
    }}
  
  if(output == "vector"){
    out %>% 
      pull(psi)
  } else if(output == "components"){
    out %>% 
      select(starts_with("psi"))
  } else if(output == "df_vector"){
    out %>% 
      pull(psi) %>% 
      bind_cols(.data, psi = .)
  } else if(output == "df_components"){
    out
  }
}


# flucp -----------------------------------------------------
flucp <- function(.data, rr, sbp, spo2, gcs, infiltrates,
                output = c("vector", "components", "df_vector", "df_components"), 
                na_to_zeros = TRUE, all_na_rm = TRUE){
  
  .rr = enquo(rr)
  .sbp = enquo(sbp)
  .spo2 = enquo(spo2)
  .gcs = enquo(gcs)
  .infiltrates = enquo(infiltrates)
  
  out = .data %>% 
    dplyr::mutate_at(vars(!! .rr, !! .sbp, !! .spo2, !! .gcs), as.numeric) %>% 
    dplyr::mutate_at(vars(!! .infiltrates), ~ as.character(.) %>% tolower() %>% trimws()) %>% 
    
    mutate(
      flucp_rr = case_when(
        !! .rr >=  30 ~ 1, 
        !! .rr < 30 ~ 0,
        TRUE ~ NA_real_),
      
      flucp_sbp = case_when(
        !! .sbp <  90 ~ 1, 
        !! .sbp >= 90 ~ 0,
        TRUE ~ NA_real_),
      
      flucp_spo2 = case_when(
        !! .spo2 <=  90 ~ 1, 
        !! .spo2 > 90 ~ 0,
        TRUE ~ NA_real_),
      
      flucp_gcs = case_when(
        !! .gcs <=  14 ~ 1, 
        !! .gcs > 14 ~ 0,
        TRUE ~ NA_real_),      
      
      flucp_infiltrates = case_when(
        !! .infiltrates == "yes" ~ 1, 
        !! .infiltrates == "no" ~ 0,
        TRUE ~ NA_real_)
      
    ) %>% 
    mutate(
      flucp = rowSums(dplyr::select(., dplyr::starts_with("flucp_")),
                      na.rm = na_to_zeros)
    ) %>% 
    
    {if(all_na_rm){
      dplyr::mutate(., flucp = dplyr::if_else(
        is.na(flucp_rr) &
          is.na(flucp_sbp) &
          is.na(flucp_gcs) &
          is.na(flucp_spo2) & 
          is.na(flucp_infiltrates), NA_real_, flucp))
    } else {
      .
    }}
  
  if(output == "vector"){
    out %>% 
      pull(flucp)
  } else if(output == "components"){
    out %>% 
      select(starts_with("flucp"))
  } else if(output == "df_vector"){
    out %>% 
      pull(flucp) %>% 
      bind_cols(.data, flucp = .)
  } else if(output == "df_components"){
    out
  }
}


# Smart-cop -----------------------------------------------------
smartcop <- function(.data, age, rr, hr, gcs, po2, spo2, fio2, ph, sbp, infiltrates, #alb,
                  output = c("vector", "components", "df_vector", "df_components"), 
                  na_to_zeros = TRUE, all_na_rm = TRUE){
  
  .age = enquo(age)
  # .alb = enquo(alb)
  .rr = enquo(rr)
  .hr = enquo(hr)
  .gcs = enquo(gcs)
  .po2 = enquo(po2)
  .spo2 = enquo(spo2)
  .fio2 = enquo(fio2)
  .ph = enquo(ph)
  .sbp = enquo(sbp)
  .infiltrates = enquo(infiltrates)
  
  out = .data %>% 
    dplyr::mutate_at(vars(!! .age, !! .rr, !! .hr, !! .gcs, !! .po2, !! .spo2, !! .fio2, !! .ph, !! .sbp), as.numeric) %>%
    dplyr::mutate_at(vars(!! .infiltrates), ~ as.character(.) %>% tolower() %>% trimws()) %>% 
    
    mutate(
      # Age included as 0 to carry over for later
      smartcop_age = case_when(
        !! .age <= 1 ~ 1,
        !! .age > 1 ~ 0, 
        TRUE ~ NA_real_),
      
      smartcop_infiltrates = case_when(
        !! .infiltrates == "yes" ~ 1, 
        !! .infiltrates == "no" ~ 0,
        TRUE ~ NA_real_),
      
      # smartcop_alb = case_when(
      #   !! .alb <  35 ~ 1, 
      #   !! .alb >= 35 ~ 0,
      #   TRUE ~ NA_real_),
      
      # Multilobar involvement on CXR not available in dataset
      
      smartcop_rr = case_when(
        !! .rr >=  30 & !! .age > 50 ~ 1, 
        !! .rr >=  25 & !! .age <= 50 ~ 1, 
        !! .rr <  30 & !! .age > 50 ~ 0,
        !! .rr <  25 & !! .age <= 50 ~ 0,
        TRUE ~ NA_real_),
      
      smartcop_hr = case_when(
        !! .hr >=  125 ~ 1, 
        !! .hr < 125 ~ 0,
        TRUE ~ NA_real_),
      
      smartcop_gcs = case_when(
        !! .gcs <=  14 ~ 1, 
        !! .gcs > 14 ~ 0,
        TRUE ~ NA_real_),  
      
      # po2 included as 0 to carry over for later
      smartcop_po2 = case_when(
        !! .po2 <= 1 ~ 1,
        !! .po2 > 1 ~ 0, 
        TRUE ~ NA_real_),
      
       # Spo2 included as 0 to carry over for later
      smartcop_spo2 = case_when(
        !! .spo2 < 0 ~ 1,
        !! .spo2 >= 0 ~ 0, 
        TRUE ~ NA_real_),
      
      # fio2 included as 0 to carry over for later
      smartcop_fio2 = case_when(
        !! .fio2 < 0 ~ 1,
        !! .fio2 >= 0 ~ 0, 
        TRUE ~ NA_real_),
      
      # PaO2/FiO2 ratio
      .pf_ratio = (!! .po2*7.5)/!! .fio2,
      
      # Smartcop_respi parameters: # age <=50: PaO₂ <70 mmHg, SaO₂ ≤93%, or PaO₂/FiO₂ <333; age >50 PaO₂ <60 mmHg, SaO₂ ≤90%, or PaO₂/FiO₂ <250
      # po2 converted to kPa (/7.5)
      smartcop_respi = case_when(
        !! .age > 50 & (!! .po2 < 8 | !! .spo2 <= 90 | .pf_ratio < 250) ~ 2,   # age > 50
        !! .age <= 50 & (!! .po2 < 9.333 | !! .spo2 <= 93 | .pf_ratio < 333) ~ 2,  # age <= 50
        !! .age > 50 & (!! .po2 > 8 | !! .spo2 > 90 | .pf_ratio >= 250) ~ 0,   # age > 50 
        !! .age <= 50 & (!! .po2 > 9.333 | !! .spo2 > 93 | .pf_ratio >= 333) ~ 0,  # age <= 50
        TRUE ~ NA_real_),
      
      smartcop_ph = case_when(
        !! .ph <  7.35 ~ 2, 
        !! .ph >= 7.35 ~ 0,
        TRUE ~ NA_real_),
      
      smartcop_sbp = case_when(
        !! .sbp <  90 ~ 2, 
        !! .sbp >= 90 ~ 0,
        TRUE ~ NA_real_)
      
    ) %>% 
    mutate(
      smartcop = rowSums(dplyr::select(., dplyr::starts_with("smartcop_")),
                      na.rm = na_to_zeros)
    ) %>% 
    
    {if(all_na_rm){
      dplyr::mutate(., smartcop = dplyr::if_else(
        is.na(smartcop_age) &
          is.na(smartcop_infiltrates) &
          # is.na(smartcop_alb) &
          is.na(smartcop_rr) &
          is.na(smartcop_hr) &
          is.na(smartcop_gcs) &
          is.na(smartcop_spo2) & 
          is.na(smartcop_po2) &
          is.na(smartcop_fio2) &
          is.na(smartcop_ph) &
          is.na(smartcop_respi) &
          is.na(smartcop_sbp), NA_real_, smartcop))
    } else {
      .
    }}
  
  if(output == "vector"){
    out %>% 
      pull(smartcop)
  } else if(output == "components"){
    out %>% 
      select(starts_with("smartcop"))
  } else if(output == "df_vector"){
    out %>% 
      pull(smartcop) %>% 
      bind_cols(.data, smartcop = .)
  } else if(output == "df_components"){
    out
  }
}



# DL score -------------------------------------------------------------------------------------------
DLscore <- function(.data, age, sex, neutro, lymph, plts, crp, creat,
                   output = c("vector", "components", "df_vector", "df_components")){
  
  .age = enquo(age)
  .sex = enquo(sex)
  .neutro = enquo(neutro)
  .lymph = enquo(lymph)
  .plts = enquo(plts)
  .crp = enquo(crp)
  .creat = enquo(creat)
  
  out = .data %>% 
    dplyr::mutate_at(vars(!! .age, !! .neutro, !! .lymph, !! .plts, !! .crp, !! .creat), as.numeric) %>%
    dplyr::mutate_at(vars(!! .sex), ~ as.character(.) %>% tolower() %>% trimws()) %>%
    
    mutate(
      # DLscore
      DLscore_age = !! .age * 0.0142,
      DLscore_neutro = !! .neutro * 0.2249,   
      DLscore_lymph = !! .lymph * -1.3255,
      DLscore_plts = !! .plts * -0.0052, 
      DLscore_crp = !! .crp * 0.0138, 
      DLscore_creat = !! .creat * 0.0015, 
  
      DLscore_sex = case_when(
                  !! .sex == "male" ~ -0.8035, 
                  is.na(!! .sex) ~ NA_real_,
                  TRUE ~ 0),
  
      DLscore_link = -3.5172 + DLscore_age + DLscore_sex + DLscore_neutro + DLscore_lymph + DLscore_plts + DLscore_crp + DLscore_creat,
      DLscore = boot::inv.logit((DLscore_link))
    )
  
  if(output == "vector"){
    out %>% 
      pull(DLscore)
  } else if(output == "components"){
    out %>% 
      select(starts_with("DLscore"))
  } else if(output == "df_vector"){
    out %>% 
      pull(DLscore) %>% 
      bind_cols(.data, DLscore = .)
  } else if(output == "df_components"){
    out
  }
}



# DCSL score -------------------------------------------------------------------------------------------
DCSLscore <- function(.data, age, sex, neutro, lymph, plts, crp, creat,
                    resp_comrb, diabetes_mhym, diabetescom_mhyn, cancer_comrb, cough, dyspnoe,
                    output = c("vector", "components", "df_vector", "df_components")){
  
  .age = enquo(age)
  .sex = enquo(sex)
  .neutro = enquo(neutro)
  .lymph = enquo(lymph)
  .plts = enquo(plts)
  .crp = enquo(crp)
  .creat = enquo(creat)
  .resp_comrb = enquo(resp_comrb)
  .diabetes_mhym = enquo(diabetes_mhym)
  .diabetescom_mhyn = enquo(diabetescom_mhyn)
  .cancer_comrb = enquo(cancer_comrb)
  .cough = enquo(cough)
  .dyspnoe = enquo(dyspnoe)
  
  out = .data %>% 
    dplyr::mutate_at(vars(!! .age, !! .neutro, !! .lymph, !! .plts, !! .crp, !! .creat), as.numeric) %>%
    dplyr::mutate_at(vars(!! .sex, !! .resp_comrb, !! .diabetes_mhym, !! .diabetescom_mhyn, !! .cancer_comrb, 
                          !! .cough, !! .dyspnoe), ~ as.character(.) %>% tolower() %>% trimws()) %>%
    
    mutate(
      # DLscore
      DCSLscore_age = !! .age * -0.0020,
      DCSLscore_neutro = !! .neutro * 0.1793,   
      DCSLscore_lymph = !! .lymph * -1.4449,
      DCSLscore_plts = !! .plts * -0.0059, 
      DCSLscore_crp = !! .crp * 0.0140, 
      DCSLscore_creat = !! .creat * 0.0012, 
      
      DCSLscore_sex = case_when(
        !! .sex == "male" ~ -0.8563, 
        is.na(!! .sex) ~ NA_real_,
        TRUE ~ 0),
      
      DCSLscore_resp_comrb = case_when(
        !! .resp_comrb == "yes" ~ 0.4054, 
        !! .resp_comrb == "no" ~ 0, 
        TRUE ~ NA_real_),
      
      DCSLscore_diabetes = case_when(
        !! .diabetes_mhym == "yes" ~ 0.3168, 
        !! .diabetescom_mhyn == "yes" ~ 0.3168,
        !! .diabetes_mhym == "no" ~ 0, 
        !! .diabetescom_mhyn == "no" ~ 0, 
        TRUE ~ NA_real_),
      
      DCSLscore_cancer_comrb = case_when(
        !! .cancer_comrb == "yes" ~ -0.0019, 
        !! .cancer_comrb == "no" ~ 0, 
        TRUE ~ NA_real_),
      
      DCSLscore_cough = case_when(
        !! .cough == "yes" ~ 0.9446, 
        !! .cough == "no" ~ 0, 
        TRUE ~ NA_real_),
      
      DCSLscore_dyspnoe = case_when(
        !! .dyspnoe == "yes" ~ 0.8559, 
        !! .dyspnoe == "no" ~ 0, 
        TRUE ~ NA_real_),
      
      DCSLscore_link = -3.2048 + DCSLscore_age + DCSLscore_sex + DCSLscore_neutro + DCSLscore_lymph + 
        DCSLscore_plts + DCSLscore_crp + DCSLscore_creat + DCSLscore_resp_comrb + DCSLscore_diabetes +
        DCSLscore_cancer_comrb + DCSLscore_cough + DCSLscore_dyspnoe,
        DCSLscore = boot::inv.logit((DCSLscore_link))
    )
  
  if(output == "vector"){
    out %>% 
      pull(DCSLscore)
  } else if(output == "components"){
    out %>% 
      select(starts_with("DCSLscore"))
  } else if(output == "df_vector"){
    out %>% 
      pull(DCSLscore) %>% 
      bind_cols(.data, DCSLscore = .)
  } else if(output == "df_components"){
    out
  }
}




# Surgisphere score -----------------------------------------------------------------------------
surgisphere <- function(.data, comorbidSS,
                        mobility,
                        rr, spo2, dyspnoe, gcs, 
                        temp, hr, sbp,
                        output = c("vector", "components", "df_vector", "df_components"), 
                        na_to_zeros = TRUE, all_na_rm = TRUE){
  
  .comorbidSS = enquo(comorbidSS)
  .mobility = enquo(mobility)
  .rr = enquo(rr)
  .spo2 = enquo(spo2)
  .dyspnoe = enquo(dyspnoe)
  .gcs = enquo(gcs)
  .temp = enquo(temp)
  .hr = enquo(hr)
  .sbp = enquo(sbp)
  
  out = .data %>% 
    dplyr::mutate_at(vars(!! .comorbidSS, !! .rr, !! .spo2, !! .gcs, !! .temp, 
                          !! .hr, !! .sbp,), as.numeric) %>% 
    dplyr::mutate_at(vars(!! .mobility, 
                          !! .dyspnoe), ~ as.character(.) %>% tolower() %>% trimws()) %>% 
    
    mutate(
      # surgisphere score
      surgisphere_comorbidSS = case_when(
        !! .comorbidSS == "1" ~ 2,
        !! .comorbidSS == "0" ~ 0,
        TRUE ~ NA_real_),
      
      # Imputed based on GCS score
      surgisphere_mobility = case_when(
        !! .gcs > 14 ~ 0,
        !! .gcs > 12 ~ 1,
        !! .gcs <= 12 ~ 2,
        TRUE ~ NA_real_
      ),
      
      surgisphere_assessment = case_when(
        !! .dyspnoe == "yes" | !! .spo2 <= 95 | !! .gcs <= 8  ~ 1,
        !! .dyspnoe == "no" & !! .spo2 > 95 & !! .gcs > 8 ~ 0,
        TRUE ~ NA_real_
      ),
      
      # RR <= 9 +2, 20-27 +2, =>28 +4     
      surgisphere_rr = case_when(
        !! .rr < 10 ~ 2,
        !! .rr < 20 ~ 0,
        !! .rr < 28 ~ 2,
        !! .rr >= 28 ~ 4,
        TRUE ~ NA_real_),
      
      # Temperature <=35 +2, =>38.5 +3
      surgisphere_temp = case_when(
        !! .temp <= 35 ~ 2,
        !! .temp < 38.5 ~ 0,
        !! .temp >= 38.5 ~ 3,
        TRUE ~ NA_real_
      ),
      
      # Pulse <=45 2, => 110 3 
      surgisphere_hr = case_when(
        !! .hr <= 45 ~ 2,
        !! .hr < 110 ~ 0,
        !! .hr >= 110 ~ 3,
        TRUE ~ NA_real_
      ),
      
      # SBP <=90 +4 , SBP =>160 +2
      surgisphere_sbp = case_when(
        !! .sbp <= 90 ~ 4,
        !! .sbp < 160 ~ 0,
        !! .sbp >= 160 ~ 2,
        TRUE ~ NA_real_),
      
    ) %>% 
    mutate(
      surgisphere = rowSums(dplyr::select(., dplyr::starts_with("surgisphere_")),
                            na.rm = na_to_zeros)
    ) %>% 
    
    {if(all_na_rm){
      dplyr::mutate(., surgisphere = dplyr::if_else(
        is.na(surgisphere_comorbidSS) &
          is.na(surgisphere_mobility) &
          is.na(surgisphere_rr) &
          is.na(surgisphere_assessment) &
          is.na(surgisphere_temp) &
          is.na(surgisphere_hr) &
          is.na(surgisphere_sbp), NA_real_, surgisphere))
    } else {
      .
    }}
  
  if(output == "vector"){
    out %>% 
      pull(surgisphere)
  } else if(output == "components"){
    out %>% 
      select(starts_with("surgisphere"))
  } else if(output == "df_vector"){
    out %>% 
      pull(surgisphere) %>% 
      bind_cols(.data, surgisphere = .)
  } else if(output == "df_components"){
    out
  }
}



# Covid_gram score ------
covid_gram <- function(.data, age, infiltrates, haemoptysis, dyspnoea, gcs,
                       comorbid_gram, malignancy, nlr, ldh, bil,
                       output = c("vector", "components", "df_vector", "df_components")){
  
  .age = enquo(age)
  .infiltrates = enquo(infiltrates)
  .haemoptysis = enquo(haemoptysis)
  .dyspnoea = enquo(dyspnoea)
  .gcs = enquo(gcs)
  .comorbid_gram = enquo(comorbid_gram)
  .malignancy = enquo(malignancy)
  .nlr = enquo(nlr)
  .ldh = enquo(ldh)
  .bil = enquo(bil)
  
  out = .data %>% 
    dplyr::mutate_at(vars(!! .age, !! .gcs, !! .comorbid_gram, !! .nlr, !! .ldh, !! .bil), as.numeric) %>%
    dplyr::mutate_at(vars(!! .infiltrates, !! .haemoptysis, !! .dyspnoea,
                          !! .malignancy), ~ as.character(.) %>% tolower() %>% trimws()) %>%
    
    mutate(
      # covid_gram
      covid_gram_age = !! .age * 0.0295588,
      covid_gram_comorbid_gram = !! .comorbid_gram * 0.4700036, 
      covid_gram_nlr = !! .nlr * 0.05826891, 
      covid_gram_ldh = !! .ldh * 0.001998003,
      covid_gram_bil = !! .bil * 0.1397619,
      
      covid_gram_infiltrates = case_when(
        !! .infiltrates == "Yes" ~ 1.22083, 
        is.na(!! .infiltrates) ~ NA_real_,
        TRUE ~ 0),
      
      covid_gram_haemoptysis = case_when(
        !! .haemoptysis == "YES" ~ 1.510722, 
        is.na(!! .haemoptysis) ~ NA_real_,
        TRUE ~ 0),
      
      covid_gram_dyspnoea = case_when(
        !! .dyspnoea == "YES" ~ 0.6312718, 
        is.na(!! .dyspnoea) ~ NA_real_,
        TRUE ~ 0),
      
      covid_gram_gcs = case_when(
        !! .gcs <= "8" ~ 1.549688, 
        is.na(!! .gcs) ~ NA_real_,
        TRUE ~ 0),
      
      covid_gram_malignancy = case_when(
        !! .malignancy == "Yes" ~ 1.403643, 
        is.na(!! .malignancy) ~ NA_real_,
        TRUE ~ 0),
      
      
      covid_gram_link = -6.907755 + covid_gram_age + covid_gram_infiltrates + 
        covid_gram_haemoptysis + covid_gram_dyspnoea + covid_gram_gcs + 
        covid_gram_comorbid_gram + covid_gram_malignancy + covid_gram_nlr + 
        covid_gram_ldh + covid_gram_bil,
      covid_gram = boot::inv.logit((covid_gram_link))
    )
  
  if(output == "vector"){
    out %>% 
      pull(covid_gram)
  } else if(output == "components"){
    out %>% 
      select(starts_with("covid_gram"))
  } else if(output == "df_vector"){
    out %>% 
      pull(covid_gram) %>% 
      bind_cols(.data, covid_gram = .)
  } else if(output == "df_components"){
    out
  }
}



# Xie score --------
xie_score <- function(.data, age, ldh, lymph, sats,
                      output = c("vector", "components", "df_vector", "df_components")){
  
  .age = enquo(age)
  .ldh = enquo(ldh)
  .lymph = enquo(lymph)
  .sats = enquo(sats)
  
  out = .data %>% 
    dplyr::mutate_at(vars(!! .age, !! .ldh, !! .lymph, !! .sats), as.numeric) %>%
    
    mutate(
      # xie_score
      xie_score_age = !! .age * 0.047,
      xie_score_ldh = !! .ldh * 0.003,
      xie_score_lymph = !! .lymph * -1.094,
      xie_score_sats = !! .sats * -0.098, 
      
      xie_score_link = 4.559 + xie_score_age + xie_score_ldh + xie_score_lymph + xie_score_sats,
      xie_score = boot::inv.logit((xie_score_link))
    )
  
  if(output == "vector"){
    out %>% 
      pull(xie_score)
  } else if(output == "components"){
    out %>% 
      select(starts_with("xie_score"))
  } else if(output == "df_vector"){
    out %>% 
      pull(xie_score) %>% 
      bind_cols(.data, xie_score = .)
  } else if(output == "df_components"){
    out
  }
}








# Cover I ----

coverI <- function(.data, age, sex, cancer, copd, diabetes, heart, htn, #hyperl,
                   renal,
                   output = c("vector", "components", "df_vector", "df_components"),
                   na_to_zeros = TRUE, all_na_rm = TRUE){
  
  .age = enquo(age)
  .sex = enquo(sex)
  .cancer = enquo(cancer)
  .copd = enquo(copd)
  .diabetes = enquo(diabetes)
  .heart = enquo(heart)
  .htn = enquo(htn)
  #.hyperl = enquo(hyperl)
  .renal = enquo(renal)
  
  out = .data %>%
    dplyr::mutate_at(vars(!! .age,
    ), as.numeric) %>%
    dplyr::mutate_at(vars(!! .sex, !! .cancer, !! .copd, !! .diabetes, !! .heart,
                          !! .htn, #!! .hyperl, 
                          !! .renal), ~ as.character(.) %>% tolower() %>% trimws()) %>%
    
    mutate(
      coverI_age = case_when(
        !! .age <18 ~ 0,
        !! .age <20 ~ -10,
        !! .age <25 ~ 2,
        !! .age <30 ~ -1,
        !! .age < 35 ~ 0,
        !! .age < 40 ~ 0,
        !! .age < 45 ~ 3,
        !! .age < 50 ~ 4,
        !! .age < 55 ~ 10,
        !! .age < 60 ~ 12,
        !! .age < 65 ~ 16,
        !! .age < 70 ~ 22,
        !! .age < 75 ~ 21,
        !! .age < 80 ~ 22,
        !! .age < 85 ~ 21,
        !! .age < 90 ~ 25,
        !! .age < 95 ~ 21,
        TRUE ~ NA_real_),
      
      coverI_sex = case_when(
        !! .sex == "male" ~ 31,
        !! .sex == "female" ~ 27,
        TRUE ~ NA_real_),
      
      coverI_cancer = case_when(
        !! .cancer == "Yes" ~ 1,
        !! .cancer == "No" ~ 0,
        TRUE ~ NA_real_),
      
      coverI_copd = case_when(
        !! .copd == "Yes" ~ 6,
        !! .copd == "No" ~ 0,
        TRUE ~ NA_real_),
      
      coverI_diabetes = case_when(
        !! .diabetes == "Yes" ~ 4,
        !! .diabetes == "No" ~ 0,
        TRUE ~ NA_real_),
      
      coverI_heart = case_when(
        !! .heart == "Yes" ~ 4,
        !! .heart == "No" ~ 0,
        TRUE ~ NA_real_),
      
      coverI_htn = case_when(
        !! .htn == "Yes" ~ 5,
        !! .htn == "No" ~ 0,
        TRUE ~ NA_real_),
      
      # coverI_hyperl = case_when(
      #   !! .hyperl == "Yes" ~ -4,
      #   !! .hyperl == "No" ~ 0,
      #   TRUE ~ NA_real_),
      # 
      coverI_renal = case_when(
        !! .renal == "Yes" ~ 4,
        !! .renal == "No" ~ 0,
        TRUE ~ NA_real_),
      
    ) %>%
    mutate(
      coverI = rowSums(dplyr::select(., dplyr::starts_with("coverI_")),
                       na.rm = na_to_zeros)
    ) %>%
    
    {if(all_na_rm){
      dplyr::mutate(., coverI = dplyr::if_else(
        is.na(coverI_age) &
          is.na(coverI_sex) &
          is.na(coverI_cancer) &
          is.na(coverI_copd) &
          is.na(coverI_diabetes) &
          is.na(coverI_heart) &
          is.na(coverI_htn) &
          # is.na(coverI_hyperl) &
          is.na(coverI_renal),
        NA_real_, coverI))
    } else {
      .
    }}
  
  if(output == "vector"){
    out %>%
      pull(coverI)
  } else if(output == "components"){
    out %>%
      select(starts_with("coverI"))
  } else if(output == "df_vector"){
    out %>%
      pull(coverI) %>%
      bind_cols(.data, coverI = .)
  } else if(output == "df_components"){
    out
  }
}



# Cover F ----

coverF <- function(.data, age, sex, cancer, copd, diabetes, heart, htn, #hyperl,
                   renal,
                   output = c("vector", "components", "df_vector", "df_components"),
                   na_to_zeros = TRUE, all_na_rm = TRUE){
  
  .age = enquo(age)
  .sex = enquo(sex)
  .cancer = enquo(cancer)
  .copd = enquo(copd)
  .diabetes = enquo(diabetes)
  .heart = enquo(heart)
  .htn = enquo(htn)
  # .hyperl = enquo(hyperl)
  .renal = enquo(renal)
  
  out = .data %>%
    dplyr::mutate_at(vars(!! .age,
    ), as.numeric) %>%
    dplyr::mutate_at(vars(!! .sex, !! .cancer, !! .copd, !! .diabetes, !! .heart,
                          !! .htn, #!! .hyperl, 
                          !! .renal), ~ as.character(.) %>% tolower() %>% trimws()) %>%
    
    mutate(
      coverF_age = case_when(
        !! .age <18 ~ 0,
        !! .age <20 ~ -15,
        !! .age <25 ~ -8,
        !! .age <30 ~ -20,
        !! .age < 35 ~ -5,
        !! .age < 40 ~ 0,
        !! .age < 45 ~ -6,
        !! .age < 50 ~ 1,
        !! .age < 55 ~ 15,
        !! .age < 60 ~ 12,
        !! .age < 65 ~ 16,
        !! .age < 70 ~ 27,
        !! .age < 75 ~ 31,
        !! .age < 80 ~ 35,
        !! .age < 85 ~ 40,
        !! .age < 90 ~ 45,
        !! .age < 95 ~ 30,
        TRUE ~ NA_real_),
      
      coverF_sex = case_when(
        !! .sex == "male" ~ 31,
        !! .sex == "female" ~ 27,
        TRUE ~ NA_real_),
      
      coverF_cancer = case_when(
        !! .cancer == "Yes" ~ 3,
        !! .cancer == "No" ~ 0,
        TRUE ~ NA_real_),
      
      coverF_copd = case_when(
        !! .copd == "Yes" ~ 4,
        !! .copd == "No" ~ 0,
        TRUE ~ NA_real_),
      
      coverF_diabetes = case_when(
        !! .diabetes == "Yes" ~ 2,
        !! .diabetes == "No" ~ 0,
        TRUE ~ NA_real_),
      
      coverF_heart = case_when(
        !! .heart == "Yes" ~ 2,
        !! .heart == "No" ~ 0,
        TRUE ~ NA_real_),
      
      coverF_htn = case_when(
        !! .htn == "Yes" ~ 3,
        !! .htn == "No" ~ 0,
        TRUE ~ NA_real_),
      
      # coverF_hyperl = case_when(
      #   !! .hyperl == "Yes" ~ -7,
      #   !! .hyperl == "No" ~ 0,
      #   TRUE ~ NA_real_),
      
      coverF_renal = case_when(
        !! .renal == "Yes" ~ 2,
        !! .renal == "No" ~ 0,
        TRUE ~ NA_real_),
      
    ) %>%
    mutate(
      coverF = rowSums(dplyr::select(., dplyr::starts_with("coverF_")),
                       na.rm = na_to_zeros)
    ) %>%
    
    {if(all_na_rm){
      dplyr::mutate(., coverF = dplyr::if_else(
        is.na(coverF_age) &
          is.na(coverF_sex) &
          is.na(coverF_cancer) &
          is.na(coverF_copd) &
          is.na(coverF_diabetes) &
          is.na(coverF_heart) &
          is.na(coverF_htn) &
          # is.na(coverF_hyperl) &
          is.na(coverF_renal),
        NA_real_, coverF))
    } else {
      .
    }}
  
  if(output == "vector"){
    out %>%
      pull(coverF)
  } else if(output == "components"){
    out %>%
      select(starts_with("coverF"))
  } else if(output == "df_vector"){
    out %>%
      pull(coverF) %>%
      bind_cols(.data, coverF = .)
  } else if(output == "df_components"){
    out
  }
}
