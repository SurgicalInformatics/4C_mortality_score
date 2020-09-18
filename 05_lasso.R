# ISARIC WHO CCP-UK study: 4C Mortality Score
# Binary logistic regression using LASSO
# 05_lasso.R
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2020

# 1. Apply variables changes across imputed datasets
# 2. glmnet using MICE datasets
# 3. Extract glmnet coefficients, combine and scale

# Packages -------------------------------------
library(purrr)
library(glmnet)
library(recipes)

# Categorise continuous variables as per GAM plot analyses -----------------------------------------
## Note this method applies mutate to all mice datasets in one move.
sets_train_cat = sets_train %>% 
  complete("all") %>% 
  map(. %>% 
        select(death, age, sex, no_comorbid, rr_vsorres, oxy_vsorres, 
               daily_gcs_vsorres, daily_bun_lborres, daily_crp_lborres) %>% 
        mutate(
          age = case_when(
            age < 50 ~ "<50",
            age < 60 ~ "50-59",
            age < 70 ~ "60-69",
            age < 80 ~ "70-79",
            age >= 80 ~ "80+",
            TRUE ~ NA_character_) %>% 
            factor(),
          
          sex = fct_relevel(sex, "Female"),
          
          no_comorbid = case_when(
            no_comorbid == 0 ~ "0",
            no_comorbid == 1 ~ "1",
            no_comorbid >= 2 ~ "2",
            TRUE ~ NA_character_),
          
          rr_vsorres = case_when(
            rr_vsorres < 20 ~ "<20",
            rr_vsorres < 30 ~ "20-29",
            rr_vsorres >=30 ~ "30+",
            TRUE ~ NA_character_),
          
          oxy_vsorres = case_when(
            oxy_vsorres < 92 ~ "<92",
            oxy_vsorres >= 92 ~ ">=92",
            TRUE ~ NA_character_) %>% 
            fct_relevel(">=92"),
          
          daily_gcs_vsorres = case_when(
            daily_gcs_vsorres <=  14 ~ "<15",
            daily_gcs_vsorres > 14 ~ "15",
            TRUE ~ NA_character_),
          
          daily_bun_lborres = case_when(
            daily_bun_lborres < 7 ~ "<7",
            daily_bun_lborres < 14 ~ "<14",
            daily_bun_lborres >=  14 ~ "14+",
            TRUE ~ NA_character_) %>% 
            fct_relevel("<7", "<14"),
          
          daily_crp_lborres = case_when(
            daily_crp_lborres < 50 ~ "<50",
            daily_crp_lborres < 100 ~ "50-99",
            daily_crp_lborres >=100 ~ "100+",
            TRUE ~ NA_character_) %>% 
            fct_relevel("<50", "50-99"))
  )

# LASSO using MICE data ------------------------------------------------------
## Define design matrix, then pull to environment as X
## K-fold cross-validation performed within function

lasso_out = sets_train_cat %>%
  map(function(.x){
    
    # Define X and y
    .x %>% 
      recipe(death ~ .) %>% 
      step_dummy(all_nominal(), -death) %>%
      step_naomit(everything()) %>% 
      prep() %>% 
      juice() %T>%
      {X <<- select(., -death) %>% as.matrix()} %>% 
      {y <<- pull(., death) %>% as.numeric() %>% {. - 1}}
    
    # Fit
    fits = glmnet(X, y, 
                  family = "binomial", maxit = 1000)
    
    # K-fold CV, nfolds = 10
    cv.fits = cv.glmnet(X, y, standardize = FALSE,
                        family = "binomial", maxit = 1000)
    
    opt.lam = c(cv.fits$lambda.min, cv.fits$lambda.1se)

    return(coef(fits, cv.fits$lambda.min))
  }
  )

# Extract and combine coefficients +/- scale and round
lasso_out

# Coefficients from mice datasets combined (mean)
(Reduce('+', lasso_out) / 10) %>% 
  matrix() %>% 
  as.data.frame() %>% 
  round(3)

# Scaled
(Reduce('+', lasso_out) / 10 * 3) %>% 
  matrix() %>% 
  as.data.frame() %>% 
  round(0)
