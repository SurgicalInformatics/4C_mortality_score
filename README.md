ISARIC WHO CCP-UK study: 4C Mortality Score public repo
==============

Public repo containing functions and code used in the development of the 4C Mortality Score

## Purpose

The functions in this repo are for use with ISARIC WHO CCP-UK study. 

Data should be prepared, cleaned, and carefully checked as per standard procedures. 

Collaboration with consortium members is advised to ensure limitations and vagaries of data are understood. 

We are grateful if any bugs or errors can be reported in issues. Many thanks!


## Data sharing

We welcome applications for data and material access via our Independent Data And Material Access Committee (https://isaric4c.net).

## Data preparation

### Clone and apply 
https://github.com/SurgicalInformatics/cocin_ccp.

### `01_data_prep.R`

1. Packages.
2. Variable definitions.
3. Extreme values in continuous variables. 
4. Training and test definitions. 
5. Geographical split definitions

## Functions

### `02_functions.R`

1. ggplot templates / extraction functions.
2. Table defaults.
3. Prognostic scoring.

## Multiple imputation by chained equations

### `03_mice.R`

1. Missing data inspection, description and characterisation .
2. Basic `mice()` function and approach.

## Generalised additive models

### `04_gam.R`

1. GAM with complete data.
2. GAM with mice data.
3. `purrr` methods for metrics, combining by Rubin's rules. 


## glmnet regression

### `05_lasso.R`

1. Apply variables changes across imputed datasets.
2. `glmnet()` using MICE datasets.
3. Extract `glmnet` coefficients, combine and scale.

## 4C mortality score

### `06_4c_mortality_score.R`

1. 4C mortality score function.
2. Prognostic index discrimination using mice data.
3. Score distribution.
4. Calibration.


## Risk tables

### `07_risk_tables.R`

1. `prognos` function for easy cut-off table generation.
2. Risk tables generation at specified cut-offs.

## Comparisons with pre-existing scores

### `08_score_comparisons.R`

1. Generate risk scores.
2. `ff_aucroc()` function for applying AUCROC and counts across multiple scores.
3. Apply `ff_auroc()` to risk scores.


## XGBoost

### `09_xgboost.R`

1. Variable definition.
2. Derivation and validation matrices created.
3. XGBoost training.
4. Discrimination (AUROC).

## Decision curve analysis

### `10_decision_curve_analysis.R`

1. Apply comparison scores in derivation and validation data. 
2. DCA: fit in derivation and predict in validation (recalibration of comparators). 
3. DCA: fit and predict in validation data (recalibration of all).

## Mortality table for each score


<style>
table th:first-of-type {
    width: 10%;
}
table th:nth-of-type(2) {
    width: 10%;
}
table th:nth-of-type(3) {
    width: 50%;
}
table th:nth-of-type(4) {
    width: 30%;
}
</style>

 4C mortality score <img width=50/> | Mortality (%) <img width=500/>
------------- | -------------
1	| 0.3
2	| 0.8
3	| 2.3
4	| 4.8
5	| 7.5
6	| 7.8
7	| 11.7
8	| 14.4
9	| 19.2
10	| 22.9
11	| 26.9
12	| 32.9
13	| 40.1
14	| 44.6
15	| 51.6
16	| 59.1
17	| 66.1
18	| 75.8
19	| 77.4
20	| 82.9
21	| 87.5




