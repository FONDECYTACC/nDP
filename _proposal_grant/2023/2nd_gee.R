# 0. Load and structure ---------------------------------------------------------
rm(list = ls()) 
folder_path <- ifelse(dir.exists("E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/_proposal_grant/2023/"),
                      "E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/_proposal_grant/2023/",
                      "C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/_proposal_grant/2023/")
load(paste0(folder_path,"an_grant_23_24_3.RData"))

if(!require(geepack)){install.packages("geepack");library(geepack)}
if(!require(geeM)){install.packages("geeM");library(geeM)}
if(!require(tidyverse)){install.packages("tidyverse");library(tidyverse)}
if(!require(MASS)){install.packages("MASS");library(MASS)}
if(!require(geeasy)){install.packages("geeasy");library(geeasy)}
if(!require(MuMIn)){remotes::install_version("MuMIn", "1.46.0");library(MuMIn)}
if(!require(tableone)){install.packages("tableone");library(tableone)}

counting_process_output<-
  function(object=NULL){
    broom::tidy(object$m, exponentiate=T, conf.int=T) %>% 
      dplyr::mutate(across(c("estimate","std.error","robust.se","statistic","conf.low","conf.high"),~sprintf("%1.2f",.))) %>% 
      dplyr::mutate(p.value= sprintf("%1.4f",p.value))
  }
#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_

### 0.a. Descriptives ---------------------------------------------------------

invisible("To account for variability by treatment setting, we stratified the analysis by setting: basic ambulatory GP intensive ambulatory GP residential WO intensive ambulatory WO residential")
table(subset(Base_fiscalia_v15f_grant_23_24_long2_miss_proc_multtr, is_first_occurrence==1, select= "tipo_de_plan_2_mod"))

data_mine_miss_restr_proc2 %>% 
  group_by(hash_key) %>% 
  summarise(min(fech_ing_num)) %>% nrow()
#[1] 13317

subset(data_mine_miss_restr_proc2 , is_first_occurrence==1) %>% nrow()
#[1] 13317

data_mine_miss_restr_proc2_baseline<-
  subset(data_mine_miss_restr_proc2 , is_first_occurrence==1) 

#https://chat.openai.com/share/69263a7b-e38c-4208-9115-aca538522419
variables_vector <- c("tr_outcome", "policonsumo2", "comp_bpsc_y3_severe",
                      "less_90d_tr1", "log_dias_treat_imp_sin_na",
                      "edad_al_ing_1", "ano_nac_corr", "susinidum_oh", 
                      "susinidum_coc", "susinidum_pbc", "susinidum_mar", 
                      "psycom_dum_study", "psycom_dum_with", "freq_cons_dum_5day", 
                      "cond_oc_dum_2inact", "cond_oc_dum_3unemp", "susprindum_coc", 
                      "susprindum_pbc", "susprindum_mar", "susprindum_oh")#,
                      #"lag_comp_bpsc_y3_severe", "lag_less_90d_tr1", 
                      #"log_lag_dias_treat_imp_sin_na", "lag_policonsumo2", 
                      #"lag_tr_outcome")

attr(data_mine_miss_restr_proc2_baseline$tr_outcome,"label") <- "Complete status of treatment (binary)"
attr(data_mine_miss_restr_proc2_baseline$tr_outcome,"label") <- "Complete status of treatment (binary)"
attr(data_mine_miss_restr_proc2_baseline$policonsumo2,"label") <- "Polysubstance use"
attr(data_mine_miss_restr_proc2_baseline$comp_bpsc_y3_severe,"label") <- "Biopsychosocial compromise (Severe)"
attr(data_mine_miss_restr_proc2_baseline$less_90d_tr1,"label") <- "Treatment duration (binary) (<90 days)"
attr(data_mine_miss_restr_proc2_baseline$log_dias_treat_imp_sin_na,"label") <- "Treatment duration (log-scaled days)"
attr(data_mine_miss_restr_proc2_baseline$edad_al_ing_1,"label") <- "Age at admission to treatment"
attr(data_mine_miss_restr_proc2_baseline$ano_nac_corr,"label") <- "Birth year"
attr(data_mine_miss_restr_proc2_baseline$susinidum_oh,"label") <- "Primary substance (initial diagnosis): alcohol"
attr(data_mine_miss_restr_proc2_baseline$susinidum_coc,"label") <- "Primary substance (initial diagnosis): cocaine hydrochloride"
attr(data_mine_miss_restr_proc2_baseline$susinidum_pbc,"label") <- "Primary substance (initial diagnosis): cocaine base paste"
attr(data_mine_miss_restr_proc2_baseline$susinidum_mar,"label") <- "Primary substance (initial diagnosis): marijuana"
attr(data_mine_miss_restr_proc2_baseline$psycom_dum_study,"label") <- "Psychiatric comorbidity (ICD-10): In study"
attr(data_mine_miss_restr_proc2_baseline$psycom_dum_with,"label") <- "Psychiatric comorbidity (ICD-10): Diagnosed"
attr(data_mine_miss_restr_proc2_baseline$freq_cons_dum_5day,"label") <- "Daily frequence of primary substance use at admission"
attr(data_mine_miss_restr_proc2_baseline$cond_oc_dum_2inact,"label") <- "Occupational Status: Inactive"
attr(data_mine_miss_restr_proc2_baseline$cond_oc_dum_3unemp,"label") <- "Occupational Status: Unemployed"
attr(data_mine_miss_restr_proc2_baseline$susprindum_oh,"label") <- "Primary substance at admission to treatment: alcohol"
attr(data_mine_miss_restr_proc2_baseline$susprindum_coc,"label") <- "Primary substance at admission to treatment: cocaine hydrochloride"
attr(data_mine_miss_restr_proc2_baseline$susprindum_pbc,"label") <- "Primary substance at admission to treatment: cocaine base paste"
attr(data_mine_miss_restr_proc2_baseline$susprindum_mar,"label") <- "Primary substance at admission to treatment: marijuana"
attr(data_mine_miss_restr_proc2_baseline$tipo_de_plan_2_mod,"label") <- "Treatment setting"

invisible("Table for imputed dataset with selected covariates")

library(tableone)

vars <- setdiff(c(variables_vector,"tipo_de_plan_2_mod"), "policonsumo2")
factorVars <- setdiff(variables_vector, c("policonsumo2", "edad_al_ing_1", "log_dias_treat_imp_sin_na", "ano_nac_corr"))

tableOne <- CreateTableOne(vars = vars, data = data_mine_miss_restr_proc2_baseline,
                           factorVars = factorVars, strata = "policonsumo2", 
                           addOverall = TRUE,  # <-- This is the key addition
                           includeNA = TRUE,
                           test = TRUE) # Remove statistical tests for speed

print(tableOne, smd = TRUE) # Calculate SMD separately if needed


folder_path <- ifelse(dir.exists("E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/_proposal_grant/2023/"),
                      "E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/_proposal_grant/2023/",
                      "C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/_proposal_grant/2023/")

as.data.frame.TableOne(tableOne, smd=T, nonnormal= T)%>% 
  dplyr::mutate(char2=characteristic) %>% 
  tidyr::fill(char2) %>% 
  dplyr::select(char2,everything()) %>% 
  dplyr::mutate(level=ifelse(is.na(level),"[Missing]",level)) %>% 
  dplyr::mutate(char2=dplyr::case_when(characteristic=="NA"~NA_character_,TRUE~as.character(characteristic))) %>% 
  format_cells(1, 1:length(names(.)), "bold") %>%
  dplyr::select(-1) %>% 
  dplyr::mutate_all(~ str_replace_all(., pattern = "\\( ", replacement = "\\(")) %>% 
  dplyr::mutate_all(~ str_trim(.)) %>% 
  {
    #knitr::kable(.,size=10, format="markdown",caption= "Summary descriptives, Polysubstance(1) and no Polysubstance use (0)", escape=T)
    write.table(.,paste0(folder_path,"baseline_psu_desc_subsamp_smd.csv"), dec=",", sep="\t")
  }
  

## 0.b. IIWs ---------------------------------------------------------

rbind.data.frame(
  cbind.data.frame(model= "Primary (no corrections, lag=0)", t(matrix(summary(data_mine_miss_restr_proc2$iiw_nocorr_st)))),
  cbind.data.frame(model= "Alternative (no corrections, lag=1)", t(matrix(summary(data_mine_miss_restr_proc2$iiw_nocorr_alt_st)))),
  cbind.data.frame(model= "Primary (after PH, lag=0)", t(matrix(summary(data_mine_miss_restr_proc2_iiw_after_ph$iiw_after_ph_st)))),
  cbind.data.frame(model= "Alternative (after PH, lag=1)", t(matrix(summary(data_mine_miss_restr_proc2_iiw_after_ph_alt$iiw_after_ph_st)))),
  cbind.data.frame(model= "Primary (strata, lag=0)", t(matrix(summary(data_mine_miss_proc2_iiw_strata$iiw_strata_st)))),
  cbind.data.frame(model= "Alternative (strata, lag=1)", t(matrix(summary(data_mine_miss_proc2_iiw_strata_alt$iiw_strata_st)))),
  cbind.data.frame(model= "Alternative (2nd strata intervals, lag=1)", t(matrix(summary(data_mine_miss_proc2_iiw_strata_alt$iiw_strata_st))))
  ) %>%  #Min.	First quartile	Median	Mean	Third quartile	Max.
  dplyr::mutate_if(is.numeric, ~sprintf("%1.2f",.)) %>% 
  {
    #knitr::kable(.,size=10, format="markdown",caption= "Summary descriptives, Polysubstance(1) and no Polysubstance use (0)", escape=T)
    write.table(.,paste0(folder_path,"baseline_psu_desc_subsamp_smd.csv"), dec=",", sep="\t")
    copiar_nombres2(.)
  }

## 0.c. Cox PH ---------------------------------------------------------

rbind.data.frame(
  cbind.data.frame(model= "Main weights", counting_process_output(iiw_nocorr)),
  cbind.data.frame(model= "Alternative weights", counting_process_output(iiw_nocorr_alt))
) %>% 

broom::tidy(iiw_nocorr_alt$m, exponentiate=T, conf.int=T) %>% 
  dplyr::mutate(across(c("estimate","std.error","robust.se","statistic","conf.low","conf.high"),~sprintf("%1.2f",.))) %>% 
  dplyr::mutate(p.value= sprintf("%1.4f",p.value))

#iiw_model_after_ph_ba   iiw_model_after_ph_ba_alt   iiw_model_after_ph_gp_ia iiw_model_after_ph_gp_ia_alt  iiw_model_after_ph_gp_res iiw_model_after_ph_gp_res_alt  iiw_model_after_ph_wo_ia  iiw_model_after_ph_wo_ia_alt  iiw_model_after_ph_wo_res  iiw_model_after_ph_wo_res_alt

iiws_strat_alt_alt[[1]]$model$m

for (i in 1:30){
  print(iiws_strat_alt_alt[[i]]$comb)
}

iiws_strat_alt[[1]]$model$m

for (i in 1:30){
  print(iiws_strat_alt[[i]]$comb)
}

iiws_strat[[1]]$model$m


for (i in 1:15){
  print(iiws_strat[[i]]$comb)
}

## 0.c. Cox PH, roots ---------------------------------------------------------
library(rms)
library(broom)
library(survival)

#https://rdrr.io/github/gforge/Greg/src/R/tidy.rms.R
source("tidy_rms.R")

term_mapping <- data.frame(
  term = c("lag_tr_outcome", "lag_comp_bpsc_y3_severe", "lag_less_90d_tr1", 
           "log_lag_dias_treat_imp_sin_na", "lag_policonsumo2", "edad_al_ing_1", 
           "ano_nac_corr", "susinidum_oh", "susinidum_coc", "susinidum_pbc", 
           "susinidum_mar", "psycom_dum_study", "psycom_dum_with", "freq_cons_dum_5day", 
           "cond_oc_dum_2inact", "cond_oc_dum_3unemp", "susprindum_oh", "susprindum_coc", 
           "susprindum_pbc", "susprindum_mar", "lag_tr_outcome_rec", "lag_less_90d_tr1_rec", 
           "lag_comp_bpsc_y3_severe_rec", "susinidum_coc_rec2", "psycom_dum_with_rec2"),
  term_label = c("Treatment outcome of the previous treatment", "Previous biopsychosocial compromise (severe)", 
                 "Previous treatment duration (<90 days)", "Previous treatment duration (in logarithmic scaled days)", 
                 "Polysubstance use status of the previous treatment", "Age at admission to treatment", "Birth year", 
                 "Primary substance (initial diagnosis), alcohol", "Primary substance (initial diagnosis), cocaine", 
                 "Primary substance (initial diagnosis), cocaine base paste", "Primary substance (initial diagnosis), marijuana", 
                 "Psychiatric comorbidity (diagnosis unknown or under study)", "Psychiatric comorbidity (confirmed comorbidity)", 
                 "Daily frequence of primary substance use at admission", "Occupational status (inactive)", "Occupational status (unemployed)", 
                 "Primary substance at admission to treatment (alcohol)", "Primary substance at admission to treatment (cocaine hydrochloride)", 
                 "Primary substance at admission to treatment (cocaine base paste)", "Primary substance at admission to treatment (marijuana)", 
                 "Treatment outcome of the previous treatment (recoded for interaction with time)", 
                 "Previous biopsychosocial compromise (severe, recoded for interaction with time)", 
                 "Previous treatment duration (<90 days, recoded for interaction with time)", 
                 "Primary substance (initial diagnosis), cocaine (recoded for interaction with time)", 
                 "Psychiatric comorbidity (confirmed comorbidity, recoded for interaction with time)")
)


cph_nocorr<-
  cph(Surv(lag_time,time,event)~
        cluster(id)+ 
        lag_tr_outcome+
        lag_comp_bpsc_y3_severe+
        lag_less_90d_tr1+
        log_lag_dias_treat_imp_sin_na +
        lag_policonsumo2 + 
        edad_al_ing_1 + 
        ano_nac_corr + 
        susinidum_oh +
        susinidum_coc +
        susinidum_pbc +
        susinidum_mar +
        psycom_dum_study +
        psycom_dum_with +
        freq_cons_dum_5day +
        cond_oc_dum_2inact +
        cond_oc_dum_3unemp +
        susprindum_oh +
        susprindum_coc +
        susprindum_pbc +
        susprindum_mar +
        strat(tipo_de_plan_2_mod),
      data= data_mine_miss_restr_proc2 %>% data.table::as.data.table() %>% data.frame(), 
      x=TRUE, y=TRUE, surv=TRUE, iter.max = 250*4, tol = 1e-6)

cph_after_ph<-
cph(Surv(lag_time,time,event==1)~ 
                  cluster(id)+ 
                  lag_tr_outcome_rec +
                  log_lag_dias_treat_imp_sin_na +
                  lag_less_90d_tr1_rec+
                  lag_comp_bpsc_y3_severe_rec + 
                  lag_policonsumo2 + 
                  edad_al_ing_1 + 
                  ano_nac_corr + 
                  susinidum_coc_rec2 +
                  susinidum_oh +
                  susinidum_pbc +
                  susinidum_mar +
                  psycom_dum_with_rec2 +
                  psycom_dum_study + 
                  freq_cons_dum_5day +
                  cond_oc_dum_2inact +
                  cond_oc_dum_3unemp +
                  susprindum_oh +
                  susprindum_coc +
                  susprindum_pbc +
                  susprindum_mar+
                  strat(tipo_de_plan_2_mod), 
                data=data_mine_miss_restr_proc2, 
                x=TRUE, y=TRUE, surv=TRUE, iter.max = 250*4, tol = 1e-6)

models_hr_table2<-
rbind.data.frame(
cbind.data.frame(model="No correction", tidy.rms(cph_nocorr, conf.int=T, exponentiate=T)[, c("term","estimate", "conf.low", "conf.high")]%>% 
  dplyr::mutate_if(is.numeric, ~sprintf("%1.2f",.))),
cbind.data.frame(model="After PH", tidy.rms(cph_after_ph, conf.int=T, exponentiate=T)[, c("term","estimate", "conf.low", "conf.high")]%>% 
  dplyr::mutate_if(is.numeric, ~sprintf("%1.2f",.))),
cbind.data.frame(model="Stratifying", tidy.rms(model_after_time_strat, conf.int=T, exponentiate=T)[, c("term","estimate", "conf.low", "conf.high")]%>% 
  dplyr::mutate_if(is.numeric, ~sprintf("%1.2f",.))),
cbind.data.frame(model="Stratifying, alt strata", tidy.rms(model_after_time_strat_alt, conf.int=T, exponentiate=T)[, c("term","estimate", "conf.low", "conf.high")] %>% 
  dplyr::mutate_if(is.numeric, ~sprintf("%1.2f",.)))
)

models_hr_table2$term_label <- term_mapping$term_label[match(models_hr_table2$term, term_mapping$term)]

models_hr_table2%>%
  dplyr::select(term_label, everything()) %>% 
  {
    #copiar_nombres2(.)
    write.table(.,paste0(folder_path,"coxph_intensity_model_prev.csv"), dec=",", sep="\t")
    knitr::kable(.,size=10, format="markdown",caption= "Intensity model", escape=T)
  }

## 1. No correction ---------------------------------------------------------

plan_names <- attr(table(data_mine_miss_restr_proc2$tipo_de_plan_2_mod),"dimnames")[[1]]


### 1.1. No correciton, no weight ----------------------------------------------

# Initialize a list to store models
nocorr_nowgt_list <- list()

# Loop over each unique plan name to fit a model
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_restr_proc2, tipo_de_plan_2_mod == plan_names[i])
  
  # Fit the GEE model for the current subset
  model <- geese(tr_outcome ~ policonsumo2 +
                   comp_bpsc_y3_severe+
                   edad_al_ing_1 + 
                   ano_nac_corr + 
                   susinidum_oh +
                   susinidum_coc +
                   susinidum_pbc +
                   susinidum_mar +
                   psycom_dum_study +
                   psycom_dum_with +
                   freq_cons_dum_5day +
                   cond_oc_dum_2inact +
                   cond_oc_dum_3unemp +
                   susprindum_coc +
                   susprindum_pbc +
                   susprindum_mar, #origen_ingreso_mod fis_comorbidity_icd_10 dg_trs_cons_sus_or
                 id = id, 
                 data = current_data,
                 family = poisson(), 
                 corstr = "independence", 
                 jack = T)
  
  # Assign the model to the list with a name based on the plan name
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  nocorr_nowgt_list[[paste("model", model_name, sep = "_")]] <- model
}


#### 1.1.1 No correction, no weight, GEEM Poisson ----------------------------------------------

# Initialize a list to store models
geem_nocorr_nowgt_list <- list()

# Loop over each unique plan name to fit a model
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_restr_proc2, tipo_de_plan_2_mod == plan_names[i])
  
  # Fit the GEE model for the current subset using geem
  model <- geem(tr_outcome ~ policonsumo2 +
                  comp_bpsc_y3_severe+
                  edad_al_ing_1 + 
                  ano_nac_corr + 
                  susinidum_oh +
                  susinidum_coc +
                  susinidum_pbc +
                  susinidum_mar +
                  psycom_dum_study +
                  psycom_dum_with +
                  freq_cons_dum_5day +
                  cond_oc_dum_2inact +
                  cond_oc_dum_3unemp +
                  susprindum_coc +
                  susprindum_pbc +
                  susprindum_mar, # Include relevant predictors
                id = id, 
                data = current_data,
                family = poisson("log"), 
                corstr = "independence")
  
  # Assign the model to the list with a name based on the plan name
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  if(model$converged==FALSE){stop("model did not coverge")}
  cat(sprintf("QIC for plan %s = %.1f\n", plan_names[i], as.numeric(MuMIn::QIC(model))))
  geem_nocorr_nowgt_list[[paste("model", model_name, sep = "_")]] <- model
}
# QIC for plan basic ambulatory = 19452.9
# QIC for plan GP intensive ambulatory = 22259.1
# QIC for plan GP residential = 9817.2
# QIC for plan WO intensive ambulatory = 3420.7
# QIC for plan WO residential = 4823.9

#### 1.1.2 No correction, no weight, GEEM Negative binomial ----------------------------------------------


# Define a sequence of x values
x_values <- seq(158e4, 159e4, by = 100)

# Initialize a list to store models and their QICs
geem2_nocorr_nowgt_list <- list()

# Loop over each unique plan name to fit models
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_restr_proc2, tipo_de_plan_2_mod == plan_names[i])
  
  # Initialize variable to store the best QIC and associated model
  best_qic <- Inf
  best_model <- NULL
  best_x <- NA
  
  # Loop through each x value to find the one with the lowest QIC
  for (x in x_values) {
    # Fit the GEE model for the current subset using geem with negative binomial
    model <- geem(tr_outcome ~ policonsumo2 +
                    comp_bpsc_y3_severe+
                    edad_al_ing_1 + 
                    ano_nac_corr + 
                    susinidum_oh +
                    susinidum_coc +
                    susinidum_pbc +
                    susinidum_mar +
                    psycom_dum_study +
                    psycom_dum_with +
                    freq_cons_dum_5day +
                    cond_oc_dum_2inact +
                    cond_oc_dum_3unemp +
                    susprindum_coc +
                    susprindum_pbc +
                    susprindum_mar, # Include relevant predictors
                  id = id, 
                  data = current_data,
                  family = MASS::negative.binomial(x),
                  corstr = "independence")
    
    if (model$converged) {
      # Calculate QIC for the model
      model_qic <- MuMIn::QIC(model)
      
      # Check if this model has a better QIC
      if (model_qic < best_qic) {
        best_qic <- model_qic
        best_model <- model
        best_x <- x
      }
    }
  }
  
  # Store the best model and its details in the list
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  geem2_nocorr_nowgt_list[[paste("model", model_name, sep = "_")]] <- list(best_model = best_model, best_x = best_x, best_qic = best_qic)
  
  # Optional: Print out some information on progress
  cat(sprintf("Best QIC for plan %s with x = %.1f: %f\n", plan_names[i], best_x, best_qic))
}
# Best QIC for plan basic ambulatory with x = 1588300.0: 19452.859311
# Best QIC for plan GP intensive ambulatory with x = 1588300.0: 22259.103757
# Best QIC for plan GP residential with x = 1588300.0: 9817.174703
# Best QIC for plan WO intensive ambulatory with x = 1588300.0: 3420.678029
# Best QIC for plan WO residential with x = 1588300.0: 4823.862856

# Check the result for a particular plan
print(geem2_nocorr_nowgt_list[[1]])


### 1.2. No correction, weight ----------------------------------------------

# Initialize a list to store models
nocorr_wgt_list <- list()

# Loop over each unique plan name to fit a model
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_restr_proc2, tipo_de_plan_2_mod == plan_names[i])
  
  # Fit the GEE model for the current subset
  model <- geese(tr_outcome ~ policonsumo2 +
                   comp_bpsc_y3_severe+
                   edad_al_ing_1 + 
                   ano_nac_corr + 
                   susinidum_oh +
                   susinidum_coc +
                   susinidum_pbc +
                   susinidum_mar +
                   psycom_dum_study +
                   psycom_dum_with +
                   freq_cons_dum_5day +
                   cond_oc_dum_2inact +
                   cond_oc_dum_3unemp +
                   susprindum_coc +
                   susprindum_pbc +
                   susprindum_mar, 
                 id = id, 
                 data = current_data,
                 family = poisson(), 
                 weight= current_data$iiw_nocorr_st,
                 corstr = "independence", 
                 jack = T)
  
  # Assign the model to the list with a name based on the plan name
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  nocorr_wgt_list[[paste("model", model_name, sep = "_")]] <- model
}


# Initialize a list to store models
nocorr_wgt_list <- list()

# Loop over each unique plan name to fit a model
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_restr_proc2, tipo_de_plan_2_mod == plan_names[i])
  
  # Fit the GEE model for the current subset
  model <- geese(tr_outcome ~ policonsumo2 +
                   comp_bpsc_y3_severe+
                   edad_al_ing_1 + 
                   ano_nac_corr + 
                   susinidum_oh +
                   susinidum_coc +
                   susinidum_pbc +
                   susinidum_mar +
                   psycom_dum_study +
                   psycom_dum_with +
                   freq_cons_dum_5day +
                   cond_oc_dum_2inact +
                   cond_oc_dum_3unemp +
                   susprindum_coc +
                   susprindum_pbc +
                   susprindum_mar, 
                 id = id, 
                 data = current_data,
                 family = poisson(), 
                 weight= current_data$iiw_nocorr_st,
                 corstr = "independence", 
                 jack = T)
  
  # Assign the model to the list with a name based on the plan name
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  nocorr_wgt_list[[paste("model", model_name, sep = "_")]] <- model
}

#### 1.2.1 No correction, no weight, GEEM Poisson ----------------------------------------------

# Initialize a list to store models
geem_nocorr_wgt_list <- list()

# Loop over each unique plan name to fit a model
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_restr_proc2, tipo_de_plan_2_mod == plan_names[i])
  
  # Fit the GEE model for the current subset using geem
  model <- geem(tr_outcome ~ policonsumo2 +
                  comp_bpsc_y3_severe+
                  edad_al_ing_1 + 
                  ano_nac_corr + 
                  susinidum_oh +
                  susinidum_coc +
                  susinidum_pbc +
                  susinidum_mar +
                  psycom_dum_study +
                  psycom_dum_with +
                  freq_cons_dum_5day +
                  cond_oc_dum_2inact +
                  cond_oc_dum_3unemp +
                  susprindum_coc +
                  susprindum_pbc +
                  susprindum_mar, # Include relevant predictors
                id = id, 
                data = current_data,
                weight= current_data$iiw_nocorr_st,
                family = poisson("log"), 
                corstr = "independence")
  
  # Assign the model to the list with a name based on the plan name
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  if(model$converged==FALSE){stop("model did not coverge")}
  cat(sprintf("QIC for plan %s = %.1f\n", plan_names[i], as.numeric(MuMIn::QIC(model))))
  geem_nocorr_wgt_list[[paste("model", model_name, sep = "_")]] <- model
}
# QIC for plan basic ambulatory = 19463.5
# QIC for plan GP intensive ambulatory = 22272.1
# QIC for plan GP residential = 9832.0
# QIC for plan WO intensive ambulatory = 3431.1
# QIC for plan WO residential = 4836.4

#### 1.2.2 No correction, no weight, GEEM Negative binomial ----------------------------------------------


# Define a sequence of x values
x_values <- seq(158e4, 159e4, by = 100)

# Initialize a list to store models and their QICs
geem2_nocorr_wgt_list <- list()

# Loop over each unique plan name to fit models
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_restr_proc2, tipo_de_plan_2_mod == plan_names[i])
  
  # Initialize variable to store the best QIC and associated model
  best_qic <- Inf
  best_model <- NULL
  best_x <- NA
  
  # Loop through each x value to find the one with the lowest QIC
  for (x in x_values) {
    # Fit the GEE model for the current subset using geem with negative binomial
    model <- geem(tr_outcome ~ policonsumo2 +
                    comp_bpsc_y3_severe+
                    edad_al_ing_1 + 
                    ano_nac_corr + 
                    susinidum_oh +
                    susinidum_coc +
                    susinidum_pbc +
                    susinidum_mar +
                    psycom_dum_study +
                    psycom_dum_with +
                    freq_cons_dum_5day +
                    cond_oc_dum_2inact +
                    cond_oc_dum_3unemp +
                    susprindum_coc +
                    susprindum_pbc +
                    susprindum_mar, # Include relevant predictors
                  id = id, 
                  data = current_data,
                  weight= current_data$iiw_nocorr_st,
                  family = MASS::negative.binomial(x),
                  corstr = "independence")
    
    if (model$converged) {
      # Calculate QIC for the model
      model_qic <- MuMIn::QIC(model)
      
      # Check if this model has a better QIC
      if (model_qic < best_qic) {
        best_qic <- model_qic
        best_model <- model
        best_x <- x
      }
    }
  }
  
  # Store the best model and its details in the list
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  geem2_nocorr_wgt_list[[paste("model", model_name, sep = "_")]] <- list(best_model = best_model, best_x = best_x, best_qic = best_qic)
  
  # Optional: Print out some information on progress
  cat(sprintf("Best QIC for plan %s with x = %.1f: %f\n", plan_names[i], best_x, best_qic))
}
# Best QIC for plan basic ambulatory with x = 1588300.0: 19463.524238
# Best QIC for plan GP intensive ambulatory with x = 1588300.0: 22272.106976
# Best QIC for plan GP residential with x = 1588300.0: 9832.041378
# Best QIC for plan WO intensive ambulatory with x = 1588300.0: 3431.119178
# Best QIC for plan WO residential with x = 1588300.0: 4836.385892

# Check the result for a particular plan
print(geem2_nocorr_wgt_list[[1]])


### 1.3. No correction, alt. weight ----------------------------------------------

# Initialize a list to store models
nocorr_alt_wgt_list <- list()

# Loop over each unique plan name to fit a model
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_restr_proc2, tipo_de_plan_2_mod == plan_names[i])
  
  # Fit the GEE model for the current subset
  model <- geese(tr_outcome ~ policonsumo2 +
                   comp_bpsc_y3_severe+
                   edad_al_ing_1 + 
                   ano_nac_corr + 
                   susinidum_oh +
                   susinidum_coc +
                   susinidum_pbc +
                   susinidum_mar +
                   psycom_dum_study +
                   psycom_dum_with +
                   freq_cons_dum_5day +
                   cond_oc_dum_2inact +
                   cond_oc_dum_3unemp +
                   susprindum_coc +
                   susprindum_pbc +
                   susprindum_mar, 
                 id = id, 
                 data = current_data,
                 family = poisson(), 
                 weight= current_data$iiw_nocorr_alt_st,
                 corstr = "independence", 
                 jack = T)
  
  # Assign the model to the list with a name based on the plan name
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  nocorr_alt_wgt_list[[paste("model", model_name, sep = "_")]] <- model
}


#### 1.3.1 No correction, alt weight, GEEM Poisson ----------------------------------------------

# Initialize a list to store models
geem_nocorr_alt_wgt_list <- list()

# Loop over each unique plan name to fit a model
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_restr_proc2, tipo_de_plan_2_mod == plan_names[i])
  
  # Fit the GEE model for the current subset using geem
  model <- geem(tr_outcome ~ policonsumo2 +
                  comp_bpsc_y3_severe+
                  edad_al_ing_1 + 
                  ano_nac_corr + 
                  susinidum_oh +
                  susinidum_coc +
                  susinidum_pbc +
                  susinidum_mar +
                  psycom_dum_study +
                  psycom_dum_with +
                  freq_cons_dum_5day +
                  cond_oc_dum_2inact +
                  cond_oc_dum_3unemp +
                  susprindum_coc +
                  susprindum_pbc +
                  susprindum_mar, # Include relevant predictors
                id = id, 
                data = current_data,
                weight= current_data$iiw_nocorr_alt_st,
                family = poisson("log"), 
                corstr = "independence")
  
  # Assign the model to the list with a name based on the plan name
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  if(model$converged==FALSE){stop("model did not coverge")}
  cat(sprintf("QIC for plan %s = %.1f\n", plan_names[i], as.numeric(MuMIn::QIC(model))))
  geem_nocorr_alt_wgt_list[[paste("model", model_name, sep = "_")]] <- model
}
# QIC for plan basic ambulatory = 19457.6
# QIC for plan GP intensive ambulatory = 22268.1
# QIC for plan GP residential = 9826.1
# QIC for plan WO intensive ambulatory = 3426.6
# QIC for plan WO residential = 4831.9

#### 1.3.2 No correction, alt weight, GEEM Negative binomial ----------------------------------------------

x_values <- seq(158e4, 159e4, by = 100)

# Initialize a list to store models and their QICs
geem2_nocorr_alt_wgt_list <- list()

# Loop over each unique plan name to fit models
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_restr_proc2, tipo_de_plan_2_mod == plan_names[i])
  
  # Initialize variable to store the best QIC and associated model
  best_qic <- Inf
  best_model <- NULL
  best_x <- NA
  
  # Loop through each x value to find the one with the lowest QIC
  for (x in x_values) {
    # Fit the GEE model for the current subset using geem with negative binomial
    model <- geem(tr_outcome ~ policonsumo2 +
                    comp_bpsc_y3_severe+
                    edad_al_ing_1 + 
                    ano_nac_corr + 
                    susinidum_oh +
                    susinidum_coc +
                    susinidum_pbc +
                    susinidum_mar +
                    psycom_dum_study +
                    psycom_dum_with +
                    freq_cons_dum_5day +
                    cond_oc_dum_2inact +
                    cond_oc_dum_3unemp +
                    susprindum_coc +
                    susprindum_pbc +
                    susprindum_mar, # Include relevant predictors
                  id = id, 
                  data = current_data,
                  weight= current_data$iiw_nocorr_alt_st,
                  family = MASS::negative.binomial(x),
                  corstr = "independence")
    
    if (model$converged) {
      # Calculate QIC for the model
      model_qic <- MuMIn::QIC(model)
      
      # Check if this model has a better QIC
      if (model_qic < best_qic) {
        best_qic <- model_qic
        best_model <- model
        best_x <- x
      }
    }
  }
  
  # Store the best model and its details in the list
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  geem2_nocorr_alt_wgt_list[[paste("model", model_name, sep = "_")]] <- list(best_model = best_model, best_x = best_x, best_qic = best_qic)
  
  # Optional: Print out some information on progress
  cat(sprintf("Best QIC for plan %s with x = %.1f: %f\n", plan_names[i], best_x, best_qic))
}
# Best QIC for plan basic ambulatory with x = 1588300.0: 19457.646069
# Best QIC for plan GP intensive ambulatory with x = 1588300.0: 22268.120241
# Best QIC for plan GP residential with x = 1588300.0: 9826.117679
# Best QIC for plan WO intensive ambulatory with x = 1588300.0: 3426.650151
# Best QIC for plan WO residential with x = 1588300.0: 4831.902116

# Check the result for a particular plan
print(geem2_nocorr_alt_wgt_list[[1]])



## 2. After PH ---------------------------------------------------------

#summary(data_mine_miss_restr_proc2_iiw_after_ph$iiw_after_ph_st)
#summary(data_mine_miss_restr_proc2_iiw_after_ph_alt$iiw_after_ph_st)

### 2.1. After PH, no weight ----------------------------------------------

# Initialize a list to store models
afterph_nowgt_list <- list()

# Loop over each unique plan name to fit a model
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_restr_proc2_iiw_after_ph, tipo_de_plan_2_mod == plan_names[i])
  
  # Fit the GEE model for the current subset
  model <- geese(tr_outcome ~ policonsumo2 +
                   comp_bpsc_y3_severe+
                   edad_al_ing_1 + 
                   ano_nac_corr + 
                   susinidum_oh +
                   susinidum_coc +
                   susinidum_pbc +
                   susinidum_mar +
                   psycom_dum_study +
                   psycom_dum_with +
                   freq_cons_dum_5day +
                   cond_oc_dum_2inact +
                   cond_oc_dum_3unemp +
                   susprindum_coc +
                   susprindum_pbc +
                   susprindum_mar, 
                 id = id, 
                 data = current_data,
                 family = poisson(), 
                 #weight= current_data$iiw_nocorr_alt_st,
                 corstr = "independence", 
                 jack = T)
  
  # Assign the model to the list with a name based on the plan name
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  afterph_nowgt_list[[paste("model", model_name, sep = "_")]] <- model
}

#### 2.1.1 After PH, no weight, GEEM Poisson ----------------------------------------------

# Initialize a list to store models
geem_afterph_nowgt_list <- list()

# Loop over each unique plan name to fit a model
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_restr_proc2_iiw_after_ph, tipo_de_plan_2_mod == plan_names[i])
  
  # Fit the GEE model for the current subset using geem
  model <- geem(tr_outcome ~ policonsumo2 +
                  comp_bpsc_y3_severe+
                  edad_al_ing_1 + 
                  ano_nac_corr + 
                  susinidum_oh +
                  susinidum_coc +
                  susinidum_pbc +
                  susinidum_mar +
                  psycom_dum_study +
                  psycom_dum_with +
                  freq_cons_dum_5day +
                  cond_oc_dum_2inact +
                  cond_oc_dum_3unemp +
                  susprindum_coc +
                  susprindum_pbc +
                  susprindum_mar, # Include relevant predictors
                id = id, 
                data = current_data,
                family = poisson("log"), 
                corstr = "independence")
  
  # Assign the model to the list with a name based on the plan name
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  if(model$converged==FALSE){stop("model did not coverge")}
  cat(sprintf("QIC for plan %s = %.1f\n", plan_names[i], as.numeric(MuMIn::QIC(model))))
  geem_afterph_nowgt_list[[paste("model", model_name, sep = "_")]] <- model
}
# QIC for plan basic ambulatory = 19452.9
# QIC for plan GP intensive ambulatory = 22259.1
# QIC for plan GP residential = 9817.2
# QIC for plan WO intensive ambulatory = 3420.7
# QIC for plan WO residential = 4823.9

#### 2.1.2 After PH, no weight, GEEM Negative binomial ----------------------------------------------

# Define a sequence of x values
x_values <- seq(158e4, 159e4, by = 100)

# Initialize a list to store models and their QICs
geem2_afterph_nowgt_list <- list()

# Loop over each unique plan name to fit models
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_restr_proc2_iiw_after_ph, tipo_de_plan_2_mod == plan_names[i])
  
  # Initialize variable to store the best QIC and associated model
  best_qic <- Inf
  best_model <- NULL
  best_x <- NA
  
  # Loop through each x value to find the one with the lowest QIC
  for (x in x_values) {
    # Fit the GEE model for the current subset using geem with negative binomial
    model <- geem(tr_outcome ~ policonsumo2 +
                    comp_bpsc_y3_severe+
                    edad_al_ing_1 + 
                    ano_nac_corr + 
                    susinidum_oh +
                    susinidum_coc +
                    susinidum_pbc +
                    susinidum_mar +
                    psycom_dum_study +
                    psycom_dum_with +
                    freq_cons_dum_5day +
                    cond_oc_dum_2inact +
                    cond_oc_dum_3unemp +
                    susprindum_coc +
                    susprindum_pbc +
                    susprindum_mar, # Include relevant predictors
                  id = id, 
                  data = current_data,
                  family = MASS::negative.binomial(x),
                  corstr = "independence")
    
    if (model$converged) {
      # Calculate QIC for the model
      model_qic <- MuMIn::QIC(model)
      
      # Check if this model has a better QIC
      if (model_qic < best_qic) {
        best_qic <- model_qic
        best_model <- model
        best_x <- x
      }
    }
  }
  
  # Store the best model and its details in the list
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  geem2_afterph_nowgt_list[[paste("model", model_name, sep = "_")]] <- list(best_model = best_model, best_x = best_x, best_qic = best_qic)
  
  # Optional: Print out some information on progress
  cat(sprintf("Best QIC for plan %s with x = %.1f: %f\n", plan_names[i], best_x, best_qic))
}
# Best QIC for plan basic ambulatory with x = 1588300.0: 19452.859311
# Best QIC for plan GP intensive ambulatory with x = 1588300.0: 22259.103757
# Best QIC for plan GP residential with x = 1588300.0: 9817.174703
# Best QIC for plan WO intensive ambulatory with x = 1588300.0: 3420.678029
# Best QIC for plan WO residential with x = 1588300.0: 4823.862856

# Check the result for a particular plan
print(geem2_nocorr_nowgt_list[[1]])

invisible("As expected, QIC is equal, because the covariates and observations were the same")

### 2.2. After PH, weight ----------------------------------------------

# Initialize a list to store models
afterph_wgt_list <- list()

# Loop over each unique plan name to fit a model
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_restr_proc2_iiw_after_ph, tipo_de_plan_2_mod == plan_names[i])
  
  # Fit the GEE model for the current subset
  model <- geese(tr_outcome ~ policonsumo2 +
                   comp_bpsc_y3_severe+
                   edad_al_ing_1 + 
                   ano_nac_corr + 
                   susinidum_oh +
                   susinidum_coc +
                   susinidum_pbc +
                   susinidum_mar +
                   psycom_dum_study +
                   psycom_dum_with +
                   freq_cons_dum_5day +
                   cond_oc_dum_2inact +
                   cond_oc_dum_3unemp +
                   susprindum_coc +
                   susprindum_pbc +
                   susprindum_mar, 
                 id = id, 
                 data = current_data,
                 family = poisson(), 
                 weight= current_data$iiw_after_ph_st,
                 corstr = "independence", 
                 jack = T)
  
  # Assign the model to the list with a name based on the plan name
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  afterph_wgt_list[[paste("model", model_name, sep = "_")]] <- model
}

#### 2.2.1 After PH, weight, GEEM Poisson ----------------------------------------------

# Initialize a list to store models
geem_afterph_wgt_list <- list()

# Loop over each unique plan name to fit a model
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_restr_proc2_iiw_after_ph, tipo_de_plan_2_mod == plan_names[i])
  
  # Fit the GEE model for the current subset using geem
  model <- geem(tr_outcome ~ policonsumo2 +
                  comp_bpsc_y3_severe+
                  edad_al_ing_1 + 
                  ano_nac_corr + 
                  susinidum_oh +
                  susinidum_coc +
                  susinidum_pbc +
                  susinidum_mar +
                  psycom_dum_study +
                  psycom_dum_with +
                  freq_cons_dum_5day +
                  cond_oc_dum_2inact +
                  cond_oc_dum_3unemp +
                  susprindum_coc +
                  susprindum_pbc +
                  susprindum_mar, # Include relevant predictors
                id = id, 
                data = current_data,
                weight= current_data$iiw_after_ph_st,
                family = poisson("log"), 
                corstr = "independence")
  
  # Assign the model to the list with a name based on the plan name
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  if(model$converged==FALSE){stop("model did not coverge")}
  cat(sprintf("QIC for plan %s = %.1f\n", plan_names[i], as.numeric(MuMIn::QIC(model))))
  geem_afterph_wgt_list[[paste("model", model_name, sep = "_")]] <- model
}
# QIC for plan basic ambulatory = 19477.3
# QIC for plan GP intensive ambulatory = 22280.5
# QIC for plan GP residential = 9828.8
# QIC for plan WO intensive ambulatory = 3441.0
# QIC for plan WO residential = 4837.1

#### 2.2.2 After PH, weight, GEEM Negative binomial ----------------------------------------------


# Define a sequence of x values
x_values <- seq(158e4, 159e4, by = 100)

# Initialize a list to store models and their QICs
geem2_afterph_wgt_list <- list()

# Loop over each unique plan name to fit models
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_restr_proc2_iiw_after_ph, tipo_de_plan_2_mod == plan_names[i])
  
  # Initialize variable to store the best QIC and associated model
  best_qic <- Inf
  best_model <- NULL
  best_x <- NA
  
  # Loop through each x value to find the one with the lowest QIC
  for (x in x_values) {
    # Fit the GEE model for the current subset using geem with negative binomial
    model <- geem(tr_outcome ~ policonsumo2 +
                    comp_bpsc_y3_severe+
                    edad_al_ing_1 + 
                    ano_nac_corr + 
                    susinidum_oh +
                    susinidum_coc +
                    susinidum_pbc +
                    susinidum_mar +
                    psycom_dum_study +
                    psycom_dum_with +
                    freq_cons_dum_5day +
                    cond_oc_dum_2inact +
                    cond_oc_dum_3unemp +
                    susprindum_coc +
                    susprindum_pbc +
                    susprindum_mar, # Include relevant predictors
                  id = id, 
                  data = current_data,
                  weight= current_data$iiw_after_ph_st,
                  family = MASS::negative.binomial(x),
                  corstr = "independence")
    
    if (model$converged) {
      # Calculate QIC for the model
      model_qic <- MuMIn::QIC(model)
      
      # Check if this model has a better QIC
      if (model_qic < best_qic) {
        best_qic <- model_qic
        best_model <- model
        best_x <- x
      }
    }
  }
  
  # Store the best model and its details in the list
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  geem2_afterph_wgt_list[[paste("model", model_name, sep = "_")]] <- list(best_model = best_model, best_x = best_x, best_qic = best_qic)
  
  # Optional: Print out some information on progress
  cat(sprintf("Best QIC for plan %s with x = %.1f: %f\n", plan_names[i], best_x, best_qic))
}
# Best QIC for plan basic ambulatory with x = 1588300.0: 19477.330613
# Best QIC for plan GP intensive ambulatory with x = 1588300.0: 22280.505800
# Best QIC for plan GP residential with x = 1588300.0: 9828.771323
# Best QIC for plan WO intensive ambulatory with x = 1588300.0: 3441.006449
# Best QIC for plan WO residential with x = 1588300.0: 4837.095539

# Check the result for a particular plan
print(geem2_afterph_wgt_list[[1]])


### 2.3. After PH, alt. weight ----------------------------------------------

# Initialize a list to store models
afterph_alt_wgt_list <- list()

# Loop over each unique plan name to fit a model
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_restr_proc2_iiw_after_ph_alt, tipo_de_plan_2_mod == plan_names[i])
  
  # Fit the GEE model for the current subset
  model <- geese(tr_outcome ~ policonsumo2 +
                   comp_bpsc_y3_severe+
                   edad_al_ing_1 + 
                   ano_nac_corr + 
                   susinidum_oh +
                   susinidum_coc +
                   susinidum_pbc +
                   susinidum_mar +
                   psycom_dum_study +
                   psycom_dum_with +
                   freq_cons_dum_5day +
                   cond_oc_dum_2inact +
                   cond_oc_dum_3unemp +
                   susprindum_coc +
                   susprindum_pbc +
                   susprindum_mar, 
                 id = id, 
                 data = current_data,
                 family = poisson(), 
                 weight= current_data$iiw_after_ph_st,
                 corstr = "independence", 
                 jack = T)
  
  # Assign the model to the list with a name based on the plan name
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  afterph_alt_wgt_list[[paste("model", model_name, sep = "_")]] <- model
}


#### 2.3.1 After PH, alt weight, GEEM Poisson ----------------------------------------------

# Initialize a list to store models
geem_afterph_alt_wgt_list <- list()

# Loop over each unique plan name to fit a model
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_restr_proc2_iiw_after_ph_alt, tipo_de_plan_2_mod == plan_names[i])
  
  # Fit the GEE model for the current subset using geem
  model <- geem(tr_outcome ~ policonsumo2 +
                  comp_bpsc_y3_severe+
                  edad_al_ing_1 + 
                  ano_nac_corr + 
                  susinidum_oh +
                  susinidum_coc +
                  susinidum_pbc +
                  susinidum_mar +
                  psycom_dum_study +
                  psycom_dum_with +
                  freq_cons_dum_5day +
                  cond_oc_dum_2inact +
                  cond_oc_dum_3unemp +
                  susprindum_coc +
                  susprindum_pbc +
                  susprindum_mar, # Include relevant predictors
                id = id, 
                data = current_data,
                weight= current_data$iiw_after_ph_st,
                family = poisson("log"), 
                corstr = "independence")
  
  # Assign the model to the list with a name based on the plan name
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  if(model$converged==FALSE){stop("model did not coverge")}
  cat(sprintf("QIC for plan %s = %.1f\n", plan_names[i], as.numeric(MuMIn::QIC(model))))
  geem_afterph_alt_wgt_list[[paste("model", model_name, sep = "_")]] <- model
}
# QIC for plan basic ambulatory = 19514.7
# QIC for plan GP intensive ambulatory = 22318.8
# QIC for plan GP residential = 9865.7
# QIC for plan WO intensive ambulatory = 3463.6
# QIC for plan WO residential = 4857.5

#### 2.3.2 After PH, alt weight, GEEM Negative binomial ----------------------------------------------

# Define a sequence of x values
x_values <- seq(158e4, 159e4, by = 100)

# Initialize a list to store models and their QICs
geem2_afterph_alt_wgt_list <- list()

# Loop over each unique plan name to fit models
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_restr_proc2_iiw_after_ph_alt, tipo_de_plan_2_mod == plan_names[i])
  
  # Initialize variable to store the best QIC and associated model
  best_qic <- Inf
  best_model <- NULL
  best_x <- NA
  
  # Loop through each x value to find the one with the lowest QIC
  for (x in x_values) {
    # Fit the GEE model for the current subset using geem with negative binomial
    model <- geem(tr_outcome ~ policonsumo2 +
                    comp_bpsc_y3_severe+
                    edad_al_ing_1 + 
                    ano_nac_corr + 
                    susinidum_oh +
                    susinidum_coc +
                    susinidum_pbc +
                    susinidum_mar +
                    psycom_dum_study +
                    psycom_dum_with +
                    freq_cons_dum_5day +
                    cond_oc_dum_2inact +
                    cond_oc_dum_3unemp +
                    susprindum_coc +
                    susprindum_pbc +
                    susprindum_mar, # Include relevant predictors
                  id = id, 
                  data = current_data,
                  weight= current_data$iiw_after_ph_st,
                  family = MASS::negative.binomial(x),
                  corstr = "independence")
    
    if (model$converged) {
      # Calculate QIC for the model
      model_qic <- MuMIn::QIC(model)
      
      # Check if this model has a better QIC
      if (model_qic < best_qic) {
        best_qic <- model_qic
        best_model <- model
        best_x <- x
      }
    }
  }
  
  # Store the best model and its details in the list
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  geem2_afterph_alt_wgt_list[[paste("model", model_name, sep = "_")]] <- list(best_model = best_model, best_x = best_x, best_qic = best_qic)
  
  # Optional: Print out some information on progress
  cat(sprintf("Best QIC for plan %s with x = %.1f: %f\n", plan_names[i], best_x, best_qic))
}
# Best QIC for plan basic ambulatory with x = 1588300.0: 19514.731908
# Best QIC for plan GP intensive ambulatory with x = 1588300.0: 22318.800489
# Best QIC for plan GP residential with x = 1588300.0: 9865.659326
# Best QIC for plan WO intensive ambulatory with x = 1588300.0: 3463.577861
# Best QIC for plan WO residential with x = 1588300.0: 4857.472774

# Check the result for a particular plan
print(geem2_afterph_alt_wgt_list[[1]])



## 3. Stratifying ---------------------------------------------------------

# summary(data_mine_miss_proc2_iiw_strata$iiw_strata_st)
# 
# summary(data_mine_miss_proc2_iiw_strata_alt$iiw_strata_st)
# 
# summary(data_mine_miss_proc2_iiw_strata_alt_alt$iiw_strata_st)

### 3.1. Stratifying, no weight ----------------------------------------------

# Initialize a list to store models
strata_nowgt_list <- list()

# Loop over each unique plan name to fit a model
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_proc2_iiw_strata, tipo_de_plan_2_mod == plan_names[i])
  
  # Fit the GEE model for the current subset
  model <- geese(tr_outcome ~ policonsumo2 +
                   comp_bpsc_y3_severe+
                   edad_al_ing_1 + 
                   ano_nac_corr + 
                   susinidum_oh +
                   susinidum_coc +
                   susinidum_pbc +
                   susinidum_mar +
                   psycom_dum_study +
                   psycom_dum_with +
                   freq_cons_dum_5day +
                   cond_oc_dum_2inact +
                   cond_oc_dum_3unemp +
                   susprindum_coc +
                   susprindum_pbc +
                   susprindum_mar, 
                 id = id, 
                 data = current_data,
                 family = poisson(), 
                 #weight= current_data$iiw_nocorr_alt_st,
                 corstr = "independence", 
                 jack = T)
  
  # Assign the model to the list with a name based on the plan name
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  strata_nowgt_list[[paste("model", model_name, sep = "_")]] <- model
}

#### 3.1.1 Stratifying, no weight, GEEM Poisson ----------------------------------------------

# Initialize a list to store models
geem_strata_nowgt_list <- list()

# Loop over each unique plan name to fit a model
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_proc2_iiw_strata, tipo_de_plan_2_mod == plan_names[i])
  
  # Fit the GEE model for the current subset using geem
  model <- geem(tr_outcome ~ policonsumo2 +
                  comp_bpsc_y3_severe+
                  edad_al_ing_1 + 
                  ano_nac_corr + 
                  susinidum_oh +
                  susinidum_coc +
                  susinidum_pbc +
                  susinidum_mar +
                  psycom_dum_study +
                  psycom_dum_with +
                  freq_cons_dum_5day +
                  cond_oc_dum_2inact +
                  cond_oc_dum_3unemp +
                  susprindum_coc +
                  susprindum_pbc +
                  susprindum_mar, # Include relevant predictors
                id = id, 
                data = current_data,
                family = poisson("log"), 
                corstr = "independence")
  
  # Assign the model to the list with a name based on the plan name
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  if(model$converged==FALSE){stop("model did not coverge")}
  cat(sprintf("QIC for plan %s = %.1f\n", plan_names[i], as.numeric(MuMIn::QIC(model))))
  geem_strata_nowgt_list[[paste("model", model_name, sep = "_")]] <- model
}
# QIC for plan basic ambulatory = 19452.9
# QIC for plan GP intensive ambulatory = 22259.1
# QIC for plan GP residential = 9817.2
# QIC for plan WO intensive ambulatory = 3420.7
# QIC for plan WO residential = 4823.9

#### 3.1.2 Stratifying, no weight, GEEM Negative binomial ----------------------------------------------

# Define a sequence of x values
x_values <- seq(158e4, 159e4, by = 100)

# Initialize a list to store models and their QICs
geem2_strata_nowgt_list <- list()

# Loop over each unique plan name to fit models
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_proc2_iiw_strata, tipo_de_plan_2_mod == plan_names[i])
  
  # Initialize variable to store the best QIC and associated model
  best_qic <- Inf
  best_model <- NULL
  best_x <- NA
  
  # Loop through each x value to find the one with the lowest QIC
  for (x in x_values) {
    # Fit the GEE model for the current subset using geem with negative binomial
    model <- geem(tr_outcome ~ policonsumo2 +
                    comp_bpsc_y3_severe+
                    edad_al_ing_1 + 
                    ano_nac_corr + 
                    susinidum_oh +
                    susinidum_coc +
                    susinidum_pbc +
                    susinidum_mar +
                    psycom_dum_study +
                    psycom_dum_with +
                    freq_cons_dum_5day +
                    cond_oc_dum_2inact +
                    cond_oc_dum_3unemp +
                    susprindum_coc +
                    susprindum_pbc +
                    susprindum_mar, # Include relevant predictors
                  id = id, 
                  data = current_data,
                  family = MASS::negative.binomial(x),
                  corstr = "independence")
    
    if (model$converged) {
      # Calculate QIC for the model
      model_qic <- MuMIn::QIC(model)
      
      # Check if this model has a better QIC
      if (model_qic < best_qic) {
        best_qic <- model_qic
        best_model <- model
        best_x <- x
      }
    }
  }
  
  # Store the best model and its details in the list
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  geem2_strata_nowgt_list[[paste("model", model_name, sep = "_")]] <- list(best_model = best_model, best_x = best_x, best_qic = best_qic)
  
  # Optional: Print out some information on progress
  cat(sprintf("Best QIC for plan %s with x = %.1f: %f\n", plan_names[i], best_x, best_qic))
}
# Best QIC for plan basic ambulatory with x = 1588300.0: 19452.859311
# Best QIC for plan GP intensive ambulatory with x = 1588300.0: 22259.103757
# Best QIC for plan GP residential with x = 1588300.0: 9817.174703
# Best QIC for plan WO intensive ambulatory with x = 1588300.0: 3420.678029
# Best QIC for plan WO residential with x = 1588300.0: 4823.862856

# Check the result for a particular plan
print(geem2_nocorr_nowgt_list[[1]])

### 3.2. Stratifying, weight ----------------------------------------------

# Initialize a list to store models
strata_wgt_list <- list()

# Loop over each unique plan name to fit a model
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_proc2_iiw_strata, tipo_de_plan_2_mod == plan_names[i])
  
  # Fit the GEE model for the current subset
  model <- geese(tr_outcome ~ policonsumo2 +
                   comp_bpsc_y3_severe+
                   edad_al_ing_1 + 
                   ano_nac_corr + 
                   susinidum_oh +
                   susinidum_coc +
                   susinidum_pbc +
                   susinidum_mar +
                   psycom_dum_study +
                   psycom_dum_with +
                   freq_cons_dum_5day +
                   cond_oc_dum_2inact +
                   cond_oc_dum_3unemp +
                   susprindum_coc +
                   susprindum_pbc +
                   susprindum_mar, 
                 id = id, 
                 data = current_data,
                 family = poisson(), 
                 weight= current_data$iiw_strata_st,
                 corstr = "independence", 
                 jack = T)
  
  # Assign the model to the list with a name based on the plan name
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  strata_wgt_list[[paste("model", model_name, sep = "_")]] <- model
}

#### 3.2.1 Stratifying, weight, GEEM Poisson ----------------------------------------------

# Initialize a list to store models
geem_strata_wgt_list <- list()

# Loop over each unique plan name to fit a model
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_proc2_iiw_strata, tipo_de_plan_2_mod == plan_names[i])
  
  # Fit the GEE model for the current subset using geem
  model <- geem(tr_outcome ~ policonsumo2 +
                  comp_bpsc_y3_severe+
                  edad_al_ing_1 + 
                  ano_nac_corr + 
                  susinidum_oh +
                  susinidum_coc +
                  susinidum_pbc +
                  susinidum_mar +
                  psycom_dum_study +
                  psycom_dum_with +
                  freq_cons_dum_5day +
                  cond_oc_dum_2inact +
                  cond_oc_dum_3unemp +
                  susprindum_coc +
                  susprindum_pbc +
                  susprindum_mar, # Include relevant predictors
                id = id, 
                data = current_data,
                weight= current_data$iiw_strata_st,
                family = poisson("log"), 
                corstr = "independence")
  
  # Assign the model to the list with a name based on the plan name
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  if(model$converged==FALSE){stop("model did not coverge")}
  cat(sprintf("QIC for plan %s = %.1f\n", plan_names[i], as.numeric(MuMIn::QIC(model))))
  geem_strata_wgt_list[[paste("model", model_name, sep = "_")]] <- model
}
# QIC for plan basic ambulatory = 19474.7
# QIC for plan GP intensive ambulatory = 22278.5
# QIC for plan GP residential = 9836.9
# QIC for plan WO intensive ambulatory = 3448.0
# QIC for plan WO residential = 4846.9

#### 3.2.2 Stratifying, weight, GEEM Negative binomial ----------------------------------------------


# Define a sequence of x values
x_values <- seq(158e4, 159e4, by = 100)

# Initialize a list to store models and their QICs
geem2_strata_wgt_list <- list()

# Loop over each unique plan name to fit models
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_proc2_iiw_strata, tipo_de_plan_2_mod == plan_names[i])
  
  # Initialize variable to store the best QIC and associated model
  best_qic <- Inf
  best_model <- NULL
  best_x <- NA
  
  # Loop through each x value to find the one with the lowest QIC
  for (x in x_values) {
    # Fit the GEE model for the current subset using geem with negative binomial
    model <- geem(tr_outcome ~ policonsumo2 +
                    comp_bpsc_y3_severe+
                    edad_al_ing_1 + 
                    ano_nac_corr + 
                    susinidum_oh +
                    susinidum_coc +
                    susinidum_pbc +
                    susinidum_mar +
                    psycom_dum_study +
                    psycom_dum_with +
                    freq_cons_dum_5day +
                    cond_oc_dum_2inact +
                    cond_oc_dum_3unemp +
                    susprindum_coc +
                    susprindum_pbc +
                    susprindum_mar, # Include relevant predictors
                  id = id, 
                  data = current_data,
                  weight= current_data$iiw_strata_st,
                  family = MASS::negative.binomial(x),
                  corstr = "independence")
    
    if (model$converged) {
      # Calculate QIC for the model
      model_qic <- MuMIn::QIC(model)
      
      # Check if this model has a better QIC
      if (model_qic < best_qic) {
        best_qic <- model_qic
        best_model <- model
        best_x <- x
      }
    }
  }
  
  # Store the best model and its details in the list
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  geem2_strata_wgt_list[[paste("model", model_name, sep = "_")]] <- list(best_model = best_model, best_x = best_x, best_qic = best_qic)
  
  # Optional: Print out some information on progress
  cat(sprintf("Best QIC for plan %s with x = %.1f: %f\n", plan_names[i], best_x, best_qic))
}
# Best QIC for plan basic ambulatory with x = 1588300.0: 19471.069994
# Best QIC for plan GP intensive ambulatory with x = 1588300.0: 22275.839993
# Best QIC for plan GP residential with x = 1588300.0: 9837.039458
# Best QIC for plan WO intensive ambulatory with x = 1588300.0: 3441.723758
# Best QIC for plan WO residential with x = 1588300.0: 4844.010341

# Check the result for a particular plan
print(geem2_strata_wgt_list[[1]])

### 3.3. Stratifying, alt. weight ----------------------------------------------

# Initialize a list to store models
strata_alt_wgt_list <- list()

# Loop over each unique plan name to fit a model
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_proc2_iiw_strata_alt, tipo_de_plan_2_mod == plan_names[i])
  
  # Fit the GEE model for the current subset
  model <- geese(tr_outcome ~ policonsumo2 +
                   comp_bpsc_y3_severe+
                   edad_al_ing_1 + 
                   ano_nac_corr + 
                   susinidum_oh +
                   susinidum_coc +
                   susinidum_pbc +
                   susinidum_mar +
                   psycom_dum_study +
                   psycom_dum_with +
                   freq_cons_dum_5day +
                   cond_oc_dum_2inact +
                   cond_oc_dum_3unemp +
                   susprindum_coc +
                   susprindum_pbc +
                   susprindum_mar, 
                 id = id, 
                 data = current_data,
                 family = poisson(), 
                 weight= current_data$iiw_strata_st,
                 corstr = "independence", 
                 jack = T)
  
  # Assign the model to the list with a name based on the plan name
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  strata_alt_wgt_list[[paste("model", model_name, sep = "_")]] <- model
}


#### 3.3.1 No correction, alt weight, GEEM Poisson ----------------------------------------------

# Initialize a list to store models
geem_strata_alt_wgt_list <- list()

# Loop over each unique plan name to fit a model
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_proc2_iiw_strata_alt, tipo_de_plan_2_mod == plan_names[i])
  
  # Fit the GEE model for the current subset using geem
  model <- geem(tr_outcome ~ policonsumo2 +
                  comp_bpsc_y3_severe+
                  edad_al_ing_1 + 
                  ano_nac_corr + 
                  susinidum_oh +
                  susinidum_coc +
                  susinidum_pbc +
                  susinidum_mar +
                  psycom_dum_study +
                  psycom_dum_with +
                  freq_cons_dum_5day +
                  cond_oc_dum_2inact +
                  cond_oc_dum_3unemp +
                  susprindum_coc +
                  susprindum_pbc +
                  susprindum_mar, # Include relevant predictors
                id = id, 
                data = current_data,
                weight= current_data$iiw_strata_st,
                family = poisson("log"), 
                corstr = "independence")
  
  # Assign the model to the list with a name based on the plan name
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  if(model$converged==FALSE){stop("model did not coverge")}
  cat(sprintf("QIC for plan %s = %.1f\n", plan_names[i], as.numeric(MuMIn::QIC(model))))
  geem_strata_alt_wgt_list[[paste("model", model_name, sep = "_")]] <- model
}
# QIC for plan basic ambulatory = 19465.3
# QIC for plan GP intensive ambulatory = 22265.5
# QIC for plan GP residential = 9836.6
# QIC for plan WO intensive ambulatory = 3434.3
# QIC for plan WO residential = 4839.3

invisible("Alternative, better than main")

#### 3.3.2 No correction, alt weight, GEEM Negative binomial ----------------------------------------------


# Define a sequence of x values
x_values <- seq(158e4, 159e4, by = 100)

# Initialize a list to store models and their QICs
geem2_strata_alt_wgt_list <- list()

# Loop over each unique plan name to fit models
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_proc2_iiw_strata_alt, tipo_de_plan_2_mod == plan_names[i])
  
  # Initialize variable to store the best QIC and associated model
  best_qic <- Inf
  best_model <- NULL
  best_x <- NA
  
  # Loop through each x value to find the one with the lowest QIC
  for (x in x_values) {
    # Fit the GEE model for the current subset using geem with negative binomial
    model <- geem(tr_outcome ~ policonsumo2 +
                    comp_bpsc_y3_severe+
                    edad_al_ing_1 + 
                    ano_nac_corr + 
                    susinidum_oh +
                    susinidum_coc +
                    susinidum_pbc +
                    susinidum_mar +
                    psycom_dum_study +
                    psycom_dum_with +
                    freq_cons_dum_5day +
                    cond_oc_dum_2inact +
                    cond_oc_dum_3unemp +
                    susprindum_coc +
                    susprindum_pbc +
                    susprindum_mar, # Include relevant predictors
                  id = id, 
                  data = current_data,
                  weight= current_data$iiw_strata_st,
                  family = MASS::negative.binomial(x),
                  corstr = "independence")
    
    if (model$converged) {
      # Calculate QIC for the model
      model_qic <- MuMIn::QIC(model)
      
      # Check if this model has a better QIC
      if (model_qic < best_qic) {
        best_qic <- model_qic
        best_model <- model
        best_x <- x
      }
    }
  }
  
  # Store the best model and its details in the list
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  geem2_strata_alt_wgt_list[[paste("model", model_name, sep = "_")]] <- list(best_model = best_model, best_x = best_x, best_qic = best_qic)
  
  # Optional: Print out some information on progress
  cat(sprintf("Best QIC for plan %s with x = %.1f: %f\n", plan_names[i], best_x, best_qic))
}
# Best QIC for plan basic ambulatory with x = 1588300.0: 19465.498970
# Best QIC for plan GP intensive ambulatory with x = 1588300.0: 22265.533863
# Best QIC for plan GP residential with x = 1588300.0: 9836.723230
# Best QIC for plan WO intensive ambulatory with x = 1588300.0: 3435.308412
# Best QIC for plan WO residential with x = 1588300.0: 4839.167216

# Check the result for a particular plan
print(geem2_strata_alt_wgt_list[[1]])

### 3.4. Stratifying, 2nd alt. weight ----------------------------------------------

# Initialize a list to store models
strata_alt_alt_wgt_list <- list()

# Loop over each unique plan name to fit a model
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_proc2_iiw_strata_alt_alt, tipo_de_plan_2_mod == plan_names[i])
  
  # Fit the GEE model for the current subset
  model <- geese(tr_outcome ~ policonsumo2 +
                   comp_bpsc_y3_severe+
                   edad_al_ing_1 + 
                   ano_nac_corr + 
                   susinidum_oh +
                   susinidum_coc +
                   susinidum_pbc +
                   susinidum_mar +
                   psycom_dum_study +
                   psycom_dum_with +
                   freq_cons_dum_5day +
                   cond_oc_dum_2inact +
                   cond_oc_dum_3unemp +
                   susprindum_coc +
                   susprindum_pbc +
                   susprindum_mar, 
                 id = id, 
                 data = current_data,
                 family = poisson(), 
                 weight= current_data$iiw_strata_st,
                 corstr = "independence", 
                 jack = T)
  
  # Assign the model to the list with a name based on the plan name
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  strata_alt_alt_wgt_list[[paste("model", model_name, sep = "_")]] <- model
}

#### 3.4.1 No correction, alt weight, GEEM Poisson ----------------------------------------------

# Initialize a list to store models
geem_strata_alt_alt_wgt_list <- list()

# Loop over each unique plan name to fit a model
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_proc2_iiw_strata_alt_alt, tipo_de_plan_2_mod == plan_names[i])
  
  # Fit the GEE model for the current subset using geem
  model <- geem(tr_outcome ~ policonsumo2 +
                  comp_bpsc_y3_severe+
                  edad_al_ing_1 + 
                  ano_nac_corr + 
                  susinidum_oh +
                  susinidum_coc +
                  susinidum_pbc +
                  susinidum_mar +
                  psycom_dum_study +
                  psycom_dum_with +
                  freq_cons_dum_5day +
                  cond_oc_dum_2inact +
                  cond_oc_dum_3unemp +
                  susprindum_coc +
                  susprindum_pbc +
                  susprindum_mar, # Include relevant predictors
                id = id, 
                data = current_data,
                weight= current_data$iiw_strata_st,
                family = poisson("log"), 
                corstr = "independence")
  
  # Assign the model to the list with a name based on the plan name
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  if(model$converged==FALSE){stop("model did not coverge")}
  cat(sprintf("QIC for plan %s = %.1f\n", plan_names[i], as.numeric(MuMIn::QIC(model))))
  geem_strata_alt_alt_wgt_list[[paste("model", model_name, sep = "_")]] <- model
}
# QIC for plan basic ambulatory = 19502.9
# QIC for plan GP intensive ambulatory = 22276.4
# QIC for plan GP residential = 9884.0
# QIC for plan WO intensive ambulatory = 3465.1
# QIC for plan WO residential = 4870.5

invisible("Alternative, worst model of strata")

#### 3.4.2 No correction, alt weight, GEEM Negative binomial ----------------------------------------------


# Define a sequence of x values
x_values <- seq(158e4, 159e4, by = 100)

# Initialize a list to store models and their QICs
geem2_strata_alt_alt_wgt_list <- list()

# Loop over each unique plan name to fit models
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_proc2_iiw_strata_alt_alt, tipo_de_plan_2_mod == plan_names[i])
  
  # Initialize variable to store the best QIC and associated model
  best_qic <- Inf
  best_model <- NULL
  best_x <- NA
  
  # Loop through each x value to find the one with the lowest QIC
  for (x in x_values) {
    # Fit the GEE model for the current subset using geem with negative binomial
    model <- geem(tr_outcome ~ policonsumo2 +
                    comp_bpsc_y3_severe+
                    edad_al_ing_1 + 
                    ano_nac_corr + 
                    susinidum_oh +
                    susinidum_coc +
                    susinidum_pbc +
                    susinidum_mar +
                    psycom_dum_study +
                    psycom_dum_with +
                    freq_cons_dum_5day +
                    cond_oc_dum_2inact +
                    cond_oc_dum_3unemp +
                    susprindum_coc +
                    susprindum_pbc +
                    susprindum_mar, # Include relevant predictors
                  id = id, 
                  data = current_data,
                  weight= current_data$iiw_strata_st,
                  family = MASS::negative.binomial(x),
                  corstr = "independence")
    
    if (model$converged) {
      # Calculate QIC for the model
      model_qic <- MuMIn::QIC(model)
      
      # Check if this model has a better QIC
      if (model_qic < best_qic) {
        best_qic <- model_qic
        best_model <- model
        best_x <- x
      }
    }
  }
  
  # Store the best model and its details in the list
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  geem2_strata_alt_alt_wgt_list[[paste("model", model_name, sep = "_")]] <- list(best_model = best_model, best_x = best_x, best_qic = best_qic)
  
  # Optional: Print out some information on progress
  cat(sprintf("Best QIC for plan %s with x = %.1f: %f\n", plan_names[i], best_x, best_qic))
}
# Best QIC for plan basic ambulatory with x = 1588300.0: 19502.863188
# Best QIC for plan GP intensive ambulatory with x = 1588300.0: 22276.375237
# Best QIC for plan GP residential with x = 1588300.0: 9884.015600
# Best QIC for plan WO intensive ambulatory with x = 1588300.0: 3465.093976
# Best QIC for plan WO residential with x = 1588300.0: 4870.474169

# Check the result for a particular plan
print(geem2_strata_alt_alt_wgt_list[[1]])


# Model output ----------------------------------------------

types_of_model<-
  c("No correction",
    "No correction, main",
    "No correction, alternative",
    "Correction for PH, no weight",
    "Correction for PH, main",
    "Correction for PH, alternative",
    "Stratifying, no weight",
    "Stratifying, main",
    "Stratifying, alternative",
    "Stratifying, 2nd alternative, main"
)
types_of_model2<-
  c("No correction",
    "No correction, NB",
    "No correction, main",
    "No correction, main, NB",
    "No correction, alternative",
    "No correction, alternative, NB",
    "Correction for PH, no weight",
    "Correction for PH, no weight, NB",
    "Correction for PH, main",
    "Correction for PH, main, NB",
    "Correction for PH, alternative",
    "Correction for PH, alternative, NB",
    "Stratified, no weight",
    "Stratified, no weight, NB",
    "Stratified, main",
    "Stratified, main, NB",
    "Stratified, alternative",
    "Stratified, alternative, NB",
    "Stratified, 2nd alternative",
    "Stratified, 2nd alternative, NB"    
  )

plan_names[1]
cbind.data.frame(model= types_of_model,
rbind.data.frame(
dplyr::filter(tidy_geese_model(nocorr_nowgt_list[[1]]), Term=="policonsumo2"),
dplyr::filter(tidy_geese_model(nocorr_wgt_list[[1]]), Term=="policonsumo2"),
dplyr::filter(tidy_geese_model(nocorr_alt_wgt_list[[1]]), Term=="policonsumo2"),
dplyr::filter(tidy_geese_model(afterph_nowgt_list[[1]]), Term=="policonsumo2"),
dplyr::filter(tidy_geese_model(afterph_wgt_list[[1]]), Term=="policonsumo2"),
dplyr::filter(tidy_geese_model(afterph_alt_wgt_list[[1]]), Term=="policonsumo2"),
dplyr::filter(tidy_geese_model(strata_nowgt_list[[1]]), Term=="policonsumo2"),
dplyr::filter(tidy_geese_model(strata_wgt_list[[1]]), Term=="policonsumo2"),
dplyr::filter(tidy_geese_model(strata_alt_wgt_list[[1]]), Term=="policonsumo2"),
dplyr::filter(tidy_geese_model(strata_alt_alt_wgt_list[[1]]), Term=="policonsumo2")))%>% 
  {
    copiar_nombres2(.)
    assign(paste0("result_data_geese_", gsub(" ","_",plan_names[1])), ., envir = .GlobalEnv)
    print(.)
  }

cbind.data.frame(model= types_of_model2,
                 QIC= c(as.numeric(QIC(geem_nocorr_nowgt_list[[1]])),
                        as.numeric(geem2_nocorr_nowgt_list[[1]]$best_qic),
                        as.numeric(QIC(geem_nocorr_wgt_list[[1]])),
                        as.numeric(geem2_nocorr_wgt_list[[1]]$best_qic),
                        as.numeric(QIC(geem_nocorr_alt_wgt_list[[1]])),
                        as.numeric(geem2_nocorr_alt_wgt_list[[1]]$best_qic),
                        as.numeric(QIC(geem_afterph_nowgt_list[[1]])),
                        as.numeric(geem2_afterph_nowgt_list[[1]]$best_qic),
                        as.numeric(QIC(geem_afterph_wgt_list[[1]])),
                        as.numeric(geem2_afterph_wgt_list[[1]]$best_qic),
                        as.numeric(QIC(geem_afterph_alt_wgt_list[[1]])),
                        as.numeric(geem2_afterph_alt_wgt_list[[1]]$best_qic),
                        
                        as.numeric(QIC(geem_strata_nowgt_list[[1]])),
                        as.numeric(geem2_strata_nowgt_list[[1]]$best_qic),
                        as.numeric(QIC(geem_strata_wgt_list[[1]])),
                        as.numeric(geem2_strata_wgt_list[[1]]$best_qic),
                        as.numeric(QIC(geem_strata_alt_wgt_list[[1]])),
                        as.numeric(geem2_strata_alt_wgt_list[[1]]$best_qic),
                        as.numeric(QIC(geem_strata_alt_alt_wgt_list[[1]])),
                        as.numeric(geem2_strata_alt_alt_wgt_list[[1]]$best_qic)                        
                        ),
                 rbind.data.frame(
                   dplyr::filter(summary2.geem(geem_nocorr_nowgt_list[[1]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_nocorr_nowgt_list[[1]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_nocorr_wgt_list[[1]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_nocorr_wgt_list[[1]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_nocorr_alt_wgt_list[[1]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_nocorr_alt_wgt_list[[1]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_afterph_nowgt_list[[1]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_afterph_nowgt_list[[1]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_afterph_wgt_list[[1]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_afterph_wgt_list[[1]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_afterph_alt_wgt_list[[1]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_afterph_alt_wgt_list[[1]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   
                   dplyr::filter(summary2.geem(geem_strata_nowgt_list[[1]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_strata_nowgt_list[[1]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),                   
                   dplyr::filter(summary2.geem(geem_strata_wgt_list[[1]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_strata_wgt_list[[1]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_strata_alt_wgt_list[[1]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_strata_alt_wgt_list[[1]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_strata_alt_alt_wgt_list[[1]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_strata_alt_alt_wgt_list[[1]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2")
                   ))%>% 
  {
    copiar_nombres2(.)
    assign(paste0("result_data_geem_", gsub(" ","_",plan_names[1])), ., envir = .GlobalEnv)
    print(.)
  }

i=2
plan_names[i]
cbind.data.frame(model= types_of_model,
rbind.data.frame(
  dplyr::filter(tidy_geese_model(nocorr_nowgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(nocorr_wgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(nocorr_alt_wgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(afterph_nowgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(afterph_wgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(afterph_alt_wgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(strata_nowgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(strata_wgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(strata_alt_wgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(strata_alt_alt_wgt_list[[i]]), Term=="policonsumo2")))%>% 
  {
    copiar_nombres2(.)  
    print(.)
  }

#[1] "GP intensive ambulatory". significativo
cbind.data.frame(model= types_of_model2,
                 QIC= c(as.numeric(QIC(geem_nocorr_nowgt_list[[i]])),
                        as.numeric(geem2_nocorr_nowgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_nocorr_wgt_list[[i]])),
                        as.numeric(geem2_nocorr_wgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_nocorr_alt_wgt_list[[i]])),
                        as.numeric(geem2_nocorr_alt_wgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_afterph_nowgt_list[[i]])),
                        as.numeric(geem2_afterph_nowgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_afterph_wgt_list[[i]])),
                        as.numeric(geem2_afterph_wgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_afterph_alt_wgt_list[[i]])),
                        as.numeric(geem2_afterph_alt_wgt_list[[i]]$best_qic),
                        
                        as.numeric(QIC(geem_strata_nowgt_list[[i]])),
                        as.numeric(geem2_strata_nowgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_strata_wgt_list[[i]])),
                        as.numeric(geem2_strata_wgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_strata_alt_wgt_list[[i]])),
                        as.numeric(geem2_strata_alt_wgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_strata_alt_alt_wgt_list[[i]])),
                        as.numeric(geem2_strata_alt_alt_wgt_list[[i]]$best_qic)                        
                 ),
                 rbind.data.frame(
                   dplyr::filter(summary2.geem(geem_nocorr_nowgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_nocorr_nowgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_nocorr_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_nocorr_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_nocorr_alt_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_nocorr_alt_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_afterph_nowgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_afterph_nowgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_afterph_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_afterph_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_afterph_alt_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_afterph_alt_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   
                   dplyr::filter(summary2.geem(geem_strata_nowgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_strata_nowgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),                   
                   dplyr::filter(summary2.geem(geem_strata_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_strata_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_strata_alt_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_strata_alt_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_strata_alt_alt_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_strata_alt_alt_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2")
                 ))%>% 
  {
    copiar_nombres2(.)
    assign(paste0("result_data_geem_", gsub(" ","_",plan_names[i])), ., envir = .GlobalEnv)
    print(.)
  }


i=3
plan_names[i]
cbind.data.frame(model= types_of_model,
rbind.data.frame(
  dplyr::filter(tidy_geese_model(nocorr_nowgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(nocorr_wgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(nocorr_alt_wgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(afterph_nowgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(afterph_wgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(afterph_alt_wgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(strata_nowgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(strata_wgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(strata_alt_wgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(strata_alt_alt_wgt_list[[i]]), Term=="policonsumo2"))) %>% 
  {
    copiar_nombres2(.)  
    print(.)
  }
cbind.data.frame(model= types_of_model2,
                 QIC= c(as.numeric(QIC(geem_nocorr_nowgt_list[[i]])),
                        as.numeric(geem2_nocorr_nowgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_nocorr_wgt_list[[i]])),
                        as.numeric(geem2_nocorr_wgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_nocorr_alt_wgt_list[[i]])),
                        as.numeric(geem2_nocorr_alt_wgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_afterph_nowgt_list[[i]])),
                        as.numeric(geem2_afterph_nowgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_afterph_wgt_list[[i]])),
                        as.numeric(geem2_afterph_wgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_afterph_alt_wgt_list[[i]])),
                        as.numeric(geem2_afterph_alt_wgt_list[[i]]$best_qic),
                        
                        as.numeric(QIC(geem_strata_nowgt_list[[i]])),
                        as.numeric(geem2_strata_nowgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_strata_wgt_list[[i]])),
                        as.numeric(geem2_strata_wgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_strata_alt_wgt_list[[i]])),
                        as.numeric(geem2_strata_alt_wgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_strata_alt_alt_wgt_list[[i]])),
                        as.numeric(geem2_strata_alt_alt_wgt_list[[i]]$best_qic)                        
                 ),
                 rbind.data.frame(
                   dplyr::filter(summary2.geem(geem_nocorr_nowgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_nocorr_nowgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_nocorr_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_nocorr_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_nocorr_alt_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_nocorr_alt_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_afterph_nowgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_afterph_nowgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_afterph_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_afterph_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_afterph_alt_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_afterph_alt_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   
                   dplyr::filter(summary2.geem(geem_strata_nowgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_strata_nowgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),                   
                   dplyr::filter(summary2.geem(geem_strata_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_strata_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_strata_alt_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_strata_alt_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_strata_alt_alt_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_strata_alt_alt_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2")
                 ))%>% 
  {
    copiar_nombres2(.)
    assign(paste0("result_data_geem_", gsub(" ","_",plan_names[i])), ., envir = .GlobalEnv)
    print(.)
  }


i=4
plan_names[i]
cbind.data.frame(model= types_of_model,
rbind.data.frame(
  dplyr::filter(tidy_geese_model(nocorr_nowgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(nocorr_wgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(nocorr_alt_wgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(afterph_nowgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(afterph_wgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(afterph_alt_wgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(strata_nowgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(strata_wgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(strata_alt_wgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(strata_alt_alt_wgt_list[[i]]), Term=="policonsumo2"))) %>% 
  {
    copiar_nombres2(.)  
    print(.)
  }
cbind.data.frame(model= types_of_model2,
                 QIC= c(as.numeric(QIC(geem_nocorr_nowgt_list[[i]])),
                        as.numeric(geem2_nocorr_nowgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_nocorr_wgt_list[[i]])),
                        as.numeric(geem2_nocorr_wgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_nocorr_alt_wgt_list[[i]])),
                        as.numeric(geem2_nocorr_alt_wgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_afterph_nowgt_list[[i]])),
                        as.numeric(geem2_afterph_nowgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_afterph_wgt_list[[i]])),
                        as.numeric(geem2_afterph_wgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_afterph_alt_wgt_list[[i]])),
                        as.numeric(geem2_afterph_alt_wgt_list[[i]]$best_qic),
                        
                        as.numeric(QIC(geem_strata_nowgt_list[[i]])),
                        as.numeric(geem2_strata_nowgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_strata_wgt_list[[i]])),
                        as.numeric(geem2_strata_wgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_strata_alt_wgt_list[[i]])),
                        as.numeric(geem2_strata_alt_wgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_strata_alt_alt_wgt_list[[i]])),
                        as.numeric(geem2_strata_alt_alt_wgt_list[[i]]$best_qic)                        
                 ),
                 rbind.data.frame(
                   dplyr::filter(summary2.geem(geem_nocorr_nowgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_nocorr_nowgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_nocorr_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_nocorr_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_nocorr_alt_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_nocorr_alt_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_afterph_nowgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_afterph_nowgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_afterph_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_afterph_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_afterph_alt_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_afterph_alt_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   
                   dplyr::filter(summary2.geem(geem_strata_nowgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_strata_nowgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),                   
                   dplyr::filter(summary2.geem(geem_strata_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_strata_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_strata_alt_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_strata_alt_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_strata_alt_alt_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_strata_alt_alt_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2")
                 ))%>% 
  {
    copiar_nombres2(.)
    assign(paste0("result_data_geem_", gsub(" ","_",plan_names[i])), ., envir = .GlobalEnv)
    print(.)
  }

i=5
plan_names[i]
cbind.data.frame(model= types_of_model,
rbind.data.frame(
  dplyr::filter(tidy_geese_model(nocorr_nowgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(nocorr_wgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(nocorr_alt_wgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(afterph_nowgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(afterph_wgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(afterph_alt_wgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(strata_nowgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(strata_wgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(strata_alt_wgt_list[[i]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(strata_alt_alt_wgt_list[[i]]), Term=="policonsumo2"))) %>% 
  {
        copiar_nombres2(.)  
        print(.)
  }
  
#[1] "WO residential". significativo

cbind.data.frame(model= types_of_model2,
                 QIC= c(as.numeric(QIC(geem_nocorr_nowgt_list[[i]])),
                        as.numeric(geem2_nocorr_nowgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_nocorr_wgt_list[[i]])),
                        as.numeric(geem2_nocorr_wgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_nocorr_alt_wgt_list[[i]])),
                        as.numeric(geem2_nocorr_alt_wgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_afterph_nowgt_list[[i]])),
                        as.numeric(geem2_afterph_nowgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_afterph_wgt_list[[i]])),
                        as.numeric(geem2_afterph_wgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_afterph_alt_wgt_list[[i]])),
                        as.numeric(geem2_afterph_alt_wgt_list[[i]]$best_qic),
                        
                        as.numeric(QIC(geem_strata_nowgt_list[[i]])),
                        as.numeric(geem2_strata_nowgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_strata_wgt_list[[i]])),
                        as.numeric(geem2_strata_wgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_strata_alt_wgt_list[[i]])),
                        as.numeric(geem2_strata_alt_wgt_list[[i]]$best_qic),
                        as.numeric(QIC(geem_strata_alt_alt_wgt_list[[i]])),
                        as.numeric(geem2_strata_alt_alt_wgt_list[[i]]$best_qic)                        
                 ),
                 rbind.data.frame(
                   dplyr::filter(summary2.geem(geem_nocorr_nowgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_nocorr_nowgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_nocorr_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_nocorr_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_nocorr_alt_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_nocorr_alt_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_afterph_nowgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_afterph_nowgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_afterph_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_afterph_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_afterph_alt_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_afterph_alt_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   
                   dplyr::filter(summary2.geem(geem_strata_nowgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_strata_nowgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),                   
                   dplyr::filter(summary2.geem(geem_strata_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_strata_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_strata_alt_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_strata_alt_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem_strata_alt_alt_wgt_list[[i]],exponentiate=T, digits=2),Term=="policonsumo2"),
                   dplyr::filter(summary2.geem(geem2_strata_alt_alt_wgt_list[[i]]$best_model,exponentiate=T, digits=2),Term=="policonsumo2")
                 ))%>% 
  {
    copiar_nombres2(.)
    assign(paste0("result_data_geem_", gsub(" ","_",plan_names[i])), ., envir = .GlobalEnv)
    print(.)
  }


rbind.data.frame(cbind.data.frame(setting=plan_names[1],result_data_geem_basic_ambulatory),
                 cbind.data.frame(setting=plan_names[2],result_data_geem_GP_intensive_ambulatory),
                 cbind.data.frame(setting=plan_names[3],result_data_geem_GP_residential),
                 cbind.data.frame(setting=plan_names[4],result_data_geem_WO_intensive_ambulatory),
                 cbind.data.frame(setting=plan_names[5],result_data_geem_WO_residential)
                 ) %>% 
  dplyr::group_by(setting) %>% 
  dplyr::mutate(min_qic_setting=ifelse(QIC==min(QIC),1,0)) %>% 
  dplyr::ungroup() %>% 
  copiar_nombres2()

library(emmeans)

# Marginal plot ----------------------------------------------

df_marginal_plots<-
  cbind.data.frame(wgt= c("wgt", "iiw_nocorr_st", "iiw_nocorr_alt_st", "wgt", "iiw_after_ph_st", "iiw_after_ph_st"), 
        df= c("data_mine_miss_restr_proc2","data_mine_miss_restr_proc2","data_mine_miss_restr_proc2","data_mine_miss_restr_proc2_iiw_after_ph","data_mine_miss_restr_proc2_iiw_after_ph","data_mine_miss_restr_proc2_iiw_after_ph_alt"),
        model= c("no corr no wgt","no corr main","no corr alt", "after ph no wgt", "after ph main", "after ph alt"))
df_marginal_setting_plots<-
expand_grid(df_marginal_plots, plan_names)

# Loop over each unique plan name to fit a model
for (i in seq_along(df_marginal_setting_plots$wgt)) {
  # Subset the data for the current plan type
  current_data <- subset(get(df_marginal_setting_plots$df[i]) %>% dplyr::mutate(wgt=1), tipo_de_plan_2_mod == df_marginal_setting_plots$plan_names[i])
  model_name<- paste0("geeglm_", gsub(" ","_",df_marginal_setting_plots$model[i]),"_", gsub(" ","_",df_marginal_setting_plots$plan_names[i]))
  geeglm( tr_outcome  ~ policonsumo2  +
                        edad_al_ing_1 + 
                        ano_nac_corr + 
                        susinidum_oh +
                        susinidum_coc +
                        susinidum_pbc +
                        susinidum_mar +
                        psycom_dum_study +
                        psycom_dum_with +
                        freq_cons_dum_5day +
                        cond_oc_dum_2inact +
                        cond_oc_dum_3unemp +
                        susprindum_coc +
                        susprindum_pbc +
                        susprindum_mar,
                    id= id, 
                    data= current_data, 
                    weight= current_data %>% pull(df_marginal_setting_plots$wgt[i]), 
                    family= poisson, 
                    corstr= "independence") %>% 
  assign(model_name, ., envir = .GlobalEnv)
  print(message(paste0("Models: ",model_name)))
  emmeans_response <- emmeans(get(model_name), ~policonsumo2,  rg.limit=2e5, type = "response")%>% 
    assign(gsub("geeglm_","emmeans_resp_",model_name), ., envir = .GlobalEnv)
  print(message(paste0("Emmeans: ", gsub("geeglm_","emmeans_resp_",model_name))))
}
# 
# Results are averaged over the levels of: susinidum_oh, susinidum_coc, susinidum_pbc, susinidum_mar, psycom_dum_study, psycom_dum_with, freq_cons_dum_5day, cond_oc_dum_2inact, cond_oc_dum_3unemp, susprindum_coc, susprindum_pbc, susprindum_mar 
# Covariance estimate used: vbeta 
# Confidence level used: 0.95 
# Intervals are back-transformed from the logit scale 
# Convert to Response Scale (Probability)

# Models: geeglm_no_corr_no_wgt
# NULL
# Emmeans: emmeans_response_no_corr_no_wgt
# NULL
# Models: geeglm_no_corr_main
# NULL
# Emmeans: emmeans_response_no_corr_main
# NULL
# Models: geeglm_no_corr_alt
# NULL
# Emmeans: emmeans_response_no_corr_alt
# NULL
# Models: geeglm_after_ph_no_wgt
# NULL
# Emmeans: emmeans_response_after_ph_no_wgt
# NULL
# Models: geeglm_after_ph_main
# NULL
# Emmeans: emmeans_response_after_ph_main
# NULL
# Models: geeglm_after_ph_alt
# NULL
# Emmeans: emmeans_response_after_ph_alt



# Models: geeglm_no_corr_no_wgt_basic_ambulatory
# NULL
# Emmeans: emmeans_response_no_corr_no_wgt_basic_ambulatory
# NULL
# Models: geeglm_no_corr_no_wgt_GP_intensive_ambulatory
# NULL
# Emmeans: emmeans_response_no_corr_no_wgt_GP_intensive_ambulatory
# NULL
# Models: geeglm_no_corr_no_wgt_GP_residential
# NULL
# Emmeans: emmeans_response_no_corr_no_wgt_GP_residential
# NULL
# Models: geeglm_no_corr_no_wgt_WO_intensive_ambulatory
# NULL
# Emmeans: emmeans_response_no_corr_no_wgt_WO_intensive_ambulatory
# NULL
# Models: geeglm_no_corr_no_wgt_WO_residential
# NULL
# Emmeans: emmeans_response_no_corr_no_wgt_WO_residential
# NULL
# Models: geeglm_no_corr_main_basic_ambulatory
# NULL
# Emmeans: emmeans_response_no_corr_main_basic_ambulatory
# NULL
# Models: geeglm_no_corr_main_GP_intensive_ambulatory
# NULL
# Emmeans: emmeans_response_no_corr_main_GP_intensive_ambulatory
# NULL
# Models: geeglm_no_corr_main_GP_residential
# NULL
# Emmeans: emmeans_response_no_corr_main_GP_residential
# NULL
# Models: geeglm_no_corr_main_WO_intensive_ambulatory
# NULL
# Emmeans: emmeans_response_no_corr_main_WO_intensive_ambulatory
# NULL
# Models: geeglm_no_corr_main_WO_residential
# NULL
# Emmeans: emmeans_response_no_corr_main_WO_residential
# NULL
# Models: geeglm_no_corr_alt_basic_ambulatory
# NULL
# Emmeans: emmeans_response_no_corr_alt_basic_ambulatory
# NULL
# Models: geeglm_no_corr_alt_GP_intensive_ambulatory
# NULL
# Emmeans: emmeans_response_no_corr_alt_GP_intensive_ambulatory
# NULL
# Models: geeglm_no_corr_alt_GP_residential
# NULL
# Emmeans: emmeans_response_no_corr_alt_GP_residential
# NULL
# Models: geeglm_no_corr_alt_WO_intensive_ambulatory
# NULL
# Emmeans: emmeans_response_no_corr_alt_WO_intensive_ambulatory
# NULL
# Models: geeglm_no_corr_alt_WO_residential
# NULL
# Emmeans: emmeans_response_no_corr_alt_WO_residential
# NULL
# Models: geeglm_after_ph_no_wgt_basic_ambulatory
# NULL
# Emmeans: emmeans_response_after_ph_no_wgt_basic_ambulatory
# NULL
# Models: geeglm_after_ph_no_wgt_GP_intensive_ambulatory
# NULL
# Emmeans: emmeans_response_after_ph_no_wgt_GP_intensive_ambulatory
# NULL
# Models: geeglm_after_ph_no_wgt_GP_residential
# NULL
# Emmeans: emmeans_response_after_ph_no_wgt_GP_residential
# NULL
# Models: geeglm_after_ph_no_wgt_WO_intensive_ambulatory
# NULL
# Emmeans: emmeans_response_after_ph_no_wgt_WO_intensive_ambulatory
# NULL
# Models: geeglm_after_ph_no_wgt_WO_residential
# NULL
# Emmeans: emmeans_response_after_ph_no_wgt_WO_residential
# NULL
# Models: geeglm_after_ph_main_basic_ambulatory
# NULL
# Emmeans: emmeans_response_after_ph_main_basic_ambulatory
# NULL
# Models: geeglm_after_ph_main_GP_intensive_ambulatory
# NULL
# Emmeans: emmeans_response_after_ph_main_GP_intensive_ambulatory
# NULL
# Models: geeglm_after_ph_main_GP_residential
# NULL
# Emmeans: emmeans_response_after_ph_main_GP_residential
# NULL
# Models: geeglm_after_ph_main_WO_intensive_ambulatory
# NULL
# Emmeans: emmeans_response_after_ph_main_WO_intensive_ambulatory
# NULL
# Models: geeglm_after_ph_main_WO_residential
# NULL
# Emmeans: emmeans_response_after_ph_main_WO_residential
# NULL
# Models: geeglm_after_ph_alt_basic_ambulatory
# NULL
# Emmeans: emmeans_response_after_ph_alt_basic_ambulatory
# NULL
# Models: geeglm_after_ph_alt_GP_intensive_ambulatory
# NULL
# Emmeans: emmeans_response_after_ph_alt_GP_intensive_ambulatory
# NULL
# Models: geeglm_after_ph_alt_GP_residential
# NULL
# Emmeans: emmeans_response_after_ph_alt_GP_residential
# NULL
# Models: geeglm_after_ph_alt_WO_intensive_ambulatory
# NULL
# Emmeans: emmeans_response_after_ph_alt_WO_intensive_ambulatory
# NULL
# Models: geeglm_after_ph_alt_WO_residential
# NULL
# Emmeans: emmeans_response_after_ph_alt_WO_residential

#contrasts_wgt <- contrast(marginal_differences_wgt, by=, rg.limit=2e5, type="response"), method = "pairwise")



# Output ----------------------------------------------


folder_path <- ifelse(dir.exists("E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/_proposal_grant/2023/"),
                      "E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/_proposal_grant/2023/",
                      "C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/_proposal_grant/2023/")
save.image(paste0(folder_path,"an_grant_23_24_4m.RData"))
