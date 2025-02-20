install.packages("mets"); library(mets)
#time son los meses (30.5 dias) desde el primer ingreso hasta el egreso, de existir. L particularidad de esta submuestra, es que no tiene censura (cens_time)
prtw <- mets::ipw(Surv(time2,tr_outcome==0)~  ano_nac_corr+ edad_ini_cons+ edad_al_ing_1 + comp_bpsc_y+ strata(tipo_de_plan_2), # sacar cluster(hash_key)+ 
            data= data_mine_miss_restr_proc2 %>% dplyr::mutate(time2=ifelse(tr_outcome==1, time, cens_time)),
            cluster="hash_key",
            weight.name="ipwc")

summary(prtw$ipwc)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0004389 0.8132962 0.9959005 0.8513378 0.9999274 1.0000000 




# Prepare the data

invisible("NO sirve")


prtw <- mets::ipw(Surv(time2,tr_outcome==1)~  log(edad_al_ing_1)+ edad_ini_cons + strata(tipo_de_plan_2), # sacar cluster(hash_key)+ 
                  data= data_mine_miss_restr_proc2%>% dplyr::mutate(time2=ifelse(tr_outcome==1, time, cens_time)),
                  cluster="hash_key",
                  trunc.prob=T,
                  weight.name2="trunc",
                  weight.name="ipwc")

summary(prtw$ipwc)

summary(prtw$pr)


data_mine_miss_restr_proc2$cens_event <- ifelse(data_mine_miss_restr_proc2$tr_outcome == 0, 1, 0)


library(ipw)

# Estimamos el modelo para la probabilidad de censura.
# En el numerador se usa una fórmula simple (modelo estabilizado sin covariables),
# y en el denominador se incluyen las covariables (por ejemplo, log(edad_al_ing_1), edad_ini_cons y la estratificación por tipo_de_plan_2).
ipcw_fit <- ipwpoint(exposure = cens_event,
                     family = "binomial",
                     link = "logit",
                     numerator = ~ 1,
                     denominator = ~ log(edad_al_ing_1) + edad_ini_cons + edad_al_ing_1 + comp_bpsc_y + strata(tipo_de_plan_2),
                     data = data_mine_miss_restr_proc2)

# Los pesos de censura (IPCW) se encuentran en:
data_mine_miss_restr_proc2$ipcw <- ipcw_fit$ipw.weights
summary(data_mine_miss_restr_proc2$ipcw)
 #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 # 0.3728  0.8995  0.9649  1.0011  1.0650  2.5195 

lim_inf <- quantile(data_mine_miss_restr_proc2$ipcw, 0.01)
lim_sup <- quantile(data_mine_miss_restr_proc2$ipcw, 0.99)
data_mine_miss_restr_proc2$ipcw_trunc <- pmin(pmax(data_mine_miss_restr_proc2$ipcw, lim_inf), lim_sup)

# Estimamos el modelo de supervivencia con pesos de censura.
summary(data_mine_miss_restr_proc2$ipcw_trunc)
 #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 # 0.5171  0.8995  0.9649  0.9996  1.0650  1.8287 
 # 
 

nocorr_wgt_list_ipwc <- list()

#```{r gee12, warning=T, echo=T, error=F, eval=T}
# class-output: center-table

for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_restr_proc2exp, tipo_de_plan_2_mod == plan_names[i])
  current_data_ipwc <- subset(data_mine_miss_restr_proc2, tipo_de_plan_2_mod == plan_names[i])
  # Fit the GEE model for the current subset
  model <- geese(tr_outcome ~ policonsumo2 +
                   comp_bpsc_y3_severe+
                   edad_al_ing_1 + 
                   ano_nac_corr + 
                   susinidumrec_otr +
                   susinidumrec_coc +
                   susinidumrec_pbc +
                   susinidumrec_mar +
                   psycom_dum_study +
                   psycom_dum_with +
                   freq_cons_dum_5day +
                   cond_oc_dum_2inact +
                   cond_oc_dum_3unemp +
                   susprindumrec_coc +
                   susprindumrec_pbc +
                   susprindumrec_mar +
                   susprindumrec_otr, 
                 id = id, 
                 data = current_data,
                 family = poisson(), 
                 weight = current_data$iiw_nocorr_st*current_data_ipwc$ipcw_trunc,
                 corstr = "independence", 
                 jack = T)
  
  # Assign the model to the list with a name based on the plan name
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  nocorr_wgt_list_ipwc[[paste("model", model_name, sep = "_")]] <- model
}

rbind.data.frame(
  dplyr::filter(tidy_geese_model(nocorr_nowgt_list[[1]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(nocorr_nowgt_list[[2]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(nocorr_nowgt_list[[3]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(nocorr_nowgt_list[[4]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(nocorr_nowgt_list[[5]]), Term=="policonsumo2"))
# A tibble: 5 x 7
#   Term         Estimate Std.Error z.value p.value CI.Lower CI.Upper
#   <chr>        <chr>    <chr>     <chr>   <chr>   <chr>    <chr>   
# 1 policonsumo2 1.03     0.01      1.82    0.0689  1.00     1.05    
# 2 policonsumo2 1.04     0.01      2.65    0.0082  1.01     1.07    
# 3 policonsumo2 0.97     0.03      -1.22   0.2207  0.92     1.02    
# 4 policonsumo2 0.99     0.03      -0.37   0.7150  0.93     1.05    
# 5 policonsumo2 1.14     0.04      3.43    0.0006  1.06     1.23 
rbind.data.frame(
  dplyr::filter(tidy_geese_model(nocorr_wgt_list_ipwc[[1]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(nocorr_wgt_list_ipwc[[2]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(nocorr_wgt_list_ipwc[[3]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(nocorr_wgt_list_ipwc[[4]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(nocorr_wgt_list_ipwc[[5]]), Term=="policonsumo2"))
#   Term         Estimate Std.Error z.value p.value CI.Lower CI.Upper
#   <chr>        <chr>    <chr>     <chr>   <chr>   <chr>    <chr>   
# 1 policonsumo2 1.03     0.02      1.57    0.1164  0.99     1.06    
# 2 policonsumo2 1.04     0.02      2.42    0.0157  1.01     1.08    
# 3 policonsumo2 0.98     0.02      -0.95   0.3439  0.93     1.02    
# 4 policonsumo2 0.99     0.04      -0.22   0.8252  0.92     1.07    
# 5 policonsumo2 1.15     0.04      3.44    0.0006  1.06     1.25  
# 
invisible("No me convence porque en este caso la censura es no ser observado no más, lo cual se confunde con mi evento de interés")

# Ahora con casos completos -----------------------------------------------



nonimputed_data_with_final_hashs_complete_cases<-
  subset(Base_fiscalia_v15f_grant_23_24_long2, hash_key %in% unique(Base_fiscalia_v15f_grant_23_24_long2_miss_proc_multtr$hash_key))[which(complete.cases(subset(Base_fiscalia_v15f_grant_23_24_long2, hash_key %in% unique(Base_fiscalia_v15f_grant_23_24_long2_miss_proc_multtr$hash_key), select= c(hash_key, tipo_de_plan_2, tipo_de_plan_2_mod, sex,  edad_al_ing_1,  escolaridad_rec, sus_principal_mod, freq_cons_sus_prin_ord, condicion_ocupacional_corr24, policonsumo, policonsumo2, dg_cie_10_rec, dg_trs_cons_sus_or, sus_ini_mod_mvv, ano_nac_corr,  compromiso_biopsicosocial,compromiso_biopsicosocial.y)))),] 


nonimputed_data_with_final_hashs_complete_cases_more_strict_original_missings<-
  subset(Base_fiscalia_v15f_grant_23_24_long2, hash_key %in% unique(Base_fiscalia_v15f_grant_23_24_long2_miss_proc_multtr$hash_key))[which(complete.cases(subset(Base_fiscalia_v15f_grant_23_24_long2, hash_key %in% unique(Base_fiscalia_v15f_grant_23_24_long2_miss_proc_multtr$hash_key), select= c(hash_key, tipo_de_plan_2, tipo_de_plan_2_mod, sex,  edad_al_ing_1, edad_ini_cons, escolaridad_rec, sus_principal_mod, freq_cons_sus_prin_ord, condicion_ocupacional_corr24, policonsumo, policonsumo2, num_hijos_mod_joel_bin, tenencia_de_la_vivienda_mod, macrozona, dg_cie_10_rec, dg_trs_cons_sus_or, clas_r, porc_pobr, sus_ini_mod_mvv, ano_nac_corr, con_quien_vive_joel, tipo_centro_pub, origen_ingreso_mod, compromiso_biopsicosocial,compromiso_biopsicosocial.y)))),] 

#unique(nonimputed_data_with_final_hashs_complete_cases$hash_key)


nocorr_wgt_list_cc <- list()

#```{r gee12, warning=T, echo=T, error=F, eval=T}
# class-output: center-table

for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_restr_proc2exp, tipo_de_plan_2_mod == plan_names[i] & 
                           hash_key %in% unique(nonimputed_data_with_final_hashs_complete_cases$hash_key))
  # Fit the GEE model for the current subset
  model <- geese(tr_outcome ~ policonsumo2 +
                   comp_bpsc_y3_severe+
                   edad_al_ing_1 + 
                   ano_nac_corr + 
                   susinidumrec_otr +
                   susinidumrec_coc +
                   susinidumrec_pbc +
                   susinidumrec_mar +
                   psycom_dum_study +
                   psycom_dum_with +
                   freq_cons_dum_5day +
                   cond_oc_dum_2inact +
                   cond_oc_dum_3unemp +
                   susprindumrec_coc +
                   susprindumrec_pbc +
                   susprindumrec_mar +
                   susprindumrec_otr, 
                 id = id, 
                 data = current_data,
                 family = poisson(), 
                 weight = current_data$iiw_nocorr_st,
                 corstr = "independence", 
                 jack = T)
  
  # Assign the model to the list with a name based on the plan name
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  nocorr_wgt_list_cc[[paste("model", model_name, sep = "_")]] <- model
}


rbind.data.frame(
  dplyr::filter(tidy_geese_model(nocorr_wgt_list_cc[[1]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(nocorr_wgt_list_cc[[2]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(nocorr_wgt_list_cc[[3]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(nocorr_wgt_list_cc[[4]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(nocorr_wgt_list_cc[[5]]), Term=="policonsumo2"))
# A tibble: 5 x 7
#   Term         Estimate Std.Error z.value p.value CI.Lower CI.Upper
#   <chr>        <chr>    <chr>     <chr>   <chr>   <chr>    <chr>   
# 1 policonsumo2 1.01     0.02      0.84    0.3992  0.98     1.05    
# 2 policonsumo2 1.04     0.02      2.36    0.0183  1.01     1.08    
# 3 policonsumo2 0.96     0.03      -1.32   0.1864  0.91     1.02    
# 4 policonsumo2 1.00     0.04      -0.09   0.9254  0.92     1.08    
# 5 policonsumo2 1.15     0.04      3.17    0.0015  1.05     1.25  

nocorr_wgt_list_cc2 <- list()


for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_restr_proc2exp, tipo_de_plan_2_mod == plan_names[i] & 
                           hash_key %in% unique(nonimputed_data_with_final_hashs_complete_cases_more_strict_original_missings$hash_key))
  # Fit the GEE model for the current subset
  model <- geese(tr_outcome ~ policonsumo2 +
                   comp_bpsc_y3_severe+
                   edad_al_ing_1 + 
                   ano_nac_corr + 
                   susinidumrec_otr +
                   susinidumrec_coc +
                   susinidumrec_pbc +
                   susinidumrec_mar +
                   psycom_dum_study +
                   psycom_dum_with +
                   freq_cons_dum_5day +
                   cond_oc_dum_2inact +
                   cond_oc_dum_3unemp +
                   susprindumrec_coc +
                   susprindumrec_pbc +
                   susprindumrec_mar +
                   susprindumrec_otr, 
                 id = id, 
                 data = current_data,
                 family = poisson(), 
                 weight = current_data$iiw_nocorr_st,
                 corstr = "independence", 
                 jack = T)
  
  # Assign the model to the list with a name based on the plan name
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  nocorr_wgt_list_cc2[[paste("model", model_name, sep = "_")]] <- model
}

rbind.data.frame(
  dplyr::filter(tidy_geese_model(nocorr_wgt_list_cc2[[1]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(nocorr_wgt_list_cc2[[2]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(nocorr_wgt_list_cc2[[3]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(nocorr_wgt_list_cc2[[4]]), Term=="policonsumo2"),
  dplyr::filter(tidy_geese_model(nocorr_wgt_list_cc2[[5]]), Term=="policonsumo2"))
#   Term         Estimate Std.Error z.value p.value CI.Lower CI.Upper
#   <chr>        <chr>    <chr>     <chr>   <chr>   <chr>    <chr>   
# 1 policonsumo2 1.02     0.02      0.97    0.3298  0.98     1.05    
# 2 policonsumo2 1.04     0.02      2.42    0.0157  1.01     1.08    
# 3 policonsumo2 0.97     0.03      -1.16   0.2452  0.91     1.02    
# 4 policonsumo2 1.00     0.04      0.03    0.9748  0.92     1.08    
# 5 policonsumo2 1.14     0.05      2.80    0.0051  1.04     1.24    