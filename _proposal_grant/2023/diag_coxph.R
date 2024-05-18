load("E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/an_grant_23_24_3.RData")

install.packages("E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/_proposal_grant/2023/_lit_rev/goftte_1.0.5.tar.gz", repos = NULL, type = "source")

library(goftte)

require(survival)

#Diagnostics for proportional hazards of specific covariate(s) of the Cox model. The limiting null distribution of the score process 
#is approximated using either Lin's method (1993) or Liu's (2008). P-values are derived for KS, CvM and AD statistics.

#Fitting Cox's model
fit.coxph <- coxph(Surv(lag_time,time,event==1)~ 
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
                     strata(tipo_de_plan_2_mod), 
                   data=data_mine_miss_restr_proc2 %>% 
                     data.table::as.data.table() %>% data.frame())


#Checking the proportional hazards assumption
prop(fit.coxph) 
#Error in prop.coxph(fit.coxph) : Expected right-censored data.