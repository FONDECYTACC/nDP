
# Step 1: Calculate the position for labels
labels_data <- p_alluvial$data %>% 
  dplyr::group_by(x)%>%
  dplyr::mutate(Percentage = round(n / sum(n) * 100,1)) %>% 
  dplyr::mutate(print=glue::glue("{format(n, big.mark=',')}; {Percentage}%")) %>% 
  dplyr::ungroup() %>%
  group_by(alluvial_id, x) %>%
  summarise(y_position = sum(n) / 2, .groups = 'drop') %>%
  ungroup()

# Merge back to get label text if it's not directly available in labels_data
labels_data <- merge(labels_data, p_alluvial$data %>% 
                       dplyr::group_by(x)%>%
                       dplyr::mutate(Percentage = round(n / sum(n) * 100,1)) %>% 
                       dplyr::mutate(print=glue::glue("{format(n, big.mark=',')}; {Percentage}%")) %>% 
                       dplyr::ungroup(), by = c("alluvial_id", "x"), all.x = TRUE)

# Assuming you want to label with 'value' or a custom text, ensure it's in labels_data
labels_data <- labels_data %>%
  distinct(alluvial_id, x, y_position, .keep_all = TRUE) %>%
  mutate(label_text = paste(alluvial_id, value, sep = ": "))  # Customize this as needed





i <- iiw.weights(Surv(lag_time,time,event)~policonsumo2+ event.lag+ less_90d_tr.lag+ sex+ edad_ini_cons+ escolaridad_rec+ sus_principal_mod+ freq_cons_sus_prin_ord+ condicion_ocupacional_corr24+ num_hijos_mod_joel_bin+ tenencia_de_la_vivienda_mod+ macrozona+ dg_cie_10_rec+ dg_trs_cons_sus_or+ clas_r+ porc_pobr+ sus_ini_mod_mvv+ ano_nac_corr+ con_quien_vive_joel+ origen_ingreso_mod+ compromiso_biopsicosocial+ cluster(hash_key),
                 id="id",
                 time="time",
                 event="event",
                 data= Base_fiscalia_v15f_grant_23_24_long2_proc,
                 invariant=c("hash_key", "sex", "edad_ini_cons", "escolaridad_rec", "sus_principal_mod", "freq_cons_sus_prin_ord", "condicion_ocupacional_corr24", "num_hijos_mod_joel_bin", "tenencia_de_la_vivienda_mod",  "macrozona", "dg_cie_10_rec", "dg_trs_cons_sus_or", "clas_r", "porc_pobr", "sus_ini_mod_mvv", "ano_nac_corr", "tipo_centro_pub", "con_quien_vive_joel", "origen_ingreso_mod", "compromiso_biopsicosocial"),
                 lagvars=c("time","event","less_90d_tr"),#
                 maxfu= Base_fiscalia_v15f_grant_23_24_long2_proc$cens_time, #f individuals have different follow-up times, maxfu should have the same number of elements as there are rows of data
                 lagfirst=c(0,0,0),
                 first= T #If TRUE, the first observation for each individual is assigned an intensity of 1. This is appropriate if the first visit is a baseline visit at which recruitment to the study occurred
)
i$m
#Error in maxfu[, 2] : número incorreto de dimensiones
summary(i$iiw.weight)

invisible("La ponderación de los IIW fue igual incluyendo el tiempo máximo de seguimiento que no haciéndolo")




i0$datacox %>% 
  dplyr::group_by(id) %>% 
  dplyr::mutate(lag_time= lag(time)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(dplyr::case_when(time <=lag_time & !is.na(lag_time)~T,T~F)) %>% 
  dplyr::select(hash_key, time, lag_time, motivodeegreso_mod_imp, dias_treat_imp_sin_na, enter_time, fech_ing_num, fech_egres_num, cens_time)%>%
  {
    if (nrow(.) > 0) {
      stop("There are ties or previous events that occured later than the actual events")
    } else {
      # Behavior when the condition isn't met (e.g., return an empty data frame)
      data.frame(.) 
    }  
  }






#install.packages("pscl")
library(pscl)

invisible("Modelo original i1, con el lag time reemplazado por 0 si NA")
cox.zph(coxph(Surv(lag_time,time,event)~ lag_tr_outcome+ lag_comp_bpsc_y2_moderate+ lag_comp_bpsc_y3_severe+ lag_less_90d_tr1+ lag_policonsumo2+ cluster(hash_key)+ strata(tipo_de_plan_2), data = dplyr::mutate(data_mine_miss_proc2, lag_time=ifelse(is.na(lag_time),0,lag_time))))
#                            chisq df      p
# lag_tr_outcome              81.9  1 <2e-16
# lag_comp_bpsc_y2_moderate  799.7  1 <2e-16
# lag_comp_bpsc_y3_severe    946.9  1 <2e-16
# lag_less_90d_tr1           806.3  1 <2e-16
# lag_policonsumo2           276.8  1 <2e-16
# GLOBAL                    1503.1  5 <2e-16

extractAIC(coxph(Surv(fech_ing_num,cox.zph(final$m)time2 = ,event)~ lag_tr_outcome+ lag_comp_bpsc_y2_moderate+ lag_comp_bpsc_y3_severe+ lag_less_90d_tr1+ lag_policonsumo2+ cluster(hash_key)+ strata(tipo_de_plan_2), data = dplyr::mutate(data_mine_miss_proc2, lag_time=ifelse(is.na(lag_time),0,lag_time))))
#[1]       5 1217746


invisible("Modelo original i1, con el lag time con NA")
cox.zph(coxph(Surv(lag_time,time,event)~ lag_tr_outcome+ lag_comp_bpsc_y2_moderate+ lag_comp_bpsc_y3_severe+ lag_less_90d_tr1+ lag_policonsumo2+ cluster(hash_key)+ strata(tipo_de_plan_2), data = data_mine_miss_proc2))
#                           chisq df      p
# lag_tr_outcome            19.45  1  1e-05
# lag_comp_bpsc_y2_moderate  6.14  1 0.0132
# lag_comp_bpsc_y3_severe    9.26  1 0.0023
# lag_less_90d_tr1          75.15  1 <2e-16
# lag_policonsumo2           4.94  1 0.0262
# GLOBAL                    92.30  5 <2e-16

extractAIC(coxph(Surv(lag_time,time,event)~ lag_tr_outcome+ lag_comp_bpsc_y2_moderate+ lag_comp_bpsc_y3_severe+ lag_less_90d_tr1+ lag_policonsumo2+ cluster(hash_key)+ strata(tipo_de_plan_2), data = data_mine_miss_proc2))
#[1]      5.0 195187.3


invisible("Modelo original i1+ edad al ingreso y año de nacimiento, con el lag time reemplazado por 0 si NA")
cox.zph(coxph(Surv(lag_time,time,event)~ lag_tr_outcome+ lag_comp_bpsc_y2_moderate+ lag_comp_bpsc_y3_severe+ lag_less_90d_tr1+ lag_policonsumo2+ edad_al_ing_1+ ano_nac_corr+ cluster(hash_key)+ strata(tipo_de_plan_2), data = dplyr::mutate(data_mine_miss_proc2, lag_time=ifelse(is.na(lag_time),0,lag_time))))
#                            chisq df      p
# lag_tr_outcome              96.2  1 <2e-16
# lag_comp_bpsc_y2_moderate  848.3  1 <2e-16
# lag_comp_bpsc_y3_severe   1004.1  1 <2e-16
# lag_less_90d_tr1           851.0  1 <2e-16
# lag_policonsumo2           280.7  1 <2e-16
# edad_al_ing_1               75.1  1 <2e-16
# ano_nac_corr                47.3  1  6e-12
# GLOBAL                    1700.3  7 <2e-16

extractAIC(coxph(Surv(lag_time,time,event)~ lag_tr_outcome+ lag_comp_bpsc_y2_moderate+ lag_comp_bpsc_y3_severe+ lag_less_90d_tr1+ lag_policonsumo2+ cluster(hash_key)+ strata(tipo_de_plan_2), data = data_mine_miss_proc2))
#[1]      5.0 195187.3


invisible("Modelo original i1, con el lag time con NA")
cox.zph(coxph(Surv(lag_time,time,event)~ lag_tr_outcome+ lag_comp_bpsc_y2_moderate+ lag_comp_bpsc_y3_severe+ lag_less_90d_tr1+ lag_policonsumo2+ edad_al_ing_1+ ano_nac_corr+ cluster(hash_key)+ strata(tipo_de_plan_2), data = data_mine_miss_proc2))
#                             chisq df       p
# lag_tr_outcome             27.272  1 1.8e-07
# lag_comp_bpsc_y2_moderate  12.503  1 0.00041
# lag_comp_bpsc_y3_severe    18.720  1 1.5e-05
# lag_less_90d_tr1           75.249  1 < 2e-16
# lag_policonsumo2            9.339  1 0.00224
# edad_al_ing_1               5.396  1 0.02019
# ano_nac_corr                0.127  1 0.72164
# GLOBAL                    208.940  7 < 2e-16

extractAIC(coxph(Surv(lag_time,time,event)~ lag_tr_outcome+ lag_comp_bpsc_y2_moderate+ lag_comp_bpsc_y3_severe+ lag_less_90d_tr1+ lag_policonsumo2+ edad_al_ing_1+ ano_nac_corr+ cluster(hash_key)+ strata(tipo_de_plan_2), data = data_mine_miss_proc2))
#[1]      7.0 193116.7





extractAIC(coxph(Surv(lag_time,time,event)~ lag_event+ lag_comp_bpsc_y2_moderate+ lag_comp_bpsc_y3_severe+ lag_less_90d_tr1+ lag_policonsumo2+ edad_al_ing_1+ ano_nac_corr+ cluster(hash_key)+ strata(tipo_de_plan_2), data = 
                   dplyr::mutate(group_by(data_mine2_miss_proc, hash_key), lag_event= lag(event, default=1), lag_comp_bpsc_y2_moderate= lag(comp_bpsc_y2_moderate, default=0), lag_comp_bpsc_y3_severe= lag(comp_bpsc_y3_severe, default=1), lag_less_90d_tr1= lag(less_90d_tr1, default=1), lag_policonsumo2= lag(policonsumo2, default=1),surv_time= ifelse(surv_time<0.001, 0.0001, surv_time)) %>% dplyr::ungroup()))
#[1]      7.0 193116.7


cox.zph(coxph(Surv(lag_time,time,event)~ lag_event+ lag_comp_bpsc_y2_moderate+ lag_comp_bpsc_y3_severe+ lag_less_90d_tr1+ lag_policonsumo2+ edad_al_ing_1+ ano_nac_corr+ esc_dum_rec_3prim+ 
                esc_dum_rec_2high +
                susprindum_oh +
                susprindum_coc +
                susprindum_pbc +
                susprindum_mar+
                freq_cons_dum_5day+
                freq_cons_dum_44to6wk+
                freq_cons_dum_32to3wk+
                freq_cons_dum_21wkmore+
                cond_oc_dum_3unemp+
                cond_oc_dum_2inact+
                viv_dum_illegal+
                viv_dum_own+
                viv_dum_rent+
                viv_dum_temp+
                macro_dum_south+
                macro_dum_north+
                psycom_dum_with+
                psycom_dum_study+
                rurality_rural+
                rurality_mix+
                susinidum_oh +
                susinidum_coc +
                susinidum_pbc +
                susinidum_mar+
                cohab_dum_alone+
                cohab_dum_fam_or+
                cohab_dum_cpl_child+ 
                porc_pobr+ 
                cluster(hash_key)+ strata(tipo_de_plan_2), data = data_mine2_miss_proc2))
#                              chisq df       p
# lag_event                  25.2276  1 5.1e-07
# lag_comp_bpsc_y2_moderate  11.8219  1 0.00059
# lag_comp_bpsc_y3_severe    17.1571  1 3.4e-05
# lag_less_90d_tr1           73.9287  1 < 2e-16
# lag_policonsumo2            7.1190  1 0.00763
# edad_al_ing_1               5.3330  1 0.02093
# ano_nac_corr                0.1242  1 0.72450
# esc_dum_rec_3prim           0.2654  1 0.60646
# esc_dum_rec_2high           0.3427  1 0.55825
# susprindum_oh              13.3933  1 0.00025
# susprindum_coc              3.3122  1 0.06877
# susprindum_pbc             17.8397  1 2.4e-05
# susprindum_mar              0.2977  1 0.58532
# freq_cons_dum_5day          7.2201  1 0.00721
# freq_cons_dum_44to6wk       0.8513  1 0.35619
# freq_cons_dum_32to3wk       2.5871  1 0.10774
# freq_cons_dum_21wkmore      0.0215  1 0.88341
# cond_oc_dum_3unemp         10.8463  1 0.00099
# cond_oc_dum_2inact          0.0176  1 0.89451
# viv_dum_illegal             0.0976  1 0.75479
# viv_dum_own                 0.0596  1 0.80720
# viv_dum_rent                2.0519  1 0.15201
# viv_dum_temp                0.1792  1 0.67205
# macro_dum_south             0.0712  1 0.78965
# macro_dum_north             1.4148  1 0.23426
# psycom_dum_with             4.7984  1 0.02849
# psycom_dum_study           18.6494  1 1.6e-05
# rurality_rural              7.0660  1 0.00786
# rurality_mix                0.1629  1 0.68654
# susinidum_oh               13.2778  1 0.00027
# susinidum_coc               0.7418  1 0.38909
# susinidum_pbc               0.1599  1 0.68923
# susinidum_mar              10.7719  1 0.00103
# cohab_dum_alone             1.3335  1 0.24819
# cohab_dum_fam_or            0.3555  1 0.55099
# cohab_dum_cpl_child         5.5522  1 0.01846
# porc_pobr                   3.9785  1 0.04608
# GLOBAL                    243.7481 37 < 2e-16