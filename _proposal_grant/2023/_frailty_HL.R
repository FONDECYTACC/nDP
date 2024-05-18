
rat_cox<-frailtyHL(Surv(lag_fech_egres_num/30.1,fech_egres_num/30.1,event)~ lag_tr_outcome+ 
                     log(lag_dias_treat_imp_sin_na+0.0001)+ 
                     lag_comp_bpsc_y2_moderate+ 
                     lag_comp_bpsc_y3_severe+ 
                     lag_policonsumo2+ 
                     edad_al_ing_1+ 
                     ano_nac_corr+ 
                     esc_dum_rec_3prim+ 
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
                     (1|id),
                   data= data_mine_miss_proc2,
                   varfixed=TRUE, # Logical value: if TRUE (FALSE), the value of one or more of the variance terms  for the frailties is fixed (estimated).
                   varinit=c(0)) #Starting values for frailties, the default is 0.1
