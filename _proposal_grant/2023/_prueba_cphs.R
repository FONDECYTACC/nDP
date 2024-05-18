cox_iiw_proposed3<-
  cph(Surv(lag_time,time,event)~ lag_event+ log(lag_dias_treat_imp_sin_na+.0001)+ comp_bpsc_y2_moderate+ comp_bpsc_y3_severe+ edad_al_ing_1+ ano_nac_corr+ 
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
        porc_pobr+ cluster(hash_key)+ strat(tipo_de_plan_2), data = dplyr::mutate(data_mine2_miss_proc2, lag_dias_treat_imp_sin_na= ifelse(lag_dias_treat_imp_sin_na<0.001, 0.0001, lag_dias_treat_imp_sin_na)), x=TRUE, y=TRUE, surv=TRUE)

#lag_dias_treat_imp_sin_na   


#72404                           

invisible("Saco los missings")
data_mine2_miss_proc2$lag_dias_treat_imp_sin_na_nomiss<-ifelse(is.na(data_mine2_miss_proc2$lag_dias_treat_imp_sin_na),4.499811,data_mine2_miss_proc2$lag_dias_treat_imp_sin_na)


cox_iiw_proposed4<-
  cph(Surv(lag_time,time,event)~ lag_event+ lag_dias_treat_imp_sin_na_nomiss+ lag_comp_bpsc_y2_moderate+ lag_comp_bpsc_y3_severe+ lag_policonsumo2+ edad_al_ing_1+ ano_nac_corr+ 
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
        porc_pobr+ cluster(hash_key)+ strat(tipo_de_plan_2), data = dplyr::mutate(data_mine2_miss_proc2, lag_dias_treat_imp_sin_na_nomiss= ifelse(lag_dias_treat_imp_sin_na_nomiss<0.001, 0.0001, lag_dias_treat_imp_sin_na_nomiss)), x=TRUE, y=TRUE, surv=TRUE)

# chisq df       p
# lag_event                        3.20e+01  1 1.5e-08
# lag_dias_treat_imp_sin_na_nomiss 2.02e+02  1 < 2e-16
# lag_comp_bpsc_y2_moderate        1.23e+01  1 0.00045
# lag_comp_bpsc_y3_severe          1.77e+01  1 2.5e-05
# lag_policonsumo2                 7.23e+00  1 0.00715
# edad_al_ing_1                    7.13e+00  1 0.00759
# ano_nac_corr                     5.57e-01  1 0.45553
# esc_dum_rec_3prim                2.56e-01  1 0.61257
# esc_dum_rec_2high                4.49e-01  1 0.50304
# susprindum_oh                    1.53e+01  1 8.9e-05
# susprindum_coc                   3.47e+00  1 0.06245
# susprindum_pbc                   2.03e+01  1 6.6e-06
# susprindum_mar                   2.51e-01  1 0.61609
# freq_cons_dum_5day               7.50e+00  1 0.00617
# freq_cons_dum_44to6wk            8.52e-01  1 0.35593
# freq_cons_dum_32to3wk            2.63e+00  1 0.10488
# freq_cons_dum_21wkmore           2.75e-02  1 0.86822
# cond_oc_dum_3unemp               1.18e+01  1 0.00061
# cond_oc_dum_2inact               2.79e-02  1 0.86736
# viv_dum_illegal                  3.23e-02  1 0.85729
# viv_dum_own                      6.10e-04  1 0.98030
# viv_dum_rent                     1.75e+00  1 0.18549
# viv_dum_temp                     2.42e-01  1 0.62303
# macro_dum_south                  2.50e-01  1 0.61737
# macro_dum_north                  1.62e+00  1 0.20271
# psycom_dum_with                  7.61e+00  1 0.00581
# psycom_dum_study                 2.57e+01  1 4.0e-07
# rurality_rural                   8.89e+00  1 0.00287
# rurality_mix                     4.07e-01  1 0.52354
# susinidum_oh                     1.47e+01  1 0.00013
# susinidum_coc                    8.45e-01  1 0.35808
# susinidum_pbc                    2.24e-01  1 0.63610
# susinidum_mar                    1.17e+01  1 0.00063
# cohab_dum_alone                  1.50e+00  1 0.22018
# cohab_dum_fam_or                 3.29e-01  1 0.56634
# cohab_dum_cpl_child              6.07e+00  1 0.01376
# porc_pobr                        3.31e+00  1 0.06898
# GLOBAL                           3.63e+02 37 < 2e-16

cox_iiw_proposed5<-
  cph(Surv(lag_time,time,event)~ lag_event+ log(lag_dias_treat_imp_sin_na_nomiss+0.0001)+ lag_comp_bpsc_y2_moderate+ lag_comp_bpsc_y3_severe+ lag_policonsumo2+ edad_al_ing_1+ ano_nac_corr+ 
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
        porc_pobr+ cluster(hash_key)+ strat(tipo_de_plan_2), 
      data = dplyr::mutate(data_mine2_miss_proc2, 
      lag_dias_treat_imp_sin_na_nomiss= ifelse(lag_dias_treat_imp_sin_na_nomiss<0.001, 
          0.0001, lag_dias_treat_imp_sin_na_nomiss)), x=TRUE, y=TRUE, surv=TRUE)
# chisq df       p
# lag_event                                      26.4734  1 2.7e-07
# log(lag_dias_treat_imp_sin_na_nomiss + 1e-04)  87.9112  1 < 2e-16
# lag_comp_bpsc_y2_moderate                      11.6893  1 0.00063
# lag_comp_bpsc_y3_severe                        16.9619  1 3.8e-05
# lag_policonsumo2                                7.0712  1 0.00783
# edad_al_ing_1                                   5.5172  1 0.01883
# ano_nac_corr                                    0.1793  1 0.67199
# esc_dum_rec_3prim                               0.2780  1 0.59804
# esc_dum_rec_2high                               0.3794  1 0.53792
# susprindum_oh                                  13.2641  1 0.00027
# susprindum_coc                                  3.2076  1 0.07330
# susprindum_pbc                                 17.5975  1 2.7e-05
# susprindum_mar                                  0.2767  1 0.59885
# freq_cons_dum_5day                              7.4549  1 0.00633
# freq_cons_dum_44to6wk                           0.8760  1 0.34930
# freq_cons_dum_32to3wk                           2.6669  1 0.10245
# freq_cons_dum_21wkmore                          0.0214  1 0.88377
# cond_oc_dum_3unemp                             11.2207  1 0.00081
# cond_oc_dum_2inact                              0.0123  1 0.91184
# viv_dum_illegal                                 0.0170  1 0.89629
# viv_dum_own                                     0.0200  1 0.88767
# viv_dum_rent                                    1.8553  1 0.17317
# viv_dum_temp                                    0.1812  1 0.67033
# macro_dum_south                                 0.0813  1 0.77559
# macro_dum_north                                 1.3750  1 0.24095
# psycom_dum_with                                 5.6993  1 0.01697
# psycom_dum_study                               22.1544  1 2.5e-06
# rurality_rural                                  7.4752  1 0.00626
# rurality_mix                                    0.3110  1 0.57707
# susinidum_oh                                   13.1464  1 0.00029
# susinidum_coc                                   0.8868  1 0.34634
# susinidum_pbc                                   0.1243  1 0.72437
# susinidum_mar                                  10.6727  1 0.00109
# cohab_dum_alone                                 1.4333  1 0.23122
# cohab_dum_fam_or                                0.2560  1 0.61287
# cohab_dum_cpl_child                             5.6641  1 0.01732
# porc_pobr                                       3.2740  1 0.07039
# GLOBAL                                        259.6986 37 < 2e-16

AIC(cox_iiw_proposed2) #[1] 192989.3
AIC(cox_iiw_proposed3) #[1] 193009.2
AIC(cox_iiw_proposed4) #[1] 192989.3
AIC(cox_iiw_proposed5) #[1] 193015.6
BIC(cox_iiw_proposed2) #[1] 193277.2
BIC(cox_iiw_proposed3) #[1] 193297.1
BIC(cox_iiw_proposed4) #[1] 193277.2
BIC(cox_iiw_proposed5) #[1] 193303.5

# Checking residuals and proportionality
cox.zph(cox_iiw_proposed2) #GLOBAL 3.63e+02 37 < 2e-16
cox.zph(cox_iiw_proposed3) #GLOBAL 265.8114 37 < 2e-16
cox.zph(cox_iiw_proposed4) #GLOBAL 3.63e+02 37 < 2e-16
cox.zph(cox_iiw_proposed5) #GLOBAL 259.6986 37 < 2e-16

invisible("Por lo visto, sacarle los perdidos y hacer log +.0001 mejora el PH, pero no mejora el ajuste en BIC o AIC")


# Define break points for time intervals
breaks <- c(0, 10, 20, 40, 60, 80, 100, max(data_mine2_miss_proc2$time))
# Create a factor variable with time intervals
data_mine2_miss_proc2$time_interval <- cut(data_mine2_miss_proc2$time, breaks, include.lowest = TRUE)

cox_iiw_proposed6<-
  cph(Surv(lag_time,time,event)~ 
        lag_event+ 
        log(lag_dias_treat_imp_sin_na_nomiss+0.0001)+ 
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
        porc_pobr+ cluster(hash_key)+ strat(tipo_de_plan_2)+ strat(time_interval), 
      data = dplyr::mutate(data_mine2_miss_proc2, 
                           lag_dias_treat_imp_sin_na_nomiss= ifelse(lag_dias_treat_imp_sin_na_nomiss<0.001, 
                                                                    0.0001, lag_dias_treat_imp_sin_na_nomiss)), x=TRUE, y=TRUE, surv=TRUE)
cox.zph(cox_iiw_proposed6)
# chisq df      p
# lag_event                                     1.02e+01  1 0.0014
# log(lag_dias_treat_imp_sin_na_nomiss + 1e-04) 9.66e+00  1 0.0019
# lag_comp_bpsc_y2_moderate                     5.35e-01  1 0.4644
# lag_comp_bpsc_y3_severe                       4.91e-01  1 0.4836
# lag_policonsumo2                              7.22e-01  1 0.3954
# edad_al_ing_1                                 2.98e+00  1 0.0841
# ano_nac_corr                                  5.16e+00  1 0.0232
# esc_dum_rec_3prim                             2.28e+00  1 0.1314
# esc_dum_rec_2high                             1.23e+00  1 0.2665
# susprindum_oh                                 6.28e-03  1 0.9369
# susprindum_coc                                1.94e-01  1 0.6598
# susprindum_pbc                                2.41e-04  1 0.9876
# susprindum_mar                                3.69e-01  1 0.5437
# freq_cons_dum_5day                            8.61e-01  1 0.3536
# freq_cons_dum_44to6wk                         6.24e-02  1 0.8028
# freq_cons_dum_32to3wk                         5.73e-02  1 0.8108
# freq_cons_dum_21wkmore                        1.33e+00  1 0.2493
# cond_oc_dum_3unemp                            4.42e-01  1 0.5063
# cond_oc_dum_2inact                            3.01e-01  1 0.5832
# viv_dum_illegal                               1.23e-03  1 0.9720
# viv_dum_own                                   5.02e-07  1 0.9994
# viv_dum_rent                                  2.21e-02  1 0.8819
# viv_dum_temp                                  1.48e-01  1 0.7007
# macro_dum_south                               1.28e-01  1 0.7203
# macro_dum_north                               2.06e-01  1 0.6502
# psycom_dum_with                               8.76e-01  1 0.3492
# psycom_dum_study                              5.32e+00  1 0.0211
# rurality_rural                                1.72e+00  1 0.1895
# rurality_mix                                  2.04e-01  1 0.6512
# susinidum_oh                                  3.63e-01  1 0.5470
# susinidum_coc                                 2.17e+00  1 0.1408
# susinidum_pbc                                 5.67e-02  1 0.8118
# susinidum_mar                                 1.14e+00  1 0.2867
# cohab_dum_alone                               4.12e+00  1 0.0425
# cohab_dum_fam_or                              1.14e+00  1 0.2853
# cohab_dum_cpl_child                           9.78e-03  1 0.9212
# porc_pobr                                     6.14e-04  1 0.9802
# GLOBAL                                        5.04e+01 37 0.0696

AIC(cox_iiw_proposed6) #[1] 157617.2
BIC(cox_iiw_proposed6) #[1] 157905.1

# Status
# Stratum                                                       No Event Event
# tipo_de_plan_2=basic ambulatory.time_interval=[0,10]              34   242
# tipo_de_plan_2=intensive ambulatory.time_interval=[0,10]          42   530
# tipo_de_plan_2=residential.time_interval=[0,10]                   39   392
# tipo_de_plan_2=basic ambulatory.time_interval=(10,20]            166   789
# tipo_de_plan_2=intensive ambulatory.time_interval=(10,20]        259  1337
# tipo_de_plan_2=residential.time_interval=(10,20]                 279   716
# tipo_de_plan_2=basic ambulatory.time_interval=(20,40]            374  1224
# tipo_de_plan_2=intensive ambulatory.time_interval=(20,40]        700  2176
# tipo_de_plan_2=residential.time_interval=(20,40]                 637  1067
# tipo_de_plan_2=basic ambulatory.time_interval=(40,60]            247   659
# tipo_de_plan_2=intensive ambulatory.time_interval=(40,60]        460  1219
# tipo_de_plan_2=residential.time_interval=(40,60]                 359   578
# tipo_de_plan_2=basic ambulatory.time_interval=(60,80]            129   321
# tipo_de_plan_2=intensive ambulatory.time_interval=(60,80]        227   711
# tipo_de_plan_2=residential.time_interval=(60,80]                 213   304
# tipo_de_plan_2=basic ambulatory.time_interval=(80,100]            55   135
# tipo_de_plan_2=intensive ambulatory.time_interval=(80,100]       140   332
# tipo_de_plan_2=residential.time_interval=(80,100]                109   136
# tipo_de_plan_2=basic ambulatory.time_interval=(100,135]           27    54
# tipo_de_plan_2=intensive ambulatory.time_interval=(100,135]       58   112
# tipo_de_plan_2=residential.time_interval=(100,135]                47    36
# 
# Model Tests       Discrimination    
# Indexes    
# Obs      17671    LR chi2    458.54       R2       0.026    
# Events   13070    d.f.           37    R2(37,17671)0.024    
# Center 49.4507    Pr(> chi2) 0.0000    R2(37,13070)0.032    
# Score chi2 444.63       Dxy      0.201    
# Pr(> chi2) 0.0000                         
# 
# Coef    S.E.   Wald Z Pr(>|Z|)
# lag_event                         0.2453 0.0247  9.93  <0.0001 
# lag_dias_treat_imp_sin_na_nomiss -0.0461 0.0081 -5.67  <0.0001 
# lag_comp_bpsc_y2_moderate         0.0598 0.0370  1.62  0.1059  
# lag_comp_bpsc_y3_severe           0.0931 0.0391  2.38  0.0172  
# lag_policonsumo2                  0.0242 0.0246  0.98  0.3257  
# edad_al_ing_1                     0.0159 0.0050  3.16  0.0016  
# ano_nac_corr                      0.0246 0.0049  5.03  <0.0001 
# esc_dum_rec_3prim                 0.0499 0.0320  1.56  0.1190  
# esc_dum_rec_2high                 0.0404 0.0295  1.37  0.1705  
# susprindum_oh                     0.1112 0.0757  1.47  0.1419  
# susprindum_coc                    0.1044 0.0763  1.37  0.1713  
# susprindum_pbc                    0.1751 0.0745  2.35  0.0187  
# susprindum_mar                    0.1203 0.0818  1.47  0.1415  
# freq_cons_dum_5day                0.0034 0.0434  0.08  0.9376  
# freq_cons_dum_44to6wk            -0.0336 0.0464 -0.72  0.4685  
# freq_cons_dum_32to3wk            -0.0484 0.0439 -1.10  0.2704  
# freq_cons_dum_21wkmore           -0.0385 0.0549 -0.70  0.4836  
# cond_oc_dum_3unemp               -0.0031 0.0210 -0.15  0.8841  
# cond_oc_dum_2inact               -0.0395 0.0257 -1.53  0.1249  
# viv_dum_illegal                  -0.0427 0.0879 -0.49  0.6268  
# viv_dum_own                       0.0227 0.0513  0.44  0.6581  
# viv_dum_rent                     -0.0352 0.0532 -0.66  0.5086  
# viv_dum_temp                      0.0192 0.0507  0.38  0.7050  
# macro_dum_south                  -0.1165 0.0407 -2.86  0.0042  
# macro_dum_north                  -0.0623 0.0254 -2.45  0.0143  
# psycom_dum_with                  -0.0253 0.0201 -1.25  0.2098  
# psycom_dum_study                 -0.0013 0.0254 -0.05  0.9595  
# rurality_rural                   -0.0046 0.0394 -0.12  0.9074  
# rurality_mix                     -0.0131 0.0342 -0.38  0.7024  
# susinidum_oh                     -0.0032 0.0587 -0.06  0.9561  
# susinidum_coc                     0.0655 0.0701  0.93  0.3501  
# susinidum_pbc                     0.0532 0.0649  0.82  0.4131  
# susinidum_mar                     0.0433 0.0593  0.73  0.4653  
# cohab_dum_alone                  -0.0066 0.0466 -0.14  0.8866  
# cohab_dum_fam_or                 -0.0416 0.0354 -1.18  0.2396  
# cohab_dum_cpl_child              -0.0205 0.0365 -0.56  0.5747  
# porc_pobr                        -0.0533 0.1328 -0.40  0.6883  


invisible("Ahora sacamos el último número lo colapsamos de los intervalos de tiempo")

# Define break points for time intervals
breaks2 <- c(0, 10, 20, 40, 60, 80, max(data_mine2_miss_proc2$time))
# Create a factor variable with time intervals
data_mine2_miss_proc2$time_interval2 <- cut(data_mine2_miss_proc2$time, breaks2, include.lowest = TRUE)


cox_iiw_proposed7<-
  cph(Surv(lag_time,time,event)~ 
        cluster(hash_key)+ #If a frailty model is used, the cluster(id) term should appear before other covariates
        lag_event+ 
        log(lag_dias_treat_imp_sin_na_nomiss+0.0001)+ 
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
        strat(tipo_de_plan_2)+   
 #       strat(time_interval2)
      #  strat(time_int_0_5)+
        strat(time_int_0_10)+
      #  strat(time_int_5_10)+
        strat(time_int_10_20)+
        strat(time_int_20_40)+
        strat(time_int_40_60)+
        strat(time_int_60_80)+
        strat(time_int_80_135)
        , 
      data = dplyr::mutate(data_mine2_miss_proc2, dias_treat_imp_sin_na= ifelse(dias_treat_imp_sin_na<0.001, 0.0001, dias_treat_imp_sin_na), 
                           log_dias_treat_imp_sin_na= log(dias_treat_imp_sin_na+0.0001), 
                          # time_int_0_5=ifelse(as.character(time_interval2)=="[0,5]",1,0),
                           time_int_0_10=ifelse(as.character(time_interval2)=="[0,10]",1,0), 
                          # time_int_5_10=ifelse(as.character(time_interval2)=="(5,10]",1,0), 
                           time_int_10_20=ifelse(as.character(time_interval2)=="(10,20]",1,0), 
                           time_int_20_40=ifelse(as.character(time_interval2)=="(20,40]",1,0), 
                           time_int_40_60=ifelse(as.character(time_interval2)=="(40,60]",1,0),
                           time_int_60_80=ifelse(as.character(time_interval2)=="(60,80]",1,0), 
                           time_int_80_135=ifelse(as.character(time_interval2)=="(80,135]",1,0)) %>% as.data.frame(), x=TRUE, y=TRUE, surv=TRUE)
cox.zph(cox_iiw_proposed7)
# chisq df      p
# lag_event                                     8.59e+00  1 0.0034
# log(lag_dias_treat_imp_sin_na_nomiss + 1e-04) 8.91e+00  1 0.0028
# lag_comp_bpsc_y2_moderate                     7.03e-01  1 0.4016
# lag_comp_bpsc_y3_severe                       5.21e-01  1 0.4702
# lag_policonsumo2                              8.12e-02  1 0.7757
# edad_al_ing_1                                 1.89e+00  1 0.1692
# ano_nac_corr                                  4.89e+00  1 0.0271
# esc_dum_rec_3prim                             1.71e+00  1 0.1905
# esc_dum_rec_2high                             1.10e+00  1 0.2948
# susprindum_oh                                 2.44e-04  1 0.9875
# susprindum_coc                                2.29e-01  1 0.6326
# susprindum_pbc                                3.50e-03  1 0.9528
# susprindum_mar                                1.04e+00  1 0.3068
# freq_cons_dum_5day                            1.80e-01  1 0.6714
# freq_cons_dum_44to6wk                         3.34e-02  1 0.8550
# freq_cons_dum_32to3wk                         2.91e-03  1 0.9570
# freq_cons_dum_21wkmore                        7.16e-01  1 0.3975
# cond_oc_dum_3unemp                            8.19e-01  1 0.3655
# cond_oc_dum_2inact                            6.34e-01  1 0.4258
# viv_dum_illegal                               7.73e-03  1 0.9299
# viv_dum_own                                   8.41e-02  1 0.7718
# viv_dum_rent                                  3.41e-01  1 0.5595
# viv_dum_temp                                  1.18e-02  1 0.9135
# macro_dum_south                               2.53e-03  1 0.9599
# macro_dum_north                               6.61e-03  1 0.9352
# psycom_dum_with                               2.45e+00  1 0.1173
# psycom_dum_study                              4.68e+00  1 0.0304
# rurality_rural                                1.51e+00  1 0.2197
# rurality_mix                                  1.57e-02  1 0.9003
# susinidum_oh                                  7.37e-02  1 0.7861
# susinidum_coc                                 2.70e+00  1 0.1005
# susinidum_pbc                                 5.02e-05  1 0.9943
# susinidum_mar                                 8.82e-01  1 0.3477
# cohab_dum_alone                               2.38e+00  1 0.1233
# cohab_dum_fam_or                              2.54e-01  1 0.6141
# cohab_dum_cpl_child                           1.18e-01  1 0.7308
# porc_pobr                                     7.71e-01  1 0.3800
# GLOBAL                                        5.77e+01 37 0.0164

invisible("Este es el final")
#GLOBAL                                        5.72e+01 37 0.0180

invisible("If we split to 0-5 5-10 instead 0-10 in months time intervals")
#GLOBAL                                        6.26e+01 37 0.0053

# Status
# Stratum                                                                                                                                     No Event Event
# tipo_de_plan_2=basic ambulatory.time_int_0_10=1.time_int_10_20=0.time_int_20_40=0.time_int_40_60=0.time_int_60_80=0.time_int_80_135=0           34   242
# tipo_de_plan_2=intensive ambulatory.time_int_0_10=1.time_int_10_20=0.time_int_20_40=0.time_int_40_60=0.time_int_60_80=0.time_int_80_135=0       42   530
# tipo_de_plan_2=residential.time_int_0_10=1.time_int_10_20=0.time_int_20_40=0.time_int_40_60=0.time_int_60_80=0.time_int_80_135=0                39   392
# tipo_de_plan_2=basic ambulatory.time_int_0_10=0.time_int_10_20=1.time_int_20_40=0.time_int_40_60=0.time_int_60_80=0.time_int_80_135=0          166   789
# tipo_de_plan_2=intensive ambulatory.time_int_0_10=0.time_int_10_20=1.time_int_20_40=0.time_int_40_60=0.time_int_60_80=0.time_int_80_135=0      259  1337
# tipo_de_plan_2=residential.time_int_0_10=0.time_int_10_20=1.time_int_20_40=0.time_int_40_60=0.time_int_60_80=0.time_int_80_135=0               279   716
# tipo_de_plan_2=basic ambulatory.time_int_0_10=0.time_int_10_20=0.time_int_20_40=1.time_int_40_60=0.time_int_60_80=0.time_int_80_135=0          374  1224
# tipo_de_plan_2=intensive ambulatory.time_int_0_10=0.time_int_10_20=0.time_int_20_40=1.time_int_40_60=0.time_int_60_80=0.time_int_80_135=0      700  2176
# tipo_de_plan_2=residential.time_int_0_10=0.time_int_10_20=0.time_int_20_40=1.time_int_40_60=0.time_int_60_80=0.time_int_80_135=0               637  1067
# tipo_de_plan_2=basic ambulatory.time_int_0_10=0.time_int_10_20=0.time_int_20_40=0.time_int_40_60=1.time_int_60_80=0.time_int_80_135=0          247   659
# tipo_de_plan_2=intensive ambulatory.time_int_0_10=0.time_int_10_20=0.time_int_20_40=0.time_int_40_60=1.time_int_60_80=0.time_int_80_135=0      460  1219
# tipo_de_plan_2=residential.time_int_0_10=0.time_int_10_20=0.time_int_20_40=0.time_int_40_60=1.time_int_60_80=0.time_int_80_135=0               359   578
# tipo_de_plan_2=basic ambulatory.time_int_0_10=0.time_int_10_20=0.time_int_20_40=0.time_int_40_60=0.time_int_60_80=1.time_int_80_135=0          129   321
# tipo_de_plan_2=intensive ambulatory.time_int_0_10=0.time_int_10_20=0.time_int_20_40=0.time_int_40_60=0.time_int_60_80=1.time_int_80_135=0      227   711
# tipo_de_plan_2=residential.time_int_0_10=0.time_int_10_20=0.time_int_20_40=0.time_int_40_60=0.time_int_60_80=1.time_int_80_135=0               213   304
# tipo_de_plan_2=basic ambulatory.time_int_0_10=0.time_int_10_20=0.time_int_20_40=0.time_int_40_60=0.time_int_60_80=0.time_int_80_135=1           82   189
# tipo_de_plan_2=intensive ambulatory.time_int_0_10=0.time_int_10_20=0.time_int_20_40=0.time_int_40_60=0.time_int_60_80=0.time_int_80_135=1      198   444
# tipo_de_plan_2=residential.time_int_0_10=0.time_int_10_20=0.time_int_20_40=0.time_int_40_60=0.time_int_60_80=0.time_int_80_135=1               156   172

# Stratum                                                       No Event Event
# tipo_de_plan_2=basic ambulatory.time_interval2=[0,10]             34   242
# tipo_de_plan_2=intensive ambulatory.time_interval2=[0,10]         42   530
# tipo_de_plan_2=residential.time_interval2=[0,10]                  39   392
# tipo_de_plan_2=basic ambulatory.time_interval2=(10,20]           166   789
# tipo_de_plan_2=intensive ambulatory.time_interval2=(10,20]       259  1337
# tipo_de_plan_2=residential.time_interval2=(10,20]                279   716
# tipo_de_plan_2=basic ambulatory.time_interval2=(20,40]           374  1224
# tipo_de_plan_2=intensive ambulatory.time_interval2=(20,40]       700  2176
# tipo_de_plan_2=residential.time_interval2=(20,40]                637  1067
# tipo_de_plan_2=basic ambulatory.time_interval2=(40,60]           247   659
# tipo_de_plan_2=intensive ambulatory.time_interval2=(40,60]       460  1219
# tipo_de_plan_2=residential.time_interval2=(40,60]                359   578
# tipo_de_plan_2=basic ambulatory.time_interval2=(60,80]           129   321
# tipo_de_plan_2=intensive ambulatory.time_interval2=(60,80]       227   711
# tipo_de_plan_2=residential.time_interval2=(60,80]                213   304
# tipo_de_plan_2=basic ambulatory.time_interval2=(80,135]           82   189
# tipo_de_plan_2=intensive ambulatory.time_interval2=(80,135]      198   444
# tipo_de_plan_2=residential.time_interval2=(80,135]               156   172


invisible("Si hago la división de tipos por estratos discvretos con TODAS LAS CATEGORÍAS, los coeficientes y los cox.zph son iguales")







```{r irrelong-3-iiw-weights-fp, warning=FALSE, echo=T, error=T, eval=T}
invisible("Se reemplaza el lag de binario <90 >= 90 días")
cox_iiw_proposed2<-
  cph(Surv(lag_time,time,event)~ lag_event+ lag_dias_treat_imp_sin_na+ lag_comp_bpsc_y2_moderate+ lag_comp_bpsc_y3_severe+ lag_policonsumo2+ edad_al_ing_1+ ano_nac_corr+ 
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
        porc_pobr+ cluster(hash_key)+ strat(tipo_de_plan_2), data = dplyr::mutate(data_mine2_miss_proc2, lag_dias_treat_imp_sin_na= ifelse(lag_dias_treat_imp_sin_na<0.001, 0.0001, lag_dias_treat_imp_sin_na)), x=TRUE, y=TRUE, surv=TRUE)

cox_iiw_proposed2
#                                      Status
# Stratum                               No Event Event
#   tipo_de_plan_2=basic ambulatory         1032  3424
#   tipo_de_plan_2=intensive ambulatory     1886  6417
#   tipo_de_plan_2=residential              1683  3229
# 
#                           Model Tests       Discrimination    
#                                                    Indexes    
# Obs       17671    LR chi2    2936.59       R2       0.153    
# Events    13070    d.f.            37    R2(37,17671)0.151    
# Center 417.6992    Pr(> chi2)  0.0000    R2(37,13070)0.199    
#                    Score chi2 2891.00       Dxy      0.263    
#                    Pr(> chi2)  0.0000                         
# 
#                           Coef    S.E.   Wald Z Pr(>|Z|)
# lag_event                  0.3345 0.0246 13.57  <0.0001 
# lag_dias_treat_imp_sin_na -0.0005 0.0001 -7.98  <0.0001 
# lag_comp_bpsc_y2_moderate  0.0714 0.0369  1.94  0.0530  
# lag_comp_bpsc_y3_severe    0.1665 0.0393  4.24  <0.0001 
# lag_policonsumo2           0.0120 0.0235  0.51  0.6088  
# edad_al_ing_1              0.1973 0.0053 37.42  <0.0001 
# ano_nac_corr               0.2075 0.0051 40.33  <0.0001 
# esc_dum_rec_3prim          0.0849 0.0308  2.75  0.0059  
# esc_dum_rec_2high          0.0513 0.0285  1.80  0.0722  
# susprindum_oh             -0.0648 0.0881 -0.74  0.4622  
# susprindum_coc            -0.0512 0.0892 -0.57  0.5659  
# susprindum_pbc             0.0335 0.0877  0.38  0.7020  
# susprindum_mar            -0.0629 0.0943 -0.67  0.5043  
# freq_cons_dum_5day        -0.0352 0.0448 -0.78  0.4329  
# freq_cons_dum_44to6wk     -0.0803 0.0474 -1.69  0.0902  
# freq_cons_dum_32to3wk     -0.0913 0.0453 -2.02  0.0437  
# freq_cons_dum_21wkmore    -0.1289 0.0555 -2.32  0.0203  
# cond_oc_dum_3unemp         0.0746 0.0206  3.61  0.0003  
# cond_oc_dum_2inact         0.0663 0.0260  2.55  0.0109  
# viv_dum_illegal           -0.0631 0.0891 -0.71  0.4789  
# viv_dum_own               -0.0028 0.0502 -0.06  0.9554  
# viv_dum_rent              -0.0496 0.0522 -0.95  0.3421  
# viv_dum_temp              -0.0487 0.0499 -0.98  0.3291  
# macro_dum_south           -0.0693 0.0389 -1.78  0.0745  
# macro_dum_north           -0.1189 0.0254 -4.68  <0.0001 
# psycom_dum_with            0.0414 0.0202  2.05  0.0402  
# psycom_dum_study           0.0884 0.0252  3.51  0.0005  
# rurality_rural            -0.0027 0.0364 -0.08  0.9399  
# rurality_mix              -0.0301 0.0326 -0.92  0.3562  
# susinidum_oh               0.0479 0.0599  0.80  0.4239  
# susinidum_coc              0.1166 0.0721  1.62  0.1060  
# susinidum_pbc              0.1669 0.0659  2.53  0.0113  
# susinidum_mar              0.1682 0.0605  2.78  0.0054  
# cohab_dum_alone            0.0523 0.0460  1.14  0.2558  
# cohab_dum_fam_or          -0.0611 0.0351 -1.74  0.0816  
# cohab_dum_cpl_child       -0.0624 0.0361 -1.73  0.0840  
# porc_pobr                 -0.0212 0.1357 -0.16  0.8759

cox.zph(cox_iiw_proposed2, "rank")             # tests of PH
#                              chisq df       p
# lag_event                 3.34e+01  1 7.6e-09 ##
# lag_dias_treat_imp_sin_na 1.87e+02  1 < 2e-16 ##
# lag_comp_bpsc_y2_moderate 1.11e+01  1 0.00086 #
# lag_comp_bpsc_y3_severe   1.60e+01  1 6.2e-05 ##
# lag_policonsumo2          7.87e+00  1 0.00502 #
# edad_al_ing_1             7.36e+00  1 0.00667 #
# ano_nac_corr              6.18e-01  1 0.43176
# esc_dum_rec_3prim         2.48e-01  1 0.61826
# esc_dum_rec_2high         4.81e-01  1 0.48810
# susprindum_oh             1.56e+01  1 7.7e-05 ##
# susprindum_coc            3.04e+00  1 0.08110 
# susprindum_pbc            1.97e+01  1 9.2e-06 ##
# susprindum_mar            2.29e-01  1 0.63197
# freq_cons_dum_5day        7.14e+00  1 0.00755 #
# freq_cons_dum_44to6wk     7.99e-01  1 0.37146
# freq_cons_dum_32to3wk     2.59e+00  1 0.10740
# freq_cons_dum_21wkmore    1.29e-02  1 0.90958
# cond_oc_dum_3unemp        1.19e+01  1 0.00055 #
# cond_oc_dum_2inact        1.01e-01  1 0.75113
# viv_dum_illegal           1.31e-02  1 0.90892
# viv_dum_own               3.58e-04  1 0.98491
# viv_dum_rent              1.83e+00  1 0.17610
# viv_dum_temp              2.63e-01  1 0.60780
# macro_dum_south           2.73e-01  1 0.60161
# macro_dum_north           1.32e+00  1 0.25081
# psycom_dum_with           6.91e+00  1 0.00858 #
# psycom_dum_study          2.25e+01  1 2.1e-06 ##
# rurality_rural            8.76e+00  1 0.00308
# rurality_mix              4.45e-01  1 0.50481
# susinidum_oh              1.37e+01  1 0.00022 #
# susinidum_coc             8.78e-01  1 0.34870
# susinidum_pbc             1.69e-01  1 0.68069
# susinidum_mar             1.09e+01  1 0.00094 #
# cohab_dum_alone           1.30e+00  1 0.25506
# cohab_dum_fam_or          3.87e-01  1 0.53368
# cohab_dum_cpl_child       5.62e+00  1 0.01775
# porc_pobr                 3.33e+00  1 0.06790
# GLOBAL                    3.51e+02 37 < 2e-16 ##


res2 <- resid(cox_iiw_proposed2, "scaledsch")
time2 <- as.numeric(dimnames(res2)[[1]])

z21 <- loess(res2[,"lag_event"] ~ time2, span=0.50)   # residuals for sex
plot(time2, fitted(z21))
lines(supsmu(time2, res2[,"lag_event"]),lty=2)


z22 <- loess(res2[,"lag_dias_treat_imp_sin_na"] ~ time2, span=0.50)   # residuals for sex
plot(time2, fitted(z22))
lines(supsmu(time2, res2[,"lag_dias_treat_imp_sin_na"]),lty=2)


z23 <- loess(res2[,"lag_comp_bpsc_y3_severe"] ~ time2, span=0.50)   # residuals for sex
plot(time2, fitted(z23))
lines(supsmu(time2, res2[,"lag_comp_bpsc_y3_severe"]),lty=2)


z24 <- loess(res2[,"susprindum_oh"] ~ time2, span=0.50)   # residuals for sex
plot(time2, fitted(z24))
lines(supsmu(time2, res2[,"susprindum_oh"]),lty=2)


z25 <- loess(res2[,"susprindum_pbc"] ~ time2, span=0.50)   # residuals for sex
plot(time2, fitted(z25))
lines(supsmu(time2, res2[,"susprindum_pbc"]),lty=2)


z26 <- loess(res2[,"psycom_dum_study"] ~ time2, span=0.50)   # residuals for sex
plot(time2, fitted(z26))
lines(supsmu(time2, res2[,"psycom_dum_study"]),lty=2)

# lag_event  lag_dias_treat_imp_sin_na  lag_comp_bpsc_y3_severe  susprindum_oh  susprindum_pbc  psycom_dum_study
# lag_comp_bpsc_y2_moderate  lag_policonsumo2  edad_al_ing_1  freq_cons_dum_5day  cond_oc_dum_3unemp  psycom_dum_with  susinidum_oh  susinidum_mar

invisible("Test different transformation of lag_less_90d_tr1")
aic_values_ya <- numeric(length = length(seq(-3, 3, 0.2)))
coxzph_values_ya <- numeric(length = length(seq(-3, 3, 0.2)))

for (i in 1:length(seq(-2, 2, 0.2))) {
  l <- seq(-2, 2, 0.2)[[i]]
  model <- cph(Surv(lag_time,time,event)~ lag_event+ I(lag_dias_treat_imp_sin_na^(l))+ lag_comp_bpsc_y2_moderate+ lag_comp_bpsc_y3_severe+ lag_less_90d_tr1+ lag_policonsumo2+ edad_al_ing_1+ ano_nac_corr+ 
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
                 porc_pobr+ cluster(hash_key)+ strat(tipo_de_plan_2), data = dplyr::mutate(data_mine2_miss_proc2, lag_dias_treat_imp_sin_na= ifelse(lag_dias_treat_imp_sin_na<0.001, 0.0001, lag_dias_treat_imp_sin_na)), x=TRUE, y=TRUE)
  aic_values_ya[i] <-extractAIC(model)[[2]]
  coxzph_values_ya[i] <-cox.zph(model, "rank")$table[nrow(cox.zph(model, "rank")$table),1]
  cat(print(l))
  print(aic_values_ya[i])
  print(coxzph_values_ya[i])      
}

iiw.weights(Surv(time.lag,time,event)~ 
              cluster(hash_key)+ #If a frailty model is used, the cluster(id) term should appear before other covariates
              event.lag+ 
              log_dias_treat_imp_sin_na.lag+ 
              comp_bpsc_y2_moderate.lag+ 
              comp_bpsc_y3_severe.lag+ 
              policonsumo2.lag+ 
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
              strata(tipo_de_plan_2)+ 
              strata(time_int_0_10)+
              strata(time_int_10_20)+
              strata(time_int_20_40)+
              strata(time_int_40_60)+
              strata(time_int_60_80)+
              strata(time_int_80_135), 
            id= "id",
            time= "time",
            event= "event", #character string indicating which column of the data indicates whether or not a visit occurred. If every row corresponds to a visit, then this column will consist entirely of ones
            maxfu= 135,
            invariant= c("edad_al_ing_1", "ano_nac_corr", "esc_dum_rec_3prim", "esc_dum_rec_2high", "susprindum_oh", "susprindum_coc", "susprindum_pbc", "susprindum_mar", "freq_cons_dum_5day", "freq_cons_dum_44to6wk", "freq_cons_dum_32to3wk", "freq_cons_dum_21wkmore", "cond_oc_dum_3unemp", "cond_oc_dum_2inact", "viv_dum_illegal", "viv_dum_own", "viv_dum_rent", "viv_dum_temp", "macro_dum_south", "macro_dum_north", "psycom_dum_with", "psycom_dum_study", "rurality_rural", "rurality_mix", "susinidum_oh", "susinidum_coc", "susinidum_pbc", "susinidum_mar", "cohab_dum_alone", "cohab_dum_fam_or", "cohab_dum_cpl_child", "porc_pobr"),
            lagvars= c("time", "tr_outcome", "comp_bpsc_y2_moderate", "comp_bpsc_y3_severe", "log_dias_treat_imp_sin_na","policonsumo2"),
            lagfirst= c(1,1,0,1,4.499811,1), #4.499811 es 90 días
            first= T,
            data = dplyr::mutate(data_mine2_miss_proc2, dias_treat_imp_sin_na= ifelse(dias_treat_imp_sin_na<0.001, 0.0001, dias_treat_imp_sin_na), 
                                 log_dias_treat_imp_sin_na= log(dias_treat_imp_sin_na),#+0.0001), 
                                 time_int_0_10=ifelse(as.character(time_interval2)=="[0,10]",1,0), time_int_10_20=ifelse(as.character(time_interval2)=="(10,20]",1,0), time_int_20_40=ifelse(as.character(time_interval2)=="(20,40]",1,0), time_int_40_60=ifelse(as.character(time_interval2)=="(40,60]",1,0),
                                 time_int_60_80=ifelse(as.character(time_interval2)=="(60,80]",1,0),time_int_80_135=ifelse(as.character(time_interval2)=="(80,135]",1,0)) %>% as.data.frame()
)

# Error in is.data.frame(data) : objeto 'datacox' no encontrado
# Además: Warning message:
# In Surv(time.lag, time, event) : Stop time must be > start time, NA created
# Error in is.data.frame(data) : objeto 'datacox' no encontrado
# Además: Warning message:
# In Surv(time.lag, time, event) : Stop time must be > start time, NA created  

# Surv(time.lag,time,event)~ event.lag+ comp_bpsc_y2_moderate.lag+ comp_bpsc_y3_severe.lag+ less_90d_tr1.lag+ policonsumo2.lag+ policonsumo2+
#     
```




```{r irrelong-3-iiw-weights-fp, warning=FALSE, echo=T, error=T, eval=T}
cox.zph(coxph(Surv(lag_time,time,event)~ lag_event+ lag_comp_bpsc_y2_moderate+ lag_comp_bpsc_y3_severe+ lag_less_90d_tr1+ lag_policonsumo2+ edad_al_ing_1+ ano_nac_corr+ cluster(hash_key), data = 
                dplyr::mutate(group_by(data_mine2_miss_proc, hash_key), lag_event= lag(event, default=1), lag_comp_bpsc_y2_moderate= lag(comp_bpsc_y2_moderate, default=0), lag_comp_bpsc_y3_severe= lag(comp_bpsc_y3_severe, default=1), lag_less_90d_tr1= lag(less_90d_tr1, default=1), lag_policonsumo2= lag(policonsumo2, default=1),surv_time= ifelse(surv_time<0.001, 0.0001, surv_time)) %>% dplyr::ungroup()))

cox.zph(coxph(Surv(lag_time,time,event)~ lag_event+ lag_comp_bpsc_y2_moderate+ lag_comp_bpsc_y3_severe+ lag_less_90d_tr1:I((surv_time)^(-0.2))+ lag_policonsumo2+ policonsumo+ edad_al_ing_1 + ano_nac_corr + comp_bpsc_y+ cluster(hash_key), data = 
                dplyr::mutate(group_by(data_mine2_miss_proc, hash_key), lag_event= lag(event, default=1), lag_comp_bpsc_y2_moderate= lag(comp_bpsc_y2_moderate, default=0), lag_comp_bpsc_y3_severe= lag(comp_bpsc_y3_severe, default=1), lag_less_90d_tr1= lag(less_90d_tr1, default=1), lag_policonsumo2= lag(policonsumo2, default=1),surv_time= ifelse(surv_time<0.001, 0.0001, surv_time)) %>% dplyr::ungroup()))
#                             chisq df       p
# lag_event                  26.979  1 2.1e-07
# lag_comp_bpsc_y2_moderate  18.067  1 2.1e-05
# lag_comp_bpsc_y3_severe    27.031  1 2.0e-07
# lag_less_90d_tr1           80.106  1 < 2e-16
# lag_policonsumo2            8.895  1 0.00286
# policonsumo                 7.688  1 0.00556
# edad_al_ing_1               6.190  1 0.01284
# ano_nac_corr                0.223  1 0.63644
# comp_bpsc_y                14.458  2 0.00073
# GLOBAL                    229.310 10 < 2e-16


invisible("Test different transformation of lag_less_90d_tr1")
aic_values0 <- numeric(length = length(seq(-3, 3, 0.2)))
coxzph_values0 <- numeric(length = length(seq(-3, 3, 0.2)))

for (i in 1:length(seq(-3, 3, 0.2))) {
  l <- seq(-3, 3, 0.2)[[i]]
  model <- coxph(Surv(lag_time,time,event)~ lag_event+ lag_comp_bpsc_y2_moderate+ lag_comp_bpsc_y3_severe+ lag_less_90d_tr1:I((surv_time)^(l))+ lag_policonsumo2+ policonsumo+ edad_al_ing_1 + ano_nac_corr + comp_bpsc_y+ cluster(hash_key)+ strata(tipo_de_plan_2), data = 
                   dplyr::mutate(group_by(data_mine2_miss_proc, hash_key), lag_event= lag(event, default=1), lag_comp_bpsc_y2_moderate= lag(comp_bpsc_y2_moderate, default=0), lag_comp_bpsc_y3_severe= lag(comp_bpsc_y3_severe, default=1), lag_less_90d_tr1= lag(less_90d_tr1, default=1), lag_policonsumo2= lag(policonsumo2, default=1),surv_time= ifelse(surv_time<0.001, 0.0001, surv_time)) %>% dplyr::ungroup(), robust = TRUE)
  aic_values0[i] <-extractAIC(model)[[2]]
  coxzph_values0[i] <-cox.zph(model)$table[nrow(cox.zph(model)$table),1]
  cat(print(l))
  print(extractAIC(model))
  print(cox.zph(model)$table[nrow(cox.zph(model)$table),1])      
}
plot()
```


```{r irrelong-3-iiw-weights-fp2, warning=FALSE, echo=T, error=T, eval=T}
rsq0 <- array(dim=8)
rsq0[1] <- cox.zph(coxph(Surv(lag_time,time,event)~ lag_event+ lag_comp_bpsc_y2_moderate+ lag_comp_bpsc_y3_severe+ lag_less_90d_tr1:I((time)^-1)+ lag_policonsumo2+ policonsumo+ edad_al_ing_1 + ano_nac_corr + comp_bpsc_y, data = 
                           dplyr::mutate(group_by(data_mine2_miss_proc, hash_key), lag_event= lag(event, default=1), lag_comp_bpsc_y2_moderate= lag(comp_bpsc_y2_moderate, default=0), lag_comp_bpsc_y3_severe= lag(comp_bpsc_y3_severe, default=1), lag_less_90d_tr1= lag(less_90d_tr1, default=1), lag_policonsumo2= lag(policonsumo2, default=1)) %>% dplyr::ungroup()))

rsq1 <- array(dim=8)
rsq1[1] <- glm(policonsumo2~time,data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq1[2] <- glm(policonsumo2~I((time)^0.5),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq1[3] <- glm(policonsumo2~I((time)^2),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq1[4] <- glm(policonsumo2~I((time)^3),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq1[5] <- glm(policonsumo2~log(time),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq1[6] <- glm(policonsumo2~I((time)^(-0.5)),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq1[7] <- glm(policonsumo2~I((time)^(-1)),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq1[8] <- glm(policonsumo2~I((time)^(-2)),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
which.min(rsq1)
rsq1[which.min(rsq1)]
#log(time)

fit_and_pr2 <- function(type, formula) {
  data <-  dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time))
  result <- tryCatch({
    pR2_df <- as.data.frame(t(pR2(glm(as.formula(formula), data = data, family = binomial(link = "logit")))))
    cbind.data.frame(type = type, pR2_df)
  }, error = function(e) {
    # Print the error message for debugging
    message("Error in model with type '", type, "': ", e$message)
    # Return a data.frame with NAs
    data.frame(type = type, llh = NA, llhNull = NA, G2 = NA, 
               McFadden = NA, r2ML = NA, r2CU = NA)
  })
  
  return(result)
}

results_df <- do.call(rbind.data.frame, list(
  fit_and_pr2("time", policonsumo2~time),
  fit_and_pr2("I((time)^0.5)", policonsumo2~I((time)^0.5)),
  fit_and_pr2("I((time)^2)", policonsumo2~I((time)^2)),
  fit_and_pr2("I((time)^3)", policonsumo2~I((time)^3)),
  fit_and_pr2("log(time)", policonsumo2~log(time)),
  fit_and_pr2("I((time)^(-0.5))", policonsumo2~I((time)^(-0.5))),
  fit_and_pr2("I((time)^(-1))", policonsumo2~I((time)^(-1))),
  fit_and_pr2("I((time)^(-2))", policonsumo2~I((time)^(-2)))
))

results_df$type[which.max(results_df$G2)]
results_df$type[which.max(results_df$McFadden)]
results_df$type[which.max(results_df$r2ML)]
results_df$type[which.max(results_df$r2CU)]

rsq2 <- array(dim=8)
rsq2[1] <- glm(policonsumo2~log(time)+ time,data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq2[2] <- glm(policonsumo2~log(time)+ I((time)^0.5),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq2[3] <- glm(policonsumo2~log(time)+ I((time)^2),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq2[4] <- glm(policonsumo2~log(time)+ I((time)^3),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq2[5] <- glm(policonsumo2~log(time)+ time:log(time),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq2[6] <- glm(policonsumo2~log(time)+ I((time)^(-0.5)),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq2[7] <- glm(policonsumo2~log(time)+ I((time)^(-1)),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq2[8] <- glm(policonsumo2~log(time)+ I((time)^(-2)),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 

which.min(rsq2)
rsq2[which.min(rsq2)]

results_df2 <- do.call(rbind.data.frame, list(
  fit_and_pr2("time", policonsumo2~log(time)+ time),
  fit_and_pr2("I((time)^0.5)", policonsumo2~log(time)+ I((time)^0.5)),
  fit_and_pr2("I((time)^2)", policonsumo2~log(time)+ I((time)^2)),
  fit_and_pr2("I((time)^3)", policonsumo2~log(time)+ I((time)^3)),
  fit_and_pr2("log(time)", policonsumo2~log(time)+ time:log(time)),
  fit_and_pr2("I((time)^(-0.5))", policonsumo2~log(time)+ I((time)^(-0.5))),
  fit_and_pr2("I((time)^(-1))", policonsumo2~log(time)+ I((time)^(-1))),
  fit_and_pr2("I((time)^(-2))", policonsumo2~log(time)+ I((time)^(-2)))
))

results_df2$type[which.max(results_df2$G2)]
results_df2$type[which.max(results_df2$McFadden)]
results_df2$type[which.max(results_df2$r2ML)]
results_df2$type[which.max(results_df2$r2CU)]


rsq3 <- array(dim=8)
rsq3[1] <- glm(policonsumo2~log(time)+ time+ time,data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq3[2] <- glm(policonsumo2~log(time)+ time+ I((time)^0.5),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq3[3] <- glm(policonsumo2~log(time)+ time+ I((time)^2),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq3[4] <- glm(policonsumo2~log(time)+ time+ I((time)^3),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq3[5] <- glm(policonsumo2~log(time)+ time+ time:log(time),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq3[6] <- glm(policonsumo2~log(time)+ time+ I((time)^(-0.5)),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq3[7] <- glm(policonsumo2~log(time)+ time+ I((time)^(-1)),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq3[8] <- glm(policonsumo2~log(time)+ time+ I((time)^(-2)),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 

which.min(rsq3)
rsq3[which.min(rsq3)]

results_df3 <- do.call(rbind.data.frame, list(
  fit_and_pr2("time", policonsumo2~log(time)+ time+ time),
  fit_and_pr2("I((time)^0.5)", policonsumo2~log(time)+ time+ I((time)^0.5)),
  fit_and_pr2("I((time)^2)", policonsumo2~log(time)+ time+ I((time)^2)),
  fit_and_pr2("I((time)^3)", policonsumo2~log(time)+ time+ I((time)^3)),
  fit_and_pr2("log(time)", policonsumo2~log(time)+ time+ time:log(time)),
  fit_and_pr2("I((time)^(-0.5))", policonsumo2~log(time)+ time+ I((time)^(-0.5))),
  fit_and_pr2("I((time)^(-1))", policonsumo2~log(time)+ time+ I((time)^(-1))),
  fit_and_pr2("I((time)^(-2))", policonsumo2~log(time)+ time+ I((time)^(-2)))
))

results_df3$type[which.max(results_df3$G2)]
results_df3$type[which.max(results_df3$McFadden)]
results_df3$type[which.max(results_df3$r2ML)]
results_df3$type[which.max(results_df3$r2CU)]



rsq4 <- array(dim=8)
rsq4[1] <- glm(policonsumo2~log(time)+ time+ I((time)^(-0.5))+ time,data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq4[2] <- glm(policonsumo2~log(time)+ time+ I((time)^(-0.5))+ I((time)^0.5),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq4[3] <- glm(policonsumo2~log(time)+ time+ I((time)^(-0.5))+ I((time)^2),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq4[4] <- glm(policonsumo2~log(time)+ time+ I((time)^(-0.5))+ I((time)^3),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq4[5] <- glm(policonsumo2~log(time)+ time+ I((time)^(-0.5))+ time:log(time),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq4[6] <- glm(policonsumo2~log(time)+ time+ I((time)^(-0.5))+ I((time)^(-0.5)),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq4[7] <- glm(policonsumo2~log(time)+ time+ I((time)^(-0.5))+ I((time)^(-1)),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 
rsq4[8] <- glm(policonsumo2~log(time)+ time+ I((time)^(-0.5))+ I((time)^(-2)),data= dplyr::mutate(data_mine2_miss_proc, time= ifelse(time<0.001, 0.0001, time)), family = binomial(link = "logit"))$ aic 

which.min(rsq4)
rsq4[which.min(rsq4)]

results_df4 <- do.call(rbind.data.frame, list(
  fit_and_pr2("time", policonsumo2~log(time)+ time+ time+ I((time)^(-0.5))),
  fit_and_pr2("I((time)^0.5)", policonsumo2~log(time)+ time+ I((time)^(-0.5))+ I((time)^0.5)),
  fit_and_pr2("I((time)^2)", policonsumo2~log(time)+ time+ I((time)^(-0.5))+ I((time)^2)),
  fit_and_pr2("I((time)^3)", policonsumo2~log(time)+ time+ I((time)^(-0.5))+ I((time)^3)),
  fit_and_pr2("log(time)", policonsumo2~log(time)+ time+ I((time)^(-0.5))+ time:log(time)),
  fit_and_pr2("I((time)^(-0.5))", policonsumo2~log(time)+ time+ I((time)^(-0.5))+ I((time)^(-0.5))),
  fit_and_pr2("I((time)^(-1))", policonsumo2~log(time)+ time+ I((time)^(-0.5))+ I((time)^(-1))),
  fit_and_pr2("I((time)^(-2))", policonsumo2~log(time)+ time+ I((time)^(-0.5))+ I((time)^(-2)))
))

results_df4$type[which.max(results_df4$G2)]
results_df4$type[which.max(results_df4$McFadden)]
results_df4$type[which.max(results_df4$r2ML)]
results_df4$type[which.max(results_df4$r2CU)]


```

split_data <- survSplit(Surv(surv_time,event)~ lag_tr_outcome+ 
                          lag_less_90d_tr1+
                          log(lag_dias_treat_imp_sin_na+.0001)+
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
                          cluster(hash_key)+ 
                          strat(tipo_de_plan_2), 
                        data = data_mine_miss_proc2, cut = breaks2)
# Define break points for time intervals
breaks2 <- c(0, 10, 20, 40, 60, 80, max(data_mine_miss_proc2$fech_egres_num/30.1-data_mine_miss_proc2$lag_fech_egres_num/30.1, na.rm=T))
# Create a factor variable with time intervals
data_mine_miss_proc2$time_interval2 <- cut(data_mine_miss_proc2$fech_egres_num/30.1-data_mine_miss_proc2$lag_fech_egres_num/30.1, breaks2, include.lowest = TRUE)
