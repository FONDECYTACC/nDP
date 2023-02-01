#https://www.lexjansen.com/wuss/2003/DataAnalysis/i-cox_time_scales.pdf

#length-biased failure times (y), the truncation variable (a), the censoring indicator (delta), and two covariates, X1 with 
#binary values (x1) and X2 with continuous values that range from 0 to 1 (x2). 
#The vector of forward recurrence times (v) is the difference between the failure times and the backward recurrence times (y-a).

invisible("length time bias")
#In a sample, the individuals with short survival die earlier and appear fewer in the sample. This natural phenomenon induces bias in the survival curve estimation. This package offers various corrections to such bias.


station.test(a = Base_fiscalia_v13_pris$edad_al_egres_imp, 
             v = Base_fiscalia_v13_pris$age_offending_imp - Base_fiscalia_v13_pris$edad_al_egres_imp, 
             delta = ifelse(!is.na(Base_fiscalia_v13_pris$dateofbirth_imp),1,0))
#se demora como 3 hrs.
# test.statistic p.value
# 67.999         <0.001 

#Addona, V. and Wolfson, D. B. (2006). A formal test for the stationarity of the incidence rate using
#data from a prevalent cohort study with follow-up. Lifetime data analysis, 12(3), 267-284.
#Chi Hyun Lee, Heng Zhou, Jing Ning, Diane D. Liu and Yu Shen , The R Journal (2020) 12:1, pages 118-130.
#https://www.jstor.org/stable/4616631
#El harazd a la base depende no sólo de la duración sino de la fecha de inicio del fenómeno de interés

fit.eel <- coxphlb(Surv(edad_al_egres_imp, age_offending_imp, delta) ~ motivodeegreso_mod_imp_rec+ 
      edad_al_ing_fmt,
        # edad_ini_cons+ dias_treat_imp_sin_na_1+ escolaridad_rec+ sus_principal_mod+ freq_cons_sus_prin+ 
        # compromiso_biopsicosocial+ origen_ingreso_mod+ numero_de_hijos_mod+ tenencia_de_la_vivienda_mod+ dg_cie_10_rec+
        # dg_trs_cons_sus_or+ macrozona+ n_prev_off+ n_off_vio+ n_off_acq+ n_off_sud+ n_off_oth
      data= dplyr::mutate(Base_fiscalia_v13_pris, delta= ifelse(!is.na(dateofbirth_imp),1,0),
                          motivodeegreso_mod_imp_rec= factor(motivodeegreso_mod_imp_rec)) %>% 
        dplyr::filter(hash_key!="38c6714556fce94b584f25af4d4bc786", !is.na(edad_al_egres_imp), !is.na(age_offending_imp))%>%as.data.frame(),
                   method = "EE")
#Error in !data[, 2] : invalid argument type
#Dejar como factores las variables que son string
#https://stackoverflow.com/questions/18025797/invalid-argument-type-error-with-all-equal-r
#there is no extend

options(error=recover)

ftest1 <- coxphlb.ftest(fit = fit.ee1, data = dat1, spec.p = 2, seed.n = 1234)

#https://ruor.uottawa.ca/bitstream/10393/35748/3/Bentoumi_Rachid_2017_th%C3%A8se.pdf

#he partial likelihood approach proposed for left-truncated data can be applied to estimate the covariate effects for length-biased data under the Cox model 

# a) The follow-up period is still relatively short. 
# Perhaps as the follow-up period increases, the timescale will be more of an issue. 
# b) Using method #2, a plot of the cumulative hazard 
# function by age looked approximately exponentially 
# distributed. According to Korn et al., this is one 
# situation where a time-on-study versus age time-scale 
# will not make a big difference to the HR estimates. 


#In this sense, length-biased sampling is a special form of left truncation under the stationarity assumption. Since the stationarity
#assumption implies that truncation times are uniform distributed, it is also referred
#to as the uniform truncation assumption.
#https://deepblue.lib.umich.edu/bitstream/handle/2027.42/136934/fannwu_1.pdf?sequence=1



errores_edad