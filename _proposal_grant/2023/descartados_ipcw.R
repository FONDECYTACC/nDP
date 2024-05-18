


invisible("IPWC: Proportional hazards")
ipwc_cox_zph<-cox.zph(coxph(Surv(surv_time,event==0)~ treatment+ edad_al_ing_1+ comp_bpsc_y2_moderate+ comp_bpsc_y3_severe, data=data_mine2_miss_proc))
ipwc_cox_zph
#                        chisq df       p
# treatment             2769.8  1 < 2e-16
# edad_al_ing_1           60.3  1 8.1e-15
# comp_bpsc_y2_moderate   86.3  1 < 2e-16
# comp_bpsc_y3_severe    232.7  1 < 2e-16
# GLOBAL                2875.5  4 < 2e-16

#visual inspection warn us that in 118 we need a breakpoint
plot(ipwc_cox_zph, var=1)
abline(h = coef(coxph(Surv(surv_time,event==0)~ treatment+ edad_al_ing_1+ ano_nac_corr+ comp_bpsc_y2_moderate+ comp_bpsc_y3_severe, data=data_mine2_miss_proc))[1], col = "red", lwd = 2)

png(filename="_proposal_grant/2023/cox_zph_cens_treatment.png", units="in", width=10, height=10, res=500)
plot(ipwc_cox_zph, var=1)
abline(h = coef(coxph(Surv(surv_time,event==0)~ treatment+ edad_al_ing_1+ ano_nac_corr+ comp_bpsc_y2_moderate+ comp_bpsc_y3_severe, data=data_mine2_miss_proc))[1], col = "red", lwd = 2)
dev.off()
invisible("The interaction term with lowest zph")


cox.zph(coxph(Surv(surv_time, event == 0) ~ treatment:I((surv_time)^(-0.5)) + edad_al_ing_1 + dg_trs_cons_sus_or + tipo_de_plan_2 + origen_ingreso_mod + comp_bpsc_y2_moderate+ comp_bpsc_y3_severe + cluster(hash_key), data = data_mine2_miss_proc, robust=T))
#                                  chisq df       p
# edad_al_ing_1                   111.82  1 < 2e-16
# dg_trs_cons_sus_or               79.43  1 < 2e-16
# tipo_de_plan_2                  423.91  2 < 2e-16
# origen_ingreso_mod               51.28  4 1.9e-10
# comp_bpsc_y2_moderate           104.77  1 < 2e-16
# comp_bpsc_y3_severe             262.10  1 < 2e-16
# treatment:I((surv_time)^(-0.5))   4.31  1   0.038
# GLOBAL                          704.69 11 < 2e-16

invisible("Added plan type as a strata")

cox.zph(coxph(Surv(surv_time, event == 0) ~ treatment:I((surv_time)^(-0.41)) + edad_al_ing_1 + dg_trs_cons_sus_or + origen_ingreso_mod + comp_bpsc_y2_moderate+ comp_bpsc_y3_severe + cluster(hash_key) + strata(tipo_de_plan_2), data = data_mine2_miss_proc, robust=T))
#                                   chisq df       p
# edad_al_ing_1                     70.00  1 < 2e-16
# dg_trs_cons_sus_or                40.50  1 2.0e-10
# origen_ingreso_mod                43.41  4 8.5e-09
# comp_bpsc_y2_moderate             14.81  1 0.00012
# comp_bpsc_y3_severe               79.32  1 < 2e-16
# treatment:I((surv_time)^(-0.41))   7.11  1 0.00768
# GLOBAL                           231.65  9 < 2e-16

invisible("Check non-proportionallity in Age of admission to treatment")

png(filename="_proposal_grant/2023/cox_zph_cens2_edad_al_ing.png", units="in", width=10, height=10, res=500)
plot(cox.zph(coxph(Surv(surv_time, event == 0) ~ treatment:I((surv_time)^(-0.41)) + edad_al_ing_1 + dg_trs_cons_sus_or + origen_ingreso_mod + comp_bpsc_y2_moderate+ comp_bpsc_y3_severe + cluster(hash_key) + strata(tipo_de_plan_2), data = data_mine2_miss_proc, robust=T)),var=1)
abline(h = coef(coxph(Surv(surv_time, event == 0) ~ treatment:I((surv_time)^(-0.41)) + edad_al_ing_1 + dg_trs_cons_sus_or + origen_ingreso_mod + comp_bpsc_y2_moderate+ comp_bpsc_y3_severe + cluster(hash_key) + strata(tipo_de_plan_2), data = data_mine2_miss_proc, robust=T))[1], col = "red", lwd = 2)
dev.off()

extractAIC(coxph(Surv(surv_time, event == 0) ~ treatment:I((surv_time)^(-0.41)) + edad_al_ing_1 + dg_trs_cons_sus_or + origen_ingreso_mod + comp_bpsc_y2_moderate+ comp_bpsc_y3_severe + cluster(hash_key) + strata(tipo_de_plan_2), data = data_mine2_miss_proc, robust=T))
#[1]      9.0 399398.9

invisible("Test different transformation of edad_al_ing")
aic_values <- numeric(length = length(seq(-3, 3, 0.2)))
coxzph_values <- numeric(length = length(seq(-3, 3, 0.2)))

for (i in seq(-3, 3, 0.2)) {
  model <- coxph(Surv(surv_time, event == 0) ~ treatment:I((surv_time)^(-0.41)) + I((edad_al_ing_1)^(i)) + dg_trs_cons_sus_or + origen_ingreso_mod + comp_bpsc_y2_moderate + comp_bpsc_y3_severe + cluster(hash_key) + strata(tipo_de_plan_2), data = data_mine2_miss_proc, robust = TRUE)
  aic_values <-extractAIC(model)
  coxzph_values <-cox.zph(model)$table[nrow(cox.zph(model)$table),1]
  cat(print(i))
  print(extractAIC(model))
  print(cox.zph(model)$table[nrow(cox.zph(model)$table),1])
}
aic_values
coxzph_values

invisible("Now they are seemingly proportional, we test a manual transformation based on visual inspection")

transform_treatment_effect <- function(treatment, t, t_change1 = 119, t_change2 = 144) {
  # Initialize the transformed treatment effect to 0
  treatment_effect <- 0
  
  # Between 0 and t_change1, decrease effect with time
  treatment_effect <- ifelse(t <= t_change1, treatment * (1 - (t / t_change1)), treatment_effect)
  
  # After t_change1 until t_change2, increase effect with time
  treatment_effect <- ifelse(t > t_change1 & t <= t_change2, treatment * ((t - t_change1) / (t_change2 - t_change1)), treatment_effect)
  
  # Keep the effect constant after t_change2
  treatment_effect <- ifelse(t > t_change2, treatment, treatment_effect)
  
  return(treatment_effect)
}

#transformed the variable conditional on time
data_mine2_miss$treatment_time <- with(data_mine2_miss, transform_treatment_effect (treatment, surv_time))

cox.zph(coxph(Surv(surv_time, event == 0) ~treatment_time + edad_al_ing_1 + ano_nac_corr + comp_bpsc_y, data = data_mine2_miss, robust=T))
#                chisq df       p
# treatment_time 18.07  1 2.1e-05
# edad_al_ing_1   5.59  1   0.018
# ano_nac_corr    5.32  1   0.021
# comp_bpsc_y     5.68  2   0.059
# GLOBAL         31.08  5 9.0e-06

extractAIC(coxph(Surv(surv_time, event == 0) ~treatment_time + edad_al_ing_1 + ano_nac_corr + comp_bpsc_y, data = data_mine2_miss, robust=T))
#[1]      5.0 348931.4

invisible(" Error in cox.zph(cox_model) : function not defined for models with tt() terms")

invisible("En vista del tiempo, no me importa tanto la linealidad")

# survaic1 <- array(dim=8)
# survaic1[1] <-extractAIC(coxph(Surv(surv_time,event==0)~ treatment+ edad_al_ing_1+ ano_nac_corr+ comp_bpsc_y, data=data_mine2_miss))[2]
# survaic1[2] <-extractAIC(coxph(Surv(surv_time,event==0)~ treatment+ edad_al_ing_1+  I((ano_nac_corr)^(0.5))+ comp_bpsc_y, data=data_mine2_miss))[2]
# survaic1[3] <-extractAIC(coxph(Surv(surv_time,event==0)~ treatment+ edad_al_ing_1+ I((ano_nac_corr)^(2))+ comp_bpsc_y, data=data_mine2_miss))[2]
# survaic1[4] <-extractAIC(coxph(Surv(surv_time,event==0)~ treatment+ edad_al_ing_1+ I((ano_nac_corr)^(3))+ comp_bpsc_y, data=data_mine2_miss))[2]
# survaic1[5] <-extractAIC(coxph(Surv(surv_time,event==0)~ treatment+ edad_al_ing_1+ log(ano_nac_corr)+ comp_bpsc_y, data=data_mine2_miss))[2]
# survaic1[6] <-extractAIC(coxph(Surv(surv_time,event==0)~ treatment+ edad_al_ing_1+ I((ano_nac_corr)^(-0.5))+ comp_bpsc_y, data=data_mine2_miss))[2]
# survaic1[7] <-extractAIC(coxph(Surv(surv_time,event==0)~ treatment+ edad_al_ing_1+ I((ano_nac_corr)^(-1))+ comp_bpsc_y, data=data_mine2_miss))[2]
# survaic1[8] <-extractAIC(coxph(Surv(surv_time,event==0)~ treatment+ edad_al_ing_1+ I((ano_nac_corr)^(-2))+ comp_bpsc_y, data=data_mine2_miss))[2]
# 
# which.min(survaic1)
# survaic1[which.min(survaic1)]
# 
# survaic2 <- array(dim=8)
# survaic2[1] <-extractAIC(coxph(Surv(surv_time,event==0)~ treatment+ ano_nac_corr+ edad_al_ing_1+ comp_bpsc_y, data=data_mine2_miss))[2]
# survaic2[2] <-extractAIC(coxph(Surv(surv_time,event==0)~ treatment+ ano_nac_corr+  I((edad_al_ing_1)^(0.5))+ comp_bpsc_y, data=data_mine2_miss))[2]
# survaic2[3] <-extractAIC(coxph(Surv(surv_time,event==0)~ treatment+ ano_nac_corr+ I((edad_al_ing_1)^(2))+ comp_bpsc_y, data=data_mine2_miss))[2]
# survaic2[4] <-extractAIC(coxph(Surv(surv_time,event==0)~ treatment+ ano_nac_corr+ I((edad_al_ing_1)^(3))+ comp_bpsc_y, data=data_mine2_miss))[2]
# survaic2[5] <-extractAIC(coxph(Surv(surv_time,event==0)~ treatment+ ano_nac_corr+ log(edad_al_ing_1)+ comp_bpsc_y, data=data_mine2_miss))[2]
# survaic2[6] <-extractAIC(coxph(Surv(surv_time,event==0)~ treatment+ ano_nac_corr+ I((edad_al_ing_1)^(-0.5))+ comp_bpsc_y, data=data_mine2_miss))[2]
# survaic2[7] <-extractAIC(coxph(Surv(surv_time,event==0)~ treatment+ ano_nac_corr+ I((edad_al_ing_1)^(-1))+ comp_bpsc_y, data=data_mine2_miss))[2]
# survaic2[8] <-extractAIC(coxph(Surv(surv_time,event==0)~ treatment+ ano_nac_corr+ I((edad_al_ing_1)^(-2))+ comp_bpsc_y, data=data_mine2_miss))[2]
# 
# which.min(survaic2)
# survaic2[which.min(survaic2)]

ipwc_cox_zph2<-
  cox.zph(coxph(Surv(surv_time, event == 0) ~ treatment:I((surv_time)^(-0.41)) + edad_al_ing_1 +  origen_ingreso_mod + comp_bpsc_y+ cluster(hash_key)+ strata(tipo_de_plan_2), data = data_mine2_miss, robust=T))
ipwc_cox_zph2
#                                   chisq df       p
# edad_al_ing_1                     70.31  1 < 2e-16
# origen_ingreso_mod                42.27  4 1.5e-08
# comp_bpsc_y                      106.30  2 < 2e-16
# treatment:I((surv_time)^(-0.41))   7.36  1  0.0067
# GLOBAL                           208.97  8 < 2e-16

plot(ipwc_cox_zph2, var=3)
abline(h = coef(coxph(Surv(surv_time, event == 0) ~ treatment:I((surv_time)^(-0.41)) + edad_al_ing_1 +  origen_ingreso_mod + comp_bpsc_y+ cluster(hash_key)+ strata(tipo_de_plan_2), data = data_mine2_miss, robust=T))[6], col = "red", lwd = 2, lty=4)
abline(h = coef(coxph(Surv(surv_time, event == 0) ~ treatment:I((surv_time)^(-0.41)) + edad_al_ing_1 +  origen_ingreso_mod + comp_bpsc_y+ cluster(hash_key)+ strata(tipo_de_plan_2), data = data_mine2_miss, robust=T))[7], col = "blue", lwd = 2, lty=2)
abline(h = coef(coxph(Surv(surv_time, event == 0) ~ treatment:I((surv_time)^(-0.41)) + edad_al_ing_1 +  origen_ingreso_mod + comp_bpsc_y+ cluster(hash_key)+ strata(tipo_de_plan_2), data = data_mine2_miss, robust=T))[7], col = "green", lwd = 2, lty=3)


png(filename="_proposal_grant/2023/cox_zph_cens2_comp_biops.png", units="in", width=10, height=10, res=500)
plot(ipwc_cox_zph2, var=3)
dev.off()
invisible("The interaction term with lowest zph")


invisible("IPWC: Linearity")
data_mine2_miss$treatment_time2 <-as.numeric(data_mine2_miss$treatment*I((data_mine2_miss$surv_time)^(-0.41))) 

invisible("Manual transformation La transformación manual hace lo mismo que introducir el término en el modelo")

prtw <- ipw(Surv(surv_time,event==0)~  treatment_time2+ edad_al_ing_1 +  origen_ingreso_mod + comp_bpsc_y+ strata(tipo_de_plan_2), # sacar cluster(hash_key)+ 
            data= data_mine2_miss,
            cluster="hash_key",
            weight.name="ipwc")

summary(prtw$ipwc)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0004389 0.8132962 0.9959005 0.8513378 0.9999274 1.0000000 



```{r irrelong-3-icw, warning=FALSE, echo=T, error=T, eval=T}
invisible("We format the data to include non-events")
# data_mine2_miss <- Base_fiscalia_v15f_grant_23_24_long2_miss_proc

# policonsumo2+ edad_al_ing_1+ sex+ edad_ini_cons+ escolaridad_rec+ sus_principal_mod+ freq_cons_sus_prin_ord+ condicion_ocupacional_corr24+ num_hijos_mod_joel_bin+ tenencia_de_la_vivienda_mod+ macrozona+ dg_cie_10_rec+ dg_trs_cons_sus_or+ clas_r+ porc_pobr+ sus_ini_mod_mvv+ ano_nac_corr+ con_quien_vive_joel+ cluster(hash_key)

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

# policonsumo2~log(time)+ time+ I((time)^(-0.5))+ I((time)^3))


final_wgts <- iiw.weights(
  Surv(time.lag,time,event)~ event.lag+ comp_bpsc_y2_moderate.lag+ comp_bpsc_y3_severe.lag+ less_90d_tr1.lag+ policonsumo2.lag+ policonsumo2+
    cluster(hash_key)+ strata(tipo_de_plan_2),
  id= "id",
  time= "time",
  event= "event",
  data= data_mine2_miss_proc,
  invariant= "id",
  lagvars= c("time", "tr_outcome", "comp_bpsc_y2_moderate", "comp_bpsc_y3_severe", "less_90d_tr1","policonsumo2"),
  maxfu= 12*12,#Base_fiscalia_v15f_grant_23_24_long2$cens_time,
  lagfirst= c(1,1,0,1,1,1,1),
  formulanull= Surv(time.lag,time,event)~ 1,
  first= T  #If TRUE, the first observation for each individual is assigned an intensity of 1. This is appropriate if the first visit is a baseline visit at which recruitment to the study occurred
)

i5b$m
broom::tidy(i5b$m)

```
