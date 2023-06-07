#formula a formula with a minimal structure of Surv(time, status) ~ rand(arm, rx) where

#https://cran.r-project.org/web/packages/rpsftm/vignettes/rpsftm_vignette.html

#arm is the randomised treatment arm, and
#rx is the proportion of time spent on treatment, taking values in [0, 1].

#The rank preserving structural failure time model (RPSFTM) is a method used 
#to adjust for treatment switching in trials with survival outcomes. 
#Treatment switching occurs when patients switch from their randomised arm to the other treatment during the study.

invisible("podría servir para estimar la segunda readmisión")
invisible("en nuestro caso la primera estrategia sí podría tener rx. Lo único que no podría tener rx son los que no son readmitidos?.") 
invisible("Pero ahí cómo cuento el otro tratamiento?")
require(rpsftm)
#censor_time the time at which censoring would, or has occurred. This is provided for all 
#observations unlike standard Kaplan-Meier or Cox regression where it is only given for 
#censored observations. If no value is given then recensoring is not applied.

set.seed(2125)
Base_fiscalia_v14_sample<-
subset(Base_fiscalia_v14, select= c("hash_key", vars_cov, "fech_ing_num_1", "fech_nac_rec", "offender_d", "age_offending_imp", "edad_al_egres_imp")) %>% 
  dplyr::arrange(hash_key, fech_ing_num_1)  %>% 
  dplyr::mutate(age_at_censor_date=lubridate::time_length(lubridate::interval(fech_nac_rec, as.Date("2019-11-13")),unit="years")) %>% 
  data.table::data.table() %>% 
  #dplyr::slice(sample(1:nrow(Base_fiscalia_v14),5000)) %>% 
  #SI ESTUVO EN TRATAMIENTO MÁS DE 90 DÍAS (16395), CONTAREMOS LA PROPORCIÓN EN TRATAMIENTO ACTIVO (NO DA LO MISMO SI COMPLETÓ O NO)
  #dplyr::filter(dplyr::case_when((edad_al_egres_imp-edad_al_ing_1)<=(90/365.25) ~TRUE,T~FALSE)) %>% nrow() #8499
  dplyr::mutate(in_tr= dplyr::case_when(edad_al_egres_imp-edad_al_ing_1>=(90/365.25) & grepl("non-completion",motivodeegreso_mod_imp_rec)~1,
                                        !grepl("non-completion",motivodeegreso_mod_imp_rec)~1,
                                        T~0))%>% 
  #dplyr::mutate(no_PSU= dplyr::case_when(policonsumo==0~1,T~0)) %>% 
  #OJO QUE AQUÍ YA NO ESTÁN LOS DERIVADOS NI CON TTOS EN CURSO
  dplyr::filter(dplyr::case_when((edad_al_egres_imp-edad_al_ing_1)<=0~FALSE,T~TRUE)) %>% 
  #
  dplyr::mutate(xo= dplyr::case_when(edad_al_egres_imp-edad_al_ing_1<(90/365.25)~0,T~1))%>% 
  dplyr::mutate(xoyrs= dplyr::case_when(xo==1~(edad_al_egres_imp-edad_al_ing_1)/365.25, T~0)) %>% 
  dplyr::mutate(edad_cens= ((age_at_censor_date-edad_al_ing_1)/365.25)) %>% 
  dplyr::mutate(edad_off= ((age_offending_imp-edad_al_ing_1)/365.25)) %>% 
  #dplyr::mutate(late_comp= dplyr::case_when(!grepl("Early",motivodeegreso_mod_imp_rec)~1,T~0)) %>% 
  dplyr::mutate(event= dplyr::case_when(!is.na(offender_d)~1,T~0)) %>% 
  #You have observed events AFTER censoring
  dplyr::filter(edad_cens>=edad_off)

#the proportion of time on active treatment (arm=1 or the non-reference level of the factor)

rx <- with(Base_fiscalia_v14_sample, 1 - xoyrs/edad_off)

warning("Casosen que la probabildiad está mal asignada: ", nrow(Base_fiscalia_v14_sample[which(rx>1),]))s

rpsftm_fit_lr <- rpsftm(formula=Surv(edad_off, event) ~ rand(PSU, rx), 
                        data=Base_fiscalia_v14_sample, 
                        censor_time=edad_cens,
                        low_psi=-1, 
                        hi_psi=-0.1)
warnings()
#This suggests widening the search interval via trial and error until the values of Z(ψ) at 
#low_psi and hi_psi are of opposite sign. The second warning message occurs when uniroot , 
#the function used to search the interval (low_psi, hi_psi) for ψ^ and its 95% confidence interval, 
#fails to find any one of these. It will set the value to NA and produce the following warning message

rpsftm_fit_cph <- rpsftm(formula=Surv(edad_off, event) ~ rand(PSU, rx), 
                         data=Base_fiscalia_v14_sample, 
                         censor_time=edad_cens,
                         test=coxph)
warnings()
summary(rpsftm_fit_cph)

# id participant ID number = hash_key
# def indicator that the participant was assigned to the Deferred treatment arm == PSU
# imm indicator that the participant was assigned to the Immediate treatment arm == no_PSU
# censyrs censoring time, corresponding to the close of study minus the time of randomisation for each participant == age_offending_imp
# xo an indicator that crossover occurred == abandono
# xoyrs the time from randomisation at which crossover happened, or 0 for participants in the Immediate arm == 
# prog an indicator of disease progression (1), or censoring (0) == event
# progyrs time from randomisation of disease progression or censoring == age_offending_imp 
# entry the time from the global start of study of the participant’s entry into the study== edad_al_egres_imp

# 
# Este marco de datos (data frame) es el resultado de una técnica de análisis llamada modelo de tiempo de falla con selección aleatoria (rpsftm, por sus siglas en inglés). En este caso, se ha usado para evaluar un parámetro (ψ, representado por la columna 'psi') que está siendo optimizado en el modelo.
# 
# La columna 'Z' representa los valores de la estadística Z obtenidos al evaluar el modelo en 100 puntos diferentes a lo largo del intervalo de búsqueda para el parámetro ψ. La estadística Z es una medida de cuánto se desvía una observación del promedio, y es un componente clave en muchas pruebas estadísticas.
# 
# Las filas del marco de datos representan diferentes valores del parámetro ψ, desde -1 a 1, evaluados en 100 puntos.
# 
# Interpretar los valores específicos de la estadística Z puede ser complejo, ya que depende del contexto y de las hipótesis estadísticas que se estén probando. Sin embargo, en términos generales, un valor de Z más alto en magnitud (ya sea positivo o negativo) indica que el parámetro ψ correspondiente es menos probable bajo la hipótesis nula.
# 
# En este caso, parece que los valores de Z están alcanzando un máximo alrededor de ψ = -0.01010101 y luego disminuyendo, lo que podría sugerir que este es el valor óptimo del parámetro ψ. Sin embargo, la interpretación final dependerá del contexto específico y del tipo de análisis que estés realizando.


rm(list=ls());gc()

statadf_main          <- rio::import("mariel_feb_23.dta")
statadf_main_cc       <-  statadf_main[complete.cases(statadf_main[, c(vars,"motivodeegreso_mod_imp_rec")]),] %>% dplyr::mutate(motivodeegreso_mod_imp_rec=factor(motivodeegreso_mod_imp_rec)) #%>% dplyr::mutate(across(vars,~factor(.)))
# statadf_main_l        <- rio::import("mariel_feb_23_late.dta")
# statadf_main_e        <- rio::import("mariel_feb_23_early.dta")
# statadf_main_e_l      <- rio::import("mariel_feb_23_early_late.dta")

statadf_main_pris     <- rio::import("mariel_feb_23_2.dta")
statadf_main_pris_cc  <- statadf_main_pris[complete.cases(statadf_main_pris[, c(vars,"motivodeegreso_mod_imp_rec")]),] %>% dplyr::mutate(motivodeegreso_mod_imp_rec=factor(motivodeegreso_mod_imp_rec)) 
# statadf_main_pris_l   <- rio::import("mariel_feb_23_2_late.dta")
# statadf_main_pris_e   <- rio::import("mariel_feb_23_2_early.dta")
# statadf_main_pris_e_l <- rio::import("mariel_feb_23_2_early_late.dta")
statadf_miss          <- rio::import("mariel_feb_23_m1.dta")%>% dplyr::mutate(motivodeegreso_mod_imp_rec=factor(motivodeegreso_mod_imp_rec)) #%>%
statadf_miss_pris     <- rio::import("mariel_feb_23_2_m1.dta")%>% dplyr::mutate(motivodeegreso_mod_imp_rec=factor(motivodeegreso_mod_imp_rec)) #%>%

sub1_statadf_miss<- subset(statadf_miss,subset= motivodeegreso_mod_imp_rec!=3) |> dplyr::mutate(motivodeegreso_mod_imp_rec= ifelse(motivodeegreso_mod_imp_rec==2,1,0))|> slice(1:5e5)|>data.table::data.table()

mod_tvc <- stpm2(Surv(diff,event==1)~motivodeegreso_mod_imp_rec+ tr_mod2+ sex_dum2+ edad_ini_cons+ esc1+ 
                   esc2+ sus_prin2+ sus_prin3+ sus_prin4+ sus_prin5+ fr_cons_sus_prin2+ fr_cons_sus_prin3+ 
                   fr_cons_sus_prin4+ fr_cons_sus_prin5+ cond_ocu2+ cond_ocu3+ cond_ocu4+ cond_ocu5+ 
                   cond_ocu6+ policonsumo+ num_hij2+ tenviv1+ tenviv2+ tenviv4+ tenviv5+ mzone2+ 
                   mzone3+ n_off_vio+ n_off_acq+ n_off_sud+ n_off_oth+ psy_com2+ psy_com3+ dep2+ 
                   rural2+ rural3+ porc_pobr+ susini2+ susini3+ susini4+ susini5+ ano_nac_corr+ cohab2+ 
                   cohab3+ cohab4+ fis_com2+ fis_com3+ rc_x1+ rc_x2+ rc_x3,data=sub1_statadf_miss,df=5,
                 tvc=list(motivodeegreso_mod_imp_rec=1))
require(rstpm2)
predict(mod_tvc, newdata = data.frame(motivodeegreso_mod_imp_rec = 1, diff=3), type = "meanhr", var = "motivodeegreso_mod_imp_rec")
# Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
#   variable lengths differ (found for 'tr_mod2')
# In addition: Warning message:
#   'newdata' had 1 row but variables found have 35074 rows 