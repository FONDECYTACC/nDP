rm(list=ls());gc()
if(!grepl("4.1.2",R.version.string)){stop("Different version (must be 4.1.2)")}
path<-getwd()#we define it again later in setup chunk
if (grepl("CISS Fondecyt",path)==T){
  try(setwd("C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)"));load("C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/14.Rdata")
} else if (grepl("andre",path)==T){
  try(setwd('C:/Users/andre/Desktop/SUD_CL/'));load("E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/14.Rdata")
} else if (grepl("E:",path)==T){
  try(setwd("E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/SUD_CL/"));load("E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/14.Rdata")
} else {
  try(setwd(paste0(path)));load(paste0(gsub("SUD_CL","",gsub("2022","2019",path)),"/14.Rdata"))
}
library(tidyverse)

#2022-11-01, added the age at discharge.
CONS_C1_df_dup_SEP_2020$edad_al_egres <-
  #difftime(lubridate::ymd(CONS_C1_df_dup_SEP_2020$fech_egres_imp), lubridate::ymd(CONS_C1_df_dup_SEP_2020 $fech_nac))/365.25
  lubridate::time_length(lubridate::interval(lubridate::ymd(CONS_C1_df_dup_SEP_2020 $fech_nac),lubridate::ymd(CONS_C1_df_dup_SEP_2020$fech_egres_imp)),unit="years")

CONS_C1_df_dup_SEP_2020$edad_al_ing_fmt <-
  #lubridate::time_length(lubridate::ymd(CONS_C1_df_dup_SEP_2020$fech_ing), lubridate::ymd(CONS_C1_df_dup_SEP_2020 $fech_nac),"years")
  lubridate::time_length(lubridate::interval(lubridate::ymd(CONS_C1_df_dup_SEP_2020 $fech_nac),lubridate::ymd(CONS_C1_df_dup_SEP_2020$fech_ing)),unit="years")

#2022-11-29, added nombre_centro y tipo_centro_pub
CONS_C1_df_dup_SEP_2020_22<-
  CONS_C1_df_dup_SEP_2020 %>% 
  subset(select= c("hash_key", "fech_nac", "fech_ing", "fech_egres_imp", "dup", "ano_bd_first", "duplicates_filtered", "id_centro", "tipo_centro", "tipo_de_programa_2", "tipo_de_plan_2", "senda", "macrozona", "nombre_region", "comuna_residencia_cod", "escolaridad_rec", "estado_conyugal_2", "compromiso_biopsicosocial", "sexo_2", "edad_al_ing", "edad_al_ing_fmt", "edad_al_egres", "edad_ini_cons", "edad_ini_sus_prin" ,"edad_ini_sus_prin_grupos", "freq_cons_sus_prin", "via_adm_sus_prin_act", 
                   "sus_ini_2_mod", "sus_ini_3_mod", "sus_ini_mod", "con_quien_vive", "sus_principal_mod", "nombre_centro",
                   "origen_ingreso_mod", "numero_de_hijos_mod", "tipo_de_vivienda_mod", "tenencia_de_la_vivienda_mod", "rubro_trabaja_mod", "cat_ocupacional", "estatus_ocupacional", "sus_ini_mod_mvv","cat_ocupacional_corr", "condicion_ocupacional_corr", "otras_sus1_mod", "otras_sus2_mod",  "otras_sus3_mod", "fech_ing_num", "fech_egres_num", "motivodeegreso_mod_imp","motivoegreso_derivacion", "evaluacindelprocesoteraputico",
                   paste0("tipo_de_plan_2_",1:10),paste0("motivodeegreso_mod_imp_",1:10),
                   paste0("dias_treat_imp_sin_na_",1:10), "dg_trs_cons_sus_or", "dg_total_cie_10", "dg_cie_10_rec", "dg_total_dsm_iv", "dg_dsm_iv_rec", "cnt_diagnostico_trs_fisico", "diagnostico_trs_fisico", "dg_fis_anemia", "dg_fis_card", "dg_fis_in_study", "dg_fis_enf_som", "dg_fis_ets", "dg_fis_hep_alc", "dg_fis_hep_b", "dg_fis_hep_cro", "dg_fis_inf", "dg_fis_otr_cond_fis_ries_vit", "dg_fis_otr_cond_fis", "dg_fis_pat_buc", "dg_fis_pat_ges_intrau", "dg_fis_trau_sec", "otros_pr_sm_abu_sex", "otros_pr_sm_exp_com_sex", "otros_pr_sm_otros", "otros_pr_sm_vif")) %>% 
  purrr::when(dplyr::filter(.,abs(edad_al_ing_fmt-edad_al_ing)>0.02) %>% nrow()>0 ~ stop("Age at admission was calculated differently"), ~.) %>%
  dplyr::mutate(edad_al_ing=edad_al_ing_fmt) %>% 
  dplyr::mutate(comorbidity_icd_10=dplyr::case_when(dg_total_cie_10>=2~ "Two or more", dg_total_cie_10==1~ "One", as.character(dg_cie_10_rec)=="Diagnosis unknown (under study)"~"Diagnosis unknown (under study)", as.character(dg_cie_10_rec)=="Without psychiatric comorbidity"~"Without psychiatric comorbidity")) %>%
  dplyr::mutate(comorbidity_icd_10=as.factor(comorbidity_icd_10)) %>% 
  dplyr::mutate(estatus_ocupacional=  dplyr::case_when(!is.na(cat_ocupacional)&!is.na(estatus_ocupacional)~"Empleado", TRUE~as.character(estatus_ocupacional)))%>% 
  dplyr::mutate(estatus_ocupacional= as.factor(estatus_ocupacional))%>% 
  dplyr::mutate(cnt_mod_cie_10_dg_cons_sus_or= dplyr::case_when(as.character(dg_trs_cons_sus_or)== "Drug dependence"~ dg_total_cie_10+1, TRUE~dg_total_cie_10))%>% 
  dplyr::mutate(freq_cons_sus_prin=  dplyr::case_when(as.character(freq_cons_sus_prin)=="Did not use"~ "Less than 1 day a week", TRUE~as.character(freq_cons_sus_prin)))%>% 
  dplyr::mutate(freq_cons_sus_prin= as.factor(freq_cons_sus_prin)) %>% 
  dplyr::mutate(tipo_centro_pub=  factor(dplyr::if_else(as.character(tipo_centro)=="Public",TRUE,FALSE,NA))) %>%  dplyr::mutate(dg_trs_fis_rec=  factor(dplyr::case_when(as.character(diagnostico_trs_fisico)=="En estudio"~"Diagnosis unknown (under study)",as.character(diagnostico_trs_fisico)=="Sin trastorno"~'Without physical comorbidity',cnt_diagnostico_trs_fisico>0 ~'With physical comorbidity',
                                                                                                                                                                         TRUE~NA_character_)))%>%
  dplyr::mutate(escolaridad_rec= readr::parse_factor(as.character(escolaridad_rec), levels=c('3-Completed primary school or less', '2-Completed high school or less', '1-More than high school'), ordered=T,trim_ws=T,include_na =F, locale=readr::locale(encoding = "Latin1"))) %>%   
  dplyr::mutate(freq_cons_sus_prin= readr::parse_factor(as.character(freq_cons_sus_prin), levels=c('Did not use', 'Less than 1 day a week','2 to 3 days a week','4 to 6 days a week','1 day a week or more','Daily'), ordered =T,trim_ws=T,include_na =F, locale=readr::locale(encoding = "UTF-8"))) %>% 
  dplyr::mutate(evaluacindelprocesoteraputico= dplyr::case_when(grepl("1",as.character(evaluacindelprocesoteraputico))~ '1-High Achievement',grepl("2",as.character(evaluacindelprocesoteraputico))~ '2-Medium Achievement',grepl("3",as.character(evaluacindelprocesoteraputico))~ '3-Minimum Achievement', TRUE~as.character(evaluacindelprocesoteraputico))) %>% 
  dplyr::mutate(evaluacindelprocesoteraputico= readr::parse_factor(as.character(evaluacindelprocesoteraputico),levels=c('1-High Achievement', '2-Medium Achievement','3-Minimum Achievement'), ordered =T,trim_ws=T,include_na =F, locale=readr::locale(encoding = "UTF-8"))) %>% 
  dplyr::mutate(tenencia_de_la_vivienda_mod= factor(dplyr::case_when(tenencia_de_la_vivienda_mod=="Allegado"~"Stays temporarily with a relative", tenencia_de_la_vivienda_mod=="Arrienda"~"Renting", tenencia_de_la_vivienda_mod=="Cedida"~"Owner/Transferred dwellings/Pays Dividends", tenencia_de_la_vivienda_mod=="Ocupación Irregular"~"Illegal Settlement", tenencia_de_la_vivienda_mod=="Otros"~"Others", tenencia_de_la_vivienda_mod=="Paga dividendo"~"Owner/Transferred dwellings/Pays Dividends", tenencia_de_la_vivienda_mod=="Propia"~"Owner/Transferred dwellings/Pays Dividends", T~NA_character_))) %>% 
  dplyr::mutate(freq_cons_sus_prin=dplyr::case_when(freq_cons_sus_prin=="1 day a week or less"~"1 day a week or more",T~as.character(freq_cons_sus_prin))) %>% 
  dplyr::mutate(freq_cons_sus_prin=ordered(freq_cons_sus_prin,levels=c("Did not use", "Less than 1 day a week", "1 day a week or more", "2 to 3 days a week","4 to 6 days a week", "Daily"))) %>%   
  data.table::data.table() %>% 
  dplyr::group_by(hash_key) %>% 
  dplyr::mutate(rn_hash_discard=row_number())%>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(tipo_centro_pub_discard=tipo_centro_pub, nombre_centro_discard= nombre_centro, fech_ing_num_discard=fech_ing_num, fech_egres_num_discard= fech_egres_num, fech_ing_discard= fech_ing, fech_egres_imp_discard=fech_egres_imp)%>% 
  #WIDE
  tidyr::pivot_wider(
    names_from =  rn_hash_discard, 
    names_sep="_",
    values_from = c(fech_ing_num_discard, fech_egres_num_discard, edad_al_ing, edad_al_egres, fech_ing_discard, fech_egres_imp_discard, tipo_centro_pub_discard, nombre_centro_discard))%>%
  #FILL COLUMNS BY PATIENT
  dplyr::group_by(hash_key)%>%
  dplyr::mutate_at(vars(fech_ing_num_discard_1:fech_egres_num_discard_10),~suppressWarnings(max(as.character(.),na.rm=T)))%>%
  dplyr::mutate_at(vars(edad_al_ing_1:edad_al_ing_10),~suppressWarnings(max(as.character(.),na.rm=T)))%>%
  #2022-11-01, added the age at discharge and the dates
  dplyr::mutate_at(vars(edad_al_egres_1:edad_al_egres_10),~suppressWarnings(max(as.character(.),na.rm=T)))%>%
  dplyr::mutate_at(vars(fech_ing_discard_1:fech_ing_discard_10),~suppressWarnings(max(as.character(.),na.rm=T)))%>%
  dplyr::mutate_at(vars(fech_egres_imp_discard_1:fech_egres_imp_discard_10),~suppressWarnings(max(as.character(.),na.rm=T)))%>%
  dplyr::ungroup() %>% 
  purrr::when(nrow(.)>nrow(CONS_C1_df_dup_SEP_2020) ~ stop("More cases in the new database"), ~.)


name_vec <- setNames(c(paste0("fech_ing_num_discard_",1:10), paste0("fech_egres_num_discard_",1:10), 
                       paste0("fech_ing_discard_",1:10), paste0("fech_egres_imp_discard_",1:10), 
                       paste0("tipo_centro_pub_discard_",1:10), paste0("nombre_centro_discard_",1:10)),
                     #names:                     
                     c(paste0("fech_ing_num_",1:10), paste0("fech_egres_num_",1:10), paste0("fech_ing_",1:10), 
                       paste0("fech_egres_imp_",1:10),paste0("tipo_centro_pub_",1:10), paste0("nombre_centro_",1:10)))
# #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_# #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

# #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_# #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
# #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
# Transform into numeric wide variables
library(data.table)
CONS_C1_df_dup_SEP_2020_22_b<-
  CONS_C1_df_dup_SEP_2020_22 %>% 
  rename(!!!name_vec) %>% 
  dplyr::mutate_at(vars(fech_ing_num_1:fech_ing_num_10),~suppressWarnings(as.numeric(.)))%>%
  dplyr::mutate_at(vars(fech_egres_num_1:fech_egres_num_10),~suppressWarnings(as.numeric(.)))%>% 
  dplyr::mutate_at(vars(edad_al_ing_1:edad_al_ing_10),~suppressWarnings(as.numeric(.)))%>%
  dplyr::mutate_at(vars(edad_al_egres_1:edad_al_egres_10),~suppressWarnings(as.numeric(.)))%>%
  dplyr::group_by(hash_key)%>%
  dplyr::mutate_at(vars(nombre_centro_1:nombre_centro_10),~suppressWarnings(max(as.character(.),na.rm=T)))%>%
  dplyr::mutate_at(vars(tipo_centro_pub_1:tipo_centro_pub_10),~suppressWarnings(max(as.character(.),na.rm=T)))%>%
  dplyr::ungroup()%>%
  as.data.table()%>% 
  purrr::when(nrow(.)>nrow(CONS_C1_df_dup_SEP_2020_22) ~ stop("More cases in the new database"), ~.)


# #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#
# Previous join and resolution of inconsistencies
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
# #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
CONS_C1_df_dup_SEP_2020_22_c<-
  CONS_C1_df_dup_SEP_2020_22_b %>% 
  dplyr::left_join(subset(Base_fiscalia_v9, rn_id==1,c("id","sex_imp","dateofbirth_imp")), by=c("hash_key"="id")) %>% 
  #
  # #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
  # If there are inconsistencies in sex
  dplyr::mutate(sex= dplyr::case_when(!is.na(sex_imp) & sex_imp!=as.character(sexo_2)~ sex_imp, !is.na(sex_imp) & is.na(sexo_2)~ sex_imp, T~as.character(sexo_2))) %>% 
  dplyr::mutate(tipo_de_plan_2= dplyr::case_when(sex=="Men" & tipo_de_plan_2=="M-PAB"~ "PG-PAB", sex=="Men" & tipo_de_plan_2=="M-PAI"~ "PG-PAI", sex=="Men" & tipo_de_plan_2=="M-PR"~ "PG-PR", T~ as.character(tipo_de_plan_2))) %>%
  dplyr::mutate(tipo_de_programa_2= dplyr::case_when(sex=="Men" & tipo_de_programa_2=="Women specific"~ "General population", T~ as.character(tipo_de_programa_2))) %>% 
  dplyr::mutate_at(vars(tipo_de_plan_2_1:tipo_de_plan_2_10),
                   ~suppressWarnings(dplyr::case_when(sex=="Men"& as.character(.)=="M-PAB"~ "PG-PAB", sex=="Men"& as.character(.)=="M-PAI"~ "PG-PAI", sex=="Men"& as.character(.)=="M-PR"~ "PG-PR", T~as.character(.))))%>%
  dplyr::select(hash_key, sex, everything()) %>% 
  #  
  # #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
  # If there are inconsistencies in the date of birth
  dplyr::mutate(fech_nac= lubridate::ymd(fech_nac)) %>% 
  dplyr::mutate(dob_imp= lubridate::ymd(dateofbirth_imp)) %>%
  #to test if there are differences in the actual and past calculation of age at admission of SENDAs treatments only, greater than 0.002
  dplyr::mutate(edad_al_ing_1_b= (fech_ing_num_1-  as.numeric(lubridate::ymd(fech_nac)))/365.25) %>% 
  purrr::when(dplyr::filter(.,abs(edad_al_ing_1-edad_al_ing_1_b)>0.02) %>% nrow()>0 ~ stop("Age at admission was calculated differently"), ~.) %>%
  # #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
  # replace year at admission
  dplyr::mutate(edad_al_ing_imp= dplyr::case_when(!is.na(dob_imp) & fech_nac!= dob_imp~ difftime(fech_ing, dob_imp)/365.25, !is.na(dob_imp) & is.na(fech_nac)~ (fech_ing- dob_imp)/365.25, T~ as.numeric(edad_al_ing_1)))%>% 
  # replace year at discharge (2022-11-01)
  dplyr::mutate(edad_al_egres_imp= dplyr::case_when(!is.na(dob_imp) & fech_nac!= dob_imp~ lubridate::time_length(lubridate::interval(dob_imp, fech_egres_imp),unit="years"), !is.na(dob_imp) & is.na(fech_nac)~ lubridate::time_length(lubridate::interval(dob_imp, fech_egres_imp),unit="years"), T~ as.numeric(edad_al_egres_1)))%>%   
  dplyr::select(-sex_imp, -sexo_2, -edad_al_ing_1_b, -edad_al_ing_imp) %>% 
  #2022-11-01
  dplyr::mutate(fech_nac_rec=dplyr::case_when(!is.na(dob_imp) & fech_nac!= dob_imp~ dob_imp, !is.na(dob_imp) & is.na(fech_nac)~ dob_imp, T~ fech_nac))

# Age at admission for each treatment: if the date of birth (PO) is not empty and the date of birth of senda is different of date of birth, for each age at admission (for different admissions), we compute the difference of the date of admission (at each admission) with the date of birth (PO) and divided by years; if the date of birth (PO) is not empty but SENDA is empty, we compute the difference of the date of admission (at each admission) with the date of birth (PO) and divided by years; else will be taken from the date at admission from the date of birth of SENDA.
#The same for age at discharge


CONS_C1_df_dup_SEP_2020_22_d <- CONS_C1_df_dup_SEP_2020_22_c
for (i in 1:10) {
  yr<- 365.25
  column_name <- paste0("fech_ing_",1:10)[i]
  column_name2 <- paste0("fech_egres_imp_",1:10)[i]
  log_column_name <- paste0("edad_al_ing_",1:10)[i]
  log_column_name2 <- paste0("edad_al_egres_",1:10)[i]
  
  CONS_C1_df_dup_SEP_2020_22_d <- 
    CONS_C1_df_dup_SEP_2020_22_d %>% 
    #age at admission
    dplyr::mutate(!!log_column_name := lubridate::time_length(lubridate::interval(fech_nac_rec, !!rlang::sym(column_name)), unit="years")) %>% 
    #age at discharge  
    dplyr::mutate(!!log_column_name2 := lubridate::time_length(lubridate::interval(fech_nac_rec, !!rlang::sym(column_name2)), unit="years")) %>% 
    as.data.table()%>% 
    purrr::when(nrow(.)>nrow(CONS_C1_df_dup_SEP_2020_22) ~ stop("More cases in the new database"), ~.) 
  
}

#_#_#_#_#_#__#_#_#_#_#_#_
#https://github.com/hputter/mstate/blob/master/R/plot.MarkovTest.R

# Base_fiscalia_v8 %>% 
#   #arrange the rut from the first date of comission of a crime, but we are not detecting if he/she is the victim or not
#   dplyr::arrange(rut_enc_saf, fec_comision_simple) %>% 
#   dplyr::right_join(subset(CONS_C1_df_dup_SEP_2020_22_d, subset= dup==1),by=c("rut_enc_saf"="hash_key")) %>%
  subset(CONS_C1_df_dup_SEP_2020_22_d, subset= dup==1) %>% 
  data.frame() %>% 
   # janitor::tabyl(tipo_centro_pub_10)
  rio::export(file = paste0("_ig_borquez/fiscalia_ig_bo_dic_2022_SENDA.dta"))

#table(is.na(subset(CONS_C1_df_dup_SEP_2020_22_d, subset= dup==1)$sex))
#table(is.na(subset(CONS_C1_df_dup_SEP_2020_22_d, subset= dup==1)$fech_nac_rec))  
Base_fiscalia_v8 %>% 
  rio::export(file = paste0("fiscalia_ig_bo_sep_2022.dta"))