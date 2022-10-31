rm(list=ls());gc()
if(!grepl("4.1.2",R.version.string)){stop("Different version (must be 4.1.2)")}
path<-getwd()#we define it again later in setup chunk
if (grepl("CISS Fondecyt",path)==T){
  try(setwd("C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)"));load("C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/13.Rdata")
} else if (grepl("andre",path)==T){
  try(setwd('C:/Users/andre/Desktop/SUD_CL/'));load("E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/13.Rdata")
} else if (grepl("E:",path)==T){
  try(setwd("E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/SUD_CL/"));load("E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/13.Rdata")
} else {
  try(setwd(paste0(path)));load(paste0(gsub("SUD_CL","",gsub("2022","2019",path)),"/13.Rdata"))
}

CONS_C1_df_dup_SEP_2020_22<-
  CONS_C1_df_dup_SEP_2020 %>% 
  subset(select= c("hash_key", "dup", "ano_bd_first", "duplicates_filtered", "id_centro", "tipo_centro", "tipo_de_programa_2", "tipo_de_plan_2", "senda", "macrozona", "nombre_region", "escolaridad_rec", "estado_conyugal_2", "compromiso_biopsicosocial", "edad_ini_cons", "edad_ini_sus_prin" ,"edad_ini_sus_prin_grupos", "freq_cons_sus_prin", "via_adm_sus_prin_act", 
                   "sus_ini_2_mod", "sus_ini_3_mod", "sus_ini_mod", "con_quien_vive", "sus_principal_mod", "tipo_de_vivienda_mod", "tenencia_de_la_vivienda_mod", "rubro_trabaja_mod", "cat_ocupacional", "estatus_ocupacional", "sus_ini_mod_mvv","cat_ocupacional_corr", "condicion_ocupacional_corr", "otras_sus1_mod", "otras_sus2_mod",  "otras_sus3_mod", "fech_ing_num", "fech_egres_num", "motivodeegreso_mod_imp","motivoegreso_derivacion", "evaluacindelprocesoteraputico",
                   paste0("tipo_de_plan_2_",1:10),paste0("motivodeegreso_mod_imp_",1:10),
                   paste0("dias_treat_imp_sin_na_",1:10), "dg_trs_cons_sus_or", "dg_total_cie_10", "dg_cie_10_rec", "dg_total_dsm_iv", "dg_dsm_iv_rec", "cnt_diagnostico_trs_fisico", "diagnostico_trs_fisico", "dg_fis_anemia", "dg_fis_card", "dg_fis_in_study", "dg_fis_enf_som", "dg_fis_ets", "dg_fis_hep_alc", "dg_fis_hep_b", "dg_fis_hep_cro", "dg_fis_inf", "dg_fis_otr_cond_fis_ries_vit", "dg_fis_otr_cond_fis", "dg_fis_pat_buc", "dg_fis_pat_ges_intrau", "dg_fis_trau_sec", "otros_pr_sm_abu_sex", "otros_pr_sm_exp_com_sex", "otros_pr_sm_otros", "otros_pr_sm_vif")) %>% 
  dplyr::mutate(comorbidity_icd_10=dplyr::case_when(dg_total_cie_10>=2~ "Two or more", dg_total_cie_10==1~ "One", as.character(dg_cie_10_rec)=="Diagnosis unknown (under study)"~"Diagnosis unknown (under study)", as.character(dg_cie_10_rec)=="Without psychiatric comorbidity"~"Without psychiatric comorbidity")) %>%
  dplyr::mutate(comorbidity_icd_10=as.factor(comorbidity_icd_10)) %>% 
  dplyr::mutate(estatus_ocupacional= dplyr::case_when(!is.na(cat_ocupacional)&!is.na(estatus_ocupacional)~"Empleado", TRUE~as.character(estatus_ocupacional)))%>% 
  dplyr::mutate(estatus_ocupacional= as.factor(estatus_ocupacional))%>% 
  dplyr::mutate(cnt_mod_cie_10_dg_cons_sus_or= dplyr::case_when(as.character(dg_trs_cons_sus_or)=="Drug dependence"~dg_total_cie_10+1, TRUE~dg_total_cie_10))%>% 
  dplyr::mutate(freq_cons_sus_prin= dplyr::case_when(as.character(freq_cons_sus_prin)=="Did not use"~ "Less than 1 day a week", TRUE~as.character(freq_cons_sus_prin)))%>% 
  dplyr::mutate(freq_cons_sus_prin= as.factor(freq_cons_sus_prin)) %>% 
  dplyr::mutate(tipo_centro_pub= factor(dplyr::if_else(as.character(tipo_centro)=="Public",TRUE,FALSE,NA))) %>%  dplyr::mutate(dg_trs_fis_rec= factor(dplyr::case_when(as.character(diagnostico_trs_fisico)=="En estudio"~"Diagnosis unknown (under study)",as.character(diagnostico_trs_fisico)=="Sin trastorno"~'Without physical comorbidity',cnt_diagnostico_trs_fisico>0 ~'With physical comorbidity',
                                                                                                                                                                       TRUE~NA_character_)))%>%
  dplyr::mutate(escolaridad_rec= readr::parse_factor(as.character(escolaridad_rec),levels=c('3-Completed primary school or less', '2-Completed high school or less', '1-More than high school'), ordered=T,trim_ws=T,include_na =F, locale=readr::locale(encoding = "Latin1"))) %>%   
  dplyr::mutate(freq_cons_sus_prin= readr::parse_factor(as.character(freq_cons_sus_prin),levels=c('Did not use', 'Less than 1 day a week','2 to 3 days a week','4 to 6 days a week','1 day a week or more','Daily'), ordered =T,trim_ws=T,include_na =F, locale=readr::locale(encoding = "UTF-8"))) %>% 
  dplyr::mutate(evaluacindelprocesoteraputico= dplyr::case_when(grepl("1",as.character(evaluacindelprocesoteraputico))~'1-High Achievement',grepl("2",as.character(evaluacindelprocesoteraputico))~'2-Medium Achievement',grepl("3",as.character(evaluacindelprocesoteraputico))~'3-Minimum Achievement', TRUE~as.character(evaluacindelprocesoteraputico))) %>% 
  dplyr::mutate(evaluacindelprocesoteraputico= readr::parse_factor(as.character(evaluacindelprocesoteraputico),levels=c('1-High Achievement', '2-Medium Achievement','3-Minimum Achievement'), ordered =T,trim_ws=T,include_na =F, locale=readr::locale(encoding = "UTF-8"))) %>% 
  dplyr::mutate(tenencia_de_la_vivienda_mod= factor(dplyr::case_when(tenencia_de_la_vivienda_mod=="Allegado"~"Stays temporarily with a relative", tenencia_de_la_vivienda_mod=="Arrienda"~"Renting", tenencia_de_la_vivienda_mod=="Cedida"~"Owner/Transferred dwellings/Pays Dividends", tenencia_de_la_vivienda_mod=="Ocupación Irregular"~"Illegal Settlement", tenencia_de_la_vivienda_mod=="Otros"~"Others", tenencia_de_la_vivienda_mod=="Paga dividendo"~"Owner/Transferred dwellings/Pays Dividends", tenencia_de_la_vivienda_mod=="Propia"~"Owner/Transferred dwellings/Pays Dividends", T~NA_character_))) %>% 
  dplyr::mutate(freq_cons_sus_prin=dplyr::case_when(freq_cons_sus_prin=="1 day a week or less"~"1 day a week or more",T~as.character(freq_cons_sus_prin))) %>% 
  dplyr::mutate(freq_cons_sus_prin=ordered(freq_cons_sus_prin,levels=c("Did not use", "Less than 1 day a week", "1 day a week or more", "2 to 3 days a week","4 to 6 days a week", "Daily"))) %>%   
  data.table::data.table() %>% 
  dplyr::group_by(hash_key) %>% 
  dplyr::mutate(rn_hash_discard=row_number())%>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(fech_ing_num_discard=fech_ing_num, fech_egres_num_discard= fech_egres_num) %>% 
  tidyr::pivot_wider(
    names_from =  rn_hash_discard, 
    names_sep="_",
    values_from = c(fech_ing_num_discard, fech_egres_num_discard))%>% #glimpse() 
  dplyr::group_by(hash_key)%>%
  dplyr::mutate_at(vars(fech_ing_num_discard_1:fech_egres_num_discard_10),~suppressWarnings(max(as.character(.),na.rm=T)))%>%
  dplyr::ungroup()

name_vec <- setNames(c(paste0("fech_ing_num_discard_",1:10),paste0("fech_egres_num_discard_",1:10)),c(paste0("fech_ing_num_",1:10),paste0("fech_egres_num_",1:10)))

CONS_C1_df_dup_SEP_2020_22<-
  CONS_C1_df_dup_SEP_2020_22 %>% rename(!!!name_vec)

#_#_#_#_#_#__#_#_#_#_#_#_

Base_fiscalia_v8 %>% 
  #arrange the rut from the first date of comission of a crime, but we are not detecting if he/she is the victim or not
  dplyr::arrange(rut_enc_saf, fec_comision_simple) %>% 
  dplyr::right_join(subset(CONS_C1_df_dup_SEP_2020_22, subset= dup==1),by=c("rut_enc_saf"="hash_key")) %>%
  data.table::data.table() %>% 
  rio::export(file = paste0("fiscalia_ig_bo_sep_2022_match_SENDA.dta"))

Base_fiscalia_v8 %>% 
  rio::export(file = paste0("fiscalia_ig_bo_sep_2022.dta"))