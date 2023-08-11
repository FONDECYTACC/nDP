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
if(!require(pacman)){install.packages("pacman")}
pacman::p_load(APCtools, ggpattern, withr, boot, matrixStats, knitr, tidyr, stringi,stringr, rateratio.test, ggplot2, Hmisc, kableExtra, plotly, janitor, rbokeh, zoo, broom, sqldf, devtools, codebook, data.table, panelr, RColorBrewer, lsmeans, finalfit, ggiraph, sf, treemapify, dplyr, tidyverse, epiR, survminer, ggfortify, survMisc, foreign, reshape2, stargazer, tableone, MatchIt, cobalt, eha, igraph, Amelia, DiagrammeR, DiagrammeRsvg, rsvg, mstate, htmltools, webshot, flexsurv, muhaz, Metrics, rpivotTable, caret, polycor, ClusterR, flextable, ggstatsplot, ggside, daff, explore, sjPlot, compareGroups, job, missForest, showtext, ggpattern, distill, showtext, googleVis, tidylog, magick, dlookr, easystats, tidylog, install=F)
try(webshot::install_phantomjs())

if(!require(bpmn)){try(devtools::install_github("bergant/bpmn",upgrade =F))}

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_


irrs<-function(x, y="event", z="person_days",db){
  #x= variable que agrupa
  #y= evento explicado
  #z= person days
  #db= base de datos
  fmla <- as.formula(paste0(y,"~",x))
  fmla2 <- as.formula(paste0(z,"~",x))
  assign(paste0("irr_",y,"_por_",x),
         rateratio.test::rateratio.test(
           x=as.numeric(xtabs(fmla, data=get(db)))[c(2,1)],
           n=as.numeric(xtabs(fmla, data=get(db)))[c(2,1)]
         )
  )
  return(
    rateratio.test::rateratio.test(
      x=as.numeric(xtabs(fmla, data=get(db)))[c(2,1)],
      n=as.numeric(xtabs(fmla2, data=get(db)))[c(2,1)]
    )
  )
}
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
invisible("Exported database in 22-09-2022")
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

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#_#_#_#_#_#__#_#_#_#_#_#_

Base_fiscalia_v8_join_Senda_offenders<-
Base_fiscalia_v8 %>% 
  #arrange the rut from the first date of commission of a crime, but we are not detecting if he/she is the victim or not
  dplyr::arrange(rut_enc_saf, fec_comision_simple) %>% 
  #AMPLIAMOS A IMPUTADOS Y VICTIMAS
  #dplyr::filter(encontrado_como_imputado=="SI") %>% 
  dplyr::mutate(edad_comision_imp_grupos=dplyr::case_when(edad_comision_imp>=50~ ">=50",
                                                          edad_comision_imp>=40~ "40-49",
                                                          edad_comision_imp>=30~"30-39",
                                                          edad_comision_imp>=18~ "18-29",
               T~ NA_character_))%>% 
  dplyr::mutate(edad_comision_imp_grupos= factor(edad_comision_imp_grupos, 
                levels=c("<=18","18-29", "30-39", "40-49", ">=50"),ordered =T)) %>% 
  
  dplyr::left_join(subset(CONS_C1_df_dup_SEP_2020_22, subset= dup==1),by=c("rut_enc_saf"="hash_key")) %>%
  dplyr::mutate(last = exec(pmax, !!! rlang::syms(paste0("fech_egres_num_",1:10)), na.rm = TRUE)) %>% 
  dplyr::mutate(entry = exec(pmin, !!! rlang::syms(paste0("fech_ing_num_",1:10)), na.rm = TRUE)) %>% 
  dplyr::filter(!is.na(fech_ing_num)) %>% 
  dplyr::mutate(edad_comision_imp_grupos=dplyr::case_when(edad_comision_imp>=50~ ">=50",
                                                          edad_comision_imp>=40~ "40-49",
                                                          edad_comision_imp>=30~"30-39",
                                                          edad_comision_imp>=18~ "18-29",
                                                          T~ NA_character_))%>% 
  data.table::data.table() 
  

invisible("1. qué es incidencia de delitos")
invisible("1. se seleccionan casos válidos calzados por SENDA con FISCALIA")
invisible("1. si es sólo delitos, lo hago con la base de fiscalía: obtengo el tiempo disponible en el sis. judicial")
invisible("1. selecciono los delitos únicos de la combinación RUT-RUC-delito, luego selecciono la primera 
          fila para cada combinación. y así no trabajamos con relaciones (en caso en que para una causa ese mismo delito se replique o afecte a más de una víctima)")
invisible("1. mantengo tanto a imputados como a no, para ver cuánto tiempo estuvieron disponibles en el sistema")

Base_fiscalia_v8_MJ_inc<-
  Base_fiscalia_v8_join_Senda_offenders %>% 
  dplyr::group_by(rut_enc_saf, ruc, gls_materia) %>% 
  dplyr::slice(1) %>% 
#475,128
  dplyr::ungroup() %>% 
  #2022-10-02 include an age at 2019-11-13
  dplyr::mutate(edad_comision_imp_cens=as.numeric(as.Date("2019-11-13")-imp_birth_date)/365.25) %>%
  dplyr::group_by(rut_enc_saf) %>% 
  dplyr::mutate(edad_comision_imp_t_persona=max(edad_comision_imp_cens,na.rm=T)-min(edad_comision_imp,na.rm=T),
                #cuento la cantidad de delitos en los que el usuario ha sido imputado.
                number_of_distinct_offenses_by_ruc_hash= sum(encontrado_como_imputado=="SI",na.rm=T)
                ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(event=dplyr::if_else(number_of_distinct_offenses_by_ruc_hash>0,1,0,NA_real_))  
  
invisible("Veo los casos únicos por RUT")
  Base_fiscalia_v8_MJ_inc2<-
    dplyr::distinct(Base_fiscalia_v8_MJ_inc,rut_enc_saf,.keep_all=T) %>% 
    dplyr::mutate(programa=factor(dplyr::case_when(grepl("PG-PR",tipo_de_plan_2_1)~"Residencial",
                                                   grepl("PG-PAB|PG-PAI",tipo_de_plan_2_1)~"Ambulatorio",
                                                   grepl("M-",tipo_de_plan_2_1)~"Mujeres",
                                                   T~NA_character_),levels=c("Ambulatorio","Residencial","Mujeres")))%>%
    dplyr::mutate(modalidad=factor(dplyr::case_when(grepl("PR",tipo_de_plan_2_1)~"Residencial",
                                                   grepl("PAB|PAI",tipo_de_plan_2_1)~"Ambulatorio",
                                                   T~NA_character_),levels=c("Ambulatorio","Residencial")))%>%
    data.table::data.table()

invisible("Calculo la incidencia")
irr<-
biostat3::survRate(Surv(as.numeric(edad_comision_imp_t_persona), 
                        event==1) ~ 1, 
                   data=Base_fiscalia_v8_MJ_inc2)
cbind(irr[,1:2],round(irr[,3:5]*1e5,0))


irr_edad<-
biostat3::survRate(Surv(edad_comision_imp_t_persona, 
                                   event) ~ edad_comision_imp_grupos, 
                                   data=Base_fiscalia_v8_MJ_inc2)
cbind(irr_edad[,1:3],(irr_edad[,4:6]*1e5)) %>% 
  data.table() %>% mutate(res= paste0(round(rate,0),"(IC 95%= ",round(lower,0),",",round(upper,0),")")) %>% 
  copiar_nombres()

irrs(y="event",x="edad_comision_imp_grupos",z="edad_comision_imp_t_persona" ,db="Base_fiscalia_v8_MJ_inc2")

irr_sex<-
  biostat3::survRate(Surv(edad_comision_imp_t_persona, 
                          event) ~ sex_imp, 
                     data=Base_fiscalia_v8_MJ_inc2)
cbind(irr_sex[,1:3],(irr_sex[,4:6]*1e5)) %>% data.table() %>% mutate(res= paste0(round(rate,0),"(IC 95%= ",round(lower,0),",",round(upper,0),")")) %>% 
  copiar_nombres()

irrs(y="event",x="sex_imp",z="edad_comision_imp_t_persona" ,db="Base_fiscalia_v8_MJ_inc2")


irr_esc<-
  biostat3::survRate(Surv(edad_comision_imp_t_persona, 
                          event) ~ escolaridad_rec, 
                     data=Base_fiscalia_v8_MJ_inc2)
cbind(irr_esc[,1:3],(irr_esc[,4:6]*1e5)) %>% data.table() %>% mutate(res= paste0(round(rate,0),"(IC 95%= ",round(lower,0),",",round(upper,0),")")) %>% 
  copiar_nombres()

irrs(y="event",x="escolaridad_rec",z="edad_comision_imp_t_persona" ,db="Base_fiscalia_v8_MJ_inc2")


irr_modalidad<-
  biostat3::survRate(Surv(edad_comision_imp_t_persona, 
                          event) ~ modalidad, 
                     data=Base_fiscalia_v8_MJ_inc2)
cbind(irr_modalidad[,1:3],(irr_modalidad[,4:6]*1e5)) %>% data.table() %>% mutate(res= paste0(round(rate,0),"(IC 95%= ",round(lower,0),",",round(upper,0),")")) %>% 
  copiar_nombres()

irrs(y="event",x="modalidad",z="edad_comision_imp_t_persona" ,db="Base_fiscalia_v8_MJ_inc2")


irr_programa<-
  biostat3::survRate(Surv(edad_comision_imp_t_persona, 
                          event) ~ programa, 
                     data=Base_fiscalia_v8_MJ_inc2)
cbind(irr_programa[,1:3],(irr_programa[,4:6]*1e5)) %>% data.table() %>% mutate(res= paste0(round(rate,0),"(IC 95%= ",round(lower,0),",",round(upper,0),")")) %>% 
  copiar_nombres()

irrs(y="event",x="programa",z="edad_comision_imp_t_persona" ,db="Base_fiscalia_v8_MJ_inc2")


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

xtabs(number_of_distinct_offenses_by_ruc_hash~event, 
      data=Base_fiscalia_v8_MJ_inc2)

xtabs(number_of_distinct_offenses_by_ruc_hash~event+edad_comision_imp_grupos, 
      data=Base_fiscalia_v8_MJ_inc2)

xtabs(number_of_distinct_offenses_by_ruc_hash~event+sex_imp, 
      data=Base_fiscalia_v8_MJ_inc2)

xtabs(number_of_distinct_offenses_by_ruc_hash~event+escolaridad_rec, 
      data=Base_fiscalia_v8_MJ_inc2)

xtabs(number_of_distinct_offenses_by_ruc_hash~event+modalidad, 
      data=Base_fiscalia_v8_MJ_inc2)

xtabs(number_of_distinct_offenses_by_ruc_hash~event+programa, 
      data=Base_fiscalia_v8_MJ_inc2)
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_


m1<-as.matrix(cbind(
  xtabs(~event, 
        data=subset(dplyr::distinct(Base_fiscalia_v8_MJ_inc2,rut_enc_saf,.keep_all=T)))[[2]],
  xtabs(edad_comision_imp_t_persona~event, 
        data=dplyr::distinct(Base_fiscalia_v8_MJ_in2c,rut_enc_saf,.keep_all=T))[[2]]))

epi.conf(m1, ctype = "inc.rate", method = "exact", N = 472742, design = 1, 
         conf.level = 0.95) * 1e5

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_



#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#gls_tipo_imputado  medida_alternativa_46 

tbone_event_tipo_imp<-
  CreateTableOne(vars= c("edad_comision_imp_grupos",  "sex_imp",  "escolaridad_rec",  "modalidad",  "programa"), 
     data=  Base_fiscalia_v8_MJ_inc %>% dplyr::filter(encontrado_como_imputado=="SI"), 
     factorVars = c("edad_comision_imp_grupos",  "sex_imp",  "escolaridad_rec",  "modalidad",  "programa"), 
     smd=T, strata=c("gls_tipo_imputado"), addOverall = T, includeNA=T,test=T)

as.data.frame.TableOne(tbone_event_tipo_imp, smd=T)%>% 
  dplyr::mutate(char2=characteristic) %>% 
  tidyr::fill(char2) %>% 
  dplyr::select(char2,everything()) %>% 
  dplyr::mutate(level=ifelse(is.na(level),"[Missing]",level)) %>% 
  dplyr::mutate(char2=dplyr::case_when(characteristic=="NA"~NA_character_,T~as.character(characteristic))) %>% 
  format_cells(1, 1:length(names(.)), "bold") %>%
  dplyr::select(-1) %>% 
  kable("html", size=10, format="html",caption= "Descriptivos por Tipo de Imputado") %>% 
  kableExtra::kable_classic()

tbone_medida_alt<-
  CreateTableOne(vars= c("edad_comision_imp_grupos",  "sex_imp",  "escolaridad_rec",  "modalidad",  "programa"), 
                 data=  Base_fiscalia_v8_MJ_inc %>% dplyr::filter(encontrado_como_imputado=="SI"), 
                 factorVars = c("edad_comision_imp_grupos",  "sex_imp",  "escolaridad_rec",  "modalidad",  "programa"), 
                 smd=T, strata=c("medida_alternativa_46"), addOverall = T, includeNA=T,test=T)

as.data.frame.TableOne(tbone_medida_alt, smd=T)%>% 
  dplyr::mutate(char2=characteristic) %>% 
  tidyr::fill(char2) %>% 
  dplyr::select(char2,everything()) %>% 
  dplyr::mutate(level=ifelse(is.na(level),"[Missing]",level)) %>% 
  dplyr::mutate(char2=dplyr::case_when(characteristic=="NA"~NA_character_,T~as.character(characteristic))) %>% 
  format_cells(1, 1:length(names(.)), "bold") %>%
  dplyr::select(-1) %>% 
  kable(size=10, format="html",caption= "Descriptivos por Tipo de Medida alternativa", escape=T) %>% 
  kableExtra::kable_classic()