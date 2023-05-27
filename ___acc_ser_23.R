rm(list=ls());gc()
load("data_acc_ser_23.RData")


local({r <- getOption("repos")
r["CRAN"] <- "http://cran.r-project.org" 
options(repos=r)
})
copiar_nombres <- function(x,row.names=FALSE,col.names=TRUE,dec=",",...) {
  if(class(try(dplyr::ungroup(x)))[1]=="tbl_df"){
    if(options()$OutDec=="."){
      options(OutDec = dec)
      write.table(format(data.frame(x)),"clipboard",sep="\t",row.names=FALSE,col.names=col.names,...)
      options(OutDec = ".")
      return(x)
    } else {
      options(OutDec = ",")
      write.table(format(data.frame(x)),"clipboard",sep="\t",row.names=FALSE,col.names=col.names,...)
      options(OutDec = ",")
      return(x)    
    }
  } else {
    if(options()$OutDec=="."){
      options(OutDec = dec)
      write.table(format(x),"clipboard",sep="\t",row.names=FALSE,col.names=col.names,...)
      options(OutDec = ".")
      return(x)
    } else {
      options(OutDec = ",")
      write.table(format(x),"clipboard",sep="\t",row.names=FALSE,col.names=col.names,...)
      options(OutDec = ",")
      return(x)       
    }
  }
}  
pacman::p_unlock(lib.loc = .libPaths()) #para no tener problemas reinstalando paquetes

if(!require(pacman)){install.packages("pacman")}
if(!require(devtools)){install.packages("devtools", type = "win.binary", dependencies=T)}

pacman::p_load(APCtools, ggpattern, withr, boot, matrixStats, knitr, tidyr, stringi,stringr, ggplot2, Hmisc, kableExtra, plotly, janitor, rbokeh, zoo, broom, sqldf, devtools, codebook, data.table, panelr, RColorBrewer, lsmeans, finalfit, ggiraph, sf, treemapify, dplyr, tidyverse, epiR, survminer, ggfortify, survMisc, foreign, reshape2, stargazer, tableone, MatchIt, cobalt, eha, igraph, Amelia, DiagrammeR, DiagrammeRsvg, rsvg, mstate, htmltools, webshot, flexsurv, muhaz, Metrics, rpivotTable, caret, polycor, ClusterR, flextable, ggstatsplot, ggside, daff, explore, sjPlot, compareGroups, job, missForest, showtext, ggpattern, distill, showtext, googleVis, tidylog, magick, dlookr, easystats, tidylog, sqldf,  adjustedCurves, ggpmisc, rms, install=T)

#Error in if (options$noisey == TRUE) message(paste("\n", options$engine, : argument is of length zero


if(!require(survcomp)){BiocManager::install("survcomp")}




Base_fiscalia_v10<-
  sqldf("SELECT *
  FROM CONS_C1_df_dup_SEP_2020_22_d AS x  
  LEFT JOIN (SELECT *
             FROM Base_fiscalia_v9
             ) AS y
  ON x.hash_key == y.id AND 
  x.edad_al_ing_1 <= y.age_offending_imp AND x.dup = 1") #2022-11-25  added dup
#183307
invisible("It might be that those discharged not necessarily were, because this date corresponded to a referral")

paste0("Observations of SENDA database: ",nrow(CONS_C1_df_dup_SEP_2020_22_d))#109756)
paste0("Observations of PO database: ", nrow(Base_fiscalia_v10))#204,115 nrow

Base_fiscalia_v11<-
  Base_fiscalia_v10 %>% 
  #discrepancies in names of variables
  janitor::clean_names() %>%   #janitor::tabyl(!is.na(dob_imp_num))
  #previously recoded, 
  dplyr::select(-sex_2, -dateofbirth_imp, -country, -victim, -id_victim, -crime_code_c , -reg_c, -end_type_2c, -cod_comunadelito, -cod_lugarocurrencia, -sex_imp, -region_delito, -filter, -id)%>%
  plyr::rename(c("dateofbirth_imp_2"="dateofbirth_imp")) %>% 
  dplyr::ungroup() %>% 
  #_#_#_#_#_#_#_#_
  #generates errors with survival setting
  #make censorship date of age of comission
  purrr::when(dplyr::filter(., is.na(fech_nac_rec)) %>% nrow() >7 ~ stop("Missing values in the age"), ~.) %>% 
  dplyr::mutate(age_offending_imp= dplyr::case_when(is.na(age_offending_imp)~
                                                      lubridate::time_length(lubridate::interval(fech_nac_rec, as.Date("2019-11-13")),unit="years"), T~ age_offending_imp)) %>% 
  dplyr::group_by(hash_key) %>% 
  #KEY STEP: select the first and with ties (more than  one) --> 2023-04-14, was discussed, but did not had consequences (See https://docs.google.com/document/d/1UvtQFM3ToazUyA6G9C7pBYMgk98n31zQSY2-M9d1nEo/edit#)
  dplyr::slice_min(age_offending_imp, n = 1, with_ties = T) %>% 
  dplyr::ungroup() %>% 
  purrr::when(nrow(dplyr::filter(.,age_offending_imp-edad_al_ing_1<0))>0 ~ stop("Cases with negative time after admission to commission of crime"), ~.) %>% 
  dplyr::mutate(motivodeegreso_mod_imp_rec= dplyr::case_when(grepl("Therapeutic",motivodeegreso_mod_imp)~ "Treatment completion", grepl("Early|Late|Administrative", motivodeegreso_mod_imp) & (fech_egres_num_1-fech_ing_num_1 <90) ~  "Treatment non-completion (Early)", grepl("Early|Late|Administrative", motivodeegreso_mod_imp) & (fech_egres_num_1-fech_ing_num_1 >=90) ~ "Treatment non-completion (Late)", grepl("Referral|Death|Ongoing", motivodeegreso_mod_imp)~ "Censored", T~NA_character_))

# Base_fiscalia_v11 %>% 
#   dplyr::group_by(hash_key) %>% 
#   summarise(n=n()) %>% 
#   dplyr::filter(n>2)
invisible("2,082  hash key que se repiten; 404 se repiten más de una vez")

warning(paste0("There are ",nrow(dplyr::group_by(Base_fiscalia_v11, hash_key) %>% dplyr::mutate(rn_hash=row_number()) %>% dplyr::filter(rn_hash>1))," cases with more than one offense commited at the youngest age (p= ",dplyr::group_by(Base_fiscalia_v11, hash_key) %>% dplyr::mutate(rn_hash=row_number()) %>% dplyr::filter(rn_hash>1) %>% dplyr::distinct(hash_key) %>% nrow(),")"))

warning(paste0("There are ",nrow(dplyr::filter(Base_fiscalia_v11,is.na(fech_nac_rec)))," missing cases in date of birth (were ",nrow(dplyr::filter(janitor::clean_names(Base_fiscalia_v10),is.na(fech_nac_rec)))," in Base_fiscalia_v10)"))

warning(paste0("There are ", scales::percent(as.numeric(table(is.na(Base_fiscalia_v11$crime_code_group_rec))[[2]])/nrow(Base_fiscalia_v11)), " observations with events of contacts with justice"))

# 2022-11-01, filter
# Base_fiscalia_v12<-
#   dplyr::filter(Base_fiscalia_v11, !grepl("Referral|Death|Censored|Ongoing",motivodeegreso_mod_imp))

invisible("Ver por qué los valores negativos-R: por que entre la admisión y terminar el tratamiento hay casos que registraron un delito")
invisible("ver por qué no recodifica a los motivo de egresos de manera iterativa")
invisible("QUEDA POR LIMPIAR LA BASE DE FISCALIA")
invisible("Qué hacer con Death, Referral to another treatment, Ongoing treatment")

Base_fiscalia_v10b<-
  sqldf("SELECT *
  FROM CONS_C1_df_dup_SEP_2020_22_d AS x  
  LEFT JOIN (SELECT *
             FROM Base_fiscalia_v9
             ) AS y
  ON x.hash_key == y.id AND 
  x.edad_al_ing_1 > y.age_offending_imp AND x.dup = 1") #2022-11-25  added dup // #changed the direction to past events, where age at discharge is greater than the age of commission


Base_fiscalia_v11b<-
  Base_fiscalia_v10b %>% 
  #discrepancies in names of variables
  janitor::clean_names() %>%   #janitor::tabyl(!is.na(dob_imp_num))
  #previously recoded, 
  dplyr::select(-dateofbirth_imp, -country, -victim, -id_victim, -crime_code_c , -reg_c, -end_type_2c, -cod_comunadelito, -cod_lugarocurrencia, -sex_imp, -region_delito, -filter, -id)%>%
  plyr::rename(c("dateofbirth_imp_2"="dateofbirth_imp")) %>% 
  dplyr::ungroup() %>% 
  #selected the first row with distinct information regarding patient ID, case ID, crime code.
  dplyr::group_by(hash_key, caseid, crime_code_group_rec_prof) %>%
  dplyr::slice(1) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(hash_key) %>% 
  summarise(n_off_acq= ifelse(sum(crime_code_group_rec_prof=="Acquisitive", na.rm=T)>0, 1,0), n_off_vio= ifelse(sum(crime_code_group_rec_prof=="Violent", na.rm=T)>0, 1,0), n_off_sud= ifelse(sum(crime_code_group_rec_prof== "Substance-related", na.rm=T)>0, 1,0), n_off_oth=  ifelse(sum(crime_code_group_rec_prof== "Other", na.rm=T)>0, 1,0)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(n_prev_off= rowSums(select(., starts_with("n_")))) 

warning(paste0("Users in the database of previous crimes: ",format(length(unique(Base_fiscalia_v11b$hash_key)), big.mark=",")))

warning(paste0("Users in the merged database (after filtering for observations coded as referrals, deaths, censored at baseline treatment or with ongoing treatments) : ",format(length(unique(Base_fiscalia_v12$hash_key)), big.mark=",")))

# Pre-treatment Criminality 
Base_fiscalia_v13<-
  Base_fiscalia_v11 %>% 
  dplyr::group_by(hash_key) %>% 
  #select the first and without ties (only one)
  dplyr::slice_min(age_offending_imp, n = 1, with_ties = F) %>% 
  dplyr::ungroup() %>% 
  dplyr::inner_join(Base_fiscalia_v11b, by="hash_key") %>% 
  #to see 
  #dplyr::select(hash_key, fech_nac_rec, n_off_acq, )
  dplyr::mutate(policonsumo= ifelse(!is.na(otras_sus1_mod),1,0)) %>% 
  dplyr::mutate(cut_fec_nac=cut2(fech_nac_rec, cuts=as.Date(attr(dlookr::binning(as.numeric(fech_nac_rec)),"breaks"))),cut_com_del=cut2(fec_comision_simple, cuts=as.Date(attr(dlookr::binning(as.numeric(fec_comision_simple)),"breaks")))) %>%
  dplyr::mutate(tr_modality=dplyr::case_when(grepl("PR", as.character(tipo_de_plan_2_1))~ "Residential", grepl("PAI|PAB", as.character(tipo_de_plan_2_1))~ "Ambulatory", T~ NA_character_)) %>% 
  dplyr::mutate(time_to_off_from_adm=age_offending_imp-edad_al_egres_imp) %>% 
  dplyr::mutate(time_to_off_from_disch=age_offending_imp-edad_al_egres_imp) %>% 
  as.data.table()%>% 
  purrr::when(nrow(.)>nrow(Base_fiscalia_v11) ~ stop("More cases in the new database"), ~.) 

#length(unique(Base_fiscalia_v13$hash_key))   
warning(paste0("Number of cases that are different by at least 0,02 years between 'edad_al_egres_imp' & 'edad_al_egres_1'= ",
               nrow(cbind.data.frame(round(Base_fiscalia_v13$edad_al_ing_1,4),round(Base_fiscalia_v13$edad_al_ing_fmt,4)) %>% 
                      dplyr::filter(abs(.[[1]]-.[[2]])>0.02))," probably due to the discrepancies in getting the differences of dates"))

# cbind.data.frame(round(Base_fiscalia_v13$edad_al_ing_1,4),
#                  round(Base_fiscalia_v13$edad_al_ing_fmt,4)) %>% 
#   dplyr::mutate(diff=abs(.[[1]]-.[[2]])) %>% 
#   dplyr::filter(diff>0.02)

invisible("Recode in 0/1 for more than one treatment")
Base_fiscalia_v13$dup_filt<- ifelse(Base_fiscalia_v13$duplicates_filtered>1,1,0)

Base_fiscalia_v13 %>% 
  janitor::tabyl(motivodeegreso_mod_imp_rec,dup_filt)