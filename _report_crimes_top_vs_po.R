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


pacman::p_unlock(lib.loc = .libPaths()) #para no tener problemas reinstalando paquetes

if(!require(pacman)){install.packages("pacman")}
if(!require(devtools)){install.packages("devtools", type = "win.binary", dependencies=T)}

pacman::p_load(APCtools, ggpattern, withr, boot, matrixStats, knitr, tidyr, 
               stringi,stringr, ggplot2, Hmisc, kableExtra, plotly, janitor, 
               rbokeh, zoo, broom, sqldf, devtools, codebook, data.table, panelr, 
               RColorBrewer, lsmeans, finalfit, ggiraph, sf, treemapify, dplyr, 
               tidyverse, epiR, survminer, ggfortify, survMisc, foreign, reshape2, 
               stargazer, tableone, MatchIt, cobalt, eha, igraph, Amelia, DiagrammeR, 
               DiagrammeRsvg, rsvg, mstate, htmltools, webshot, flexsurv, muhaz, Metrics, 
               rpivotTable, caret, polycor, ClusterR, flextable, ggstatsplot, ggside, psych,
               daff, explore, sjPlot, compareGroups, job, missForest, showtext, ggpattern, 
               distill, showtext, googleVis, tidylog, magick, dlookr, easystats, tidylog, 
               sqldf,  adjustedCurves, ggpmisc, rms, install=T)

#Error in if (options$noisey == TRUE) message(paste("\n", options$engine, : argument is of length zero


if(!require(survcomp)){try(devtools::install_github("bhklab/survcomp",upgrade ="never"))}

try(webshot::install_phantomjs())

if(!require(bpmn)){try(devtools::install_github("bergant/bpmn",upgrade ="never"))}



Base_fiscalia_v10b_dic_2022<-
  sqldf("SELECT *
  FROM CONS_C1_df_dup_SEP_2020_22_d AS x  
  LEFT JOIN (SELECT *
             FROM Base_fiscalia_v9
             ) AS y
  ON x.hash_key == y.id AND 
  x.edad_al_egres_imp > y.age_offending_imp AND x.dup = 1") #2022-11-25  added dup // #changed the direction to past events, where age at discharge is greater than the age of commission


Base_fiscalia_v11b_dic_2022<-
  Base_fiscalia_v10b_dic_2022 %>% 
  #discrepancies in names of variables
  janitor::clean_names() %>%   #janitor::tabyl(!is.na(dob_imp_num))
  #previously recoded, 
  dplyr::select(-dateofbirth_imp, -country, -victim, -id_victim, -crime_code_c , -reg_c, -end_type_2c, -cod_comunadelito, -cod_lugarocurrencia, -sex_imp, -region_delito, -filter, -id)%>%
  plyr::rename(c("dateofbirth_imp_2"="dateofbirth_imp")) %>% 
  dplyr::ungroup() %>% 
  #selected the first row with distinct information regarding patient ID, case ID, crime code.
  dplyr::group_by(hash_key, dateofbirth_imp, caseid, crime_code_group_rec_prof) %>%
  dplyr::slice(1) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(hash_key,dateofbirth_imp) %>% 
  summarise(n_off_acq= ifelse(sum(crime_code_group_rec_prof=="Acquisitive", na.rm=T)>0, 1,0), n_off_vio= ifelse(sum(crime_code_group_rec_prof=="Violent", na.rm=T)>0, 1,0), n_off_sud= ifelse(sum(crime_code_group_rec_prof== "Substance-related", na.rm=T)>0, 1,0), n_off_oth=  ifelse(sum(crime_code_group_rec_prof== "Other", na.rm=T)>0, 1,0)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(n_prev_off= rowSums(select(., starts_with("n_")))) 



#```{r join4_pregunta_ACC, echo=T, fig.align='center', message=T, error=T, eval=F}

#Registrar hurtos, robos, violencia intrafamiliar y otras acciones cometidas en las últimas 4 semanas
#Violencia Intrafamiliar (Maltrato físico o psicológico)
CONS_TOP_2022<-
  # 107307
  CONS_TOP%>% 
  dplyr::left_join(subset(dplyr::mutate(dplyr::group_by(Base_fiscalia_v11b_dic_2022, hash_key), hash_rn=row_number())%>% ungroup(), hash_rn==1), by= c("HASH_KEY" = "hash_key"))%>% 
  dplyr::mutate(fech_ap_top_num= as.numeric(as.Date(str_sub(as.character(lubridate::parse_date_time(Fecha.Aplicación.TOP, c("%Y-%m-%d"),exact=T)),1,10))))%>% #No parse failures
  dplyr::select(HASH_KEY, fech_ap_top_num, Fecha.Aplicación.TOP, dateofbirth_imp, Hurto, Robo, Venta.Drogas, Riña, Total.VIF, Otro) %>% 
  dplyr::filter(!is.na(HASH_KEY)) %>% 
  dplyr::mutate_at(vars("Hurto", "Robo", "Venta.Drogas", "Riña", "Otro"), ~ifelse(.=="S",1,0)) %>% 
  dplyr::mutate(Total.VIF= ifelse(Total.VIF>0,1,0))%>% 
  dplyr::mutate(tot_off_top = base::rowSums(dplyr::select(.,c(Hurto, Robo, Venta.Drogas, Riña, Total.VIF, Otro)), na.rm = T)) %>% 
  dplyr::mutate(dateofbirth_imp_num= as.numeric(dateofbirth_imp),
                fech_ap_top= lubridate::parse_date_time(Fecha.Aplicación.TOP, c("%Y-%m-%d"),exact=T),
                edad_a_ap_top_num= lubridate::time_length(lubridate::interval(dateofbirth_imp, fech_ap_top),unit="years"),
                edad_b_ap_top_num= (fech_ap_top_num-dateofbirth_imp_num)/365.25,
                edad_a_ap_top_num_lim= edad_a_ap_top_num-(1/12),
                edad_b_ap_top_num_lim= edad_b_ap_top_num-(1/12)) %>% 
  dplyr::select(-dateofbirth_imp, -dateofbirth_imp_num) %>% 
  dplyr::filter(!is.na(edad_a_ap_top_num)) %>% 
  dplyr::group_by(HASH_KEY, edad_a_ap_top_num) %>% 
  dplyr::slice(1) %>% 
  dplyr::ungroup()


#reduzco la base a casos en los que no tenga la misma condena por el mismo delito

Base_fiscalia_v9_dic_2022<-
Base_fiscalia_v9 %>% 
  dplyr::group_by(id, caseid, end_type, fec_comision_simple, crime_code_c) %>% 
  dplyr::slice(1) %>% 
  dplyr::ungroup()


#busco en la base de datos de fiscalía, algún delito que haya cometido en el transcurso 
#del último mes anterior a la aplicación del TOP
Base_fiscalia_v13c_dic_2022<-
  sqldf("SELECT *
  FROM CONS_TOP_2022 AS x  
  LEFT JOIN (SELECT *
             FROM Base_fiscalia_v9_dic_2022
             ) AS y
  ON x.HASH_KEY == y.id AND 
  x.edad_a_ap_top_num > y.age_offending_imp AND 
  x.edad_a_ap_top_num_lim < y.age_offending_imp") #2022-11-25  added dup // #changed the direction to past events, where age at discharge is greater than the age of commission


#Luego, contar por cada combinación de HASH y fecha de aplicación, el número de delitos segun familai de delito
#crime_code_group
Base_fiscalia_v13c_dic_2022_2<-
  Base_fiscalia_v13c_dic_2022 %>% 
  dplyr::select(tidyr::any_of(c("HASH_KEY", "fech_ap_top_num", "Fecha.Aplicación.TOP", "Hurto", "Robo", "Venta.Drogas", "Riña",
                "Total.VIF", "Otro", "tot_off_top", "fech_ap_top", "edad_a_ap_top_num", "edad_b_ap_top_num",
                "edad_a_ap_top_num_lim", "edad_b_ap_top_num_lim", "caseid", "end_type", "fec_comision_simple",
                "crime_code_c", "crime_code_group_rec", "crime_code_group_rec_prof", "age_offending_imp")))
#HASH_KEY
#fech_ap_top_num
#Fecha.Aplicación.TOP
#Hurto
#Robo
#Venta.Drogas
#Riña
#Total.VIF
#Otro
#tot_off_top
#fech_ap_top
#edad_a_ap_top_num
#edad_b_ap_top_num
#edad_a_ap_top_num_lim
#edad_b_ap_top_num_lim
#caseid
#end_type
#fec_comision_simple
#crime_code_c
#crime_code_group_rec_prof
#crime_code_group_rec
#age_offending_imp

Base_fiscalia_v13c_dic_2022_3<-
Base_fiscalia_v13c_dic_2022_2 %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(vio=dplyr::if_else(crime_code_group_rec_prof=="Violent",1,0,0),
                acq=dplyr::if_else(crime_code_group_rec_prof=="Acquisitive",1,0,0),
                sud=dplyr::if_else(crime_code_group_rec_prof=="Substance-related",1,0,0),
                oth=dplyr::if_else(crime_code_group_rec_prof=="Other",1,0,0)) %>% 
  dplyr::mutate(tot= rowSums(select(., c("vio","acq","sud","oth")))) %>% 
  dplyr::group_by(HASH_KEY, fech_ap_top_num) %>% 
  dplyr::mutate(vio=sum(vio,na.rm=T), acq=sum(acq,na.rm=T), sud=sum(sud,na.rm=T), 
                oth=sum(oth,na.rm=T), tot=sum(tot,na.rm=T), n=n()) %>%
  dplyr::slice(1) %>% 
  dplyr::ungroup() 
#Los que tienen todo pelado en caseid es porque no tienen fecha de comisión en los últimos 30 días



dodge <- position_dodge(.3) # how much jitter on the x-axis?

Base_fiscalia_v13c_dic_2022_3 %>% 
  ggplot(aes(tot, tot_off_top))+
  geom_point(position = position_jitter(w = 0.8, h = 0.8))

mean(Base_fiscalia_v13c_dic_2022_3$tot,na.rm=T)
quantile(Base_fiscalia_v13c_dic_2022_3$tot,na.rm=T)
mean(Base_fiscalia_v13c_dic_2022_3$tot_off_top,na.rm=T)
quantile(Base_fiscalia_v13c_dic_2022_3$tot_off_top,na.rm=T)

cor.test(Base_fiscalia_v13c_dic_2022_3$tot_off_top, Base_fiscalia_v13c_dic_2022_3$tot, method="spearman", exact=F)
cor.test(Base_fiscalia_v13c_dic_2022_3$Venta.Drogas, Base_fiscalia_v13c_dic_2022_3$sud, method="spearman", exact=F)

cor.test(Base_fiscalia_v13c_dic_2022_3$tot_off_top, Base_fiscalia_v13c_dic_2022_3$tot)


var(Base_fiscalia_v13c_dic_2022_3$tot_off_top)/mean(Base_fiscalia_v13c_dic_2022_3$tot_off_top)
#The variance is much greater than the mean, which suggests that we will have over-dispersion in the model.

var(Base_fiscalia_v13c_dic_2022_3$Venta.Drogas,na.rm=T)/mean(Base_fiscalia_v13c_dic_2022_3$Venta.Drogas,na.rm=T)
#can be obtained with a poisson regression

ggstatsplot::ggscatterstats(tot_off_top, tot, data=Base_fiscalia_v13c_dic_2022_3, 
                            type="nonparametric", point.width.jitter=.9, point.height.jitter=.9,
                            xlab="Recuento delitos TOP", ylab="Recuento delitos Fiscalía", 
                            title="Relación entre datos fiscalía y TOP, con registros de delitos \ncometidos últimos 30 días desde la aplicación del TOP")

round(prop.table(table(Base_fiscalia_v13c_dic_2022_3$tot_off_top, Base_fiscalia_v13c_dic_2022_3$tot)),2)  

pol_p<-
polychor(Base_fiscalia_v13c_dic_2022_3$tot_off_top, Base_fiscalia_v13c_dic_2022_3$tot, std.err=T, ML=T)

pchisq(pol_p$chisq,pol_p$df, lower.tail=F)


pol_p2<-
  polychor(Base_fiscalia_v13c_dic_2022_3$Venta.Drogas, Base_fiscalia_v13c_dic_2022_3$sud, std.err=T, ML=T)

pchisq(pol_p2$chisq,pol_p2$df, lower.tail=F)

Base_fiscalia_v13c_dic_2022_3$adquisitivo<- ifelse(Base_fiscalia_v13c_dic_2022_3$Robo==0, Base_fiscalia_v13c_dic_2022_3$Hurto,0)

Base_fiscalia_v13c_dic_2022_3$acq_rec<-ifelse(Base_fiscalia_v13c_dic_2022_3$acq>0,1,0)

Base_fiscalia_v13c_dic_2022_3$tot_rec<-ifelse(Base_fiscalia_v13c_dic_2022_3$tot>0,1,0)


round(prop.table(table(Base_fiscalia_v13c_dic_2022_3$acq, Base_fiscalia_v13c_dic_2022_3$adquisitivo)),2)  
round(prop.table(table(Base_fiscalia_v13c_dic_2022_3$acq_rec, Base_fiscalia_v13c_dic_2022_3$adquisitivo)),2)  

chisq.test(table(Base_fiscalia_v13c_dic_2022_3$acq_rec, Base_fiscalia_v13c_dic_2022_3$adquisitivo))

ggstatsplot::ggpiestats(acq, adquisitivo, data=Base_fiscalia_v13c_dic_2022_3, 
                            type="nonparametric", point.width.jitter=.9, point.height.jitter=.9,
                        #palette = "default_jama",
                            xlab="Presencia delitos adquisitivos TOP", ylab="Recuento delitos Adquisitivos Fiscalía", 
                            title="Relación entre datos fiscalía y TOP, con registros de delitos \ncometidos últimos 30 días desde la aplicación del TOP")

library(ROCit)
## Warning: package 'ROCit' was built under R version 3.5.2
ROCit_obj <- rocit(score=Base_fiscalia_v13c_dic_2022_3$adquisitivo,class=Base_fiscalia_v13c_dic_2022_3$acq_rec)
plot(ROCit_obj)

Base_fiscalia_v13c_dic_2022_3$tot_off_top_rec<-ifelse(Base_fiscalia_v13c_dic_2022_3$tot_off_top>0,1,0)

ROCit_obj <- rocit(score=Base_fiscalia_v13c_dic_2022_3$tot_off_top,class=Base_fiscalia_v13c_dic_2022_3$tot_rec)
plot(ROCit_obj)

#Del YOuden, el óptimo es...
ciROC(ROCit_obj,nboot = 500)
#FPR: 1-especificidad
#TPR: Sensibilidad

ROCit_obj2 <- rocit(score=Base_fiscalia_v13c_dic_2022_3$tot_off_top_rec,class=Base_fiscalia_v13c_dic_2022_3$tot_rec)
plot(ROCit_obj2)

#Del YOuden, el óptimo es...
ciROC(ROCit_obj2,nboot = 500)
#FPR: 1-especificidad
#TPR: Sensibilidad