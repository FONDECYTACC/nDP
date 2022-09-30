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
pacman::p_load(APCtools, ggpattern, withr, boot, matrixStats, knitr, tidyr, stringi,stringr, ggplot2, Hmisc, kableExtra, plotly, janitor, rbokeh, zoo, broom, sqldf, devtools, codebook, data.table, panelr, RColorBrewer, lsmeans, finalfit, ggiraph, sf, treemapify, dplyr, tidyverse, epiR, survminer, ggfortify, survMisc, foreign, reshape2, stargazer, tableone, MatchIt, cobalt, eha, igraph, Amelia, DiagrammeR, DiagrammeRsvg, rsvg, mstate, htmltools, webshot, flexsurv, muhaz, Metrics, rpivotTable, caret, polycor, ClusterR, flextable, ggstatsplot, ggside, daff, explore, sjPlot, compareGroups, job, missForest, showtext, ggpattern, distill, showtext, googleVis, tidylog, magick, dlookr, easystats, tidylog, install=F)
try(webshot::install_phantomjs())

if(!require(bpmn)){try(devtools::install_github("bergant/bpmn",upgrade =F))}

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#_#_#_#_#_#__#_#_#_#_#_#_

Base_fiscalia_v8_join_Senda<-
Base_fiscalia_v8 %>% 
  #arrange the rut from the first date of comission of a crime, but we are not detecting if he/she is the victim or not
  dplyr::arrange(rut_enc_saf, fec_comision_simple) %>% 
  dplyr::left_join(subset(CONS_C1_df_dup_SEP_2020_22, subset= dup==1),by=c("rut_enc_saf"="hash_key")) %>%
  data.table::data.table() 























Base_fiscalia_v7



tbone_idsujeto_victima_0<-
  CreateTableOne(vars= setdiff(c(myVars,"cut_com_del2","cut_fec_nac2"), c("gls_comuna", "gls_sitiosuceso",
                                                              "tipo_sujeto_vic", "gls_materia", 
                                                              "encontrado_como_victima",  "reg")), 
     data=  dplyr::mutate(Base_fiscalia_v7, idsujeto_victima_0=ifelse(idsujeto_victima==0,1,0),
                          cut_fec_nac2=cut2(imp_birth_date, cuts=as.Date(attr(binning(as.numeric(imp_birth_date)),"breaks"))),
                          cut_com_del2=cut2(fec_comision_simple, cuts=as.Date(attr(binning(as.numeric(fec_comision_simple)),"breaks")))) %>% 
       data.table(), factorVars = setdiff(catVars, c("gls_comuna", "gls_sitiosuceso", "reg")), smd=T, strata=c("idsujeto_victima_0"), addOverall = T, includeNA=T,test=T)

as.data.frame.TableOne(tbone_idsujeto_victima_0, smd=T)%>% 
  dplyr::mutate(char2=characteristic) %>% 
  tidyr::fill(char2) %>% 
  dplyr::select(char2,everything()) %>% 
  dplyr::mutate(level=ifelse(is.na(level),"[Missing]",level)) %>% 
  dplyr::mutate(char2=dplyr::case_when(characteristic=="NA"~NA_character_,T~as.character(characteristic))) %>% 
  format_cells(1, 1:length(names(.)), "bold") %>%
  dplyr::select(-1) %>% 
  kable(size=10, format="markdown",caption= "Summary descriptives, missing values in ID of the victim (1)")