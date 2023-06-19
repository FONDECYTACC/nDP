# Load data & packages ----------------------------------------------------------------

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


# Modify databases ----------------------------------------------------------------

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
Base_fiscalia_v13$dup_filt<- ifelse(Base_fiscalia_v13$duplicates_filtered>1,">1 treatment","only one treatment")
Base_fiscalia_v13$dup_filt_num<- ifelse(Base_fiscalia_v13$duplicates_filtered>1,1,0)
Base_fiscalia_v13$offense_after_adm<- ifelse(!is.na(Base_fiscalia_v13$offender_d),"offender after adm","no offense after admission")
Base_fiscalia_v13$off_aft_adm<- ifelse(!is.na(Base_fiscalia_v13$offender_d),1,0)
Base_fiscalia_v13$mot_egres_mod_imp_rec2<- dplyr::case_when(Base_fiscalia_v13$motivodeegreso_mod_imp_rec=="Treatment completion"~"Tr.Completion",
                                                            grepl("non",Base_fiscalia_v13$motivodeegreso_mod_imp_rec)~"Non-completion",
                                                            T~"Censored")
Base_fiscalia_v13$mot_egres_mod_imp_rec_num<- dplyr::case_when(Base_fiscalia_v13$motivodeegreso_mod_imp_rec=="Treatment completion"~1,
                                                            grepl("non",Base_fiscalia_v13$motivodeegreso_mod_imp_rec)~0,
                                                            T~0)
Base_fiscalia_v13$tr_mod <- ifelse(Base_fiscalia_v13$tr_modality=="Residential",1,0)
Base_fiscalia_v13$mot_egres_mod_imp_rec2<- dplyr::case_when(Base_fiscalia_v13$motivodeegreso_mod_imp_rec=="Treatment completion"~"Tr.Completion",
                                                            grepl("non",Base_fiscalia_v13$motivodeegreso_mod_imp_rec)~"Non-completion",
                                                            T~"Censored")

#INSTRUCCIONES ACC:
#Res yy Amb
# Alta terapéutica vs. no
# Readmisión vs. no

# Analyses ----------------------------------------------------------------

require(glca)

mydata_preds3<- Base_fiscalia_v13 %>% data.table::data.table()

Base_fiscalia_v13 %>% 
  janitor::tabyl(offense_after_adm,mot_egres_mod_imp_rec2,dup_filt)


invisible("glca format")
f_preds2<- item(dup_filt_num, mot_egres_mod_imp_rec_num, tr_mod) ~ 1

seed<-2125
old <- Sys.time()

lca202 <- glca(f_preds2, data = mydata_preds3, nclass = 2, seed = seed, verbose = FALSE, n.init = 5e1, decreasing=T, maxiter = 1e4,testiter = 500)
#43 minutes each more or less
lca203 <- glca(f_preds2, data = mydata_preds3, nclass = 3, seed = seed, verbose = FALSE, n.init = 5e1, decreasing=T, maxiter = 1e4,testiter = 500)
lca204 <- glca(f_preds2, data = mydata_preds3, nclass = 4, seed = seed, verbose = FALSE, n.init = 5e1, decreasing=T, maxiter = 1e4,testiter = 500)
lca205 <- glca(f_preds2, data = mydata_preds3, nclass = 5, seed = seed, verbose = FALSE, n.init = 5e1, decreasing=T, maxiter = 1e4,testiter = 500)
lca206 <- glca(f_preds2, data = mydata_preds3, nclass = 6, seed = seed, verbose = FALSE, n.init = 5e1, decreasing=T, maxiter = 1e4,testiter = 500)
lca207 <- glca(f_preds2, data = mydata_preds3, nclass = 7, seed = seed, verbose = FALSE, n.init = 5e1, decreasing=T, maxiter = 1e4,testiter = 500)
lca208 <- glca(f_preds2, data = mydata_preds3, nclass = 8, seed = seed, verbose = FALSE, n.init = 5e1, decreasing=T, maxiter = 1e4,testiter = 500)

gof2<-
  gofglca(lca202, lca203, lca204, lca205, lca206, lca207, lca208, test = "chisq")

new_med<-(Sys.time())
paste0("The model took ",round(new_med-old,2)," until every LCA was computed")


save.image("__acc_ser_23_2.RData")


rho_glca<- 
  do.call("bind_rows",lca202$param$rho$ALL) %>% 
  t() %>% 
  round(2) %>% 
  data.table::data.table(keep.rownames = T) %>% 
  magrittr::set_colnames(c("variables", paste0("Class",1:length(lca202$param$gamma)))) %>% 
  tidyr::separate(variables, into=c("var", "prob"), sep=".Y =")

lcmodel_glca <- reshape2::melt(rho_glca, level=2) %>% dplyr::rename("class"="variable") %>%
    dplyr::mutate(prob=readr::parse_integer(prob)) %>% 
    dplyr::mutate(label_pre=dplyr::case_when(var=="dup_filt_num" & prob=="1"~"only one treatment",
    var=="dup_filt_num" & prob=="2"~">1 treatment", 
    var=="mot_egres_mod_imp_rec_num" & prob=="1"~"Censored & Non-completion", 
    var=="mot_egres_mod_imp_rec_num" & prob=="2"~"Tr.completion",
    var=="tr_mod" & prob=="1"~"Ambulatory",
    var=="tr_mod" & prob=="2"~"Residential"))


lcmodel_glca$text_label<-paste0("Category:",lcmodel_glca$label_pre,"<br>%: ",scales::percent(lcmodel_glca$value))

zp3 <- ggplot(lcmodel_glca,aes(x = var, y = value, fill = factor(prob), label=text_label))
zp3 <- zp3 + geom_bar(stat = "identity", position = "stack")
zp3 <- zp3 + facet_grid(class ~ .) 
zp3 <- zp3 + scale_fill_brewer(type="seq", palette="Greys", na.value = "white") +theme_bw()
zp3 <- zp3 + labs(y = "Response probabilities", 
                  x = "",
                  fill ="Response\nCategories")
zp3 <- zp3 + theme( axis.text.y=element_blank(),
                    axis.ticks.y=element_blank(),                    
                    panel.grid.major.y=element_blank())
zp3 <- zp3 + guides(fill = guide_legend(reverse=TRUE))
zp3 <- zp3 + theme(axis.text.x = element_text(angle = 30, hjust = 1))
#print(zp1)

ggplotly(zp3, tooltip = c("text_label"))%>% layout(xaxis= list(showticklabels = T),height=600, width=800)

ggsave("_LCA_dist_glca.png",zp3, dpi= 600)

lcmodel_glca %>% rio::export("var_probs_glca.xlsx")

# #sexo_2= Men
# #escolaridad_rec= 2-Completed high school or less
# #sus_ini_mod_mvv= Alcohol
# #freq_cons_sus_prin= Daily
# #dg_trs_cons_sus_or= Drug dependence
# #sus_principal_mod= Cocaine paste 


round(apply(lca202$posterior$ALL, 2,mean)*100,2)

posterior_glca_07<-
  lca202$posterior$ALL %>% 
  dplyr::mutate_all(~ifelse(.>.7,1,0)) %>% 
  dplyr::mutate(final_07=dplyr::case_when(`Class 1`==1~1,`Class 2`==1~2))
posterior_glca_07 %>% 
  rowwise() %>%
  mutate(count_ones = sum(c_across(starts_with("Class")) == 1)) %>%
  ungroup() %>% 
  janitor::tabyl(final_07,count_ones)


Base_fiscalia_v13$class<-ifelse(posterior_glca_07$final_07==2,1,0)

require(finalfit)

explanatory = c("sex", "escolaridad_rec", "sus_ini_mod_mvv", "dg_trs_cons_sus_or","sus_principal_mod")
dependent = "class"
explanatory2 = c("sex.factor", "escolaridad_rec.factor", "sus_ini_mod_mvv.factor", "dg_trs_cons_sus_or.factor","sus_principal_mod.factor")

Base_fiscalia_v13 %>% 
  dplyr::mutate_at(c(dependent,explanatory), ~factor(.)) %>% 
  rename_with(~paste0(., ".factor"), c(explanatory)) %>% 
summary_factorlist(dependent, explanatory2, 
                   p = TRUE, 
                   #cont = "median", 
                   na_include = TRUE,
                   na_include_dependent = T, #para que la variable dependiente tenga missing
                   column = TRUE, 
                   total_col = TRUE) -> t
#remotes::install_github('ewenharrison/finalfit')

Base_fiscalia_v13 %>% 
  dplyr::mutate_at(c(dependent,explanatory), ~factor(.)) %>%
  dplyr::select(c(dependent, explanatory)) %>% 
  dplyr::mutate_at(explanatory, ~as.character(.)) %>%
  finalfit(dependent, explanatory, metrics = T, na_include = T, confint_type = "default")



lcmodel_glca <- reshape2::melt(rho_glca, level=2) %>% dplyr::rename("class"="variable") %>%
  dplyr::mutate(prob=readr::parse_integer(prob)) %>% 
  dplyr::mutate(label_pre=dplyr::case_when(var=="dup_filt_num" & prob=="1"~"only one treatment",
                                           var=="dup_filt_num" & prob=="2"~">1 treatment", 
                                           var=="mot_egres_mod_imp_rec_num" & prob=="1"~"Censored & Non-completion", 
                                           var=="mot_egres_mod_imp_rec_num" & prob=="2"~"Tr.completion",
                                           var=="tr_mod" & prob=="1"~"Ambulatory",
                                           var=="tr_mod" & prob=="2"~"Residential"))


lcmodel_glca$text_label<-paste0("Category:",lcmodel_glca$label_pre,"<br>%: ",scales::percent(lcmodel_glca$value))

invisible("Hacer un recuento y porcentajes de cada categoría")
#dup_filt_num, mot_egres_mod_imp_rec_num, tr_mod
tabulated_data<-
rbind.data.frame(
Base_fiscalia_v13 %>%
  dplyr::select("dup_filt_num", explanatory) %>%
  na.omit() %>% 
  mutate(across(c("dup_filt_num", explanatory), ~ factor(.))) %>%
  gather(variable, measure, -dup_filt_num) %>% 
  group_split(variable)%>%
  map(~ tabyl(.,dup_filt_num, measure)) %>% 
  melt() %>% 
  dplyr::mutate(var="dup_filt_num") %>% 
  dplyr::rename("category"="dup_filt_num") %>% 
  dplyr::mutate(L1= factor(L1, labels=c("SUD\nComorbidity", "Educational\nAttainment", "Sex", "First\nsubstance used", "Primary Substance\nat Admission"))) %>% 
  dplyr::select(var, L1, category, variable, value) %>% 
  dplyr::arrange(var, L1, category, variable, value) %>% 
  dplyr::group_by(var, L1, category) %>% 
  dplyr::mutate(perc=value/sum(value)),
Base_fiscalia_v13 %>%
  dplyr::select("mot_egres_mod_imp_rec_num", explanatory) %>%
  na.omit() %>% 
  mutate(across(c("mot_egres_mod_imp_rec_num", explanatory), ~ factor(.))) %>%
  gather(variable, measure, -mot_egres_mod_imp_rec_num) %>% 
  group_split(variable)%>%
  map(~ tabyl(.,mot_egres_mod_imp_rec_num, measure)) %>% 
  melt() %>% 
  dplyr::mutate(var="mot_egres_mod_imp_rec_num") %>% 
  dplyr::rename("category"="mot_egres_mod_imp_rec_num") %>% 
  dplyr::mutate(L1= factor(L1, labels=c("SUD\nComorbidity", "Educational\nAttainment", "Sex", "First\nsubstance used", "Primary Substance\nat Admission"))) %>%   dplyr::select(var, L1, category, variable, value) %>% 
  dplyr::arrange(var, L1, category, variable, value) %>% 
  dplyr::group_by(var, L1, category) %>% 
  dplyr::mutate(perc=value/sum(value)),
Base_fiscalia_v13 %>%
  dplyr::select("tr_mod", explanatory) %>%
  na.omit() %>% 
  mutate(across(c("tr_mod", explanatory), ~ factor(.))) %>%
  dplyr::select("tr_mod", explanatory) %>%
  gather(variable, measure, -tr_mod) %>% 
  group_split(variable)%>%
  map(~ tabyl(.,tr_mod, measure)) %>% 
  melt() %>% 
  dplyr::mutate(var="tr_mod") %>% 
  dplyr::rename("category"="tr_mod") %>% 
  dplyr::mutate(L1= factor(L1, labels=c("SUD\nComorbidity", "Educational\nAttainment", "Sex", "First\nsubstance used", "Primary Substance\nat Admission"))) %>%   dplyr::select(var, L1, category, variable, value) %>% 
  dplyr::arrange(var, L1, category, variable, value) %>% 
  dplyr::group_by(var, L1, category) %>% 
  dplyr::mutate(perc=value/sum(value))
)
#dup_filt_num, mot_egres_mod_imp_rec_num, tr_mod
colors_plot<-
c("grey80","grey20","grey80","grey50","grey20","grey80","grey20","grey90","grey70","grey50","grey30","grey10","grey90","grey70","grey50","grey30","grey10")
names_plot<-
  c(levels(tabulated_data$variable),"Alcohol", "Cocaine hydrochloride", "Cocaine paste","Marijuana", "Other")
  # c("Drug dependence", "Hazardous consumption", 
  #   "1-More than high school", "2-Completed high school or less", 
  #   "3-Completed primary school or less", 
  #   "Men", "Women", 
  #   "Alcohol", "Cocaine hydrochloride", "Cocaine paste","Marijuana", "Other",
  #   "Alcohol", "Cocaine hydrochloride", "Cocaine paste","Marijuana", "Other")

dat_zp4 <-
tabulated_data %>% 
  ungroup() %>% 
 dplyr::mutate(var2= dplyr::case_when(var=="dup_filt_num"~"No. of treatments (1. >1 treatment)",
  var=="tr_mod"~"Tr.Setting (1.Residential)",var=="mot_egres_mod_imp_rec_num"~"Tr.Outcome (1. Completion)")) %>% 
  mutate(ya=interaction(as.factor(category), L1)) %>% 
  dplyr::mutate(variable=factor(dplyr::case_when(as.character(variable)=="2-Completed high school or less"~"2-High school or less",
    as.character(variable)=="3-Completed primary school or less"~"3-Primary school or less",
                                          T~as.character(variable))))

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_


zp4 <- 
  ggplot(data=dat_zp4) +
  geom_bar(aes(x = interaction(as.factor(category), L1), y = perc, fill = variable),
           stat = "identity", position = "stack") +
  facet_grid(category~.) +
  scale_fill_manual(aesthetics = "fill",
                    values = colors_plot,
                    name = "Response Categories",
                    labels = names_plot,
                    breaks = names_plot,
                    guide = guide_legend(title.position = "left")) +
  labs(y = "Response probabilities",
       x = "",
       fill = "Group") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "right") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_continuous(breaks = seq(0, 1, by = .2), labels = scales::percent_format(accuracy = 1)) +
  theme_sjplot()

zp4

require(patchwork)

ls_p <- 
  dat_zp4 %>%
  split(., .$L1) %>%
  map(~{ggplot(.x)+
      geom_bar(aes(x = category, y = perc, fill = variable),
               stat = "identity", position = "stack") +
      facet_grid(rows = vars(var2))+
      scale_fill_manual(aesthetics = "fill",
                         values = colors_plot,
                         name = "Response Categories",
                         labels = names_plot,
                         breaks = names_plot,
                         guide = guide_legend(title.position = "left")) +
      labs(y = "Response probabilities",
           x = "",
           fill = "Group") +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.text.x = element_text(angle = 30, hjust = 1),
            legend.position = "right") +
      guides(fill = guide_legend(reverse = TRUE)) +
      scale_y_continuous(breaks = seq(0, 1, by = .2), labels = scales::percent_format(accuracy = 1)) +
      theme_sjplot()+ theme(text = element_text(size = 14, lineheight = 1.2))})

wrap_plots(ls_p[[1]]+ guides(fill = guide_legend(title = "SUD\nComorbidity"))+ labs(x = "SUD\nComorbidity") + 
             theme(strip.text = element_blank()), 
           ls_p[[2]]+ guides(fill = guide_legend(title = "Educational\nAttainment"))+ labs(x = "Educational\nAttainment", y = NULL)+ 
             theme(strip.text = element_blank(), axis.text.y = element_blank()),
           ls_p[[3]]+ guides(fill = guide_legend(title = "Sex"))+ labs(x = "Sex", y = NULL)+ 
             theme(strip.text = element_blank(), axis.text.y = element_blank()),
           ls_p[[4]]+ guides(fill = guide_legend(title = "First subsance\nused"))+ labs(x = "First subsance\nused", y = NULL)+ 
             theme(strip.text = element_blank(), axis.text.y = element_blank()),
           ls_p[[5]]+ guides(fill = guide_legend(title = "Primary\nsubstance at admission"))+ labs(x = "Primary\nsubstance at admission", y = NULL)+
             theme(axis.text.y = element_blank()),
           guides = "collect",ncol =5)
ggsave("acc_ser23_plot.pdf", height=10, width=13)#, height=5, width=6.5, dpi=300)
legend <- cowplot::get_legend(ls_p[[1]])


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
invisible("1.")
mean(table(CONS_C1_2010_19$TABLE))

invisible("2.")
bind_rows(CONS_C1_2010_19,CONS_C1_2019_22) %>%  #dplyr::filter(is.na(motivo_de_egreso)) %>% 
  dplyr::mutate(date_adm= readr::parse_date(fecha_ingreso_a_tratamiento,c("%d/%m/%Y"))) %>% 
  #dplyr::select(TABLE, fecha_ingreso_a_tratamiento, date_adm) %>% View()
  janitor::tabyl(sustancia_principal) %>% melt() %>% dplyr::filter(variable=="n") %>% arrange(value) %>% View()
invisible("3.")  
bind_rows(CONS_C1_2010_19,CONS_C1_2019_22) %>%  #dplyr::filter(is.na(motivo_de_egreso)) %>% 
  dplyr::mutate(date_adm= readr::parse_date(fecha_ingreso_a_tratamiento,c("%d/%m/%Y"))) %>% 
  #dplyr::select(TABLE, fecha_ingreso_a_tratamiento, date_adm) %>% View()
  janitor::tabyl(sustancia_principal,otras_sustancias_no1, otras_sustancias_no2, otras_sustancias_no3) %>% melt() %>% dplyr::filter(variable=="n") %>% arrange(value) %>% View()

bind_rows(CONS_C1_2010_19,CONS_C1_2019_22) %>% 
  dplyr::filter(sustancia_principal!="Alcohol"|is.na(sustancia_principal)) %>% 
  janitor::tabyl(otras_sustancias_no1, otras_sustancias_no2,otras_sustancias_no3) %>% melt() %>% 
  dplyr::select(otras_sustancias_no1, variable, L1, value) %>% 
  dplyr::filter(otras_sustancias_no1=="Alcohol"|variable=="Alcohol"|L1=="Alcohol") %>% dplyr::filter(value>0) %>% summarise(sum=sum(value))




explanatory = c("sex", "escolaridad_rec", "sus_ini_mod_mvv", "dg_trs_cons_sus_or","sus_principal_mod") 
#dup_filt_num tr_mod mot_egres_mod_imp_rec_num

dat_cor_zp4<-
Base_fiscalia_v13 %>%
  dplyr::mutate(factor= factor(interaction(tr_mod, mot_egres_mod_imp_rec_num,dup_filt_num), 
                               labels=c("Ambulatory\nNon-complete\nSingle","Residential\nNon-complete\nSingle",
                                        "Ambulatory\nComplete\nSingle","Residential\nComplete\nSingle",
                                        "Ambulatory\nNon-complete\n>1","Residential\nNon-complete\n>1",
                                        "Ambulatory\nComplete\n>1","Residential\nComplete\n>1"))) %>% 
  #janitor::tabyl(factor)
  dplyr::select("factor", explanatory) %>%
  na.omit() %>% 
  mutate(across(c("factor", explanatory), ~ factor(.))) %>%
  gather(variable, measure, -factor) %>% 
  group_split(variable)%>%
  map(~ tabyl(.,factor, measure)) %>% 
  melt() %>% 
  dplyr::mutate(var="factor") %>% 
  dplyr::rename("category"="factor") %>% 
  dplyr::mutate(L1= factor(L1, labels=c("SUD\nComorbidity", "Educational\nAttainment", "Sex", "First\nsubstance used", "Primary Substance\nat Admission"))) %>% 
  dplyr::select(var, L1, category, variable, value) %>% 
  dplyr::arrange(var, L1, category, variable, value) %>% 
  dplyr::group_by(var, L1, category) %>% 
  dplyr::mutate(perc=value/sum(value))


#p + geom_label_repel(aes(fill=factor(cyl)), colour="white", segment.colour="black")
# Define a function to generate the color sequence
color_gen <- function(low, high, n) {
  palette <- colorRampPalette(c(low, high))
  colors <- palette(n)
  return(colors)
}

require(ggrepel)
zp4_cor_splt<-
dat_cor_zp4 %>%
  dplyr::mutate(lab=paste0(round(perc,2)*100,"%"), half=(perc/10)*4) %>% 
  split(., .$L1) %>%
  map(~{ggplot(.x,aes(x = category, y = perc, fill = variable, label=lab)) +
  geom_bar(stat = "identity", position = "stack") +
  #  geom_text(aes(x = category,label=`lab`, y=.5, fill = variable), size = 5) +
      theme(
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        line = element_blank())+
      geom_label_repel(aes(label=lab),#aes(y=lab, label=lab),
                 #position = position_dodge(width = .5),    # move to center of bars
                 #vjust = 0,    # nudge above top of bar
                 #position = position_dodge(width = .8),
                 #vjust = .1,
                 position = position_stack(vjust = 0.5),
                 size = 8,
                 force_pull = 1,
                 max.time = 6,
                 max.iter = 1e6,
                 #direction = "y",
                 #force=1,
                 seed=2125,
                 colour = "white", fontface = "bold")+
      labs(y = "Response probabilities",
           x = NULL,
           fill = NULL) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.text = element_text(size=12),
            axis.text.x = element_text(angle = 30, hjust = 1),
            legend.position = "right") +
      guides(fill = guide_legend(reverse = F,override.aes = aes(label = "", alpha = 1))) +
      scale_y_continuous(breaks = seq(0, 1, by = .2), labels = scales::percent_format(accuracy = 1)) +
      theme_sjplot()+
     # scale_fill_brewer(palette = color_gen("#21177A60", "#ff7f50", 
    # length(levels(.x$variable))))
     scale_fill_manual(values = color_gen("#21177A60", "#ff7f5060", 
                                                   length(unique(.x$variable))))#, levels(.x$variable)))
  })
#warning("Sacamos a los que son derivación") resuelto

ggsave(plot= zp4_cor_splt[[1]], filename="acc_ser23_plot_sud_com.pdf", height=10, width=13)#, height=5, width=6.5, dpi=300)
ggsave(plot= zp4_cor_splt[[2]], filename="acc_ser23_plot_ed_att.pdf", height=10, width=13)#, height=5, width=6.5, dpi=300)
ggsave(plot= zp4_cor_splt[[3]], filename="acc_ser23_plot_sex.pdf", height=10, width=13)#, height=5, width=6.5, dpi=300)
ggsave(plot= zp4_cor_splt[[4]], filename="acc_ser23_plot_1st_subs_used.pdf", height=10, width=13)#, height=5, width=6.5, dpi=300)
ggsave(plot= zp4_cor_splt[[5]], filename="acc_ser23_plot_prim_subs_adm.pdf", height=10, width=13)#, height=5, width=6.5, dpi=300)

ggsave(plot= zp4_cor_splt[[1]], filename="acc_ser23_plot_sud_com.png", height=10, width=13)#, height=5, width=6.5, dpi=300)
ggsave(plot= zp4_cor_splt[[2]], filename="acc_ser23_plot_ed_att.png", height=10, width=13)#, height=5, width=6.5, dpi=300)
ggsave(plot= zp4_cor_splt[[3]], filename="acc_ser23_plot_sex.png", height=10, width=13)#, height=5, width=6.5, dpi=300)
ggsave(plot= zp4_cor_splt[[4]], filename="acc_ser23_plot_1st_subs_used.png", height=10, width=13)#, height=5, width=6.5, dpi=300)
ggsave(plot= zp4_cor_splt[[5]], filename="acc_ser23_plot_prim_subs_adm.png", height=10, width=13)#, height=5, width=6.5, dpi=300)

df<-
Base_fiscalia_v13%>% 
  dplyr::group_by(hash_key)%>%
  dplyr::mutate(n_hash=n())%>% 
  slice(1) %>% 
  ungroup() %>% 
  dplyr::select(starts_with("dias_treat_imp_sin_na_")) %>%  #mean_cum_dias_trat_sin_na_1
  gather(option,value) %>%
  dplyr::mutate(option=dplyr::case_when(option=="dias_treat_imp_sin_na_1"~"01",
                                        option=="dias_treat_imp_sin_na_2"~"02",
                                        option=="dias_treat_imp_sin_na_3"~"03",
                                        T~"04+")) %>% 
  ggplot(aes(x = factor(option), y=value,group= option)) +
  stat_summary(fun = mean, geom="bar",alpha=.8)+
  stat_summary(fun = median, geom="point")+
  stat_summary(fun.y = median,
               fun.min = function(x) quantile(x,.25), 
               fun.max = function(x) quantile(x,.75), 
               geom = "errorbar", width = 0.5)+
  geom_label(inherit.aes = FALSE, data = . %>% group_by(option) %>% slice(1), 
             aes(label = paste0(count, " Obs."), x = option), y = -0.5)+
  geom_label(inherit.aes = FALSE, data = . %>% group_by(option) %>% slice(1), 
             aes(label = paste0(count, " Obs."), x = option), y = -0.5)+
  theme_bw()+
  labs(x="Number of Treatment of Each User", y="Days in Treatment",
       caption=paste0("Note. Bars=Means, Dots= Medians, Error bars= Percentiles 25 and 75"))
       
zp5_cor<- 
Base_fiscalia_v13%>% 
  dplyr::group_by(hash_key)%>%
  dplyr::mutate(n_hash=n())%>% 
  slice(1) %>% 
  ungroup() %>% 
dplyr::mutate(factor= factor(interaction(tr_mod, mot_egres_mod_imp_rec_num), 
                             labels=c("Ambulatory\nNon-complete","Residential\nNon-complete",
                                      "Ambulatory\nComplete","Residential\nComplete"))) %>% 
  split(., .$factor) %>%
  map(~{
  dplyr::select(.x, starts_with("dias_treat_imp_sin_na_")) %>%  #mean_cum_dias_trat_sin_na_1
  gather(option,value) %>%
dplyr::mutate(option=dplyr::case_when(option=="dias_treat_imp_sin_na_1"~"01",
                                      option=="dias_treat_imp_sin_na_2"~"02",
                                      option=="dias_treat_imp_sin_na_3"~"03",
                                      T~"04+")) %>% 
  dplyr::mutate(option=factor(option)) %>% 
  dplyr::group_by(option) %>% 
  dplyr::mutate(count = length(na.omit(value))) %>% 
  dplyr::ungroup() %>% 
  ggplot(aes(x = option, y=value,group= option)) +
  stat_summary(fun = mean, geom="bar",alpha=.8, na.rm = T)+
  stat_summary(fun = median, geom="point", na.rm = T)+
  stat_summary(fun = median,
               fun.min = function(x) quantile(x,.25, na.rm = T), 
               fun.max = function(x) quantile(x,.75, na.rm = T), 
               geom = "errorbar", width = 0.5)+
  geom_label(inherit.aes = FALSE, data = . %>% group_by(option) %>% slice(1), 
             aes(label = paste0(count, " Obs."), x = option), y = -0.5)+
  geom_label(inherit.aes = FALSE, data = . %>% group_by(option) %>% slice(1), 
             aes(label = paste0(count, " Obs."), x = option), y = -0.5)+
  theme_bw()+
  theme(plot.caption = element_text(hjust = 0, face= "italic",size=9))+
  #geom_bar(stat = "identity")+
  #geom_errorbar() +
  labs(x="Number of Treatment of Each User", y="Days in Treatment",caption=paste0("Note. Bars=Means, Dots= Medians, Error bars= Percentiles 25 and 75"))
})
  #janitor::tabyl(factor)
#c("Ambulatory\nNon-complete","Residential\nNon-complete",
#  "Ambulatory\nComplete","Residential\nComplete"))) %>% 
  
ggsave(plot= zp5_cor[[1]], filename="acc_ser23_plot_amb_noncomp.pdf", height=10, width=13)#, height=5, width=6.5, dpi=300)
ggsave(plot= zp5_cor[[2]], filename="acc_ser23_plot_res_noncomp.pdf", height=10, width=13)#, height=5, width=6.5, dpi=300)
ggsave(plot= zp5_cor[[3]], filename="acc_ser23_plot_amb_comp.pdf", height=10, width=13)#, height=5, width=6.5, dpi=300)
ggsave(plot= zp5_cor[[4]], filename="acc_ser23_plot_res_comp.pdf", height=10, width=13)#, height=5, width=6.5, dpi=300)

