# Load data & packages ----------------------------------------------------------------

rm(list = ls()) 
unlink("*_cache", recursive=T)
#fuentes: 
#https://rpubs.com/georgy_makarov/897844
load("1_ndp_2023_07_05_19_00_00.RData")

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

pacman::p_load(powerSurvEpi, APCtools, ggpattern, withr, boot, matrixStats, knitr, tidyr, stringi,stringr, ggplot2, Hmisc, kableExtra, plotly, janitor, rbokeh, zoo, broom, sqldf, devtools, codebook, data.table, panelr, RColorBrewer, lsmeans, finalfit, ggiraph, sf, treemapify, dplyr, tidyverse, epiR, survminer, ggfortify, survMisc, foreign, reshape2, stargazer, tableone, MatchIt, cobalt, eha, igraph, Amelia, DiagrammeR, DiagrammeRsvg, rsvg, mstate, htmltools, webshot, flexsurv, muhaz, Metrics, rpivotTable, caret, polycor, ClusterR, flextable, ggstatsplot, ggside, daff, explore, sjPlot, compareGroups, job, missForest, showtext, ggpattern, distill, showtext, googleVis, tidylog, magick, dlookr, easystats, tidylog, sqldf,  adjustedCurves, ggpmisc, rms, install=T)

#Error in if (options$noisey == TRUE) message(paste("\n", options$engine, : argument is of length zero


if(!require(survcomp)){BiocManager::install("survcomp")}



# Modify databases ----------------------------------------------------------------


# tesis ags ---------------------------------------------------------------

paste0("Number of observations: ",
bind_rows(CONS_C1_2010_19,CONS_C1_2019_22) %>%nrow() %>% format(big.mark=","))
paste0("Number of users: ",
bind_rows(CONS_C1_2010_19,CONS_C1_2019_22) %>% dplyr::distinct(HASH_KEY)%>% nrow() %>% format(big.mark=","))

bind_rows(CONS_C1_2010_19,CONS_C1_2019_22) %>%
  dplyr::group_by(HASH_KEY) %>% 
  dplyr::mutate(n=n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(n>1) %>% 
  dplyr::distinct(HASH_KEY)

invisible("With referrals")
bind_rows(CONS_C1_2010_19,CONS_C1_2019_22) %>% 
  #dplyr::filter(motivo_de_egreso!="Derivación") %>% 
  dplyr::summarize(p0025_dias= quantile(dias_en_tratamiento, .0025,na.rm=T),
                   length_p0025= length(dias_en_tratamiento[dias_en_tratamiento<= quantile(dias_en_tratamiento, .0025,na.rm=T)]),
                   p005_dias= quantile(dias_en_tratamiento, .005,na.rm=T),
                   length_p005= length(dias_en_tratamiento[dias_en_tratamiento<= quantile(dias_en_tratamiento, .005,na.rm=T)]),
                   p01_dias= quantile(dias_en_tratamiento, .01,na.rm=T),
                   length_p01= length(dias_en_tratamiento[dias_en_tratamiento<= quantile(dias_en_tratamiento, .01,na.rm=T)]),
                   p025_dias= quantile(dias_en_tratamiento, .025,na.rm=T),
                   length_p025= length(dias_en_tratamiento[dias_en_tratamiento<= quantile(dias_en_tratamiento, .025,na.rm=T)])
  )
invisible("Without referrals")
bind_rows(CONS_C1_2010_19,CONS_C1_2019_22) %>% 
  dplyr::filter(motivo_de_egreso!="Derivación") %>% 
  dplyr::summarize(p0025_dias= quantile(dias_en_tratamiento, .0025,na.rm=T),
                   length_p0025= length(dias_en_tratamiento[dias_en_tratamiento<= quantile(dias_en_tratamiento, .0025,na.rm=T)]),
    p005_dias= quantile(dias_en_tratamiento, .005,na.rm=T),
                   length_p005= length(dias_en_tratamiento[dias_en_tratamiento<= quantile(dias_en_tratamiento, .005,na.rm=T)]),
                   p01_dias= quantile(dias_en_tratamiento, .01,na.rm=T),
                   length_p01= length(dias_en_tratamiento[dias_en_tratamiento<= quantile(dias_en_tratamiento, .01,na.rm=T)]),
                   p025_dias= quantile(dias_en_tratamiento, .025,na.rm=T),
                   length_p025= length(dias_en_tratamiento[dias_en_tratamiento<= quantile(dias_en_tratamiento, .025,na.rm=T)])
                   )
paste0("Total of treatment episodes w/o referrals: ",
CONS_C1_2010_19 %>%
  bind_rows(CONS_C1_2019_22) %>%
  filter(motivo_de_egreso != "Derivación", dias_en_tratamiento >= 0) %>%
  nrow() %>% as.numeric() %>% format(big.mark=",")
)
paste0("Total of treatment episodes w referrals: ",
       CONS_C1_2010_19 %>%
         bind_rows(CONS_C1_2019_22) %>%
         filter(motivo_de_egreso == "Derivación", dias_en_tratamiento >= 0) %>%
         nrow() %>% as.numeric() %>% format(big.mark=",")
)

CONS_C1_2010_19 %>%
  bind_rows(CONS_C1_2019_22) %>%
  filter(motivo_de_egreso != "Derivación", dias_en_tratamiento >= 0) %>%
  {
    ggplot(data = .) +
      geom_histogram(aes(x = dias_en_tratamiento), bins = 80) +
      theme_sjplot() +
      labs(x = "Days in treatment", y = "Count") +
      geom_vline(xintercept = quantile(.$dias_en_tratamiento, .0025, na.rm = TRUE), color="red")+
      geom_vline(xintercept = quantile(.$dias_en_tratamiento, .01, na.rm = TRUE), color="purple")+
      geom_vline(xintercept = quantile(.$dias_en_tratamiento, .025, na.rm = TRUE), color="blue")+
      xlim(0,2000)+
      labs(caption=paste0("Note. Red, Violet and Blue lines depicts percentiles 0.25 ",
                         # format(nrow(quantile(.$dias_en_tratamiento, .0025, na.rm = TRUE)),big.mark=","),
                          ", 1", 
                         #format(nrow(quantile(.$dias_en_tratamiento, .01, na.rm = TRUE)),big.mark=","),
                          " & 2.5, respectively"))
                         #format(nrow(quantile(.$dias_en_tratamiento, .025, na.rm = TRUE)),big.mark=",")))
    }


invisible( "Sólo los primeros episodios de tratamiento por cada sujeto, para ver si hay superposición con los casos puros que yo quiero seleccionar")
bind_rows(CONS_C1_2010_19,CONS_C1_2019_22) %>%  #dplyr::filter(is.na(motivo_de_egreso)) %>% 
  dplyr::mutate(date_adm= readr::parse_date(fecha_ingreso_a_tratamiento,c("%d/%m/%Y"))) %>% 
  dplyr::arrange(HASH_KEY, date_adm) %>% 
  dplyr::group_by(HASH_KEY) %>% 
  dplyr::slice(1) %>% # nrow()  #106,534
  dplyr::ungroup() %>% 
  dplyr::filter(motivo_de_egreso!="Derivación", dias_en_tratamiento>=0) %>% 
  ggplot()+
  geom_histogram(aes(x=dias_en_tratamiento), bins=80)+ #149 casos en 0 // 2,076 casos en 21 días o menos
  sjPlot::theme_sjplot()+
  labs(x="Days in treatment", y= "Count")


# Sample size -------------------------------------------------------------

# http://powerandsamplesize.com/Calculators/Test-Time-To-Event-Data/Cox-PH-Equivalence
# https://ph-ivshiny.iowa.uiowa.edu/rpterson/MSDshiny/
require(powerSurvEpi)


#ssizeEpiCont.default Sample Size Calculation for Cox Proportional Hazards Regression with Nonbinary Covariates for Epidemiological Studies
#Sample size calculation for Cox proportional hazards regression with nonbinary covariates for Epidemiological Studies.

# power numeric. postulated power.
# theta numeric. postulated hazard ratio.
# sigma2 numeric. variance of the covariate of interest.
# psi numeric. proportion of subjects died of the disease of interest.
# rho2 numeric. square of the multiple correlation coefficient between the covariate of
# interest and other covariates.
# alpha numeric. type I error rate.


CONS_C1_2010_19 %>%
  bind_rows(CONS_C1_2019_22) %>% 
  summarise(mean=mean(dias_en_tratamiento, na.rm=T), (sd(dias_en_tratamiento, na.rm=T)^2))

ssizeEpiCont.default(power= 0.8,
                     theta= 0.99,
                     sigma2= sd(cpdata2$time_in_trt)^2,
                     psi= 0.00743,
                     rho2= 0.8, #(0.7)
                     alpha = 0.1)

#stpower cox -.01005034, power(.8) sd(176.5311) r2(0.9) failprob(0.00743)
#stpower cox -.01005034, power(.95) sd(176.5311) r2(0.8) failprob(0.00743)
ssizeEpiCont.default(power= 0.90,
                     theta= .99,
                     sigma2= sd(cpdata2$time_in_trt)^2,
                     psi= 0.00743,
                     rho2= 0.8, #(0.7)
                     alpha = 0.01)



#An item's SMC value, its squared multiple correlation, indicates the proportion of the item's 
#variance which may be linked to, or predicted from, the other items in the subtest. 
#As mentioned in the previous topic, the SMC is sometimes used as an estimate of the amount of 
#variance any single item has in common with the other items.

#SMC was 0.67. We may interpret this as meaning that 67% of Q2's 
#variance can be explained by the other items in the subtest. 

# Hsieh and Lavori (2000) assumed one-sided test,
# while this implementation assumed two-sided test.
# Hence alpha=0.1 here (two-sided test) will correspon

#PLONSKY, L. and GHANBAR, H. (2018), Multiple Regression in L2 Research: A Methodological Synthesis and Guide to 
#Interpreting R2 Values  . The Modern Language Journal, 102: 713-731. https://doi.org/10.1111/modl.12509

#Researchers can detect multicollinearity and singularity by an array of statistics such as 
#squared multiple correlation (SMC), variance inflation rate (VIF), tolerance value, and 
#condition index, all provided by most statistical packages.


# People who abandoned ---------------------------------------------------------------------
#2023-08-02
load("an_grant_23_24.RData")
mat_dias_tto_por_mot_egr<-
CONS_C1_2010_19 %>% 
  dplyr::group_by(motivo_de_egreso) %>% 
  count(dias_en_tratamiento) %>% 
  dplyr::filter(dias_en_tratamiento>=0) %>%
  ggplot2::ggplot(aes(x = dias_en_tratamiento, y = n, color=factor(motivo_de_egreso), label = paste0("n=",n))) +
  geom_line() +
  theme_bw()+
  xlim(0,300)+
  labs(x="Days in treatment", y="Count of patients treated", color="Cause of\ndischarge")

ggsave("days_in_treatment_cause_of_disch.png", mat_dias_tto_por_mot_egr, dpi=500)

round(prop.table(table(CONS_C1_2010_19$evaluacion_del_proceso_terapeutico,
                       CONS_C1_2010_19$motivo_de_egreso),2),2)

CONS_C1_df_dup_SEP_2020_22_d %>%
  dplyr::group_by(motivodeegreso_mod_imp_1) %>% 
  count(dias_treat_imp_sin_na_1) %>% 
  ggplot2::ggplot(aes(x = dias_treat_imp_sin_na_1, y = n, color=factor(motivodeegreso_mod_imp_1), label = paste0("n=",n))) +
  geom_line() +
  theme_bw()+
  xlim(0,300)+
  labs(x="Days in treatment", y="Count of patients treated", color="Cause of\ndischarge")


round(prop.table(table(CONS_C1_df_dup_SEP_2020_22_d$evaluacindelprocesoteraputico,
                       CONS_C1_df_dup_SEP_2020_22_d$motivodeegreso_mod_imp_1),2),2)

# People who abandoned ---------------------------------------------------------------------
#2023-08-02
load("an_grant_23_24.RData", an_grant_23_24 <- new.env() )

require(tidyverse)
require(tidySEM)

CONS_C1_df_dup_SEP_2020<-
an_grant_23_24$CONS_C1_df_dup_SEP_2020

CONS_C1_df_dup_SEP_2020_22_d<-
  an_grant_23_24$CONS_C1_df_dup_SEP_2020_22_d

rm(an_grant_23_24)

#evaluacindelprocesoteraputico
mydata_preds1<- 
  subset(CONS_C1_df_dup_SEP_2020, dup==1) %>% 
  dplyr::select(eva_consumo,
                eva_fam,
                eva_relinterp,
                eva_ocupacion,
                eva_sm,
                eva_fisica,
                eva_transgnorma) %>%  
  data.table::data.table()

mydata_preds2 <- mydata_preds1%>% 
  dplyr::mutate(across(c("eva_consumo", "eva_fam", "eva_relinterp",
                         "eva_ocupacion","eva_sm", "eva_fisica", "eva_transgnorma"), ~ 
                         dplyr::case_when(is.na(.)~ 1, T~ parse_number(as.character(.))+1)))%>% 
  dplyr::mutate(across(c("eva_consumo", "eva_fam", "eva_relinterp",
                         "eva_ocupacion","eva_sm", "eva_fisica", "eva_transgnorma"), ~ 
                         as.ordered(.)))
set.seed(123)
res <- tidySEM::mx_lca(data = mydata_preds2, classes = 1:6)
invisible("Se demora como 1 día en hacer 6, y son todas no concluyentes")
# Name Classes        LL  Parameters       AIC       BIC     saBIC   Entropy  prob_min  prob_max      n_min     n_max  np_ratio
# 1    1       1 -732948.0 21 1465937.9 1466134.3 1466067.5 1.0000000 1.0000000 1.0000000 1.00000000 1.0000000 4049.9048
# 2    2       2 -542079.3 43 1084244.6 1084646.7 1084510.0 0.9470386 0.9836945 0.9866397 0.48460869 0.5153913 1977.8605
# 3    3       3 -459786.9 65  919703.8  920311.6  920105.0 0.9474716 0.9586688 0.9848738 0.23224532 0.3911086 1308.4308
# 4    4       4 -384087.8 87  768349.7  769163.2  768886.7 0.9530913 0.9682008 1.0000000 0.06164754 0.3766461  977.5632
# 5    5       5 -375106.1 109  750430.2  751449.5  751103.1 0.9111643 0.8540046 1.0000000 0.06164754 0.3118709  780.2569
# 6    6       6 -369728.0 131  739718.0  740943.0  740526.7 0.9017279 0.8561914 1.0000000 0.06164754 0.3171268  649.2214
# np_local
# 1 4049.9048
# 2 1962.6190
# 3  940.5714
# 4  249.6667
# 5  249.6667
# 6  249.6667



# DAG ---------------------------------------------------------------------

if(!require(ggdag)){install.packages("ggdag")}
if(!require(dagitty)){install.packages("dagitty")}

#out.height=450, 
dag34 <- dagitty('dag {
"Contact w/ justice-system" [outcome,pos="1.047,1.167"]
"Improved occupational status" [outcome,pos="1.018,0.793"]
"Length of treatment" [exposure,pos="-0.221,1.666"]
"Previous events [until admission]" [adjusted,pos="-1.121,1.220"]
"Readmission to treatment" [outcome,pos="1.018,0.432"]
"Sociodemographic [at admission]" [adjusted,pos="-1.404,0.015"]
"Health status [at admission]" [adjusted,pos="-1.473,0.541"]
"Related to the geographic location [at admission]" [adjusted,pos="-1.358,0.856"]
"Substance use patterns [at admission]" [adjusted,pos="-1.473,0.271"]
Death [outcome,pos="1.380,1.666"]
"Length of treatment" -> "Contact w/ justice-system"
"Length of treatment" -> "Improved occupational status"
"Length of treatment" -> "Readmission to treatment"
"Length of treatment" -> Death
"Previous events [until admission]" -> "Contact w/ justice-system"
"Previous events [until admission]" -> "Improved occupational status"
"Previous events [until admission]" -> "Length of treatment"
"Previous events [until admission]" -> "Readmission to treatment"
"Previous events [until admission]" -> Death
"Sociodemographic [at admission]" -> "Length of treatment"
"Sociodemographic [at admission]" -> "Readmission to treatment"
"Sociodemographic [at admission]" -> Death
"Health status [at admission]" -> "Contact w/ justice-system"
"Health status [at admission]" -> "Improved occupational status"
"Health status [at admission]" -> "Length of treatment"
"Health status [at admission]" -> "Readmission to treatment"
"Health status [at admission]" -> Death
"Related to the geographic location [at admission]" -> "Contact w/ justice-system"
"Related to the geographic location [at admission]" -> "Improved occupational status"
"Related to the geographic location [at admission]" -> "Length of treatment"
"Related to the geographic location [at admission]" -> "Readmission to treatment"
"Related to the geographic location [at admission]" -> Death
"Substance use patterns [at admission]" -> "Improved occupational status"
"Substance use patterns [at admission]" -> "Length of treatment"
}
')

#https://go-bayes.github.io/psych-447/posts/11_1/
#https://rpubs.com/leahmoubadder/726250

tidy_dag34 <- tidy_dagitty(dag34) %>% 
  dplyr::mutate(label=dplyr::case_when(grepl("A0",as.character(name))~"A0",
                                       T~as.character(name))) %>% 
  dplyr::mutate(label2=dplyr::case_when(grepl("at admission",name)~"adj",grepl("until admission",name)~"adj",grepl("LM",name)~"white",
                                        T~"black")) %>% 
  #Allows us to modify transparency of arrows as a function of whether or not a variable is adjusted:
  dplyr::mutate(adjusted=factor(dplyr::case_when(grepl("at admission",name)~"adjusted",grepl("until admission",name)~"adjusted",T~"unadjusted")), arrow =
                  ifelse(adjusted == "adjusted", 0.15, 0.85)) %>% 
  dplyr::mutate(label= gsub("\\[", "\\\n\\[",label))

edge_function <- ggdag:::edge_type_switch("link_arc")

dag34_plot<-
  ggdag:::if_not_tidy_daggity(tidy_dag34) %>% ggdag:::node_status() %>% 
  ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, 
                               yend = yend, color = status, shape=factor(adjusted)))+ 
  #edge_function()+
  scale_adjusted()+ ggdag:::breaks(c("exposure", "outcome","latent"))+
  theme_dag()+
  # geom_dag_edges(aes(#Adjusts transparency of arrows:
  #   edge_alpha = arrow), edge_width = 0.5) +
  geom_dag_edges()+
  #geom_dag_edges_arc(curvature = c(rep(0,240)))+ #14 y 16 de 24
  ggdag:::geom_dag_point(size = 16)+
  ggdag:::geom_dag_label_repel(ggplot2::aes_string(label = "label", 
                                                   fill = "status"), size = 4, col = "white", 
                               show.legend = FALSE)+
  
  scale_shape_manual(values = c(15, 16), name="Ajustado", labels=c("Sí", "No"))+ 
  scale_fill_manual(values = c("gray70", "gray35","gray30"), name="Status",na.value="black", labels=c("Exposure", "Outcome"), limits = c('exposure', 'outcome'))+  
  scale_color_manual(values = c("gray75", "gray35","gray30"), name="Status",na.value="black", labels=c("Exposure", "Outcome"), limits = c('exposure', 'outcome'))+#E6E6E6
  guides(linetype="none", edge_alpha="none", shape="none")+
  guides(color=guide_legend(override.aes = list(arrow = NULL)))+#,guide_colourbar(order = 1)
  theme(plot.caption = element_text(hjust = 0))+
  theme(legend.position = "bottom", aspect.ratio=4/10, text = element_text(size = 15))+
  labs(caption="Source. Prepared by the authors")

dag34_plot

ggsave("dag.pdf", width=10, height=5, dpi=300)
