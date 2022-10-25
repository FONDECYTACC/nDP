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


# *Actually, I realize that imprisonment variable is exactly the same that marca_pena_44*
#   
#   ***Step 3:  Generate Outcome variable option 2: violent crimes (conservative measure, only considering homicide and violent thefts) (N=32450, valid cases)****
#   ***I am following the chilean prosecutor office to operationalize "violent crime", but I am not considering abortion nor Quasi-delicts 
#committed by health professionals. Also I am not sure if I should add OFFENCES OF TORTURE, ILL-TREATMENT, GENOCIDE, ETC, because the prosecutor 
#Office does not consider it, but also because many crimes on this group are "the intent of"(intent of genocide, give the order of torture, etc.)*** 
#   
#   **Sobre la familia "malos tratos, genocidio y tortura" se considera por la fiscalia 4 delitos: 1. crimenes de lesa humanidad,2. obtencion de 
#declaraciones forzadas, 3. tormentos y apremios cometidos por empleados publicosñ 4. tormentos y apremios cometidos por paticulares.// Considerar 
#que en el codigo penal: "ART. 150 A.    El empleado público que, abusando de su cargo o sus funciones, aplicare, ordenare o consintiere en que 
#se aplique tortura, será penado con presidio mayor en su grado mínimo. Igual sanción se impondrá al empleado público que, conociendo de la 
#ocurrencia de estas conductas, no impidiere o no hiciere cesar la aplicación de tortura, teniendo la facultad o autoridad necesaria para 
#ello o estando en posición para hacerlo.La misma pena se aplicará al particular que, en el ejercicio de funciones públicas, o a instigación de un 
#empleado público, o con el consentimiento o aquiescencia de éste, ejecutare los actos a que se refiere este artículo." ***
#   
# gen violent=.
# replace violent=1 if familia_delito=="HOMICIDIOS"
# replace violent=1 if familia_delito=="ROBOS"
# replace violent=1 if familia_delito=="LESIONES"
# replace violent=1 if gls_materia=="SECUESTRO CON HOMICIDIO, VIOLACIÓN O LE"
# replace violent=1 if gls_materia=="SECUESTRO CON LESIONES"
# replace violent=1 if gls_materia=="SECUESTRO CON VIOLACIÓN"
# replace violent=1 if gls_materia=="SUSTRACCIÓN DE MENORES. ART. 142"
# replace  violent=0 if violent==.
# label values violent dico
# table violent if validcases==1



paste0(paste0("Número de casos: ",format(nrow(Base_fiscalia_v8),big.mark=",")))

Base_fiscalia_v8 %>% 
  dplyr::mutate(familia_delito_rec=
  dplyr::case_when(familia_delito_rec= 
                   #familia_delito=="HOMICIDIOS"~toupper("genocide torture and ill-treatment"),
                   #familia_delito=="ROBOS"~toupper("genocide torture and ill-treatment"),
                   #familia_delito=="LESIONES"~toupper("genocide torture and ill-treatment"),
                   gls_materia=="SECUESTRO CON HOMICIDIO, VIOLACIÓN O LE"~toupper("genocide torture and ill-treatment"),
                   gls_materia=="SECUESTRO CON LESIONES"~toupper("genocide torture and ill-treatment"),
                   gls_materia=="SECUESTRO CON VIOLACIÓN"~toupper("genocide torture and ill-treatment"),
                   gls_materia=="SUSTRACCIÓN DE MENORES. ART. 142"~toupper("genocide torture and ill-treatment"),
                   T~familia_delito_rec)) %>% 
  janitor::tabyl(gls_materia,familia_delito_rec) %>% 
  dplyr::mutate_if(is.numeric,~ifelse(.>0,"X","")) %>%
  data.frame() %>%  copiar_nombres()

Base_fiscalia_v8 %>% 
dplyr::filter(grepl("CONDENATORIA",agrupa_terminos)) %>% 
  dplyr::filter(grepl("SI",encontrado_como_imputado))

rut_ruc_distinct_offenses<-
Base_fiscalia_v8 %>% 
  dplyr::filter(grepl("CONDENATORIA",agrupa_terminos)) %>% 
  dplyr::filter(grepl("SI",encontrado_como_imputado)) %>% 
  dplyr::group_by(rut_enc_saf, ruc) %>%
  dplyr::summarise(n_distinct_offenses=n_distinct(gls_materia)) %>% 
  dplyr::filter(n_distinct_offenses>1)


Base_fiscalia_v8 %>% 
  dplyr::left_join(rut_ruc_distinct_offenses, by= c("rut_enc_saf","ruc")) %>% 
  dplyr::filter(!is.na(n_distinct_offenses)) %>%
  dplyr::select(rut_enc_saf, ruc, gls_materia, region_delito_rec, familia_delito_rec, 
                fec_comision_simple, clasificacion_pena_47, tramos_condena_48,
                marca_suspension_43, marca_pena_44, marca_multa_45,
                medida_alternativa_46) %>% copiar_nombres()

Base_fiscalia_v8 %>% 
  dplyr::left_join(rut_ruc_distinct_offenses, by= c("rut_enc_saf","ruc")) %>% 
  dplyr::filter(!is.na(n_distinct_offenses)) %>%
  dplyr::group_by(rut_enc_saf, ruc) %>%
  dplyr::mutate(dist_tramo_cond= n_distinct(tramos_condena_48)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(rut_enc_saf, ruc, gls_materia, region_delito_rec, idsujeto_victima,#familia_delito_rec, 
                fec_comision_simple, clasificacion_pena_47, tramos_condena_48,
                marca_suspension_43, marca_pena_44, marca_multa_45,
                medida_alternativa_46, dist_tramo_cond) %>%
  dplyr::filter(dist_tramo_cond>1)

paste0("Son 112 casos de ",Base_fiscalia_v8 %>% 
         dplyr::left_join(rut_ruc_distinct_offenses, by= c("rut_enc_saf","ruc")) %>% 
         dplyr::filter(!is.na(n_distinct_offenses)) %>%
         dplyr::group_by(rut_enc_saf, ruc) %>%
         dplyr::mutate(dist_tramo_cond= n_distinct(tramos_condena_48)) %>% 
         dplyr::ungroup() %>% 
         dplyr::select(rut_enc_saf, ruc, gls_materia, region_delito_rec, idsujeto_victima,#familia_delito_rec, 
                       fec_comision_simple, clasificacion_pena_47, tramos_condena_48,
                       marca_suspension_43, marca_pena_44, marca_multa_45,
                       medida_alternativa_46, dist_tramo_cond) %>%nrow(),
       " que son los que tienen más de un delito distinto enun mismo RUC")

Base_fiscalia_v8 %>% 
  dplyr::left_join(rut_ruc_distinct_offenses, by= c("rut_enc_saf","ruc")) %>% 
  dplyr::filter(!is.na(n_distinct_offenses)) %>%
  dplyr::group_by(rut_enc_saf, ruc) %>%
  dplyr::mutate(dist_tramo_cond= n_distinct(tramos_condena_48)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(rut_enc_saf, ruc, gls_materia, region_delito_rec, idsujeto_victima,#familia_delito_rec, 
                fec_comision_simple, clasificacion_pena_47, tramos_condena_48,
                marca_suspension_43, marca_pena_44, marca_multa_45,
                medida_alternativa_46, dist_tramo_cond) %>%
  dplyr::filter(dist_tramo_cond>1) %>% 
  dplyr::filter(rut_enc_saf=="df9b1858811e9e744a40d92bbcdbd5fe")
