load("E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/10.RData")
#2023-04-28
#el siguiente código buscaba ver qué filas fueron colapsadas por tratamientos continuos (row_cont_entries), 
#en una submuestra de casos (df_q1). 

require(tidyverse)
glimpse(CONS_C1_df_dup_SEP_2020)
require(tidylog)
# Calcular la cantidad de filas colapsadas por tratamientos continuos en el dataframe CONS_C1_df_dup_SEP_2020
CONS_C1_df_dup_SEP_2020 %>%  
  dplyr::mutate(collapsed= ifelse(nchar(row_cont_entries)>2,1,0)) %>% janitor::tabyl(collapsed)

# Filtrar las filas colapsadas y agregar un índice
CONS_C1_df_dup_SEP_2020_sep<-
  CONS_C1_df_dup_SEP_2020 %>%  
  dplyr::filter(nchar(row_cont_entries)>2) %>% 
  dplyr::mutate(index=1) %>% 
  dplyr::select(hash_key, fech_ing_num, index)

# Leer el archivo df_q1.xlsx y convertir la columna fech_ing_num_1 a formato numérico
df_q1 <- readxl::read_excel("_ig_borquez/df_q1.xlsx") %>% 
  dplyr::mutate(fech_ing_num_1=as.numeric(as.Date(stringr::str_extract(fech_ing_num_1,"^.{10}"))))

# Comprobar si hay duplicados en el dataframe combinado
if(dplyr::left_join(df_q1, CONS_C1_df_dup_SEP_2020_sep, by=c("rut_enc_saf"="hash_key", "fech_ing_num_1"="fech_ing_num")) %>% nrow()>nrow(df_q1)){
  dup_cases<-
    dplyr::left_join(df_q1, CONS_C1_df_dup_SEP_2020_sep, by=c("rut_enc_saf"="hash_key", "fech_ing_num_1"="fech_ing_num")) %>% 
    dplyr::group_by(rut_enc_saf, fech_ing_num_1) %>% dplyr::mutate(n=n()) %>% 
    dplyr::filter(n>2) %>% nrow()
  warning(paste0("The join made more rows (n=",dup_cases,")"))
}

# Unir df_q1 con el dataframe filtrado y calcular la cantidad de casos por grupo
dplyr::left_join(df_q1, CONS_C1_df_dup_SEP_2020_sep, by=c("rut_enc_saf"="hash_key", "fech_ing_num_1"="fech_ing_num")) %>% 
  dplyr::group_by(rut_enc_saf, fech_ing_num_1) %>% dplyr::mutate(n=n())

# Calcular la distribución de la variable index en el dataframe combinado
dplyr::left_join(df_q1, CONS_C1_df_dup_SEP_2020_sep, by=c("rut_enc_saf"="hash_key", "fech_ing_num_1"="fech_ing_num")) %>% janitor::tabyl(index)

# Exportar el dataframe combinado a un archivo de Excel
dplyr::left_join(df_q1, CONS_C1_df_dup_SEP_2020_sep, by=c("rut_enc_saf"="hash_key", "fech_ing_num_1"="fech_ing_num")) %>% 
  rio::export("casos_con_derivacion_menos_45_dias_2.xlsx")

# Calcular el número de episodios en el dataframe CONS_C1_df_dup_SEP_2020
CONS_C1_df_dup_SEP_2020_num_episodes<-
  CONS_C1_df_dup_SEP_2020 %>%  # 109756
  dplyr::filter(nchar(row_cont_entries)>2) %>%
  mutate(num_words = stringr::str_count(row_cont_entries, ";") + 1, num_words_minus_1= stringr::str_count(row_cont_entries, ";")) %>% 
  dplyr::select(hash_key, fech_ing_num, row_cont_entries, num_words, num_words_minus_1) %>% janitor::tabyl(num_words_minus_1)

# Generar una tabla para ver 
dplyr::left_join(df_q1, CONS_C1_df_dup_SEP_2020_num_episodes, by=c("rut_enc_saf"="hash_key", "fech_ing_num_1"="fech_ing_num")) %>% 
  janitor::tabyl(num_words_minus_1)

#nrow(CONS_C1_df_dup_JUN_2020)-6679
#109756 -6679
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#Para seguridad y porque no calzaban los totales, se decidió comparar con los casos registrados en el proceso de deduplicación 4 y corroborar
#la información


rm(list=ls());gc()
load("E:/Mi unidad/Alvacast/SISTRAT 2019 (github)/7.RData")

derivacion_45_dias<-
CONS_C1_df_dup_JUN_2020%>% 
  dplyr::filter(!is.na(diff_bet_treat))%>%
  #  dplyr::group_by(hash_key)%>%
  #  dplyr::mutate(sum_validos=sum(!is.na(diff_bet_treat)))%>%
  #  ungroup()%>%
  #  dplyr::filter(sum_validos>0)%>%
  dplyr::filter(diff_bet_treat<45 & motivoegreso_derivacion=="Referral")%>%
  dplyr::select(hash_key, fech_ing)%>% 
  dplyr::mutate(fech_ing_num=as.numeric(as.Date(stringr::str_extract(fech_ing,"^.{10}"))))

if(dplyr::left_join(df_q1, derivacion_45_dias, by=c("rut_enc_saf"="hash_key", "fech_ing_num_1"="fech_ing_num")) %>% nrow()>nrow(df_q1)){
  dup_cases<-
    dplyr::left_join(df_q1, derivacion_45_dias, by=c("rut_enc_saf"="hash_key", "fech_ing_num_1"="fech_ing_num")) %>% 
    dplyr::group_by(rut_enc_saf, fech_ing_num_1) %>% dplyr::mutate(n=n()) %>% 
    dplyr::filter(n>2) %>% nrow()
  warning(paste0("The join made more rows (n=",dup_cases,")"))
}

dplyr::left_join(df_q1, derivacion_45_dias, by=c("rut_enc_saf"="hash_key", "fech_ing_num_1"="fech_ing_num")) %>% 
  dplyr::mutate(fech_ing=ifelse(nchar(as.character(fech_ing))>0,1,0)) %>% janitor::tabyl(fech_ing)


dplyr::left_join(df_q1, derivacion_45_dias, by=c("rut_enc_saf"="hash_key", "fech_ing_num_1"="fech_ing_num")) %>% 
  dplyr::mutate(fech_ing=ifelse(nchar(as.character(fech_ing))>0,1,0)) %>% 
  rio::export("casos_con_derivacion_menos_45_dias.xlsx")

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_