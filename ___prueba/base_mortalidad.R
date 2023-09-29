data<-
  rio::import("___datos_minsal/2023-08-11 DatosDefuncionesEncrip.csv")
#C:/Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\___datos_minsal

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

path<-paste0(getwd())
dir_c12<-paste0(gsub("22","19",path),"/Encriptados c1 - Minsal/")

#matches a string that starts with the number 2, followed by any number of characters, followed by a space, followed by the word "txt".
SISTRAT23_c12<-list.files(path=toString(dir_c12), pattern="_encrip")

#Import datasets from May 3, 2022
for (i in 1:length(SISTRAT23_c12)) {
  x<-SISTRAT23_c12[i]
  readr::read_delim(paste0(dir_c12, x),
                    na = c("", "NA","null"),
                    guess_max = min(1e5, Inf)) %>% 
    janitor::clean_names() %>% 
    as.data.frame() %>% 
    dplyr::rename("HASH_KEY"="run") %>% 
    dplyr::select(HASH_KEY, everything()) %>% 
    assign(paste0("SISTRAT232_c1_",stringr::str_sub(x, 1, 4)),.,envir = .GlobalEnv)
}

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#MERGE DATABASES
CONS_C1_2010_222=data.table::rbindlist(mget(paste0("SISTRAT232_c1_",c(2010:2022))), idcol="TABLE", fill=T) %>% 
  dplyr::mutate(TABLE = sub(".+(....)$", "\\1", TABLE))


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

data %>% group_by(ANO_DEF, SEXO) %>% count() %>% dplyr::ungroup() %>%  dplyr::group_by(SEXO) %>% dplyr::mutate(perc=scales::percent(n/sum(n))) %>% ggplot(aes(x=ANO_DEF, y=n))+geom_line()

data %>% dplyr::filter(DIA_NAC==3, MES_NAC==12, ANO1_NAC==19, ANO2_NAC==56, ANO_DEF==2002) %>% View()


data %>%
  dplyr::filter(HASHKEY=="1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464")
invisible("El rut vacío o inválido murió el 1998")

#AHOGAMIENTO Y SUMERSION NO ESPECIFICADOS: OTRO LUGAR ESPECIFICADO
#AHOGAMIENTO Y SUMERSION NO MORTAL
# https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Frepositoriodeis.minsal.cl%2FContenidoSitioWeb2020%2Fuploads%2F2017%2F08%2FSerie-defunciones-y-mortalidad-observada-por-tumores-malignos-edad_sexo.-Chile-1997-2015.xlsx&wdOrigin=BROWSELINK
# Comuna es la comuna de residencia

invisible("codigo unico territorial")
# https://www.bcn.cl/leychile/navegar?idNorma=168185&idVersion=2018-09-21&idParte=8362028
# https://www.subdere.gov.cl/documentacion/c%C3%B3digos-%C3%BAnicos-territoriales-actualizados-al-06-de-septiembre-2018

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#

#base encriptada a sept 2023
a_2012_2_encrip <- readr::read_delim("___prueba/2012_2_encrip.csv", na = c("", "NA","null"),
                                     guess_max = min(1e5, Inf),
                                     skip=0)
#View(a_2012_2_encrip)

a_2012_2_encrip %>% 
  dplyr::select("RUN","CodigoIdentificaciÂ.n", "SENDA", "Edad", "Nacionalidad", "ComunaResidencia", 
                "Sexo", "FechaIngresoaTratamiento", "FechaEgresodeTratamiento", "IDcentro",
                "MotivodeegresoAltaAdministra") %>% 
  dplyr::filter(RUN=="1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464") %>% 
  dplyr::select(SENDA, Edad, Nacionalidad, FechaIngresoaTratamiento, FechaEgresodeTratamiento)

#base de datos original a mayo 2023
a_2012_original_encrip <- readr::read_delim(paste0(gsub("22","19",paste0(getwd())),"/Encriptados c1 - Minsal/",
                                    "2012_encrip.csv"), na = c("", "NA","null"),
                                    guess_max = min(1e5, Inf),
                                    skip=0) #sep="\t",

#load("C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/___datos_minsal/comparacion_datos_minsal_senda_c1.RData")

dplyr::select(a_2012_original_encrip,  "RUN","CodigoIdentificaciÂ.n", "SENDA", "Edad", "Nacionalidad", 
                                                      "Sexo", "FechaIngresoaTratamiento", "FechaEgresodeTratamiento", 
                                                      "MotivodeegresoAltaAdministra") %>% 
  dplyr::filter(`RUN`=="1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464")

nuevos<-
  data %>% 
  dplyr::filter(HASHKEY %in% unique(a_2012_2_encrip$RUN)) %>% dplyr::select(HASHKEY)

a_2012_2_encrip %>% 
  dplyr::filter(RUN %in% nuevos$HASHKEY)

length(unique(a_2012_2_encrip$RUN)) #10481

#zombies tratados
a_2012_2_encrip %>% 
  dplyr::inner_join(data, by=c("RUN"="HASHKEY")) %>% 
  dplyr::select("RUN","CodigoIdentificaciÂ.n", "SENDA", "Edad", "Nacionalidad", 
"Sexo", "FechaIngresoaTratamiento", "FechaEgresodeTratamiento", 
"MotivodeegresoAltaAdministra", "DIA_NAC", "MES_NAC", "ANO1_NAC", "ANO2_NAC",
"SEXO", "EDAD_TIPO", "DIA_DEF", "MES_DEF", "ANO_DEF") %>% 
  #filtré el malo
  dplyr::filter(RUN!="1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464") %>% 
  dplyr::mutate(fech_egres= readr::parse_date(FechaEgresodeTratamiento, format="%d-%m-%Y")) %>%
  dplyr::mutate(ano_nac_senda= stringr::str_sub(`CodigoIdentificaciÂ.n`, nchar(`CodigoIdentificaciÂ.n`)-3,nchar(`CodigoIdentificaciÂ.n`))) %>% 
  dplyr::mutate(ano_nac_deis= paste0(ANO1_NAC,ANO2_NAC)) %>% 
  dplyr::mutate(Sexo=ifelse(Sexo=="Hombre",1,2)) %>% 
  dplyr::mutate(fech_falles= readr::parse_date(paste0(DIA_DEF,"-",MES_DEF,"-",ANO_DEF), format="%d-%m-%Y")) %>% 
  #760 pareados
  dplyr::filter(fech_falles<fech_egres) %>% 
  dplyr::mutate(diff_fech= fech_egres-fech_falles) %>% 
  #tratados después de morir, segun SENDA: 58
  dplyr::filter(ano_nac_senda!=ano_nac_deis|Sexo!=SEXO) %>%
  # de esos, sólo 4 no coincide el sexo  o la edad
  View()
  #janitor::tabyl(Sexo, SEXO)

paste0("Personas con año nacimiento o sexo distinto: ",
a_2012_2_encrip %>% 
  dplyr::inner_join(data, by=c("RUN"="HASHKEY")) %>% 
  dplyr::select("RUN","CodigoIdentificaciÂ.n", "SENDA", "Edad", "Nacionalidad", 
                "Sexo", "FechaIngresoaTratamiento", "FechaEgresodeTratamiento", 
                "MotivodeegresoAltaAdministra", "DIA_NAC", "MES_NAC", "ANO1_NAC", "ANO2_NAC",
                "SEXO", "EDAD_TIPO", "DIA_DEF", "MES_DEF", "ANO_DEF") %>% 
  dplyr::filter(RUN!="1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464") %>% 
  dplyr::mutate(fech_egres= readr::parse_date(FechaEgresodeTratamiento, format="%d-%m-%Y")) %>%
  dplyr::mutate(ano_nac_senda= stringr::str_sub(`CodigoIdentificaciÂ.n`, nchar(`CodigoIdentificaciÂ.n`)-3,nchar(`CodigoIdentificaciÂ.n`))) %>% 
  dplyr::mutate(ano_nac_deis= paste0(ANO1_NAC,ANO2_NAC)) %>% 
  dplyr::mutate(Sexo=ifelse(Sexo=="Hombre",1,2)) %>% 
  dplyr::mutate(fech_falles= readr::parse_date(paste0(DIA_DEF,"-",MES_DEF,"-",ANO_DEF), format="%d-%m-%Y")) %>% 
  #760 pareados
  #dplyr::filter(fech_falles<fech_egres) %>% 
  dplyr::mutate(diff_fech= fech_egres-fech_falles) %>% 
  #tratados después de morir, segun SENDA: 58
  dplyr::filter(ano_nac_senda!=ano_nac_deis|Sexo!=SEXO) %>%
  # de esos, sólo 4 no coincide el sexo  o la edad
  nrow())

#distribución dferencias
a_2012_2_encrip %>% 
  dplyr::inner_join(data, by=c("RUN"="HASHKEY")) %>% 
  dplyr::select("RUN","CodigoIdentificaci¢n", "SENDA", "Edad", "Nacionalidad", 
                "Sexo", "FechaIngresoaTratamiento", "FechaEgresodeTratamiento", 
                "MotivodeegresoAltaAdministra", "DIA_NAC", "MES_NAC", "ANO1_NAC", "ANO2_NAC",
                "SEXO", "EDAD_TIPO", "DIA_DEF", "MES_DEF", "ANO_DEF") %>% 
  dplyr::filter(RUN!="1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464") %>% 
  dplyr::mutate(fech_egres= readr::parse_date(FechaEgresodeTratamiento, format="%d-%m-%Y")) %>%
  dplyr::mutate(ano_nac_senda= stringr::str_sub(`CodigoIdentificaci¢n`, nchar(`CodigoIdentificaci¢n`)-3,nchar(`CodigoIdentificaci¢n`))) %>% 
  dplyr::mutate(ano_nac_deis= paste0(ANO1_NAC,ANO2_NAC)) %>% 
  dplyr::mutate(Sexo=ifelse(Sexo=="Hombre",1,2)) %>% 
  dplyr::mutate(fech_falles= readr::parse_date(paste0(DIA_DEF,"-",MES_DEF,"-",ANO_DEF), format="%d-%m-%Y")) %>% 
  #760 pareados
  dplyr::filter(fech_falles<fech_egres) %>% 
  dplyr::mutate(diff_fech= fech_egres-fech_falles) %>% 
  #summarise(min=min(diff_fech), p025=quantile(diff_fech, .025), p25=quantile(diff_fech, .25), p5=quantile(diff_fech, .5), p75=quantile(diff_fech, .75), p975=quantile(diff_fech, .975))
  ggplot(aes(x=diff_fech))+
  geom_histogram()+
  labs(x="Días de diferencia entre fecha de fecha de\negreso de tratamiento y fallecimiento", y="Recuento")+
  theme_bw()

run_malos_nueva<- a_2012_2_encrip %>% 
  dplyr::mutate(rn=row_number()) %>% 
  dplyr::filter(RUN=="1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464") %>% 
  mutate(concat= paste0(SENDA, Edad, Nacionalidad, ComunaResidencia, 
                        Sexo, FechaIngresoaTratamiento, FechaEgresodeTratamiento, IDcentro,
                        MotivodeegresoAltaAdministra))
nrow(run_malos_nueva)


run_malos_antigua<- 
a_2012_original_encrip%>% 
  dplyr::mutate(rn=row_number()) %>% 
  dplyr::filter(RUN=="1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464") %>% 
  mutate(concat= paste0(SENDA, Edad, Nacionalidad, Comuna.Residencia, 
                        Sexo, Fecha.Ingreso.a.Tratamiento, Fecha.Egreso.de.Tratamiento, ID.centro,
                        Motivo.de.egreso.Alta.Administrativa))
nrow(run_malos_antigua)

run_malos_nueva %>% 
  inner_join(run_malos_antigua[,c("RUN","concat","rn")], by="concat")
#los rut malos de la nueva no son lo mismo que de la antigua


run_malos_nueva %>% 
  inner_join(run_malos_antigua[,c("RUN","concat","rn")], by="rn")
#55 se parearon
#no se parearon por concatenación por como están formualdas las fechas



run_malos_nueva %>% 
  inner_join(a_2012_original_encrip%>% 
               dplyr::mutate(rn=row_number()) %>% 
               dplyr::filter(RUN!="1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464") %>% 
               mutate(concat= paste0(SENDA, Edad, Nacionalidad, Comuna.Residencia, 
                                     Sexo, Fecha.Ingreso.a.Tratamiento, Fecha.Egreso.de.Tratamiento, ID.centro,
                                     Motivo.de.egreso.Alta.Administrativa)) %>% 
               dplyr::select(RUN, concat, rn), by="rn")
#casos que pasaron a ser malos con el rut transformado conforme a .do
#CRFA109041967
#LUVE110041984
#JUMI131081983
#MASA106071956
#posiciones: 3435 7959 8372 11140


CONS_C1_2010_222 %>%  #CONS_C1_2019_222
  dplyr::inner_join(data, by=c("HASH_KEY"="HASHKEY"))

#ver los que originalmente se unieron 
CONS_C1_2010_222 %>%  #CONS_C1_2019_222
  dplyr::inner_join(data, by=c("HASH_KEY"="HASHKEY")) %>%
  dplyr::filter(HASH_KEY!="1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464") %>% distinct(HASH_KEY)
#BEAR114121952 JARA101071953 BEAR114121952 (base datos 2018)


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
run_malos_antigua
run_malos_nueva

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#En las anteriores comparé lso métodos de encriptación de la base al 2023, mientras que ahora estaré comparando con la base al 2019
invisible("Retrieve CONS_C1_df_dup_SEP_2020 from previous 8.RData (Fondecyt 1191282)")

load("13.RData", fiscalia_merge3 <- new.env() )

CONS_C1_df_dup_SEP_2020<- fiscalia_merge3$CONS_C1_df_dup_SEP_2020

rm(fiscalia_merge3)

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#Advertencia: no tienen la misma cantidad de RUT. La encriptación MINSAL tiene más: 11521 vs 11470

#original original (2019)
length(unique(janitor::clean_names(rio::import("C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2019 (github)/Encriptado c1/Personas tratadas C1/2012tab-Resultado-20191113.txt"))$opci_a3n_discapacidad))
#10486 rut distintos, de 11470 observaciones

#RUT mal procesado, MINSAL
length(unique(janitor::clean_names(rio::import("C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2019 (github)/Encriptados c1 - Minsal/2012_encrip.csv"))$run))
#9597 rut distintos, de 11521 observaciones

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#parear bases originales de cada uno

a_2012_2_encrip_est<-
a_2012_2_encrip %>% 
  dplyr::rename("Codigo Identificación"="CodigoIdentificaci¢n", "Comuna Residencia"= "ComunaResidencia", 
                "Fecha Ingreso a Tratamiento"=  "FechaIngresoaTratamiento", "Fecha Egreso de Tratamiento"="FechaEgresodeTratamiento",
                "ID centro"="IDcentro", "Motivo de egreso Alta Administrativa"="MotivodeegresoAltaAdministra")%>% 
  dplyr::select("RUN","Codigo Identificación", "SENDA", "Edad", "Nacionalidad", "Comuna Residencia", 
                "Sexo", "Fecha Ingreso a Tratamiento", "Fecha Egreso de Tratamiento", "ID centro",
                "Motivo de egreso Alta Administrativa") %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(fecha_egreso_de_tratamiento= ifelse(fecha_egreso_de_tratamiento=="13082013","13-08-2013",fecha_egreso_de_tratamiento)) %>% 
  dplyr::mutate(fecha_ingreso_a_tratamiento= readr::parse_date(fecha_ingreso_a_tratamiento, format="%d-%m-%Y")) %>% 
  dplyr::mutate(fecha_egreso_de_tratamiento= readr::parse_date(fecha_egreso_de_tratamiento, format="%d-%m-%Y")) %>% 
  dplyr::mutate(nacionalidad= dplyr::case_when(grepl("Bhut",nacionalidad)~"Bhután",
                                               grepl("Canad",nacionalidad)~"Canadá", 
                                               grepl("Espa",nacionalidad)~"España",
                                               grepl("Per",nacionalidad)~"Perú",
                                               grepl("blica Checa",nacionalidad)~"República Checa",
                                               grepl("Dominicana",nacionalidad)~"República Dominicana",
                                               T~nacionalidad))

a_2012_encrip_orig_orig <- readr::read_delim("C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2019 (github)/Encriptado c1/Personas tratadas C1/2012tab-Resultado-20191113.txt", na = c("", "NA","null"),
                                     guess_max = min(1e5, Inf),
                                     skip=0)

paste0("Filas pareadas del total (n=", nrow(a_2012_encrip_orig_orig),"): ",
a_2012_encrip_orig_orig%>% 
  dplyr::rename("RUN"="Opción discapacidad") %>% 
  dplyr::select("RUN","Codigo Identificación", "SENDA", "Edad", "Nacionalidad", "Comuna Residencia", 
                "Sexo", "Fecha Ingreso a Tratamiento", "Fecha Egreso de Tratamiento", "ID centro",
                "Motivo de egreso Alta Administrativa") %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(fecha_ingreso_a_tratamiento= str_replace_all(fecha_ingreso_a_tratamiento,"/","-"),
                fecha_egreso_de_tratamiento= str_replace_all(fecha_egreso_de_tratamiento,"/","-")) %>% 
  dplyr::mutate(fecha_egreso_de_tratamiento= ifelse(fecha_egreso_de_tratamiento=="13082013","13-08-2013",fecha_egreso_de_tratamiento)) %>% 
  dplyr::mutate(fecha_ingreso_a_tratamiento= readr::parse_date(fecha_ingreso_a_tratamiento, format="%d-%m-%Y")) %>% 
  dplyr::mutate(fecha_egreso_de_tratamiento= readr::parse_date(fecha_egreso_de_tratamiento, format="%d-%m-%Y")) %>% 
  dplyr::inner_join(a_2012_2_encrip_est, by= c("codigo_identificacion", "senda", 
                                               "edad", "comuna_residencia", "nacionalidad",  #"nacionalidad", creaba problemas
                                               "sexo", "fecha_ingreso_a_tratamiento", "fecha_egreso_de_tratamiento", 
                                               "id_centro", "motivo_de_egreso_alta_administrativa")) %>% nrow()
)#11445 11458 coinciden

paste0("Usuarios de la base antigua que no están en la nueva")
a_2012_2_encrip_est %>% 
  dplyr::filter(codigo_identificacion  %in% 
                  c("-VAR114051968", "-ASA207041973", 
                    "-FCA218051984", "-PAN129041981", "-KVE215051981", "-PLA125051990", 
                    "-SCU125031989", "-RTA219081971", "-RQU108111973", "-KAB220121988", 
                    "-COL108121980", "-RFR109061983"))

#janitor::clean_names(rio::import("C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2019 (github)/Encriptado c1/Personas tratadas C1/2012tab-Resultado-20191113.txt"))

#janitor::clean_names(rio::import("C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2019 (github)/Encriptado c1/Personas tratadas C1/2012tab-Resultado-20191113.txt"))

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#joined data frame

homolog_run_orig_minsal<-
a_2012_encrip_orig_orig%>% 
  dplyr::rename("RUN"="Opción discapacidad") %>% 
  dplyr::select("RUN","Codigo Identificación", "SENDA", "Edad", "Nacionalidad", "Comuna Residencia", 
                "Sexo", "Fecha Ingreso a Tratamiento", "Fecha Egreso de Tratamiento", "ID centro",
                "Motivo de egreso Alta Administrativa") %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(fecha_ingreso_a_tratamiento= str_replace_all(fecha_ingreso_a_tratamiento,"/","-"),
                fecha_egreso_de_tratamiento= str_replace_all(fecha_egreso_de_tratamiento,"/","-")) %>% 
  dplyr::mutate(fecha_egreso_de_tratamiento= ifelse(fecha_egreso_de_tratamiento=="13082013","13-08-2013",fecha_egreso_de_tratamiento)) %>% 
  dplyr::mutate(fecha_ingreso_a_tratamiento= readr::parse_date(fecha_ingreso_a_tratamiento, format="%d-%m-%Y")) %>% 
  dplyr::mutate(fecha_egreso_de_tratamiento= readr::parse_date(fecha_egreso_de_tratamiento, format="%d-%m-%Y")) %>% 
  dplyr::inner_join(a_2012_2_encrip_est, by= c("codigo_identificacion", "senda", 
                                               "edad", "comuna_residencia", "nacionalidad",  #"nacionalidad", creaba problemas
                                               "sexo", "fecha_ingreso_a_tratamiento", "fecha_egreso_de_tratamiento", 
                                               "id_centro", "motivo_de_egreso_alta_administrativa")) %>% 
  dplyr::rename("run_orig"="run.x", "run_minsal"="run.y") %>% 
  dplyr::select("run_orig","run_minsal")

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#join every database: sep 2020, new encryption and thus, mortality

CONS_C1_df_dup_SEP_2020_bd2012<-
homolog_run_orig_minsal %>% 
  #janitor::clean_names() %>% 
  dplyr::inner_join(CONS_C1_df_dup_SEP_2020, by=c("run_orig"="hash_key")) %>% 
  dplyr::left_join(data, by=c("run_minsal"="HASHKEY")) %>% 
  dplyr::filter(run_minsal!="1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464") %>%  
  dplyr::mutate(fech_falles= readr::parse_date(paste0(DIA_DEF,"-",MES_DEF,"-",ANO_DEF), format="%d-%m-%Y")) %>%  #18704 parsing failures. posiblemente no tienen fecha defuncón
  dplyr::mutate(fech_nac_minsal= readr::parse_date(paste0(DIA_NAC,"-",MES_NAC,"-",ANO1_NAC,ANO2_NAC), format="%d-%m-%Y"))   #18704 parsing failures. posiblemente no tienen fecha defuncón
  #glimpse()# 19,912
  
