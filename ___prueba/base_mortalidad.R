data<-
  rio::import("E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/___datos_minsal/2023-08-11 DatosDefuncionesEncrip.csv")

data %>% group_by(ANO_DEF, SEXO) %>% count() %>% dplyr::ungroup() %>%  dplyr::group_by(SEXO) %>% dplyr::mutate(perc=scales::percent(n/sum(n))) %>% ggplot(aes(x=ANO_DEF, y=n))+geom_line()

data %>% dplyr::filter(DIA_NAC==3, MES_NAC==12, ANO1_NAC==19, ANO2_NAC==56, ANO_DEF==2002) %>% View()

#AHOGAMIENTO Y SUMERSION NO ESPECIFICADOS: OTRO LUGAR ESPECIFICADO
#AHOGAMIENTO Y SUMERSION NO MORTAL
# https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Frepositoriodeis.minsal.cl%2FContenidoSitioWeb2020%2Fuploads%2F2017%2F08%2FSerie-defunciones-y-mortalidad-observada-por-tumores-malignos-edad_sexo.-Chile-1997-2015.xlsx&wdOrigin=BROWSELINK
# Comuna es la comuna de residencia

invisible("codigo unico territorial")
# https://www.bcn.cl/leychile/navegar?idNorma=168185&idVersion=2018-09-21&idParte=8362028
# https://www.subdere.gov.cl/documentacion/c%C3%B3digos-%C3%BAnicos-territoriales-actualizados-al-06-de-septiembre-2018