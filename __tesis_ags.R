

bind_rows(CONS_C1_2010_19,CONS_C1_2019_22) %>% 
  dplyr::filter(motivo_de_egreso!="Derivación") %>% 
  summarise(min=min(dias_en_tratamiento, na.rm=T), p01= quantile(dias_en_tratamiento, .01,na.rm=T), 
            p025=quantile(dias_en_tratamiento, .025,na.rm=T), max= max(dias_en_tratamiento, na.rm=T))


bind_rows(CONS_C1_2010_19,CONS_C1_2019_22) %>% 
  dplyr::filter(motivo_de_egreso!="Derivación", dias_en_tratamiento< quantile(dias_en_tratamiento, .01,na.rm=T)) %>%
  nrow()


bind_rows(CONS_C1_2010_19,CONS_C1_2019_22) %>% 
  dplyr::filter(motivo_de_egreso!="Derivación", dias_en_tratamiento< quantile(dias_en_tratamiento, .025,na.rm=T)) %>%
  nrow()


bind_rows(CONS_C1_2010_19,CONS_C1_2019_22) %>% 
  dplyr::filter(motivo_de_egreso!="Derivación", dias_en_tratamiento>=0) %>% 
  ggplot()+
  geom_histogram(aes(x=dias_en_tratamiento), bins=80)+
  sjPlot::theme_sjplot()+
  labs(x="Days in treatment", y= "Count")

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

