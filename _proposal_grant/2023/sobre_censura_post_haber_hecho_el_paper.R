install.packages("mets"); library(mets)
#time son los meses (30.5 dias) desde el primer ingreso hasta el egreso, de existir. L particularidad de esta submuestra, es que no tiene censura (cens_time)
prtw <- mets::ipw(Surv(time2,tr_outcome==0)~  ano_nac_corr+ edad_ini_cons+ edad_al_ing_1 + comp_bpsc_y+ strata(tipo_de_plan_2), # sacar cluster(hash_key)+ 
            data= data_mine_miss_restr_proc2 %>% dplyr::mutate(time2=ifelse(tr_outcome==1, time, cens_time)),
            cluster="hash_key",
            weight.name="ipwc")

summary(prtw$ipwc)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0004389 0.8132962 0.9959005 0.8513378 0.9999274 1.0000000 




# Prepare the data

invisible("NO sirve")


prtw <- mets::ipw(Surv(time2,tr_outcome==0)~  log(edad_al_ing_1)+ edad_ini_cons + strata(tipo_de_plan_2), # sacar cluster(hash_key)+ 
                  data= data_mine_miss_restr_proc2%>% dplyr::mutate(time2=ifelse(tr_outcome==1, time, cens_time)),
                  cluster="hash_key",
                  trunc.prob=T,
                  weight.name2="trunc",
                  weight.name="ipwc")

summary(prtw$ipwc)

summary(prtw$pr)