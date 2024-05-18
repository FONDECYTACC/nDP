rm(list = ls());gc()

if(!require(janitor)){install.packages("janitor");require(janitor)}
if(!require(survex)){install.packages("survex");require(survex)}
if(!require(tidyverse)){install.packages("tidyverse");require(tidyverse)}
if(!require(rio)){install.packages("rio");require(rio)}
if(!require(ranger)){install.packages("ranger");require(ranger)}
if(!require(survival)){install.packages("survival");require(survival)}
if(!require(glmnet)){install.packages("glmnet");require(glmnet)}
if(!require(randomForestSRC)){install.packages("randomForestSRC");require(randomForestSRC)}

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

data<-
  rio::import("E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/_proposal_grant/2023/data_mine_miss_proc2_20240417.gz.parquet") %>% 
  dplyr::group_by(id) %>% 
  dplyr::mutate(lag_fech_egres_num= lag(fech_egres_num, default= 14283)) %>%  #fecha mínima menos 1
  dplyr::ungroup() %>% 
  dplyr::mutate(log_lag_dias_treat_imp_sin_na= log(ifelse(is.na(lag_dias_treat_imp_sin_na),4.499811, lag_dias_treat_imp_sin_na)+0.0001)) %>% #4.499811 es 90 días
  dplyr::mutate(event=1) %>% 
  data.frame()
# create a model



# Lasso -------------------------------------------------------------------


invisible("Model dont let missing variables in dependent variables")

ysurv<- Surv(ifelse(is.na(data$lag_time),0,data$lag_time),data$time,data$event==1)


invisible("2024-04-21: para darle trayectoria")
data[which(is.na(ysurv)),"time"]<-.001

ysurv<- Surv(ifelse(is.na(data$lag_time),0,data$lag_time),data$time,data$event==1)

#Error in response.coxnet(y) : NAs encountered in response, not allowed
invisible("2024-04-21: me vi obligado a convertir el lag_time a presencia")
xmat <- model.matrix(~ lag_tr_outcome + log_lag_dias_treat_imp_sin_na + 
                       lag_comp_bpsc_y2_moderate + lag_comp_bpsc_y3_severe + lag_policonsumo2 + 
                       edad_al_ing_1 + ano_nac_corr + esc_dum_rec_3prim + esc_dum_rec_2high + 
                       susprindum_oh + susprindum_coc + susprindum_pbc + susprindum_mar + 
                       freq_cons_dum_5day + freq_cons_dum_44to6wk + freq_cons_dum_32to3wk + 
                       freq_cons_dum_21wkmore + cond_oc_dum_3unemp + cond_oc_dum_2inact + 
                       viv_dum_illegal + viv_dum_own + viv_dum_rent + viv_dum_temp + 
                       psycom_dum_with + psycom_dum_study + susinidum_oh + susinidum_coc + 
                       susinidum_pbc + susinidum_mar + cohab_dum_alone + cohab_dum_fam_or + 
                       cohab_dum_cpl_child - 1, data = data)  # `-1` to exclude intercept

#https://stackoverflow.com/questions/75352910/apply-lasso-in-r-using-glmnet-package-for-cox-model
lasso<- cv.glmnet(xmat, ysurv, alpha = 1, family = 'cox', nfolds = 10) #alpha=1, which is all LASSO, no ridge,

#LASSO: Es una técnica de regularización que ayuda a mejorar la precisión de un modelo estadístico. Lo que hace LASSO es 
#penalizar el modelo por tener demasiados coeficientes (representando las variables), reduciendo o eliminando aquellos 
#que no son importantes. Esto ayuda a simplificar el modelo eliminando variables que no contribuyen significativamente a 
#explicar el evento estudiado, lo que a su vez mejora la generalización del modelo a nuevos datos.


plot(lasso)
c<-coef(lasso ,s='lambda.min')
c2<-coef(lasso, s="lambda.1se")

coef_matrix1 <- as.matrix(c)
coef_df1 <- as.data.frame(coef_matrix1)
coef_df1$variable <- rownames(coef_matrix1)
rownames(coef_df1)<-NULL
names(coef_df1)[1] <- "coefficient"

coef_matrix2 <- as.matrix(c2)
coef_df2 <- as.data.frame(coef_matrix2)
coef_df2$variable <- rownames(coef_matrix2)
rownames(coef_df2)<-NULL
names(coef_df2)[1] <- "coefficient"

## Lasso, sólo con más de 1 tto -------------------------------------------------------------------

invisible("Select people with multiple treatments")


ysurv2<- Surv(ifelse(is.na(subset(data, treatment>1)$lag_time),0,subset(data, treatment>1)$lag_time),subset(data, treatment>1)$time,subset(data, treatment>1)$event==1)

xmat2 <- model.matrix(~ lag_tr_outcome + log_lag_dias_treat_imp_sin_na + 
                       lag_comp_bpsc_y2_moderate + lag_comp_bpsc_y3_severe + lag_policonsumo2 + 
                       edad_al_ing_1 + ano_nac_corr + esc_dum_rec_3prim + esc_dum_rec_2high + 
                       susprindum_oh + susprindum_coc + susprindum_pbc + susprindum_mar + 
                       freq_cons_dum_5day + freq_cons_dum_44to6wk + freq_cons_dum_32to3wk + 
                       freq_cons_dum_21wkmore + cond_oc_dum_3unemp + cond_oc_dum_2inact + 
                       viv_dum_illegal + viv_dum_own + viv_dum_rent + viv_dum_temp + 
                       macro_dum_south + macro_dum_north + psycom_dum_with + psycom_dum_study + 
                       rurality_rural + rurality_mix + susinidum_oh + susinidum_coc + 
                       susinidum_pbc + susinidum_mar + cohab_dum_alone + cohab_dum_fam_or + 
                       cohab_dum_cpl_child + porc_pobr - 1, data = subset(data, treatment>1))  # `-1` to exclude intercept


lasso2<- cv.glmnet(xmat2, ysurv2, alpha = 1, family = 'cox', nfolds = 10) #lpha=1, which is all LASSO, no ridge,

plot(lasso2)
cb<-coef(lasso2 ,s='lambda.min')
cb2<-coef(lasso2, s="lambda.1se")


coef_matrix3 <- as.matrix(cb)
coef_df3 <- as.data.frame(coef_matrix3)
coef_df3$variable <- rownames(coef_matrix3)
rownames(coef_df3)<-NULL
names(coef_df3)[1] <- "coefficient"

coef_matrix4 <- as.matrix(cb2)
coef_df4 <- as.data.frame(coef_matrix4)
coef_df4$variable <- rownames(coef_matrix4)
rownames(coef_df4)<-NULL
names(coef_df4)[1] <- "coefficient"

invisible("Select people with multiple treatments; more flexible ")
lasso3<- cv.glmnet(xmat2, ysurv2, alpha = 0.95, family = 'cox', nfolds = 10) #lpha=1, which is all LASSO, no ridge,
cc<-coef(lasso3 ,s='lambda.min')
cc2<-coef(lasso3, s="lambda.1se")

coef_matrix5 <- as.matrix(cc)
coef_df5 <- as.data.frame(coef_matrix5)
coef_df5$variable <- rownames(coef_matrix5)
rownames(coef_df5)<-NULL
names(coef_df5)[1] <- "coefficient"

coef_matrix6 <- as.matrix(cc2)
coef_df6 <- as.data.frame(coef_matrix6)
coef_df6$variable <- rownames(coef_matrix6)
rownames(coef_df6)<-NULL
names(coef_df6)[1] <- "coefficient"


subset(coef_df1, coefficient==0) #recomienda sacar
# coefficient               variable
# 8            0      esc_dum_rec_3prim
# 9            0      esc_dum_rec_2high
# 10           0          susprindum_oh
# 16           0  freq_cons_dum_32to3wk
# 17           0 freq_cons_dum_21wkmore
# 18           0     cond_oc_dum_3unemp
# 20           0        viv_dum_illegal
# 21           0            viv_dum_own
# 23           0           viv_dum_temp
# 27           0          susinidum_coc
# 30           0        cohab_dum_alone

subset(coef_df2, coefficient!=0) #recomienda no sacar
#      coefficient                      variable
# 1   7.604771e-02                lag_tr_outcome
# 2  -1.785484e-01 log_lag_dias_treat_imp_sin_na
# 3  -2.003369e-02     lag_comp_bpsc_y2_moderate
# 4   5.138561e-01       lag_comp_bpsc_y3_severe
# 6  -2.884237e-03                 edad_al_ing_1
# 7  -1.482459e-05                  ano_nac_corr
# 24 -2.322566e-02               psycom_dum_with
# 25  6.339134e-01              psycom_dum_study
# 26 -2.312091e-03                  susinidum_oh
# 29  3.528435e-03                 susinidum_mar

subset(coef_df3, coefficient==0) #recomienda sacar
#    coefficient                  variable
# 3            0 lag_comp_bpsc_y2_moderate
# 5            0          lag_policonsumo2
# 8            0         esc_dum_rec_3prim
# 9            0         esc_dum_rec_2high
# 30           0              susinidum_oh

subset(coef_df4, coefficient!=0) 
# coefficient                      variable
# 1   0.1187389859                lag_tr_outcome
# 2  -0.0363916315 log_lag_dias_treat_imp_sin_na
# 4   0.0341563312       lag_comp_bpsc_y3_severe
# 5  -0.0776012229              lag_policonsumo2
# 7   0.0001954855                  ano_nac_corr
# 13 -0.0092210457                susprindum_mar
# 14  0.0335335482            freq_cons_dum_5day
# 19  0.0272487762            cond_oc_dum_2inact
# 25 -0.0157433400               macro_dum_north
# 27  0.0122935062              psycom_dum_study
# 29  0.0366599593                  rurality_mix
# 30 -0.0284702924                  susinidum_oh
# 33  0.0208042150                 susinidum_mar
# 37 -1.6920290090                     porc_pobr

subset(coef_df5, coefficient==0) 
# coefficient               variable
# 17           0 freq_cons_dum_21wkmore
# 23           0           viv_dum_temp
# 30           0           susinidum_oh

subset(coef_df6, coefficient!=0) 
# coefficient                      variable
# 1   0.1069187313                lag_tr_outcome
# 2  -0.0347001243 log_lag_dias_treat_imp_sin_na
# 4   0.0256046602       lag_comp_bpsc_y3_severe
# 5  -0.0616591066              lag_policonsumo2
# 7   0.0001871718                  ano_nac_corr
# 14  0.0253127501            freq_cons_dum_5day
# 19  0.0123718741            cond_oc_dum_2inact
# 27  0.0016620570              psycom_dum_study
# 29  0.0078267992                  rurality_mix
# 30 -0.0250701244                  susinidum_oh
# 33  0.0108899486                 susinidum_mar
# 37 -1.5865107318                     porc_pobr


# Survex, importance -------------------------------------------------------------------

#https://cran.r-project.org/web/packages/randomForestSRC/randomForestSRC.pdf#page=98&zoom=100,133,289
library(randomForestSRC)

inviisble("Si pongo fech_egres_num/30.1 me tira Error in parseFormula(formula, data, ytry) : Survival formula incorrectly specified.")

rfsc <- rfsrc(
  Surv(time_final,event)~ lag_tr_outcome+ #2024-04-20, corregí acorde
    log_lag_dias_treat_imp_sin_na+ 
    lag_comp_bpsc_y2_moderate+ 
    lag_comp_bpsc_y3_severe+ 
    lag_policonsumo2+ 
    edad_al_ing_1+ 
    ano_nac_corr+ 
    esc_dum_rec_3prim+ 
    esc_dum_rec_2high +
    susprindum_oh +
    susprindum_coc +
    susprindum_pbc +
    susprindum_mar+
    freq_cons_dum_5day+
    freq_cons_dum_44to6wk+
    freq_cons_dum_32to3wk+
    freq_cons_dum_21wkmore+
    cond_oc_dum_3unemp+
    cond_oc_dum_2inact+
    viv_dum_illegal+
    viv_dum_own+
    viv_dum_rent+
    viv_dum_temp+
    macro_dum_south+
    macro_dum_north+
    psycom_dum_with+
    psycom_dum_study+
    rurality_rural+
    rurality_mix+
    susinidum_oh +
    susinidum_coc +
    susinidum_pbc +
    susinidum_mar+
    cohab_dum_alone+
    cohab_dum_fam_or+
    cohab_dum_cpl_child+ 
    porc_pobr,
  data= dplyr::mutate(data.frame(dplyr::mutate(data,time_final=time-lag_time)),time_final=time-lag_time), 
  importance = "anti",
  seed= 2125
)

explainer <- survex::explain(rfsc)

#Partial dependence: How does a variable affect the average prediction?
m_profile <- survex::model_profile(explainer)

plot(m_profile,
     numerical_plot_type="lines")
#muy grande y no la entiendo bien     

m_parts <- survex::model_parts(explainer)
#loss_function, type, output_type
plot(m_parts)
#Permutational feature importance for the rfsrc model:

#search_interactions<-
#find.interaction(rfsc, method = "vimp", nvar = 8)
invisible("No se puede")



## Survex, sólo on más de 1 tto -------------------------------------------------------------------


rfsc2 <- rfsrc(
  Surv(time_final,event)~ lag_tr_outcome+  #2024-04-20, corregí acorde
    log_lag_dias_treat_imp_sin_na+ 
    lag_comp_bpsc_y2_moderate+ 
    lag_comp_bpsc_y3_severe+ 
    lag_policonsumo2+ 
    edad_al_ing_1+ 
    ano_nac_corr+ 
    esc_dum_rec_3prim+ 
    esc_dum_rec_2high +
    susprindum_oh +
    susprindum_coc +
    susprindum_pbc +
    susprindum_mar+
    freq_cons_dum_5day+
    freq_cons_dum_44to6wk+
    freq_cons_dum_32to3wk+
    freq_cons_dum_21wkmore+
    cond_oc_dum_3unemp+
    cond_oc_dum_2inact+
    viv_dum_illegal+
    viv_dum_own+
    viv_dum_rent+
    viv_dum_temp+
    macro_dum_south+
    macro_dum_north+
    psycom_dum_with+
    psycom_dum_study+
    rurality_rural+
    rurality_mix+
    susinidum_oh +
    susinidum_coc +
    susinidum_pbc +
    susinidum_mar+
    cohab_dum_alone+
    cohab_dum_fam_or+
    cohab_dum_cpl_child+ 
    porc_pobr,
  data= subset(data.frame(dplyr::mutate(data,time_final=time-lag_time)), treatment>1), 
  importance = "anti",
  seed= 2125
)



explainer2 <- survex::explain(rfsc2)

#Partial dependence: How does a variable affect the average prediction?
m_profile2 <- survex::model_profile(explainer2)


m_parts2 <- survex::model_parts(explainer2)
#loss_function, type, output_type
plot(m_parts2)
#Permutational feature importance for the rfsrc model:


#save.image("E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/_proposal_grant/2023/20240418_survex.RData")