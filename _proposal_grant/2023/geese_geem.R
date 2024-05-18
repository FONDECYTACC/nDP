
load("E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/an_grant_23_24_3.RData")

if(!require(timereg)){install.packages("timereg");library(timereg)}
if(!require(coxphw)){install.packages("coxphw");library(coxphw)}


# 1. Timecox --------------------------------------------------------------


# timecox_pwp<- 
#   timecox(Surv(Tstart, Tstop, status) ~ tipo_de_plan_res_1*strata(trans)+  
#             TD_1+ TD_2+ TD_3+ TD_4+ cluster(id), Nit = 100,
#           data = dt_ms_d_match_surv, n.sim=3000, robust = 1, weighted.test = 1)

timecox<-
timecox(Surv(lag_time,time,event==1)~ lag_tr_outcome_rec +
        log_lag_dias_treat_imp_sin_na +
        lag_less_90d_tr1_rec+
        lag_comp_bpsc_y3_severe_rec + 
        lag_policonsumo2 + 
        edad_al_ing_1 + 
        ano_nac_corr + 
        susinidum_coc_rec2 +
        susinidum_oh +
        susinidum_pbc +
        susinidum_mar +
        psycom_dum_with_rec2 +
        psycom_dum_study + 
        freq_cons_dum_5day +
        cond_oc_dum_2inact +
        cond_oc_dum_3unemp +
        susprindum_oh +
        susprindum_coc +
        susprindum_pbc +
        susprindum_mar+
        strata(tipo_de_plan_2_mod),  #+ cluster(id)
      data=data_mine_miss_restr_proc2 %>% 
        data.table::as.data.table() %>% data.frame(),Nit = 100, n.sim=3000, robust = 1, weighted.test = 1, clusters=data_mine_miss_restr_proc2$id)
#Error in read.design(m, Terms) :  More than one cluster(-) term was included in the formula

invisible("")
#Error in timecoxBase(times, ldata, X, status, id, bhat, sim = sim, antsim = n.sim,  : 
#                       'Calloc' could not allocate memory (441800 of 8 bytes)



summary_test_t_inv0<-capture.output(summary(timecox))
summary_test_t_inv_sel0<-summary_test_t_inv0[which(grepl("Test for time invariant", summary_test_t_inv0)):(which(grepl("Call", summary_test_t_inv0))-3)]
summary_test_t_inv_sel0_df<-
  data.frame(summary_test_t_inv_sel0) %>% 
  dplyr::mutate(summary_test_t_inv_sel0=gsub("\\s+\\s+\\s+\\s+\\s+", "\\[\\[", summary_test_t_inv_sel0)) %>%  
  tidyr::separate(summary_test_t_inv_sel0,c("Variable","Test","pvalue"),sep="\\[\\[") %>% 
  dplyr::slice(-1) %>% 
  #test p-value H_0:constant effect
  dplyr::mutate(type=dplyr::case_when(grepl("Kolmogorov",Test)~ "Kolmogorov-Smirnov",
                                      grepl("Cramer",Test)~ "Cramer von Mises",
                                      T~NA_character_)) %>% 
  tidyr::fill(type,.direction = "down") %>% 
  dplyr::select(Variable, type, Test, pvalue) %>% 
  tidyr::drop_na(pvalue)




# 2. Coxphw ---------------------------------------------------------------
#https://cran.r-project.org/web/packages/coxphw/coxphw.pdf

coxphwmod<-
  coxphw(Surv(lag_time,time,event==1)~ lag_tr_outcome_rec +
            log_lag_dias_treat_imp_sin_na +
            lag_less_90d_tr1_rec+
            lag_comp_bpsc_y3_severe_rec + 
            lag_policonsumo2 + 
            edad_al_ing_1 + 
            ano_nac_corr + 
            susinidum_coc_rec2 +
            susinidum_oh +
            susinidum_pbc +
            susinidum_mar +
            psycom_dum_with_rec2 +
            psycom_dum_study + 
            freq_cons_dum_5day +
            cond_oc_dum_2inact +
            cond_oc_dum_3unemp +
            susprindum_oh +
            susprindum_coc +
            susprindum_pbc +
            susprindum_mar+
           cluster(id)+ strata(tipo_de_plan_2_mod),  #
          data=data_mine_miss_restr_proc2 %>% 
            data.table::as.data.table() %>% data.frame())
#Error: no se puede ubicar un vector de tamaño  1.3 Gb


# 3. GEE ---------------------------------------------------------------
#https://cran.r-project.org/web/packages/coxphw/coxphw.pdf


install.packages("geeM")
library(geeM)

summary.geem <- function(object, ...)  {
  Coefs <- matrix(NA,nrow=length(object$beta),ncol=5)
  Coefs[,1] <- c(object$beta)
  naive <- is.character(object$var)
  if(!naive && any(diag(object$var) < 0) ){
    naive <- TRUE
    warning("Some elements of robust variance estimate < 0.  Reporting model based SE.")
  }
  Coefs[,2] <- sqrt(diag(object$naiv.var))
  if(naive){Coefs[,3] <- rep(NA, length(object$beta))}else{Coefs[,3] <- sqrt(diag(object$var))}
  if(naive){Coefs[,4] <- Coefs[,1]/Coefs[,2]}else{Coefs[,4] <- Coefs[,1]/Coefs[,3]}
  Coefs[,5] <- round(2*pnorm(abs(Coefs[,4]), lower.tail=F), digits=8)
  colnames(Coefs) <- c("Estimates","Model SE","Robust SE", "wald", "p")
  summ <<- list(beta = Coefs[,1], se.model = Coefs[,2], se.robust = Coefs[,3], wald.test = Coefs[,4], p = Coefs[,5],
                alpha = object$alpha, corr = object$corr, phi = object$phi, niter = object$niter, clusz = object$clusz,
                coefnames = object$coefnames, weights=object$weights, biggest.R.alpha = object$biggest.R.alpha)
  class(summ) <- 'summary.geem'
  return(summ)
}

#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4289620/
# The 'M' in the name 'geeM' is meant to emphasize the use of the Matrix package, which allows for an
# implementation based fully in R
# https://cran.r-project.org/web/packages/geeM/geeM.pdf
sens_mod1<-
  geem(tr_outcome  ~ policonsumo2 +
           edad_al_ing_1 + 
           ano_nac_corr + 
           susinidum_oh +
           susinidum_coc +
           susinidum_pbc +
           susinidum_mar +
           psycom_dum_study +
           psycom_dum_with +
           freq_cons_dum_5day +
           cond_oc_dum_2inact +
           cond_oc_dum_3unemp +
           susprindum_coc +
           susprindum_pbc +
           susprindum_mar, 
         id=id, data=data.frame(data_mine_miss_restr_proc2_iiw_after_ph), family=binomial) #, corstr="ar1"
summary(sens_mod1)
summ_mod1<-
  cbind.data.frame(cc=summ$coefnames,
                   Estimate=summ$beta,Std.err=summ$se.robust) %>% 
  dplyr::mutate(lwr=Estimate-qnorm((1+0.95)/2)*Std.err,upr=Estimate+qnorm((1+0.95)/2)*Std.err) %>% 
  dplyr::mutate(across(where(is.numeric),~round(exp(.),2)))



install.packages("geepack")
library(geepack)


install.packages("geeasy")
library(geeasy)


#geepack uses sandwich standard errors (san.se)

coef.geese <- function(object) object$beta
vcov.geese <- function(object, std.err = "san.se") { 
  covmat <- switch(std.err, 
                   jack = {
                     object$vbeta.ajs
                   }, 
                   j1s = {
                     object$vbeta.j1s
                   }, 
                   fij = {
                     object$vbeta.fij
                   }, 
                   object$vbeta)
  nm <- names(coef(object))
  dimnames(covmat) <- list(nm, nm)
  covmat
}

mod4<-geepack::geese(tr_outcome  ~ policonsumo2 +
                       edad_al_ing_1 + 
                       ano_nac_corr + 
                       susinidum_oh +
                       susinidum_coc +
                       susinidum_pbc +
                       susinidum_mar +
                       psycom_dum_study +
                       psycom_dum_with +
                       freq_cons_dum_5day +
                       cond_oc_dum_2inact +
                       cond_oc_dum_3unemp +
                       susprindum_coc +
                       susprindum_pbc +
                       susprindum_mar, 
                     id=id, data=data.frame(data_mine_miss_restr_proc2_iiw_after_ph), 
                     family=poisson,jack = T)

confint(mod4, parm = NULL, level = 0.95, std.err = "san.se")

install.packages("geesmv")
library(geesmv)

# Example using geesmv
mvgee_model <- mvgee(formula = y ~ x1 + x2, id = id_variable, data = data, corstr = "exchangeable", robust = "Huber-White")

summary(mvgee_model)