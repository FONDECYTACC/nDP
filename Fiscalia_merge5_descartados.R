# IPW

We calculated the restricted mean survival time and lost, considering that proportionality of trends could be violated.

::: controlly 
```{r rmst-adj0, echo=T, fig.align='center', message=T, error=T, eval=T}
invisible("Discard missing values in cause of discharge")
Base_fiscalia_v14_mod<-Base_fiscalia_v14[which(!is.na(Base_fiscalia_v14$motivodeegreso_mod_imp_rec))]
# Base_fiscalia_v14$clas_r <- relevel(factor(Base_fiscalia_v14$Clasificación), ref = "Urbana")
# Base_fiscalia_v14_pris$clas_r <- relevel(factor(Base_fiscalia_v14$Clasificación), ref = "Urbana")
# Base_fiscalia_v14$via_adm_sus_prin_act <- relevel(factor(Base_fiscalia_v14$via_adm_sus_prin_act), ref = "Oral (drunk or eaten)")
# Base_fiscalia_v14_pris$via_adm_sus_prin_act <- relevel(factor(Base_fiscalia_v14_pris$via_adm_sus_prin_act), ref = "Oral (drunk or eaten)")
# 
# Base_fiscalia_v14$yrs_to_condemn<- Base_fiscalia_v14$age_offending_imp - Base_fiscalia_v14$edad_al_egres_imp
# Base_fiscalia_v14_pris$yrs_to_pris<- Base_fiscalia_v14_pris$age_offending_imp - Base_fiscalia_v14_pris$edad_al_egres_imp


set.seed(2125)
W2 <- weightit(as.formula(paste("motivodeegreso_mod_imp_rec~", paste(vars_cov, collapse = "+"))),
               data=Base_fiscalia_v14[which(!is.na(Base_fiscalia_v14$motivodeegreso_mod_imp_rec))], stabilize = T, method = "ps", estimand = "ATE", int=T, use.mlogit = FALSE)
invisible("Warning: Missing values are present in the covariates. See ?WeightIt::method_ps for information on how these are handled.")
invisible("Warning message:
Some extreme weights were generated. Examine them with summary() and maybe trim them with trim().")
Base_fiscalia_v14_mod$weight_mult_a1 <-W2$weights
Base_fiscalia_v14_mod$weight_mult_a2 <-trim(W2$weights,.01)
Base_fiscalia_v14_mod$weight_mult_a3 <-trim(W2$weights,.05)
Base_fiscalia_v14_mod$weight_mult_a4 <-trim(W2$weights,.1)
summary(W2)

#Error: The treatment must have at least two unique values.
#Error in set(x, j = name, value = value) : 
#  Supplied 27185 items to be assigned to 70863 items of column 'weight_mult_a00'. If you wish to 'recycle' the RHS please use rep() to make this intent clear to readers of your code.

#truncate weights at 1%
Base_fiscalia_v14_mod$weight_tr <- ifelse(Base_fiscalia_v14_mod$weight_mult_a1 < quantile(Base_fiscalia_v14_mod$weight_mult_a1, probs=.01), quantile(Base_fiscalia_v14_mod$weight_mult_a1, probs=.01), Base_fiscalia_v14_mod$weight_mult_a1)
Base_fiscalia_v14_mod$weight_tr <- ifelse(Base_fiscalia_v14_mod$weight_mult_a1 > quantile(Base_fiscalia_v14_mod$weight_mult_a1, probs=.99), quantile(Base_fiscalia_v14_mod$weight_mult_a1, probs=.99), Base_fiscalia_v14_mod$weight_tr)

#truncate weights at 5%
Base_fiscalia_v14_mod$weight_tr2 <- ifelse(Base_fiscalia_v14_mod$weight_mult_a1 < quantile(Base_fiscalia_v14_mod$weight_mult_a1, probs=.05), quantile(Base_fiscalia_v14_mod$weight_mult_a1, probs=.05), Base_fiscalia_v14_mod$weight_mult_a1)
Base_fiscalia_v14_mod$weight_tr2 <- ifelse(Base_fiscalia_v14_mod$weight_mult_a1 > quantile(Base_fiscalia_v14_mod$weight_mult_a1, probs=.95), quantile(Base_fiscalia_v14_mod$weight_mult_a1, probs=.95), Base_fiscalia_v14_mod$weight_tr2)

#truncate weights at 10%
Base_fiscalia_v14_mod$weight_tr3 <- ifelse(Base_fiscalia_v14_mod$weight_mult_a1 < quantile(Base_fiscalia_v14_mod$weight_mult_a1, probs=.1), quantile(Base_fiscalia_v14_mod$weight_mult_a1, probs=.1), Base_fiscalia_v14_mod$weight_mult_a1)
Base_fiscalia_v14_mod$weight_tr3 <- ifelse(Base_fiscalia_v14_mod$weight_mult_a1 >
                                             quantile(Base_fiscalia_v14_mod$weight_mult_a1, probs=.9), quantile(Base_fiscalia_v14_mod$weight_mult_a1, probs=.9), Base_fiscalia_v14_mod$weight_tr3)

# AKM RMST adjusted for age
source("https://raw.githubusercontent.com/s-conner/akm-rmst/master/AKM_rmst.R")


cat("===============================================================================")

cat("at 3 months (adjusted), not trimmed weights")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_mult_a1, tau=.33)

cat("At 1 year (adjusted), not trimmed weights")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_mult_a1, tau=1)

cat("At 3 years (adjusted), not trimmed weights")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_mult_a1, tau=3)

cat("At 5 years (adjusted), not trimmed weights")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_mult_a1, tau=5)


cat("===============================================================================")

cat("at 3 months (adjusted), weights trimmed by 1%")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_mult_a2, tau=.33)

cat("At 1 year (adjusted), weights trimmed by 1%")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_mult_a2, tau=1)

cat("At 3 years (adjusted), weights trimmed by 1%")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_mult_a2, tau=3)

cat("At 5 years (adjusted), weights trimmed by 1%")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_mult_a2, tau=5)


cat("===============================================================================")

cat("at 3 months (adjusted), weights trimmed by 5%")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_mult_a3, tau=.33)

cat("At 1 year (adjusted), weights trimmed by 5%")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_mult_a3, tau=1)

cat("At 3 years (adjusted), weights trimmed by 5%")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_mult_a3, tau=3)

cat("At 5 years (adjusted), weights trimmed by 5%")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_mult_a3, tau=5)


cat("===============================================================================")

cat("at 3 months (adjusted), weights trimmed by 10%")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_mult_a4, tau=.33)

cat("At 1 year (adjusted), weights trimmed by 10%")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_mult_a4, tau=1)

cat("At 3 years (adjusted), weights trimmed by 10%")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_mult_a4, tau=3)

cat("At 5 years (adjusted), weights trimmed by 10%")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_mult_a4, tau=5)

cat("===============================================================================")

cat("===============================================================================")

cat("at 3 months (adjusted), not stabilized weights")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_tr, tau=.33)

cat("At 1 year (adjusted), not stabilized weights")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_tr, tau=1)

cat("At 3 years (adjusted), not stabilized weights")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_tr, tau=3)

cat("At 5 years (adjusted), not stabilized weights")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_tr, tau=5)

cat("===============================================================================")

cat("at 3 months (adjusted), weights stabilized by 1%")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_tr2, tau=.33)

cat("At 1 year (adjusted), weights stabilized by 1%")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_tr2, tau=1)

cat("At 3 years (adjusted), weights stabilized by 1%")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_tr2, tau=3)

cat("At 5 years (adjusted), weights stabilized by 1%")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_tr2, tau=5)

cat("===============================================================================")

cat("at 3 months (adjusted), weights stabilized by 5%")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_tr3, tau=.33)

cat("At 1 year (adjusted), weights stabilized by 5%")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_tr3, tau=1)

cat("At 3 years (adjusted), weights stabilized by 5%")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_tr3, tau=3)

cat("At 5 years (adjusted), weights stabilized by 5%")
akm_rmst(time=Base_fiscalia_v14_mod$yrs_to_condemn, status=Base_fiscalia_v14_mod$event_r, group= Base_fiscalia_v14_mod$motivodeegreso_mod_imp_rec, weight=Base_fiscalia_v14_mod$weight_tr3, tau=5)

```

We used the kaplan-meier curves with survey-weighting, according to a commentary made by Thomas Lumley.

::: controlly
```{r adj-km, echo=T, fig.align='center', message=T, error=T, eval=T}
#Rader, Kevin Andrew. 2014. Methods for Analyzing Survival and Binary Data in Complex Surveys. Doctoral dissertation, Harvard University.http://nrs.harvard.edu/urn-3:HUL.InstRepos:12274283

#load(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/__analisis_joel.RData"))
library(survey)
# survey::svylogrank
#First, you can use the survey::svylogrank function, as @IRTFM suggests. This will treat the weights as sampling weights, but I think that's ok with the robust standard errors that svylogrank uses.

design_weights <- survey::svydesign(ids = ~1, data = dplyr::mutate(Base_fiscalia_v14_mod, yrs_to_condemn= dplyr::case_when(yrs_to_condemn==0~0.0001,T~yrs_to_condemn)), weights = ~weight_mult_a1)
# Error in .logrank(formula, design, rho, gamma, ...) : 


survey::svykm(Surv(time=yrs_to_condemn,event=event_r)~motivodeegreso_mod_imp_rec,
              #con_quien_vive_joel, 
              design=design_weights, rho=0, gamma=0)

system.time({
  s1<- survey::svylogrank(Surv(time=yrs_to_condemn,event=event_r)~con_quien_vive_joel_alone,
                          #con_quien_vive_joel, 
                          design=design_weights, rho=0, gamma=0) #tslow , se=T
  #primero es familia de origen vs. el resto
  #alone vs. el resto da -41.77638 60.81932 -0.6868932 0.49215
  #couple/children vs. el resto da .003 
})

system.time({
  s2<- survey::svylogrank(Surv(time=yrs_to_condemn,event=event_r)~con_quien_vive_joel_coup_chil,
                          #con_quien_vive_joel, 
                          design=design_weights, rho=0, gamma=0) #tslow , se=T
  #primero es familia de origen vs. el resto
  #alone vs. el resto da -41.77638 60.81932 -0.6868932 0.49215
  #couple/children vs. el resto da .003 
})

system.time({
  s3<- survey::svylogrank(Surv(time=yrs_to_condemn,event=event_r)~con_quien_vive_joel_fam_or,
                          #con_quien_vive_joel, 
                          design=design_weights, rho=0, gamma=0) #tslow , se=T
  #primero es familia de origen vs. el resto
  #alone vs. el resto da -41.77638 60.81932 -0.6868932 0.49215
  #couple/children vs. el resto da .003 
})

system.time({
  s4<- survey::svylogrank(Surv(time=yrs_to_condemn,event=event_r)~con_quien_vive_joel,
                          #con_quien_vive_joel, 
                          design=design_weights, rho=0, gamma=0) #tslow , se=T
  #primero es familia de origen vs. el resto
  #alone vs. el resto da -41.77638 60.81932 -0.6868932 0.49215
  #couple/children vs. el resto da .003 
})

cat("Alone (1) vs. With couple/children and Family of origin")
s1
cat("With couple/children(1) vs. Alone and Family of origin")
s2
cat("Family of origin(1) vs. Alone and With couple/children")
s3
cat("Every level")
s4

#svykm -->
# Error: cannot allocate vector of size 3.1 Gb
# Timing stopped at: 6913 368.1 7453

```
:::
  
  
  We tested weights that account for a multinomial response using Propensity score weighting using Bayesian additive regression trees (BART) (`weight_mult_a0`), multinomial regression (`weight_mult_a`), and Generalized Boosted Regression Modeling with multinomial response on the average effect on the treated or ATE (`weight_mult_b`),  weights with multiple treatments or ATO (Li, & Li, 2019) (`weight_mult_c`) and weights to simultaneously compare three treatment groups or ATM (Yoshida et al., 2007) (`weight_mult_d`).


```{r alt-an1, echo=T, fig.align='center', message=T, error=T, eval=T}
#load(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/__analisis_joel.RData"))
prueba2_imp22<-prueba2_imp2 %>% dplyr::mutate(con_quien_vive_joel= factor(con_quien_vive_joel))#, numero_de_hijos_mod_joel= as.ordered(numero_de_hijos_mod_joel)) %>% dplyr::mutate(freq_cons_sus_prin=ordered(freq_cons_sus_prin,levels=c("Did not use", "Less than 1 day a week", "1 day a week or more", "2 to 3 days a week","4 to 6 days a week", "Daily")))
#A multi-category example using GBM predicted probabilities
library(caret)

#Here, we'll set multinomial distribution, 10 cross-validation fold, and 200 trees.
#learning rate (shrinkage) of 0.001. This is a very small learning rate and typically requires a large number of trees to find the minimum MSE. 
#find the better model
#https://www.webpages.uidaho.edu/~stevel/517/Gradient%20Boosting%20Machines%20at%20UC.html
set.seed(2125)
W1 <- weightit(as.formula(paste("con_quien_vive_joel~", paste(vars, collapse = "+"))),
               data=prueba2_imp22, stabilize = T,
               method = "bart", estimand = "ATE", int=T,
               use.mlogit = FALSE)
prueba2_imp22$weight_mult_a0 <-W1$weights

set.seed(2125)
mod_gbm = gbm::gbm(as.formula(paste("con_quien_vive_joel~", paste(vars, collapse = "+"))),
                   data = prueba2_imp22,
                   distribution = "multinomial",
                   cv.folds = 10,
                   shrinkage = .001,
                   n.minobsinnode = 10,
                   n.trees = 5e3)
#gbm.perf(mod_gbm, method = "cv") weightit.fit

ps.multi <- drop(predict(mod_gbm, type = "response", n.trees = 5e3))
#https://ngreifer.github.io/WeightIt/reference/method_ps.html
#https://search.r-project.org/CRAN/refmans/WeightIt/html/get_w_from_ps.html
prueba2_imp22$weight_mult_b <- WeightIt::get_w_from_ps(ps.multi, prueba2_imp22$con_quien_vive_joel, estimand = "ATE", stabilize=T)
prueba2_imp22$weight_mult_c <- WeightIt::get_w_from_ps(ps.multi, prueba2_imp22$con_quien_vive_joel, estimand = "ATO", stabilize=T)
prueba2_imp22$weight_mult_d <- WeightIt::get_w_from_ps(ps.multi, prueba2_imp22$con_quien_vive_joel, estimand = "ATM", stabilize=T)

# Multinomial Treatments
# 
# - estimand = "ATO"
# Li, F., & Li, F. (2019). Propensity score weighting for causal inference with multiple treatments. The Annals of Applied Statistics, 13(4), 2389<U+2013>2415. 10.1214/19-AOAS1282
# 
# - estimand = "ATM"

```


We assess the covariates' balance after the weighting

::: controlly 
```{r post-rmst-alt-an, echo=T, fig.align='center', message=T, error=T, eval=T}
bal_tab2<-
cobalt::bal.tab(event ~ con_quien_vive_joel+ edad_al_ing+ sexo_2 + escolaridad_rec + sus_ini_mod_mvv + freq_cons_sus_prin + via_adm_sus_prin_act + condicion_ocupacional_corr+ compromiso_biopsicosocial + numero_de_hijos_mod_joel + tipo_de_plan_2_mod+ tenencia_de_la_vivienda_mod + tipo_centro+ cnt_mod_cie_10_dg_cons_sus_or + sus_principal_mod+ macrozona + cnt_mod_cie_10_or, data = prueba2_imp22,
                weights = list("Multinomial Logistic, not stabilized" = "weight_mult_a00", "Multinomial Logistic, stabilized at 1%"="weight_tr", "Multinomial Logistic, stabilized at 5%"="weight_tr2", "Multinomial Logistic, stabilized at 10%"="weight_tr3", "Bayesian additive regression trees (BART)"="weight_mult_a0", "Propensity Scores (WeightIt)"="weight_mult_a", "Generalized Boosted Regression Modeling with multinomial response, ATE"="weight_mult_b", "Generalized Boosted Regression Modeling with multinomial response, ATO"="weight_mult_c", "Generalized Boosted Regression Modeling with multinomial response, ATM"="weight_mult_d"),
                estimand = "ATE",
                which.treat = .all,
                un = T, 
                thresholds = c(cor = .2),
                 binary = "std", continuous = "std",
                stats = c("mean.diffs", "variance.ratios"))

bal_tab2$Balance %>% 
  dplyr::mutate_if(is.numeric, ~round(.,2)) %>% 
  knitr::kable("markdown", caption="Balance Measures (multinomial response weights)")
  # knitr::kable("html", caption="Balance Measures") %>% 
  #      kableExtra::kable_classic()
```
:::

::: controlly 
```{r post-rmst2-alt-an, echo=T, fig.align='center', message=T, error=T, eval=T}
bal_tab2$Observations %>% 
  #dplyr::mutate_if(is.numeric, ~round(.,2)) %>% 
  knitr::kable("markdown", caption="Effective sample sizes")
```
:::

The model with multinomial logistic regression (`weight_mult_a`) performed better in terms of balance of the variables. We estimate the restricted survival times using these weights. 

::: controlly 
```{r rmst-adj-tr3-alt-an, echo=T, fig.align='center', message=T, error=T, eval=T}


#(3) (PDF) Adjusted restricted mean survival times in observational studies. Available from: https://www.researchgate.net/publication/333326572_Adjusted_restricted_mean_survival_times_in_observational_studies [accessed Feb 02 2023].


akm_rmst(time=prueba2_imp22$yrs_to_tr_dropout, status=prueba2_imp22$event, group=prueba2_imp22$con_quien_vive_joel,weight=prueba2_imp22$weight_mult_a, tau=.33)

akm_rmst(time=prueba2_imp22$yrs_to_tr_dropout, status=prueba2_imp22$event, group=prueba2_imp22$con_quien_vive_joel,weight=prueba2_imp22$weight_mult_a, tau=1)

akm_rmst(time=prueba2_imp22$yrs_to_tr_dropout, status=prueba2_imp22$event, group=prueba2_imp22$con_quien_vive_joel,weight=prueba2_imp22$weight_mult_a, tau=3)

```
:::

- Who lives with his/her couple and children have lower mean time without experiencing dropout in comparison with vs. Family of origin and alone, meaning that experience it more quickly.

<br>

# Alternative Analysis

## Flexsurvreg


::: controlly

```{r miss, warning=FALSE, echo=T, error=T, eval=T}
#https://cran.r-project.org/web/packages/flexsurv/vignettes/flexsurv.pdf
#https://search.r-project.org/CRAN/refmans/flexsurv/html/standsurv.html

fiform<-
as.formula(paste("Surv(yrs_to_pris,  event_r) ~ motivodeegreso_mod_imp_rec + rcs(edad_al_ing_1,4) +", paste(setdiff(vars_cov,"edad_al_ing_1"), collapse = "+")))


dists_w_covs<-cbind.data.frame(covs=c(rep("fits_",(20)*1)), formal= rep(c("Weibull (AFT)", "Weibull (PH)", "Gompertz", "Log-logistic", "Gamma", "Generalized gamma", "Generalized gamma (original)", "Lognormal", "Exponential", "Generalized F", "Generalized F(original)", paste0("RP0",2:9),"RP10"),1*1), dist=c("weibull", "weibullph", "gompertz", "llogis", "gamma", "gengamma","gengamma.orig", "lnorm", "exp", "genf","genf.orig",rep("no dist",9)), model=rep(c("wei", "weiph", "gomp", "llogis", "gam","ggam", "ggam_orig", "logn", "exp", "genf","genf_orig", paste0("rp0",2:9),"rp10"),1*1), number= c(rep(0,11),2:10))

no_attempts <- 5
list_flexsurvreg <- list()

for (i in 1:length(dists_w_covs$formal)){
 if(grepl("RP", dists_w_covs$formal[i])==FALSE){
    r <- NULL
    attempt <- 0
    while( is.null(r) && attempt <= no_attempts ) {
      attempt <- attempt + 1
      try(
          r <- flexsurvreg(formula= as.formula(paste("Surv(yrs_to_condemn,  event_r) ~ motivodeegreso_mod_imp_rec + rcs(edad_al_ing_1,4) +", paste(setdiff(vars_cov,"edad_al_ing_1"), collapse = "+"))),
                               data = Base_fiscalia_v14,
                               dist = "gengamma.orig")
      )
    }
    paste0("Model ",dists_w_covs$formal[i])
    list_flexsurvreg[[i]] <- r
 } else {
    r <- NULL
    attempt <- 0
    while( is.null(r) && attempt <= no_attempts ) {
      attempt <- attempt + 1
      try(
        r <- flexsurvspline(formula= as.formula(paste("Surv(yrs_to_condemn,  event_r) ~ motivodeegreso_mod_imp_rec + rcs(edad_al_ing_1,4) +", paste(setdiff(vars_cov,"edad_al_ing_1"), collapse = "+"))), 
                            data = Base_fiscalia_v14,
                            k= dists_w_covs$number[i])
      )
    }
    paste0("Model ",dists_w_covs$formal[i])
    list_flexsurvreg[[i]] <- r
 }
}
#APR 2022, had to center the variable to avoid convergence issues
Base_fiscalia_v14$edad_al_ing_1_cen<-as.numeric(scale(Base_fiscalia_v14$edad_al_ing_1, scale = F))
Base_fiscalia_v14$ano_nac_corr_cen<-as.numeric(scale(Base_fiscalia_v14$ano_nac_corr, scale = F))
Base_fiscalia_v14$porc_pobr_cen<-as.numeric(scale(Base_fiscalia_v14$porc_pobr, scale = F))

if(is.ordered(Base_fiscalia_v14$fis_comorbidity_icd_10)){ Base_fiscalia_v14$fis_comorbidity_icd_10_chr <- as.character(Base_fiscalia_v14$fis_comorbidity_icd_10)}
if(is.ordered(Base_fiscalia_v14$freq_cons_sus_prin)){ Base_fiscalia_v14$freq_cons_sus_prin_chr <- as.character(Base_fiscalia_v14$freq_cons_sus_prin)}
if(is.ordered(Base_fiscalia_v14$escolaridad_rec)){ Base_fiscalia_v14$escolaridad_rec_chr <- as.character(Base_fiscalia_v14$escolaridad_rec)}

#m_nostag_rp6_tvc_1
rp06 <- flexsurvspline(formula= as.formula(paste("Surv(yrs_to_condemn,  event_r) ~ motivodeegreso_mod_imp_rec + rcs(edad_al_ing_1_cen,4) + ano_nac_corr_cen+ porc_pobr_cen+ fis_comorbidity_icd_10_chr+ freq_cons_sus_prin_chr+ escolaridad_rec_chr+", paste(setdiff(vars_cov,c("edad_al_ing_1", "ano_nac_corr", "porc_pobr", "fis_comorbidity_icd_10", "freq_cons_sus_prin", "escolaridad_rec")), collapse = "+"))), data = Base_fiscalia_v14, k= 6, control=list(reltol=1e-16,maxit = 1e5),method="Nelder-Mead") #reltol=1e-16),fnscale = 400, 
#Choice of algorithm (CG, nlm, L-BFGS-B, Nelder-Mead)
#-2 * -59529.74

invisible("https://www.theanalysisfactor.com/wacky-hessian-matrix/")
invisible("https://stackoverflow.com/a/58468209/9975513")
invisible("https://github.com/chjackson/flexsurv-dev/issues/67")
invisible("https://github.com/chjackson/flexsurv-dev/issues/54")
invisible("https://github.com/chjackson/flexsurv-dev/issues/74")
# There isn't necessarily going to be a solution. The likelihood surface that it is trying to maximise might just be too flat, so that there is no meaningful maximum. This usually happens when the data don't contain sufficient information about the parameters of the model.
# BFGS is used as a default because it makes use of the gradient of the log-likelihood, which makes optimisation faster. In well-behaved problems, all optimisers should give the same answer. If they don't, then the answer with the higher likelihood (lower minus log likelihood) should be preferred.
# Go to help(optim) as the manual page suggests - see "reltol" and "abstol". For example, flexsurvreg(..., control=list(reltol=1e-16), though this might not make any difference.


#debugonce(flexsurvspline)

# km.lc[[i]] <- survfit(formula= , data = Base_fiscalia_v14)

# newdat_a_sc<-mutate(newdat_a,arrival=scale(1096- attr(scale(ms_d_match_surv_exp$arrival,scale=F),"scaled:center"),scale=F))
# newdat_b_sc<-mutate(newdat_b,arrival=scale(1096- attr(scale(ms_d_match_surv_exp$arrival,scale=F),"scaled:center"),scale=F))
# 
# newdat_c_sc<-mutate(newdat_c,arrival=scale(731- attr(scale(ms_d_match_surv_oct_2022_exp$arrival,scale=F),"scaled:center"),scale=F))
# newdat_d_sc<-mutate(newdat_d,arrival=scale(731- attr(scale(ms_d_match_surv_oct_2022_exp$arrival,scale=F),"scaled:center"),scale=F))


```
:::