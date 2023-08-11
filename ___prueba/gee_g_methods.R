FormatData(dplyr::filter(ms_d_match_surv, id %in% c(1:50)),
            idvar="id",
            timevar="trans",
            An= "tipo_de_plan_res",
            
gestMultiple(dplyr::filter(ms_d_match_surv, id %in% c(1:50)), 
             idvar= "id", timevar="trans", Yn="Tstop", An="tipo_de_plan_res", 
             Cn=NULL, #es un vecto con 0 o 1, para indicar censura. Debiese ponerla?
             outcomemodels= , #ist of formulas specifying the outcome models for Yn that includes all the confounders. See notes below on how best to specify these models.
             propensitymodel= ,  #A formula or formula object specifying the propensity score model for An.
             censoringmodel= ,  #A formula or formula object specifying the censoring model for Cn.
             type= "",  #De 1-4, que corresponde al distinto tipo de SNMM en la sección 2. Si tiene 2 o 4, es porque hay modificvador del efcto
             EfmVar, # name of the effect modifying variable for types 2 or 4 
             cutoff= "" #sólo para estimación g con variables tiempo-dependientes, Entero de 1 a T para cortar el tiempo hasta ahí
             )

library(readxl)
prueba_g_methods <- read_excel("C:/Users/Usuario/Desktop/prueba_g_methods.xlsx")
View(prueba_g_methods)

gformula_survival(
  # outcome_type = ’survival’,
  prueba_g_methods,
  id="id", #nombre de la variable ID
  time_points = max(prueba_g_methods$Tstop),#número máximo de registros por usuario
  time_name = "Tstop", #especifica el nombre de la variable tiempo, ¿será que buscan que sea "Tstop" o es outcome?
  covnames = c("tipo_de_plan_res", "TD", "days_treated"),
  c("binary","binary","zero-inflated normal"),
  covparams,
  covfits_custom = NA,
  covpredict_custom = NA,
  histvars = NULL,
  histories = NA,
  basecovs = NA,
  outcome_name,
  ymodel,
  compevent_name = NULL,
  compevent_model = NA,
  compevent_cens = FALSE,
  censor_name = NULL,
  censor_model = NA,
  intvars = NULL,
  interventions = NULL,
  int_times = NULL,
  int_descript = NULL,
  ref_int = 0,
  intcomp = NA,
  visitprocess = NA,
  restrictions = NA,
  yrestrictions = NA,
  compevent_restrictions = NA, #List of vectors. Each vector containins as its first entry a condition and its sec ond entry an integer
  baselags = FALSE, #convention used for lagi and lag_cumavgi terms in the model statements when pre-baseline times are not include. . If this argument is set to FALSE, the value of all lagi and lag_cumavgi terms in this context are set to 0 (for non-categorical covariates) or the reference level (for categorical covariates)
  nsimul = NA, #Number of subjects for whom to simulate data. By default, this argument is set equal to the number of subjects in obs_data.
  sim_data_b = FALSE, #o return the simulated  data set. If bootstrap samples are used (i.e., nsamples is set to a value greater than 0), this argument must be set to FALSE. The default is FALSE.
  seed= 2125,
  nsamples = 10, #subir
  parallel = T,
  ncores = parallel::detectCores() -1,
  ci_method = "percentile", #method for calculating the bootstrap 95% CIs, if applicable. "percentile" and "normal".
  threads,
  model_fits = FALSE, #
  boot_diag = T, # whether to return the coefficients, standard errors, and variance-covariance matrices of the parameters of the fitted models in the boot strap samples
  show_progress = TRUE,
  ipw_cutoff_quantile = NULL,
  ipw_cutoff_value = NULL
)

install.packages("geecure")

library(geecure)

geetonsilexch <- geecure2(Surv(time, status) ~ tipo_de_plan_res+ TD + arrival_cen + T,
                          cureform = ~ tipo_de_plan_res+ TD + arrival_cen + T, data = ms_d_match_surv,
                          id = ms_d_match_surv$id, corstr = "exchangeable", stdz = TRUE, Var = T, boots=T, nboot=1e3)
#When corstr = exchangeable, stdz = TRUE and boots = TRUE, the model parameters are estimated by the 
#expectation-solution (ES) algorithm and the standard error estimates are obtained from bootstrap variance formula based on and Niu et al. (2013).
