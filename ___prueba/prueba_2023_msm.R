
load("base_mortalidad_tmle.RData")

#https://docs.google.com/document/d/1mz70nuKHlANRLf0dG0Mkfy7iwQ-g_wbl/edit?pli=1
require(tidyverse)
CONS_C1_df_dup_SEP_2020_bd2012_mod<-
CONS_C1_df_dup_SEP_2020_bd2012 %>% 
  dplyr::group_by(run_minsal) %>% 
  dplyr::slice_min(fech_ing) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(mort=if_else(fech_falles>fech_egres_imp,1,0,0)) %>% 
  dplyr::mutate(tr_comp=if_else(motivodeegreso_mod_imp_1=="Alta Terapéutica",1,0,0)) %>% 
  dplyr::mutate(fech_falles2=ifelse(mort==0, as.Date("2023-08-18"),fech_falles)) %>% 
  dplyr::mutate(cut_tr= cut(dias_treat_imp_sin_na, 10))%>% 
  dplyr::mutate(cut_tr2= cut(dias_treat_imp_sin_na, 20))

# simCausalSurvival ------------------------------------------------------------------
#https://github.com/bblette1/Sim-Causal-Survival/blob/master/simCausalSurvival.R
#Function to simulate data (a

###         ID : subject identifier variable
###      Delta : indica evento
###          A : tratamiento tiempo dependiente
###          L : confusor tiempo-dependiente
### Delta_prev : evento por el tiempo previo: indicator of event by previous time point
###     A_prev : tratamiento en el tiempo previo: treatment at previous time point
###     L_prev : confusor en el tiempo preview: confounder at previous time point
###       Time : TIEMPO DEL EVENTO
simCausalSurvival <- function(n = 1000, tpoints = 10, psi = 0.3,
                              lambda = 0.05, gamma0 = 0, gamma1 = 0.5,
                              gamma2 = 0.2, gamma3 = 0.3, alpha0 = 0.1,
                              alpha1 = 0.3, alpha2 = 0.5, alpha3 = 0.2) {
  #######################################################################
  
  # Simulate potential time under no exposure
  T_0 <- rexp(n, lambda)
  
  # Initialize confounder, treatment, and outcome arrays
  L <- array(NA, dim = c(n, tpoints))
  A <- array(NA, dim = c(n, tpoints))
  Y <- array(NA, dim = c(n, tpoints))
  L_prev <- array(NA, dim = c(n, tpoints))
  A_prev <- array(NA, dim = c(n, tpoints))
  Y_prev <- array(NA, dim = c(n, tpoints))
  check_times <- array(NA, dim = c(n, tpoints))
  Times <- array(NA, dim = c(n, tpoints))
  
  # Do first measurements separately
  L_prev[, 1] <- rep(0, n)
  A_prev[, 1] <- rep(0, n)
  Y_prev[, 1] <- rep(0, n)
  
  L_logit <- gamma0 + gamma1*(T_0 < 20)
  L_probs <- exp(L_logit) / (1 + exp(L_logit))
  L[, 1] <- rbinom(n, 1, L_probs)
  
  A_logit <- alpha0 + alpha1*L[, 1]
  A_probs <- exp(A_logit) / (1 + exp(A_logit))
  A[, 1] <- rbinom(n, 1, A_probs)
  
  check_times[, 1] <- exp(psi*A[, 1])
  Y[, 1] <- as.numeric(T_0 <= check_times[, 1])
  Times[T_0 <= check_times[, 1], 1] <- 
    T_0[T_0 <= check_times[, 1]]*exp(-psi*A[T_0 <= check_times[, 1], 1])
  
  
  # Now generate second through m-th measurements in a loop
  for (j in 2:tpoints) {
    
    Y_prev[, j] <- Y[, (j - 1)]
    
    L_logit <- gamma0 + gamma1*(T_0 < 20) + gamma2*A[, (j - 1)] +
      gamma3*L[, (j - 1)]
    L_probs <- exp(L_logit) / (1 + exp(L_logit))
    L[, j] <- rbinom(n, 1, L_probs)
    
    A_logit <- alpha0 + alpha1*L[, j] + alpha2*L[, (j - 1)] +
      alpha3*A[, (j - 1)]
    A_probs <- exp(A_logit) / (1 + exp(A_logit))
    A[, j] <- rbinom(n, 1, A_probs)
    
    L[, j][Y_prev[, j] == 1] <- 0
    A[, j][Y_prev[, j] == 1] <- 0
    
    check_times[, j] <- check_times[, (j - 1)] + exp(psi*A[, j])
    
    Y[, j] <- as.numeric(T_0 <= check_times[, j])
    
    group1 <- (Y[, j] == 1 & Y_prev[, j] == 1)
    group2 <- (Y[, j] == 1 & Y_prev[, j] == 0)
    
    Times[, j][group1] <-
      Times[, (j - 1)][group1]
    
    Times[, j][group2] <-
      (j - 1 + ((T_0[group2] - check_times[, (j-1)][group2])*
                  exp(-psi*A[, j][group2])))
    
    L_prev[, j] <- L[, (j - 1)] 
    A_prev[, j] <- A[, (j - 1)] 
    
  }
  
  # Make data set
  ID <- rep(1:n, each = tpoints)
  data <- data.frame(ID, c(t(Y)), c(t(A)), c(t(L)), c(t(Y_prev)),
                     c(t(A_prev)), c(t(L_prev)), c(t(Times)))
  colnames(data) <- c("ID", "Delta", "A", "L", "Delta_prev", "A_prev",
                      "L_prev", "Time")
  
  return(data)
  
}


# ID Delta A L Delta_prev A_prev L_prev      Time
# 1    1     0 1 0          0      0      0        NA
# 2    1     0 1 0          0      1      0        NA
# 3    1     0 0 0          0      1      0        NA
# 4    1     0 0 0          0      0      0        NA
# 5    1     1 0 1          0      0      0 4.8636314
# 6    1     1 0 0          1      0      1 4.8636314
# 7    1     1 0 0          1      0      0 4.8636314
# 8    1     1 0 0          1      0      0 4.8636314
# 9    1     1 0 0          1      0      0 4.8636314
# 10   1     1 0 0          1      0      0 4.8636314
# 11   2     0 1 1          0      0      0        NA
# 12   2     1 1 1          0      1      1 1.2863488
# 13   2     1 0 0          1      1      1 1.2863488
# 14   2     1 0 0          1      0      0 1.2863488
# 15   2     1 0 0          1      0      0 1.2863488
# 16   2     1 0 0          1      0      0 1.2863488
# 17   2     1 0 0          1      0      0 1.2863488
# 18   2     1 0 0          1      0      0 1.2863488
# 19   2     1 0 0          1      0      0 1.2863488
# 20   2     1 0 0          1      0      0 1.2863488
# 21   3     0 0 1          0      0      0        NA
# 22   3     0 1 0          0      0      1        NA
# 23   3     0 1 1          0      1      0        NA
# 24   3     0 1 1          0      1      1        NA

# Keil ------------------------------------------------------------------

library(readxl)
person_level_Keil_2015 <- read_excel("___prueba/person_level Keil 2015.xlsx")

person_level_Keil_2015_2 <-
person_level_Keil_2015 %>% 
  dplyr::mutate(wait = waitdays/30.5) %>% 
  dplyr::mutate(agesq = age^2) #%>% 
  #dplyr::mutate(agecurs1 = age^2) %>% 
# *restricted cubic spline on age (knots at 17, 25.4, 30, 41.4);
# agecurs1 = (age>17.0)*(age-17.0)**3-((age>30.0)*(age-30.0)**3)*(41.4-17.0)/(41.4-30.0);
# agecurs2 = (age>25.4)*(age-25.4)**3-((age>41.4)*(age-41.4)**3)*(41.4-25.4)/(41.4-30.0);

# DAGs ------------------------------------------------------------------
pacman::p_load(devtools, here, showtext, ggpattern, RefManageR, 
               pagedown, magick, bibtex, DiagrammeR, xaringan, 
               xaringanExtra, xaringanthemer, fontawesome, 
               widgetframe, datapasta, tidyverse, psych, 
               cowplot, coxphw, future, timereg, flexsurv, 
               mstate, compareGroups, dagitty, ggdag, geepack, 
               survey, ipw, reshape, geeM,  confoundr, install=F) # MuMIn,

#out.height=450, 
dag34 <- dagitty::dagitty('dag {
bb="0,0,1,1"
L1_c1 [pos="0.117,0.205"]
L2_c2 [pos="0.222,0.112"]
dt1 [outcome,pos="0.294,0.491"]
dt2 [outcome,pos="0.572,0.491"]
dt3 [outcome,pos="0.838,0.491"]
t1 [exposure,pos="0.092,0.491"]
t2 [exposure,pos="0.380,0.491"]
t3 [exposure,pos="0.657,0.491"]
v1 [pos="0.289,0.686"]
v2 [pos="0.576,0.668"]
v3 [pos="0.849,0.657"]
L1_c1 -> dt1
L1_c1 -> dt2
L1_c1 -> dt3
L1_c1 -> t1
L1_c1 -> t2
L1_c1 -> t3
L2_c2 -> dt1
L2_c2 -> dt2
L2_c2 -> dt3
L2_c2 -> t1
L2_c2 -> t2
L2_c2 -> t3
t1 -> dt1
t1 -> t2 [pos="0.210,0.652"]
t2 -> dt2
t2 -> t3 [pos="0.514,0.677"]
t3 -> dt3
v1 -> dt1
v1 -> t1
v1 -> t2
v2 -> dt2
v2 -> t2
v2 -> t3
v3 -> dt3
v3 -> t3
dt1 -> dt2
dt2 -> dt3
}')

# tidy_dag34 <- tidy_dagitty(dag34) %>% 
  dplyr::mutate(label=dplyr::case_when(grepl("A0",as.character(name))~"A0",
                                       T~as.character(name))) %>% 
  dplyr::mutate(label2=dplyr::case_when(grepl("L|v",name)~"adj",grepl("LM",name)~"white",
                                        T~"black")) %>% 
  dplyr::mutate(adjusted=factor(dplyr::case_when(grepl("L|v",name)~"adjusted",T~"unadjusted")))

edge_function <- ggdag:::edge_type_switch("link_arc")
dag34_plot<-
  ggdag:::if_not_tidy_daggity(tidy_dag34) %>% ggdag:::node_status() %>% 
  ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, 
                               yend = yend, color = status, shape=factor(adjusted)))+ 
  #edge_function()+
  scale_adjusted()+ ggdag:::breaks(c("exposure", "outcome","latent"))+
  theme_dag()+
  geom_dag_edges_arc(curvature = c(rep(0,12),5,5,0,5,0,5,rep(0,9)))+ #14 y 16 de 24
  ggdag:::geom_dag_point(size = 16)+
  ggdag:::geom_dag_label_repel(ggplot2::aes_string(label = "label", 
                                                   fill = "status"), size = 4.88, col = "white", 
                               show.legend = FALSE)+
  
  scale_shape_manual(values = c(15, 16), name="Ajustado", labels=c("Sí", "No"))+ 
  scale_fill_manual(values = c("gray80", "gray30","gray30"), name="Estatus",na.value="black", labels=c("Exposición", "Resultado"), limits = c('exposure', 'outcome'))+  
  scale_color_manual(values = c("gray75", "gray35","gray30"), name="Estatus",na.value="black", labels=c("Exposición", "Resultado"), limits = c('exposure', 'outcome'))+#E6E6E6
  guides(linetype="none", edge_alpha="none", shape="none")+
  guides(color=guide_legend(override.aes = list(arrow = NULL)))+#,guide_colourbar(order = 1)
  theme(plot.caption = element_text(hjust = 0))+
  theme(legend.position = "bottom", aspect.ratio=6/10)+
  labs(caption="Nota. Ak= Modalidad (Residencial/Ambulatoria); dtk= Meses libre de readmisión;\nvk= Confusor tiempo-dependiente; c1= Policonsumo; c2= Edad de inicio consumo de sustancias")

dag34_plot

# IVs ------------------------------------------------------------------

#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5994515/
require(AER)

#Instrumental variables are not a panacea for unmeasured confounding as they require certain assumptions about the data to properly establish causality


AER::ivreg(CONS_C1_df_dup_SEP_2020_bd2012_mod$mort ~ CONS_C1_df_dup_SEP_2020_bd2012_mod$tr_comp | CONS_C1_df_dup_SEP_2020_bd2012_mod$dias_treat_imp_sin_na) %>% summary(diagnostics=T)
#Weak instruments: falso
#Wu-Hausman, no significativo
#Sargan, NA

CONS_C1_df_dup_SEP_2020_bd2012_mod %>% 
  dplyr::mutate(tr_comp1=if_else(motivodeegreso_mod_imp_1=="Alta Terapéutica",1,0,0)) %>% 
  dplyr::mutate(tr_comp2=if_else(motivodeegreso_mod_imp_2=="Alta Terapéutica",1,0,0)) %>% 
  group_by(tr_comp1, tr_comp2, mort) %>% 
  dplyr::summarise(mean=mean(dias_treat_imp_sin_na, na.rm=T),
                   median= quantile(dias_treat_imp_sin_na, .5, na.rm=T),
                   p25= quantile(dias_treat_imp_sin_na, .25, na.rm=T),
                   p75= quantile(dias_treat_imp_sin_na, .75, na.rm=T),
                   mean2=mean(dias_treat_imp_sin_na_2, na.rm=T),
                   median2= quantile(dias_treat_imp_sin_na_2, .5, na.rm=T),
                   p252= quantile(dias_treat_imp_sin_na_2, .25, na.rm=T),
                   p752= quantile(dias_treat_imp_sin_na_2, .75, na.rm=T))

#Quienes mueren tienen en promedio más días de los que no en quienes no completan los dos primeros ttos.
#Quienes completan en el seguundo, tienen misma cantidad de días entre quienes mueren y no; el segundo tiene más días de tto quienes mueren
#Quienes completan sólo el primero, tienen en promedio días en tratamiento similar; en el segundo tienesn más días de tto. los que mueren
#Quienes compeltan ambos y mueren, tienen menos días en el primer tratamiento, pero igual dias en el segundo

coxph(Surv(as.numeric(fech_egres_imp), as.numeric(fech_falles), mort)~
        tr_comp*dias_treat_imp_sin_na, data=CONS_C1_df_dup_SEP_2020_bd2012_mod) %>% summary()

ggsurvplot(
  survfit(Surv(as.numeric(fech_egres_imp), as.numeric(fech_falles2), mort)~ cut, 
          data=CONS_C1_df_dup_SEP_2020_bd2012_mod %>% 
            dplyr::mutate(cut=cut(dias_treat_imp_sin_na,5)) %>% dplyr::filter(tr_comp==0)),
  conf.int=T, xlim = c(15000, 20000), ylim=c(.6,1))
ggsurvplot(
survfit(Surv(as.numeric(fech_egres_imp), as.numeric(fech_falles2), mort)~ cut, 
        data=CONS_C1_df_dup_SEP_2020_bd2012_mod %>% 
          dplyr::mutate(cut=cut(dias_treat_imp_sin_na,5)) %>% dplyr::filter(tr_comp==1)),
conf.int=T, xlim = c(15000, 20000), ylim=c(.6,1))


require(gesttools)
require(tidyverse)
dplyr::bind_rows(

  # [1] "(-1.89,189]""(189,378]""(378,567]""(567,756]""(756,945]"          
  # [6] "(945,1.13e+03]""(1.13e+03,1.32e+03]" "(1.32e+03,1.51e+03]" "(1.51e+03,1.7e+03]"  "(1.7e+03,1.89e+03]"                 10                   2                   1 
  # 
bind_rows(
  data.table::data.table(
  biostat3::survRate(Surv(as.numeric(fech_falles2)-as.numeric(fech_egres_imp), mort) ~ cut_tr, 
            data= subset(CONS_C1_df_dup_SEP_2020_bd2012_mod, tr_comp==0))  %>% 
            dplyr::mutate_at(c("rate", "lower", "upper"),~round(.*100000,2)), keep.rownames=T),
data.table::data.table(
  biostat3::survRate(Surv(as.numeric(fech_falles2)-as.numeric(fech_egres_imp), mort) ~ cut_tr, 
                     data= subset(CONS_C1_df_dup_SEP_2020_bd2012_mod, tr_comp==1))  %>% 
            dplyr::mutate_at(c("rate", "lower", "upper"),~round(.*100000,2)), keep.rownames = T)
)

#
bind_rows(
  data.table::data.table(
    biostat3::survRate(Surv(as.numeric(fech_falles2)-as.numeric(fech_egres_imp), mort) ~ cut_tr2, 
                       data= subset(CONS_C1_df_dup_SEP_2020_bd2012_mod, tr_comp==0))  %>% 
      dplyr::mutate_at(c("rate", "lower", "upper"),~round(.*100000,2)), keep.rownames=T) %>% 
    dplyr::slice(1:7),
  data.table::data.table(
    biostat3::survRate(Surv(as.numeric(fech_falles2)-as.numeric(fech_egres_imp), mort) ~ cut_tr2, 
                       data= subset(CONS_C1_df_dup_SEP_2020_bd2012_mod, tr_comp==1))  %>% 
      dplyr::mutate_at(c("rate", "lower", "upper"),~round(.*100000,2)), keep.rownames = T) %>% 
    dplyr::slice(1:7)
) %>% 
  tibble::add_column(comp=c(rep(0,7),rep(1,7))) %>% 
  pivot_wider(names_from=comp, values_from = c("tstop", "event", "rate", "lower", "upper")) %>% 
  dplyr::select(-rn) %>% 
  dplyr::select(cut_tr2, event_0, rate_0, lower_0, upper_0, 
                         event_1, rate_1, lower_1, upper_1)

#Despues de los 284 días en el primer tto., se empieza a ver q quienes no completan cagan

set.seed(2125)
run_minsal_random<-
sample(unique(CONS_C1_df_dup_SEP_2020_bd2012$run_minsal),500)

FormatData(dplyr::filter(CONS_C1_df_dup_SEP_2020_bd2012, run_minsal %in% run_minsal_random),
           idvar="id",
           timevar="trans",
           An= "tipo_de_plan_res"
)
           

# Petersen ------------------------------------------------------------------
#Petersen M. L. (2014). Commentary: Applying a causal road map in settings with time-dependent confounding. Epidemiology (Cambridge, Mass.), 25(6), 898–901. https://doi.org/10.1097/EDE.0000000000000178
GenerateData <- function(n, estimate.truth=FALSE) {
  rexpit <- function (x) rbinom(n=length(x), size=1,
                                prob=plogis(x))
  L1 <- runif(n, min=8, max=12)
  if (estimate.truth) {
    A1 <- rep(0, n) #estimate the true expected value of Y3 when A1 and A2 are set to zero
  } else {
    A1 <- rexpit(-5 + 0.5*L1)
  }
  Y2 <- rexpit(-4.5 + A1 + 2/(L1 - 7))
  L2 <- A2 <- Y3 <- rep(NA, n)
  alive <- Y2 == 0 #Y2==1 indicates death
  L2[alive] <- L1[alive] + A1[alive] + rnorm(n=sum(alive))
  if (estimate.truth) {
    A2[alive] <- 0 #estimate the true expected value of Y3 when A1 and A2 are set to zero
  } else {
    A2[alive] <- rexpit(-5 + 0.25*L1[alive] + A1[alive] +
                          0.25*L2[alive])
  }
  Y3[alive] <- rexpit(-5.5 + A1[alive] + A2[alive] +
                        2/(L1[alive]-7) + 2/(L2[alive]-7))
  Y3[!alive] <- 1
  if (estimate.truth) {
    return(mean(Y3))
  } else {
    return(data.frame(L1, A1, Y2, L2, A2, Y3))
  }
}

set.seed(6)
library(ltmle)
truth <- GenerateData(n=1000, estimate.truth=TRUE) #estimat the true expected value of Y3 when A1 and A2 are set to zero
cat("True parameter value = ", truth, "\n")
data <- GenerateData(n=200) #generate the data, letting A1 andA2 follow the data generating process
cat("\n------ using Main Terms GLM ------\n")
result <- ltmle(data, Anodes=c("A1", "A2"), Lnodes="L2",
                Ynodes=c("Y2", "Y3"), abar=c(0,0), survivalOutcome=TRUE)
print(summary(result, estimator="tmle"))
cat("\n------ using SuperLearner ------\n")
library(SuperLearner)
library(arm)
SL.library <- list("SL.glm", "SL.stepAIC", "SL.gam", "SL.knn",
                   "SL.bayesglm") #many prediction algorithms are available, this is just a sample
result.SL <- ltmle(data, Anodes=c("A1", "A2"), Lnodes="L2",
                   Ynodes=c("Y2", "Y3"), abar=c(0,0), survivalOutcome=TRUE,
                   SL.library=SL.library)
print(summary(result.SL, estimator="tmle"))


# Joy Shi ------------------------------------------------------------------
#https://github.com/joy-shi1/sncftms/blob/main/sncftm-data-example.R
n <- 10000
set.seed(54432)
time.points <- 5

# id
id <- seq(1:n)

# baseline u (note: can be time-varying)
u <- rbinom(n, size=1, prob=0.25)

# baseline z (note: can be time-varying)
z <- rbinom(n, size=1, prob=0.5)

# time-varying treatment a
a <- rbinom(n*time.points, size=1, prob=(0.25*rep(z, time.points) + 0.5*rep(u, time.points)))

# time-varying outcome y 
y <- log(1/39)+0.3*rep(u, time.points)+0.5*rep(a, time.points)
y <- rbinom(n*5, size=1, p=1/(1+exp(-y)))

# censoring due to lost to follow-up
censor.lost <- log(1/19) + 0.3*rep(u, time.points) + 0.3*a
censor.lost <- rbinom(n*5, size=1, p=1/(1+exp(-censor.lost)))

# censoring due to death
censor.death <- log(1/19) + 0.4*rep(u, time.points) + 0.4*a
censor.death <- rbinom(n*5, size=1, p=1/(1+exp(-censor.death)))

# Combining into one dataset
sim.data <- data.frame(id=rep(id, time.points),
                       time=rep(1:time.points, each=n),
                       u=rep(u, time.points),
                       z=rep(z, time.points),
                       a=a,
                       y=y,
                       censor.lost=censor.lost,
                       censor.death=censor.death)

# Note: since this is simulated data, the number of rows for each participant
# is equal to the number of time points simulated. In reality, there may be 
# fewer rows per participant because follow-up ends earlier (i.e. due to 
# developing the outcome or censoring). As such, we will remove observations
# after the first instance of developing the outcome or censoring.

# Create variable for time point where outcome/censoring occurred
y.time <- ave(ifelse(y==0, 5, y*sim.data$time), id, FUN=min)
censor.lost.time <- ave(ifelse(censor.lost==0, 5, censor.lost*sim.data$time), id, FUN=min)
censor.death.time <- ave(ifelse(censor.death==0, 5, censor.death*sim.data$time), id, FUN=min)

# Drop observations if after event or censoring
sim.data <- sim.data[which(sim.data$time<=y.time & 
                             sim.data$time<=censor.lost.time &
                             sim.data$time<=censor.death.time),]

# Only permit event or censoring (not both) to occur at any given time point
# Arbitrarily prioritize y>censor.death>censor.lost
sim.data$censor.lost <- ifelse(sim.data$y==1|sim.data$censor.death==1, 0, censor.lost)
sim.data$censor.death <- ifelse(sim.data$y==1, 0, censor.death)

# outcome should be missing if censored
sim.data$y <- ifelse(sim.data$censor.lost==1|sim.data$censor.death==1, NA, sim.data$y)

# Arrange by id and time
sim.data <- sim.data[order(sim.data$id, sim.data$time),]

# Final set-up of data:
rownames(sim.data) <- NULL
head(sim.data, n=20)

source("https://raw.githubusercontent.com/joy-shi1/sncftms/main/sncftm-function-confounding-adjustment.R")

sncftm.confresults <- sncftm.conf(data = sim.data,
                                  id = "id",
                                  time = "time",
                                  x = "a",
                                  x.modelvars = ~u+as.factor(time),
                                  x.linkfunction="logit",
                                  y = "y",
                                  clost = "censor.lost",
                                  clost.modelvars = ~a + u + as.factor(time),
                                  cdeath = "censor.death",
                                  cdeath.modelvars = ~a + u + as.factor(time),
                                  blipfunction = 1,
                                  start.value = 0,
                                  grid=T,
                                  grid.range=1.5,
                                  grid.increment=0.01,
                                  blipupdown=T,
                                  boot=T,
                                  R=10,
                                  parallel=T)

## ------------------ Posterior example ------------------
#https://github.com/joy-shi1/sncftms/blob/main/sncftm-simulations.R

### ------------------ Loading and installing relevant packages ------------------

if (!require('parallel')) install.packages('parallel'); library('parallel')
if (!require('optimx')) install.packages('optimx'); library('optimx')
if (!require('gfoRmula')) install.packages('gfoRmula'); library('gfoRmula')

### --------------------------- Simulation parameters ----------------------------

b <- 1000

### -------------------------- Loading SNCFTM functions --------------------------

source('./SNCFTM Function/SNCFTM Confounding Function.R')
source('./SNCFTM Function/SNCFTM IV Function.R')



sncftm.sim3 <- function(n=25000, b=1000, hazard, seed=84756, analysis, beta_ay=0){
  
  # Creating list of seeds
  set.seed(seed)
  seed.list <- round(runif(b)*100000)
  
  # Looping through b iterations
  results <- do.call(rbind, lapply(1:b, function(i){  
    
    set.seed(seed.list[i])
    
    # Generating data
    id <- seq(1:n)
    l1 <- rbinom(n, size=1, prob=0.25)
    z <- rbinom(n, size=1, prob=0.5)
    a1 <- rbinom(n, size=1, prob=(0.25*z+0.5*l1))
    u <- rbinom(n, size=1, prob=0.25)
    l2 <- rbinom(n, size=1, prob=(0.1+0.25*a1+0.25*l1+0.25*u))
    a2 <- rbinom(n, size=1, prob=(0.25*z+0.5*l2))
    logodds.y1 <- log(hazard/(1-hazard))+0.5*l1+beta_ay*a1
    logodds.y2 <- log(hazard/(1-hazard))+0.5*u+beta_ay*a2
    prob.y1 <- 1/(1+exp(-logodds.y1))
    prob.y2 <- 1/(1+exp(-logodds.y2))
    y1 <- rbinom(n, size=1, p=prob.y1)
    y2 <- rbinom(n, size=1, p=prob.y2)
    y2 <- ifelse(y1==1, NA, y2)
    sim.data <- rbind(data.frame(id, time=rep(1,n), l=l1, z, a=a1, y=y1),
                      data.frame(id, time=rep(2,n), l=l2, z, a=a2, y=y2))
    sim.data <- sim.data[which(!is.na(sim.data$y)),]    
    
    # Generating case-control sample
    if (analysis=="iv-cc"){
      
      # Selecting controls
      id.list1 <- sample(id[which(y1==0)], size=sum(y1)*2, replace=T)
      id.list2 <- sample(id[which(y1==0 & y2==0)], size=sum(y1==0 & y2==1)*2, replace=T)
      sim.controls <- data.frame(originalid=c(id.list1, id.list2), newid=1:(length(id.list1)+length(id.list2)))
      sim.controls <- merge(sim.controls, sim.data, by.x="originalid", by.y="id")
      sim.controls$z.include <- 1
      
      # Adding cases
      sim.cases <- sim.data[which(sim.data$id %in% id[which(y1==1|y2==1)]),]
      names(sim.cases)[1] <- "originalid"
      sim.cases$newid <- (max(sim.controls$newid)+1):(max(sim.controls$newid)+nrow(sim.cases))
      sim.cases$newid <- ave(sim.cases[['newid']], sim.cases[['originalid']], FUN=min)
      sim.cases$z.include <- 0
      
      # Combining data
      sim.ccdata <- rbind(sim.controls, sim.cases)
      
      # Renaming ID variable of original simulated data
      colnames(sim.data)[1] <- "newid"      
    }
    
    # Analysis
    if (analysis=="unadj"){
      results.unadj <- sncftm.conf(data=sim.data, id="id", time="time", x="a", 
                                   x.modelvars=~as.factor(time), x.linkfunction="logit", y="y", blipfunction=1, boot=F, 
                                   parallel=F)
      
      results2 <- c(results.unadj$psi, results.unadj$psi.esteq, results.unadj$blip.results[,3], results.unadj$blip.results[,4])
      names(results2) <- c("psi", "esteq", paste("y0.t", paste(1:max(sim.data$time)), sep=""),paste("yg.t", paste(1:max(sim.data$time)), sep=""))
      
    } else if (analysis=="adj"){
      results.adj <- sncftm.conf(data=sim.data, id="id", time="time", x="a", 
                                 x.modelvars=~l*as.factor(time), x.linkfunction="logit", y="y", blipfunction=1, boot=F, 
                                 parallel=F)
      
      results2 <- c(results.adj$psi, results.adj$psi.esteq, results.adj$blip.results[,3], results.adj$blip.results[,4])
      names(results2) <- c("psi", "esteq", paste("y0.t", paste(1:max(sim.data$time)), sep=""), paste("yg.t", paste(1:max(sim.data$time)), sep=""))    
      
    } else if (analysis=="iv"){
      results.iv <- sncftm.iv(data=sim.data, id="id", time="time", z="z", z.modelvars=~1,
                              z.family="binomial", x="a", y="y", blipfunction=1, boot=F, parallel=F)  
      
      results2 <- c(results.iv$psi, results.iv$psi.esteq, results.iv$blip.results[,3], results.iv$blip.results[,4])
      names(results2) <- c("psi", "esteq", paste("y0.t", paste(1:max(sim.data$time)), sep=""), paste("yg.t", paste(1:max(sim.data$time)), sep="")) 
      
    } else if (analysis=="iv-cc"){
      results.ivcc <- sncftm.iv(data=sim.ccdata, id="newid", time="time", z="z", z.modelvars=~1,
                                z.family="binomial", x="a", y="y", blipfunction=1, blip.data=sim.data, boot=F, parallel=F,
                                z.indicator="z.include")
      
      results.pre <- c(results.ivcc$psi, results.ivcc$psi.esteq, results.ivcc$blip.results[,3], results.ivcc$blip.results[,4])
      if (length(results.pre)<6){
        results2 <- rep(NA,6)
        results2[1:length(results.pre)] <- results.pre
      } else{
        results2 <- results.pre
      }      
      
      names(results2) <- c("psi", "esteq", paste("y0.t", paste(1:max(sim.data$time)), sep=""), paste("yg.t", paste(1:max(sim.data$time)), sep=""))  
      
    } else if (analysis=="g-formula"){
      
      gformula.data <- sim.data
      gformula.data$time <- gformula.data$time-1
      
      interventions <- list(list(c(static, rep(0, 2))), list(c(static, rep(1, 2))))
      
      results.gformula <- gformula(gformula.data, id="id", time_points=2, time_name="time",
                                   covnames=c("l","a"), outcome_name="y", outcome_type="survival",
                                   covtypes=c("binary", "binary"), histories=c(lagged, lagged),
                                   histvars=list(c("a", "l"), c("l")),
                                   covparams=list(covmodels=c(l~lag1_a + lag1_l, a~lag1_l)),
                                   ymodel=y~a*l*time,
                                   intvars=list('a', 'a'),
                                   interventions=interventions,
                                   int_descript <- c('Never treat', 'Always treat'),
                                   seed=123,
                                   model_fits=F)
      
      interventions <- list(list(c(static, rep(0, 2))),
                            list(c(static, rep(1, 2))))
      
      results2 <- as.numeric(c(results.gformula$result[2,4], results.gformula$result[5,4],
                               results.gformula$result[3,4], results.gformula$result[6,4]))
      
      names(results2) <- c(paste("y0.t", paste(1:max(sim.data$time)), sep=""), paste("yg.t", paste(1:max(sim.data$time)), sep="")) 
    }
    return(results2)
  }))
  
  # Returning results
  return(results)
}    

# ---------------------- Example execution of simulations ----------------------
sim.results1 <- sncftm.sim1(hazard=1/20, b=b, analysis="adj", beta_ay=0)
sim.results2 <- sncftm.sim2(hazard=1/20, b=b, analysis="iv", beta_ay=0.5)
sim.results3 <- sncftm.sim3(hazard=1/10, b=b, analysis="iv-cc", beta_ay=0)


# DTReg ------------------------------------------------------------------
#https://cdn-links.lww.com/permalink/ede/b/ede_28_2_2016_11_03_wallace_15-0900_sdc3.pdf #tutorial más personalizado
#https://cran.r-project.org/web/packages/DTRreg/DTRreg.pdf

#### example single run of a 2-stage DWSurv analysis
set.seed(1)
# expit function
expit <- function(x) {1 / (1 + exp(-x))}
# sample size and parameters
n <- 1000
theta1 <- c(6.3, 1.5, -0.8, 0.1, 0.1)
theta2 <- c(4, 1.1, -0.2, -0.9, 0.6, -0.1)
lambda <- 1/300
p <- 0.9
beta <- 2
# covariates and treatment (X = patient information, A = treatment)
X1 <- runif(n, 0.1, 1.29)
X14 <- X1^4
A1 <- rbinom(n, size = 1, prob = expit(2*X1 - 1))
X2 <- runif(n, 0.9, 2)
X23 <- X2^3
A2 <- rbinom(n, size = 1, prob = expit(-2*X2 + 2.8))
delta <- rbinom(n, size = 1, prob = expit(2*X1 - 0.4))
eta2 <- rbinom(n, 1, prob = 0.8)
delta2 <- delta[eta2 == 1]
# survival time
logY2 <- logT2 <- theta2[1] + theta2[2]*X2[eta2 == 1] + theta2[3]*X23[eta2 == 1]
+ theta2[4]*A2[eta2 == 1] + theta2[5]*A2[eta2 == 1]*X2[eta2 == 1]
+ theta2[6]*X1[eta2 == 1] + rnorm(sum(eta2), sd = 0.3)
trueA2opt <- ifelse(theta2[4]*A2[eta2 == 1]
                    + theta2[5]*A2[eta2 == 1]*X2[eta2 == 1] > 0, 1, 0)
logT2opt <- logT2 + (trueA2opt - A2[eta2 == 1])*(theta2[4]*A2[eta2 == 1]
                                                 + theta2[5]*A2[eta2 == 1]*X2[eta2 == 1])
logT <- theta1[1] + theta1[2]*X1 + theta1[3]*X14 + theta1[4]*A1
+ theta1[5]*A1*X1 + rnorm(n, sd = 0.3)
T1 <- exp(logT[eta2 == 1 & delta == 1]) - exp(logT2opt[delta2 == 1])
logT[eta2 == 1 & delta == 1] <- log(T1 + exp(logT2[delta2 == 1]))
# censoring time
C <- (- log(runif(n - sum(delta), 0, 1))/(lambda * exp(beta * X1[delta == 0])))^(1/p)
eta2d0 <- eta2[delta == 0]
C1 <- rep(NA, length(C))
C2 <- rep(NA, length(C))

for(i in 1:length(C))
{
  if(eta2d0[i] == 0){
    C1[i] <- C[i]
    C2[i] <- 0
  }else{
    C1[i] <- runif(1, 0, C[i])
    C2[i] <- C[i] - C1[i]
  }
}
# observed survival time
Y2 <- rep(NA, n)
Y1 <- rep(NA, n)
Y2[delta == 0] <- C2
Y1[delta == 0] <- C1
Y1[delta == 1 & eta2 == 1] <- T1
Y1[delta == 1 & eta2 == 0] <- exp(logT[delta == 1 & eta2 == 0])
Y2[delta == 1 & eta2 == 0] <- 0
Y2[delta == 1 & eta2 == 1] <- exp(logT2[delta2 == 1])
logY <- log(Y1 + Y2)
logY2 <- log(Y2[eta2 == 1])
# data and run DWSurv
mydata <- data.frame(X1,X14,A1,X2,X23,A2,delta,Y1,Y2)
mod <- DWSurv(time = list(~Y1, ~Y2), #A list of formula specifying the survival time variable for each stage in order. The time variable should be specified on the right hand side of the formula. No dependent variable should be specified. The list should be as long as the maximum number of stages.
              blip.mod = list(~X1, ~X2), #A list of formula objects specifying covariates of a (linear) blip function for each stage in order. No dependent variable should be specified.
              treat.mod = list(A1~X1, A2~X2), #A list of formula objects specifying the treatment model for each stage in order. The treatment variable should be binary and included as the dependent variable. Logistic regression models are used.
              tf.mod = list(~X1 + X14, ~X2 + X23 + X1), #A list of formula objects specifying covariates of a (linear) treatment-free model for each stage in order. No dependent variable should be specified
              cens.mod = list(delta~X1, delta~X1),#A list of formula objects specifying the censoring model for each stage in order. The event indicator, which takes value 1 if an event was observed and 0otherwise, should be included as the dependent variable and should be the same across stages. In the absence of censoring, one still needs to specify an event indicator with 1s on the right-hand side of the formula and leave the left-hand side empty (see example below).
              var.estim = "asymptotic",  #Covariance matrix estimation method, either "asymptotic", "bootstrap" or "none" (default).
              #asymp.opt= "adjusted", #If the asymptotic variance estimation is used, specify either the "adjusted" (default) or "naive" version
              #weight = "default", #A user-supplied function for the weights to be used in DWSurv. The function must have the four following arguments: treatment received A, probability of receiving treatment A=1, status, probability of being observed status = 1. Default is the inverse probability of censoring weights combined with |A - E[A|...]|.
              #optimization= "max", #If "max" (default), it is assumed that larger values/longer survival times are preferred. Set to "min" if the sequence of optimal decision rules should minimize survival time
              #boot.opt= "standard",# If bootstrap is used for variance estimation, specify either the "standard" (default), "empirical" or "normal". The last two are parametric bootstraps
              #B= 500, #Number of bootstrap resamples, if applicable
              data = mydata)#A data frame containing all necessary covariates contained in the above mod
mod

require(DTRreg)
data(datdwsurv) #not available


# Mansournia? ------------------------------------------------------------------
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8045477/bin/aje_188_4_753_s3.pdf


# IPWM ------------------------------------------------------------------
#Use string as formula for ipwtm function?
#https://stackoverflow.com/a/66466355/9975513
library("ipw")           
data("haartdat")

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#ii) R codes for structural accelerated failure time model
packages <- c("tidyverse","GGally","data.table","broom",
              "ggplot2","tabplot","gridExtra","here","parallel",
              "VIM","dummies","foreign","cmprsk","lubridate")
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package,repos='http://lib.stat.cmu.edu/R/CRAN')
  }
}
for (package in packages) {
  library(package, character.only=T)
}

thm<- theme_classic() +
  theme(
    legend.position = "top",
    #legend.title=element_blank(),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)



#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#https://core.ac.uk/download/pdf/214202029.pdf
rio::import("C:/Users/CISS Fondecyt/OneDrive/Escritorio/AOAS_5_2A_746_772_supplementA.dat")
# Han, X., Small, D. S., Foster, D. P., & Patel, V. (2011). The Effect of Winning an Oscar Award on Survival: Correcting for Healthy
# Performer Survivor Bias With a Rank Preserving Structural Accelerated Failure Time Model. Annals of Applied Statistics, 5 (2A),
# 746-772. http://dx.doi.org/10.1214/10-AOAS424


# Oscar ------------------------------------------------------------------



#**********************************************************************
#  Basic Setup
#**********************************************************************
  library("survival")
require(date)
#oscar<-read.csv("oscarupdate.TXT",sep=",",header=T)
oscar<-rio::import("C:/Users/CISS Fondecyt/OneDrive/Escritorio/AOAS_5_2A_746_772_supplementA.dat")
#rio::export(oscar,"C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/oscar.dta")
oscar<-rio::import("C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/oscar.dat")
# ***********************************
#   Check vadility,enter censoring date
# and rebuild the dataframe
# ***********************************
#   
Year<-oscar[,"Year"]
levels(Year)
Year<-as.character(Year)

Name<-oscar[,"Name"]
Name<-as.character(Name)

DOB<-oscar[,"DOB"]
DOB<-as.character(DOB)
DOB<-as.date(DOB,order="mdy")

DOD<-oscar[,"DOD"]
DOD<-as.character(DOD)
nr.dod<-length(DOD)
for(i in 1:nr.dod){
  if(DOD[i]=="")
    DOD[i]<-"7/25/2007"
}
DOD<-as.date(DOD,order="mdy")

Award<-oscar[,"Award"]
Award<-as.character(Award)

Win<-oscar[,"Win?"]

Nationality<-oscar[,"Born in US"]
Born.Place<-as.character(Nationality)

Year.of.first.film<-oscar[,"Year of First Feature Film"] 
Year.of.first.film<-as.character(Year.of.first.film)

Film<-oscar[,"For Film"]
Film<-as.character(Film)

oscar.frame<-data.frame(Year=Year,Name=Name,DOB=DOB,DOD=DOD,Award=Award,Win=Win,Born.Place=Born.Place,Year.of.first.film=Year.of.first.film,Film=Film)

# ***************************
#   Include date of Oscar Award
# ***************************
  
nr<-length(Year)
Nomination<-rep(NA,nr)
level.year<-levels(factor(oscar[,"Year"]))
l<-length(level.year)
DOA<-date::as.date(c("5/16/1929","4/3/1930","11/5/1930","11/10/1931","11/18/1932","3/16/1934","2/27/1935","3/5/1936","3/4/1937","3/10/1938","2/23/1939","2/29/1940","2/27/1941","2/26/1942","3/4/1943","3/2/1944","3/15/1945","3/7/1946","3/13/1947","3/20/1948","3/24/1949","3/23/1950","3/29/1951","3/20/1952","3/19/1953","3/25/1954","3/30/1955","3/21/1956","3/27/1957","3/26/1958","4/6/1959","4/4/1960","4/17/1961","4/9/1962","4/8/1963","4/13/1964","4/13/1965","4/18/1966","4/10/1967","4/10/1968","4/14/1969","4/7/1970","4/15/1971","4/10/1972","4/27/1973","4/2/1974","4/8/1975","3/29/1976","3/28/1977","4/3/1978","4/9/1979","4/14/1980","3/31/1981","3/29/1982","4/11/1983","4/9/1984","3/25/1985","3/24/1986","3/31/1987","4/11/1988","4/29/1989","3/26/1990","3/25/1991","3/30/1992","3/29/1993","3/21/1994","3/27/1995","3/25/1996","3/24/1997","3/23/1998","3/21/1999","3/26/2000","3/25/2001","3/24/2002","3/23/2003","2/29/2004","2/27/2005","3/5/2006","2/25/2007"),order="mdy")
for(i in 1:l){
  for(j in 1:nr){
    if(Year[j]==level.year[i])
      Nomination[j]<-DOA[i]
  }
}
Nomination<-as.date(Nomination)

oscar.frame.new<-cbind(Nomination,oscar.frame)



# ******************************************************
#   Add First Win Date, First Nom Date & Second Nom Date
# ******************************************************
  
nr.newframe<-nrow(oscar.frame.new)

first.nom.date<-rep(NA,nr.newframe)
second.nom.date<-rep(NA,nr.newframe)
for(i in 1:nr.newframe){
  t<-oscar.frame.new[,"Name"]==oscar.frame.new[i,"Name"]
  first.nom.date[i]<-oscar.frame.new[t,"Nomination"][1]
  second.nom.date[i]<-oscar.frame.new[t,"Nomination"][2]
}
first.nom.date<-as.date(first.nom.date)
second.nom.date<-as.date(second.nom.date)


first.win.date<-rep(NA,nr.newframe)
for(j in 1:nr.newframe){
  p<-oscar.frame.new[,"Name"]==oscar.frame.new[j,"Name"]
  q<-oscar.frame.new[,"Win"]==1
  t<-p&q
  first.win.date[j]<-oscar.frame.new[t,"Nomination"][1]
}
first.win.date<-as.date(first.win.date)

oscar.new<-cbind(oscar.frame.new,First.Nom.Date=first.nom.date,First.Win.Date=first.win.date,Second.Nom.Date=second.nom.date)

age<-(oscar.new[,"DOD"]-oscar.new[,"DOB"])/365

alive<-as.numeric(oscar.new[,"DOD"]==17372)

r<-is.na(oscar.new[,"First.Win.Date"])
oscar.new[r,"First.Win.Date"]<-99999

oscar.new<-cbind(oscar.new,Age=age,Alive=alive)

age.first.nom<-(oscar.new[,"First.Nom.Date"]-oscar.new[,"DOB"])/365
oscar.new<-cbind(oscar.new,Age.First.Nom=age.first.nom)


male<-rep(0,nr.newframe)
for(i in 1:nr.newframe){
  if((oscar.new[i,"Award"]=="Best Actor")|(oscar.new[i,"Award"]=="Best Supporting Actor"))
  {male[i]<-1}
}
oscar.new<-cbind(oscar.new,Male=male)

# 
# ***********************************
#   delete posthumous cases
# ***********************************
sep.post<-oscar.new[,"Nomination"]<oscar.new[,"DOD"]
oscar.new<-oscar.new[sep.post,]


# 
# ***********************************
#   Add Win Times
# ***********************************
nr.oscar<-nrow(oscar.new)
wintimes<-rep(0,nr.oscar)

for(i in 2:nr.oscar){
  if(oscar.new[i,"Win"]==1){
    sep1<-oscar.new[i,"Name"]==oscar.new[1:(i-1),"Name"]
    a<-rep(FALSE,nr.oscar-i+1)
    sep1<-c(sep1,a)
    sep2<-oscar.new[sep1,"Win"]==1
    wintimes[i]<-sum(sep2)+1
  }   
  
}
oscar.new<-cbind(oscar.new,wintimes)
# 
# 
# ***********************************
#   Winning Affects Future Nomination
# ***********************************
future<-(!is.na(oscar.new[,"Second.Nom.Date"]))&(oscar.new[,"Second.Nom.Date"]!=oscar.new[,"First.Nom.Date"])
oscar.select<-oscar.new[future,]
mfit<-coxph(Surv((Second.Nom.Date-First.Nom.Date)/365)~(First.Win.Date==First.Nom.Date)+Age.First.Nom,data=oscar.select)

# 
# 
# **********************************************
#   pvalue function 
# **********************************************
  pvalue.multinom<-function(phi,theta){
    phi<-0
    pvalue<-rep(0,3)
    nr.oscar<-nrow(oscar.new)
    oft<-rep(0,nr.oscar)
    ct<-rep(0,nr.oscar)
    pft<-rep(0,nr.oscar)
    value<-rep(0,nr.oscar)
    nomage<-rep(0,nr.oscar)
    numprenom<-rep(0,nr.oscar)
    numprewin<-rep(0,nr.oscar)
    
    
    for(i in 1:nr.oscar){
      oft[i]<-(oscar.new[i,"DOD"]-oscar.new[i,"Nomination"])/365
    }
    
    
    
    for(i in 1:nr.oscar){
      if(oscar.new[i,"Nomination"]<=oscar.new[i,"First.Win.Date"]){
        ct[i]<-min((17372-oscar.new[i,"Nomination"])/365,(17372-oscar.new[i,"Nomination"])*exp(phi)/365)
      }
      else{
        ct[i]<-(17372-oscar.new[i,"Nomination"])/365
      }
    }
    
    for(i in 1:nr.oscar){
      if(oscar.new[i,"First.Win.Date"]==99999){
        pft[i]<-oft[i]
      }
      else{
        if(oscar.new[i,"Nomination"]<=oscar.new[i,"First.Win.Date"]){
          pft[i]<-(oscar.new[i,"First.Win.Date"]-oscar.new[i,"Nomination"]+(oscar.new[i,"DOD"]-oscar.new[i,"First.Win.Date"])*exp(phi))/365
        }
        else{
          pft[i]<-(oscar.new[i,"DOD"]-oscar.new[i,"Nomination"])/365
        }
      }
    }
    
    
    for(i in 1:nr.oscar){
      value[i]<-min(pft[i],ct[i])
    }
    
    for(i in 1:nr.oscar){
      nomage[i]<-(oscar.new[i,"Nomination"]-oscar.new[i,"DOB"])/365
    }
    
    for(i in 2:nr.oscar){
      sep<-oscar.new[i,"Name"]==oscar.new[1:(i-1),"Name"]
      numprenom[i]<-sum(sep)
    }
    
    
    for(i in 2:nr.oscar){
      sep1<-oscar.new[i,"Name"]==oscar.new[1:(i-1),"Name"]
      a<-rep(FALSE,nr.oscar-i+1)
      sep1<-c(sep1,a)
      sep2<-oscar.new[sep1,"Win"]==1
      numprewin[i]<-sum(sep2)
    }
    
    oscar.new.temp<-cbind(oscar.new,value)
    
    
    Winner<-oscar.new.temp$Win
    Award.date<-oscar.new.temp$Nomination
    Award<-oscar.new.temp$Award
    value<-oscar.new.temp$value
    nomage.square<-nomage^2
    nomage.cubic<-nomage^3
    inter<-Award.date*as.numeric(Award)*10000
    
    
    t1<-clogit(Winner~value+nomage+nomage.square+nomage.cubic+numprenom+strata(inter),subset=as.logical(numprewin==0))
    t2<-clogit(Winner~value+numprenom+strata(inter))
    
    mean<-unlist(summary(t1))$coef1
    se<-unlist(summary(t1))$coef11
    
    pvalue[1]<-(mean-theta)/se
    if(pvalue[1]>0){ 
      pvalue[2]<-2*(1-pnorm(pvalue[1])) 
    } else {
      pvalue[2]<-2*pnorm(pvalue[1])
    } 
    pvalue[3]<-mean
    return(pvalue)
    
  }


# 
# ******************************
#   nomage with cubic splines
# ******************************
q<-quantile(nomage,probs=seq(0,1,0.25))


t3<-clogit(Winner~value+bs(nomage,df=5)+numprenom+strata(inter))

# 
# *********************************************************************
#   sensitivity analysis for confidence interval,and for phi, using bisection method
# *********************************************************************
odds<-seq(0.5,1.5,by=0.1)
number<-length(odds)
theta<-log(odds)/10
phi<-matrix(0,number,3)
phi[,1]<-theta
for(i in 1:number){
  x.old.minus<- -1
  x.old.plus<-1
  f1<-pvalue.multinom(x.old.plus,theta[i])[1]
  if(f1>=1.96){
    while(abs(x.old.minus-x.old.plus)>0.0001){
      x.new<-(x.old.minus+x.old.plus)/2
      f<-pvalue.multinom(x.new,theta[i])[1]
      if(abs(f-1.96)<0.001){
        x.final<-x.new
        break
      }
      else{
        if(f-1.96<=-0.001){
          x.old.minus<-x.new
          x.old.plus<-x.old.plus
        }
        else{
          x.old.plus<-x.new
          x.old.minus<-x.old.minus
        }
      }
    }
    phi[i,3]<-x.final
  }
}

for(i in 1:number){
  x.old.minus<- -0.7
  x.old.plus<-1
  h1<-pvalue.multinom(x.old.minus,theta[i])[1]
  if(h1<=-1.96){
    while(abs(x.old.minus-x.old.plus)>0.0001){
      x.new<-(x.old.minus+x.old.plus)/2
      f<-pvalue.multinom(x.new,theta[i])[1]
      if(abs(f+1.96)<0.001){
        x.final<-x.new
        break
      }
      else{
        if(f+1.96<=-0.001){
          x.old.minus<-x.new
          x.old.plus<-x.old.plus
        }
        else{
          x.old.plus<-x.new
          x.old.minus<-x.old.minus
        }
      }
    }
    phi[i,2]<-x.final
  }
}



#*********************************************************
#*********************************************************
  
  sep.win<-oscar.new[,"Win"]==1
oscar.win<-oscar.new[sep.win,]
r<-nrow(oscar.win)
t<-0
for(i in 1:r){
  sep<-oscar.win[i,"Name"]==oscar.win[1:(i-1),"Name"]
  if(sum(sep)==0){
    t<-t+1
  }
}


sep.nom<-oscar.new[,"First.Win.Date"]==99999
oscar.nom<-oscar.new[sep.nom,]
r<-nrow(oscar.nom)
t<-0
for(i in 1:r){
  sep<-oscar.nom[i,"Name"]==oscar.nom[1:(i-1),"Name"]
  if(sum(sep)==0){
    t<-t+1
  }
}


sep.win.dead<-(oscar.new[,"First.Win.Date"]==oscar.new[,"Nomination"])&(oscar.new[,"Win"]==1)&(oscar.new[,"Alive"]==0)
(sum(pft[sep.win.dead])-sum(oft[sep.win.dead]))/sum(sep.win.dead)


# ****************************************************
#   survival advantage in terms of years
# ****************************************************
  
  sep.win<-(oscar.new[,"First.Win.Date"]==oscar.new[,"Nomination"])&(oscar.new[,"Win"]==1)
oscar.firstwin<-oscar.new[sep.win,]
nr.firstwin<-nrow(oscar.firstwin)

survival.year<-function(phi){
  nr.firstwin<-260
  oft.firstwin<-rep(0,nr.firstwin)
  ct.firstwin<-rep(0,nr.firstwin)
  pft.firstwin<-rep(0,nr.firstwin)
  value.firstwin<-rep(0,nr.firstwin)  
  
  for(i in 1:nr.firstwin){
    oft.firstwin[i]<-(oscar.firstwin[i,"DOD"]-oscar.firstwin[i,"Nomination"])/365
  }
  
  for(i in 1:nr.firstwin){
    ct.firstwin[i]<-min((17372-oscar.firstwin[i,"Nomination"])/365,(17372-oscar.firstwin[i,"Nomination"])*exp(phi)/365)     
  }
  
  for(i in 1:nr.firstwin){
    pft.firstwin[i]<-(oscar.firstwin[i,"DOD"]-oscar.firstwin[i,"Nomination"])*exp(phi)/365   
  }
  for(i in 1:nr.firstwin){
    value.firstwin[i]<-min(pft.firstwin[i],ct.firstwin[i])
  }   
  
  
  winstatus<-c(rep(1,nr.firstwin),rep(0,nr.firstwin))
  time<-c(oft.firstwin,value.firstwin)
  censorstatus<-c(oscar.firstwin[,"Alive"],pft.firstwin>=ct.firstwin)
  mfit.temp<-survfit(Surv(time,censorstatus!=1)~winstatus)   
  return(mfit.temp)
}



survival<-matrix(0,nrow(phi),3)
survival[,1]<-phi[,1]
for(i in 1:nrow(phi)){
  survival[i,2]<-survival.year(phi[i,2])
  survival[i,3]<-survival.year(phi[i,3])
}



# *******************************************************
#   Diagnostic Plots for SAFTM
# *******************************************************
  nomage.temp<-nomage[numprewin==0]
value.temp<-value[numprewin==0]
alive<-oscar.new[numprewin==0,"Alive"]
win<-oscar.new[numprewin==0,"Win"]


q.new<-quantile(nomage.temp,probs=seq(0,1,0.2))
sep1<-nomage.temp<=q.new[2]
sep2<-(nomage.temp>q.new[2])&(nomage.temp<=q.new[3])
sep3<-(nomage.temp>q.new[3])&(nomage.temp<=q.new[4])
sep4<-(nomage.temp>q.new[4])&(nomage.temp<=q.new[5])
sep5<-(nomage.temp>q.new[5])&(nomage.temp<=q.new[6])

par(mfrow=c(3,2),mar=c(4,4,2,1),las=1) 
boxplot(value.temp[sep1]~win[sep1],names=c("Control","Treatment"),main="nomage<=30.23",lwd=2,cex.axis=2,cex.lab=3,cex.main=2,col="grey")
boxplot(value.temp[sep2]~win[sep2],names=c("Control","Treatment"),main="30.23<nomage<=35.51",lwd=2,cex.axis=2,cex.lab=3,cex.main=2,col="grey")
boxplot(value.temp[sep3]~win[sep3],names=c("Control","Treatment"),main="35.51<nomage<=41.2",lwd=2,cex.axis=2,cex.lab=3,cex.main=2,col="grey")
boxplot(value.temp[sep4]~win[sep4],names=c("Control","Treatment"),main="41.2<nomage<=50.64",lwd=2,cex.axis=2,cex.lab=3,cex.main=2,col="grey")
boxplot(value.temp[sep5]~win[sep5],names=c("Control","Treatment"),main="50.64<nomage<=87.78",lwd=2,cex.axis=2,cex.lab=3,cex.main=2,col="grey")

# **********************************************
#   analogue for previous methods
# ********************************************** 
  r<-nrow(oscar.new)
nomyear<-rep(0,r)
birthyear<-rep(0,r)
finalyear<-rep(0,r)
firstnomyear<-rep(0,r)
firstwinyear<-rep(0,r)
numprenom<-rep(0,r)
for(i in 1:r){
  nomyear[i]<-date.mdy(oscar.new[i,"Nomination"])$year
  birthyear[i]<-date.mdy(oscar.new[i,"DOB"])$year
  finalyear[i]<-date.mdy(oscar.new[i,"DOD"])$year
  firstnomyear[i]<-date.mdy(oscar.new[i,"First.Nom.Date"])$year
  firstwinyear[i]<-date.mdy(oscar.new[i,"First.Win.Date"])$year
}
for(i in 2:r){
  sep<-oscar.new[i,"Name"]==oscar.new[1:(i-1),"Name"]
  numprenom[i]<-sum(sep)
}


lifetime<-finalyear-birthyear
oscar.nom.temp<-cbind(oscar.new,nomyear,birthyear,finalyear,lifetime,firstnomyear,firstwinyear)
m<-sum(lifetime)+r
td.oscar.nom<-matrix(0,m,4)
colnames(td.oscar.nom)<-c("Start","Stop","Death.status","Treatment")
row<-0
for(i in 1:r){
  for(j in 0:110){
    if(oscar.nom.temp[i,"birthyear"]+j>=oscar.nom.temp[i,"finalyear"]) break
    else{
      row<-row+1
      start<-j
      stop<-start+1
      death.status<-if(stop==ceiling(oscar.nom.temp[i,"lifetime"])&&oscar.nom.temp[i,"Alive"]==0) 1 else 0
      treatment<-if((oscar.nom.temp[i,"firstwinyear"]!=2233)&(start>=(oscar.nom.temp[i,"firstwinyear"]-oscar.nom.temp[i,"birthyear"]))) 1 else 0
      td.oscar.nom[row,]<-c(start,stop,death.status,treatment)
    }
  }
}
td.oscar.nom<-as.data.frame(td.oscar.nom)
a<-td.oscar.nom[,"Stop"]!=0
td.oscar.nom<-td.oscar.nom[a,]
td.oscar.cox<-coxph(Surv(Start,Stop,Death.status)~Treatment!=0,td.oscar.nom)



td.oscar.nom.new<-matrix(0,m,7)
colnames(td.oscar.nom.new)<-c("Start","Stop","Death.status","Nom.status","Treatment","birthyear","numprenom")
row<-0
for(i in 1:r){
  for(j in 0:110){
    if(oscar.nom.temp[i,"birthyear"]+j>oscar.nom.temp[i,"finalyear"]) break
    else{
      row<-row+1
      start<-j
      stop<-start+1
      death.status<-if((stop==oscar.nom.temp[i,"finalyear"]-oscar.nom.temp[i,"birthyear"])&&oscar.nom.temp[i,"Alive"]==0) 1 else 0
      nom.status<-if(start>=oscar.nom.temp[i,"firstnomyear"]-oscar.nom.temp[i,"birthyear"]) 0 else 1
      treatment<-if((oscar.nom.temp[i,"firstwinyear"]!=2233)&&(start>=oscar.nom.temp[i,"firstwinyear"]-oscar.nom.temp[i,"birthyear"])) 1 else 0
      td.oscar.nom.new[row,]<-c(start,stop,death.status,nom.status,treatment,birthyear[i],numprenom[i])
    }
  }
}
td.oscar.nom.new<-as.data.frame(td.oscar.nom.new)
b<-td.oscar.nom.new[,"Nom.status"]==0
temp<-td.oscar.nom.new[b,]
td.new.cox<-coxph(Surv(Start,Stop,Death.status)~(Treatment!=0),temp)
td.new.cox2<-coxph(Surv(Start,Stop,Death.status)~(Treatment!=0)+numprenom,temp)


calendar<-temp[,"birthyear"]+temp[,"Start"]
temp<-cbind(temp,calendar)
fit<-glm(Death.status~Treatment+Start+calendar,data=temp,family=binomial)


alive<-oscar.new[,"Alive"]
sim.cox<-coxph(Surv(finalyear-birthyear,alive!=1)~firstwinyear!=2233)


#_#_#_#_#_#_
#simualtion1

# ************************************************************************************
#   Previous four methods are biased and SAFTM corrects the bias when nomination is 
# affected by past winning history and winning is affected by past nomination history
# ************************************************************************************
  
library(survival)

phi<-0
sim.round<-1000
pvalue<-matrix(0,sim.round,5)
colnames(pvalue)<-c("cox","td.cox","td.cox.new","td.py.new","multinom")
winnersnoms.temp<-matrix(0,sim.round,78)

for(u in 1:sim.round){ 
  # *********************************************************
  #   From 1830 to 1999, 5 actors born each year
  # Generate dataset "actor" with columns "birthyear"
  # "healthyear" and "lifetime"
  # *********************************************************    
    repeat{
      begin.year<-1830
      end.year<-1999
      n<-end.year-begin.year+1
      num<-5
      variables<-9
      vol<-n*num*variables
      actor<-matrix(rep(0,vol),nrow=(n*num),ncol=variables)
      colnames(actor)<-c("birthyear","healthyear","lifetime","N30","N60","N70","A30","A60","A70")
      temp<-rep(begin.year,num)
      for(i in (begin.year+1):end.year){
        temp<-c(temp,rep(i,num))
      }
      actor[,"birthyear"]<-temp
      
      
      for(k in 1:(n*num)){
        range<-sample(1:3,1)
        actor[k,"lifetime"]<-range*10+60
        actor[k,"healthyear"]<-range*10+50
      }
      
      actor<-as.data.frame(actor)
      item<-1:(n*num)
      actor<-cbind(item,actor)
      
      # ************************************************
      #   Select nominees and winners for each award
      # ************************************************
        
        award.list<-matrix(0,78,7)
      award.list<-as.data.frame(award.list)
      colnames(award.list)<-c("Year","Nom1","Nom2","Nom3","Nom4","Nom5","Winner")
      award.list[,"Year"]<-1927:2004
      
      s<-nrow(award.list)
      for(t in 1:s){
        sep<-((actor[,"birthyear"]+actor[,"healthyear"])>(award.list[t,"Year"]))&((actor[,"birthyear"]+10)<award.list[t,"Year"])
        award.age<-award.list[t,"Year"]-actor[,"birthyear"]
        
        item.temp<-sample(item[sep],0)
        sep.temp1<-(award.age>=30)&(award.age<40)&sep
        sep.temp2<-(award.age>=60)&(award.age<70)&sep
        sep.temp3<-(award.age>=70)&(award.age<80)&sep
        if((sum(sep.temp1)>=2)&(sum(sep.temp2)>=2)&(sum(sep.temp3)>=1)){
          
          item.temp<-c(item.temp,sample(item[sep.temp1],2))
          
          ntemp<-sum(sep.temp2)
          weight<-rep(1,ntemp)
          tempactor<-actor[sep.temp2,]
          for(p in 1:ntemp){
            winpast<-tempactor[p,"item"]==award.list[1:t,"Winner"]
            winpast3<-(sum(winpast)>=1)&(tempactor[p,"lifetime"]==90)
            winpast2<-(sum(winpast)>=1)&(tempactor[p,"lifetime"]==80)
            nowinpast3<-(sum(winpast)==0)&(tempactor[p,"lifetime"]==90)
            nowinpast2<-(sum(winpast)==0)&(tempactor[p,"lifetime"]==80)
            if(winpast3){
              weight[p]<-9
            }
            if(winpast2){
              weight[p]<-8
            }
            if(nowinpast3){
              weight[p]<-7
            }
            if(nowinpast2){
              weight[p]<-1
            }
          }
          nom.prob<-weight/sum(weight)
          item.temp<-c(item.temp,sample(item[sep.temp2],2,prob=nom.prob))
          
          if(length(item[sep.temp3])==1){
            item.temp<-c(item.temp,item[sep.temp3])
          }
          else{
            item.temp<-c(item.temp,sample(item[sep.temp3],1))
          }
          
          item.nom<-item.temp
          
          award.list[t,2:6]<-item.nom
          
          award.age.nom<-rep(0,5)
          odd<-rep(0,5)
          for(f in 1:5){
            sep.nom<-item==item.nom[f]
            award.age.nom[f]<-award.age[sep.nom]
            nom30<-award.age.nom[f]>=30 & award.age.nom[f]<40
            nom60<-award.age.nom[f]>=60 & award.age.nom[f]<70
            nom70<-award.age.nom[f]>=70 & award.age.nom[f]<80
            nom.age.coe<-c(0.5,1,2)
            nom.coe<-c(0.5,0.5,0.5)
            win.coe<-c(0.5,0.5,0.5)
            nom.value<-c(sum(nom30),sum(nom60),sum(nom70))
            actornom<-c(actor[sep.nom,"N30"],actor[sep.nom,"N60"],actor[sep.nom,"N70"])
            actorwin<-c(actor[sep.nom,"A30"],actor[sep.nom,"A60"],actor[sep.nom,"A70"])
            odd[f]<-sum(nom.age.coe*nom.value)+sum(nom.coe*actornom)+sum(win.coe*actorwin)
            
            if(nom30){
              actor[sep.nom,"N30"]<- actor[sep.nom,"N30"]+1
            }
            if(nom60){
              actor[sep.nom,"N60"]<- actor[sep.nom,"N60"]+1
            }
            if(nom70){
              actor[sep.nom,"N70"]<- actor[sep.nom,"N70"]+1
            }
          }
          win.prob<-exp(odd)/sum(exp(odd))
          
          repeat{
            z<-sample(item.nom,1,prob=win.prob)
            if(z!=0){
              award.list[t,7]<-z
              break
            }
          }
          sep.win<-item==award.list[t,7]
          win30<-award.age[sep.win]>=30 & award.age[sep.win]<40
          win60<-award.age[sep.win]>=60 & award.age[sep.win]<70
          win70<-award.age[sep.win]>=70 & award.age[sep.win]<80
          if(win30){
            actor[sep.win,"A30"]<-actor[sep.win,"A30"]+1
          }
          if(win60){
            actor[sep.win,"A60"]<-actor[sep.win,"A60"]+1
          }
          if(win70){
            actor[sep.win,"A70"]<-actor[sep.win,"A70"]+1
          }
        }
        else{
          break
        }
      }
      if(t==s){
        break
      }
    }
  
  
  for(i in 1:78){
    winnersnoms.temp[u,i]<-sum(award.list[,2:6]==award.list[i,7])
  }
  
  # ******************************************************
  #   add firstnomyear, firstwinyear, finalyear to dataset
  # ******************************************************
    firstnomyear<-rep(0,850)
  firstwinyear<-rep(0,850)
  secondwinyear<-rep(0,850)
  left<-0
  l<-2008
  for(i in 1:850){
    m<-2008
    for(j in 2:6){
      a<-i==award.list[,j]
      b<-award.list[a,"Year"]
      m<-c(m,b)
    }
    firstnomyear[i]<-min(m)
    d<-i==award.list[,"Winner"]
    e<-award.list[d,"Year"]
    firstwinyear[i]<-min(c(e,l))
    left<-setdiff(c(e,l),firstwinyear[i])
    if(length(left)>0){
      secondwinyear[i]<-min(setdiff(c(e,l),firstwinyear[i]))
    }
    else{
      secondwinyear[i]<-2008
    }
  }
  actor.temp<-cbind(actor,firstnomyear,firstwinyear,secondwinyear)
  
  
  alive<-rep(0,850)
  for(i in 1:850){
    alive[i]<-if((actor.temp[i,"birthyear"]+actor.temp[i,"lifetime"])>2004) 1 else 0
  }
  actor.temp<-cbind(actor.temp,alive)
  
  finalyear<-rep(0,850)
  for(i in 1:850){
    v<-actor.temp[i,"birthyear"]+actor.temp[i,"lifetime"]
    finalyear[i]<-if(v<2005) ceiling(v) else 2005
  }
  
  actor.temp<-cbind(actor.temp,finalyear)
  
  # 
  # ******************************************************************
  #   Cox model without time-dependent covariates,birthday as time-zero
  # ******************************************************************  
  #   
    sep.nom<-actor.temp[,"firstnomyear"]!=2008
  actor.temp.nom<-actor.temp[sep.nom,]
  sim.cox<-coxph(Surv(finalyear-birthyear,alive!=1)~firstwinyear!=2008,actor.temp.nom)
  
  pvalue[u,"cox"]<-unlist(summary(sim.cox))$coefficients5
  # 
  # *******************************************************************
  #   Cox model with time-dependent covariates, birthday as time-zero
  # *******************************************************************
    r<-nrow(actor.temp.nom)
  c<-ncol(actor.temp.nom)+4
  m<-sum(actor.temp.nom[,"lifetime"])+r
  td.actor.temp.nom<-matrix(0,m,c)
  colnames(td.actor.temp.nom)<-c("Start","Stop","Death.status","Treatment",names(actor.temp.nom))
  row<-0
  for(i in 1:r){
    for(j in 0:100){
      if(actor.temp.nom[i,"birthyear"]+j>=actor.temp.nom[i,"finalyear"]) break
      else{
        row<-row+1
        start<-j
        stop<-start+1
        death.status<-if(stop==ceiling(actor.temp.nom[i,"lifetime"])&&actor.temp.nom[i,"alive"]==0) 1 else 0
        treatment<-if((actor.temp.nom[i,"firstwinyear"]!=2008)&(start>=(actor.temp.nom[i,"firstwinyear"]-actor.temp.nom[i,"birthyear"]))) 1 else 0
        td.actor.temp.nom[row,]<-c(start,stop,death.status,treatment,unlist(actor.temp.nom[i,]))
      }
    }
  }
  td.actor.temp.nom<-as.data.frame(td.actor.temp.nom)
  a<-td.actor.temp.nom[,"Stop"]!=0
  td.actor.temp.nom<-td.actor.temp.nom[a,]
  td.actor.cox<-coxph(Surv(Start,Stop,Death.status)~Treatment!=0,td.actor.temp.nom)
  
  pvalue[u,"td.cox"]<-unlist(summary(td.actor.cox))$coefficients5
  
  # 
  # *******************************************************************
  #   Cox model with time-dependent covariates, nomination as time-zero
  # *******************************************************************
    r.new<-nrow(actor.temp.nom)
  c.new<-ncol(actor.temp.nom)+5
  m.new<-sum(actor.temp.nom[,"finalyear"])-sum(actor.temp.nom[,"birthyear"])+r.new
  td.actor.temp.nom.new<-matrix(0,m.new,c.new)
  colnames(td.actor.temp.nom.new)<-c("Start","Stop","Death.status","Nom.status","Treatment",names(actor.temp.nom))
  row<-0
  for(i in 1:r.new){
    for(j in 0:100){
      if(actor.temp.nom[i,"birthyear"]+j>actor.temp.nom[i,"finalyear"]) break
      else{
        row<-row+1
        start<-j
        stop<-start+1
        death.status<-if(stop==actor.temp.nom[i,"finalyear"]-actor.temp.nom[i,"birthyear"]&& actor.temp.nom[i,"alive"]==0)1 else 0
        nom.status<-if(start>=actor.temp.nom[i,"firstnomyear"]-actor.temp.nom[i,"birthyear"]) 0 else 1
        treatment<-if((actor.temp.nom[i,"firstwinyear"]!=2008) && (start>=actor.temp.nom[i,"firstwinyear"]-actor.temp.nom[i,"birthyear"])) 1 else 0
        td.actor.temp.nom.new[row,]<-c(start,stop,death.status,nom.status,treatment,unlist(actor.temp.nom[i,]))
      }
    }
  }
  td.actor.temp.nom.new<-as.data.frame(td.actor.temp.nom.new)
  sep<-td.actor.temp.nom.new[,"Nom.status"]==0
  temp<-td.actor.temp.nom.new[sep,]
  td.new.cox<-coxph(Surv(Start,Stop,Death.status)~(Treatment!=0),temp)
  
  pvalue[u,"td.cox.new"]<-unlist(summary(td.new.cox))$coefficients5
  
  # 
  # ***********************************************************
  #   binomial logistic regression model
  # ***********************************************************
    calendar<-temp[,"birthyear"]+temp[,"Start"]
  temp<-cbind(temp,calendar)
  
  fit<-glm(Death.status~Treatment+Start+calendar,data=temp,family=binomial)
  t<-coef(fit)[2]/sqrt(vcov(fit)[2,2])
  pvalue[u,"td.py.new"]<-if(t>0) 1-(pnorm(t)-.5)*2 else pnorm(t)*2 
  
  
  # ***********************************************************
  #   structural accelerated failure time model
  # ***********************************************************
    
    potential<-matrix(0,390,8)
  potential<-as.data.frame(potential)
  colnames(potential)<-c("Awardyear","Item","Winner","Observedfailuretime","Censoringtime","Potentialfailuretime","Value","Age")
  te<-rep(1927,5)
  for(i in 1928:2004){
    te<-c(te,rep(i,5))
  }
  potential[,"Awardyear"]<-te
  
  it<-unlist(award.list[1,2:6])
  for(i in 2:78){
    it<-c(it,unlist(award.list[i,2:6]))
  }
  potential[,"Item"]<-it
  
  potential[,"Winner"]<-rep(0,390)
  for(i in 1:78){
    for(j in ((i-1)*5+1):(i*5)){
      if(potential[j,"Item"]==award.list[i,7]) potential[j,"Winner"]<-1
    }
  }
  
  nr<-nrow(potential)
  wintimes<-rep(0,nr)
  if(potential[1,"Winner"]==1){
    wintimes[1]<-1
  }
  for(i in 2:nr){
    if(potential[i,"Winner"]==1){
      sep1<-potential[i,"Item"]==potential[1:(i-1),"Item"]
      a<-rep(FALSE,nr-i+1)
      sep1<-c(sep1,a)
      sep2<-potential[sep1,"Winner"]==1
      wintimes[i]<-sum(sep2)+1
    }
  }
  potential<-cbind(potential,wintimes)
  
  
  
  for(i in 1:390){
    sep<-potential[i,"Item"]==actor.temp[,"item"]   
    potential[i,"Observedfailuretime"]<-actor.temp[sep,"finalyear"]-potential[i,"Awardyear"]
  }
  
  
  for(i in 1:390){
    potential[i,"Censoringtime"]<-min((2005-potential[i,"Awardyear"]),(2005-potential[i,"Awardyear"])*exp(phi))
  }
  
  for(i in 1:390){
    sep<-potential[i,"Item"]==actor.temp[,"item"]
    if(actor.temp[sep,"firstwinyear"]==2008){
      potential[i,"Potentialfailuretime"]<-potential[i,"Observedfailuretime"]
    }
    else{
      if(potential[i,"Awardyear"]<=actor.temp[sep,"firstwinyear"]){
        potential[i,"Potentialfailuretime"]<-actor.temp[sep,"firstwinyear"]-potential[i,"Awardyear"]+(actor.temp[sep,"finalyear"]-actor.temp[sep,"firstwinyear"])*exp(phi)
      }
      else{
        potential[i,"Potentialfailuretime"]<-actor.temp[sep,"finalyear"]-potential[i,"Awardyear"]
      }
    }
  }
  
  
  for(i in 1:390){
    potential[i,"Value"]<-min(potential[i,"Potentialfailuretime"],potential[i,"Censoringtime"])
  }
  
  
  for(i in 1:390){
    sep<-potential[i,"Item"]==actor.temp[,"item"]
    potential[i,"Age"]<-potential[i,"Awardyear"]-actor.temp[sep,"birthyear"]
  }
  
  
  Age<-potential$Age
  Value<-potential$Value
  Winner<-potential$Winner
  Awardyear<-potential$Awardyear
  age3040<-(Age>=30)*(Age<40)
  age6070<-(Age>=60)*(Age<70)
  t<-clogit(Winner~Value+age3040+age6070+strata(Awardyear))
  pvalue[u,"multinom"]<-unlist(summary(t))$coefficients13
}

pvalue

#simualtion2

# **************************************************************************************************
#   Calculate mortality rates conditional on nomination history and winning history when 
# nomination is affected by past winning history and winning is affected by past nomination history
# **************************************************************************************************
#   
  sim.round<-1000
mortality<-matrix(-2,sim.round,59)
colnames(mortality)<-c("D7N32N60","D7N31N60","D7N31N61","D7N30N60","D7N30N61","D7N30N62","D8N31N60N70","D8N31N60N71","D8N31N61N70","D8N30N61N70","D8N30N61N71","D8N30N62N70","D8N30N60N70","D8N30N60N71","D8N30N60N72","D8N32N60N70","D7N32A32N60A60","D7N32A31N60A60","D7N32A30N60A60","D7N31A31N60A60","D7N31A31N61A60","D7N31A31N61A61","D7N31A30N60A60","D7N31A30N61A60","D7N31A30N61A61","D7N30A30N60A60","D7N30A30N61A60","D7N30A30N61A61","D7N30A30N62A60","D7N30A30N62A61","D7N30A30N62A62","D8N32A32N60A60N70A70","D8N32A31N60A60N70A70","D8N32A30N60A60N70A70","D8N31A31N60A60N70A70","D8N31A31N60A60N71A70","D8N31A31N60A60N71A71","D8N31A30N60A60N70A70","D8N31A30N60A60N71A70","D8N31A30N60A60N71A71","D8N31A31N61A61N70A70","D8N31A31N61A60N70A70","D8N31A30N61A61N70A70","D8N31A30N61A60N70A70","D8N30A30N61A61N70A70","D8N30A30N61A61N71A70","D8N30A30N61A61N71A71","D8N30A30N61A60N70A70","D8N30A30N61A60N71A70","D8N30A30N61A60N71A71","D8N30A30N62A62N70A70","D8N30A30N62A61N70A70","D8N30A30N62A60N70A70","D8N30A30N60A60N70A70","D8N30A30N60A60N72A70","D8N30A30N60A60N72A71","D8N30A30N60A60N72A72","D8N30A30N60A60N71A70","D8N30A30N60A60N71A71")

pvalue<-rep(0,sim.round)
test.value<-rep(0,sim.round)


for(u in 1:sim.round){
  
  repeat{
    begin.year<-1830
    end.year<-1999
    n<-end.year-begin.year+1
    num<-5
    variables<-9
    vol<-n*num*variables
    actor<-matrix(rep(0,vol),nrow=(n*num),ncol=variables)
    colnames(actor)<-c("birthyear","healthyear","lifetime","N30","A30","N60","A60","N70","A70")
    temp<-rep(begin.year,num)
    for(i in (begin.year+1):end.year){
      temp<-c(temp,rep(i,num))
    }
    actor[,"birthyear"]<-temp
    
    
    for(k in 1:(n*num)){
      range<-sample(1:3,1)
      actor[k,"lifetime"]<-range*10+60
      actor[k,"healthyear"]<-range*10+50
    }
    
    actor<-as.data.frame(actor)
    item<-1:(n*num)
    actor<-cbind(item,actor)
    
    
    
    award.list<-matrix(0,78,7)
    award.list<-as.data.frame(award.list)
    colnames(award.list)<-c("Year","Nom1","Nom2","Nom3","Nom4","Nom5","Winner")
    award.list[,"Year"]<-1927:2004
    
    s<-nrow(award.list)
    for(t in 1:s){
      sep<-((actor[,"birthyear"]+actor[,"healthyear"])>(award.list[t,"Year"]))&((actor[,"birthyear"]+10)<award.list[t,"Year"])
      N2<-(actor[,"N30"]+actor[,"N60"]+actor[,"N70"])<2
      sep<-sep & N2
      award.age<-award.list[t,"Year"]-actor[,"birthyear"]
      
      item.temp<-sample(item[sep],0)
      sep.temp1<-(award.age>=30)&(award.age<40)&sep
      sep.temp2<-(award.age>=60)&(award.age<70)&sep
      sep.temp3<-(award.age>=70)&(award.age<80)&sep
      if((sum(sep.temp1)>=2)&(sum(sep.temp2)>=2)&(sum(sep.temp3)>=1)){
        
        item.temp<-c(item.temp,sample(item[sep.temp1],2))
        
        ntemp<-sum(sep.temp2)
        weight<-rep(1,ntemp)
        tempactor<-actor[sep.temp2,]
        for(p in 1:ntemp){
          winpast<-tempactor[p,"item"]==award.list[1:t,"Winner"]
          winpast3<-(sum(winpast)>=1)&(tempactor[p,"lifetime"]==90)
          winpast2<-(sum(winpast)>=1)&(tempactor[p,"lifetime"]==80)
          nowinpast3<-(sum(winpast)==0)&(tempactor[p,"lifetime"]==90)
          nowinpast2<-(sum(winpast)==0)&(tempactor[p,"lifetime"]==80)
          if(winpast3){
            weight[p]<-9
          }
          if(winpast2){
            weight[p]<-8
          }
          if(nowinpast3){
            weight[p]<-7
          }
          if(nowinpast2){
            weight[p]<-1
          }
        }
        nom.prob<-weight/sum(weight)
        item.temp<-c(item.temp,sample(item[sep.temp2],2,prob=nom.prob))
        
        if(length(item[sep.temp3])==1){
          item.temp<-c(item.temp,item[sep.temp3])
        }
        else{
          item.temp<-c(item.temp,sample(item[sep.temp3],1))
        }
        item.nom<-item.temp
        
        award.list[t,2:6]<-item.nom
        
        award.age.nom<-rep(0,5)
        odd<-rep(0,5)
        for(f in 1:5){
          sep.nom<-item==item.nom[f]
          award.age.nom[f]<-award.age[sep.nom]
          nom30<-award.age.nom[f]>=30 & award.age.nom[f]<40
          nom60<-award.age.nom[f]>=60 & award.age.nom[f]<70
          nom70<-award.age.nom[f]>=70 & award.age.nom[f]<80
          nom.age.coe<-c(0.5,1,2)
          nom.coe<-c(0.5,0.5,0.5)
          win.coe<-c(0.5,0.5,0.5)
          nom.value<-c(sum(nom30),sum(nom60),sum(nom70))
          actornom<-c(actor[sep.nom,"N30"],actor[sep.nom,"N60"],actor[sep.nom,"N70"])
          actorwin<-c(actor[sep.nom,"A30"],actor[sep.nom,"A60"],actor[sep.nom,"A70"])
          odd[f]<-sum(nom.age.coe*nom.value)+sum(nom.coe*actornom)+sum(win.coe*actorwin)
          
          if(nom30){
            actor[sep.nom,"N30"]<- actor[sep.nom,"N30"]+1
          }
          if(nom60){
            actor[sep.nom,"N60"]<- actor[sep.nom,"N60"]+1
          }
          if(nom70){
            actor[sep.nom,"N70"]<- actor[sep.nom,"N70"]+1
          }
        }
        win.prob<-exp(odd)/sum(exp(odd))
        
        repeat{
          z<-sample(item.nom,1,prob=win.prob)
          if(z!=0){
            award.list[t,7]<-z
            break
          }
        }
        sep.win<-item==award.list[t,7]
        win30<-award.age[sep.win]>=30 & award.age[sep.win]<40
        win60<-award.age[sep.win]>=60 & award.age[sep.win]<70
        win70<-award.age[sep.win]>=70 & award.age[sep.win]<80
        if(win30){
          actor[sep.win,"A30"]<-actor[sep.win,"A30"]+1
        }
        if(win60){
          actor[sep.win,"A60"]<-actor[sep.win,"A60"]+1
        }
        if(win70){
          actor[sep.win,"A70"]<-actor[sep.win,"A70"]+1
        }
      }
      else{
        break
      }
    }
    if(t==s){
      break
    }
  }
  
  
  
  age70<-(actor[,"lifetime"]==70) 
  age80<-(actor[,"lifetime"]==80) 
  age90<-(actor[,"lifetime"]==90) 
  
  
  N32<-(actor[,"N30"]==2)
  N31<-(actor[,"N30"]==1)
  N30<-(actor[,"N30"]==0)
  N60<-(actor[,"N60"]==0)
  N61<-(actor[,"N60"]==1)
  N62<-(actor[,"N60"]==2)
  N70<-(actor[,"N70"]==0)
  N71<-(actor[,"N70"]==1)
  N72<-(actor[,"N70"]==2)
  A30<-(actor[,"A30"]==0)
  A31<-(actor[,"A30"]==1)
  A32<-(actor[,"A30"]==2)
  A60<-(actor[,"A60"]==0)
  A61<-(actor[,"A60"]==1)
  A62<-(actor[,"A60"]==2)
  A70<-(actor[,"A70"]==0)
  A71<-(actor[,"A70"]==1)
  A72<-(actor[,"A70"]==2)
  
  
  N32N60<-N32&N60
  N31N60<-N31&N60
  N31N61<-N31&N61
  N30N60<-N30&N60
  N30N61<-N30&N61
  N30N62<-N30&N62
  N31N60N70<-N31&N60&N70
  N31N60N71<-N31&N60&N71
  N31N61N70<-N31&N61&N70
  N30N61N70<-N30&N61&N70
  N30N61N71<-N30&N61&N71
  N30N62N70<-N30&N62&N70
  N30N60N70<-N30&N60&N70
  N30N60N71<-N30&N60&N71
  N30N60N72<-N30&N60&N72
  N32N60N70<-N32&N60&N70
  
  
  N32A32N60A60<-N32&A32&N60&A60
  N32A31N60A60<-N32&A31&N60&A60
  N32A30N60A60<-N32&A30&N60&A60
  N31A31N60A60<-N31&A31&N60&A60
  N31A31N61A60<-N31&A31&N61&A60
  N31A31N61A61<-N31&A31&N61&A61  
  N31A30N60A60<-N31&A30&N60&A60
  N31A30N61A60<-N31&A30&N61&A60
  N31A30N61A61<-N31&A30&N61&A61
  N30A30N60A60<-N30&A30&N60&A60
  N30A30N61A60<-N30&A30&N61&A60
  N30A30N61A61<-N30&A30&N61&A61
  N30A30N62A60<-N30&A30&N62&A60
  N30A30N62A61<-N30&A30&N62&A61
  N30A30N62A62<-N30&A30&N62&A62
  
  
  
  N32A32N60A60N70A70<-N32&A32&N60&A60&N70&A70  
  N32A31N60A60N70A70<-N32&A31&N60&A60&N70&A70 
  N32A30N60A60N70A70<-N32&A30&N60&A60&N70&A70 
  N31A31N60A60N70A70<-N31&A31&N60&A60&N70&A70 
  N31A31N60A60N71A70<-N31&A31&N60&A60&N71&A70 
  N31A31N60A60N71A71<-N31&A31&N60&A60&N71&A71
  N31A30N60A60N70A70<-N31&A30&N60&A60&N70&A70
  N31A30N60A60N71A70<-N31&A30&N60&A60&N71&A70
  N31A30N60A60N71A71<-N31&A30&N60&A60&N71&A71
  N31A31N61A61N70A70<-N31&A31&N61&A61&N70&A70
  N31A31N61A60N70A70<-N31&A31&N61&A60&N70&A70
  N31A30N61A61N70A70<-N31&A30&N61&A61&N70&A70
  N31A30N61A60N70A70<-N31&A30&N61&A60&N70&A70
  N30A30N61A61N70A70<-N30&A30&N61&A61&N70&A70   
  N30A30N61A61N71A70<-N30&A30&N61&A61&N71&A70
  N30A30N61A61N71A71<-N30&A30&N61&A61&N71&A71
  N30A30N61A60N70A70<-N30&A30&N61&A60&N70&A70
  N30A30N61A60N71A70<-N30&A30&N61&A60&N71&A70
  N30A30N61A60N71A71<-N30&A30&N61&A60&N71&A71
  
  N30A30N62A62N70A70<-N30&A30&N62&A62&N70&A70
  N30A30N62A61N70A70<-N30&A30&N62&A61&N70&A70
  N30A30N62A60N70A70<-N30&A30&N62&A60&N70&A70
  N30A30N60A60N70A70<-N30&A30&N60&A60&N70&A70
  N30A30N60A60N72A70<-N30&A30&N60&A60&N72&A70
  N30A30N60A60N72A71<-N30&A30&N60&A60&N72&A71
  N30A30N60A60N72A72<-N30&A30&N60&A60&N72&A72
  N30A30N60A60N71A70<-N30&A30&N60&A60&N71&A70
  N30A30N60A60N71A71<-N30&A30&N60&A60&N71&A71
  
  
  age8090<-age80 | age90
  t1<-sum(N32N60)
  t2<-sum(N31N60)
  t3<-sum(N31N61)
  t4<-sum(N30N60)
  t5<-sum(N30N61)
  t6<-sum(N30N62)
  t7<-sum(age80 & N31N60N70)+sum(age90 & N31N60N70)
  t8<-sum(age80 & N31N60N71)+sum(age90 & N31N60N71)
  t9<-sum(age80 & N31N61N70)+sum(age90 & N31N61N70)
  t10<-sum(age80 & N30N61N70)+sum(age90 & N30N61N70)
  t11<-sum(age80 & N30N61N71)+sum(age90 & N30N61N71)
  t12<-sum(age80 & N30N62N70)+sum(age90 & N30N62N70)
  t13<-sum(age80 & N30N60N70)+sum(age90 & N30N60N70)
  t14<-sum(age80 & N30N60N71)+sum(age90 & N30N60N71)
  t15<-sum(age80 & N30N60N72)+sum(age90 & N30N60N72)
  t16<-sum(age80 & N32N60N70)+sum(age90 & N32N60N70)   
  
  
  
  
  if(t1>0) mortality[u,"D7N32N60"]<-sum(age70 & N32N60)/t1
  if(t2>0) mortality[u,"D7N31N60"]<-sum(age70 & N31N60)/t2
  if(t3>0) mortality[u,"D7N31N61"]<-sum(age70 & N31N61)/t3
  if(t4>0) mortality[u,"D7N30N60"]<-sum(age70 & N30N60)/t4
  if(t5>0) mortality[u,"D7N30N61"]<-sum(age70 & N30N61)/t5
  if(t6>0) mortality[u,"D7N30N62"]<-sum(age70 & N30N62)/t6
  if(t7>0) mortality[u,"D8N31N60N70"]<-sum(age80 & N31N60N70)/t7
  if(t8>0) mortality[u,"D8N31N60N71"]<-sum(age80 & N31N60N71)/t8
  if(t9>0) mortality[u,"D8N31N61N70"]<-sum(age80 & N31N61N70)/t9
  if(t10>0) mortality[u,"D8N30N61N70"]<-sum(age80 & N30N61N70)/t10
  if(t11>0) mortality[u,"D8N30N61N71"]<-sum(age80 & N30N61N71)/t11
  if(t12>0) mortality[u,"D8N30N62N70"]<-sum(age80 & N30N62N70)/t12
  if(t13>0) mortality[u,"D8N30N60N70"]<-sum(age80 & N30N60N70)/t13
  if(t14>0) mortality[u,"D8N30N60N71"]<-sum(age80 & N30N60N71)/t14
  if(t15>0) mortality[u,"D8N30N60N72"]<-sum(age80 & N30N60N72)/t15
  if(t16>0) mortality[u,"D8N32N60N70"]<-sum(age80 & N32N60N70)/t16
  
  
  
  t17<-sum(N32A32N60A60)
  t18<-sum(N32A31N60A60)
  t19<-sum(N32A30N60A60)
  t20<-sum(N31A31N60A60)
  t21<-sum(N31A31N61A60)
  t22<-sum(N31A31N61A61)  
  t23<-sum(N31A30N60A60)
  t24<-sum(N31A30N61A60)
  t25<-sum(N31A30N61A61)
  t26<-sum(N30A30N60A60)
  t27<-sum(N30A30N61A60)
  t28<-sum(N30A30N61A61)
  t29<-sum(N30A30N62A60)
  t30<-sum(N30A30N62A61)
  t31<-sum(N30A30N62A62)
  
  
  
  
  
  
  if(t17>0) mortality[u,"D7N32A32N60A60"]<-sum(age70 & N32A32N60A60)/t17
  if(t18>0) mortality[u,"D7N32A31N60A60"]<-sum(age70 & N32A31N60A60)/t18
  if(t19>0) mortality[u,"D7N32A30N60A60"]<-sum(age70 & N32A30N60A60)/t19
  if(t20>0) mortality[u,"D7N31A31N60A60"]<-sum(age70 & N31A31N60A60)/t20
  if(t21>0) mortality[u,"D7N31A31N61A60"]<-sum(age70 & N31A31N61A60)/t21
  if(t22>0) mortality[u,"D7N31A31N61A61"]<-sum(age70 & N31A31N61A61)/t22
  if(t23>0) mortality[u,"D7N31A30N60A60"]<-sum(age70 & N31A30N60A60)/t23
  if(t24>0) mortality[u,"D7N31A30N61A60"]<-sum(age70 & N31A30N61A60)/t24
  if(t25>0) mortality[u,"D7N31A30N61A61"]<-sum(age70 & N31A30N61A61)/t25
  if(t26>0) mortality[u,"D7N30A30N60A60"]<-sum(age70 & N30A30N60A60)/t26
  if(t27>0) mortality[u,"D7N30A30N61A60"]<-sum(age70 & N30A30N61A60)/t27
  if(t28>0) mortality[u,"D7N30A30N61A61"]<-sum(age70 & N30A30N61A61)/t28
  if(t29>0) mortality[u,"D7N30A30N62A60"]<-sum(age70 & N30A30N62A60)/t29
  if(t30>0) mortality[u,"D7N30A30N62A61"]<-sum(age70 & N30A30N62A61)/t30
  if(t31>0) mortality[u,"D7N30A30N62A62"]<-sum(age70 & N30A30N62A62)/t31
  
  
  
  t32<-sum(age80 & N32A32N60A60N70A70)+sum(age90 & N32A32N60A60N70A70) 
  t33<-sum(age80 & N32A31N60A60N70A70)+sum(age90 & N32A31N60A60N70A70)
  t34<-sum(age80 & N32A30N60A60N70A70)+sum(age90 & N32A30N60A60N70A70) 
  t35<-sum(age80 & N31A31N60A60N70A70)+sum(age90 & N31A31N60A60N70A70) 
  t36<-sum(age80 & N31A31N60A60N71A70)+sum(age90 & N31A31N60A60N71A70) 
  t37<-sum(age80 & N31A31N60A60N71A71)+sum(age90 & N31A31N60A60N71A71)
  t38<-sum(age80 & N31A30N60A60N70A70)+sum(age90 & N31A30N60A60N70A70)
  t39<-sum(age80 & N31A30N60A60N71A70)+sum(age90 & N31A30N60A60N71A70)
  t40<-sum(age80 & N31A30N60A60N71A71)+sum(age90 & N31A30N60A60N71A71)
  t41<-sum(age80 & N31A31N61A61N70A70)+sum(age90 & N31A31N61A61N70A70)
  t42<-sum(age80 & N31A31N61A60N70A70)+sum(age90 & N31A31N61A60N70A70)
  t43<-sum(age80 & N31A30N61A61N70A70)+sum(age90 & N31A30N61A61N70A70)
  t44<-sum(age80 & N31A30N61A60N70A70)+sum(age90 & N31A30N61A60N70A70)
  t45<-sum(age80 & N30A30N61A61N70A70)+sum(age90 & N30A30N61A61N70A70)   
  t46<-sum(age80 & N30A30N61A61N71A70)+sum(age90 & N30A30N61A61N71A70)
  t47<-sum(age80 & N30A30N61A61N71A71)+sum(age90 & N30A30N61A61N71A71)
  t48<-sum(age80 & N30A30N61A60N70A70)+sum(age90 & N30A30N61A60N70A70)
  t49<-sum(age80 & N30A30N61A60N71A70)+sum(age90 & N30A30N61A60N71A70)
  t50<-sum(age80 & N30A30N61A60N71A71)+sum(age90 & N30A30N61A60N71A71)
  
  t51<-sum(age80 & N30A30N62A62N70A70)+sum(age90 & N30A30N62A62N70A70)
  t52<-sum(age80 & N30A30N62A61N70A70)+sum(age90 & N30A30N62A61N70A70)
  t53<-sum(age80 & N30A30N62A60N70A70)+sum(age90 & N30A30N62A60N70A70)
  t54<-sum(age80 & N30A30N60A60N70A70)+sum(age90 & N30A30N60A60N70A70)
  t55<-sum(age80 & N30A30N60A60N72A70)+sum(age90 & N30A30N60A60N72A70)
  t56<-sum(age80 & N30A30N60A60N72A71)+sum(age90 & N30A30N60A60N72A71)
  t57<-sum(age80 & N30A30N60A60N72A72)+sum(age90 & N30A30N60A60N72A72)
  t58<-sum(age80 & N30A30N60A60N71A70)+sum(age90 & N30A30N60A60N71A70)
  t59<-sum(age80 & N30A30N60A60N71A71)+sum(age90 & N30A30N60A60N71A71)
  
  
  
  if(t32>0) mortality[u,"D8N32A32N60A60N70A70"]<-sum(age80 & N32A32N60A60N70A70)/t32
  if(t33>0) mortality[u,"D8N32A31N60A60N70A70"]<-sum(age80 & N32A31N60A60N70A70)/t33
  if(t34>0) mortality[u,"D8N32A30N60A60N70A70"]<-sum(age80 & N32A30N60A60N70A70)/t34
  if(t35>0) mortality[u,"D8N31A31N60A60N70A70"]<-sum(age80 & N31A31N60A60N70A70)/t35
  if(t36>0) mortality[u,"D8N31A31N60A60N71A70"]<-sum(age80 & N31A31N60A60N71A70)/t36
  if(t37>0) mortality[u,"D8N31A31N60A60N71A71"]<-sum(age80 & N31A31N60A60N71A71)/t37
  if(t38>0) mortality[u,"D8N31A30N60A60N70A70"]<-sum(age80 & N31A30N60A60N70A70)/t38
  if(t39>0) mortality[u,"D8N31A30N60A60N71A70"]<-sum(age80 & N31A30N60A60N71A70)/t39
  if(t40>0) mortality[u,"D8N31A30N60A60N71A71"]<-sum(age80 & N31A30N60A60N71A71)/t40
  if(t41>0) mortality[u,"D8N31A31N61A61N70A70"]<-sum(age80 & N31A31N61A61N70A70)/t41
  if(t42>0) mortality[u,"D8N31A31N61A60N70A70"]<-sum(age80 & N31A31N61A60N70A70)/t42
  if(t43>0) mortality[u,"D8N31A30N61A61N70A70"]<-sum(age80 & N31A30N61A61N70A70)/t43
  if(t44>0) mortality[u,"D8N31A30N61A60N70A70"]<-sum(age80 & N31A30N61A60N70A70)/t44
  if(t45>0) mortality[u,"D8N30A30N61A61N70A70"]<-sum(age80 & N30A30N61A61N70A70)/t45
  if(t46>0) mortality[u,"D8N30A30N61A61N71A70"]<-sum(age80 & N30A30N61A61N71A70)/t46
  if(t47>0) mortality[u,"D8N30A30N61A61N71A71"]<-sum(age80 & N30A30N61A61N71A71)/t47
  if(t48>0) mortality[u,"D8N30A30N61A60N70A70"]<-sum(age80 & N30A30N61A60N70A70)/t48
  if(t49>0) mortality[u,"D8N30A30N61A60N71A70"]<-sum(age80 & N30A30N61A60N71A70)/t49
  if(t50>0) mortality[u,"D8N30A30N61A60N71A71"]<-sum(age80 & N30A30N61A60N71A71)/t50
  if(t51>0) mortality[u,"D8N30A30N62A62N70A70"]<-sum(age80 & N30A30N62A62N70A70)/t51
  if(t52>0) mortality[u,"D8N30A30N62A61N70A70"]<-sum(age80 & N30A30N62A61N70A70)/t52
  if(t53>0) mortality[u,"D8N30A30N62A60N70A70"]<-sum(age80 & N30A30N62A60N70A70)/t53
  if(t54>0) mortality[u,"D8N30A30N60A60N70A70"]<-sum(age80 & N30A30N60A60N70A70)/t54
  if(t55>0) mortality[u,"D8N30A30N60A60N72A70"]<-sum(age80 & N30A30N60A60N72A70)/t55
  if(t56>0) mortality[u,"D8N30A30N60A60N72A71"]<-sum(age80 & N30A30N60A60N72A71)/t56
  if(t57>0) mortality[u,"D8N30A30N60A60N72A72"]<-sum(age80 & N30A30N60A60N72A72)/t57
  if(t58>0) mortality[u,"D8N30A30N60A60N71A70"]<-sum(age80 & N30A30N60A60N71A70)/t58
  if(t59>0) mortality[u,"D8N30A30N60A60N71A71"]<-sum(age80 & N30A30N60A60N71A71)/t59
  
  
  **********************************************************************************************
    Likelihood ratio test to show that a correct Cox model provides a biased estimate of the effect
  of winning on survival
  **********************************************************************************************    
    
    full<-matrix(0,43,6)
  full[1,1:2]<-2
  full[2,1]<-2;full[2,2]<-1
  full[3,1]<-2
  full[4,1:2]<-1
  full[5,1:3]<-1
  full[6,1:4]<-1
  full[7,1]<-1
  full[8,c(1,3)]<-1
  full[9,c(1,3:4)]<-1
  full[11,3]<-1
  full[12,3:4]<-1
  full[13,3]<-2
  full[14,3]<-2;full[14,4]<-1
  full[15,3:4]<-2  
  
  full[16,1:2]<-2
  full[17,1]<-2;full[17,2]<-1
  full[18,1]<-2
  full[19,1:2]<-1
  full[20,c(1:2,5)]<-1
  full[21,c(1:2,5:6)]<-1
  full[22,1]<-1
  full[23,c(1,5)]<-1
  full[24,c(1,5:6)]<-1
  full[25,c(1:4)]<-1
  full[26,c(1:3)]<-1
  full[27,c(1,3:4)]<-1
  full[28,c(1,3)]<-1
  full[29,3:4]<-1
  full[30,3:5]<-1
  full[31,3:6]<-1
  full[32,3]<-1
  full[33,c(3,5)]<-1
  full[34,c(3,5:6)]<-1
  full[35,3:4]<-2
  full[36,3]<-2;full[36,4]<-1
  full[37,3]<-2
  full[39,5]<-2
  full[40,5]<-2;full[40,6]<-1
  full[41,5:6]<-2
  full[42,5]<-1
  full[43,5:6]<-1
  
  
  colnames(full)<-c("N30","A30","N60","A60","N70","A70")
  lifetime<-c(rep(70,15),rep(80,28))
  full<-cbind(as.numeric(mortality[u,17:59]),lifetime,full)
  
  
  
  
  reduce<-matrix(0,16,3)
  reduce[1,1]<-2
  reduce[2,1]<-1
  reduce[3,1:2]<-1
  reduce[5,2]<-1
  reduce[6,2]<-2
  reduce[7,1]<-1
  reduce[8,c(1,3)]<-1
  reduce[9,1:2]<-1
  reduce[10,2]<-1
  reduce[11,2:3]<-1
  reduce[12,2]<-2
  reduce[14,3]<-1
  reduce[15,3]<-2
  reduce[16,1]<-2   
  
  
  
  colnames(reduce)<-c("N30","N60","N70")
  lifetime2<-c(rep(70,6),rep(80,10))
  reduce<-cbind(as.numeric(mortality[u,1:16]),lifetime2,reduce)
  
  
  
  nompast<-(actor[,"N30"]+actor[,"N60"]+actor[,"N70"])>=1
  actor.temp<-actor[nompast,]
  n.temp<-nrow(actor.temp)
  h.full<-1
  h.reduce<-1
  for(i in 1:n.temp){
    if(actor.temp[i,"lifetime"]==70){
      for(j in 1:15){
        sep.temp<-actor.temp[i,4:8]==full[j,2:6]              
        if(sum(sep.temp)==5){
          h.full<-h.full*max(0,full[j,1])
        }
      }
      for(k in 1:6){
        sep.temp2<-actor.temp[i,c(4:5,7)]==reduce[k,2:4]
        if(sum(sep.temp2)==3){
          h.reduce<-h.reduce*max(0,reduce[k,1])
        }            
      }
      
    }
    if(actor.temp[i,"lifetime"]==80){
      for(j in 1:15){
        sep.temp<-(actor.temp[i,5:8]==full[j,3:6])
        if(sum(sep.temp)+sum(full[j,2]==70)==5){
          h.full<-h.full*(1-max(0,full[j,1]))                
        }
      }
      for(j in 16:43){
        sep.temp1<-actor.temp[i,4:10]==full[j,2:8]
        if(sum(sep.temp1)==7){
          h.full<-h.full*max(0,full[j,1])
        }
      }
      for(k in 1:6){
        sep.temp2<-actor.temp[i,c(5,7)]==reduce[k,3:4]
        if(sum(sep.temp2)+sum(reduce[k,2]==70)==3){
          h.reduce<-h.reduce*(1-max(0,reduce[k,1]))
        }          
      }
      for(k in 7:16){
        sep.temp3<-actor.temp[i,c(4:5,7,9)]==reduce[k,2:5]
        if(sum(sep.temp3)==4){
          h.reduce<-h.reduce*max(0,reduce[k,1])
        }
      }
    }
    
    if(actor.temp[i,"lifetime"]==90){
      for(j in 1:15){
        sep.temp<-(actor.temp[i,5:8]==full[j,3:6])
        if(sum(sep.temp)+sum(full[j,2]==70)==5){
          h.full<-h.full*(1-max(0,full[j,1]))                
        }
      }
      for(j in 16:43){
        sep.temp1<-actor.temp[i,5:10]==full[j,3:8]
        if(sum(sep.temp1)+sum(full[j,2]==80)==7){
          h.full<-h.full*(1-max(0,full[j,1]))
        }
      }
      for(k in 1:6){
        sep.temp2<-actor.temp[i,c(5,7)]==reduce[k,3:4]
        if(sum(sep.temp2)+sum(reduce[k,2]==70)==3){
          h.reduce<-h.reduce*(1-max(0,reduce[k,1]))
        }          
      }
      for(k in 7:16){
        sep.temp3<-actor.temp[i,c(5,7,9)]==reduce[k,3:5]
        if(sum(sep.temp3)+sum(reduce[k,2]==80)==4){
          h.reduce<-h.reduce*(1-max(0,reduce[k,1]))
        }
      }
    }      
    
  }
  loglik.full<-log(h.full)
  loglik.reduce<-log(h.reduce)
  test.value[u]<--2*(loglik.reduce-loglik.full)
  
  pvalue[u]<-1-pchisq(test.value[u],12)
  
  
}

mean<-rep(0,59)
for(i in 1:59){
  seq<-mortality[,i]>=0
  mean[i]<-mean(mortality[seq,i])
}
mean<-as.matrix(t(mean))
colnames(mean)<-colnames(mortality)

CI<-matrix(0,59,2)
for(i in 1:59){
  seq<-mortality[,i]>=0
  average<-mean(mortality[seq,i])
  sigma<-sd(mortality[seq,i])
  n<-sum(seq)
  CI[i,1]<-average-1.96*sigma/(sqrt(n))
  CI[i,2]<-average+1.96*sigma/(sqrt(n))
}

pvalue

mean

CI
