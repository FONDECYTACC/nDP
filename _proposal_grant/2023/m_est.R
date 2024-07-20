
#Saul, B. C., & Hudgens, M. G. (2020). The Calculus of M-Estimation in R with geex. 
#Journal of Statistical Software, 92(2), 1–15. https://doi.org/10.18637/jss.v092.i02


dat <- tibble(anemia=c(rep(0,4),rep(1,4)),
              bp=rep(c(rep(0,2),rep(1,2)),2),
              ptb=rep(c(0,1),4),
              n=c(496,74,113,25,85,15,15,3)) %>%
  uncount(n) 

n <- nrow(dat)     # Number of observations

pacman::p_load(tidyverse, numDeriv, rootSolve, geex, detectseparation, install=T)

############################################
# Regression by MLE 

reg_mle <- glm(ptb ~ anemia + bp, data=dat, family="binomial")   

ests_mle <-as.data.frame(cbind("beta"=reg_mle$coefficients,
                               "se"=sqrt(diag(vcov(reg_mle))))) %>%
  mutate(lcl = beta - 1.96*se,
         ucl = beta + 1.96*se)
intlist <- row.names(ests_mle)

print("Estimated logistic regression")
print("MLE")
print(round(ests_mle,3))

############################################
# Defining estimating equation

estimating_function <- function(beta){
  p <- plogis(beta[1] + beta[2]*dat$anemia + beta[3]*dat$bp)
  ef_1 <- (dat$ptb - p)
  ef_2 <- (dat$ptb - p)*dat$anemia
  ef_3 <- (dat$ptb - p)*dat$bp
  return(cbind(ef_1, ef_2, ef_3)) 
}

pacman::p_load(tidyverse, numDeriv, rootSolve, geex, install=T)

current_data$sc_edad_al_ing_1 <- scale(current_data$edad_al_ing_1)
current_data$sc_ano_nac_corr <- scale(current_data$ano_nac_corr)

estimating_function <- function(beta){
p <- plogis(beta[1]+ beta[2]*current_data$policonsumo2 +
              beta[3]*current_data$comp_bpsc_y3_severe+
              beta[4]*current_data$sc_edad_al_ing_1 + 
              beta[5]*current_data$sc_ano_nac_corr + 
              beta[6]*current_data$susinidumrec_otr +
              beta[7]*current_data$susinidumrec_coc +
              beta[8]*current_data$susinidumrec_pbc +
              beta[9]*current_data$susinidumrec_mar +
              beta[10]*current_data$psycom_dum_study +
              beta[11]*current_data$psycom_dum_with +
              beta[12]*current_data$freq_cons_dum_5day +
              beta[13]*current_data$cond_oc_dum_2inact +
              beta[14]*current_data$cond_oc_dum_3unemp +
              beta[15]*current_data$susprindumrec_coc +
              beta[16]*current_data$susprindumrec_pbc +
              beta[17]*current_data$susprindumrec_mar+
              beta[18]*current_data$susprindumrec_otr)
ef_1 <- (current_data$tr_outcome - p)
ef_2 <- (current_data$tr_outcome - p)*current_data$policonsumo2
ef_3 <- (current_data$tr_outcome - p)*current_data$comp_bpsc_y3_severe
ef_4 <- (current_data$tr_outcome - p)*current_data$sc_edad_al_ing_1
ef_5 <- (current_data$tr_outcome - p)*current_data$sc_ano_nac_corr
ef_6 <- (current_data$tr_outcome - p)*current_data$susinidumrec_otr
ef_7 <- (current_data$tr_outcome - p)*current_data$susinidumrec_coc
ef_8 <- (current_data$tr_outcome - p)*current_data$susinidumrec_pbc
ef_9 <- (current_data$tr_outcome - p)*current_data$susinidumrec_mar
ef_10 <- (current_data$tr_outcome - p)*current_data$psycom_dum_study
ef_11 <- (current_data$tr_outcome - p)*current_data$psycom_dum_with
ef_12 <- (current_data$tr_outcome - p)*current_data$freq_cons_dum_5day
ef_13 <- (current_data$tr_outcome - p)*current_data$cond_oc_dum_2inact
ef_14 <- (current_data$tr_outcome - p)*current_data$cond_oc_dum_3unemp
ef_15 <- (current_data$tr_outcome - p)*current_data$susprindumrec_coc
ef_16 <- (current_data$tr_outcome - p)*current_data$susprindumrec_pbc
ef_17 <- (current_data$tr_outcome - p)*current_data$susprindumrec_mar
ef_18 <- (current_data$tr_outcome - p)*current_data$susprindumrec_otr
return(cbind(ef_1, ef_2, ef_3, ef_4, ef_5, ef_6, ef_7, ef_8, ef_9, ef_10, ef_11, ef_12, ef_13, ef_14, ef_15, ef_16, ef_17, ef_18)) 
}

estimating_equation <- function(beta){
  estf = estimating_function(beta)        # Return estimating function
  este = colSums(estf)                    # Estimating equations are sum
  return(este)
}


geex_ef <- function(data){               # Function of estimating functions (to be used in geex::m_estimate)
  current_data<- data
  function(theta){
    p <- plogis(theta[1]+ theta[2]*current_data$policonsumo2 +
                  theta[3]*current_data$comp_bpsc_y3_severe+
                  theta[4]*current_data$sc_edad_al_ing_1 + 
                  theta[5]*current_data$sc_ano_nac_corr + 
                  theta[6]*current_data$susinidumrec_otr +
                  theta[7]*current_data$susinidumrec_coc +
                  theta[8]*current_data$susinidumrec_pbc +
                  theta[9]*current_data$susinidumrec_mar +
                  theta[10]*current_data$psycom_dum_study +
                  theta[11]*current_data$psycom_dum_with +
                  theta[12]*current_data$freq_cons_dum_5day +
                  theta[13]*current_data$cond_oc_dum_2inact +
                  theta[14]*current_data$cond_oc_dum_3unemp +
                  theta[15]*current_data$susprindumrec_coc +
                  theta[16]*current_data$susprindumrec_pbc +
                  theta[17]*current_data$susprindumrec_mar+
                  theta[18]*current_data$susprindumrec_otr)
    ef_1 <- (current_data$tr_outcome - p)
    ef_2 <- (current_data$tr_outcome - p)*current_data$policonsumo2
    ef_3 <- (current_data$tr_outcome - p)*current_data$comp_bpsc_y3_severe
    ef_4 <- (current_data$tr_outcome - p)*current_data$sc_edad_al_ing_1
    ef_5 <- (current_data$tr_outcome - p)*current_data$sc_ano_nac_corr
    ef_6 <- (current_data$tr_outcome - p)*current_data$susinidumrec_otr
    ef_7 <- (current_data$tr_outcome - p)*current_data$susinidumrec_coc
    ef_8 <- (current_data$tr_outcome - p)*current_data$susinidumrec_pbc
    ef_9 <- (current_data$tr_outcome - p)*current_data$susinidumrec_mar
    ef_10 <- (current_data$tr_outcome - p)*current_data$psycom_dum_study
    ef_11 <- (current_data$tr_outcome - p)*current_data$psycom_dum_with
    ef_12 <- (current_data$tr_outcome - p)*current_data$freq_cons_dum_5day
    ef_13 <- (current_data$tr_outcome - p)*current_data$cond_oc_dum_2inact
    ef_14 <- (current_data$tr_outcome - p)*current_data$cond_oc_dum_3unemp
    ef_15 <- (current_data$tr_outcome - p)*current_data$susprindumrec_coc
    ef_16 <- (current_data$tr_outcome - p)*current_data$susprindumrec_pbc
    ef_17 <- (current_data$tr_outcome - p)*current_data$susprindumrec_mar
    ef_18 <- (current_data$tr_outcome - p)*current_data$susprindumrec_otr
    c(ef_1, ef_2, ef_3, ef_4, ef_5, ef_6, ef_7, ef_8, ef_9, ef_10, ef_11, ef_12, ef_13, ef_14, ef_15, ef_16, ef_17, ef_18) 
  }
}
invisible("Starting values!!")
#https://cran.r-project.org/web/packages/detectseparation/vignettes/infinite_estimates.html
##https://statmodeling.stat.columbia.edu/2011/05/04/whassup_with_gl/
find_start_simple <- function(formula, data) {
  c(-1, double(ncol(model.matrix(formula, data = data)) - 1L))  
}

find_start_simple(tr_outcome ~ policonsumo2 +
                    comp_bpsc_y3_severe+
                    edad_al_ing_1 + 
                    ano_nac_corr + 
                    susinidumrec_otr +
                    susinidumrec_coc +
                    susinidumrec_pbc +
                    susinidumrec_mar +
                    psycom_dum_study +
                    psycom_dum_with +
                    freq_cons_dum_5day +
                    cond_oc_dum_2inact +
                    cond_oc_dum_3unemp +
                    susprindumrec_coc +
                    susprindumrec_pbc +
                    susprindumrec_mar+
                    susprindumrec_otr, current_data)

find_start_poisson <- function(formula, data, delta = 1) {
  b0 <- coef(glm(formula, data, family = poisson(link = "log")))
  mX <- -model.matrix(formula, data = data)[, -1L, drop = FALSE]
  b0[1] <- min(mX %*% b0[-1]) - delta
  b0
}
svs<-
find_start_poisson(tr_outcome ~ policonsumo2 +
                     comp_bpsc_y3_severe+
                     edad_al_ing_1 + 
                     ano_nac_corr + 
                     susinidumrec_otr +
                     susinidumrec_coc +
                     susinidumrec_pbc +
                     susinidumrec_mar +
                     psycom_dum_study +
                     psycom_dum_with +
                     freq_cons_dum_5day +
                     cond_oc_dum_2inact +
                     cond_oc_dum_3unemp +
                     susprindumrec_coc +
                     susprindumrec_pbc +
                     susprindumrec_mar+
                     susprindumrec_otr, current_data)
mestr <- geex::m_estimate(estFUN = geex_ef,                                       # Function of estimating functions
                          data = current_data,                                             # Data to be used (must be data frame)
                          root_control = setup_root_control(start = svs)) c(-2,rep(0,17))))   # Set starting values

beta_geex <- roots(mestr)             # Extract roots
se_geex <- sqrt(diag(vcov(mestr)))    # Extract finite sample variance and take sqrt to get se

ests_geex <-as.data.frame(cbind("beta"=beta_geex,
                                "se"=se_geex)) %>%
  mutate(lcl = beta - 1.96*se,
         ucl = beta + 1.96*se)

row.names(ests_geex) <- intlist

print("M-Estimation, by geex")
print(round(ests_geex,3))

#https://arxiv.org/pdf/1709.01413
gee_estfun <- function(data, formula, family) {
  X <- model.matrix(object = formula, data = data)
  Y <- model.response(model.frame(formula = formula, data = data))
  n <- nrow(X)
  
  function(theta, alpha, psi) {
    mu <- drop(family$linkinv(X %*% theta))
    
    # Cálculo de Dt
    Dt <- crossprod(X, diag(mu, nrow = n))
    
    # Cálculo de W
    W <- diag(family$variance(mu), nrow = n)
    
    # Cálculo de R
    R <- matrix(alpha, nrow = n, ncol = n)
    diag(R) <- 1
    
    # Cálculo de V
    V <- psi * (sqrt(W) %*% R %*% sqrt(W))
    
    # Resultado final
    return(Dt %*% solve(V, (Y - mu)))
  }
}

# Ejecución de GEE utilizando la librería 'gee'
g <- gee::gee(tr_outcome ~ policonsumo2 +
                comp_bpsc_y3_severe+
                edad_al_ing_1 + 
                ano_nac_corr + 
                susinidumrec_otr +
                susinidumrec_coc +
                susinidumrec_pbc +
                susinidumrec_mar +
                psycom_dum_study +
                psycom_dum_with +
                freq_cons_dum_5day +
                cond_oc_dum_2inact +
                cond_oc_dum_3unemp +
                susprindumrec_coc +
                susprindumrec_pbc +
                susprindumrec_mar+
                susprindumrec_otr, id = id, data = current_data, family=poisson(), corstr = "independence")

# Ejecución de m_estimate con los parámetros especificados
results <- m_estimate(
  estFUN = gee_estfun,
  data = current_data,
  units = "id",
  roots = coef(g),
  compute_roots = FALSE,
  outer_args = list(
    formula = tr_outcome ~ policonsumo2 +
      comp_bpsc_y3_severe+
      edad_al_ing_1 + 
      ano_nac_corr + 
      susinidumrec_otr +
      susinidumrec_coc +
      susinidumrec_pbc +
      susinidumrec_mar +
      psycom_dum_study +
      psycom_dum_with +
      freq_cons_dum_5day +
      cond_oc_dum_2inact +
      cond_oc_dum_3unemp +
      susprindumrec_coc +
      susprindumrec_pbc +
      susprindumrec_mar+
      susprindumrec_otr,
    family = "poisson"
  ),
  inner_args = list(
    alpha = g$working.correlation[1, 2],
    psi = g$scale
  )
)

geex_ef <- function(data){               # Function of estimating functions (to be used in geex::m_estimate)
  current_data<- data
  function(theta){
    p <- plogis(theta[1]+ theta[2]*current_data$policonsumo2 +
                  theta[3]*current_data$comp_bpsc_y3_severe+
                  theta[4]*current_data$sc_edad_al_ing_1 + 
                  theta[5]*current_data$sc_ano_nac_corr + 
                  theta[6]*current_data$susinidumrec_otr +
                  theta[7]*current_data$susinidumrec_coc +
                  theta[8]*current_data$susinidumrec_pbc +
                  theta[9]*current_data$susinidumrec_mar +
                  theta[10]*current_data$psycom_dum_study +
                  theta[11]*current_data$psycom_dum_with +
                  theta[12]*current_data$freq_cons_dum_5day +
                  theta[13]*current_data$cond_oc_dum_2inact +
                  theta[14]*current_data$cond_oc_dum_3unemp +
                  theta[15]*current_data$susprindumrec_coc +
                  theta[16]*current_data$susprindumrec_pbc +
                  theta[17]*current_data$susprindumrec_mar+
                  theta[18]*current_data$susprindumrec_otr)
    ef_1 <- (current_data$tr_outcome - p)
    ef_2 <- (current_data$tr_outcome - p)*current_data$policonsumo2
    ef_3 <- (current_data$tr_outcome - p)*current_data$comp_bpsc_y3_severe
    ef_4 <- (current_data$tr_outcome - p)*current_data$sc_edad_al_ing_1
    ef_5 <- (current_data$tr_outcome - p)*current_data$sc_ano_nac_corr
    ef_6 <- (current_data$tr_outcome - p)*current_data$susinidumrec_otr
    ef_7 <- (current_data$tr_outcome - p)*current_data$susinidumrec_coc
    ef_8 <- (current_data$tr_outcome - p)*current_data$susinidumrec_pbc
    ef_9 <- (current_data$tr_outcome - p)*current_data$susinidumrec_mar
    ef_10 <- (current_data$tr_outcome - p)*current_data$psycom_dum_study
    ef_11 <- (current_data$tr_outcome - p)*current_data$psycom_dum_with
    ef_12 <- (current_data$tr_outcome - p)*current_data$freq_cons_dum_5day
    ef_13 <- (current_data$tr_outcome - p)*current_data$cond_oc_dum_2inact
    ef_14 <- (current_data$tr_outcome - p)*current_data$cond_oc_dum_3unemp
    ef_15 <- (current_data$tr_outcome - p)*current_data$susprindumrec_coc
    ef_16 <- (current_data$tr_outcome - p)*current_data$susprindumrec_pbc
    ef_17 <- (current_data$tr_outcome - p)*current_data$susprindumrec_mar
    ef_18 <- (current_data$tr_outcome - p)*current_data$susprindumrec_otr
    c(ef_1, ef_2, ef_3, ef_4, ef_5, ef_6, ef_7, ef_8, ef_9, ef_10, ef_11, ef_12, ef_13, ef_14, ef_15, ef_16, ef_17, ef_18) 
  }
}


mestr <- geex::m_estimate(estFUN = geex_ef,                                       # Function of estimating functions
                          data = current_data,                                             # Data to be used (must be data frame)
                          root_control = setup_root_control(start = c(-2,rep(0,17))))   # Set starting values

beta_geex <- roots(mestr)             # Extract roots
se_geex <- sqrt(diag(vcov(mestr)))    # Extract finite sample variance and take sqrt to get se

ests_geex <-as.data.frame(cbind("beta"=beta_geex,
                                "se"=se_geex)) %>%
  mutate(lcl = beta - 1.96*se,
         ucl = beta + 1.96*se)

row.names(ests_geex) <- intlist

print("M-Estimation, by geex")
print(round(ests_geex,3))