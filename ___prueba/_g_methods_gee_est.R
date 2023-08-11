#Paquetes necesarios para generar los análisis
install.packages("geepack")
library()
library(survey)
library(ipw)
library(reshape)
library(MuMIn)
#Estimamos el modelo de probabilidad inversa con el numerador (elementos invariantes en el tiempo) y el denominador todos los confusores de interés.
w <- ipwtm(
  exposure = t,
  family = "binomial",
  link = "logit",
  # Time invariant stuff
  numerator = ~ factor(policonsumo) + edad_ini,
  # All confounders
  denominator = ~ v + factor(policonsumo) + edad_ini,
  id = id,
  timevar=time,
  type="first",
  data = datos)
summary(w$ipw.weights)
#Se incorporan los pesos a la base de datos
datos<-
  dplyr::bind_cols(datos, ipw=w$ipw.weights)
###########################################################################################
# Para obtener las salidas de los coeficientes
###########################################################################################
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
###########################################################################################
# Primer modelo GEE - GEE con errores estándar robustos  (no IPTW)
###########################################################################################
mod1<-geem(dt~ t+ time+ factor(policonsumo)+ edad_ini, id=id ,data = data.frame(datos), family=poisson,
           corstr="ar1") 
summary(mod1)
summ_mod1<-
  cbind.data.frame(cc=summ$coefnames,
                   Estimate=summ$beta,Std.err=summ$se.robust) %>% 
  dplyr::mutate(lwr=Estimate-qnorm((1+0.95)/2)*Std.err,upr=Estimate+qnorm((1+0.95)/2)*Std.err) %>% 
  dplyr::mutate(across(where(is.numeric),~round(exp(.),2)))
invisible("Quasipoisson")
mod2<-geem(dt~ t+ time+ factor(policonsumo)+ edad_ini, id=id, data = datos, 
           family = MASS::negative.binomial(2), corstr = "ar1")  #The known value of the additional parameter, theta.
mod3<-geem(dt~ t+ time+ factor(policonsumo)+ edad_ini, id=id, data = datos, 
           family = MASS::negative.binomial(1), corstr = "ar1") 
mod4<-geepack::geese(dt~ t+ time+ factor(policonsumo)+ edad_ini, id=id, data = datos, 
                     corstr = "ar1", family=poisson)


install.packages("geecure")
glimpse(ms_d_match_surv)

geetonsilexch <- geecure2(Surv(Time, Status) ~ Sex + factor(Grade) +Age + Cond + T,
                          cureform = ~ Sex + factor(Grade) + Age + Cond + T, data = ms_d_match_surv,
                          id = id, corstr = "exchangeable", stdz = TRUE, Var = FALSE)