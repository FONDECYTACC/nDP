# input theta
# alphafn
# gamma[1], gamma[2]
# id
datagen1 <- function(id,theta,alphafn,gamma){
  K1 <- rnorm(1,3,sqrt(0.5))
  K2 <- rbinom(1,1,0.55)
  K3 <- rnorm(1,-1.2,sqrt(0.5))
  M <- rbind(c(1,0,0),c(0,1,0),c(0,0,1))
  M <- diag(c(sqrt(1.8),sqrt(1.8),sqrt(1.8)))%*%M%*% diag(c(sqrt(1.8),sqrt(1.8),sqrt(1.8)))
  REvec <- mvrnorm(1,rep(0,3),M)
  Zfirst <- rnorm(1,4,2) + 0
  pI <- 1/(1+exp(-(0.5 + 0.2*Zfirst)))
  I <- rbinom(1,1,pI)
  ZI1 <- rnorm(1,2,1)
  ZI0 <- rnorm(1,4,2)
  Z <- I*ZI1 + (1-I)*ZI0 + 0
  t <- (1:200)/100
  K1t <- K1 + t
  K2t <- rep(K2,200)
  K3errors <- rnorm(200,0,sqrt(0.005))
  mat <- matrix(array(1,dim=c(200,200)),ncol=200)
  mat[upper.tri(mat)] <- 0
  K3t <- K3 + mat
  Zt1 <- rnorm(201,2,1)
  Zt0 <- rnorm(201,4,2)
  It <- I; Zt <- Z
  for(i in 1:200){
    pIt <- 1/(1+exp(-(0.5+0.2*Zt[i] - 1.5*It[i])))
    It <- c(It,rbinom(1,1,pIt))
    Zt <- c(Zt,It[i+1]*Zt1[i+1] + (1-It[i+1])*Zt0[i+1] + 0)
  }
  It <- It[-1]; Zt <- Zt[-1]
  psi <- rnorm(1,0,0.2)
  epsilon <- rnorm(201,0,0.1) + psi
  eta <- rgamma(1,10,10)
  mu <- theta*(eta-1) + REvec[1]
  muI <- theta*(eta-1) + REvec[2]
  alphat <- alphafn(t)
  K1t <- c(K1,K1t); K2t <- c(K2,K2t); K3t <- c(K3,K3t)
  It <- c(I,It); alphat <- c(alphafn(0),alphat); Zt <- c(Z,Zt)
  Yt <- mu + alphat + (2+ muI)*It - 4*(Zt - 2*It - 4*(1-It)) + epsilon
  lambdat <- 0.01*eta*exp(gamma[1]*It + gamma[2]*Zt)
  lambdat[lambdat>1] <- 1
  lastZt <- c(Zfirst,Zt[-201])
  datai <- cbind(rep(id,201),c(0,t),K1t,K2t,K3t,It,alphat,Zt,lastZt,Yt,lambdat)
  return(datai)
}
alphafn <- function(t){return(t)}
datagen <- function(nsubj,theta,alphafn,gamma){
  datalist <- lapply(1:nsubj,datagen1,theta=theta,alphafn=alphafn,gamma=gamma)
  datawide <- t(matrix(unlist(datalist),ncol=201,byrow=TRUE))
  data <- as.vector(datawide[,(0:(nsubj-1))*11+1])
  data <- as.data.frame(data); names(data) <- "id"
  data$time <- as.vector(datawide[,(0:(nsubj-1))*11+2])
  data$K1 <- as.vector(datawide[,(0:(nsubj-1))*11+3])
  data$K2 <- as.vector(datawide[,(0:(nsubj-1))*11+4])
  data$K3 <- as.vector(datawide[,(0:(nsubj-1))*11+5])
  data$I <- as.vector(datawide[,(0:(nsubj-1))*11+6])
  data$alpha <- as.vector(datawide[,(0:(nsubj-1))*11+7])
  data$Z <- as.vector(datawide[,(0:(nsubj-1))*11+8])
  data$lastZ <- as.vector(datawide[,(0:(nsubj-1))*11+9])
  data$Y <- as.vector(datawide[,(0:(nsubj-1))*11+10])
  data$lambda <- as.vector(datawide[,(0:(nsubj-1))*11+11])
  obsgen <- function(rate){
    return(rbinom(1,1,rate))
  }
  data$obs <- unlist(lapply(data$lambda,obsgen))
  data$Y[data$obs==0] <- NA
  return(data)
}
analysis.ipw.iiw <- function(data){
  data$ipw <- ipw(data)
  data$iiw <- iiw(data)
  data$weight <- data$ipw*data$iiw
  perc99 <- quantile(data$weight[data$obs==1],probs=0.99)
  data$weight.trunc <- data$weight
  data$weight.trunc[data$weight>perc99] <- perc99
  data$weight.trunc <- data$weight.trunc/mean(data$weight.trunc)
  data$mo.prob <- data$weight.trunc/max(data$weight.trunc)
  tertile <- c(quantile(data$time,prob=1/3),quantile(data$time,prob=2/3))
  m <- lm(Y ~ bs(time,degree=3,knots=c(tertile)) + I,weights=weight,data=data)
  V <- vcovCL(m, cluster = data$id)
  beta <- m$coef[length(m$coef)]
  se <- sqrt(V[nrow(V),ncol(V)])
  m <- lm(Y ~ bs(time,degree=3,knots=c(tertile)) + I,weights=weight.trunc,data=data)
  V <- vcovCL(m, cluster = data$id)
  beta <- c(beta,m$coef[length(m$coef)])
  se <- c(se,sqrt(V[nrow(V),ncol(V)]))
  return(c(beta,se))
}

ipw <- function(data){
  data$lastI <- c(NA,data$I[1:(nrow(data)-1)])
  data$lastI[data$time==0] <- 0
  data$lastZ[data$time==0] <- 0
  m <- glm(I ~ lastZ + lastI, data=data, family="binomial")
  prob <- predict(m, type="response")
  weight <- data$I/prob + (1-data$I)/(1-prob)
  return(weight)
}

iiw <- function(data){
  m <- glm(obs ~ I + Z, data=data, family="poisson")
  intensity <- predict(m, type="response")
  m <- glm(obs ~ 1, data=data, family="poisson")
  intensity.stab <- predict(m, type="response")
  return(intensity.stab/intensity)
}

iiwmo <- function(data){
  datacox <- data
  datacox$tlast <- c(NA,datacox$time[1:(nrow(datacox)-1)])
  datacox$tlast[datacox$time==0] <- NA
  m <- coxph(Surv(tlast,time,obs) ~ I + Z + frailty(id), data=datacox)
  datacox$lp <- drop(cbind(datacox$I, datacox$Z) #* intensity)
  intensity <- exp(datacox$lp)
  b <- basehaz(m, centered=FALSE)
  Lambda0 <- max(b$haz)/2
  mi <- tapply(data$obs, data$id, sum)
  Lambda <- tapply(exp(datacox$lp)*0.01*Lambda0, data$id, sum)
  theta <- m$history$`frailty(id)`$history[,1]
  sigmasq <- theta[length(theta)]
  r <- (mi + 1/sigmasq)/(Lambda+1/sigmasq)
  return(list(weight=1/intensity, sigmasq=sigmasq, eta=r))
}

analysisfn <- function(data){
  ipw.iiw <- analysis.ipw.iiw(data)
  return(ipw.iiw)
}

sim1fn <- function(it, nsubj, theta, alphafn, gamma){
  data <- datagen(nsubj=nsubj, theta=theta, alphafn=alphafn, gamma=gamma)
  res <- try(analysis.ipw.iiw(data))
  while(class(res) == "try-error"){
    data <- datagen(nsubj=nsubj, theta=theta, alphafn=alphafn, gamma=gamma)
    res <- try(analysis.ipw.iiw(data))
  }
  return(res)
}
# Proposed analysis: Liang with MO for weights
moLiang <- function(data){
  data$ipw <- ipw(data)
  i <- iiwmo(data)
  data$iiw <- i$weight
  data$weight <- data$ipw * data$iiw
  perc99 <- quantile(data$weight[data$obs==1], probs=0.99)
  data$weight.trunc <- data$weight
  data$weight.trunc[data$weight > perc99] <- perc99
  data$weight.trunc <- data$weight.trunc/mean(data$weight.trunc)
  data$mo.prob <- data$weight.trunc/max(data$weight.trunc)
  data$intercept <- 1
  data$eta <- NA
  data$eta <- i$eta[data$id]
  moest <- mo(20, Liangcausal, data=data[data$obs==1,], weights=data$weight.trunc[data$obs==1], singleobs=FALSE, id="id", time="time", keep.first=FALSE, var=FALSE, Yname="Y", Xnames="I", Wnames=c("intercept", "I"), id.Liang="id", time.Liang="time", Xfn=Xfn, datafull=data, sigmasq=i$sigmasq)
  return(moest$est)
}

simbothfn <- function(it, nsubj, theta, alphafn, gamma){
  data <- datagen(nsubj=nsubj, theta=theta, alphafn=alphafn, gamma=gamma)
  res <- try(analysis.ipw.iiw(data))
  while(class(res) == "try-error"){
    data <- datagen(nsubj=nsubj, theta=theta, alphafn=alphafn, gamma=gamma)
    res <- try(analysis.ipw.iiw(data))
  }
  resmo <- try(moLiang(data))
  if(class(resmo) == "try-error") resmo <- rep(NA, times=5)
    resmo.mod <- try(moLiangmod(data))
  if(class(resmo.mod)=="try-error") resmo.mod <- rep(NA,times=7)
  return(list(iiwest=res, iiwmoest=resmo))
}

Xfn <- function(id, time, datafull){
  return(datafull$I[datafull$id == id & datafull$time == time])
}

Wfn <- function(id, time, datafull){
  return(c(1, datafull$I[datafull$id == id & datafull$time == time]))
}

Liangcausal <- function(data,Yname,Xnames,Wnames,id.Liang,time.Liang,Xfn,Wfn,datafull,sigmasq){
  est <- Liangforsim(data=data,Yname=Yname,Xnames=Xnames,Wnames=Wnames,id=id.Liang,time=time.Liang,maxfu=2,baseline=FALSE,Xfn = Xfn,Wfn = Wfn,datafull,sigmasq=sigmasq)
  return(est)
}
# DESDE AQUÍ HAY QUE EMPEZAR A CORREGIR
Liangforsim <-
  function(data,datafull, Yname, Xnames, Wnames, Znames = NULL, formulaobs = NULL,
           id, time, invariant = NULL, lagvars = NULL, lagfirst = NULL,
           maxfu, baseline, n.knots = NULL, kappa = NULL, Xfn = NULL,
           Wfn = NULL,sigmasq=NULL)
  {
    nfull <- length(table(datafull$id)); # print("nfull"); # print(nfull)
    # print("nobs post mo"); # print(sum(data$obs))
    # print("nobs pre mo"); # print(sum(datafull$obs))
    # print(head(data))
    mjfull <- rep(0,nfull)
    ids <- as.numeric(names(table(data[, names(data)
    idsfull <- as.numeric(names(table(datafull[, names(datafull)
#CORREGIR: MALO   
    mjfull[idsfull
              id], length) - baseline
              for(i in 1:nrow(datafull)){datafull$mjfull[i] <- mjfull[idsfull==datafull$id[i]]}
              datafull$mX <- datafull$I*datafull$mjfull
              mXbar <- tapply(datafull$mX,datafull$time,sum)
              times <- as.numeric(names(table(datafull$time)))
              # # print("times"); # print(times)
              data$mXbar <- NA
              for(i in 1:nrow(data)){data$mXbar[i] <- mXbar[times==data$time[i]]}
              data$Xbar <- data$mXbar/sum(mjfull)
              if (is.null(formulaobs)) {
              fn <- function(t, tvec) return(which.min(abs(t - tvec)))
              ids <- names(table(data[, names(data)
              idnum <- array(dim = nrow(data))
              for (i in 1:nrow(data)) idnum[i] <- (1:length(ids))[data[i,
              names(data)
              
              if (is.data.frame(maxfu)) {
              maxfu.use <- maxfu
              for (i in 1:nrow(maxfu)) {
              maxfu.use[i, names(maxfu)
              names(maxfu)
              }
              }
              data[, names(data)
              if (is.null(maxfu)) {
              maxtable <- tapply(data[, names(data)
              data[, names(data)
              maxfu.use <- cbind(1:length(maxtable), maxtable +
              max(maxtable) * 0.001)
              }
              n <- length(table(data[, names(data)
              mi <- tapply(data[, names(data)
              id], length) - baseline
              Xcols <- (1:ncol(data))[is.finite(match(names(data),
              Xnames))]
              Wcols <- (1:ncol(data))[is.finite(match(names(data),
              Wnames))]
              
              X <- array(data.matrix(data[, Xcols]), dim = c(nrow(data),
              length(Xnames)))
              W <- array(data.matrix(data[, Wcols]), dim = c(nrow(data),
              length(Wnames)))
              if (length(maxfu) == 1)
              maxfu.use <- cbind(idnum, rep(maxfu, length(idnum)))
              maxfu.use <- maxfu.use[order(maxfu.use[, 1]), ]
              data <- data[order(idnum), ]
              if (length(maxfu) == 1) {
              Lambdahat <- nrow(data)/nfull
              sigmahatsq <- max((sum(mi
              {2}) - sum(mi) - nfull * Lambdahat
              {2})/(nfull *
              Lambdahat
              {2}), 0)
              Lambdahat.scalar <- Lambdahat
              Lambdahat <- rep(Lambdahat, n)
              Ci <- rep(maxfu, n)
              # print("Lambdahat"); # print(Lambdahat)
              # print("sigma"); # print(sigmahatsq)
              if(!is.null(sigmasq)) sigmahatsq <- sigmasq
              # print("sigma"); # print(sigmahatsq)
              }
              if (length(maxfu) > 1) {
              maxfu.use <- maxfu.use[order(maxfu[, 1]), ]
              ids <- as.numeric(names(table(data[, names(data)
              id])))
              Ci <- as.vector(maxfu.use[order(maxfu.use[, 1]),
              2])
              data$event <- 1
              lagcols <- (1:ncol(data))[is.finite(match(names(data),
              time))]
              invarcols <- (1:ncol(data))[is.finite(match(names(data),
              id))]
              
              datacox <- addcensoredrows(data = data, maxfu = maxfu.use,
              tinvarcols = invarcols, id = id, time = time,
              event = "event")
              datacox <- lagfn(datacox, "time", id, time)
              formulanull <- Surv(time.lag, time, event) ∼ 1
              datacox <- datacox[datacox[, names(datacox)
              time] > 0, ]
              b <- basehaz(coxph(formulanull, data = datacox))
              indexfnnocov <- function(t, time) {
              return(sum(time < t))
              }
              bindex <- sapply(Ci, indexfnnocov, time = b$time)
              bindex[bindex == 0] <- 1
              Lambdahat <- b$hazard[bindex]
              sigmahatsq <- max((sum(mi
              2) - sum(mi) - nfull*(Lambdahat.scalar
              2))/(nfull*Lambdahat.scalar
              2),
              0)
              # print("sigmasq"); # print(sigmasq)
              }
              mi.Lambdahat <- mi/Lambdahat
              mi.Lambdahat[mi == 0 & Lambdahat == 0] <- 1
              W <- cbind(rep(1,nrow(data)),data$I); # print("W"); # print(head(W))
              Bhat <- array(dim = c(nrow(data), ncol(W)))
              Bbar <- Bhat
              Xbar <- array(dim = c(nrow(data), ncol(X)))
              Bmultiplier <- array(dim = nrow(data))
              Bmultiplierfull <- array(dim = nrow(datafull))
              Bmultid <- (mi - Lambdahat) * sigmahatsq/(1 + Lambdahat *
              sigmahatsq)
              Bmultidfull <- (mjfull - Lambdahat.scalar) * sigmahatsq/(1 + Lambdahat.scalar *
              sigmahatsq)
              ids <- as.numeric(names(table(data$id)))
              for (i in 1:n) Bmultiplier[data[, names(data)
              for (i in 1:nfull) Bmultiplierfull[data[, names(datafull)
              Bhat <- sweep(array(W, dim = c(nrow(data), ncol(W))),
              1, Bmultiplier, "*")
              # print("eta"); # print(summary(data$eta))
              Wfull <- cbind(rep(1,nrow(datafull)),datafull$I)
              Bhatfull <- sweep(array(Wfull, dim = c(nrow(datafull), ncol(Wfull))),
              1, Bmultiplierfull, "*")
              # 1, datafull$eta-1, "*")
              mBhat <- sweep(Bhatfull,1,datafull$mjfull,"*")
              mBhatbar1 <- tapply(mBhat[,1],datafull$time,sum)
              mBhatbar2 <- tapply(mBhat[,2],datafull$time,sum)
              data$mBhatbar1 <- NA
              data$mBhatbar2 <- NA
              times <- as.numeric(names(table(datafull$time)))
              for(i in 1:nrow(data)){
              data$mBhatbar1[i] <- mBhatbar1[times==data$time[i]]
              data$mBhatbar2[i] <- mBhatbar2[times==data$time[i]]
              }
              data$Bbar1 <- data$mBhatbar1/sum(mjfull)
              data$Bbar2 <- data$mBhatbar2/sum(mjfull)
              Xbar <- data$Xbar
              Bbar <- cbind(data$Bbar1,data$Bbar2)
              # print("Bhat"); # print(summary(Bhat)); # print(head(Bhat))
              # print("Bbar"); # print(summary(Bbar)); # print(head(Bbar))
              regX <- array((X - Xbar), dim = c(nrow(data), ncol(X)))[data[,
              names(data)
              regB <- array(Bhat - Bbar, dim = c(nrow(data), ncol(W)))[data[,
                       names(data)
                       # print(head(regB))
                       regY <- data[, names(data)
                                    time] > 0]
                  regpredictor <- cbind(regX, regB)
                  if (sigmahatsq > 0)
                    beta <- solve(t(regpredictor)
                                  regY)
                  if (sigmahatsq == 0)
                    beta <- c(solve(t(regX)
                                    rep(NA,ncol(W)))
                                         }
                                         # print("betaLiang"); # print(beta)
                                         return(c(beta,sum(data$obs),sum(datafull$obs)))
  }

Liangmodforsim <- function(formula,data,datafull,Wnames,id, time,baseline,sigmahatsq)
  
{
  W <- cbind(rep(1,nrow(data)),data$I)
  nfull <- length(table(datafull$id))
  ids <- as.numeric(names(table(data$id)))
  n <- length(ids)
  Lambdahat <- nrow(data)/nfull
  mi <- tapply(data[, names(data)
                    Bmultiplier <- array(dim = nrow(data))
                    Bmultid <- (mi - Lambdahat) * sigmahatsq/(1 + Lambdahat *
                                                                sigmahatsq)
                    for (i in 1:n) Bmultiplier[data[, names(data)
                                                    Bhat <- sweep(array(W, dim = c(nrow(data), ncol(W))),
                                                                  1, Bmultiplier, "*")
                                                    data$Bhat1 <- Bhat[,1]
                                                    data$Bhat2 <- Bhat[,2]
                                                    m <- geeglm(formula=formula,data=data,id=id)
                                                    # print(summary(m))
                                                    return(c(m$coefficients,sum(data$obs),sum(datafull$obs)))
}

Liangmodcausal <- function(data,Wnames,id.Liang,time.Liang,datafull,sigmahatsq){
  est <- Liangmodforsim(Y ∼ time + I + Bhat1 + Bhat2,data=data,datafull=datafull,Wnames=c("Intercept","I"),id=id.Liang, time=time,baseline=FALSE,sigmahatsq=sigmahatsq)
  return(est)
}

moLiangmod <- function(data){
  data$ipw <- ipw(data)
  i <- iiwmo(data)
  data$iiw <- i$weight
  data$weight <- data$ipw*data$iiw
  perc99 <- quantile(data$weight[data$obs==1],probs=0.99)
  data$weight.trunc <- data$weight
  data$weight.trunc[data$weight>perc99] <- perc99
  data$weight.trunc <- data$weight.trunc/mean(data$weight.trunc)
  data$mo.prob <- data$weight.trunc/max(data$weight.trunc)
  data$intercept <- 1
  moest <- mo(20,Liangmodcausal,data=data[data$obs==1,],weights=data$weight.trunc[data$obs==1],singleobs=FALSE,id="id",time="time",keep.first=FALSE,var=FALSE,Wnames=c("intercept","I"),id.Liang="id",time.Liang="time",datafull=data,sigmahatsq=i$sigmasq)
  return(moest$est)
}

# input theta
# alphafn
# gamma[1], gamma[2]
# id
library(splines)
library(geepack)
library(lme4)
library(IrregLong)
library(sandwich)
library(coxme)
library(MASS)
gamma <- c(0.6,0.3)
theta <- 1
n <- 250
source("∼/ObsPatientVisit/Causal/causalmocore.depZ.indRE.R")
set.seed(3010311)
res <- lapply(1:100,simbothfn,theta=theta,alphafn=alphafn,gamma=gamma,nsubj=250)
dput(res,"res1.Robject")


