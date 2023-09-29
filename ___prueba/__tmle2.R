#Petersen, M., Schwab, J., Gruber, S., Blaser, N., Schomaker, M., & van der Laan, M. (2014). 
#Targeted Maximum Likelihood Estimation for Dynamic and Static Longitudinal Marginal Structural 
#Working Models. Journal of causal inference, 2(2), 147–185. https://doi.org/10.1515/jci-2013-0007

rexpit <- function(x) rbinom(n=length(x), size=1, prob=plogis(x))
n <- 5000
time.points <- 5
prev.L <- rnorm(n)
prev.A <- rep(0, n)
sum.A <- rep(0, n)
data2 <- data.frame(L_0 = prev.L)
for (t in 1:time.points) {
  L <- 0.1 * prev.L + 0.3 * prev.A + rnorm(n)
  A <- rexpit(L)
  
  data1 <- data.frame(L, A)
  names(data1) <- paste0(c("L_", "A_"), t)
  data2 <- cbind(data2, data1)
  
  prev.A <- A
  prev.L <- L
  
  sum.A <- sum.A + A
}
data2$Y <- rexpit(sum.A / time.points + L)
head(data2)

#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_
#https://remlapmot.github.io/cibookex-r/
library(here)

dataurls <- list()
stub <- "https://cdn1.sph.harvard.edu/wp-content/uploads/sites/1268/"
dataurls[[1]] <- paste0(stub, "2012/10/nhefs_sas.zip")
dataurls[[2]] <- paste0(stub, "2012/10/nhefs_stata.zip")
dataurls[[3]] <- paste0(stub, "2017/01/nhefs_excel.zip")
dataurls[[4]] <- paste0(stub, "1268/20/nhefs.csv")

temp <- tempfile()
for (i in 1:3) {
  download.file(dataurls[[i]], temp)
  unzip(temp, exdir = "data")
}

download.file(dataurls[[4]], here("data", "nhefs.csv"))

nhefs <- read_excel(here("data", "NHEFS.xls"))

nhefs$survtime <- ifelse(nhefs$death==0, NA, (nhefs$yrdth-83)*12+nhefs$modth) # * yrdth ranges from 83 to 92

# model to estimate E[A|L]
modelA <- glm(qsmk ~ sex + race + age + I(age*age)
              + as.factor(education) + smokeintensity
              + I(smokeintensity*smokeintensity) + smokeyrs
              + I(smokeyrs*smokeyrs) + as.factor(exercise)
              + as.factor(active) + wt71 + I(wt71*wt71),
              data=nhefs, family=binomial())

nhefs$p.qsmk <- predict(modelA, nhefs, type="response") 
d <- nhefs[!is.na(nhefs$survtime),] # select only those with observed death time
n <- nrow(d)

# define the estimating function that needs to be minimized
sumeef <- function(psi){
  
  # creation of delta indicator
  if (psi>=0){
    delta <- ifelse(d$qsmk==0 | 
                      (d$qsmk==1 & psi <= log(120/d$survtime)), 
                    1, 0)
  } else if (psi < 0) {
    delta <- ifelse(d$qsmk==1 | 
                      (d$qsmk==0 & psi > log(d$survtime/120)), 1, 0)
  }
  
  smat <- delta*(d$qsmk-d$p.qsmk)
  sval <- sum(smat, na.rm=T)
  save <- sval/n
  smat <- smat - rep(save, n)
  
  # covariance
  sigma <- t(smat) %*% smat
  if (sigma == 0){
    sigma <- 1e-16
  }
  estimeq <- sval*solve(sigma)*t(sval)
  return(estimeq)
}

res <- optimize(sumeef, interval = c(-0.2,0.2))
psi1 <- res$minimum
objfunc <- as.numeric(res$objective)


# Use simple bisection method to find estimates of lower and upper 95% confidence bounds
increm <- 0.1
for_conf <- function(x){
  return(sumeef(x) - 3.84)
}

if (objfunc < 3.84){
  # Find estimate of where sumeef(x) > 3.84
  
  # Lower bound of 95% CI
  psilow <- psi1
  testlow <- objfunc
  countlow <- 0
  while (testlow < 3.84 & countlow < 100){
    psilow <- psilow - increm
    testlow <- sumeef(psilow)
    countlow <- countlow + 1
  }
  
  # Upper bound of 95% CI
  psihigh <- psi1
  testhigh <- objfunc
  counthigh <- 0
  while (testhigh < 3.84 & counthigh < 100){
    psihigh <- psihigh + increm
    testhigh <- sumeef(psihigh)
    counthigh <- counthigh + 1
  }
  
  # Better estimate using bisection method
  if ((testhigh > 3.84) & (testlow > 3.84)){
    
    # Bisection method
    left <- psi1
    fleft <- objfunc - 3.84
    right <- psihigh
    fright <- testhigh - 3.84
    middle <- (left  + right) / 2
    fmiddle <- for_conf(middle)
    count <- 0
    diff <- right - left
    
    while (!(abs(fmiddle) < 0.0001 | diff < 0.0001 | count > 100)){
      test <- fmiddle * fleft
      if (test < 0){
        right <- middle
        fright <- fmiddle
      } else {
        left <- middle
        fleft <- fmiddle
      }
      middle <- (left + right) / 2
      fmiddle <- for_conf(middle)
      count <- count + 1
      diff <- right - left
    }
    
    psi_high <- middle
    objfunc_high <- fmiddle + 3.84
    
    # lower bound of 95% CI
    left <- psilow
    fleft <- testlow - 3.84
    right <- psi1
    fright <- objfunc - 3.84
    middle <- (left + right) / 2
    fmiddle <- for_conf(middle)
    count <- 0
    diff <- right - left
    
    while(!(abs(fmiddle) < 0.0001 | diff < 0.0001 | count > 100)){
      test <- fmiddle * fleft
      if (test < 0){
        right <- middle
        fright <- fmiddle
      } else {
        left <- middle
        fleft <- fmiddle
      }
      middle <- (left + right) / 2
      fmiddle <- for_conf(middle)
      diff <- right - left
      count <- count + 1
    }
    psi_low <- middle
    objfunc_low <- fmiddle + 3.84
    psi <- psi1
  }
}
c(psi, psi_low, psi_high)
