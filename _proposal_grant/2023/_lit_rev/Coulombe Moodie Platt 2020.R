

###################################################################################
##
## December 2019
##
## R code to run the simulations and to obtain estimates from the estimators  
## introduced in the paper Weighted Regression Analysis to Correct for Informative 
## Monitoring Times and Confounders in Longitudinal Studies
##
## Authors: Janie Coulombe, Erica E.M. Moodie, Robert Platt
##
###################################################################################


library(splines)
library(survival)
library(MASS)

## Initialize each vector of estimates #############################################################

vec_FIPTM<-c() ## estimates from FIPTM
vec_IPCTM<-c() ## estimates from IPCTM
sandwich<-c()  ## asymptotic variance of FIPTM estimator

## Parameters to set: ###############################################################################

SAMPSIZE<- 250 ## Number of patients in each simulation (sample size)
NBSIMUL <- 500 ## Number of simulations

gamma1<- 0     ## gamma1 and gamma2 are visit process parameters: gamma1d is for X
gamma2<-0      ## gamma1 and gamma2 are visit process parameters: gamma2d is for Z

alpha0t<- 5    ## to choose the intercept function. You have the following choices:

## if(alpha0t==1){alpha_0<-sqrt(time_discrete)}              --> squared root of the time
## if(alpha0t==2){alpha_0<-sin(time_discrete)}               --> sinus of the time
## if(alpha0t==3){alpha_0<-exp(2*abs(sin(3*time_discrete)))} --> complex function of time
## if(alpha0t==4){alpha_0<-2.5*time_discrete}                --> linear function of time
## if(alpha0t==5){alpha_0<-3}                                --> constant

beta0=1;       ## Parameter beta0 for the X in the outcome model
beta2=3;       ## Parameter beta2 for the Z-E(Z|X) in the outcome model
TAU=2;         ## Maximum follow-up time (2 corresponds to 200 potential time points since time discretized to 0.01)

mean_etad=1;   ## mean_etad and var_etad are parameters for the visit model
var_etad=0.01; 
meanphid=0;    ## meanphid, sigmaphid, sigmaepsilond are parameters for the residuals model
sigmaphid=0.2; 
sigmaepsilond=0.1;

mu1d=4;        ## mu1d, sigma_1d, mu2d, sigma2_2d are parameters for simulating the X and Z covariates
sigma2_1d=4; 
mu2d=2; 
sigma2_2d=1; 

agemean=1;     ## agemean and agesd are used to simulate the first confounder variable
agesd=1; 	
p_male=0.55;   ## p_male is used to simulated the second confounder variable
heightmean=0;  ## heightmean and heightsd are used to simulate the third confounder variable
heightsd=1;  

beta_t0=0.5    ## all coefficients for treatment model (beta0, beta1, beta2, beta3)    
beta_t1=0.8  
beta_t2=0.05    
beta_t3=-1

beta_o1=0.4    ## and outcome model (beta10, beta11, beta12)
beta_o2=0.05 
beta_o3=-0.6

nb_visits<-c() ## to counts the number of visits (records) per patient


## SIMULATIONS ######################################################################################

for( simul  in 1:NBSIMUL ){ 
  
  print(paste("Sim no.=",simul  ,sep="")) ## to print the simulation number we are at
  
  ## Discretize time  
  time_discrete<-seq(0.01,TAU,0.01)
  nb<-length(time_discrete) ## Keep the length for when we simulate each variable with the same length
  
  ## Create en empty matrix in which to put all variables 	
  dattotal<-matrix(NA,nrow=1,ncol=9)
  colnames(dattotal)<-c('X','Z','outcome','time_discrete','ID','age','sex','height','visit')
  ## Age, sex and height are the 3 confounder variables and visit will be an indicator of visit
  
  #######################################
  ## Simulate data for each individual ##
  #######################################
  
  ## Loop on all individuals 
  for(indiv in 1:SAMPSIZE){
    
    ## Simulate covariates
    age<-rnorm(mean=agemean,sd=agesd,n=1)  
    sex<-rbinom(p=p_male,n=1,size=1)
    height<-rnorm(mean=heightmean,sd=heightsd,n=1)
    
    ## Treatment model
    p_treated= exp(beta_t0+beta_t1*age+beta_t2*sex+beta_t3*height)/(1+exp(beta_t0+beta_t1*age+beta_t2*sex+beta_t3*height)) 
    X<- rep(rbinom(p=p_treated[1],n=1,size=1),nb) ## treatment variable
    
    ## Simulation of the mediator Z that depends on X
    Z_a<-rnorm(mean=mu1d,sd=sqrt(sigma2_1d),n=nb) ## if X=0, mediator Z
    Z_b<-rnorm(mean=mu2d,sd=sqrt(sigma2_2d),n=nb) ## if X=1, mediator Z
    Z<- ifelse(X==1,Z_b,Z_a)
    meanZ<-ifelse(X==1,mean(Z_b),mean(Z_a))       ## In the outcome model, Z is re-centered by conditional mean 
    
    ## Outcome model
    phi<-rnorm(mean=meanphid,sd=sigmaphid,n=1)
    epsilon<-rnorm(mean=phi,sd=sigmaepsilond,n=nb)
    
    ## Set the alpha_0(t) model for the intercept that may vary with time
    if(alpha0t==1){alpha_0<-sqrt(time_discrete)}
    if(alpha0t==2){alpha_0<-sin(time_discrete)}
    if(alpha0t==3){alpha_0<-exp(2*abs(sin(3*time_discrete)))}
    if(alpha0t==4){alpha_0<-2.5*time_discrete}
    if(alpha0t==5){alpha_0<-3} 
    
    ## Simulate the outcome
    outcome<- alpha_0 + beta0*X + beta2*(Z-meanZ ) + beta_o1*age + beta_o2*sex+ beta_o3*height + epsilon
    
    ## Visit times model 
    beta_etad<- var_etad/mean_etad
    alpha_etad<-mean_etad/beta_etad
    eta<-rgamma(n=1,shape=alpha_etad,scale= beta_etad)
    ratei<-eta*exp(gamma1*X + gamma2*Z)*0.01 ## We multiplied by 0.01 so that it has sensible values in the end 
    
    ## Indicator of visit: either 0 or 1, and depends on X and Z via the ratei variable
    visit<-c()
    for(vis in 1:length(time_discrete)){visit<- append(visit, rbinom(size=1,n=1,prob=ratei[vis ]))}
    
    ## Combine all variables together 
    dat1<-data.frame(cbind(X,Z,outcome,time_discrete, c(rep(indiv,length(X))),age,sex,height,visit))
    colnames(dat1)[5]<-'ID'
    dat2<-dat1[dat1$time_discrete<=TAU,] ## Keep only the data before time=TAU which is the max follow-up time
    
    ## Simulate censoring indicator + stop follow-up there
    censo<-runif(min=TAU/2,max=TAU,n=1)
    dat3<-dat2[dat2$time_discrete<=censo,]
    
    ## Create one dataset with only the visits, to count the average number of visits per individuals
    datafinal<-dat3[dat3$visit==1,] 
    nb_visits<-append(nb_visits,length(datafinal[,1]))
    
    ## Bind the final dataset for that individual to the rest of the data 
    dattotal<-rbind(dattotal,dat3)
    
  }
  
  fulldata<-dattotal[-1,] ## Remove the first row which was made of NAs
  fulldata[fulldata$visit==0,]$outcome<-NA  ## Put missing values for the outcome when the visit=0
  
  
  ## At this point, fulldata looks like this:
  
  #> head(fulldata)
  #  X         Z outcome time_discrete ID       age sex    height visit
  #2 1 1.6776061      NA          0.01  1 0.1539278   0 -0.288775     0
  #3 1 1.9598621      NA          0.02  1 0.1539278   0 -0.288775     0
  #4 1 2.2040711      NA          0.03  1 0.1539278   0 -0.288775     0
  #5 1 0.7053197      NA          0.04  1 0.1539278   0 -0.288775     0
  #6 1 0.4009147      NA          0.05  1 0.1539278   0 -0.288775     0
  #7 1 0.6985810      NA          0.06  1 0.1539278   0 -0.288775     0
  
  
  #####################################
  ## find the nearest neighbor for Y ##
  #####################################
  
  dd<-matrix(c(rep(NA,10)),ncol=10,nrow=1)
  colnames(dd)<-c("X","Z","outcome","time_discrete","ID","age","sex","height","visit","y_neigh") ## y_neigh is the nearest neighbour
  
  for(i in 1:max(fulldata$ID)){
    
    dati<-fulldata[fulldata$ID==i,]  ## Select only one patient/ID at a time 
    
    if(length(dati[,1])!=0){         ## If the dataset dati is not empty:
      
      yy<-c()
      cc<-c()
      
      for(j in 1:length(dati[,1]) ) {
        
        if(is.na(dati$outcome[j])==FALSE){         ## If outcome is not missing
          yy<-append(yy,dati$outcome[j]) ## add that outcome to yy and
          cc<-append(cc,j)		}      ## add the index to cc
      }
      
      if (length(yy)==0){                          ## If there is no outcome at all for that patient
        dati$y_neigh<-c(rep(NA,length(dati[,1])))  ## put NA everywhere
        dd<-rbind(dd,dati);    next }	         ## and rbind with previous patients' data. Go to next patient. 
      
      ## However, if length(yy) != 0 and there is >=1 outcome, do: 
      hj2<-matrix(cbind(yy,cc,seq(1,length(yy))),ncol=3)   ## cbind yy, cc, numbers from 1 to length(yy). The dimension should be the number of non missing outcomes
      dati$y_neigh<-rep(NA,length(dati[,1]))               ## for that patient. And put NA in the full nearest neighbor variable 
      
      for(j in 1:length(dati[,1]) ){                       ## compute for each row of dati the absolute difference between current time at this row, and the 
        diff<-abs(dati$time_discrete[j]*100-hj2[1,2])      ## very first outcome time for that patient
        y_neigh<-hj2[1,1]                                  ## initialize nearest neighbor y at the outcome at the very first outcome time hj2[1,1]
        
        for (k in 1:length(hj2[,1])){                        ## go over all outcomes for that individual
          diffabs<-abs(dati$time_discrete[j]*100-hj2[k,2])   ## for each outcome time, compute absolute difference between outcome time and current time
          if (diffabs<diff ){                                ## if that difference is smaller than the one at which we initialized, then replace nearest neighbor by
            y_neigh<-hj2[k,1]                               ## outcome at that time hj2[k,2] by doing y_neigh<-hj2[k,1]  and replace abs. diff. by the new smallest difference
            diff<-diffabs}  }
        dati$y_neigh[j]<-y_neigh}
      dd<-rbind(dd,dati)         }                        ## and rbind with previous patients' data. Go to next patient.
  }
  
  fulldata<-dd[-1,] ## Drop the first empty row that we have put there just to initialize the matrix.
  
  
  ## At this point, fulldata looks like this:
  
  #> fulldata[1:100,]
  
  #    X           Z  outcome time_discrete ID       age sex    height visit   y_neigh
  #2   1  1.67760608       NA          0.01  1 0.1539278   0 -0.288775     0 3.252778
  #3   1  1.95986209       NA          0.02  1 0.1539278   0 -0.288775     0 3.252778
  #4   1  2.20407113       NA          0.03  1 0.1539278   0 -0.288775     0 3.252778
  #5   1  0.70531973       NA          0.04  1 0.1539278   0 -0.288775     0 3.252778
  #6   1  0.40091472       NA          0.05  1 0.1539278   0 -0.288775     0 3.252778
  #[...]
  #88  1  1.32754981       NA          0.87  1 0.1539278   0 -0.288775     0 3.252778
  #89  1  1.26440446       NA          0.88  1 0.1539278   0 -0.288775     0 3.252778
  #90  1  2.31155114   3.252778        0.89  1 0.1539278   0 -0.288775     1 3.252778
  #91  1  2.87419742       NA          0.90  1 0.1539278   0 -0.288775     0 3.252778
  #92  1  0.88036225       NA          0.91  1 0.1539278   0 -0.288775     0 3.252778
  #93  1  1.34572953       NA          0.92  1 0.1539278   0 -0.288775     0 3.252778
  #94  1  1.70166923       NA          0.93  1 0.1539278   0 -0.288775     0 3.252778
  #95  1  3.28659053       NA          0.94  1 0.1539278   0 -0.288775     0 3.252778
  #96  1  0.51285129       NA          0.95  1 0.1539278   0 -0.288775     0 3.252778
  #97  1  3.29097206       NA          0.96  1 0.1539278   0 -0.288775     0 3.252778
  #98  1  2.79479763       NA          0.97  1 0.1539278   0 -0.288775     0 3.252778
  #99  1  2.06405239       NA          0.98  1 0.1539278   0 -0.288775     0 3.252778
  #100 1  1.30759202       NA          0.99  1 0.1539278   0 -0.288775     0 3.252778
  #101 1  0.40723876       NA          1.00  1 0.1539278   0 -0.288775     0 3.252778
  
  
  #####################################################################
  ## compute the re-centered means for treatment and all confounders ##
  #####################################################################
  
  ## create matrix W with treatment X and confounders
  W<-as.matrix(cbind(fulldata$X,fulldata$age,fulldata$sex,fulldata$height),ncol=4)
  
  ## This matrix will be filled with xbar for each time  
  ## One matrix for treatment and confounder
  mat2<-matrix(NA,ncol=(length(W[1,])+1),nrow=max(fulldata$time_discrete*100)+1)
  colnames(mat2)<-c('time_discrete','xbar','agebar','sexbar','heightbar')
  ## And onte matrix for re-centered outcome Y
  mat3<-matrix(NA,ncol=2,nrow=max(fulldata$time_discrete*100)+1)
  colnames(mat3)<-c('time_discrete','ybar')
  
  ## Estimates for gamma parameters for the visit process:
  ## First we need to obtain t1 and t2 (min and max time for each row)
  fulldata$t1<-fulldata$time_discrete-0.01
  fulldata$t2<-fulldata$time_discrete
  ## Estimation of Gamma parameters
  gamma<-coxph(Surv(fulldata$t1, fulldata$t2, fulldata$visit )~fulldata$X, data=fulldata)$coef   
  
  ## fill out adjusted means for X, age, sex and height
  for(k2 in 1:(max(fulldata$time_discrete)*100) ) {
    datk<-fulldata[round(fulldata$time_discrete,2)==round(k2/100,2),]
    if(length(datk[,1])==0){next}
    mat2[k2,2]<- sum(datk$X*(exp(gamma*datk$X) /sum( exp(gamma*datk$X )) ))
    mat2[k2,1]<- round(k2/100,2)
    mat2[k2,3]<- sum(datk$age*(exp(gamma*datk$X) /sum( exp(gamma*datk$X)) ))
    mat2[k2,4]<- sum(datk$sex*(exp(gamma*datk$X ) /sum( exp(gamma*datk$X)) ))
    mat2[k2,5]<- sum(datk$height*(exp(gamma*datk$X) /sum( exp(gamma*datk$X)) ))
    mat3[k2,2]<- sum(datk$y_neigh*(exp(gamma*datk$X) /sum( exp(gamma*datk$X) )), na.rm=TRUE)
    mat3[k2,1]<- round(k2/100,2)
  }
  
  ## and combine
  fulldata$time_discrete2<- round(fulldata$time_discrete,2)
  mat2 <-cbind(mat2, round(mat2[,1],2))
  colnames(mat2)[6]<-'time_discrete2'
  mat3 <-cbind(mat3, round(mat3[,1],2))
  colnames(mat3)[3]<-'time_discrete2'
  
  ## Because of the way the time was simulated, we need to be careful with merging and use the round() command as above 
  fulldatav2 <- merge(fulldata,mat2,by='time_discrete2')
  fulldatav3 <-fulldatav2[order(fulldatav2$ID,fulldatav2$time_discrete2),]
  fulldatav4 <- merge(fulldatav3,mat3,by='time_discrete2')
  fulldatav5 <- fulldatav4[order(fulldatav4$ID,fulldatav4$time_discrete2),]
  
  
  ## at this point, data looks like:
  
  #>   head(fulldatav5)
  #     time_discrete2 X         Z outcome time_discrete.x ID       age sex    height visit    y_neigh  t1   t2 time_discrete.y     xbar    agebar    sexbar heightbar time_discrete
  #1              0.01 1 1.6776061      NA            0.01  1 0.1539278   0 -0.288775     0 3.252778 0.00 0.01            0.01 0.553775 0.9508472 0.5245869 0.2698431          0.01
  #366            0.02 1 1.9598621      NA            0.02  1 0.1539278   0 -0.288775     0 3.252778 0.01 0.02            0.02 0.553775 0.9508472 0.5245869 0.2698431          0.02
  #545            0.03 1 2.2040711      NA            0.03  1 0.1539278   0 -0.288775     0 3.252778 0.02 0.03            0.03 0.553775 0.9508472 0.5245869 0.2698431          0.03
  #780            0.04 1 0.7053197      NA            0.04  1 0.1539278   0 -0.288775     0 3.252778 0.03 0.04            0.04 0.553775 0.9508472 0.5245869 0.2698431          0.04
  #899            0.05 1 0.4009147      NA            0.05  1 0.1539278   0 -0.288775     0 3.252778 0.04 0.05            0.05 0.553775 0.9508472 0.5245869 0.2698431          0.05
  #1026           0.06 1 0.6985810      NA            0.06  1 0.1539278   0 -0.288775     0 3.252778 0.05 0.06            0.06 0.553775 0.9508472 0.5245869 0.2698431          0.06
  #         ybar
  #1    2.167401
  #366  2.167401
  #545  2.175640
  #780  2.175640
  #899  2.156210
  #1026 2.156210
  
  ## put everything back into fulldata dataset
  fulldata<-fulldatav5
  
  
  #####################
  ## estimator IPCTM ##
  #####################
  
  fulldata$t1<-fulldata$time_discrete-0.01
  fulldata$t2<-fulldata$time_discrete
  
  ## Estimate delta parameter (only X model)
  delta<-coxph(Surv(fulldata$t1, fulldata$t2, fulldata$visit )~fulldata$X, data=fulldata)$coef   
  
  ## Estimate gamma parameters (Both X and Z model)
  gamma<-coxph(Surv(fulldata$t1, fulldata$t2, fulldata$visit )~fulldata$X+fulldata$Z, data=fulldata)$coef   
  
  ## Weight for the visit
  rho_i<- exp( gamma[1]*fulldata$X + gamma[2]*fulldata$Z )/exp(delta*fulldata$X)
  
  ## Propensity score based on re-centered variables:
  dataa<-fulldata[,c(2,6,7,8,9,14,15,16,17,18)] ## Keep only the variables we need
  dg<-dataa[!duplicated(dataa), ] ## No duplicate (we want it by individual - one for each individual)
  
  newx<-dg$X-dg$xbar
  newage<- dg$age-dg$agebar
  newsex<- dg$sex- dg$sexbar
  newheight<- dg$height- dg$heightbar
  
  ## One proprensity score model with all covariates recentered 
  psc<-predict(glm(newx ~ newage + newsex+ newheight),type='response')
  
  ## One propensity score model with only a constant, to stabilize the weights
  pti<-predict(glm(newx ~ 1),type='response')
  
  ## Now transpose that to a weight using the Normal density:
  ## first with psc which is conditional on confounders:
  
  residuals<- newx - psc
  sigmasq<- var(residuals)
  psc_bi<- 1/sqrt(2*pi)*1/sqrt(sigmasq)*exp(-residuals^2/(2*sigmasq))
  
  residuals2<- newx - pti 
  sigmasq2<- var(residuals2)
  pti_bi<- 1/sqrt(2*pi)*1/sqrt(sigmasq2)*exp(-residuals2^2/(2*sigmasq2))
  
  matweight<-matrix(cbind(dg$time_discrete,psc_bi,pti_bi,dg$ID),ncol=4)
  colnames(matweight)<-c('time_discrete','ps','pt','ID')
  fulldata2<-merge(fulldata,matweight,by=c("ID","time_discrete")) 
  fulldata<-fulldata2[order(fulldata2$ID,fulldata2$time_discrete),]
  
  ## Final estimator IPCTM
  ww<-fulldata$pt/fulldata$ps
  XX<-  c(1/sqrt(rho_i))*cbind(fulldata$X-fulldata$xbar )
  YY<-   c(1/sqrt(rho_i))*(fulldata$outcome-fulldata$ybar)
  iptcm<- lm(YY~ -1+ XX , weight=ww )$coef[1]
  vec_IPCTM<-append(vec_IPCTM,iptcm)
  
  #####################
  ## estimator FIPTM ##
  #####################
  
  fulldata<-dattotal[-1,]
  fulldata[fulldata$visit==0,]$outcome<-NA    
  
  ## compute the weight for visit process
  fulldata$t1<-fulldata$time_discrete-0.01
  fulldata$t2<-fulldata$time_discrete
  gamma<-glm(fulldata$visit~fulldata$X+fulldata$Z,family='binomial')$coef   
  fulldata$rho_i<-exp(gamma[1]+ gamma[2]*fulldata$X + gamma[3]*fulldata$Z )
  
  ## compute ipt weight
  dataa<-fulldata[,c(1,5,6,7,8)]
  dg<-dataa[!duplicated(dataa), ]
  psc<-predict(glm(dg$X~dg$age+dg$sex+dg$height, family='binomial'),type='response')
  
  matweight<-matrix(cbind(psc,dg$ID),ncol=2)
  colnames(matweight)<-c('ps','ID')
  fulldata2<-merge(matweight,fulldata,by="ID") 
  fulldata<-fulldata2[order(fulldata2$ID,fulldata2$time_discrete),]
  
  ww<-1/ifelse(fulldata$X==1,fulldata$ps,1-fulldata$ps)
  XX<- fulldata$X 
  YY<-fulldata$outcome 
  terti<-quantile(fulldata$time_discrete , c(0.3333, 0.66666), type = 1)  ## 0.333 0.6666 knots for splines
  fiptm<-lm(YY~ bs(fulldata$time_discrete,degree=3,knots=c(terti))+ XX , weight= ww*1/fulldata$rho_i)$coef[7]      ## ******* here: can take off the weight ww to test without confounding - adjusting only for visit process works if no confounding *********
  vec_FIPTM<-append(vec_FIPTM,fiptm)
  
  ##########################################################
  ### Asymptotic variance for FIPTM - sandwich estimator ###
  ##########################################################
  
  ## Beta estimates from FIPTM estimator
  betas<-matrix(lm(YY~ bs(fulldata$time_discrete,degree=3,knots=c(terti))+ XX , weight= ww*1/fulldata$rho_i)$coef,ncol=1)
  
  ## Create two data frames: one with all data rows and one with only the visit rows (when visit=1 and outcome measured)
  allrows<-as.data.frame(cbind(c(rep(1,length(XX))),bs(fulldata$time_discrete,degree=3,knots=c(terti)),XX,YY,ww,fulldata$Z,fulldata$rho_i,fulldata$age,fulldata$sex,fulldata$height,fulldata$ID,fulldata$time_discrete),ncol=16)
  allvisits<-allrows[complete.cases(allrows),]
  
  ## We have to differentiate between the dataset with only the visits (outcome recorded), and the full dataset, because 
  ## the IPT weights are computed using only the information from the dataset with only visit=1, while the visit-intensity weights are
  ## computed on the full dataset. So both are considered in the variance calculation.
  
  colnames(allvisits)<-c('intercept','bs1','bs2','bs3','bs4','bs5','x','y','ww','z','rhoi','age','sex','height','ID','time')
  colnames(allrows)<-c('intercept','bs1','bs2','bs3','bs4','bs5','x','y','ww','z','rhoi','age','sex','height','ID','time')
  
  ## Predictors X and confounders K
  Xvisits<-allvisits[,1:7]
  Kvisits<-allvisits[,c(1,12:14)]
  
  omega<- matrix(glm(fulldata$X~fulldata$age+fulldata$sex+fulldata$height, family='binomial')$coef,ncol=1)
  gamma<-matrix(glm(fulldata$visit~fulldata$X+fulldata$Z,family='binomial')$coef )
  
  expgammavisits<- exp(gamma[1]+ gamma[2]*allvisits$x+gamma[3]*allvisits$z)
  oneoverexpgammavisits<- 1/expgammavisits
  
  expomegavisits<- matrix(exp( t(omega)%*%t(Kvisits)),ncol=1)
  betasvisits<- matrix(t(t(betas)%*%t(Xvisits)),ncol=1)
  
  yminusbetasvisits<- allvisits$y-betasvisits
  
  ## Compute the number of persons, the number of visits and the number of rows
  
  ## Nb Persons
  nbpersons<-length(fulldata[!duplicated(fulldata$ID),]$ID) # total different persons
  
  ## Nb Visits  
  nit<- length(fulldata[is.na(fulldata$outcome)==0,]$outcome) # total different visits so Y available
  
  ## Nb Rows in full dataset with visit = 0 or 1
  nbrows<-length(fulldata[,1]) # total nb of rows
  
  
  ###### For all further elements ( g(o), m(o), M, G_beta, G_Psi) please refer to the web Appendix C of the main manuscript #####
  ###### to know what each term represents, in the calculation of the asymptotic variance of FIPTM                          #####
  
  
  ###################### 
  ## 1. Compute g(o): ##
  ######################
  
  reg<-glm(YY~ bs(fulldata$time_discrete,degree=3,knots=c(terti))+ XX , weight= ww*1/fulldata$rho_i) 
  
  estfun.glm2<-function(glm.obj){
    if (is.matrix(glm.obj$x))
      xmat<-glm.obj$x
    else {
      mf<-model.frame(glm.obj)
      xmat<-model.matrix(terms(glm.obj),mf)
    }
    residuals(glm.obj,"working")*glm.obj$weights*xmat   }
  
  es1<-estfun.glm2(reg)
  go<- t(es1) 
  
  vec<-matrix(NA,nrow=7,ncol=nbrows)
  indic<-ifelse(is.na(fulldata$outcome),0,1)
  ll<-1
  
  for(ik in 1:nbrows){   
    if(indic[ik]==1){vec[,ik]<-go[,ll]; ll<-ll+1}
    if(indic[ik]==0){vec[,ik]<-c(rep(0,7)) }
  }
  
  ##################### 
  ## 2. Compute m(o) ##
  #####################
  
  resfoismat<-function(glm.obj){
    if (is.matrix(glm.obj$x))
      xmat<-glm.obj$x
    else {
      mf<-model.frame(glm.obj)
      xmat<-model.matrix(terms(glm.obj),mf)
    }
    residuals(glm.obj,"working")*glm.obj$weights*xmat 
  }
  
  m1<- glm(dg$X~dg$age+dg$sex+dg$height, family='binomial') 
  resm1<-resfoismat(m1)
  
  inda<-ifelse(is.na(fulldata$outcome),0,1)
  resm1m<-matrix(NA, nrow=nbrows,ncol=4)
  
  resm1m[1,]<- resm1[1,]
  jindex<-2
  
  for(i in 2:nbrows){
    if(fulldata$ID[i]==fulldata$ID[i-1] ){  resm1m[i,] <-c(0,0,0,0) }
    if(fulldata$ID[i]!=fulldata$ID[i-1] ){  resm1m[i,] <- resm1[jindex,] ; jindex<-jindex+1} 
  }
  
  m2<- glm(fulldata$visit~fulldata$X+fulldata$Z, family='binomial')
  resm2<-resfoismat(m2)
  mo<- rbind( t(resm1m[,1]), t(resm1m[,2]), t(resm1m[,3]), t(resm1m[,4]), t(resm2[,1]), t(resm2[,2]), t(resm2[,3]))
  ## mo and go=vec are OK 
  
  ###################
  ## 3. Compute M  ##
  ###################
  
  ## matrix used for pscore computation: contains 1, age, sex, height
  
  crossprod<- cbind( c(rep(1,length(dg[,1]) ) ), dg$age,dg$sex,dg$height)
  
  Mrow1<- c(1/nbrows*sum(-exp( t(omega) %*% t(crossprod))  /(1+exp(t(omega) %*% t(crossprod)) )^2 ),
            1/nbrows*sum(-exp( t(omega) %*% t(crossprod))*dg$age  /(1+exp(t(omega) %*% t(crossprod)) )^2 ) ,
            1/nbrows*sum(-exp( t(omega) %*% t(crossprod))*dg$sex  /(1+exp(t(omega) %*% t(crossprod)) )^2 ),
            1/nbrows*sum(-exp( t(omega) %*% t(crossprod))*dg$height  /(1+exp(t(omega) %*% t(crossprod)) )^2 ) ,0,0,0 )
  Mrow2<- c(1/nbrows*sum(-dg$age*exp( t(omega) %*% t(crossprod))  /(1+exp(t(omega) %*% t(crossprod)) )^2 ),
            1/nbrows*sum(-exp( t(omega) %*% t(crossprod))*dg$age^2  /(1+exp(t(omega) %*% t(crossprod)) )^2 ) ,
            1/nbrows*sum(-exp( t(omega) %*% t(crossprod))*dg$age*dg$sex  /(1+exp(t(omega) %*% t(crossprod)) )^2 ),
            1/nbrows*sum(-exp( t(omega) %*% t(crossprod))*dg$age*dg$height  /(1+exp(t(omega) %*% t(crossprod)) )^2 ), 0,0,0  )
  Mrow3<- c(1/nbrows*sum(-dg$sex*exp( t(omega) %*% t(crossprod))  /(1+exp(t(omega) %*% t(crossprod)) )^2 ),
            1/nbrows*sum(-exp( t(omega) %*% t(crossprod))*dg$age*dg$sex  /(1+exp(t(omega) %*% t(crossprod)) )^2 ) ,
            1/nbrows*sum(-exp( t(omega) %*% t(crossprod))*dg$sex*dg$sex  /(1+exp(t(omega) %*% t(crossprod)) )^2 ),
            1/nbrows*sum(-exp( t(omega) %*% t(crossprod))*dg$height*dg$sex  /(1+exp(t(omega) %*% t(crossprod)) )^2 ) , 0,0,0 )
  Mrow4<- c(1/nbrows*sum(-dg$height*exp( t(omega) %*% t(crossprod))  /(1+exp(t(omega) %*% t(crossprod)) )^2 ),
            1/nbrows*sum(-exp( t(omega) %*% t(crossprod))*dg$age*dg$height  /(1+exp(t(omega) %*% t(crossprod)) )^2 ) ,
            1/nbrows*sum(-exp( t(omega) %*% t(crossprod))*dg$sex*dg$height /(1+exp(t(omega) %*% t(crossprod)) )^2 ),
            1/nbrows*sum(-exp( t(omega) %*% t(crossprod))*dg$height*dg$height  /(1+exp(t(omega) %*% t(crossprod)) )^2 ) ,0,0,0 )
  
  gammav2<-matrix(glm(fulldata$visit~fulldata$X+fulldata$Z,family='binomial')$coef)
  double<-cbind(c(rep(1,length(fulldata[,1]))) ,fulldata$X,fulldata$Z)
  
  Mrow5<- c(0,0,0,0, 1/nbrows*sum( -exp( t(gammav2)%*%t(double) ) /(1+exp( t(gammav2)%*%t(double)) )^2  ), 
            1/nbrows*sum(- fulldata$X*exp( t(gammav2)%*%t(double) ) /(1+exp( t(gammav2)%*%t(double)) )^2  ),
            1/nbrows*sum(- fulldata$Z*exp( t(gammav2)%*%t(double) ) /(1+exp( t(gammav2)%*%t(double)) )^2  )) 
  Mrow6<- c(0,0,0,0, 1/nbrows*sum( -fulldata$X*exp( t(gammav2)%*%t(double) ) /(1+exp( t(gammav2)%*%t(double)) )^2  ), 
            1/nbrows*sum(-fulldata$X*fulldata$X*exp( t(gammav2)%*%t(double) ) /(1+exp( t(gammav2)%*%t(double)) )^2  ),
            1/nbrows*sum(-fulldata$X*fulldata$Z*exp( t(gammav2)%*%t(double) ) /(1+exp( t(gammav2)%*%t(double)) )^2  )) 
  Mrow7<- c(0,0,0,0, 1/nbrows*sum( -fulldata$Z*exp( t(gammav2)%*%t(double) ) /(1+exp( t(gammav2)%*%t(double)) )^2  ), 
            1/nbrows*sum(-fulldata$Z*fulldata$X*exp( t(gammav2)%*%t(double) ) /(1+exp( t(gammav2)%*%t(double)) )^2  ),
            1/nbrows*sum(-fulldata$Z*fulldata$Z*exp( t(gammav2)%*%t(double) ) /(1+exp( t(gammav2)%*%t(double)) )^2  )) 
  
  M<- rbind(Mrow1,Mrow2,Mrow3,Mrow4,Mrow5,Mrow6,Mrow7)
  
  
  #########################
  ## 4. Compute G_beta  ###
  #########################
  
  g1<- c( 1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,1]*allvisits[,1]) , 1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,2]*allvisits[,1]) ,1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,3]*allvisits[,1]), 
          1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,4]*allvisits[,1]) ,1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,5]*allvisits[,1]) ,1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,6]*allvisits[,1]),1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,7]*allvisits[,1]) ) 
  g2<- c( 1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,1]*allvisits[,2]) , 1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,2]*allvisits[,2]) ,1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,3]*allvisits[,2]), 
          1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,4]*allvisits[,2]) ,1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,5]*allvisits[,2]) ,1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,6]*allvisits[,2]),1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,7]*allvisits[,2]) ) 
  g3<- c( 1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,1]*allvisits[,3]) , 1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,2]*allvisits[,3]) ,1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,3]*allvisits[,3]), 
          1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,4]*allvisits[,3]) ,1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,5]*allvisits[,3]) ,1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,6]*allvisits[,3]),1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,7]*allvisits[,3]) ) 
  g4<- c( 1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,1]*allvisits[,4]) , 1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,2]*allvisits[,4]) ,1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,3]*allvisits[,4]), 
          1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,4]*allvisits[,4]) ,1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,5]*allvisits[,4]) ,1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,6]*allvisits[,4]),1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,7]*allvisits[,4]) ) 
  g5<- c( 1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,1]*allvisits[,5]) , 1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,2]*allvisits[,5]) ,1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,3]*allvisits[,5]), 
          1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,4]*allvisits[,5]) ,1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,5]*allvisits[,5]) ,1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,6]*allvisits[,5]),1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,7]*allvisits[,5]) ) 
  g6<- c( 1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,1]*allvisits[,6]) , 1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,2]*allvisits[,6]) ,1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,3]*allvisits[,6]), 
          1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,4]*allvisits[,6]) ,1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,5]*allvisits[,6]) ,1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,6]*allvisits[,6]),1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,7]*allvisits[,6]) ) 
  g7<- c( 1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,1]*allvisits[,7]) , 1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,2]*allvisits[,7]) ,1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,3]*allvisits[,7]), 
          1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,4]*allvisits[,7]) ,1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,5]*allvisits[,7]) ,1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,6]*allvisits[,7]),1/nbrows*sum(allvisits$ww*1/allvisits$rhoi*-allvisits[,7]*allvisits[,7]) ) 
  
  gbeta<- rbind(g1,g2,g3,g4,g5,g6,g7)
  
  ########################
  ### 5. Compute G_psi ###
  ########################
  
  ag<-allvisits[,1:7]
  allk<-allvisits[,c(1,12:14)]
  expomega<-exp(t(omega)%*%t(allk))
  indd<-ifelse(ag[,7]==1, -1/expomega, expomega)
  
  gp1<- c(1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,1]* indd),
          1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,1]* indd * allk[,2]), 
          1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,1]* indd * allk[,3]), 
          1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,1]* indd * allk[,4]), 
          1/nbrows*sum(allvisits$ww*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,1]*  -1*1/allvisits$rhoi ), 
          1/nbrows*sum(allvisits$ww*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,1]* -ag[,7] *1/allvisits$rhoi ), 
          1/nbrows*sum(allvisits$ww*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,1]* -allvisits$z * 1/allvisits$rhoi ) )
  gp2<- c(1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,2]* indd),
          1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,2]* indd * allk[,2]), 
          1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,2]* indd * allk[,3]), 
          1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,2]* indd * allk[,4]), 
          1/nbrows*sum(allvisits$ww*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,2]* -1*1/allvisits$rhoi ), 
          1/nbrows*sum(allvisits$ww*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,2]* -ag[,7] *1/allvisits$rhoi ), 
          1/nbrows*sum(allvisits$ww*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,2]* -allvisits$z * 1/allvisits$rhoi ))
  gp3<- c(1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,3]* indd),
          1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,3]* indd * allk[,2]), 
          1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,3]* indd * allk[,3]), 
          1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,3]* indd * allk[,4]), 
          1/nbrows*sum(allvisits$ww*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,3]*  -1*1/allvisits$rhoi ), 
          1/nbrows*sum(allvisits$ww*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,3]* -ag[,7] *1/allvisits$rhoi ), 
          1/nbrows*sum(allvisits$ww*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,3]* -allvisits$z * 1/allvisits$rhoi ))
  gp4<- c(1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,4]* indd),
          1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,4]* indd * allk[,2]), 
          1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,4]* indd * allk[,3]), 
          1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,4]* indd * allk[,4]), 
          1/nbrows*sum(allvisits$ww*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,4]* -1*1/allvisits$rhoi ), 
          1/nbrows*sum(allvisits$ww*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,4]* -ag[,7] *1/allvisits$rhoi ), 
          1/nbrows*sum(allvisits$ww*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,4]* -allvisits$z * 1/allvisits$rhoi ))
  gp5<- c(1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,5]* indd),
          1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,5]* indd * allk[,2]), 
          1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,5]* indd * allk[,3]), 
          1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,5]* indd * allk[,4]), 
          1/nbrows*sum(allvisits$ww*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,5]*  -1*1/allvisits$rhoi ), 
          1/nbrows*sum(allvisits$ww*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,5]* -ag[,7] *1/allvisits$rhoi ), 
          1/nbrows*sum(allvisits$ww*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,5]* -allvisits$z * 1/allvisits$rhoi ))
  gp6<- c(1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,6]* indd),
          1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,6]* indd * allk[,2]), 
          1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,6]* indd * allk[,3]), 
          1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,6]* indd * allk[,4]), 
          1/nbrows*sum(allvisits$ww*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,6]*  -1*1/allvisits$rhoi ), 
          1/nbrows*sum(allvisits$ww*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,6]* -ag[,7] *1/allvisits$rhoi ), 
          1/nbrows*sum(allvisits$ww*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,6]* -allvisits$z * 1/allvisits$rhoi ))
  gp7<- c(1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,7]* indd),
          1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,7]* indd * allk[,2]), 
          1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,7]* indd * allk[,3]), 
          1/nbrows*sum(1/allvisits$rhoi*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,7]* indd * allk[,4]), 
          1/nbrows*sum(allvisits$ww*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,7]* -1*1/allvisits$rhoi ), 
          1/nbrows*sum(allvisits$ww*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,7]* -ag[,7] *1/allvisits$rhoi ), 
          1/nbrows*sum(allvisits$ww*(allvisits$y-t(t(betas)%*%t(ag)))*ag[,7]* -allvisits$z * 1/allvisits$rhoi ))
  
  Gpsi<-rbind(gp1,gp2,gp3,gp4,gp5,gp6,gp7) 
  
  ##############
  ## variance ##
  ##############
  
  psipart<-Gpsi %*% ( -solve(M)%*%mo ) 
  var<- 1/nbrows^2 * solve(gbeta) %*%( ( vec +    psipart ) %*% t( vec + psipart)) %*%  solve(gbeta)
  sandwich<- append(sandwich, var[7,7]) ## Provides the asymptotic variance for FIPTM
  
}   ## end loop 


