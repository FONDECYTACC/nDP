
coxphw_pwp_int_oct_2022_a<-
  coxphw(Surv(Tstart, Tstop, status) ~ tipo_de_plan_res_1*strata(trans_cor)+  
           cluster(id)+ arrival, data = dt_ms_d_match_surv_oct_2022_a,template="AHR",  robust=T)

coxphw_pwp_int_oct_2022_b<-
  coxphw(Surv(Tstart, Tstop,status) ~ tipo_de_plan_res_1*strata(trans_cor)+  
           cluster(id)+ arrival, data = dt_ms_d_match_surv_oct_2022_b,template="AHR",  robust=T)

coxphw_pwp_int_oct_2022_a2<-
  coxphw(Surv(Tstart, Tstop, status) ~ tipo_de_plan_res_1*strata(trans_cor)+  
           cluster(id)+ arr_trans, data = dt_ms_d_match_surv_oct_2022_a,template="AHR",  robust=T)

coxphw_pwp_int_oct_2022_b2<-
  coxphw(Surv(Tstart, Tstop,status) ~ tipo_de_plan_res_1*strata(trans_cor)+  
           cluster(id)+ arr_trans, data = dt_ms_d_match_surv_oct_2022_b, template="AHR",  robust=T)


coxphw_pwp_int_gt_oct_2022_a<-
  coxphw(Surv(time,status) ~ tipo_de_plan_res_1*strata(trans_cor)+ cluster(id) + arrival, data = dt_ms_d_match_surv_oct_2022_a, template="AHR",  robust=T)

coxphw_pwp_int_gt_oct_2022_b<-
  coxphw(Surv(time,status) ~ tipo_de_plan_res_1*strata(trans_cor)+  
           cluster(id) + arrival, data = dt_ms_d_match_surv_oct_2022_b, template="AHR",  robust=T)

coxphw_pwp_int_gt_oct_2022_a2<-
  coxphw(Surv(time,status) ~ tipo_de_plan_res_1*strata(trans_cor)+  
           cluster(id) + arr_trans, data = dt_ms_d_match_surv_oct_2022_a, template="AHR",  robust=T)

coxphw_pwp_int_gt_oct_2022_b2<-
  coxphw(Surv(time,status) ~ tipo_de_plan_res_1*strata(trans_cor)+  
           cluster(id) + arr_trans, data = dt_ms_d_match_surv_oct_2022_b, template="AHR",  robust=T)

after_coxphw<-Sys.time()
paste0("Time taken: ", round((after_coxphw-before_coxphw)/(60*60),2))

coxph_pwp_int_gt_oct_2022_b<-
  coxph(Surv(time,status) ~ tipo_de_plan_res_1*strata(trans_cor)+  
          + arr_trans, data = dt_ms_d_match_surv_oct_2022_b, cluster=id)

coxph_pwp_int_gt_oct_2022_b2<-
  coxph(Surv(time,status) ~ tipo_de_plan_res_1*strata(trans_cor)+  
          cluster(id) + arr_trans, data = dt_ms_d_match_surv_oct_2022_b)

coxph_pwp_int_gt_oct_2022_b3<-
  coxph(Surv(time,status) ~ tipo_de_plan_res_1*strata(trans_cor)+ tt(tipo_de_plan_res_1)+ 
          arr_trans, data = dt_ms_d_match_surv_oct_2022_b,  tt= function(x, t, ...) x * log(t + 20), cluster=id, ties="efron")

coxph_pwp_int_gt_oct_2022_b4<-
  coxph(Surv(time,status) ~ tipo_de_plan_res_1*strata(trans_cor)+ tt(tipo_de_plan_res_1)+ 
          arr_trans, data = dt_ms_d_match_surv_oct_2022_b,  tt= function(x, t, ...) x* nsk(t, knots=c(5, 100, 200, 400),
                                                                                           Boundary.knots = FALSE), cluster=id, ties="efron")

coxph_pwp_int_gt_oct_2022_b5<-
  coxph(Surv(time,status) ~ tipo_de_plan_res_1*strata(trans_cor)+ tt(tipo_de_plan_res_1)+ 
          arr_trans, data = dt_ms_d_match_surv_oct_2022_b,  
        tt= function(x,t,...) {print(length(x)) pspline(x + t/365.25)}, cluster=id, ties="efron")

#https://stats.stackexchange.com/questions/205475/time-varying-coefficients-in-cox-proportional-hazard-model
coxph_pwp_int_gt_oct_2022_b5<-
coxph(Surv(time, status) ~ tt(tipo_de_plan_res_1)*strata(trans_cor)+ 
        tt(arr_trans), data= dt_ms_d_match_surv_oct_2022_b,
      tt=list(
        function(x,t,...) {
          cbind(x, x + t/365.25, (x + t/365.25)^2)
        },
        function(x,t,...) {
          pspline(x + t/365.25)
        }),
      x=T,
      y=T,
      cluster=id, 
      ties="efron")

vet2 <- survSplit(Surv(time, status) ~ ., data= dt_ms_d_match_surv_oct_2022_b, cut=seq(90, 1826, by=180),
                  episode= "tgroup", id="id2")

coxph_pwp_int_gt_oct_2022_b8<-
  coxph(Surv(time,status) ~ tipo_de_plan_res_1*strata(trans_cor)+  
          + arr_trans+ cluster(id2), data = vet2, cluster=id)

# The Efron approximation is used as the default here, it is more accurate when dealing with tied death times, and is as efficient computationally. 
#A time-transform term allows variables to vary dynamically in time
# Fit a time transform model using current age

#emmeans::emmeans(coxph_pwp_int_gt_oct_2022_b2, ~tipo_de_plan_res_1*trans_cor)

#plot(emmeans::emmeans(coxph_pwp_int_gt_oct_2022_b3, ~tipo_de_plan_res_1*trans_cor))

pairs(emmeans::emmeans(coxph_pwp_int_gt_oct_2022_b2, ~ tipo_de_plan_res_1*trans_cor))

plot(pairs(emmeans::emmeans(coxph_pwp_int_gt_oct_2022_b2, ~ tipo_de_plan_res_1*trans_cor)))

plot(pairs(emmeans::emmeans(coxph_pwp_int_gt_oct_2022_b3, ~tipo_de_plan_res_1*trans_cor)))
#coxphw::wald()
pairs(emmeans::emmeans(coxph_pwp_int_gt_oct_2022_b3, ~ tipo_de_plan_res_1+ trans_cor, adjust="sidak"), type = "response")
plot(pairs(emmeans::emmeans(coxph_pwp_int_gt_oct_2022_b3, ~ tipo_de_plan_res_1+ trans_cor, adjust="sidak"), type = "response"))

aalen<-
aalen(Surv(time,status)~ tipo_de_plan_res_1+ cluster(id)+ arr_trans,
      dt_ms_d_match_surv_oct_2022_b,max.time=5*265.25,n.sim=100)

timecox(Surv(time,status)~ tipo_de_plan_res_1*trans(trans_cor)+ cluster(id)+ arr_trans,
      dt_ms_d_match_surv_oct_2022_b,max.time=5*265.25,n.sim=100)


LTF.tvc1.coxph <- coxph(Surv(time,status) ~ (tt(tipo_de_plan_res_1)+ arr_trans)*strata(trans_cor), 
                data=dt_ms_d_match_surv_oct_2022_b, ties = "efron", id = id, cluster = id, 
                tt=function(x,t,...){x*t})
cox.zph(LTF.tvc1.coxph)

#http://pauldickman.com/video/interactions/interactions_r.pdf
car::linearHypothesis(LTF.tvc1.coxph, c("tt(tipo_de_plan_res_1)",
                         "tt(tipo_de_plan_res_1):strata(trans_cor)trans_cor=2",
                         "tt(tipo_de_plan_res_1):strata(trans_cor)trans_cor=3"))
# Fail to reject the null hypothesis that the eect of sex is the same for each age group.


Sim3 <- simPH::coxsimtvc(obj = LTF.tvc1.coxph, b = "arr_trans", btvc = "strata(trans_cor)trans_cor=2",
                  qi = "First Difference", Xj = 1, tfun = "log", from = 80, to = 2000,
                  by = 10)

#see if they are proportional
stat <- t(LTF.tvc1.coxph$coef[c(2,4)]) %*% solve(LTF.tvc1.coxph$var[c(2,4), c(2,4)]) %*% LTF.tvc1.coxph$coef[c(2,4)]
 1-pchisq(stat, df=2)
