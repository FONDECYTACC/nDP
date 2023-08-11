statadf_miss          <- rio::import("mariel_feb_23_m1.dta")%>% dplyr::mutate(motivodeegreso_mod_imp_rec=factor(motivodeegreso_mod_imp_rec)) #%>%
statadf_miss_pris     <- rio::import("mariel_feb_23_2_m1.dta")%>% dplyr::mutate(motivodeegreso_mod_imp_rec=factor(motivodeegreso_mod_imp_rec)) #%>%

cobalt::bal.tab(motivodeegreso_mod_imp_rec ~ tr_mod2+ sex_dum2+ edad_ini_cons+ esc1+ esc2+ sus_prin2+ sus_prin3+ sus_prin4+ sus_prin5+ fr_cons_sus_prin2+ fr_cons_sus_prin3+ fr_cons_sus_prin4+ fr_cons_sus_prin5+ cond_ocu2+ cond_ocu3+ cond_ocu4+ cond_ocu5+ cond_ocu6+ policonsumo+ num_hij2+ tenviv1+ tenviv2+ tenviv4+ tenviv5+ mzone2+ mzone3+ n_off_vio+ n_off_acq+ n_off_sud+ n_off_oth+ psy_com2+ psy_com3+ dep2+ rural2+ rural3+ porc_pobr+ susini2+ susini3+ susini4+ susini5+ ano_nac_corr+ cohab2+ cohab3+ cohab4+ fis_com2+ fis_com3+ rc_x1+ rc_x2+ rc_x3, data = statadf_miss_pris,
  weights = list("Beyasian Additive Regression Trees" = "weight_mult_b"),
  estimand = "ATE",
  which.treat = .all,
  un = T, 
  thresholds = c(cor = .2),
  binary = "std", continuous = "std",
  stats = c("mean.diffs", "variance.ratios"))

library(KMsurv)
data(alloauto)
fit1 <- coxph(Surv(diff,event)  ~ mot_egr_early,  #+ mot_egr_late+ tr_mod2+ sex_dum2+ edad_ini_cons+ esc1+ esc2+ sus_prin2+ sus_prin3+ 
              # sus_prin4+ sus_prin5+ fr_cons_sus_prin2+ fr_cons_sus_prin3+ fr_cons_sus_prin4+ fr_cons_sus_prin5+ cond_ocu2+ cond_ocu3+ 
              #   cond_ocu4+ cond_ocu5+ cond_ocu6+ policonsumo+ num_hij2+ tenviv1+ tenviv2+ tenviv4+ tenviv5+ mzone2+ mzone3+ 
              #   n_off_vio+ n_off_acq+ n_off_sud+ n_off_oth+ psy_com2+ psy_com3+ dep2+ rural2+ rural3+ porc_pobr+ susini2+ susini3+ 
              #   susini4+ susini5+ ano_nac_corr+ cohab2+ cohab3+ cohab4+ fis_com2+ fis_com3+ rc_x1+ rc_x2+ rc_x3
              data = statadf_miss_pris, ties="breslow") #data=alloauto,
#Note: because there are no covariates, the cumulative hazard is the same for each person

# calculate TOT_g and N_g (see K&M 11.4.1 and 11.4.2 on pg 370) 
# for type variable in alloauto dataset
Arjas_calc <- function(typ, #no sé
                       time, # agregué yo, la columna de tiempo, string
                       type, # agregué yo, la columna de predictor
                       delta, # agregué yo, la columna de evento (en 0 y 1)
                       fit,  # modelo cox
                       data # agregué yo, para llamar a los datos, objeto
                       ){
  cnt <- table(data$type)[typ] # get count in each type
  data$type <- data[[type]]
  data$time <- data[[time]]
  data$delta <- data[[delta]]
  
  
  basehaz(fit) %>%  # get cumhaz, if model has covars need adjusted cumhaz for each person
    inner_join(data, by="time") %>%  # get type for each time
    mutate(d=ifelse(type==typ,delta,0), # delta for j in type
           haz=ifelse(type==typ,hazard,0), # cumhaz for j in type
           t=ifelse(type==typ,1,0), # event time for type
           N_g=cumsum(d), # number of observed events
           cumhaz=cumsum(haz), # sum cumulative hazard
           tot=cnt-cumsum(t), 
           TOT_g=cumhaz+hazard*tot,
           time_dup= ifelse(duplicated(time) | duplicated(time, fromLast = TRUE), 1,0),
           flg=ifelse(time_dup==1 & delta==0,1,0)) %>% 
    filter(flg==0) %>% # correct cum haz for ties between groups with only one censored
    select(N_g,TOT_g) %>% mutate(type=typ)
}

# Use the Arjas_calc() function and the model without type to get TOT_g and N_g for each transplant type and produce an Arjas plot of the two N_g vs. TOT_g curves

map_dfr(0:1, .f=function(x) Arjas_calc(x, fit=fit1, data=statadf_miss_pris, time="diff", type= "mot_egr_early", delta="event")) %>%
  ggplot(aes(x=N_g,y=TOT_g,col=factor(type,labels = c("allo","auto")))) +
  geom_abline(slope=1, intercept=0, alpha=0.5, lty=2) + geom_line() +
  labs(title="K&M figure 11.14", col="",
       x="Number of failures (N_g)",y="Estimated Cumulative Hazard Rates (TOT_g)")


# arjas 2 -----------------------------------------------------------------

#https://stat.ethz.ch/pipermail/r-sig-epi/2008-May/000127.html
#https://www.researchgate.net/profile/Vincenzo-Coviello/publication/4997785_STARJAS_Stata_module_to_produce_Arjas_plot_to_check_proportional_hazards_assumption/links/5af96584aca2720af9ef22a3/STARJAS-Stata-module-to-produce-Arjas-plot-to-check-proportional-hazards-assumption.pdf

#(i) divide the data material into two subgroups
early<-subset(statadf_miss_pris,mot_egr_early==1 & mot_egr_late==0)#MTX used as an GVHD prophylactit.
late<-subset(statadf_miss_pris,mot_egr_early==0 & mot_egr_late==1)#cumsum($)
comp<-subset(statadf_miss_pris,mot_egr_early==0 & mot_egr_late==0)#cumsum($)

#(ii) sort with respect to survival time within each group
#allready done
early= arrange(early, "diff")
late= arrange(late, "diff")
comp= arrange(comp, "diff")

#(iii)
#N_g(t_i), following notation from page 370 in K.M. 1997
#d3 is the event variable.
early_cumdelta<-cumsum(early$event)
late_cumdelta<-cumsum(late$event)
comp_cumdelta<-cumsum(comp$event)
#coxMTX=coxph(Surv(MTX$t1,MTX$d3)~z1+z2+z3+z4+z5+z6+z7,data=MTX,method="breslow")
earlycox <- coxph(Surv(diff,event)  ~ 1+ tr_mod2+ sex_dum2+ edad_ini_cons+ esc1+ esc2+ sus_prin2+ sus_prin3+ 
               sus_prin4+ sus_prin5+ fr_cons_sus_prin2+ fr_cons_sus_prin3+ fr_cons_sus_prin4+ fr_cons_sus_prin5+ cond_ocu2+ cond_ocu3+ 
                 cond_ocu4+ cond_ocu5+ cond_ocu6+ policonsumo+ num_hij2+ tenviv1+ tenviv2+ tenviv4+ tenviv5+ mzone2+ mzone3+ 
                 n_off_vio+ n_off_acq+ n_off_sud+ n_off_oth+ psy_com2+ psy_com3+ dep2+ rural2+ rural3+ porc_pobr+ susini2+ susini3+ 
                 susini4+ susini5+ ano_nac_corr+ cohab2+ cohab3+ cohab4+ fis_com2+ fis_com3+ rc_x1+ rc_x2+ rc_x3,
              data = early, ties="breslow") #data=alloauto,
print("Cox-model fitted for MTX-grupp.")
#coxnoMTX=coxph(Surv(noMTX$t1,noMTX$d3)~z1+z2+z3+z4+z5+z6+z7,data=noMTX,method="breslow")
latecox<- coxph(Surv(diff,event)  ~ 1+ tr_mod2+ sex_dum2+ edad_ini_cons+ esc1+ esc2+ sus_prin2+ sus_prin3+ 
           sus_prin4+ sus_prin5+ fr_cons_sus_prin2+ fr_cons_sus_prin3+ fr_cons_sus_prin4+ fr_cons_sus_prin5+ cond_ocu2+ cond_ocu3+ 
           cond_ocu4+ cond_ocu5+ cond_ocu6+ policonsumo+ num_hij2+ tenviv1+ tenviv2+ tenviv4+ tenviv5+ mzone2+ mzone3+ 
           n_off_vio+ n_off_acq+ n_off_sud+ n_off_oth+ psy_com2+ psy_com3+ dep2+ rural2+ rural3+ porc_pobr+ susini2+ susini3+ 
           susini4+ susini5+ ano_nac_corr+ cohab2+ cohab3+ cohab4+ fis_com2+ fis_com3+ rc_x1+ rc_x2+ rc_x3,
         data = late, ties="breslow") #data=alloauto,
print("Cox-model fitted for noMTX-grupp.")
compcox<- coxph(Surv(diff,event)  ~ 1+ sex_dum2+ edad_ini_cons+ esc1+ esc2+ sus_prin2+ sus_prin3+ 
               sus_prin4+ sus_prin5+ fr_cons_sus_prin2+ fr_cons_sus_prin3+ fr_cons_sus_prin4+ fr_cons_sus_prin5+ cond_ocu2+ cond_ocu3+ 
               cond_ocu4+ cond_ocu5+ cond_ocu6+ policonsumo+ num_hij2+ tenviv1+ tenviv2+ tenviv4+ tenviv5+ mzone2+ mzone3+ 
               n_off_vio+ n_off_acq+ n_off_sud+ n_off_oth+ psy_com2+ psy_com3+ dep2+ rural2+ rural3+ porc_pobr+ susini2+ susini3+ 
               susini4+ susini5+ ano_nac_corr+ cohab2+ cohab3+ cohab4+ fis_com2+ fis_com3+ rc_x1+ rc_x2+ rc_x3,
             data = comp, ties="breslow") #data=alloauto,

early_base<-basehaz(earlycox,centered=TRUE)#estimate the cumulative baseline hazard (I guess)
late_base<-basehaz(latecox,centered=TRUE)
comp_base<-basehaz(compcox,centered=TRUE)

early_cumbase<-cumsum(early_base$hazard)
late_cumbase<-cumsum(late_base$hazard)
comp_cumbase<-cumsum(comp_base$hazard)

plot(stepfun(early_cumdelta[1:length(early_cumbase)],c(0,early_cumbase)))
lines(stepfun(late_cumdelta[1:length(late_cumbase)],c(0,late_cumbase)))
lines(stepfun(comp_cumbase[1:length(comp_cumbase)],c(0,comp_cumbase)))
#lines(c(0,20),c(0,20))

data_early <- data.frame(y = c(0, early_cumbase), 
                    x = c(early_cumdelta[1], early_cumdelta[1:length(early_cumbase)]),
                    tr_outcome = rep("early", length(early_cumbase) + 1))
data_late <-  data.frame(y = c(0, late_cumbase), 
                     x = c(late_cumdelta[1], late_cumdelta[1:length(late_cumbase)]),
                     tr_outcome = rep("late", length(late_cumbase) + 1))
data_comp <-  data.frame(y = c(0, comp_cumbase), 
                     x = c(comp_cumdelta[1], comp_cumdelta[1:length(comp_cumbase)]),
                     tr_outcome = rep("comp", length(comp_cumbase) + 1))

data_earl_lat_comp <- rbind(data_early, data_late, data_comp)

#y= estimated cum number, vs. x= cum number of events

#Results indicate that the Arjas plot, a plot of estimated cumulative hazard versus number of failures, is superior to the other
#procedures under almost every form of nonproportional hazards,
#A Comparison of Graphical Methods for Assessing the Proportional Hazards Assumption in the Cox Model

ggplot()+
  geom_step(data = data_earl_lat_comp, aes(x = x, y = y, color = tr_outcome, linetype=tr_outcome), linewidth=1)+
  theme_sjplot() +
  scale_color_manual(values = c("late" = "gray30", "early" = "gray60", "comp"= "gray2"))+
  scale_linetype_manual(values = c("late" = "dashed", "early" = "dotted", "comp"= "solid"))+ 
  labs(x= "Number of events", y= "Estimated cumulative hazards")+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkred")+  #no se ve bien
  ylim(0,max(data_earl_lat_comp$x))
#curves should be approximately linear with slope close to one.

# If the hazards are proportional, then these
# curves should be approximately linear with slope
# close to one. However, even under proportional
# hazards the curves may differ from the 45 degree

#Why do length of cumulative hazard and events differ?
#The plot does not look like figure 11.15. Can someone help me correct it?

