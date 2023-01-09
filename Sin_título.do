Calculated models

~~~~
<<dd_do>>

* M1[centre]@1              /// random intercept 
* The key part of the syntax is specifying a normally distributed random effect, which we’ve called M1 (it must start with a capital letter, then a number), with the variable defining the cluster in square brackets [centre], and finally, we tell merlin not to estimate a coefficient, but rather constrain it to be 1. Our model converges nicely, and we observe an estimate of \sigma = 0.897 (95% CI: 0.774, 1.039), showing substantial heterogeneity between centres.


*A model is now fitted where the assumption of proportional excess hazards is relaxed for all covariates. This is carried out by incorporating an interaction between each covariate and a restricted cubic spline function of log time with four knots (three degrees of freedom). The knots are placed evenly according to the distribution of log death times. 

*By default, each level’s random effects are assumed to have covariance(diagonal). We can relax this by estimating a correlation
*This defines a single normally distribute d random effect, called M1, defined at the id level

*https://twitter.com/RDAnalyticsAB/status/1480550277111595010/photo/1
*https://reddooranalytics.se/2022/02/18/flexible-parametric-survival-analysis-with-frailty/
*https://arxiv.org/pdf/1806.01615.pdf

	forvalues j=2/10 {
		set seed 2125
		di in yellow "{bf: ***********}"
		di in yellow "{bf: Transition : family RP`j' (intercept-only)}"
		di in yellow "{bf: ***********}"
		qui cap noi merlin (_time				/// recurrent event times
			 tipo_de_plan_res_1					/// treatment (fixed effect)
			 M1[group_match]@1					/// random int. at trial level
			 M2[group_match>id]@1 if TD_1==13 	/// random int. at id level
			 ,									///
			 family(rp, df(`j') failure(_status))), covariance(unstructured)	//  distribution & event indicator		
			estimates store m3_rp`j'0
	}		

	// Exponential
	di in yellow "{bf: ***********}"
	di in yellow "{bf: family Exponential}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi stmerlin i.caus_disch_mod_imp_rec edad_al_ing_fmt edad_ini_cons i.sex_enc i.esc_rec i.sus_prin_mod i.fr_sus_prin i.comp_biosoc i.ten_viv i.dg_cie_10_rec i.sud_severity_icd10 i.macrozone i.policonsumo i.n_off_vio i.n_off_acq i.n_off_s, dist(exponential)										/// treatment (fixed effect)
	qui cap noi merlin (_time caus_disch_mod_imp_rec		/// recurrent event times
         M1[region_num]@13  								/// random int. at trial level- coefficient is constrained to be 1 if caus_disch_mod_imp_rec==1	
         ,																	///
         family(exponential, failure(_status))) 	//  distribution & event indicator
		 *timevar(timevar0)) //timevar window of follow-up *timevar() not currently supported with family(ggamma)
	estimates store m3_exp1
	// Weibull
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition : family Wei (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time				/// recurrent event times
         tipo_de_plan_res_1					/// treatment (fixed effect)
         M1[group_match]@1					/// random int. at trial level
         M2[group_match>id]@1 if TD_1==1 	/// random int. at id level
         ,									///
         family(weibull, failure(_status))), covariance(unstructured) //  distribution & event indicator

	estimates store m3_weib1
	// Gompertz
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition : family Gomp (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time				/// recurrent event times
         caus_disch_mod_imp_rec					/// treatment (fixed effect)
         M1[strtwodigit]@1					/// random int. at trial level
        /// //* M2[group_match>id]@1 if caus_disch_mod_imp_rec==1 	/// random int. at id level
         ,									///
         family(gompertz, failure(_d))), covariance(unstructured)	//  distribution & event indicator
	estimates store m3_gom1
	// Log logistic
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition : family Logl (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time				/// recurrent event times
         tipo_de_plan_res_1					/// treatment (fixed effect)
         M1[group_match]@1					/// random int. at trial level
         M2[group_match>id]@1 if TD_1==1 	/// random int. at id level
         ,									///
         family(loglogistic, failure(_status))), covariance(unstructured)	//  distribution & event indicator
	estimates store m3_logl1
	// Log normal
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition : family Logn (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time				/// recurrent event times
         tipo_de_plan_res_1					/// treatment (fixed effect)
         M1[group_match]@1					/// random int. at trial level
         M2[group_match>id]@1 if TD_1==1 	/// random int. at id level
         ,									///
         family(lognormal, failure(_status))), covariance(unstructured)	//  distribution & event indicator
	estimates store m3_logn1
	// Generalised gamma
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition : family Ggam (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time				/// recurrent event times
         tipo_de_plan_res_1					/// treatment (fixed effect)
         M1[group_match]@1					/// random int. at trial level
         M2[group_match>id]@1 if TD_1==1 	/// random int. at id level
         ,									///
         family(ggamma, failure(_status))), covariance(unstructured)	//  distribution & event indicator
	estimates store m3_ggam1
	// Royston Parmar models
	forvalues j=2/10 {
		set seed 2125
		di in yellow "{bf: ***********}"
		di in yellow "{bf: Transition : family RP`j' (intercept-only)}"
		di in yellow "{bf: ***********}"
		qui cap noi merlin (_time				/// recurrent event times
			 tipo_de_plan_res_1					/// treatment (fixed effect)
			 M1[group_match]@1					/// random int. at trial level
			 M2[group_match>id]@1 if TD_1==1 	/// random int. at id level
			 ,									///
			 family(rp, df(`j') failure(_status))), covariance(unstructured)	//  distribution & event indicator		
			estimates store m3_rp`j'1
	}	

	// Exponential
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition : family Exp (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time				/// recurrent event times
         tipo_de_plan_res_1					/// treatment (fixed effect)
         M1[group_match]@1					/// random int. at trial level- coefficient is constrained to be 1
         M2[group_match>id]@1 if TD_1==0 	/// random int. at id level- coefficient is constrained to be 1
         ,									///
         family(exponential, failure(_status))), covariance(unstructured) //  distribution & event indicator
		 *timevar(timevar0)) //timevar window of follow-up *timevar() not currently supported with family(ggamma)
	estimates store m3_exp0
	// Weibull
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition : family Wei (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time				/// recurrent event times
         tipo_de_plan_res_1					/// treatment (fixed effect)
         M1[group_match]@1					/// random int. at trial level
         M2[group_match>id]@1 if TD_1==0 	/// random int. at id level
         ,									///
         family(weibull, failure(_status))), covariance(unstructured) //  distribution & event indicator
	estimates store m3_weib0
	// Gompertz
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition : family Gomp (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time				/// recurrent event times
         tipo_de_plan_res_1					/// treatment (fixed effect)
         M1[group_match]@1					/// random int. at trial level
         M2[group_match>id]@1 if TD_1==0 	/// random int. at id level
         ,									///
         family(gompertz, failure(_status))), covariance(unstructured)	//  distribution & event indicator
	estimates store m3_gom0
	// Log logistic
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition : family Logl (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time				/// recurrent event times
         tipo_de_plan_res_1					/// treatment (fixed effect)
         M1[group_match]@1					/// random int. at trial level
         M2[group_match>id]@1 if TD_1==0 	/// random int. at id level
         ,									///
         family(loglogistic, failure(_status))), covariance(unstructured)	//  distribution & event indicator
	estimates store m3_logl0
	// Log normal
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition : family Logn (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time				/// recurrent event times
         tipo_de_plan_res_1					/// treatment (fixed effect)
         M1[group_match]@1					/// random int. at trial level
         M2[group_match>id]@1 if TD_1==0 	/// random int. at id level
         ,									///
         family(lognormal, failure(_status))), covariance(unstructured)	//  distribution & event indicator
	estimates store m3_logn0
	// Generalised gamma
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition : family Ggam (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time				/// recurrent event times
         tipo_de_plan_res_1					/// treatment (fixed effect)
         M1[group_match]@1					/// random int. at trial level
         M2[group_match>id]@1 if TD_1==0 	/// random int. at id level
         ,									///
         family(ggamma, failure(_status))), covariance(unstructured)	//  distribution & event indicator
	estimates store m3_ggam0
	// Royston Parmar models
	forvalues j=2/10 {
		set seed 2125
		di in yellow "{bf: ***********}"
		di in yellow "{bf: Transition : family RP`j' (intercept-only)}"
		di in yellow "{bf: ***********}"
		qui cap noi merlin (_time				/// recurrent event times
			 tipo_de_plan_res_1					/// treatment (fixed effect)
			 M1[group_match]@1					/// random int. at trial level
			 M2[group_match>id]@1 if TD_1==0 	/// random int. at id level
			 ,									///
			 family(rp, df(`j') failure(_status))), covariance(unstructured)	//  distribution & event indicator		
			estimates store m3_rp`j'0
	}		
	
estwrite _all using "${pathdata2}parmodels_m_oct_22_corr3.sters", replace
	
<</dd_do>>
~~~~


~~~~
<<dd_do>>
*file:///G:/Mi%20unidad/Alvacast/SISTRAT%202019%20(github)/_supp_mstates/stata/1806.01615.pdf

qui count if _d == 1
// we count the amount of cases with the event in the strata
//we call the estimates stored, and the results...
estimates stat m3_*, n(`r(N)')
//we store in a matrix de survival
matrix stats_total=r(S)

estimates clear

** to order AICs
*https://www.statalist.org/forums/forum/general-stata-discussion/general/1665263-sorting-matrix-including-rownames
mata :

void st_sort_matrix(
//argumento de la matriz
    string scalar matname, 
//argumento de las columnas
    real   rowvector columns
    )
{
    string matrix   rownames
    real  colvector sort_order
// defino una base	
	Y = st_matrix(matname)
	//[.,(1, 2, 3, 4, 6, 5)]
 //ordeno las columnas  
    rownames = st_matrixrowstripe(matname) //[.,(1, 2, 3, 4, 6, 5)]
    sort_order = order(st_matrix(matname),  (columns))
    st_replacematrix(matname, st_matrix(matname)[sort_order,.])
    st_matrixrowstripe(matname, rownames[sort_order,.])
}

end
//mata: mata drop st_sort_matrix()

mata : st_sort_matrix("stats_total", 5)

matrix stats_total_1 = stats_total 

esttab matrix(stats_total_1) using "${pathdata2}testreg_aic_oct_22_corr3.csv", replace
<</dd_do>>
~~~~

~~~~
<<dd_do>>
* M1[centre]@1              /// random intercept 
* The key part of the syntax is specifying a normally distributed random effect, which we’ve called M1 (it must start with a capital letter, then a number), with the variable defining the cluster in square brackets [centre], and finally, we tell merlin not to estimate a coefficient, but rather constrain it to be 1. Our model converges nicely, and we observe an estimate of \sigma = 0.897 (95% CI: 0.774, 1.039), showing substantial heterogeneity between centres.


*A model is now fitted where the assumption of proportional excess hazards is relaxed for all covariates. This is carried out by incorporating an interaction between each covariate and a restricted cubic spline function of log time with four knots (three degrees of freedom). The knots are placed evenly according to the distribution of log death times. 

*By default, each level’s random effects are assumed to have covariance(diagonal). We can relax this by estimating a correlation
*This defines a single normally distribute d random effect, called M1, defined at the id level

*https://twitter.com/RDAnalyticsAB/status/1480550277111595010/photo/1
*https://reddooranalytics.se/2022/02/18/flexible-parametric-survival-analysis-with-frailty/
*https://arxiv.org/pdf/1806.01615.pdf

	// Exponential
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition : family Exp (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time				/// recurrent event times
         i.caus_disch_mod_imp_rec edad_al_ing_fmt edad_ini_cons i.sex_enc i.esc_rec i.sus_prin_mod i.fr_sus_prin i.comp_biosoc i.ten_viv i.dg_cie_10_rec i.sud_severity_icd10 i.macrozone i.policonsumo i.n_off_vio i.n_off_acq i.n_off_s /// treatment (fixed effect)
         M1[id]@1 if caus_disch_mod_imp_rec==1				/// random int. at trial level- coefficient is constrained to be 1
         ,													///
         family(exponential, failure(_status)))	//  distribution & event indicator
		 *timevar(timevar0)) //timevar window of follow-up *timevar() not currently supported with family(ggamma)
	estimates store m4_exp1
	// Weibull
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition : family Wei (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time				/// recurrent event times
         tipo_de_plan_res_1					/// treatment (fixed effect)
         M1[id]@1 if TD_1==1				/// random int. at trial level- coefficient is constrained to be 1
         ,									///
         family(weibull, failure(_status))),  //  distribution & event indicator

	estimates store m4_weib1
	// Gompertz
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition : family Gomp (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time				/// recurrent event times
         tipo_de_plan_res_1					/// treatment (fixed effect)
         M1[id]@1 if TD_1==1				/// random int. at id level- coef constrained to be 1 restricting cov term to 1
         ,									///
         family(gompertz, failure(_status))), 	//  distribution & event indicator
	estimates store m4_gom1
	// Log logistic
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition : family Logl (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time				/// recurrent event times
         tipo_de_plan_res_1					/// treatment (fixed effect)
         M1[id]@1 if TD_1==1				/// random int. at id level- coef constrained to be 1 restricting cov term to 1
         ,									///
         family(loglogistic, failure(_status))), 	//  distribution & event indicator
	estimates store m4_logl1
	// Log normal
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition : family Logn (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time				/// recurrent event times
         tipo_de_plan_res_1					/// treatment (fixed effect)
         M1[id]@1 if TD_1==1				/// random int. at id level- coef constrained to be 1 restricting cov term to 1
         ,									///
         family(lognormal, failure(_status))), 	//  distribution & event indicator
	estimates store m4_logn1
	// Generalised gamma
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition : family Ggam (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time				/// recurrent event times
         tipo_de_plan_res_1					/// treatment (fixed effect)
         M1[id]@1 if TD_1==1				/// random int. at id level- coef constrained to be 1 restricting cov term to 1
         ,									///
         family(ggamma, failure(_status))), 	//  distribution & event indicator
	estimates store m4_ggam1
	// Royston Parmar models
	forvalues j=2/10 {
		set seed 2125
		di in yellow "{bf: ***********}"
		di in yellow "{bf: Transition : family RP`j' (intercept-only)}"
		di in yellow "{bf: ***********}"
	qui cap noi merlin (_time				/// recurrent event times
         tipo_de_plan_res_1					/// treatment (fixed effect)
         M1[id]@1 if TD_1==1				/// random int. at id level- coef constrained to be 1 restricting cov term to 1
         ,									///
			 family(rp, df(`j') failure(_status))), 	//  distribution & event indicator		
			estimates store m4_rp`j'1
	}	

	// Exponential
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition : family Exp (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time				/// recurrent event times
         tipo_de_plan_res_1					/// treatment (fixed effect)
         M1[id]@1 if TD_1==0				/// random int. at id level- coef constrained to be 1 restricting cov term to 1
         ,									///
         family(exponential, failure(_status))),  //  distribution & event indicator
		 *timevar(timevar0)) //timevar window of follow-up *timevar() not currently supported with family(ggamma)
	estimates store m4_exp0
	// Weibull
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition : family Wei (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time				/// recurrent event times
         tipo_de_plan_res_1					/// treatment (fixed effect)
         M1[id]@1 if TD_1==0				/// random int. at id level- coef constrained to be 1 restricting cov term to 1
         ,									///
         family(weibull, failure(_status))),  //  distribution & event indicator
	estimates store m4_weib0
	// Gompertz
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition : family Gomp (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time				/// recurrent event times
         tipo_de_plan_res_1					/// treatment (fixed effect)
         M1[id]@1 if TD_1==0				/// random int. at id level- coef constrained to be 1 restricting cov term to 1
         ,									///
         family(gompertz, failure(_status))), 	//  distribution & event indicator
	estimates store m4_gom0
	// Log logistic
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition : family Logl (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time				/// recurrent event times
         tipo_de_plan_res_1					/// treatment (fixed effect)
         M1[id]@1 if TD_1==0				/// random int. at id level- coef constrained to be 1 restricting cov term to 1
         ,									///
         family(loglogistic, failure(_status))), 	//  distribution & event indicator
	estimates store m4_logl0
	// Log normal
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition : family Logn (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time				/// recurrent event times
         tipo_de_plan_res_1					/// treatment (fixed effect)
         M1[id]@1 if TD_1==0				/// random int. at id level- coef constrained to be 1 restricting cov term to 1
         ,									///
         family(lognormal, failure(_status))), 	//  distribution & event indicator
	estimates store m4_logn0
	// Generalised gamma
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition : family Ggam (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time				/// recurrent event times
         tipo_de_plan_res_1					/// treatment (fixed effect)
         M1[id]@1 if TD_1==0				/// random int. at id level- coef constrained to be 1 restricting cov term to 1
         ,									///
         family(ggamma, failure(_status))), 	//  distribution & event indicator
	estimates store m4_ggam0
	// Royston Parmar models
	forvalues j=2/10 {
		set seed 2125
		di in yellow "{bf: ***********}"
		di in yellow "{bf: Transition : family RP`j' (intercept-only)}"
		di in yellow "{bf: ***********}"
	qui cap noi merlin (_time				/// recurrent event times
         tipo_de_plan_res_1					/// treatment (fixed effect)
         M1[id]@1 if TD_1==0				/// random int. at id level- coef constrained to be 1 restricting cov term to 1
         ,									///
			 family(rp, df(`j') failure(_status))), 	//  distribution & event indicator		
			estimates store m4_rp`j'0
	}		
	
estwrite _all using "${pathdata2}parmodels_m2_oct_22_corr3.sters", replace
	
<</dd_do>>
~~~~



~~~~
<<dd_do>>
qui count if _d == 1
// we count the amount of cases with the event in the strata
//we call the estimates stored, and the results...
estimates stat m4_*, n(`r(N)')
//we store in a matrix de survival
matrix stats_total2=r(S)

estimates clear

mata : st_sort_matrix("stats_total2", 5)

matrix stats_total_2 = stats_total2 

esttab matrix(stats_total_2) using "${pathdata2}testreg_aic_oct_22_corr3_2.csv", replace
<</dd_do>>
~~~~

~~~~
<<dd_do>>

qui noi estread using "${pathdata2}parmodels_m_oct_22_corr3.sters"
	
estimates restore m3_rp90
estimates replay m3_rp90
predict hr0, hr at1(tipo_de_plan_res_1 1) at2(tipo_de_plan_res_1 0) timevar(timevar0) ci
			
estimates restore m3_rp61
estimates replay m3_rp61
predict hr1, hr at1(tipo_de_plan_res_1 1) at2(tipo_de_plan_res_1 0) timevar(timevar0) ci
tw 	(rarea hr0_lci hr0_uci _time, sort lcolor(blue) fcolor(blue%25)) (line hr0 _time, sort lcolor(blue) fcolor(blue%25)) ///
	(rarea hr1_lci hr1_uci _time, sort lcolor(red) fcolor(red%25)) (line hr1 _time, sort lcolor(red) fcolor(red%25))  ///
			, xtitle("Follow-up time") ytitle("Hazard ratio") ///
			ylabel(, angle(h)format(%3.2f)) ///
			legend(order(2 "HR (TNC)" 1 "95% CI" 4 "HR (TC)" 3 "95% CI" ) ring(0) pos(1))

<</dd_do>>
~~~~


~~~~
<<dd_do>>

qui noi estread using "${pathdata2}parmodels_m2_oct_22_corr3.sters"

estimates restore m4_rp90
estimates replay m4_rp90
predict hr2, hr at1(tipo_de_plan_res_1 1) at2(tipo_de_plan_res_1 0) timevar(timevar0) ci
			
estimates restore m4_rp61
estimates replay m4_rp61
predict hr3, hr at1(tipo_de_plan_res_1 1) at2(tipo_de_plan_res_1 0) timevar(timevar0) ci
tw 	(rarea hr2_lci hr2_uci _time, sort lcolor(blue) fcolor(blue%25)) (line hr2 _time, sort lcolor(blue) fcolor(blue%25)) ///
	(rarea hr3_lci hr3_uci _time, sort lcolor(red) fcolor(red%25)) (line hr3 _time, sort lcolor(red) fcolor(red%25))  ///
			, xtitle("Follow-up time") ytitle("Hazard ratio") ///
			ylabel(, angle(h)format(%3.2f)) ///
			legend(order(2 "HR (TC)" 1 "95% CI" 4 "HR (TWC)" 3 "95% CI" ) ring(0) pos(1))

<</dd_do>>
~~~~
