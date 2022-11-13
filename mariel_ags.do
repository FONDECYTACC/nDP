<<dd_version: 2>>
<<dd_include: "E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/header.txt" >>
<<dd_include: "C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/header.txt" >>

~~~~
<<dd_do>>
clear all
cap noi which tabout
if _rc==111 {
	cap noi ssc install tabout
	}
cap noi which pathutil
if _rc==111 {
	cap noi net install pathutil, from("http://fmwww.bc.edu/repec/bocode/p/") 
	}
cap noi which pathutil
if _rc==111 {
	ssc install dirtools	
	}
cap noi which project
if _rc==111 {	
	ssc install project
	}
cap noi which stipw
if _rc==111 {	
	ssc install stipw
	}
cap noi which stpm2
if _rc==111 {	
	ssc install stpm2
	}	
cap noi which rcsgen
if _rc==111 {	
	ssc install rcsgen
	}	
cap noi which matselrc
if _rc==111 {		
cap noi net install dm79, from(http://www.stata.com/stb/stb56)
	}
<</dd_do>>
~~~~

# Exercise

Date created: <<dd_display: "`c(current_time)' `c(current_date)'">>.

Get the folder

~~~~
<<dd_do: nocommand>>
*para poner la carpeta que aloja a tu proyecto
pathutil split "`c(filename)'"
	cap qui noi cd `"`dir'"'
	global pathdata `"`dir'"' 
	di "Fecha: `c(current_date)', considerando un SO `c(os)' para el usuario: `c(username)'"
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}header.txt" >>

<<dd_display: "Path data= ${pathdata};">>

<<dd_display: "Tiempo: `c(current_date)', considerando un SO `c(os)'">>


The file is located and named as: <<dd_display: "`c(pwd)'fiscalia_mariel_oct_2022_match_SENDA.dta" >>
 

=============================================================================
## Structure database and survival
=============================================================================

 
We open the files

~~~~
<<dd_do>>
use "fiscalia_mariel_oct_2022_match_SENDA.dta", clear
encode escolaridad_rec, generate(esc_rec)
encode sex, gen(sex_enc)
encode sus_principal_mod, generate(sus_prin_mod)
encode freq_cons_sus_prin, generate(fr_sus_prin)
encode compromiso_biopsicosocial, generate(comp_biosoc)
encode tenencia_de_la_vivienda_mod, generate(ten_viv)
*encode dg_cie_10_rec, generate(dg_cie_10_mental_h) *already numeric
encode dg_trs_cons_sus_or, generate(sud_severity_icd10)
encode macrozona, generate(macrozone)
gen     motivodeegreso_mod_imp_rec3 = 1
replace motivodeegreso_mod_imp_rec3 = 2 if strpos(motivodeegreso_mod_imp_rec,"Early")>0
replace motivodeegreso_mod_imp_rec3 = 3 if strpos(motivodeegreso_mod_imp_rec,"Late")>0

*encode policonsumo, generate(policon) *already numeric

*motivodeegreso_mod_imp_rec3 edad_al_ing_fmt edad_ini_cons dias_treat_imp_sin_na_1 i.escolaridad_rec i.sus_principal_mod i.freq_cons_sus_prin i.compromiso_biopsicosocial i.tenencia_de_la_vivienda_mod i.dg_cie_10_rec i.dg_trs_cons_sus_or i.macrozona i.n_off_vio i.n_off_acq i.n_off_sud i.n_off_oth
<</dd_do>>
~~~~

Then we set the database in survival format

~~~~
<<dd_do>>
*si no está perdido cod_region, significa que hubo un registro (0/1) y el tiempo es el tiempo desde 
*set the indicator
gen event=0
replace event=1 if !missing(offender_d)
*replace event=1 if !missing(sex)

gen diff= age_offending_imp-edad_al_egres_imp

*age time
stset age_offending_imp, fail(event ==1) enter(edad_al_egres_imp)

stdescribe, weight
<</dd_do>>
~~~~

We calculate the incidence rate.

~~~~
<<dd_do>>
stsum, by (motivodeegreso_mod_imp_rec)
<</dd_do>>
~~~~


=============================================================================
## Descriptives
=============================================================================
 


~~~~
<<dd_do>>

local ttl `" "Tr Completion" "Tr Disch (Early)" "Tr Disch (Late)" "' 
forvalues i = 1/3 {
cap drop sum_*
	gettoken title ttl: ttl
cap qui egen sum_`i' = total(_t) if motivodeegreso_mod_imp_rec3==`i'
cap qui tab _d motivodeegreso_mod_imp_rec3 if _d==1 & motivodeegreso_mod_imp_rec3==`i'
scalar n_eventos`i' =r(N)
qui tabstat sum_`i', save
scalar sum_`i'_total =r(StatTotal)[1,1]
scalar ir_t = round((n_eventos`i'/sum_`i'_total)*1000,.1)
*di ir_t
*es numérico, para no generar incompatibilidad, se pasa a escalar
global dens_inc`i' "Incidence rate ratio for `title' (`i') (x1,000 person-days):  `=scalar(ir_t)'"
*di %9.1f (n_eventos`i'/sum_`i'_total)*1000
}
<</dd_do>>
~~~~

- <<dd_display: "$dens_inc1">>

- <<dd_display: "$dens_inc2">>

- <<dd_display: "$dens_inc3">>


We recode the discharge cause to contrast them

~~~~
<<dd_do>>
cap noi recode motivodeegreso_mod_imp_rec3 (1=0 "Tr Completion" ) ///
			 (2=1 "Early Disch") ///
			 (else=.), gen(tto_2_1)
cap noi recode motivodeegreso_mod_imp_rec3 (1=0 "Tr Completion" ) ///
			 (3=1 "Late Disch") ///
			 (else=.), gen(tto_3_1)
cap noi recode motivodeegreso_mod_imp_rec3 (2=0 "Early Disch" ) ///
			 (3=1 "Late Disch") ///
			 (else=.), gen(tto_3_2)
<</dd_do>>
~~~~


We explored the inicidence rate ratios (IRR) of each cause of discharge.

~~~~
<<dd_do>>

*set trace on
local stname `" "2_1" "3_1" "3_2" "' 
local titl `" "Early Disch vs Tr Completion" "Late Disch vs Tr Completion" "Late vs Early Disch" "' 
foreach s of local stname {
	gettoken title titl: titl
cap noi qui ir _d tto_`s' _t
scalar ir_`s' =round(r(irr),.01)
*di ir_`s'
scalar ir_`s'_lb =round(r(lb_irr),.01) 
*di ir_`s'_lb
scalar ir_`s'_ub =round(r(ub_irr),.01)
*di ir_`s'_ub
local ir1= ir_`s'
local ir2= ir_`s'_lb
local ir3= ir_`s'_ub
*di  in gr _col(13) " `title': IRR `ir1' (IC 95% `ir2' - `ir3') "
global irr_`s' " `title': IRR `ir1' (IC 95% `ir2' - `ir3') "
global ir_`s' "`ir1' (IC 95% `ir2' - `ir3')"
}	
<</dd_do>>
~~~~

We generated a log-rank test to compare the expected to the observed result, obtaining that:

- <<dd_display: "$irr_2_1">>, so patients with an Early Discharge had a greater incidence rate than patients in Tr Completion

- <<dd_display: "$irr_3_1">>, so patients with a Late Discharge had a greater incidence rate than patients in Tr Completion

- <<dd_display: "$irr_3_2">>, so patients with a Late Discharge had a lower incidence rate than patients with an Early Discharge


~~~~
<<dd_do>>
*set trace on
local stname `" "2_1" "3_1" "3_2" "' 
local titl `" "Early Disch vs Tr Completion" "Late Disch vs Tr Completion" "Late vs Early Disch" "' 
foreach s of local stname {
	gettoken title titl: titl
cap noi qui sts test tto_`s', logrank
scalar logrank_chi`s'= round(r(chi2),.01)
scalar logrank_df`s'= r(df)
scalar logrank_p_`s'= round(chiprob(r(df),r(chi2)),.001)
local lr1= logrank_chi`s'
local lr2= logrank_df`s'
local lr3= logrank_p_`s'
global logrank_`s' " Chi^2(`lr2')=`lr1',p=`lr3'"
}
*set trace off
<</dd_do>>
~~~~

- With a value of <<dd_display: "$logrank_2_1">>, survival curves between patients that had an Early Disch y Tr Completion were significantly different.

- With a value of <<dd_display: "$logrank_3_1">>, survival curves between patients that had an Late Disch y Tr Completion were significantly different.

- With a value of <<dd_display: "$logrank_3_2">>, survival curves between patients that had an Late y Early Disch were significantly different.


=============================================================================
## Graph
=============================================================================

We generated a graph with every type of treatment and the Nelson-Aalen estimate.

~~~~
<<dd_do>>
sts graph, na by (motivodeegreso_mod_imp_rec) ci ///
title("Comission of an offense (end with imprisonment)") /// 
subtitle("Nelson-Aalen Cum Hazards w/ Confidence Intervals 95%") ///
risktable(, size(*.5) order(1 "Tr Completion" 2 "Early Disch" 3 "Late Disch")) ///
ytitle("Cum. Hazards") ylabel(#8) ///
xtitle("Years of age") xlabel(#8) ///
note("Source: nDP, SENDA's SUD Treatments & POs Office Data period 2010-2019 ") ///
legend(rows(3)) ///
legend(cols(4)) ///
graphregion(color(white) lwidth(large)) bgcolor(white) ///
plotregion(fcolor(white)) graphregion(fcolor(white) ) /// //text(.5 1 "IR = <0.001") ///
legend(order(1 "95CI Tr Completion" 2 "Tr Completion" 3 "95CI Early Tr Disch" 4 "Early Tr Disch " 5 "95CI Late Tr Disch" 6 "Late Tr Disch" )size(*.5)region(lstyle(none)) region(c(none)) nobox)
graph save "tto.gph", replace
<</dd_do>>
~~~~

<<dd_graph: saving(tto.svg) width(800) replace>>



=============================================================================
## Survival Analyses
=============================================================================

We tested the schoefeld residuals.

~~~~
<<dd_do>>
*c("edad_al_ing_fmt", "edad_ini_cons", "dias_treat_imp_sin_na_1", "escolaridad_rec", "sus_principal_mod", "freq_cons_sus_prin", "compromiso_biopsicosocial", "tenencia_de_la_vivienda_mod", "dg_cie_10_rec", "dg_trs_cons_sus_or", "macrozona", "policonsumo", "n_prev_off", "n_off_vio", "n_off_acq", "n_off_sud", "n_off_oth")

global sim 1e5 //5e1 1e5 
global boots 1e3 //5e1 2e3
global times 0 90 365 1096 1826
range timevar0 90 1826 90

global covs "edad_al_ing_fmt edad_ini_cons dias_treat_imp_sin_na_1 sex esc_rec sus_prin_mod fr_sus_prin comp_biosoc ten_viv dg_cie_10_rec sud_severity_icd10 macrozone policonsumo n_off_vio n_off_acq n_off_sud n_off_oth"
global covs_2 "motivodeegreso_mod_imp_rec3 edad_al_ing_fmt edad_ini_cons sex_enc esc_rec sus_prin_mod fr_sus_prin comp_biosoc ten_viv dg_cie_10_rec sud_severity_icd10 macrozone policonsumo n_off_vio n_off_acq n_off_sud n_off_oth"


stcox  $covs_2 , efron robust nolog schoenfeld(sch*) scaledsch(sca*)
estat phtest, log detail
scalar chi2_scho_test = r(chi2)

mat mat_scho_test = r(phtest)

esttab matrix(mat_scho_test) using "mat_scho_test.csv", replace
esttab matrix(mat_scho_test) using "mat_scho_test.html", replace

<</dd_do>>
~~~~

<<dd_include: "${pathdata2}mat_scho_test.html" >>

~~~~
<<dd_do>>
/*
stphplot, by(n_off_vio) adjust($covs_health) ///
 xtitle("Log Time (days)", size(small)) ///
	ylabel(-4(2)8, labsize(vsmall)) ///	
	legend(pos(7) ring(0) col(1) symysize(zero) keygap(1) symxsize(large) order( 1 2) lab(1 "Outpatient") lab(2 "Residential") size(small)) ///
	ytitle("Schoenfeld residuals", size(small)) scheme(sj) graphregion(color(white)) ///
	note("{it:Note. Means and 95% CI's; Bandwidth=.8; Natural log of analysis time used.}",size(vsmall)) ///
	title("Plot of −ln{−ln(survival)} vs. ln(analysis time)" "by Treatment Modality at Baseline", size(medium)) ///
	subtitle("{it: Fourth transition}",size(small)) ///
	name(stphplot_trans_4, replace)  ///
	saving(stphplot_trans_4.gph, replace)
*/

<</dd_do>>
~~~~

We generated a list of parametric survival models with different distributions (Exponential, Weibull, Gompertz, Log-logistic, Log-normal & Generalized gamma). Aditionally, we defined a series of Royston-Parmar models with a function of restricted cubic splines, in which the knots (#df -1) are defined in each percentile of the distribution. We saved the estimates in the file called `parmodels_m2_nov_22'.


~~~~
<<dd_do>>
		// Cox w/tvc
	forvalues j=1/7 {
		di in yellow "{bf: ***********}"
		di in yellow "{bf: family Cox tvc `j'}"
		di in yellow "{bf: ***********}"
		set seed 2125
		qui cap noi stmerlin $covs_2 , dist(exponential) tvc(motivodeegreso_mod_imp_rec3) dftvc(`j')
		estimates store m2_1_cox`j'	
	}

	// Gompertz
	di in yellow "{bf: ***********}"
	di in yellow "{bf: family Gomp}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi stmerlin $covs_2 , dist(gompertz)
	//qui cap noi merlin (_time $covs if _trans == 1, family(gompertz, fail(_status)))
	estimates store m2_1_gom

	// Weibull
	di in yellow "{bf: ***********}"
	di in yellow "{bf: family Weibull}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi stmerlin $covs_2 , dist(weibull)
	//qui cap noi merlin (_time $covs if _trans == 1, family(gompertz, fail(_status)))
	estimates store m2_1_wei
	
	// Log logistic
	di in yellow "{bf: ***********}"
	di in yellow "{bf: family Logl}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi stmerlin $covs_2 , dist(loglogistic)
	//qui cap noi merlin (_time $covs if _trans == 1, family(loglogistic, fail(_status)))
	estimates store m2_1_logl

	// Log normal
	di in yellow "{bf: ***********}"
	di in yellow "{bf: family Logn}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi stmerlin $covs_2 , dist(lognormal)
	//qui cap noi merlin (_time $covs if _trans == 1, family(lognormal, fail(_status)))
	estimates store m2_1_logn
	
	// Generalised gamma
	di in yellow "{bf: ***********}"
	di in yellow "{bf: family Ggam}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi stmerlin $covs_2 , dist(ggamma)
	//qui cap noi merlin (_time $covs if _trans == 1, family(ggamma, fail(_status)))
	estimates store m2_1_ggam

	// Royston Parmar models
	forvalues j=1/10 {
		di in yellow "{bf: ***********}"
		di in yellow "{bf: family RP`j'}"
		di in yellow "{bf: ***********}"
		set seed 2125
		qui cap noi stmerlin $covs_2, dist(rp) df(`j')
		//qui cap noi merlin (_time $covs if _trans == 1, family(rp, df(`j') fail(_status)))
		estimates store m2_1_rp`j'
		*estimates save "${pathdata2}parmodels.ster", append	
	}	

*rcs(time, df(3) orthog)
estwrite _all using "${pathdata2}parmodels_m2_nov_22.sters", replace
<</dd_do>>
~~~~

We obtained a summary of distributions by AICs and BICs.

~~~~
<<dd_do>>
*file:///G:/Mi%20unidad/Alvacast/SISTRAT%202019%20(github)/_supp_mstates/stata/1806.01615.pdf
*rcs - restricted cubic splines on log hazard scale
*rp - Royston-Parmar model (restricted cubic spline on log cumulative hazard scale)
qui count if _d == 1
	// we count the amount of cases with the event in the strata
	//we call the estimates stored, and the results...
estimates stat m2_1_*, n(`r(N)')
	//we store in a matrix de survival
matrix stats_1=r(S)


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

mata : st_sort_matrix("stats_1", 5)
esttab matrix(stats_1) using "testreg_aic_nov_22.csv", replace
esttab matrix(stats_1) using "testreg_aic_nov_22.html", replace

<</dd_do>>
~~~~

<<dd_include: "${pathdata2}testreg_aic_nov_22.html" >>


In case of the more flexible parametric models (non-standard), we selected the models that showed the best trade-off between lower complexity and better fit, and this is why we also considered the BIC. If a model with less parameters had greater or equal AIC (or differences lower than 2) but also had better BIC (<=2), we favoured the model with less parameters.


### IPTCW

We estimated a Survival-time inverse-probability weighting, these estimate the weights attributed to the treatment-assignment (likelihood of being treated) and time-to-censoring models, and use them to estimate the weighted averages of the outcomes for each treatment level. 

~~~~
<<dd_do>>
*reset time, only compatible with stteffects (same entry times)
stset diff, failure(event ==1) 

cap rm bsreg1.dta bsreg2.dta

stteffects ipw (motivodeegreso_mod_imp_rec3 edad_al_ing_fmt edad_ini_cons sex_enc esc_rec sus_prin_mod fr_sus_prin comp_biosoc ten_viv dg_cie_10_rec sud_severity_icd10 macrozone policonsumo n_off_vio n_off_acq n_off_sud n_off_oth) (comp_biosoc cut_fec_nac, gamma), vce(bootstrap, nodots seed(2125) rep(200) saving(bsreg1, replace))
cap stteffects ra

stteffects ipw (motivodeegreso_mod_imp_rec3 edad_al_ing_fmt edad_ini_cons sex_enc esc_rec sus_prin_mod fr_sus_prin comp_biosoc ten_viv dg_cie_10_rec sud_severity_icd10 macrozone policonsumo n_off_vio n_off_acq n_off_sud n_off_oth) (comp_biosoc cut_fec_nac, lnormal), vce(bootstrap, nodots seed(2125) rep(200) saving(bsreg2, replace))
cap stteffects ra

*count if missing(motivodeegreso_mod_imp_rec3, edad_al_ing_fmt, edad_ini_cons, dias_treat_imp_sin_na_1, esc_rec, sus_prin_mod, fr_sus_prin, comp_biosoc, ten_viv, dg_cie_10_rec, sud_severity_icd10, macrozone, policonsumo, n_off_vio, n_off_acq, n_off_sud, n_off_oth)
<</dd_do>>
~~~~

The model indicated that is complicated to include the days in treatment, because of collinearity. However, there was no statistically significant average effect of a type of treatment.


### IPTW Royston-Parmar

First we calculated the difference between those patients who did and did not complete baseline treatment, given that the analysis is restricted to .

~~~~
<<dd_do>>
*Micki Hill & Paul C Lambert & Michael J Crowther, 2021. "Introducing stipw: inverse probability weighted parametric survival models," London Stata Conference 2021 15, Stata Users Group.
*https://view.officeapps.live.com/op/view.aspx?src=http%3A%2F%2Ffmwww.bc.edu%2Frepec%2Fusug2021%2Fusug21_hill.pptx&wdOrigin=BROWSELINK

*Treatment variable should be a binary variable with values 0 and 1.
gen     motivodeegreso_mod_imp_rec2 = 0
replace motivodeegreso_mod_imp_rec2 = 1 if strpos(motivodeegreso_mod_imp_rec,"Early")>0
replace motivodeegreso_mod_imp_rec2 = 1 if strpos(motivodeegreso_mod_imp_rec,"Late")>0

stpm2 motivodeegreso_mod_imp_rec2, scale(hazard) df(10) eform

stipw (logit motivodeegreso_mod_imp_rec2 edad_al_ing_fmt edad_ini_cons sex_enc esc_rec sus_prin_mod fr_sus_prin comp_biosoc ten_viv dg_cie_10_rec sud_severity_icd10 macrozone policonsumo n_off_vio n_off_acq n_off_sud n_off_oth), distribution(rp) df(10) ipwtype(stabilised) vce(mestimation) eform

predict rmst03 in 1, at(motivodeegreso_mod_imp_rec2 0) rmst stdp tmax(3)
predict rmst13 in 1, at(motivodeegreso_mod_imp_rec2 1) rmst stdp tmax(3)
predictnl drmst= predict(rmst at(motivodeegreso_mod_imp_rec2 1) tmax(3))- predict(rmst at(motivodeegreso_mod_imp_rec2 1) tmax(3)) in 1, se(drmst_se)

cap list rmst03 rmst13  drmst in 1
<</dd_do>>
~~~~

We used another model with only 4 degrees of freedom according to the lowest BIC 
   
~~~~
<<dd_do>>

stpm2 motivodeegreso_mod_imp_rec2, scale(hazard) df(4) eform

stipw (logit motivodeegreso_mod_imp_rec2 edad_al_ing_fmt edad_ini_cons sex_enc esc_rec sus_prin_mod fr_sus_prin comp_biosoc ten_viv dg_cie_10_rec sud_severity_icd10 macrozone policonsumo n_off_vio n_off_acq n_off_sud n_off_oth), distribution(rp) df(4) ipwtype(stabilised) vce(mestimation) eform

predict rmst03_b in 1, at(motivodeegreso_mod_imp_rec2 0) rmst stdp tmax(3)
predict rmst13_b in 1, at(motivodeegreso_mod_imp_rec2 1) rmst stdp tmax(3)
predictnl drmst_b= predict(rmst at(motivodeegreso_mod_imp_rec2 1) tmax(3))- predict(rmst at(motivodeegreso_mod_imp_rec2 1) tmax(3)) in 1, se(drmst_b_se)

cap list rmst03_b rmst13_b  drmst_b in 1
<</dd_do>>
~~~~
   
<<dd_display: "Saved at= `c(current_time)' `c(current_date)'">>

~~~~
<<dd_do:nocommand>>
	cap qui save "mariel_nov_22.dta", all replace emptyok
<</dd_do>>
~~~~


   
<<dd_do: nocommand>>
/*
FORMA DE EXPORTAR LOS DATOS Y EL MARKDOWN

cap rm "E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/analisis_mariel_nov_2022_stata.html"
dyndoc "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\mariel_ags.do", saving("E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\analisis_mariel_nov_2022_stata.html") replace nostop 
copy "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\analisis_mariel_nov_2022_stata.html" "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\_outputs\analisis_mariel_nov_2022_stata.html", replace

cap rm "C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2012 (github)/analisis_mariel_nov_2022_stata.html"
dyndoc "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\mariel_ags.do", saving("C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\analisis_mariel_nov_2022_stata.html") replace nostop 
copy "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\analisis_mariel_nov_2022_stata.html" "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\_outputs\analisis_mariel_nov_2022_stata.html", replace

_outputs
*/
<</dd_do>>