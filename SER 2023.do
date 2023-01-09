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


The file is located and named as: <<dd_display: "`c(pwd)'fiscalia_mariel_oct_2022_match_SENDA_pris.dta" >>
 

=============================================================================
## Structure database and survival
=============================================================================

 
We open the files

~~~~
<<dd_do>>
use "fiscalia_mariel_jan_2023_match_SENDA.dta", clear
encode escolaridad_rec, gen(esc_rec)
encode sex, generate(sex_enc)
encode sus_principal_mod, gen(sus_prin_mod)
encode freq_cons_sus_prin, gen(fr_sus_prin)
encode compromiso_biopsicosocial, gen(comp_biosoc)
encode tenencia_de_la_vivienda_mod, gen(ten_viv)
*encode dg_cie_10_rec, generate(dg_cie_10_mental_h) *already numeric
encode dg_trs_cons_sus_or, generate(sud_severity_icd10)
encode macrozona, generate(macrozone)
gen     motivodeegreso_mod_imp_rec3 = 1
replace motivodeegreso_mod_imp_rec3 = 2 if strpos(motivodeegreso_mod_imp_rec,"Early")>0
replace motivodeegreso_mod_imp_rec3 = 3 if strpos(motivodeegreso_mod_imp_rec,"Late")>0
*Treatment variable should be a binary variable with values 0 and 1.
gen     motivodeegreso_mod_imp_rec2 = 0
replace motivodeegreso_mod_imp_rec2 = 1 if strpos(motivodeegreso_mod_imp_rec,"Early")>0
replace motivodeegreso_mod_imp_rec2 = 1 if strpos(motivodeegreso_mod_imp_rec,"Late")>0
recode motivodeegreso_mod_imp_rec3 (1=0 "Tr Completion") (3=1 "Tr Non-completion (Late)") (2=2 "Tr Non-completion (Early)"), gen(caus_disch_mod_imp_rec) 
lab var caus_disch_mod_imp_rec "Baseline treatment outcome" 

*rename Clasificación clas 
*encode Clasificación, gen(clas)
*drop Clasificación

gen     clas = 0
replace clas = 1 if strpos(Clasificación,"Mixta")>0
replace clas = 2 if strpos(Clasificación,"Rural")>0
recode clas (0=0 "Urban") (1=1 "Mixed") (2=2 "Rural"), gen(clas2) 
drop clas

rename clas2 clas

lab var clas "Classification of Municipallities of Residence into Rural-Urban" 
lab var offender_d "Offenders (proxy of event; missing not event)" 

lab var porc_pobr "Poverty of municipallities of Residence (numeric)" 
lab var comuna_residencia_cod_rec "Municipallity of Residence (code)" 

drop Clasificación

*región
format comuna_residencia_cod_rec 
gen comuna_residencia_cod_rec_num = real(comuna_residencia_cod_rec)
gen comuna_residencia_cod_rec_str = string(comuna_residencia_cod_rec_num ,"%05.0f")

gen region=substr(comuna_residencia_cod_rec_str, 1,2)
gen region_num = real(region)

drop comuna_residencia_cod_rec_num

lab var comuna_residencia_cod_rec "Municipallity of Residence (code)" 
lab var region "Region" 
lab var region_num "Region (numeric)" 

/*
label define country1 1 "Ukraine" 2 "Bulgaria" 3 "Georgia" 4 "Russia" 5 "Lithuania" 6 "Czech Republic" ///
7 "Hungary" 8 "Slovakia" 9 "Portugal" 10 "Croatia" 11 "Ireland" 12 "Estonia" 13 "France" 14 "Cyprus" ///
15 "Poland" 16 "Germany" 17 "Great Britain" 18 "Slovenia" 19 "Israel" 20 "Spain" 21 "Belgium" ///
22 "Netherlands" 23 "Switzerland" 24 "Sweden" 25 "Norway" 26 "Denmark" 27 "Finland" 
label values country country1
*/

<</dd_do>>
~~~~

We show a table of missing values

~~~~
<<dd_do>>
misstable sum poly sus_prin_mod fr_sus_prin edad_al_ing_1 edad_ini_cons sex_enc esc_rec ten_viv dg_cie_10_rec n_off_vio n_off_acq n_off_sud clas porc_pobr macrozone
<</dd_do>>
~~~~

And missing patterns

~~~~
<<dd_do>>
misstable pat poly sus_prin_mod fr_sus_prin edad_al_ing_1 edad_ini_cons sex_enc esc_rec ten_viv dg_cie_10_rec n_off_vio n_off_acq n_off_sud clas porc_pobr macrozone
<</dd_do>>
~~~~


Then we set the data base in surirval format and bring the urban-rural classification of municipallities from this [link]("https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fwww.masvidarural.gob.cl%2Fwp-content%2Fuploads%2F2021%2F04%2FClasificacion-comunas-PNDR.xlsx&wdOrigin=BROWSELINK").


===================================================================================
## Survival 
===================================================================================

~~~~
<<dd_do>>
*si no está perdido cod_region, significa que hubo un registro (0/1) y el tiempo es el tiempo desde 
*set the indicator
gen event=0
replace event=1 if !missing(offender_d)
gen _status= event

lab var event "Event" 
lab var _status "Event" 

*replace event=1 if !missing(sex)
range timevar0 90 1826 180 //added before october 23 

gen diff= age_offending_imp-edad_al_egres_imp

lab var diff "Difference age (survival)" 

*age time
stset age_offending_imp, fail(event ==1) enter(edad_al_egres_imp)
gen _time = _t	

stipw (logit motivodeegreso_mod_imp_rec2 edad_al_ing_1 edad_ini_cons sex_enc esc_rec sus_prin_mod fr_sus_prin comp_biosoc ten_viv dg_cie_10_rec sud_severity_icd10 macrozone policonsumo n_off_vio n_off_acq n_off_sud clas), distribution(gompertz) ipwtype(stabilised) vce(mestimation) genweight(ipw_wgt) 

stdescribe, weight
<</dd_do>>
~~~~

We calculated the incidence rate.

~~~~
<<dd_do>>
stsum, by (motivodeegreso_mod_imp_rec)
<</dd_do>>
~~~~


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


**Staggered entry**



Se definió un modelo de entrada en tiempo 0.

~~~~
<<dd_do>>
stset diff, failure(event ==1) 
<</dd_do>>
~~~~

Se presenta un resumen en función de la variable de interés

~~~~
<<dd_do>>
stsum, by (motivodeegreso_mod_imp_rec)
<</dd_do>>
~~~~

We compared different random intercept mixed effects models

~~~~
<<dd_do>>

mestreg i.caus_disch_mod_imp_rec edad_al_ing_1 edad_ini_cons i.sex_enc i.esc_rec i.sus_prin_mod i.fr_sus_prin i.comp_biosoc i.ten_viv i.dg_cie_10_rec i.sud_severity_icd10 i.policonsumo i.n_off_vio i.n_off_acq i.n_off_sud i.clas porc_pobr || comuna_residencia_cod_rec: , distribution(weibull)
estimates store mestreg_wei

mestreg i.caus_disch_mod_imp_rec edad_al_ing_1 edad_ini_cons i.sex_enc i.esc_rec i.sus_prin_mod i.fr_sus_prin i.comp_biosoc i.ten_viv i.dg_cie_10_rec i.sud_severity_icd10 i.policonsumo i.n_off_vio i.n_off_acq i.n_off_sud i.clas porc_pobr || comuna_residencia_cod_rec: , distribution(exponential)
estimates store mestreg_exp

mestreg i.caus_disch_mod_imp_rec edad_al_ing_1 edad_ini_cons i.sex_enc i.esc_rec i.sus_prin_mod i.fr_sus_prin i.comp_biosoc i.ten_viv i.dg_cie_10_rec i.sud_severity_icd10 i.policonsumo i.n_off_vio i.n_off_acq i.n_off_sud i.clas porc_pobr || comuna_residencia_cod_rec: , distribution(loglogistic)
estimates store mestreg_llog

mestreg i.caus_disch_mod_imp_rec edad_al_ing_1 edad_ini_cons i.sex_enc i.esc_rec i.sus_prin_mod i.fr_sus_prin i.comp_biosoc i.ten_viv i.dg_cie_10_rec i.sud_severity_icd10 i.policonsumo i.n_off_vio i.n_off_acq i.n_off_sud i.clas porc_pobr || comuna_residencia_cod_rec: , distribution(lognormal)
estimates store mestreg_ln

mestreg i.caus_disch_mod_imp_rec edad_al_ing_1 edad_ini_cons i.sex_enc i.esc_rec i.sus_prin_mod i.fr_sus_prin i.comp_biosoc i.ten_viv i.dg_cie_10_rec i.sud_severity_icd10 i.policonsumo i.n_off_vio i.n_off_acq i.n_off_sud i.clas porc_pobr || comuna_residencia_cod_rec: , distribution(gamma)
estimates store mestreg_gam

stcox i.caus_disch_mod_imp_rec edad_al_ing_1 edad_ini_cons i.sex_enc i.esc_rec i.sus_prin_mod i.fr_sus_prin i.comp_biosoc i.ten_viv i.dg_cie_10_rec i.sud_severity_icd10 i.policonsumo i.n_off_vio i.n_off_acq i.n_off_sud i.clas porc_pobr, frailty(gammma) shared(comuna_residencia_cod_rec) /* no hay otra */
estimates store stcox_clus

*estwrite mestreg_wei mestreg_exp mestreg_llog mestreg_ln mestreg_gam stcox_clus using "${pathdata2}parmodels_m3_mestreg_23.sters", replace
estwrite mestreg_wei mestreg_exp mestreg_llog mestreg_ln mestreg_gam stcox_clus using "${pathdata2}parmodels_m3_mestreg_23_2.sters", replace

/*
============
COVARIANCE STRUCTURE
============
independent one unique variance parameter per random effect, all covariances
0; the default unless the R. notation is used
exchangeable equal variances for random effects, and one common pairwise
covariance
identity equal variances for random effects, all covariances 0; the
default if the R. notation is used
unstructured all variances and covariances to be distinctly estimated
fixed(matname) user-selected variances and covariances constrained to specified
values; the remaining variances and covariances unrestricted
pattern(matname) user-selected variances and covariances constrained to be equal;
the remaining variances and covariances unrestricted
*/

/*
============
MESTREG examples
============
https://discovery.ucl.ac.uk/id/eprint/10077100/1/Dykxhoorn_10077100_Thesis_redacted.pdf
*/

<</dd_do>>
~~~~

We checked the AICs and BICs.

~~~~
<<dd_do>>
*estread "${pathdata2}parmodels_aic_bic_22_2.sters"

*file:///G:/Mi%20unidad/Alvacast/SISTRAT%202019%20(github)/_supp_mstates/stata/1806.01615.pdf
*rcs - restricted cubic splines on log hazard scale
*rp - Royston-Parmar model (restricted cubic spline on log cumulative hazard scale)
qui count if _d == 1
	// we count the amount of cases with the event in the strata
	//we call the estimates stored, and the results...
estimates stat mestreg_*, n(`r(N)')
estimates stat stcox_*, n(`r(N)')
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
	//Y = st_matrix(matname)
	//[.,(1, 2, 3, 4, 6, 5)]
 //ordeno las columnas  
    rownames = st_matrixrowstripe(matname) //[.,(1, 2, 3, 4, 6, 5)]
    sort_order = order(st_matrix(matname),  (columns))
    st_replacematrix(matname, st_matrix(matname)[sort_order,.])
    st_matrixrowstripe(matname, rownames[sort_order,.])
}

end
//mata: mata drop st_sort_matrix()

mata : st_sort_matrix("stats_1", 6)
esttab matrix(stats_1) using "testreg_me_aic_bic_23_3.csv", replace
esttab matrix(stats_1) using "testreg_me_aic_bic_23_3.html", replace

<</dd_do>>
~~~~

<<dd_include: "${pathdata2}testreg_me_aic_bic_23_3.html" >>


We selected the log-logistic model.

~~~~
<<dd_do>>

estread "${pathdata2}parmodels_m3_mestreg_23_2.sters"

estimates replay mestreg_llog

mat mat_coef= e(b)
scalar sigmasq1 =_pi^2/(6 *exp(2* mat_coef[1,47]))
scalar sigmasq2 = mat_coef[1,48]
nlcom (rho_1: (`sigmasq2')/(`sigmasq1'+`sigmasq2'))
di `sigmasq2' / `sigmasq1'+ `sigmasq2'
di .12476762/1.3805357
*.09037624

/*
============
ICCs
============
*https://www.stata.com/stata-news/news31-2/intraclass-correlations/
*https://www.statalist.org/forums/forum/general-stata-discussion/general/1536991-command-mestreg
*We can compute the residual variance using the estimate of the log of p stored 

mat mat_coef= e(b)

di _pi^2/(6 *exp(2* mat_coef[1,48]))
    . display _pi^2/(6 *exp(2*_b[ln_p:_cons]))
*The mean survival time, assuming zero random effects, can be computed with predict, mean conditional(fixedonly).
*summm m if year==1970 (dummy)
*/





*help mestreg postestimation

test 1.clas 3.clas
test 1.clas 2.clas
test 2.clas 3.clas
test 1.caus_disch_mod_imp_rec 2.caus_disch_mod_imp_rec
test 0.caus_disch_mod_imp_rec 2.caus_disch_mod_imp_rec
test 2.esc_rec 3.esc_rec


contrast clas
contrast caus_disch_mod_imp_rec 
contrast esc_rec

*para extrapolar resultados no condicionales
stcurve, survival at1(caus_disch_mod_imp_rec=0) at2(caus_disch_mod_imp_rec=1) at3(caus_disch_mod_imp_rec=2)
*recode motivodeegreso_mod_imp_rec3 (1=0 "Tr Completion") (3=1 "Tr Non-completion (Late)") (2=2 "Tr Non-completion (Early)"), gen(caus_disch_mod_imp_rec) 

*para intentar sacarle intervalos de confianza al modelo mixto
net install st0217_1.pkg
*no tiene CIs
*https://www.statalist.org/forums/forum/general-stata-discussion/general/1456424-hazard-plots-with-confidence-intervals-with-an-ipd-meta-analysis

*https://www.stata.com/manuals/memestreg.pdf


*https://www.stata.com/manuals/memestregpostestimation.pdf

predict rmst_cause_0, med  cond(ebmeans) nooff iter(1e3)

/*
https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fars.els-cdn.com%2Fcontent%2Fimage%2F1-s2.0-S0277953620301775-mmc2.docx&wdOrigin=BROWSELINK

a random effects flexible parametric model [Stata command 
mestreg or stmixed] was also be fitted to assess the effect a random intercept term for 
centre had on the mortality reduction estimate and standard error. Furthermore, this model 
allowed testing of the random effect variance, and hence the level of unexplained variability 
between centres

https://www.thelancet.com/cms/10.1016/S0140-6736(21)00731-5/attachment/ab1ea4c1-0517-45e2-aa5b-b3603cd66800/mmc1.pdf

Obtain predicted probabilities based on the contribution of both fixed
effects and random effects
	. predict pr

Obtain predicted probabilities based on the contribution of fixed effects
only
	. predict prfixed, conditional(fixedonly)

Obtain predictions of the posterior means and their standard errors
	. predict re_means*, reses(se_means*) reffects

Obtain predictions of the posterior modes and their standard errors
	. predict re_modes*, reses(se_modes*) reffects ebmodes
	
. predict re_school re_class, remeans reses(se_school se_class)
(calculating posterior means of random effects)
(using 7 quadrature points)
. generate lower = re_school - 1.96*se_school
. generate upper = re_school + 1.96*se_school
. egen tag = tag(school)
. gsort +re_school -tag
. generate rank = sum(tag)
. generate labpos = re_school + 1.96*se_school + .1
. twoway (rcap lower upper rank) (scatter re_school rank)
> (scatter labpos rank, mlabel(school) msymbol(none) mlabpos(0)),
> xtitle(rank) ytitle(predicted posterior mean) legend(off)
> xscale(range(0 28)) xlabel(1/28) ysize(2)	

Although there is some variability in the predicted posterior means, we cannot see significant differences
among the schools in this example.	


meqrlogit mviolence || country: ,mle var
predict u0, reffects
predict u0se, reses
sort u0
gen country=sum(pickone)
gsort -pickone -country
list cntry u0 u0se country if pickone==1
capture drop u0 u0se country pickone
serrbar u0 u0se country if pickone==1, scale(1.96) yline(0) mvopts(mlabel(cntry))
//Produce a Caterpillar Plot//
//label define cat1//
tab cntry

serrbar u0 u0se country if pickone==1, scale(1.96) yline(0) ytitle("Predicted Random Intercept") ///
xtitle("Country") xlabel(1 (1) 27, valuelabel labsize(2) angle(vertical) g)


*https://research-information.bris.ac.uk/ws/portalfiles/portal/186774606/leckie2016lemma9practicalstata.pdf
*/


*https://www.statalist.org/forums/forum/general-stata-discussion/general/1371006-egen-cut
egen timevar=cut(_t), at(0(0.3)11)
cap egen rmst_cause_0_mean = mean(rmst_cause_0), by(timevar macrozone caus_disch_mod_imp_rec)
cap egen rmst_cause_0_mdn= median(rmst_cause_0), by(timevar macrozone caus_disch_mod_imp_rec)
cap egen rmst_cause_0_se = semean(`var'), by(year_qrt treat_var)
cap gen lbcirmst_cause_0 = rmst_cause_0_mean - (rmst_cause_0_se * 1.96)
cap gen ubcirmst_cause_0 = rmst_cause_0_mean + (rmst_cause_0_se * 1.96)


twoway (line rmst_cause_0_mean timevar if macrozone==1 & caus_disch_mod_imp_rec==0,  sort lcolor(gs7) msize(*.7)) ///
	(line rmst_cause_0_mean timevar if macrozone==1 & caus_disch_mod_imp_rec==1,  sort lcolor(black) msize(*.7))  ///
	(line rmst_cause_0_mean timevar if macrozone==2 & caus_disch_mod_imp_rec==0,  sort lcolor(gs4) msize(*.7)) ///
	(line rmst_cause_0_mean timevar if macrozone==2 & caus_disch_mod_imp_rec==1,  sort lcolor(gs2) msize(*.7))  
	

stcurve, mean at(caus_disch_mod_imp_rec=(0 1 2)) lpattern(solid dash dot)

stcurve, mean at1(caus_disch_mod_imp_rec==0) at2(caus_disch_mod_imp_rec==1)



* n_off_vio n_off_acq n_off_sud macrozone -- tuve que sacarlos para que corriera

/*
========================
========================
# STMIXED
========================
========================
*          merlin_setup():  3010  attempt to dereference NULL pointer
*                 <istmt>:     -  function returned error
stmixed caus_disch_mod_imp_rec  edad_ini_cons sex_enc esc_rec sus_prin_mod fr_sus_prin comp_biosoc ten_viv dg_cie_10_rec sud_severity_icd10  policonsumo || comuna_residencia_cod_rec: , dist(gompertz) /*reiterate(1e3) /// not allowed */
retolerance (1e-16) ////
gh(10) ////
nonadapt ///
showadapt
/* https://arxiv.org/pdf/1709.06633v1.pdf*/
/* initmatrix pass a matrix of initial values to ml, instead of the fixed effect models used to obtain starting value */

============
# Distributions
============
distribution(exponential) fits an exponential survival model.
distribution(weibull) fits a Weibull survival model.
distribution(gompertz) fits a Gompertz survival model.
distribution(rp) fits a Royston-Parmar survival model. This is a highly flexible fully parametric
alternative to the Cox model, modelled on the log cumulative hazard scale using restricted cubic
splines.
distribution(rcs) fits a log hazard scale flexible parametric survival model. This is a highly
flexible fully parametric alternative to the Cox model, modelled on the log hazard scale using
restricted cubic splines.
df(#)  1 and 5 is sufficient
tvc(varlist) gives the name of the variables that have time-varying coefficients 
dftvc(numlist) gives the degrees of freedom for each time-dependent effects in tvc()
covariance(vartype_list), where each vartype is diagonal | exchangeable | identity | unstructured
all covariates not listed in at() are set to their sample values.

============
# Postest
============
*permite Gompertz

*Figure 1: Predicted survival for a female, aged 45, based on the fixed portion of the model.

predict s1, timelost ci  at1(caus_disch_mod_imp_rec 1) at2(caus_disch_mod_imp_rec 0)  fixedonly

* help stmixed postestimation
stmixed caus_disch_mod_imp_rec || region: , dist(weibull)

*/