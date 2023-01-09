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
cap noi which pbalchk
if _rc==111 {
	cap noi net install pbalchk.pkg
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


The file is located and named as: <<dd_display: "`c(pwd)'ser_2023_0.dta" >>
 

=============================================================================
## database
=============================================================================

 
We open the files

~~~~
<<dd_do>>
use "ser_2023_1.dta", clear
<</dd_do>>
~~~~

=============================================================================
## Parametric Models
=============================================================================


### Intercept-only

We generated a list of parametric and intercept-only survival models with different distributions (Exponential, Weibull, Gompertz, Log-logistic, Log-normal & Generalized gamma. Aditionally, we defined a series of Royston-Parmar models with a function of restricted cubic splines, in which the knots (#df -1) are defined in each percentile of the distribution. We saved the estimates in the file called `parmodels_m_illness_death_23'.

~~~~
<<dd_do>>
forvalues i = 1/3 {
	// Exponential
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition `i': family Exp (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time if _trans == `i', family(exponential, fail(_status)))
	estimates store m`i'_exp
	// Weibull
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition `i': family Wei (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time if _trans == `i', family(weibull, fail(_status)))
	estimates store m`i'_weib
	// Gompertz
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition `i': family Gomp (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time if _trans == `i', family(gompertz, fail(_status)))
	estimates store m`i'_gom
	// Log logistic
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition `i': family Logl (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time if _trans == `i', family(loglogistic, fail(_status)))
	estimates store m`i'_logl
	// Log normal
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition `i': family Logn (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time if _trans == `i', family(lognormal, fail(_status)))
	estimates store m`i'_logn
	// Generalised gamma
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition `i': family Ggam (intercept-only)}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi merlin (_time if _trans == `i', family(ggamma, fail(_status)))
	estimates store m`i'_ggam
	// Royston Parmar models
	forvalues j=1/5 {
		set seed 2125
		di in yellow "{bf: ***********}"
		di in yellow "{bf: Transition `i': family RP`j' (intercept-only)}"
		di in yellow "{bf: ***********}"
		qui cap noi merlin (_time if _trans == `i', family(rp, df(`j') fail(_status)))
		estimates store m`i'_rp`j'
	}	
}

estwrite _all using "${pathdata2}parmodels_m_illness_death_23.sters", replace
<</dd_do>>
~~~~

We checked the AICs and BICs.


~~~~
<<dd_do>>
*file:///G:/Mi%20unidad/Alvacast/SISTRAT%202019%20(github)/_supp_mstates/stata/1806.01615.pdf
*rcs - restricted cubic splines on log hazard scale
*rp - Royston-Parmar model (restricted cubic spline on log cumulative hazard scale)
forvalues i = 1/3 {
	qui count if _trans == `i' & _d == 1
	// we count the amount of cases with the event in the strata
	//we call the estimates stored, and the results...
	estimates stat m`i'_*, n(`r(N)')
	//we store in a matrix de survival
	matrix stats_`i'_intonly= r(S) 
}

estimates clear

** to order AICs
*https://www.statalist.org/forums/forum/general-stata-discussion/general/1665263-sorting-matrix-including-rownames
cap qui noi mata: mata drop st_sort_matrix()
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

mata : st_sort_matrix("stats_1_intonly", 5)
mata : st_sort_matrix("stats_2_intonly", 5)
mata : st_sort_matrix("stats_3_intonly", 5)

matrix comb_intonly = (stats_1_intonly \ stats_2_intonly \ stats_3_intonly)
esttab matrix(comb_intonly) using "${pathdata2}testreg_aic_ser_23.csv", replace
esttab matrix(comb_intonly) using "${pathdata2}testreg_aic_ser_23.html", replace
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}testreg_aic_ser_23.html" >>

~~~~
<<dd_do:nocommand>>
*file:///G:/Mi%20unidad/Alvacast/SISTRAT%202019%20(github)/_supp_mstates/stata/2104.14483v1.pdf
<</dd_do>>
~~~~


In case of the more flexible parametric models (non-standard), we selected the models that showed the best trade-off between lower complexity and better fit, and this is why we also considered the BIC. If a model with less parameters had greater or equal AIC (or differences lower than 2) but also had better BIC (<=2), we favoured the model with less parameters.


### With covariates

Posteriorly, we compared models by adding polysubstance use. Equally, we added the time of arrival as a covariate to each transition (excluding the first). We saved the estimates in `parmodels_m_illness_death_23_2'.

~~~~
<<dd_do>>
global covs_2 "i.poly i.sus_prin_mod i.fr_sus_prin edad_al_ing_1 edad_ini_cons i.sex_enc i.esc_rec i.ten_viv i.dg_cie_10_rec i.sud_severity_icd10 n_off_vio n_off_acq n_off_sud i.clas porc_pobr i.macrozone"
global covs_3 "poly sus_prin_mod fr_sus_prin edad_al_ing_1 edad_ini_cons sex_enc esc_rec ten_viv dg_cie_10_rec n_off_vio n_off_acq n_off_sud clas porc_pobr macrozone"

local stname `" "poly " "poly " "poly _start" "' 
forvalues i = 1/3 {
	gettoken covs stname : stname 
/*
	// Cox w/tvc
	forvalues j=1/7 {
		di in yellow "{bf: ***********}"
		di in yellow "{bf: Transition `i': family Cox tvc `j'}"
		di in yellow "{bf: ***********}"
		set seed 2125
		qui cap noi stmerlin `covs' if _trans==`i', dist(cox) tvc(poly) dftvc(`j')
		estimates store m2_`i'_cox`j'	
	}
*/
	// Exponential
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition `i': family Exp}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi stmerlin `covs' if _trans==`i', dist(exponential)
	estimates store m2_`i'_exp

	// Weibull
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition `i': family Wei}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi stmerlin `covs' if _trans==`i', dist(weibull)
	//qui cap noi merlin (_time `covs' if _trans == `i', family(weibull, fail(_status)))
	estimates store m2_`i'_weib

	// Gompertz
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition `i': family Gomp}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi stmerlin `covs' if _trans==`i', dist(gompertz)
	//qui cap noi merlin (_time `covs' if _trans == `i', family(gompertz, fail(_status)))
	estimates store m2_`i'_gom

	// Log logistic
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition `i': family Logl}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi stmerlin `covs' if _trans==`i', dist(loglogistic)
	//qui cap noi merlin (_time `covs' if _trans == `i', family(loglogistic, fail(_status)))
	estimates store m2_`i'_logl

	// Log normal
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition `i': family Logn}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi stmerlin `covs' if _trans==`i', dist(lognormal)
	//qui cap noi merlin (_time `covs' if _trans == `i', family(lognormal, fail(_status)))
	estimates store m2_`i'_logn

	// Generalised gamma
	di in yellow "{bf: ***********}"
	di in yellow "{bf: Transition `i': family Ggam}"
	di in yellow "{bf: ***********}"
	set seed 2125
	qui cap noi stmerlin `covs' if _trans==`i', dist(ggamma)
	//qui cap noi merlin (_time `covs' if _trans == `i', family(ggamma, fail(_status)))
	estimates store m2_`i'_ggam

	// Royston Parmar models
	forvalues j=1/5 {
		forvalues h=1/5 {
			di in yellow "{bf: ***********}"
			di in yellow "{bf: Transition `i': family RP`j'}"
			di in yellow "{bf: ***********}"
			set seed 2125
			qui cap noi stmerlin `covs' if _trans == `i', dist(rp) df(`j') tvc(poly) dftvc(`h')
			//qui cap noi merlin (_time `covs' if _trans == `i', family(rp, df(`j') fail(_status)))
			estimates store m2_`i'_rp`j'_tvc`h'
			*estimates save "${pathdata2}parmodels.ster", append
		}
	}	
}
*rcs(time, df(3) orthog)
estwrite _all using "${pathdata2}parmodels_m_illness_death_23_2.sters", replace
<</dd_do>>
~~~~

Selected the models with lower fit indices.

~~~~
<<dd_do:quietly>>
*file:///G:/Mi%20unidad/Alvacast/SISTRAT%202019%20(github)/_supp_mstates/stata/1806.01615.pdf
*rcs - restricted cubic splines on log hazard scale
*rp - Royston-Parmar model (restricted cubic spline on log cumulative hazard scale)
*estread using "${pathdata2}parmodels_m2_jun_22_corr3.sters"
forvalues i = 1/3 {
	qui count if _trans == `i' & _d == 1
	estimates stat m2_`i'_*, n(`r(N)')
	matrix stats2_`i'=r(S)
}

estimates clear

mata : st_sort_matrix("stats2_1", 5)
mata : st_sort_matrix("stats2_2", 5)
mata : st_sort_matrix("stats2_3", 5)

matrix comb = (stats2_1 \ stats2_2 \ stats2_3 )
esttab matrix(comb) using "${pathdata2}testreg_aic_ser_23_2.csv", replace
esttab matrix(comb) using "${pathdata2}testreg_aic_ser_23_2.html", replace

*m2_1_cox6 m2_2_cox6 m2_3_rp3
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}testreg_aic_ser_23_2.html" >>


~~~~
<<dd_do>>
*https://twitter.com/RDAnalyticsAB/status/1480550270773911556/photo/1

//2023-01-08,  m2_2_rp4_tvc1 was the model with best BIC considering the lower differences between the models with best AIC
//2023-01-08,  we chose gompertz in the third because the rp3_tvc1 had worst BIC
qui estread "${pathdata2}parmodels_m_illness_death_23_2.sters"

local labs `" "Est" "Cons" "Par1" "Par2" "Par3" "Time in days""' 
esttab m2_1_rp5_tvc5 m2_2_rp4_tvc1 m2_3_gom using tab1_ser_23.html, /// transform(1 exp(@) @ @ @) ///
	label cells ("b(fmt(2) label(Coef)) ci(fmt(2)par label(CI95%))") compress nogap /// /* _sign */
	stats(family1 ll N, fmt(2 %9.0g %9.0g 3 2 %9.0g) labels(Dist Log-likelihood "Cases")) style(html)  ///
	title(Selected models: Parametric) ///
	mtitles("1st tr" "2nd tr" "3rd tr" "4th tr" "5th tr" "6th tr" "7th tr" "8th tr") ///
	hlinechar(`=char(151)') ///
	varlabels(none) ///
	eqlabels(`labs', lhs("Parameters")) /// 	//* eqlabels(none, lhs("Parameters")) ///
	addnotes("Note.") sfmt(%15,3g) plain wide replace
<</dd_do>>
~~~~

The selected models and their parameters can be summarised here:

<<dd_include: "${pathdata2}tab1_ser_23.html" >>


  
=============================================================================
## Transition probabilities & Lengths of Stay, Parametric Models
=============================================================================


<<dd_do:nocommand>>
global times 0 .3 1 3 5

global sim 1e5 //5e1 1e5 

global boots 1e3 //5e1 2e3 
  
*https://docs.google.com/presentation/d/1XRTPMYl8mmHggiqFvbOZvN2tRlyu5eaf/edit?pli=1#slide=id.g11ee4224963_0_2
<</dd_do>>


~~~~
<<dd_do>>

qui estread "${pathdata2}parmodels_m_illness_death_23_2.sters"

matrix tmat = (.,1,2 \ .,.,3 \ .,.,.)
matrix colnames tmat  = start TC Contact_JS
matrix rownames tmat   = start TC Contact_JS
matrix coleq tmat   = to to to
matrix roweq tmat  = from from from 
										
predictms, transmatrix(tmat) models(m2_1_rp5_tvc5 m2_2_rp4_tvc1 m2_3_gom) ///
	timevar(timevar0) /// mint(0) maxt(1826) timevar() cannot be specified with mint()/maxt()/obs()
	seed(2125) n($sim) prob los diff ratio latent /// * si agrego bootstrap, at#() limit reached, or unrecognised option
	from(1) ci at1(poly 0  _start .2) at2(poly 1  _start .2) reset bootstrap m($boots)   //* es una forma de hacer bootstrap 

<</dd_do>>
~~~~

~~~~
<<dd_do>>
rename (_prob_at1_1_*lci) (fprob_from1a_*lci_2m)
rename (_prob_at1_1_*uci) (fprob_from1a_*uci_2m)
rename (_prob_at1_1_*) (fprob_from1a_*_2m)

rename (_los_at1_1_*lci) (flos_from1a_*lci_2m)
rename (_los_at1_1_*uci) (flos_from1a_*uci_2m)
rename (_los_at1_1_*) (flos_from1a_*_2m)

rename (_prob_at2_1_*lci) (fprob_from1b_*lci_2m)
rename (_prob_at2_1_*uci) (fprob_from1b_*uci_2m)
rename (_prob_at2_1_*) (fprob_from1b_*_2m)

rename (_los_at2_1_*lci) (flos_from1b_*lci_2m)
rename (_los_at2_1_*uci) (flos_from1b_*uci_2m)
rename (_los_at2_1_*) (flos_from1b_*_2m)

rename (_ratio_prob_at2_1_*_lci) (fratio_prob_from1_*_lci_2m)
rename (_ratio_prob_at2_1_*_uci) (fratio_prob_from1_*_uci_2m)
rename (_ratio_prob_at2_1_*) (fratio_prob_from1_*_2m)
rename (_diff_prob_at2_1_*_lci) (fdiff_prob_from1_*_lci_2m)
rename (_diff_prob_at2_1_*_uci) (fdiff_prob_from1_*_uci_2m)
rename (_diff_prob_at2_1_*) (fdiff_prob_from1_*_2m)  

rename (_ratio_los_at2_1_*_lci) (fratio_los_from1_*_lci_2m)
rename (_ratio_los_at2_1_*_uci) (fratio_los_from1_*_uci_2m)
rename (_ratio_los_at2_1_*) (fratio_los_from1_*_2m)
rename (_diff_los_at2_1_*_lci) (fdiff_los_from1_*_lci_2m)
rename (_diff_los_at2_1_*_uci) (fdiff_los_from1_*_uci_2m)
rename (_diff_los_at2_1_*) (fdiff_los_from1_*_2m)  

qui estread "${pathdata2}parmodels_m_illness_death_23_2.sters"

predictms, transmatrix(tmat) models(m2_1_rp5_tvc5 m2_2_rp4_tvc1 m2_3_gom) ///
	timevar(timevar0) /// mint(0) maxt(1826) timevar() cannot be specified with mint()/maxt()/obs()
	seed(2125) n($sim) prob los diff ratio /// * si agrego bootstrap, at#() limit reached, or unrecognised option
	from(2) ci at1(poly 0  _start .2) at2(poly 1  _start .2) reset bootstrap m($boots)   //* es una forma de hacer bootstrap 
<</dd_do>>
~~~~

~~~~
<<dd_do>>
rename (_prob_at1_2_*lci) (fprob_from2a_*lci_2m)
rename (_prob_at1_2_*uci) (fprob_from2a_*uci_2m)
rename (_prob_at1_2_*) (fprob_from2a_*_2m)

rename (_los_at1_2_*lci) (flos_from2a_*lci_2m)
rename (_los_at1_2_*uci) (flos_from2a_*uci_2m)
rename (_los_at1_2_*) (flos_from2a_*_2m)

rename (_prob_at2_2_*lci) (fprob_from2b_*lci_2m)
rename (_prob_at2_2_*uci) (fprob_from2b_*uci_2m)
rename (_prob_at2_2_*) (fprob_from2b_*_2m)

rename (_los_at2_2_*lci) (flos_from2b_*lci_2m)
rename (_los_at2_2_*uci) (flos_from2b_*uci_2m)
rename (_los_at2_2_*) (flos_from2b_*_2m)

rename (_ratio_prob_at2_2_*_lci) (fratio_prob_from2_*_lci_2m)
rename (_ratio_prob_at2_2_*_uci) (fratio_prob_from2_*_uci_2m)
rename (_ratio_prob_at2_2_*) (fratio_prob_from2_*_2m)
rename (_diff_prob_at2_2_*_lci) (fdiff_prob_from2_*_lci_2m)
rename (_diff_prob_at2_2_*_uci) (fdiff_prob_from2_*_uci_2m)
rename (_diff_prob_at2_2_*) (fdiff_prob_from2_*_2m)  

rename (_ratio_los_at2_2_*_lci) (fratio_los_from2_*_lci_2m)
rename (_ratio_los_at2_2_*_uci) (fratio_los_from2_*_uci_2m)
rename (_ratio_los_at2_2_*) (fratio_los_from2_*_2m)
rename (_diff_los_at2_2_*_lci) (fdiff_los_from2_*_lci_2m)
rename (_diff_los_at2_2_*_uci) (fdiff_los_from2_*_uci_2m)
rename (_diff_los_at2_2_*) (fdiff_los_from2_*_2m) 

<</dd_do>>
~~~~

<<dd_display: "Saved at= `c(current_time)' `c(current_date)'">>

~~~~
<<dd_do:nocommand>>
	cap qui save "ser_2023_1.dta", all replace emptyok /* Before it was b2, but i want to keep it in case an error*/
<</dd_do>>
~~~~

   
<<dd_do: nocommand>>
/*
FORMA DE EXPORTAR LOS DATOS Y EL MARKDOWN

cap rm "E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/an_ser_2023_step_1.html"
dyndoc "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\SER 2023_2_step1.do", saving("E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\an_ser_2023_step_1.html") replace nostop 
copy "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\an_ser_2023_step_1.html" "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\_outputs\an_ser_2023_step_1.html", replace

cap rm "C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/an_ser_2023_step_1.html"
dyndoc "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\SER 2023_2_step1.do", saving("C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\an_ser_2023_step_1.html") replace nostop 
copy "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\an_ser_2023_step_1.html" "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\_outputs\an_ser_2023_step_1.html", replace

_outputs
*/
<</dd_do>>