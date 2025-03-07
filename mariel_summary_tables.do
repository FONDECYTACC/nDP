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
cap noi which stpm2_standsurv
if _rc==111 {		
cap noi net install stpm2_standsurv.pkg, from(http://fmwww.bc.edu/RePEc/bocode/s)
	}
cap noi which fs
if _rc==111 {		
	ssc install fs
	}
cap noi which mkspline2
if _rc==111 {		
	ssc install postrcspline
	}
	
cap noi which evalue
if _rc==111 {		
	ssc install evalue
	}	
<</dd_do>>
~~~~

# Summary tables

#### #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#### #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#### Condemnatory_Sentence_Listwise_Main
#### #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_


**Survival**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23.dta", clear
*estread using "mariel_feb_23.sters", replace

keep s_tr_comp s_tr_comp_lci s_tr_comp_uci s_early_drop s_early_drop_lci s_early_drop_uci s_late_drop s_late_drop_lci s_late_drop_uci tt

*sdiff_tr_comp_early_drop sdiff_tr_comp_late_drop sdiff_early_late_drop
foreach var of varlist s_tr_comp s_tr_comp_lci s_tr_comp_uci s_early_drop s_early_drop_lci s_early_drop_uci s_late_drop s_late_drop_lci s_late_drop_uci {
			scalar variable = "`var'"
					qui summarize `var' if inrange(tt, .24, .26) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt, .45, .55) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.001)*100,.1)					
					qui summarize `var' if inrange(tt, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt, 2.85, 3.15) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt,  4.85, 5.15) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
	}	

matrix est_as11 = (ests_s_tr_comp , ests_s_tr_comp_lci, ests_s_tr_comp_uci ,  ///
	ests_s_early_drop , ests_s_early_drop_lci, ests_s_early_drop_uci , ///
	ests_s_late_drop , ests_s_late_drop_lci , ests_s_late_drop_uci )
matrix colnames est_as11 = Comp Comp_lci Comp_uci Early Early_lci Early_uci Late Late_lci Late_uci 

esttab matrix(est_as11) using "${pathdata2}prob_condsent_m0_main.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}prob_condsent_m0_main.html" >>


**RMST**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23.dta", clear
*estread using "mariel_feb_23.sters", replace

keep rmst_h0 rmst_h0_lci rmst_h0_uci rmst_h1 rmst_h1_lci rmst_h1_uci rmst_h2 rmst_h2_lci rmst_h2_uci tt


*sdiff_tr_comp_early_drop sdiff_tr_comp_late_drop sdiff_early_late_drop
foreach var of varlist rmst_h0 rmst_h0_lci rmst_h0_uci rmst_h1 rmst_h1_lci rmst_h1_uci rmst_h2 rmst_h2_lci rmst_h2_uci {
			scalar variable = "`var'"
					qui summarize `var' if inrange(tt, .24, .26) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt, .45, .55) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.0001),.001)					
					qui summarize `var' if inrange(tt, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt, 2.85, 3.15) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt,  4.85, 5.15) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.0001),.001)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
	}	

matrix est_as12 = (ests_rmst_h0 , ests_rmst_h0_lci , ests_rmst_h0_uci ,  ///
	ests_rmst_h1 , ests_rmst_h1_lci , ests_rmst_h1_uci ,  ///
	ests_rmst_h2 , ests_rmst_h2_lci , ests_rmst_h2_uci )
matrix colnames est_as12 = Comp Comp_lci Comp_uci Early Early_lci Early_uci Late Late_lci Late_uci 

esttab matrix(est_as12) using "${pathdata2}rmst_condsent_m0_main.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}rmst_condsent_m0_main.html" >>

**Difference Survival**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23.dta", clear
*estread using "mariel_feb_23.sters", replace

keep sdiff_tr_comp_early_drop sdiff_tr_comp_early_drop_lci sdiff_tr_comp_early_drop_uci  sdiff_tr_comp_late_drop sdiff_tr_comp_late_drop_lci sdiff_tr_comp_late_drop_uci sdiff_early_late_drop sdiff_early_late_drop_lci sdiff_early_late_drop_uci tt

foreach var of varlist sdiff_tr_comp_early_drop sdiff_tr_comp_early_drop_lci sdiff_tr_comp_early_drop_uci  sdiff_tr_comp_late_drop sdiff_tr_comp_late_drop_lci sdiff_tr_comp_late_drop_uci sdiff_early_late_drop sdiff_early_late_drop_lci sdiff_early_late_drop_uci {
    local newname = substr("`var'", 2, 50)
    rename `var' `newname'
}

foreach var of varlist diff_tr_comp_early_drop diff_tr_comp_early_drop_lci diff_tr_comp_early_drop_uci  diff_tr_comp_late_drop diff_tr_comp_late_drop_lci diff_tr_comp_late_drop_uci diff_early_late_drop diff_early_late_drop_lci diff_early_late_drop_uci {
	scalar variable = "`var'"
					qui summarize `var' if inrange(tt, .24, .26) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt, .45, .55) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.001)*100,.1)					
					qui summarize `var' if inrange(tt, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt, 2.85, 3.15) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt,  4.85, 5.15) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
		}
	
matrix est_as13 = (ests_diff_tr_comp_early_drop , ests_diff_tr_comp_early_drop_lci , ests_diff_tr_comp_early_drop_uci ,  ///
	ests_diff_tr_comp_late_drop , ests_diff_tr_comp_late_drop_lci, ests_diff_tr_comp_late_drop_uci , ///
	ests_diff_early_late_drop , ests_diff_early_late_drop_lci , ests_diff_early_late_drop_uci )
matrix colnames est_as13 = Comp_Early Comp_Early_lci Comp_Early_uci Comp_Late Comp_Late_lci Comp_Late_uci Early_Late Early_Late_lci Early_Late_uci 

esttab matrix(est_as13) using "${pathdata2}prob_condsent_m0_main_diff.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}prob_condsent_m0_main_diff.html" >>


**Difference RMST**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23.dta", clear
*estread using "mariel_feb_23.sters", replace

keep rmstdiff_tr_comp_early_drop rmstdiff_tr_comp_early_drop_lci rmstdiff_tr_comp_early_drop_uci rmstdiff_tr_comp_late_drop rmstdiff_tr_comp_late_drop_lci rmstdiff_tr_comp_late_drop_uci rmstdiff_early_late_drop rmstdiff_early_late_drop_lci rmstdiff_early_late_drop_uci tt

foreach var of varlist rmstdiff_tr_comp_early_drop rmstdiff_tr_comp_early_drop_lci rmstdiff_tr_comp_early_drop_uci rmstdiff_tr_comp_late_drop rmstdiff_tr_comp_late_drop_lci rmstdiff_tr_comp_late_drop_uci rmstdiff_early_late_drop rmstdiff_early_late_drop_lci rmstdiff_early_late_drop_uci {
    local newname = substr("`var'", 5, 50)
    rename `var' `newname'
}

foreach var of varlist diff_tr_comp_early_drop diff_tr_comp_early_drop_lci diff_tr_comp_early_drop_uci diff_tr_comp_late_drop diff_tr_comp_late_drop_lci diff_tr_comp_late_drop_uci diff_early_late_drop diff_early_late_drop_lci diff_early_late_drop_uci {
	scalar variable = "`var'"
					qui summarize `var' if inrange(tt, .24, .26) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt, .45, .55) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.0001),.001)					
					qui summarize `var' if inrange(tt, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt, 2.85, 3.15) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt,  4.85, 5.15) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.0001),.001)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
		}
	
matrix est_as14 = (ests_diff_tr_comp_early_drop , ests_diff_tr_comp_early_drop_lci , ests_diff_tr_comp_early_drop_uci ,  ///
	ests_diff_tr_comp_late_drop , ests_diff_tr_comp_late_drop_lci, ests_diff_tr_comp_late_drop_uci , ///
	ests_diff_early_late_drop , ests_diff_early_late_drop_lci , ests_diff_early_late_drop_uci )
matrix colnames est_as14 = Comp_Early Comp_Early_lci Comp_Early_uci Comp_Late Comp_Late_lci Comp_Late_uci Early_Late Early_Late_lci Early_Late_uci 

esttab matrix(est_as14) using "${pathdata2}rmst_condsent_m0_main_diff.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}rmst_condsent_m0_main_diff.html" >>


#### Condemnatory_Sentence_Listwise_IPW

**Difference Survival**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23.dta", clear
*estread using "mariel_feb_23.sters", replace

keep sdiff_comp_vs_late sdiff_comp_vs_late_lci sdiff_comp_vs_late_uci sdiff_comp_vs_early sdiff_comp_vs_early_lci sdiff_comp_vs_early_uci sdiff_late_vs_early sdiff_late_vs_early_lci sdiff_late_vs_early_uci tt

foreach var of varlist sdiff_comp_vs_late sdiff_comp_vs_late_lci sdiff_comp_vs_late_uci sdiff_comp_vs_early sdiff_comp_vs_early_lci sdiff_comp_vs_early_uci sdiff_late_vs_early sdiff_late_vs_early_lci sdiff_late_vs_early_uci {
    local newname = substr("`var'", 2, 50)
    rename `var' `newname'
}

foreach var of varlist diff_comp_vs_late diff_comp_vs_late_lci diff_comp_vs_late_uci diff_comp_vs_early diff_comp_vs_early_lci diff_comp_vs_early_uci diff_late_vs_early diff_late_vs_early_lci diff_late_vs_early_uci {
	scalar variable = "`var'"
					qui summarize `var' if inrange(tt, .20, .30) // tolerance of .06
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt, .40, .60) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.001)*100,.1)					
					qui summarize `var' if inrange(tt, .75, 1.25) // tolerance of .16
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt, 2.80, 3.20) // tolerance of .40
					scalar e3y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt,  4.50, 5.50) // tolerance of .40
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
		}
	
matrix est_as15 = (ests_diff_comp_vs_late , ests_diff_comp_vs_late_lci , ests_diff_comp_vs_late_uci ,  ///
	ests_diff_comp_vs_early , ests_diff_comp_vs_early_lci, ests_diff_comp_vs_early_uci , ///
	ests_diff_late_vs_early , ests_diff_late_vs_early_lci , ests_diff_late_vs_early_uci )
matrix colnames est_as15 = Comp_Late Comp_Late_lci Comp_Late_uci Comp_Early Comp_Early_lci Comp_Early_uci Early_Late Early_Late_lci Early_Late_uci 

esttab matrix(est_as15) using "${pathdata2}prob_condsent_m0_IPW_diff.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}prob_condsent_m0_IPW_diff.html" >>


**Difference RMST**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23.dta", clear
*estread using "mariel_feb_23.sters", replace

keep rmstdiff_comp_vs_late rmstdiff_comp_vs_late_lci rmstdiff_comp_vs_late_uci rmstdiff_comp_vs_early rmstdiff_comp_vs_early_lci rmstdiff_comp_vs_early_uci rmstdiff_late_vs_early rmstdiff_late_vs_early_lci rmstdiff_late_vs_early_uci tt

foreach var of varlist rmstdiff_comp_vs_late rmstdiff_comp_vs_late_lci rmstdiff_comp_vs_late_uci rmstdiff_comp_vs_early rmstdiff_comp_vs_early_lci rmstdiff_comp_vs_early_uci rmstdiff_late_vs_early rmstdiff_late_vs_early_lci rmstdiff_late_vs_early_uci {
    local newname = substr("`var'", 5, 50)
    rename `var' `newname'
}

foreach var of varlist diff_comp_vs_late diff_comp_vs_late_lci diff_comp_vs_late_uci diff_comp_vs_early diff_comp_vs_early_lci diff_comp_vs_early_uci diff_late_vs_early diff_late_vs_early_lci diff_late_vs_early_uci {
	scalar variable = "`var'"
					qui summarize `var' if inrange(tt, .20, .30) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt, .40, .60) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.0001),.001)					
					qui summarize `var' if inrange(tt, .75, 1.25) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt, 2.80, 3.20) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt,  4.50, 5.50) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.0001),.001)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
		}
	
matrix est_as16 = (ests_diff_comp_vs_early , ests_diff_comp_vs_early_lci, ests_diff_comp_vs_early_uci ,  ///
	ests_diff_comp_vs_late , ests_diff_comp_vs_late_lci , ests_diff_comp_vs_late_uci , ///
	ests_diff_late_vs_early , ests_diff_late_vs_early_lci , ests_diff_late_vs_early_uci )
matrix colnames est_as16 = Comp_Early Comp_Early_lci Comp_Early_uci Comp_Late Comp_Late_lci Comp_Late_uci Early_Late Early_Late_lci Early_Late_uci 

esttab matrix(est_as16) using "${pathdata2}rmst_condsent_m0_main_IPW_diff.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}rmst_condsent_m0_main_IPW_diff.html" >>



#### #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#### #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#### Condemnatory_Sentence_Imputed_Main
#### #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_


**Survival**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23_m1.dta", clear

keep s_tr_comp s_tr_comp_lci s_tr_comp_uci s_early_drop s_early_drop_lci s_early_drop_uci s_late_drop s_late_drop_lci s_late_drop_uci tt

*sdiff_tr_comp_early_drop sdiff_tr_comp_late_drop sdiff_early_late_drop
foreach var of varlist s_tr_comp s_tr_comp_lci s_tr_comp_uci s_early_drop s_early_drop_lci s_early_drop_uci s_late_drop s_late_drop_lci s_late_drop_uci {
			scalar variable = "`var'"
					qui summarize `var' if inrange(tt, .24, .26) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt, .45, .55) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.001)*100,.1)					
					qui summarize `var' if inrange(tt, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt, 2.85, 3.15) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt,  4.85, 5.15) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
	}	

matrix est_as11m1 = (ests_s_tr_comp , ests_s_tr_comp_lci, ests_s_tr_comp_uci ,  ///
	ests_s_early_drop , ests_s_early_drop_lci, ests_s_early_drop_uci , ///
	ests_s_late_drop , ests_s_late_drop_lci , ests_s_late_drop_uci )
matrix colnames est_as11m1 = Comp Comp_lci Comp_uci Early Early_lci Early_uci Late Late_lci Late_uci 

esttab matrix(est_as11m1) using "${pathdata2}prob_condsent_m1_main.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}prob_condsent_m1_main.html" >>


**RMST**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23_m1.dta", clear

keep rmst_h0 rmst_h0_lci rmst_h0_uci rmst_h1 rmst_h1_lci rmst_h1_uci rmst_h2 rmst_h2_lci rmst_h2_uci tt


*sdiff_tr_comp_early_drop sdiff_tr_comp_late_drop sdiff_early_late_drop
foreach var of varlist rmst_h0 rmst_h0_lci rmst_h0_uci rmst_h1 rmst_h1_lci rmst_h1_uci rmst_h2 rmst_h2_lci rmst_h2_uci {
			scalar variable = "`var'"
					qui summarize `var' if inrange(tt, .24, .26) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt, .45, .55) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.0001),.001)					
					qui summarize `var' if inrange(tt, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt, 2.85, 3.15) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt,  4.85, 5.15) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.0001),.001)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
	}	

matrix est_as12m1 = (ests_rmst_h0 , ests_rmst_h0_lci , ests_rmst_h0_uci ,  ///
	ests_rmst_h1 , ests_rmst_h1_lci , ests_rmst_h1_uci ,  ///
	ests_rmst_h2 , ests_rmst_h2_lci , ests_rmst_h2_uci )
matrix colnames est_as12m1 = Comp Comp_lci Comp_uci Early Early_lci Early_uci Late Late_lci Late_uci 

esttab matrix(est_as12m1) using "${pathdata2}rmst_condsent_m1_main.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}rmst_condsent_m1_main.html" >>

**Difference Survival**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23_m1.dta", clear

keep sdiff_tr_comp_early_drop sdiff_tr_comp_early_drop_lci sdiff_tr_comp_early_drop_uci  sdiff_tr_comp_late_drop sdiff_tr_comp_late_drop_lci sdiff_tr_comp_late_drop_uci sdiff_early_late_drop sdiff_early_late_drop_lci sdiff_early_late_drop_uci tt

foreach var of varlist sdiff_tr_comp_early_drop sdiff_tr_comp_early_drop_lci sdiff_tr_comp_early_drop_uci  sdiff_tr_comp_late_drop sdiff_tr_comp_late_drop_lci sdiff_tr_comp_late_drop_uci sdiff_early_late_drop sdiff_early_late_drop_lci sdiff_early_late_drop_uci {
    local newname = substr("`var'", 2, 50)
    rename `var' `newname'
}

foreach var of varlist diff_tr_comp_early_drop diff_tr_comp_early_drop_lci diff_tr_comp_early_drop_uci  diff_tr_comp_late_drop diff_tr_comp_late_drop_lci diff_tr_comp_late_drop_uci diff_early_late_drop diff_early_late_drop_lci diff_early_late_drop_uci {
	scalar variable = "`var'"
					qui summarize `var' if inrange(tt, .24, .26) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt, .45, .55) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.001)*100,.1)					
					qui summarize `var' if inrange(tt, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt, 2.85, 3.15) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt,  4.85, 5.15) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
		}
	
matrix est_as13m1 = (ests_diff_tr_comp_early_drop , ests_diff_tr_comp_early_drop_lci , ests_diff_tr_comp_early_drop_uci ,  ///
	ests_diff_tr_comp_late_drop , ests_diff_tr_comp_late_drop_lci, ests_diff_tr_comp_late_drop_uci , ///
	ests_diff_early_late_drop , ests_diff_early_late_drop_lci , ests_diff_early_late_drop_uci )
matrix colnames est_as13m1 = Comp_Early Comp_Early_lci Comp_Early_uci Comp_Late Comp_Late_lci Comp_Late_uci Early_Late Early_Late_lci Early_Late_uci 

esttab matrix(est_as13m1) using "${pathdata2}prob_condsent_m1_main_diff.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}prob_condsent_m1_main_diff.html" >>


**Difference RMST**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23_m1.dta", clear

keep rmstdiff_tr_comp_early_drop rmstdiff_tr_comp_early_drop_lci rmstdiff_tr_comp_early_drop_uci rmstdiff_tr_comp_late_drop rmstdiff_tr_comp_late_drop_lci rmstdiff_tr_comp_late_drop_uci rmstdiff_early_late_drop rmstdiff_early_late_drop_lci rmstdiff_early_late_drop_uci tt

foreach var of varlist rmstdiff_tr_comp_early_drop rmstdiff_tr_comp_early_drop_lci rmstdiff_tr_comp_early_drop_uci rmstdiff_tr_comp_late_drop rmstdiff_tr_comp_late_drop_lci rmstdiff_tr_comp_late_drop_uci rmstdiff_early_late_drop rmstdiff_early_late_drop_lci rmstdiff_early_late_drop_uci {
    local newname = substr("`var'", 5, 50)
    rename `var' `newname'
}

foreach var of varlist diff_tr_comp_early_drop diff_tr_comp_early_drop_lci diff_tr_comp_early_drop_uci diff_tr_comp_late_drop diff_tr_comp_late_drop_lci diff_tr_comp_late_drop_uci diff_early_late_drop diff_early_late_drop_lci diff_early_late_drop_uci {
	scalar variable = "`var'"
					qui summarize `var' if inrange(tt, .10, .30) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt, .35, .65) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.0001),.001)					
					qui summarize `var' if inrange(tt, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt, 2.85, 3.15) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt,  4.85, 5.15) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.0001),.001)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
		}
	
matrix est_as14m1 = (ests_diff_tr_comp_early_drop , ests_diff_tr_comp_early_drop_lci , ests_diff_tr_comp_early_drop_uci ,  ///
	ests_diff_tr_comp_late_drop , ests_diff_tr_comp_late_drop_lci, ests_diff_tr_comp_late_drop_uci , ///
	ests_diff_early_late_drop , ests_diff_early_late_drop_lci , ests_diff_early_late_drop_uci )
matrix colnames est_as14m1 = Comp_Early Comp_Early_lci Comp_Early_uci Comp_Late Comp_Late_lci Comp_Late_uci Early_Late Early_Late_lci Early_Late_uci 

esttab matrix(est_as14m1) using "${pathdata2}rmst_condsent_m1_main_diff.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}rmst_condsent_m1_main_diff.html" >>


#### Condemnatory_Sentence_Imputed_IPW

**Difference Survival**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23_m1.dta", clear
*estread using "mariel_feb_23.sters", replace

keep sdiff_comp_vs_late sdiff_comp_vs_late_lci sdiff_comp_vs_late_uci sdiff_comp_vs_early sdiff_comp_vs_early_lci sdiff_comp_vs_early_uci sdiff_late_vs_early sdiff_late_vs_early_lci sdiff_late_vs_early_uci tt2

foreach var of varlist sdiff_comp_vs_late sdiff_comp_vs_late_lci sdiff_comp_vs_late_uci sdiff_comp_vs_early sdiff_comp_vs_early_lci sdiff_comp_vs_early_uci sdiff_late_vs_early sdiff_late_vs_early_lci sdiff_late_vs_early_uci {
    local newname = substr("`var'", 2, 50)
    rename `var' `newname'
}

foreach var of varlist diff_comp_vs_late diff_comp_vs_late_lci diff_comp_vs_late_uci diff_comp_vs_early diff_comp_vs_early_lci diff_comp_vs_early_uci diff_late_vs_early diff_late_vs_early_lci diff_late_vs_early_uci {
	scalar variable = "`var'"
					qui summarize `var' if inrange(tt2, .10, .30) // tolerance of .06
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt2, .35, .65) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.001)*100,.1)					
					qui summarize `var' if inrange(tt2, .75, 1.25) // tolerance of .16
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt2, 2.80, 3.20) // tolerance of .40
					scalar e3y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt2,  4.50, 5.50) // tolerance of .40
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
		}
	
matrix est_as15m1 = (ests_diff_comp_vs_late , ests_diff_comp_vs_late_lci , ests_diff_comp_vs_late_uci ,  ///
	ests_diff_comp_vs_early , ests_diff_comp_vs_early_lci, ests_diff_comp_vs_early_uci , ///
	ests_diff_late_vs_early , ests_diff_late_vs_early_lci , ests_diff_late_vs_early_uci )
matrix colnames est_as15m1 = Comp_Late Comp_Late_lci Comp_Late_uci Comp_Early Comp_Early_lci Comp_Early_uci Early_Late Early_Late_lci Early_Late_uci 

esttab matrix(est_as15m1) using "${pathdata2}prob_condsent_m1_IPW_diff.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}prob_condsent_m1_IPW_diff.html" >>


**Difference RMST**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23_m1.dta", clear
*estread using "mariel_feb_23.sters", replace

keep rmstdiff_comp_vs_late rmstdiff_comp_vs_late_lci rmstdiff_comp_vs_late_uci rmstdiff_comp_vs_early rmstdiff_comp_vs_early_lci rmstdiff_comp_vs_early_uci rmstdiff_late_vs_early rmstdiff_late_vs_early_lci rmstdiff_late_vs_early_uci tt2

foreach var of varlist rmstdiff_comp_vs_late rmstdiff_comp_vs_late_lci rmstdiff_comp_vs_late_uci rmstdiff_comp_vs_early rmstdiff_comp_vs_early_lci rmstdiff_comp_vs_early_uci rmstdiff_late_vs_early rmstdiff_late_vs_early_lci rmstdiff_late_vs_early_uci {
    local newname = substr("`var'", 5, 50)
    rename `var' `newname'
}

foreach var of varlist diff_comp_vs_late diff_comp_vs_late_lci diff_comp_vs_late_uci diff_comp_vs_early diff_comp_vs_early_lci diff_comp_vs_early_uci diff_late_vs_early diff_late_vs_early_lci diff_late_vs_early_uci {
	scalar variable = "`var'"
					qui summarize `var' if inrange(tt2, .10, .30) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt2, .35, .65) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.0001),.001)					
					qui summarize `var' if inrange(tt2, .75, 1.25) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt2, 2.80, 3.20) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt2,  4.50, 5.50) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.0001),.001)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
		}
	
matrix est_as16m1 = (ests_diff_comp_vs_early , ests_diff_comp_vs_early_lci, ests_diff_comp_vs_early_uci ,  ///
	ests_diff_comp_vs_late , ests_diff_comp_vs_late_lci , ests_diff_comp_vs_late_uci , ///
	ests_diff_late_vs_early , ests_diff_late_vs_early_lci , ests_diff_late_vs_early_uci )
matrix colnames est_as16m1 = Comp_Early Comp_Early_lci Comp_Early_uci Comp_Late Comp_Late_lci Comp_Late_uci Early_Late Early_Late_lci Early_Late_uci 

esttab matrix(est_as16m1) using "${pathdata2}rmst_condsent_m1_main_IPW_diff.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}rmst_condsent_m1_main_IPW_diff.html" >>


#### #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#### #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#### Condemnatory_Sentence_Imputed(2)_Main
#### #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_


**Survival**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23_m2.dta", clear

keep s_tr_comp s_tr_comp_lci s_tr_comp_uci s_early_drop s_early_drop_lci s_early_drop_uci s_late_drop s_late_drop_lci s_late_drop_uci tt

*sdiff_tr_comp_early_drop sdiff_tr_comp_late_drop sdiff_early_late_drop
foreach var of varlist s_tr_comp s_tr_comp_lci s_tr_comp_uci s_early_drop s_early_drop_lci s_early_drop_uci s_late_drop s_late_drop_lci s_late_drop_uci {
			scalar variable = "`var'"
					qui summarize `var' if inrange(tt, .24, .26) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt, .45, .55) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.001)*100,.1)					
					qui summarize `var' if inrange(tt, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt, 2.85, 3.15) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt,  4.85, 5.15) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
	}	

matrix est_as11m2 = (ests_s_tr_comp , ests_s_tr_comp_lci, ests_s_tr_comp_uci ,  ///
	ests_s_early_drop , ests_s_early_drop_lci, ests_s_early_drop_uci , ///
	ests_s_late_drop , ests_s_late_drop_lci , ests_s_late_drop_uci )
matrix colnames est_as11m2 = Comp Comp_lci Comp_uci Early Early_lci Early_uci Late Late_lci Late_uci 

esttab matrix(est_as11m2) using "${pathdata2}prob_condsent_m2_main.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}prob_condsent_m2_main.html" >>


**RMST**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23_m2.dta", clear

keep rmst_h0 rmst_h0_lci rmst_h0_uci rmst_h1 rmst_h1_lci rmst_h1_uci rmst_h2 rmst_h2_lci rmst_h2_uci tt


*sdiff_tr_comp_early_drop sdiff_tr_comp_late_drop sdiff_early_late_drop
foreach var of varlist rmst_h0 rmst_h0_lci rmst_h0_uci rmst_h1 rmst_h1_lci rmst_h1_uci rmst_h2 rmst_h2_lci rmst_h2_uci {
			scalar variable = "`var'"
					qui summarize `var' if inrange(tt, .24, .26) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt, .45, .55) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.0001),.001)					
					qui summarize `var' if inrange(tt, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt, 2.85, 3.15) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt,  4.85, 5.15) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.0001),.001)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
	}	

matrix est_as12m2 = (ests_rmst_h0 , ests_rmst_h0_lci , ests_rmst_h0_uci ,  ///
	ests_rmst_h1 , ests_rmst_h1_lci , ests_rmst_h1_uci ,  ///
	ests_rmst_h2 , ests_rmst_h2_lci , ests_rmst_h2_uci )
matrix colnames est_as12m2 = Comp Comp_lci Comp_uci Early Early_lci Early_uci Late Late_lci Late_uci 

esttab matrix(est_as12m2) using "${pathdata2}rmst_condsent_m2_main.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}rmst_condsent_m2_main.html" >>

**Difference Survival**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23_m2.dta", clear

keep sdiff_tr_comp_early_drop sdiff_tr_comp_early_drop_lci sdiff_tr_comp_early_drop_uci  sdiff_tr_comp_late_drop sdiff_tr_comp_late_drop_lci sdiff_tr_comp_late_drop_uci sdiff_early_late_drop sdiff_early_late_drop_lci sdiff_early_late_drop_uci tt

foreach var of varlist sdiff_tr_comp_early_drop sdiff_tr_comp_early_drop_lci sdiff_tr_comp_early_drop_uci  sdiff_tr_comp_late_drop sdiff_tr_comp_late_drop_lci sdiff_tr_comp_late_drop_uci sdiff_early_late_drop sdiff_early_late_drop_lci sdiff_early_late_drop_uci {
    local newname = substr("`var'", 2, 50)
    rename `var' `newname'
}

foreach var of varlist diff_tr_comp_early_drop diff_tr_comp_early_drop_lci diff_tr_comp_early_drop_uci  diff_tr_comp_late_drop diff_tr_comp_late_drop_lci diff_tr_comp_late_drop_uci diff_early_late_drop diff_early_late_drop_lci diff_early_late_drop_uci {
	scalar variable = "`var'"
					qui summarize `var' if inrange(tt, .24, .26) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt, .45, .55) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.001)*100,.1)					
					qui summarize `var' if inrange(tt, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt, 2.85, 3.15) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt,  4.85, 5.15) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
		}
	
matrix est_as13m2 = (ests_diff_tr_comp_early_drop , ests_diff_tr_comp_early_drop_lci , ests_diff_tr_comp_early_drop_uci ,  ///
	ests_diff_tr_comp_late_drop , ests_diff_tr_comp_late_drop_lci, ests_diff_tr_comp_late_drop_uci , ///
	ests_diff_early_late_drop , ests_diff_early_late_drop_lci , ests_diff_early_late_drop_uci )
matrix colnames est_as13m2 = Comp_Early Comp_Early_lci Comp_Early_uci Comp_Late Comp_Late_lci Comp_Late_uci Early_Late Early_Late_lci Early_Late_uci 

esttab matrix(est_as13m2) using "${pathdata2}prob_condsent_m2_main_diff.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}prob_condsent_m2_main_diff.html" >>


**Difference RMST**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23_m2.dta", clear


keep rmstdiff_tr_comp_early_drop rmstdiff_tr_comp_early_drop_lci rmstdiff_tr_comp_early_drop_uci rmstdiff_tr_comp_late_drop rmstdiff_tr_comp_late_drop_lci rmstdiff_tr_comp_late_drop_uci rmstdiff_early_late_drop rmstdiff_early_late_drop_lci rmstdiff_early_late_drop_uci tt

foreach var of varlist rmstdiff_tr_comp_early_drop rmstdiff_tr_comp_early_drop_lci rmstdiff_tr_comp_early_drop_uci rmstdiff_tr_comp_late_drop rmstdiff_tr_comp_late_drop_lci rmstdiff_tr_comp_late_drop_uci rmstdiff_early_late_drop rmstdiff_early_late_drop_lci rmstdiff_early_late_drop_uci {
    local newname = substr("`var'", 5, 50)
    rename `var' `newname'
}

foreach var of varlist diff_tr_comp_early_drop diff_tr_comp_early_drop_lci diff_tr_comp_early_drop_uci diff_tr_comp_late_drop diff_tr_comp_late_drop_lci diff_tr_comp_late_drop_uci diff_early_late_drop diff_early_late_drop_lci diff_early_late_drop_uci {
	scalar variable = "`var'"
					qui summarize `var' if inrange(tt, .24, .26) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt, .45, .55) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.0001),.001)					
					qui summarize `var' if inrange(tt, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt, 2.85, 3.15) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt,  4.85, 5.15) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.0001),.001)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
		}
	
matrix est_as14m2 = (ests_diff_tr_comp_early_drop , ests_diff_tr_comp_early_drop_lci , ests_diff_tr_comp_early_drop_uci ,  ///
	ests_diff_tr_comp_late_drop , ests_diff_tr_comp_late_drop_lci, ests_diff_tr_comp_late_drop_uci , ///
	ests_diff_early_late_drop , ests_diff_early_late_drop_lci , ests_diff_early_late_drop_uci )
matrix colnames est_as14m2 = Comp_Early Comp_Early_lci Comp_Early_uci Comp_Late Comp_Late_lci Comp_Late_uci Early_Late Early_Late_lci Early_Late_uci 

esttab matrix(est_as14m2) using "${pathdata2}rmst_condsent_m2_main_diff.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}rmst_condsent_m2_main_diff.html" >>




#### #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#### #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#### Imprisonment_Listwise_Main
#### #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

**Survival**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23_2.dta", clear
*estread using "mariel_feb_23_2".sters", replace

keep s_tr_comp s_tr_comp_lci s_tr_comp_uci s_early_drop s_early_drop_lci s_early_drop_uci s_late_drop s_late_drop_lci s_late_drop_uci tt

*sdiff_tr_comp_early_drop sdiff_tr_comp_late_drop sdiff_early_late_drop
foreach var of varlist s_tr_comp s_tr_comp_lci s_tr_comp_uci s_early_drop s_early_drop_lci s_early_drop_uci s_late_drop s_late_drop_lci s_late_drop_uci {
			scalar variable = "`var'"
					qui summarize `var' if inrange(tt, .24, .26) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt, .45, .55) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.001)*100,.1)					
					qui summarize `var' if inrange(tt, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt, 2.85, 3.15) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt,  4.85, 5.15) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
	}	

matrix est_as21 = (ests_s_tr_comp , ests_s_tr_comp_lci, ests_s_tr_comp_uci ,  ///
	ests_s_early_drop , ests_s_early_drop_lci, ests_s_early_drop_uci , ///
	ests_s_late_drop , ests_s_late_drop_lci , ests_s_late_drop_uci )
matrix colnames est_as21 = Comp Comp_lci Comp_uci Early Early_lci Early_uci Late Late_lci Late_uci 

esttab matrix(est_as21) using "${pathdata2}prob_prison_m0_main.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}prob_prison_m0_main.html" >>


**RMST**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23_2.dta", clear
*estread using "mariel_feb_23_2".sters", replace

keep rmst_h0 rmst_h0_lci rmst_h0_uci rmst_h1 rmst_h1_lci rmst_h1_uci rmst_h2 rmst_h2_lci rmst_h2_uci tt


*sdiff_tr_comp_early_drop sdiff_tr_comp_late_drop sdiff_early_late_drop
foreach var of varlist rmst_h0 rmst_h0_lci rmst_h0_uci rmst_h1 rmst_h1_lci rmst_h1_uci rmst_h2 rmst_h2_lci rmst_h2_uci {
			scalar variable = "`var'"
					qui summarize `var' if inrange(tt, .24, .25) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt, .45, .50) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.0001),.001)					
					qui summarize `var' if inrange(tt, .95, 1.0) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt, 2.85, 3.0) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt,  4.85, 5.00) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.0001),.001)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
	}	

matrix est_as22 = (ests_rmst_h0 , ests_rmst_h0_lci , ests_rmst_h0_uci ,  ///
	ests_rmst_h1 , ests_rmst_h1_lci , ests_rmst_h1_uci ,  ///
	ests_rmst_h2 , ests_rmst_h2_lci , ests_rmst_h2_uci )
matrix colnames est_as22 = Comp Comp_lci Comp_uci Early Early_lci Early_uci Late Late_lci Late_uci 

esttab matrix(est_as22) using "${pathdata2}rmst_prison_m0_main.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}rmst_prison_m0_main.html" >>


**Difference Survival**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23_2.dta", clear
*estread using "mariel_feb_23_2.sters", replace

keep sdiff_tr_comp_early_drop sdiff_tr_comp_early_drop_lci sdiff_tr_comp_early_drop_uci  sdiff_tr_comp_late_drop sdiff_tr_comp_late_drop_lci sdiff_tr_comp_late_drop_uci sdiff_early_late_drop sdiff_early_late_drop_lci sdiff_early_late_drop_uci tt

foreach var of varlist sdiff_tr_comp_early_drop sdiff_tr_comp_early_drop_lci sdiff_tr_comp_early_drop_uci  sdiff_tr_comp_late_drop sdiff_tr_comp_late_drop_lci sdiff_tr_comp_late_drop_uci sdiff_early_late_drop sdiff_early_late_drop_lci sdiff_early_late_drop_uci {
    local newname = substr("`var'", 2, 50)
    rename `var' `newname'
}

foreach var of varlist diff_tr_comp_early_drop diff_tr_comp_early_drop_lci diff_tr_comp_early_drop_uci  diff_tr_comp_late_drop diff_tr_comp_late_drop_lci diff_tr_comp_late_drop_uci diff_early_late_drop diff_early_late_drop_lci diff_early_late_drop_uci {
	scalar variable = "`var'"
					qui summarize `var' if inrange(tt, .24, .26) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt, .45, .55) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.001)*100,.1)					
					qui summarize `var' if inrange(tt, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt, 2.85, 3.15) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt,  4.85, 5.15) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
		}
	
matrix est_as23 = (ests_diff_tr_comp_early_drop , ests_diff_tr_comp_early_drop_lci , ests_diff_tr_comp_early_drop_uci ,  ///
	ests_diff_tr_comp_late_drop , ests_diff_tr_comp_late_drop_lci, ests_diff_tr_comp_late_drop_uci , ///
	ests_diff_early_late_drop , ests_diff_early_late_drop_lci , ests_diff_early_late_drop_uci )
matrix colnames est_as23 = Comp_Early Comp_Early_lci Comp_Early_uci Comp_Late Comp_Late_lci Comp_Late_uci Early_Late Early_Late_lci Early_Late_uci 

esttab matrix(est_as23) using "${pathdata2}prob_prison_m0_main_diff.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}prob_prison_m0_main_diff.html" >>


**Difference RMST**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23_2.dta", clear
*estread using "mariel_feb_23_2.sters", replace

keep rmstdiff_tr_comp_early_drop rmstdiff_tr_comp_early_drop_lci rmstdiff_tr_comp_early_drop_uci rmstdiff_tr_comp_late_drop rmstdiff_tr_comp_late_drop_lci rmstdiff_tr_comp_late_drop_uci rmstdiff_early_late_drop rmstdiff_early_late_drop_lci rmstdiff_early_late_drop_uci tt

foreach var of varlist rmstdiff_tr_comp_early_drop rmstdiff_tr_comp_early_drop_lci rmstdiff_tr_comp_early_drop_uci rmstdiff_tr_comp_late_drop rmstdiff_tr_comp_late_drop_lci rmstdiff_tr_comp_late_drop_uci rmstdiff_early_late_drop rmstdiff_early_late_drop_lci rmstdiff_early_late_drop_uci {
    local newname = substr("`var'", 5, 50)
    rename `var' `newname'
}

foreach var of varlist diff_tr_comp_early_drop diff_tr_comp_early_drop_lci diff_tr_comp_early_drop_uci diff_tr_comp_late_drop diff_tr_comp_late_drop_lci diff_tr_comp_late_drop_uci diff_early_late_drop diff_early_late_drop_lci diff_early_late_drop_uci {
	scalar variable = "`var'"
					qui summarize `var' if inrange(tt, .24, .26) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt, .45, .55) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.0001),.001)					
					qui summarize `var' if inrange(tt, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt, 2.85, 3.15) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt,  4.85, 5.15) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.0001),.001)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
		}
	
matrix est_as24 = (ests_diff_tr_comp_early_drop , ests_diff_tr_comp_early_drop_lci , ests_diff_tr_comp_early_drop_uci ,  ///
	ests_diff_tr_comp_late_drop , ests_diff_tr_comp_late_drop_lci, ests_diff_tr_comp_late_drop_uci , ///
	ests_diff_early_late_drop , ests_diff_early_late_drop_lci , ests_diff_early_late_drop_uci )
matrix colnames est_as24 = Comp_Early Comp_Early_lci Comp_Early_uci Comp_Late Comp_Late_lci Comp_Late_uci Early_Late Early_Late_lci Early_Late_uci 

esttab matrix(est_as24) using "${pathdata2}rmst_prison_m0_main_diff.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}rmst_prison_m0_main_diff.html" >>



#### Imprisonment, Listwise, IPW

**Difference Survival**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23_2.dta", clear

*estread using "mariel_feb_23_2.sters", replace

keep sdiff_comp_vs_late sdiff_comp_vs_late_lci sdiff_comp_vs_late_uci sdiff_comp_vs_early sdiff_comp_vs_early_lci sdiff_comp_vs_early_uci sdiff_late_vs_early sdiff_late_vs_early_lci sdiff_late_vs_early_uci tt2

foreach var of varlist sdiff_comp_vs_late sdiff_comp_vs_late_lci sdiff_comp_vs_late_uci sdiff_comp_vs_early sdiff_comp_vs_early_lci sdiff_comp_vs_early_uci sdiff_late_vs_early sdiff_late_vs_early_lci sdiff_late_vs_early_uci {
    local newname = substr("`var'", 2, 50)
    rename `var' `newname'
}

foreach var of varlist diff_comp_vs_late diff_comp_vs_late_lci diff_comp_vs_late_uci diff_comp_vs_early diff_comp_vs_early_lci diff_comp_vs_early_uci diff_late_vs_early diff_late_vs_early_lci diff_late_vs_early_uci {
	scalar variable = "`var'"
					qui summarize `var' if inrange(tt2, .10, .30) // tolerance of .06
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt2, .35, .65) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.001)*100,.1)					
					qui summarize `var' if inrange(tt2, .75, 1.25) // tolerance of .16
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt2, 2.80, 3.20) // tolerance of .40
					scalar e3y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt2,  4.50, 5.50) // tolerance of .40
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
		}
	
matrix est_as25 = (ests_diff_comp_vs_late , ests_diff_comp_vs_late_lci , ests_diff_comp_vs_late_uci ,  ///
	ests_diff_comp_vs_early , ests_diff_comp_vs_early_lci, ests_diff_comp_vs_early_uci , ///
	ests_diff_late_vs_early , ests_diff_late_vs_early_lci , ests_diff_late_vs_early_uci )
matrix colnames est_as25 = Comp_Late Comp_Late_lci Comp_Late_uci Comp_Early Comp_Early_lci Comp_Early_uci Early_Late Early_Late_lci Early_Late_uci 

*cap qui noi use "mariel_feb_23_2_early.dta", clear
*qui ds
*di word("`r(varlist)'", `c(k)')
*browse 
*NO EXTRAPOLA, REEMPLAZAR CON CERO EN UNA DE ESAS. NO EXTRAPOLA PORQUE LA PROBABILIDAD ES 1.

*cap qui noi use "mariel_feb_23_2_early_late.dta", clear
*browse
*NO HAY 6 MESES EN EARLY-LATE, SON SÓLO .78, que termina siendo absorvido por el año

esttab matrix(est_as25) using "${pathdata2}prob_prison_m0_IPW_diff.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}prob_prison_m0_IPW_diff.html" >>


**Difference RMST**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23_2.dta", clear
*estread using "mariel_feb_23_2.sters", replace

keep rmstdiff_comp_vs_late rmstdiff_comp_vs_late_lci rmstdiff_comp_vs_late_uci rmstdiff_comp_vs_early rmstdiff_comp_vs_early_lci rmstdiff_comp_vs_early_uci rmstdiff_late_vs_early rmstdiff_late_vs_early_lci rmstdiff_late_vs_early_uci tt2

foreach var of varlist rmstdiff_comp_vs_late rmstdiff_comp_vs_late_lci rmstdiff_comp_vs_late_uci rmstdiff_comp_vs_early rmstdiff_comp_vs_early_lci rmstdiff_comp_vs_early_uci rmstdiff_late_vs_early rmstdiff_late_vs_early_lci rmstdiff_late_vs_early_uci {
    local newname = substr("`var'", 5, 50)
    rename `var' `newname'
}

foreach var of varlist diff_comp_vs_late diff_comp_vs_late_lci diff_comp_vs_late_uci diff_comp_vs_early diff_comp_vs_early_lci diff_comp_vs_early_uci diff_late_vs_early diff_late_vs_early_lci diff_late_vs_early_uci {
	scalar variable = "`var'"
					qui summarize `var' if inrange(tt2, .10, .30) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt2, .35, .65) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.0001),.001)					
					qui summarize `var' if inrange(tt2, .75, 1.25) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt2, 2.80, 3.20) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt2,  4.50, 5.50) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.0001),.001)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
		}
	
matrix est_as26 = (ests_diff_comp_vs_early , ests_diff_comp_vs_early_lci, ests_diff_comp_vs_early_uci ,  ///
	ests_diff_comp_vs_late , ests_diff_comp_vs_late_lci , ests_diff_comp_vs_late_uci , ///
	ests_diff_late_vs_early , ests_diff_late_vs_early_lci , ests_diff_late_vs_early_uci )
matrix colnames est_as26 = Comp_Early Comp_Early_lci Comp_Early_uci Comp_Late Comp_Late_lci Comp_Late_uci Early_Late Early_Late_lci Early_Late_uci 

esttab matrix(est_as26) using "${pathdata2}rmst_prison_m0_main_IPW_diff.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}rmst_prison_m0_main_IPW_diff.html" >>


#### #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#### #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#### Imprisonment_Imputed_Main
#### #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

**Survival**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23_2_m1.dta", clear

keep s_tr_comp s_tr_comp_lci s_tr_comp_uci s_early_drop s_early_drop_lci s_early_drop_uci s_late_drop s_late_drop_lci s_late_drop_uci tt

*sdiff_tr_comp_early_drop sdiff_tr_comp_late_drop sdiff_early_late_drop
foreach var of varlist s_tr_comp s_tr_comp_lci s_tr_comp_uci s_early_drop s_early_drop_lci s_early_drop_uci s_late_drop s_late_drop_lci s_late_drop_uci {
			scalar variable = "`var'"
					qui summarize `var' if inrange(tt, .24, .26) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt, .45, .55) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.001)*100,.1)					
					qui summarize `var' if inrange(tt, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt, 2.85, 3.15) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt,  4.85, 5.15) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
	}	

matrix est_as21m1 = (ests_s_tr_comp , ests_s_tr_comp_lci, ests_s_tr_comp_uci ,  ///
	ests_s_early_drop , ests_s_early_drop_lci, ests_s_early_drop_uci , ///
	ests_s_late_drop , ests_s_late_drop_lci , ests_s_late_drop_uci )
matrix colnames est_as21m1 = Comp Comp_lci Comp_uci Early Early_lci Early_uci Late Late_lci Late_uci 

esttab matrix(est_as21m1) using "${pathdata2}prob_prison_m1_main.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}prob_prison_m1_main.html" >>


**RMST**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23_2_m1.dta", clear

keep rmst_h0 rmst_h0_lci rmst_h0_uci rmst_h1 rmst_h1_lci rmst_h1_uci rmst_h2 rmst_h2_lci rmst_h2_uci tt


*sdiff_tr_comp_early_drop sdiff_tr_comp_late_drop sdiff_early_late_drop
foreach var of varlist rmst_h0 rmst_h0_lci rmst_h0_uci rmst_h1 rmst_h1_lci rmst_h1_uci rmst_h2 rmst_h2_lci rmst_h2_uci {
			scalar variable = "`var'"
					qui summarize `var' if inrange(tt, .24, .26) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt, .45, .55) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.0001),.001)					
					qui summarize `var' if inrange(tt, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt, 2.85, 3.15) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt,  4.85, 5.15) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.0001),.001)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
	}	

matrix est_as22m1 = (ests_rmst_h0 , ests_rmst_h0_lci , ests_rmst_h0_uci ,  ///
	ests_rmst_h1 , ests_rmst_h1_lci , ests_rmst_h1_uci ,  ///
	ests_rmst_h2 , ests_rmst_h2_lci , ests_rmst_h2_uci )
matrix colnames est_as22m1 = Comp Comp_lci Comp_uci Early Early_lci Early_uci Late Late_lci Late_uci 

esttab matrix(est_as22m1) using "${pathdata2}rmst_prison_m1_main.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}rmst_prison_m1_main.html" >>


**Difference Survival**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23_2_m1.dta", clear

keep sdiff_tr_comp_early_drop sdiff_tr_comp_early_drop_lci sdiff_tr_comp_early_drop_uci  sdiff_tr_comp_late_drop sdiff_tr_comp_late_drop_lci sdiff_tr_comp_late_drop_uci sdiff_early_late_drop sdiff_early_late_drop_lci sdiff_early_late_drop_uci tt

foreach var of varlist sdiff_tr_comp_early_drop sdiff_tr_comp_early_drop_lci sdiff_tr_comp_early_drop_uci  sdiff_tr_comp_late_drop sdiff_tr_comp_late_drop_lci sdiff_tr_comp_late_drop_uci sdiff_early_late_drop sdiff_early_late_drop_lci sdiff_early_late_drop_uci {
    local newname = substr("`var'", 2, 50)
    rename `var' `newname'
}

foreach var of varlist diff_tr_comp_early_drop diff_tr_comp_early_drop_lci diff_tr_comp_early_drop_uci  diff_tr_comp_late_drop diff_tr_comp_late_drop_lci diff_tr_comp_late_drop_uci diff_early_late_drop diff_early_late_drop_lci diff_early_late_drop_uci {
	scalar variable = "`var'"
					qui summarize `var' if inrange(tt, .24, .26) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt, .45, .55) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.001)*100,.1)					
					qui summarize `var' if inrange(tt, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt, 2.85, 3.15) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt,  4.85, 5.15) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
		}
	
matrix est_as23m1 = (ests_diff_tr_comp_early_drop , ests_diff_tr_comp_early_drop_lci , ests_diff_tr_comp_early_drop_uci ,  ///
	ests_diff_tr_comp_late_drop , ests_diff_tr_comp_late_drop_lci, ests_diff_tr_comp_late_drop_uci , ///
	ests_diff_early_late_drop , ests_diff_early_late_drop_lci , ests_diff_early_late_drop_uci )
matrix colnames est_as23m1 = Comp_Early Comp_Early_lci Comp_Early_uci Comp_Late Comp_Late_lci Comp_Late_uci Early_Late Early_Late_lci Early_Late_uci 

esttab matrix(est_as23m1) using "${pathdata2}prob_prison_m1_main_diff.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}prob_prison_m1_main_diff.html" >>


**Difference RMST**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23_2_m1.dta", clear

keep rmstdiff_tr_comp_early_drop rmstdiff_tr_comp_early_drop_lci rmstdiff_tr_comp_early_drop_uci rmstdiff_tr_comp_late_drop rmstdiff_tr_comp_late_drop_lci rmstdiff_tr_comp_late_drop_uci rmstdiff_early_late_drop rmstdiff_early_late_drop_lci rmstdiff_early_late_drop_uci tt

foreach var of varlist rmstdiff_tr_comp_early_drop rmstdiff_tr_comp_early_drop_lci rmstdiff_tr_comp_early_drop_uci rmstdiff_tr_comp_late_drop rmstdiff_tr_comp_late_drop_lci rmstdiff_tr_comp_late_drop_uci rmstdiff_early_late_drop rmstdiff_early_late_drop_lci rmstdiff_early_late_drop_uci {
    local newname = substr("`var'", 5, 50)
    rename `var' `newname'
}

foreach var of varlist diff_tr_comp_early_drop diff_tr_comp_early_drop_lci diff_tr_comp_early_drop_uci diff_tr_comp_late_drop diff_tr_comp_late_drop_lci diff_tr_comp_late_drop_uci diff_early_late_drop diff_early_late_drop_lci diff_early_late_drop_uci {
	scalar variable = "`var'"
					qui summarize `var' if inrange(tt, .24, .26) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt, .45, .55) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.0001),.001)					
					qui summarize `var' if inrange(tt, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt, 2.85, 3.15) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt,  4.85, 5.15) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.0001),.001)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
		}
	
matrix est_as24m1 = (ests_diff_tr_comp_early_drop , ests_diff_tr_comp_early_drop_lci , ests_diff_tr_comp_early_drop_uci ,  ///
	ests_diff_tr_comp_late_drop , ests_diff_tr_comp_late_drop_lci, ests_diff_tr_comp_late_drop_uci , ///
	ests_diff_early_late_drop , ests_diff_early_late_drop_lci , ests_diff_early_late_drop_uci )
matrix colnames est_as24m1 = Comp_Early Comp_Early_lci Comp_Early_uci Comp_Late Comp_Late_lci Comp_Late_uci Early_Late Early_Late_lci Early_Late_uci 

esttab matrix(est_as24m1) using "${pathdata2}rmst_prison_m1_main_diff.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}rmst_prison_m1_main_diff.html" >>



#### Imprisonment, Imputed, IPW

**Difference Survival**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23_2_m1.dta", clear

*estread using "mariel_feb_23_2.sters", replace

keep sdiff_comp_vs_late sdiff_comp_vs_late_lci sdiff_comp_vs_late_uci sdiff_comp_vs_early sdiff_comp_vs_early_lci sdiff_comp_vs_early_uci sdiff_late_vs_early sdiff_late_vs_early_lci sdiff_late_vs_early_uci tt2

foreach var of varlist sdiff_comp_vs_late sdiff_comp_vs_late_lci sdiff_comp_vs_late_uci sdiff_comp_vs_early sdiff_comp_vs_early_lci sdiff_comp_vs_early_uci sdiff_late_vs_early sdiff_late_vs_early_lci sdiff_late_vs_early_uci {
    local newname = substr("`var'", 2, 50)
    rename `var' `newname'
}

foreach var of varlist diff_comp_vs_late diff_comp_vs_late_lci diff_comp_vs_late_uci diff_comp_vs_early diff_comp_vs_early_lci diff_comp_vs_early_uci diff_late_vs_early diff_late_vs_early_lci diff_late_vs_early_uci {
	scalar variable = "`var'"
					qui summarize `var' if inrange(tt2, .10, .30) // tolerance of .06
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt2, .35, .65) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.001)*100,.1)					
					qui summarize `var' if inrange(tt2, .75, 1.25) // tolerance of .16
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt2, 2.80, 3.20) // tolerance of .40
					scalar e3y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt2,  4.50, 5.50) // tolerance of .40
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
		}
	
matrix est_as25m1 = (ests_diff_comp_vs_late , ests_diff_comp_vs_late_lci , ests_diff_comp_vs_late_uci ,  ///
	ests_diff_comp_vs_early , ests_diff_comp_vs_early_lci, ests_diff_comp_vs_early_uci , ///
	ests_diff_late_vs_early , ests_diff_late_vs_early_lci , ests_diff_late_vs_early_uci )
matrix colnames est_as25m1 = Comp_Late Comp_Late_lci Comp_Late_uci Comp_Early Comp_Early_lci Comp_Early_uci Early_Late Early_Late_lci Early_Late_uci 

*cap qui noi use "mariel_feb_23_2_early.dta", clear
*qui ds
*di word("`r(varlist)'", `c(k)')
*browse 
*NO EXTRAPOLA, REEMPLAZAR CON CERO EN UNA DE ESAS. NO EXTRAPOLA PORQUE LA PROBABILIDAD ES 1.

*cap qui noi use "mariel_feb_23_2_early_late.dta", clear
*browse
*NO HAY 6 MESES EN EARLY-LATE, SON SÓLO .78, que termina siendo absorvido por el año

esttab matrix(est_as25m1) using "${pathdata2}prob_prison_m1_IPW_diff.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}prob_prison_m1_IPW_diff.html" >>


**Difference RMST**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23_2_m1.dta", clear
*estread using "mariel_feb_23_2.sters", replace

keep rmstdiff_comp_vs_late rmstdiff_comp_vs_late_lci rmstdiff_comp_vs_late_uci rmstdiff_comp_vs_early rmstdiff_comp_vs_early_lci rmstdiff_comp_vs_early_uci rmstdiff_late_vs_early rmstdiff_late_vs_early_lci rmstdiff_late_vs_early_uci tt2

foreach var of varlist rmstdiff_comp_vs_late rmstdiff_comp_vs_late_lci rmstdiff_comp_vs_late_uci rmstdiff_comp_vs_early rmstdiff_comp_vs_early_lci rmstdiff_comp_vs_early_uci rmstdiff_late_vs_early rmstdiff_late_vs_early_lci rmstdiff_late_vs_early_uci {
    local newname = substr("`var'", 5, 50)
    rename `var' `newname'
}

foreach var of varlist diff_comp_vs_late diff_comp_vs_late_lci diff_comp_vs_late_uci diff_comp_vs_early diff_comp_vs_early_lci diff_comp_vs_early_uci diff_late_vs_early diff_late_vs_early_lci diff_late_vs_early_uci {
	scalar variable = "`var'"
					qui summarize `var' if inrange(tt2, .10, .30) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt2, .35, .65) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.0001),.001)					
					qui summarize `var' if inrange(tt2, .75, 1.25) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt2, 2.80, 3.20) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt2,  4.50, 5.50) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.0001),.001)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
		}
	
matrix est_as26m1 = (ests_diff_comp_vs_early , ests_diff_comp_vs_early_lci, ests_diff_comp_vs_early_uci ,  ///
	ests_diff_comp_vs_late , ests_diff_comp_vs_late_lci , ests_diff_comp_vs_late_uci , ///
	ests_diff_late_vs_early , ests_diff_late_vs_early_lci , ests_diff_late_vs_early_uci )
matrix colnames est_as26m1 = Comp_Early Comp_Early_lci Comp_Early_uci Comp_Late Comp_Late_lci Comp_Late_uci Early_Late Early_Late_lci Early_Late_uci 

esttab matrix(est_as26m1) using "${pathdata2}rmst_prison_m1_main_IPW_diff.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}rmst_prison_m1_main_IPW_diff.html" >>

#### #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#### #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#### Imprisonment_Imputed(2)_Main
#### #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

**Survival**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23_2_m2.dta", clear

keep s_tr_comp s_tr_comp_lci s_tr_comp_uci s_early_drop s_early_drop_lci s_early_drop_uci s_late_drop s_late_drop_lci s_late_drop_uci tt

*sdiff_tr_comp_early_drop sdiff_tr_comp_late_drop sdiff_early_late_drop
foreach var of varlist s_tr_comp s_tr_comp_lci s_tr_comp_uci s_early_drop s_early_drop_lci s_early_drop_uci s_late_drop s_late_drop_lci s_late_drop_uci {
			scalar variable = "`var'"
					qui summarize `var' if inrange(tt, .24, .26) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt, .45, .55) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.001)*100,.1)					
					qui summarize `var' if inrange(tt, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt, 2.85, 3.15) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt,  4.85, 5.15) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
	}	

matrix est_as21m2 = (ests_s_tr_comp , ests_s_tr_comp_lci, ests_s_tr_comp_uci ,  ///
	ests_s_early_drop , ests_s_early_drop_lci, ests_s_early_drop_uci , ///
	ests_s_late_drop , ests_s_late_drop_lci , ests_s_late_drop_uci )
matrix colnames est_as21m2 = Comp Comp_lci Comp_uci Early Early_lci Early_uci Late Late_lci Late_uci 

esttab matrix(est_as21m2) using "${pathdata2}prob_prison_m2_main.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}prob_prison_m2_main.html" >>


**RMST**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23_2_m2.dta", clear

keep rmst_h0 rmst_h0_lci rmst_h0_uci rmst_h1 rmst_h1_lci rmst_h1_uci rmst_h2 rmst_h2_lci rmst_h2_uci tt


*sdiff_tr_comp_early_drop sdiff_tr_comp_late_drop sdiff_early_late_drop
foreach var of varlist rmst_h0 rmst_h0_lci rmst_h0_uci rmst_h1 rmst_h1_lci rmst_h1_uci rmst_h2 rmst_h2_lci rmst_h2_uci {
			scalar variable = "`var'"
					qui summarize `var' if inrange(tt, .24, .26) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt, .45, .55) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.0001),.001)					
					qui summarize `var' if inrange(tt, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt, 2.85, 3.15) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt,  4.85, 5.15) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.0001),.001)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
	}	

matrix est_as22m2 = (ests_rmst_h0 , ests_rmst_h0_lci , ests_rmst_h0_uci ,  ///
	ests_rmst_h1 , ests_rmst_h1_lci , ests_rmst_h1_uci ,  ///
	ests_rmst_h2 , ests_rmst_h2_lci , ests_rmst_h2_uci )
matrix colnames est_as22m2 = Comp Comp_lci Comp_uci Early Early_lci Early_uci Late Late_lci Late_uci 

esttab matrix(est_as22m2) using "${pathdata2}rmst_prison_m2_main.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}rmst_prison_m2_main.html" >>


**Difference Survival**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23_2_m2.dta", clear

keep sdiff_tr_comp_early_drop sdiff_tr_comp_early_drop_lci sdiff_tr_comp_early_drop_uci  sdiff_tr_comp_late_drop sdiff_tr_comp_late_drop_lci sdiff_tr_comp_late_drop_uci sdiff_early_late_drop sdiff_early_late_drop_lci sdiff_early_late_drop_uci tt

foreach var of varlist sdiff_tr_comp_early_drop sdiff_tr_comp_early_drop_lci sdiff_tr_comp_early_drop_uci  sdiff_tr_comp_late_drop sdiff_tr_comp_late_drop_lci sdiff_tr_comp_late_drop_uci sdiff_early_late_drop sdiff_early_late_drop_lci sdiff_early_late_drop_uci {
    local newname = substr("`var'", 2, 50)
    rename `var' `newname'
}

foreach var of varlist diff_tr_comp_early_drop diff_tr_comp_early_drop_lci diff_tr_comp_early_drop_uci  diff_tr_comp_late_drop diff_tr_comp_late_drop_lci diff_tr_comp_late_drop_uci diff_early_late_drop diff_early_late_drop_lci diff_early_late_drop_uci {
	scalar variable = "`var'"
					qui summarize `var' if inrange(tt, .24, .26) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt, .45, .55) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.001)*100,.1)					
					qui summarize `var' if inrange(tt, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt, 2.85, 3.15) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(tt,  4.85, 5.15) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
		}
	
matrix est_as23m2 = (ests_diff_tr_comp_early_drop , ests_diff_tr_comp_early_drop_lci , ests_diff_tr_comp_early_drop_uci ,  ///
	ests_diff_tr_comp_late_drop , ests_diff_tr_comp_late_drop_lci, ests_diff_tr_comp_late_drop_uci , ///
	ests_diff_early_late_drop , ests_diff_early_late_drop_lci , ests_diff_early_late_drop_uci )
matrix colnames est_as23m2 = Comp_Early Comp_Early_lci Comp_Early_uci Comp_Late Comp_Late_lci Comp_Late_uci Early_Late Early_Late_lci Early_Late_uci 

esttab matrix(est_as23m2) using "${pathdata2}prob_prison_m2_main_diff.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}prob_prison_m2_main_diff.html" >>


**Difference RMST**

~~~~
<<dd_do>>
cap qui noi use "mariel_feb_23_2_m2.dta", clear

keep rmstdiff_tr_comp_early_drop rmstdiff_tr_comp_early_drop_lci rmstdiff_tr_comp_early_drop_uci rmstdiff_tr_comp_late_drop rmstdiff_tr_comp_late_drop_lci rmstdiff_tr_comp_late_drop_uci rmstdiff_early_late_drop rmstdiff_early_late_drop_lci rmstdiff_early_late_drop_uci tt

foreach var of varlist rmstdiff_tr_comp_early_drop rmstdiff_tr_comp_early_drop_lci rmstdiff_tr_comp_early_drop_uci rmstdiff_tr_comp_late_drop rmstdiff_tr_comp_late_drop_lci rmstdiff_tr_comp_late_drop_uci rmstdiff_early_late_drop rmstdiff_early_late_drop_lci rmstdiff_early_late_drop_uci {
    local newname = substr("`var'", 5, 50)
    rename `var' `newname'
}

foreach var of varlist diff_tr_comp_early_drop diff_tr_comp_early_drop_lci diff_tr_comp_early_drop_uci diff_tr_comp_late_drop diff_tr_comp_late_drop_lci diff_tr_comp_late_drop_uci diff_early_late_drop diff_early_late_drop_lci diff_early_late_drop_uci {
	scalar variable = "`var'"
					qui summarize `var' if inrange(tt, .24, .26) // tolerance of .02
					scalar e3m_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt, .45, .55) // tolerance of .10
					scalar e6m_`var' = round(round(r(mean),.0001),.001)					
					qui summarize `var' if inrange(tt, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt, 2.85, 3.15) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(tt,  4.85, 5.15) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.0001),.001)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e6m_`var')'\ `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 6_mths 1_yr 3_yrs 5_yrs
		}
	
matrix est_as24m2 = (ests_diff_tr_comp_early_drop , ests_diff_tr_comp_early_drop_lci , ests_diff_tr_comp_early_drop_uci ,  ///
	ests_diff_tr_comp_late_drop , ests_diff_tr_comp_late_drop_lci, ests_diff_tr_comp_late_drop_uci , ///
	ests_diff_early_late_drop , ests_diff_early_late_drop_lci , ests_diff_early_late_drop_uci )
matrix colnames est_as24m2 = Comp_Early Comp_Early_lci Comp_Early_uci Comp_Late Comp_Late_lci Comp_Late_uci Early_Late Early_Late_lci Early_Late_uci 

esttab matrix(est_as24m2) using "${pathdata2}rmst_prison_m2_main_diff.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}rmst_prison_m2_main_diff.html" >>




## #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
## #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

# E-values, main analyses

#### #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#### #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_


### Condemnatory Sentence, Listwise


~~~~
<<dd_do>>
cap qui noi clear all
qui estread using "mariel_feb_23.sters"

estimates replay m_nostag_rp6_tvc_1, eform
scalar HR_early = r(table)["b",1]
scalar HR_early_lo = r(table)["ll",1]
scalar HR_early_up = r(table)["ul",1]
scalar HR_late = r(table)["b",2]
scalar HR_late_lo = r(table)["ll",2]
scalar HR_late_up = r(table)["ul",2]

set scheme s1mono

di  "`=scalar(round(HR_early,.01))'  (95%CI `=scalar(round(HR_early_lo),.01)', `=round(scalar(HR_early_up),.01)')"

evalue hr `=scalar(HR_early)' , lcl(`=scalar(HR_early_lo)') ucl(`=scalar(HR_early_up)') true(1) common figure
<</dd_do>>
~~~~

<<dd_graph: saving("eval_early.svg") width(800) replace>>

~~~~
<<dd_do>>
di  "`=scalar(round(HR_late,.01))'  (95%CI `=scalar(round(HR_late_lo),.01)', `=round(scalar(HR_late_up),.01)')"

evalue hr `=scalar(HR_late)' , lcl(`=scalar(HR_late_lo)') ucl(`=scalar(HR_late_up)') true(1) common figure
<</dd_do>>
~~~~

<<dd_graph: saving("eval_late.svg") width(800) replace>>


### Imprisonment, Listwise


~~~~
<<dd_do>>
cap qui noi clear all
qui estread using "mariel_feb_23_2.sters"

estimates replay m_nostag_rp6_tvc_1, eform
scalar HR_early2 = r(table)["b",1]
scalar HR_early_lo2 = r(table)["ll",1]
scalar HR_early_up2 = r(table)["ul",1]
scalar HR_late2 = r(table)["b",2]
scalar HR_late_lo2 = r(table)["ll",2]
scalar HR_late_up2 = r(table)["ul",2]

set scheme s1mono

di  "`=scalar(round(HR_early2,.01))'  (95%CI `=scalar(round(HR_early_lo2),.01)', `=round(scalar(HR_early_up2),.01)')"

evalue hr `=scalar(HR_early2)' , lcl(`=scalar(HR_early_lo2)') ucl(`=scalar(HR_early_up2)') true(1) figure
<</dd_do>>
~~~~

<<dd_graph: saving("eval_early_pris.svg") width(800) replace>>


~~~~
<<dd_do>>
di  "`=scalar(round(HR_late2,.01))'  (95%CI `=scalar(round(HR_late_lo2),.01)', `=round(scalar(HR_late_up2),.01)')"

evalue hr `=scalar(HR_late2)' , lcl(`=scalar(HR_late_lo2)') ucl(`=scalar(HR_late_up2)') true(1) figure
<</dd_do>>
~~~~

<<dd_graph: saving("eval_late_pris.svg") width(800) replace>>



### Condemnatory Sentence, Imputed


~~~~
<<dd_do>>
cap qui noi clear all
qui estread using "mariel_feb_23_m1.sters"

estimates replay m_nostag_rp8_tvc_1, eform
scalar HR_earlym1 = r(table)["b",1]
scalar HR_early_lom1 = r(table)["ll",1]
scalar HR_early_upm1 = r(table)["ul",1]
scalar HR_latem1 = r(table)["b",2]
scalar HR_late_lom1 = r(table)["ll",2]
scalar HR_late_upm1 = r(table)["ul",2]

set scheme s1mono

di  "`=scalar(round(HR_earlym1,.01))'  (95%CI `=scalar(round(HR_early_lom1),.01)', `=round(scalar(HR_early_upm1),.01)')"

evalue hr `=scalar(HR_earlym1)' , lcl(`=scalar(HR_early_lom1)') ucl(`=scalar(HR_early_upm1)') true(1) common figure

<</dd_do>>
~~~~

<<dd_graph: saving("eval_early_m1.svg") width(800) replace>>

~~~~
<<dd_do>>
di  "`=scalar(round(HR_latem1,.01))'  (95%CI `=scalar(round(HR_late_lom1),.01)', `=round(scalar(HR_late_upm1),.01)')"

evalue hr `=scalar(HR_latem1)' , lcl(`=scalar(HR_late_lom1)') ucl(`=scalar(HR_late_upm1)') true(1) common figure
<</dd_do>>
~~~~

<<dd_graph: saving("eval_late_m1.svg") width(800) replace>>


### Imprisonment, Imputed


~~~~
<<dd_do>>
cap qui noi clear all
qui estread using "mariel_feb_23_2_m1.sters"

estimates replay m_nostag_rp6_tvc_1, eform
scalar HR_early2m1 = r(table)["b",1]
scalar HR_early_lo2m1 = r(table)["ll",1]
scalar HR_early_up2m1 = r(table)["ul",1]
scalar HR_late2m1 = r(table)["b",2]
scalar HR_late_lo2m1 = r(table)["ll",2]
scalar HR_late_up2m1 = r(table)["ul",2]

set scheme s1mono

di  "`=scalar(round(HR_early2m1,.01))'  (95%CI `=scalar(round(HR_early_lo2m1),.01)', `=round(scalar(HR_early_up2m1),.01)')"

evalue hr `=scalar(HR_early2m1)' , lcl(`=scalar(HR_early_lo2m1)') ucl(`=scalar(HR_early_up2m1)') true(1) figure

<</dd_do>>
~~~~

<<dd_graph: saving("eval_early_pris_m1.svg") width(800) replace>>


~~~~
<<dd_do>>
di  "`=scalar(round(HR_late2m1,.01))'  (95%CI `=scalar(round(HR_late_lo2m1),.01)', `=round(scalar(HR_late_up2m1),.01)')"

evalue hr `=scalar(HR_late2m1)' , lcl(`=scalar(HR_late_lo2m1)') ucl(`=scalar(HR_late_up2m1)') true(1) figure
<</dd_do>>
~~~~



<<dd_graph: saving("eval_late_pris_m1.svg") width(800) replace>>


*#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
*#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
*#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:

# Figure 

~~~~
<<dd_do>>
*C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)
graph use "_figs\h_m_ns_rp6_stdif_s2_m1.gph"
graph export "_figs/h_m_ns_rp6_stdif_s2_m1.pdf", as(pdf) name("h_m_ns_rp6_stdif_s2_m1") replace

graph use "_figs\h_m_ns_rp6_stdif_rmst_m1.gph" 
graph export "_figs/h_m_ns_rp6_stdif_rmst_m1.pdf", as(pdf) name("h_m_ns_rp6_stdif_rmst_m1") replace

graph use "_figs\h_m_ns_rp6_stdif_s2_pris_m1.gph" 
graph export "_figs/h_m_ns_rp6_stdif_s2_pris_m1.pdf", as(pdf) name("h_m_ns_rp6_stdif_s2_pris_m1") replace

graph use "_figs\h_m_ns_rp6_stdif_rmst_pris_m1.gph" 
graph export "_figs/h_m_ns_rp6_stdif_rmst_pris_m1.pdf", as(pdf) name("h_m_ns_rp6_stdif_rmst_pris_m1") replace

<</dd_do>>
~~~~

**HACER GRAFICO DE PROBABILIDADES Y RMSTS EN EL TIEMPO**

~~~~
<<dd_do>>
graph use "_figs\h_m_ns_rp6_s_m1.gph"
gr_edit .yaxis1.title.text = {}
gr_edit .yaxis1.title.text.Arrpush `"Probibability of avoiding condemnatory sentence"'
gr_edit .legend.plotregion1.key[1].view.style.editstyle line(color(%60)) editcopy
gr_edit .legend.plotregion1.key[2].view.style.editstyle line(color(%60)) editcopy
gr_edit .legend.plotregion1.key[3].view.style.editstyle line(color(%60)) editcopy
graph export "_figs/h_m_ns_rp6_s_m1.pdf", as(pdf) name("h_m_ns_rp6_s_m1") replace


graph use "_figs\h_m_ns_rp6_s_pris_m1.gph"
gr_edit .yaxis1.title.text = {}
gr_edit .yaxis1.title.text.Arrpush `"Probibability of avoiding imprisonment"'
gr_edit .legend.plotregion1.key[1].view.style.editstyle line(color(%60)) editcopy
gr_edit .legend.plotregion1.key[2].view.style.editstyle line(color(%60)) editcopy
gr_edit .legend.plotregion1.key[3].view.style.editstyle line(color(%60)) editcopy
graph export "_figs/h_m_ns_rp6_s_pris_m1.pdf", as(pdf) name("h_m_ns_rp6_s_pris_m1") replace


<</dd_do>>
~~~~

*#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
*#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
*#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:

# Hazard tables

## Condemnatory Sentence, Imputed

~~~~
<<dd_do>>
cap qui noi clear all
qui estread using "mariel_feb_23_m1.sters"
estimates replay m_nostag_rp8_tvc_1, eform
// sacar matrices
matrix table_r = r(table)

// Column and rownames
global rownms: rown r(table)
di "$rownms"
global colnms: coln r(table)
di "$colnms"
local reqs : roweq r(table) //coleq
di "`reqs'"
global ceqs : coleq r(table) //coleq
di "`ceqs'"
local cname : colfullnames r(table)
di "`cname'"

// Eliminate equations
matrix coleq table_r = ""

// Subset matrix by column names
*https://www.stata.com/manuals13/u14.pdf
*https://www.stata.com/manuals13/dfunctions.pdf#dfunctionsDescriptionMatrixfunctionsreturningamatrix
*
matrix A = table_r[1... , "mot_egr_early"], table_r[1... , "mot_egr_late"], table_r[1... , "_rcs1".." _rcs_mot_egr_late1"], table_r[1... , "_d_rcs1".." _d_rcs_mot_egr_late1"]
matrix mod1= A["b","mot_egr_early".."_d_rcs_mot_egr_late1"] \ A["ll","mot_egr_early".."_d_rcs_mot_egr_late1"] \ A["ul","mot_egr_early".."_d_rcs_mot_egr_early1"...] \ A["pvalue","mot_egr_early".."_d_rcs_mot_egr_early1"...]  // three dots, until the last

//make another matrix
mat mod1b= mod1

//mata: mata drop st_trans_matrix()
mata:
void st_transpose_matrix(string scalar matname)
{
    // Convert Stata matrix to Mata matrix
    M = st_matrix(matname)

    // Transpose the matrix
    transposed_M = M'

    // Convert Mata matrix to Stata matrix
    st_matrix(matname, transposed_M)
}
end

//transpose function
mata: st_transpose_matrix("mod1b")

//move colnames and rownames to transpose
local cnames :  rownames mod1
di " `cnames'"
mat colnames mod1b = `cnames'
local rnames :  colnames mod1 
mat rownames mod1b = `rnames' 

//export
esttab matrix(mod1b) using "mat_tab1.html", replace

*interpreting this value directly can be misleading, because the "_rcs" term represents a change in the hazard ratio over time, and the exact shape of this change is determined by the restricted cubic spline function used in the model.
*the most effective way to interpret the "_rcs" term is to visualize the hazard ratio over time.¨
*hazard ratio starts at 1.74308 (the main effect) at time zero and then changes over time according to the .907554 "_rcs" term. The trajectory of the hazard ratio will likely be decreasing over time, 
*direct interpretation of the "_rcs" term is not as straightforward. 
*The most effective way to understand the combined effect of the main effect and the "_rcs" term is to visualize the hazard ratio over time. 
<</dd_do>>
~~~~

<<dd_include: "mat_tab1.html" >>


## Imprisonment, Imputed

~~~~
<<dd_do>>
cap qui noi clear all
qui estread using "mariel_feb_23_2_m1.sters"
estimates replay m_nostag_rp6_tvc_1, eform
// sacar matrices
matrix table_r2 = r(table)

// Column and rownames
global rownms2: rown r(table)
di "$rownms2"
global colnms2: coln r(table)
di "$colnms2"
local reqs2 : roweq r(table) //coleq
di "`reqs2'"
global ceqs2 : coleq r(table) //coleq
di "`ceqs2'"
local cname2 : colfullnames r(table)
di "`cname2'"

// Eliminate equations
matrix coleq table_r2 = ""

// Subset matrix by column names
*https://www.stata.com/manuals13/u14.pdf
*https://www.stata.com/manuals13/dfunctions.pdf#dfunctionsDescriptionMatrixfunctionsreturningamatrix
*
matrix A2 = table_r2[1... , "mot_egr_early"], table_r2[1... , "mot_egr_late"], table_r2[1... , "_rcs1".." _rcs_mot_egr_late1"], table_r2[1... , "_d_rcs1".." _d_rcs_mot_egr_late1"]
matrix mod2= A2["b","mot_egr_early".."_d_rcs_mot_egr_late1"] \ A2["ll","mot_egr_early".."_d_rcs_mot_egr_late1"] \ A2["ul","mot_egr_early".."_d_rcs_mot_egr_early1"...] \ A2["pvalue","mot_egr_early".."_d_rcs_mot_egr_early1"...]  // three dots, until the last

//make another matrix
mat mod2b= mod2

//mata: mata drop st_trans_matrix()
mata:
void st_transpose_matrix(string scalar matname)
{
    // Convert Stata matrix to Mata matrix
    M = st_matrix(matname)

    // Transpose the matrix
    transposed_M = M'

    // Convert Mata matrix to Stata matrix
    st_matrix(matname, transposed_M)
}
end

//transpose function
mata: st_transpose_matrix("mod2b")

//move colnames and rownames to transpose
local cnames2 :  rownames mod2
di " `cnames2'"
mat colnames mod2b = `cnames2'
local rnames2 :  colnames mod2 
mat rownames mod2b = `rnames2' 

//export
esttab matrix(mod2b) using "mat_tab2.html", replace

<</dd_do>>
~~~~

<<dd_include: "mat_tab2.html" >>


## Condemnatory Sentence, Listwise

~~~~
<<dd_do>>
cap qui noi clear all
qui estread using "mariel_feb_23.sters"
estimates replay m_nostag_rp6_tvc_1, eform
// sacar matrices
matrix table_r3 = r(table)

// Column and rownames
global rownms3: rown r(table)
di "$rownms3"
global colnms3: coln r(table)
di "$colnms3"
local reqs3 : roweq r(table) //coleq
di "`reqs3'"
global ceqs3 : coleq r(table) //coleq
di "`ceqs3'"
local cname3 : colfullnames r(table)
di "`cname3'"

// Eliminate equations
matrix coleq table_r3 = ""

local cname3 : colfullnames table_r3
di "`cname3'"

// Subset matrix by column names
*https://www.stata.com/manuals13/u14.pdf
*https://www.stata.com/manuals13/dfunctions.pdf#dfunctionsDescriptionMatrixfunctionsreturningamatrix
*
matrix A3 = table_r3[1... , "mot_egr_early"], table_r3[1... , "mot_egr_late"], table_r3[1... , "_rcs1".." _rcs_mot_egr_late1"], table_r3[1... , "_d_rcs1".." _d_rcs_mot_egr_late1"]
matrix mod3= A3["b","mot_egr_early".."_d_rcs_mot_egr_late1"] \ A3["ll","mot_egr_early".."_d_rcs_mot_egr_late1"] \ A3["ul","mot_egr_early".."_d_rcs_mot_egr_early1"...] \ A3["pvalue","mot_egr_early".."_d_rcs_mot_egr_early1"...]  // three dots, until the last

//make another matrix
mat mod3b= mod3

//mata: mata drop st_transpose_matrix()
mata:
void st_transpose_matrix(string scalar matname)
{
    // Convert Stata matrix to Mata matrix
    M = st_matrix(matname)

    // Transpose the matrix
    transposed_M = M'

    // Convert Mata matrix to Stata matrix
    st_matrix(matname, transposed_M)
}
end

//transpose function
mata: st_transpose_matrix("mod3b")

//move colnames and rownames to transpose
local cnames3 :  rownames mod3
di " `cnames3'"
mat colnames mod3b = `cnames3'
local rnames3 :  colfullnames mod3
mat rownames mod3b = `rnames3' 
di " `names3'"
//export
esttab matrix(mod3b) using "mat_tab3.html", replace
<</dd_do>>
~~~~

<<dd_include: "mat_tab3.html" >>


## Imprisonment, Listwise

~~~~
<<dd_do>>
cap qui noi clear all
qui estread using "mariel_feb_23_2.sters"
estimates replay m_nostag_rp6_tvc_1, eform
// sacar matrices
matrix table_r4 = r(table)

// Column and rownames
global rownms4: rown r(table)
di "$rownms4"
global colnms4: coln r(table)
di "$colnms4"
local reqs4 : roweq r(table) //coleq
di "`reqs4'"
global ceqs4 : coleq r(table) //coleq
di "`ceqs4'"
local cname4 : colfullnames r(table)
di "`cname4'"

// Eliminate equations
matrix coleq table_r4 = ""

// Subset matrix by column names
*https://www.stata.com/manuals14/u14.pdf
*https://www.stata.com/manuals14/dfunctions.pdf#dfunctionsDescriptionMatrixfunctionsreturningamatrix
*
matrix A4 = table_r4[1... , "mot_egr_early"], table_r4[1... , "mot_egr_late"], table_r4[1... , "_rcs1".." _rcs_mot_egr_late1"], table_r4[1... , "_d_rcs1".." _d_rcs_mot_egr_late1"]
matrix mod4= A4["b","mot_egr_early".."_d_rcs_mot_egr_late1"] \ A4["ll","mot_egr_early".."_d_rcs_mot_egr_late1"] \ A4["ul","mot_egr_early".."_d_rcs_mot_egr_early1"...] \ A4["pvalue","mot_egr_early".."_d_rcs_mot_egr_early1"...]  // three dots, until the last

//make another matrix
mat mod4b= mod4

//mata: mata drop st_trans_matrix()
mata:
void st_transpose_matrix(string scalar matname)
{
    // Convert Stata matrix to Mata matrix
    M = st_matrix(matname)

    // Transpose the matrix
    transposed_M = M'

    // Convert Mata matrix to Stata matrix
    st_matrix(matname, transposed_M)
}
end

//transpose function
mata: st_transpose_matrix("mod4b")

//move colnames and rownames to transpose
local cnames4 :  rownames mod4
di " `cnames4'"
mat colnames mod4b = `cnames4'
local rnames4 :  colnames mod4 
mat rownames mod4b = `rnames4' 

//export
esttab matrix(mod4b) using "mat_tab4.html", replace
<</dd_do>>
~~~~

<<dd_include: "mat_tab4.html" >>


*#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
*#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
*#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:

# Transform figures to Restricted Mean Time Lost


~~~~
<<dd_do>>
use mariel_feb_23_m1.dta, clear 
estread using "mariel_feb_23_2_m1.sters"
cap noi drop rmst_h00 rmst_h11 rmst_h22

gen rmtldiff_tr_comp_early_drop_lci= -rmstdiff_tr_comp_early_drop_lci
gen rmtldiff_tr_comp_early_drop_uci= -rmstdiff_tr_comp_early_drop_uci
gen rmtldiff_tr_comp_early_drop= -rmstdiff_tr_comp_early_drop
gen rmtldiff_tr_comp_late_drop_lci= -rmstdiff_tr_comp_late_drop_lci
gen rmtldiff_tr_comp_late_drop_uci= -rmstdiff_tr_comp_late_drop_uci
gen rmtldiff_tr_comp_late_drop= -rmstdiff_tr_comp_late_drop
gen rmtldiff_early_late_drop_lci= -rmstdiff_early_late_drop_lci
gen rmtldiff_early_late_drop_uci= -rmstdiff_early_late_drop_uci
gen rmtldiff_early_late_drop= -rmstdiff_early_late_drop

twoway  (rarea rmtldiff_tr_comp_early_drop_lci rmtldiff_tr_comp_early_drop_uci tt, color(gs2%35)) ///
                 (line rmtldiff_tr_comp_early_drop tt, lcolor(gs2)) ///
		(rarea rmtldiff_tr_comp_late_drop_lci rmtldiff_tr_comp_late_drop_uci tt, color(gs6%35)) ///
                 (line rmtldiff_tr_comp_late_drop tt, lcolor(gs6)) ///
		(rarea rmtldiff_early_late_drop_lci rmtldiff_early_late_drop_uci tt, color(gs10%35)) ///
                 (line rmtldiff_early_late_drop tt, lcolor(gs10)) ///				 
         				  (line zero tt, lcolor(black%20) lwidth(thick)) ///
         , ylabel(, format(%3.1f)) ///
         ytitle("Difference in RMTL (years)") ///
         xtitle("Years from baseline treatment outcome") ///
		 legend(order( 1 "Early vs. Tr. completion" 3 "Late vs. Tr. completion" 5 "Late vs. Early dropout") ring(0) pos(11) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) /// //text(.5 1 "IR = <0.001") ///
                 name(RMTLdiff, replace)
gr_edit .plotregion1.style.editstyle boxstyle(linestyle(color(none))) editcopy				 
graph save "`c(pwd)'\_figs\h_m_ns_rp6_stdif_rmtl_m1.gph", replace
graph export "`c(pwd)'\_figs\h_m_ns_rp6_stdif_rmtl_m1.pdf", as(pdf) replace 
<</dd_do>>
~~~~

<<dd_graph: saving("h_m_ns_rp6_stdif_rmtl_m1.svg") width(800) replace>>

~~~~
<<dd_do>>
use mariel_feb_23_2_m1.dta, clear 

cap noi drop rmst_h00 rmst_h11 rmst_h22

gen rmtldiff_tr_comp_early_drop_lci= -rmstdiff_tr_comp_early_drop_lci
gen rmtldiff_tr_comp_early_drop_uci= -rmstdiff_tr_comp_early_drop_uci
gen rmtldiff_tr_comp_early_drop= -rmstdiff_tr_comp_early_drop
gen rmtldiff_tr_comp_late_drop_lci= -rmstdiff_tr_comp_late_drop_lci
gen rmtldiff_tr_comp_late_drop_uci= -rmstdiff_tr_comp_late_drop_uci
gen rmtldiff_tr_comp_late_drop= -rmstdiff_tr_comp_late_drop
gen rmtldiff_early_late_drop_lci= -rmstdiff_early_late_drop_lci
gen rmtldiff_early_late_drop_uci= -rmstdiff_early_late_drop_uci
gen rmtldiff_early_late_drop= -rmstdiff_early_late_drop

twoway  (rarea rmtldiff_tr_comp_early_drop_lci rmtldiff_tr_comp_early_drop_uci tt, color(gs2%35)) ///
                 (line rmtldiff_tr_comp_early_drop tt, lcolor(gs2)) ///
		(rarea rmtldiff_tr_comp_late_drop_lci rmtldiff_tr_comp_late_drop_uci tt, color(gs6%35)) ///
                 (line rmtldiff_tr_comp_late_drop tt, lcolor(gs6)) ///
		(rarea rmtldiff_early_late_drop_lci rmtldiff_early_late_drop_uci tt, color(gs10%35)) ///
                 (line rmtldiff_early_late_drop tt, lcolor(gs10)) ///				 
         				  (line zero tt, lcolor(black%20) lwidth(thick)) ///
         , ylabel(, format(%3.1f)) ///
         ytitle("Difference in RMTL (years)") ///
         xtitle("Years from baseline treatment outcome") ///
		 legend(order( 1 "Early vs. Tr. completion" 3 "Late vs. Tr. completion" 5 "Late vs. Early dropout") ring(0) pos(11) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) /// //text(.5 1 "IR = <0.001") ///
                 name(RMTLdiff, replace)
gr_edit .plotregion1.style.editstyle boxstyle(linestyle(color(none))) editcopy				 
graph save "`c(pwd)'\_figs\h_m_ns_rp6_stdif_rmtl_pris_m1.gph", replace
graph export "`c(pwd)'\_figs\h_m_ns_rp6_stdif_rmtl_pris_m1.pdf", as(pdf) replace 
<</dd_do>>
~~~~

<<dd_graph: saving("h_m_ns_rp6_stdif_rmtl_pris_m1.svg") width(800) replace>>




*#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
*#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
*#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:

# Estimate time-specific survival probabilities & RMSTs

**Condemnatory Sentence, Imputed**

~~~~
<<dd_do>>
use mariel_feb_23_m1.dta, clear 
*qui estread using "mariel_feb_23_m1.sters"
generate times = .
replace times  = 1 if _n==1
replace times = 3 if _n==2
replace times = 5 if _n==3

* estimates replay m_nostag_rp8_tvc_1
*1.994719

global covs_3b_pre_dum "mot_egr_early mot_egr_late tr_mod2 sex_dum2 edad_ini_cons esc1 esc2 sus_prin2 sus_prin3 sus_prin4 sus_prin5 fr_cons_sus_prin2 fr_cons_sus_prin3 fr_cons_sus_prin4 fr_cons_sus_prin5 cond_ocu2 cond_ocu3 cond_ocu4 cond_ocu5 cond_ocu6 policonsumo num_hij2 tenviv1 tenviv2 tenviv4 tenviv5 mzone2 mzone3 n_off_vio n_off_acq n_off_sud n_off_oth psy_com2 dep2 rural2 rural3 porc_pobr susini2 susini3 susini4 susini5 ano_nac_corr cohab2 cohab3 cohab4 fis_com2 rc_x1 rc_x2 rc_x3"
set seed 2125
qui noi stpm2 $covs_3b_pre_dum , scale(hazard) df(8) eform tvc(mot_egr_early mot_egr_late) dftvc(1) 

stpm2_standsurv, at1(mot_egr_early 0 mot_egr_late 0) at2(mot_egr_early 1 mot_egr_late 0) timevar(times) rmst ci contrast(difference) ///
     atvar(rmst_v_h0 rmst_v_h1) contrastvar(rmstdiff_tr_c_early_drop)

stpm2_standsurv, at1(mot_egr_early 0 mot_egr_late 0) at2(mot_egr_early 0 mot_egr_late 1) timevar(times ) rmst ci contrast(difference) ///
     atvar(rmst_v_h00 rmst_v_h2) contrastvar(rmstdiff_tr_c_late_drop)

stpm2_standsurv, at1(mot_egr_early 1 mot_egr_late 0) at2(mot_egr_early 0 mot_egr_late 1) timevar(times ) rmst ci contrast(difference) ///
     atvar(rmst_v_h11 rmst_v_h22) contrastvar(rmstdiff_erl_late_drop)	

set seed 2125
qui noi stpm2 $covs_3b_pre_dum , scale(hazard) df(8) eform tvc(mot_egr_early mot_egr_late) dftvc(1) 

stpm2_standsurv, at1(mot_egr_early 0 mot_egr_late 0) at2(mot_egr_early 1 mot_egr_late 0) timevar(times) ci contrast(difference) ///
     atvar(s_v_tr_comp s_v_early_drop) contrastvar(sdiff_v_tr_comp_early_drop)

stpm2_standsurv, at1(mot_egr_early 0 mot_egr_late 0) at2(mot_egr_early 0 mot_egr_late 1) timevar(times) ci contrast(difference) ///
     atvar(s_v_tr_comp0 s_v_late_drop) contrastvar(sdiff_v_tr_comp_late_drop)

stpm2_standsurv, at1(mot_egr_early 1 mot_egr_late 0) at2(mot_egr_early 0 mot_egr_late 1) timevar(times) ci contrast(difference) ///
     atvar(s_v_early_drop0 s_v_late_drop0) contrastvar(sdiff_v_early_late_drop)	
*In https://fondecytacc.github.io/nDP/analisis_mariel_feb_2023_stata_m1.html
*        mot_egr_early |    1.74308   .0445435    21.74   0.000     1.657926    1.832607

*After qui noi stpm2 here
*        mot_egr_early |   1.745549   .0446049    21.80   0.000     1.660278    1.835199
<</dd_do>>
~~~~

~~~~
<<dd_do>>	 
*list times rmst_v_h0 rmst_v_h0_lci rmst_v_h0_uci rmst_v_h1 rmst_v_h1_lci rmst_v_h1_uci rmst_v_h2 rmst_v_h2_lci rmst_v_h2_uci if !missing(times)

*sdiff_tr_comp_early_drop sdiff_tr_comp_late_drop sdiff_early_late_drop
foreach var of varlist s_v_tr_comp s_v_tr_comp_lci s_v_tr_comp_uci s_v_early_drop s_v_early_drop_lci s_v_early_drop_uci s_v_late_drop s_v_late_drop_lci s_v_late_drop_uci {
			scalar variable = "`var'"		
					qui summarize `var' if inrange(times, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(times, 2.85, 3.15) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(times,  4.85, 5.15) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix ests_`var' = (`=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = "1 yr" "3 yrs" "5 yrs"
	}	

matrix est_as11m1_alt = (ests_s_v_tr_comp , ests_s_v_tr_comp_lci, ests_s_v_tr_comp_uci ,  ///
	ests_s_v_early_drop , ests_s_v_early_drop_lci, ests_s_v_early_drop_uci , ///
	ests_s_v_late_drop , ests_s_v_late_drop_lci , ests_s_v_late_drop_uci )
matrix colnames est_as11m1_alt = Comp Comp_lci Comp_uci Early Early_lci Early_uci Late Late_lci Late_uci 

esttab matrix(est_as11m1_alt) using "${pathdata2}prob_condsent_m1_main_alt.html", replace 

	

foreach var of varlist rmst_v_h0 rmst_v_h0_lci rmst_v_h0_uci rmst_v_h1 rmst_v_h1_lci rmst_v_h1_uci rmst_v_h2 rmst_v_h2_lci rmst_v_h2_uci {
			scalar variable = "`var'"
					qui summarize `var' if inrange(times, .95, 1.05) // tolerance of .02
					scalar e1y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(times, 2.95, 3.05) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(times,  4.95, 5.05) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.0001),.001)
	 cap noi matrix ests_`var' = (`=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = "1 yr" "3 yrs" "5 yrs"
	}		
	
matrix est_as12m1_alt = (ests_rmst_v_h0 , ests_rmst_v_h0_lci , ests_rmst_v_h0_uci ,  ///
	ests_rmst_v_h1 , ests_rmst_v_h1_lci , ests_rmst_v_h1_uci ,  ///
	ests_rmst_v_h2 , ests_rmst_v_h2_lci , ests_rmst_v_h2_uci )
matrix colnames est_as12m1_alt = Comp Comp_lci Comp_uci Early Early_lci Early_uci Late Late_lci Late_uci 

esttab matrix(est_as12m1_alt) using "${pathdata2}rmst_condsent_m1_main_alt.html", replace 	 
	 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}prob_condsent_m1_main_alt.html" >>

<<dd_include: "${pathdata2}rmst_condsent_m1_main_alt.html" >>

~~~~
<<dd_do>>
*sdiff_v_early_late_drop sdiff_v_tr_comp_late_drop  sdiff_v_tr_comp_early_drop
local varlist "sdiff_v_tr_comp_early_drop sdiff_v_tr_comp_early_drop_lci sdiff_v_tr_comp_early_drop_uci sdiff_v_tr_comp_late_drop sdiff_v_tr_comp_late_drop_lci sdiff_v_tr_comp_late_drop_uci sdiff_v_early_late_drop sdiff_v_early_late_drop_lci sdiff_v_early_late_drop_uci"
foreach var of local varlist {
    local newvar = subinstr("`var'", "_drop", "", .)
    gen `newvar' = `var'
}

foreach var of varlist sdiff_v_tr_comp_early sdiff_v_tr_comp_early_lci sdiff_v_tr_comp_early_uci sdiff_v_tr_comp_late sdiff_v_tr_comp_late_lci sdiff_v_tr_comp_late_uci sdiff_v_early_late sdiff_v_early_late_lci sdiff_v_early_late_uci {
	scalar variable = "`var'"
					qui summarize `var' if inrange(times, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(times, 2.95, 3.05) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(times,  4.95, 5.05) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix ests_`var' = ( `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = "1 yr" "3 yrs" "5 yrs"
		}
	
matrix est_as14m1_alt = (ests_sdiff_v_tr_comp_early , ests_sdiff_v_tr_comp_early_lci , ests_sdiff_v_tr_comp_early_uci ,  ///
	ests_sdiff_v_tr_comp_late , ests_sdiff_v_tr_comp_late_lci, ests_sdiff_v_tr_comp_late_uci , ///
	ests_sdiff_v_early_late , ests_sdiff_v_early_late_lci , ests_sdiff_v_early_late_uci )
matrix colnames est_as14m1_alt = "Comp vs Early" "Comp vs Early(lci)" "Comp vs Early(uci)" "Comp vs Late" "Comp vs Late (lci)" "Comp vs Late (uci)" "Early vs Late" "Early vs Late (lci)" "Early vs Late (uci)"

esttab matrix(est_as14m1_alt) using "${pathdata2}prob_condsent_m1_main_diff_alt.html", replace 

<</dd_do>>
~~~~

<<dd_include: "${pathdata2}prob_condsent_m1_main_diff_alt.html" >>

~~~~
<<dd_do>>
local varlist "rmstdiff_tr_c_early_drop rmstdiff_tr_c_early_drop_lci rmstdiff_tr_c_early_drop_uci rmstdiff_tr_c_late_drop rmstdiff_tr_c_late_drop_lci rmstdiff_tr_c_late_drop_uci  rmstdiff_erl_late_drop rmstdiff_erl_late_drop_lci rmstdiff_erl_late_drop_uci"
foreach var of local varlist {
    local newvar = subinstr("`var'", "_drop", "", .)
	local newvar = subinstr("`newvar'", "rmstdiff", "rmstd", .)
    gen `newvar' = `var'
}

foreach var of varlist rmstd_tr_c_early rmstd_tr_c_early_lci rmstd_tr_c_early_uci rmstd_tr_c_late rmstd_tr_c_late_lci rmstd_tr_c_late_uci rmstd_erl_late rmstd_erl_late_lci rmstd_erl_late_uci {
	scalar variable = "`var'"
					qui summarize `var' if inrange(times, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(times, 2.95, 3.05) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(times,  4.95, 5.05) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.0001),.001)
	 cap noi matrix ests_`var' = (`=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = "1 yr" "3 yrs" "5 yrs"
		}
	
matrix est_as14m1_alt = (ests_rmstd_tr_c_early, ests_rmstd_tr_c_early_lci, ests_rmstd_tr_c_early_uci, ///
					ests_rmstd_tr_c_late, ests_rmstd_tr_c_late_lci, ests_rmstd_tr_c_late_uci, ///
					ests_rmstd_erl_late, ests_rmstd_erl_late_lci, ests_rmstd_erl_late_uci )
matrix colnames est_as14m1_alt =  "Comp vs Early" "Comp vs Early(lci)" "Comp vs Early(uci)" "Comp vs Late" "Comp vs Late (lci)" "Comp vs Late (uci)" "Early vs Late" "Early vs Late (lci)" "Early vs Late (uci)"

esttab matrix(est_as14m1_alt) using "${pathdata2}rmst_condsent_m1_main_diff_alt.html", replace 

<</dd_do>>
~~~~

<<dd_include: "${pathdata2}rmst_condsent_m1_main_diff_alt.html" >>

**Imprisonment, Imputed**

~~~~
<<dd_do>>
use mariel_feb_23_2_m1.dta, clear 
*estread using "mariel_feb_23_2_m1.sters"

generate times = .
replace times  = 1 if _n==1
replace times = 3 if _n==2
replace times = 5 if _n==3

global covs_3b_pre_dum "mot_egr_early mot_egr_late tr_mod2 sex_dum2 edad_ini_cons esc1 esc2 sus_prin2 sus_prin3 sus_prin4 sus_prin5 fr_cons_sus_prin2 fr_cons_sus_prin3 fr_cons_sus_prin4 fr_cons_sus_prin5 cond_ocu2 cond_ocu3 cond_ocu4 cond_ocu5 cond_ocu6 policonsumo num_hij2 tenviv1 tenviv2 tenviv4 tenviv5 mzone2 mzone3 n_off_vio n_off_acq n_off_sud n_off_oth psy_com2 dep2 rural2 rural3 porc_pobr susini2 susini3 susini4 susini5 ano_nac_corr cohab2 cohab3 cohab4 fis_com2 rc_x1 rc_x2 rc_x3"

qui noi stpm2 $covs_3b_pre_dum , scale(hazard) df(6) eform tvc(mot_egr_early mot_egr_late) dftvc(1) 

stpm2_standsurv, at1(mot_egr_early 0 mot_egr_late 0) at2(mot_egr_early 1 mot_egr_late 0) timevar(times) rmst ci contrast(difference) ///
     atvar(rmst_v_h0 rmst_v_h1) contrastvar(rmstdiff_tr_c_early_drop)

stpm2_standsurv, at1(mot_egr_early 0 mot_egr_late 0) at2(mot_egr_early 0 mot_egr_late 1) timevar(times ) rmst ci contrast(difference) ///
     atvar(rmst_v_h00 rmst_v_h2) contrastvar(rmstdiff_tr_c_late_drop)

stpm2_standsurv, at1(mot_egr_early 1 mot_egr_late 0) at2(mot_egr_early 0 mot_egr_late 1) timevar(times ) rmst ci contrast(difference) ///
     atvar(rmst_v_h11 rmst_v_h22) contrastvar(rmstdiff_erl_late_drop)	


qui noi stpm2 $covs_3b_pre_dum , scale(hazard) df(6) eform tvc(mot_egr_early mot_egr_late) dftvc(1) 

stpm2_standsurv, at1(mot_egr_early 0 mot_egr_late 0) at2(mot_egr_early 1 mot_egr_late 0) timevar(times) ci contrast(difference) ///
     atvar(s_v_tr_comp s_v_early_drop) contrastvar(sdiff_v_tr_comp_early_drop)

stpm2_standsurv, at1(mot_egr_early 0 mot_egr_late 0) at2(mot_egr_early 0 mot_egr_late 1) timevar(times) ci contrast(difference) ///
     atvar(s_v_tr_comp0 s_v_late_drop) contrastvar(sdiff_v_tr_comp_late_drop)

stpm2_standsurv, at1(mot_egr_early 1 mot_egr_late 0) at2(mot_egr_early 0 mot_egr_late 1) timevar(times) ci contrast(difference) ///
     atvar(s_v_early_drop0 s_v_late_drop0) contrastvar(sdiff_v_early_late_drop)	
<</dd_do>>
~~~~	 
	 

~~~~
<<dd_do>>	 
*list times rmst_v_h0 rmst_v_h0_lci rmst_v_h0_uci rmst_v_h1 rmst_v_h1_lci rmst_v_h1_uci rmst_v_h2 rmst_v_h2_lci rmst_v_h2_uci if !missing(times)

*sdiff_tr_comp_early_drop sdiff_tr_comp_late_drop sdiff_early_late_drop
foreach var of varlist s_v_tr_comp s_v_tr_comp_lci s_v_tr_comp_uci s_v_early_drop s_v_early_drop_lci s_v_early_drop_uci s_v_late_drop s_v_late_drop_lci s_v_late_drop_uci {
			scalar variable = "`var'"		
					qui summarize `var' if inrange(times, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(times, 2.85, 3.15) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(times,  4.85, 5.15) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix ests_`var' = (`=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = "1 yr" "3 yrs" "5 yrs"
	}	

matrix est_as21m1_alt = (ests_s_v_tr_comp , ests_s_v_tr_comp_lci, ests_s_v_tr_comp_uci ,  ///
	ests_s_v_early_drop , ests_s_v_early_drop_lci, ests_s_v_early_drop_uci , ///
	ests_s_v_late_drop , ests_s_v_late_drop_lci , ests_s_v_late_drop_uci )
matrix colnames est_as21m1_alt = Comp Comp_lci Comp_uci Early Early_lci Early_uci Late Late_lci Late_uci 

esttab matrix(est_as21m1_alt) using "${pathdata2}prob_prison_m1_main_alt.html", replace 

	

foreach var of varlist rmst_v_h0 rmst_v_h0_lci rmst_v_h0_uci rmst_v_h1 rmst_v_h1_lci rmst_v_h1_uci rmst_v_h2 rmst_v_h2_lci rmst_v_h2_uci {
			scalar variable = "`var'"
					qui summarize `var' if inrange(times, .95, 1.05) // tolerance of .02
					scalar e1y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(times, 2.95, 3.05) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(times,  4.95, 5.05) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.0001),.001)
	 cap noi matrix ests_`var' = (`=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = "1 yr" "3 yrs" "5 yrs"
	}		
	
matrix est_as22m1_alt = (ests_rmst_v_h0 , ests_rmst_v_h0_lci , ests_rmst_v_h0_uci ,  ///
	ests_rmst_v_h1 , ests_rmst_v_h1_lci , ests_rmst_v_h1_uci ,  ///
	ests_rmst_v_h2 , ests_rmst_v_h2_lci , ests_rmst_v_h2_uci )
matrix colnames est_as22m1_alt = Comp Comp_lci Comp_uci Early Early_lci Early_uci Late Late_lci Late_uci 

esttab matrix(est_as22m1_alt) using "${pathdata2}rmst_prison_m1_main_alt.html", replace 	 
	 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}prob_prison_m1_main_alt.html" >>

<<dd_include: "${pathdata2}rmst_prison_m1_main_alt.html" >>

~~~~
<<dd_do>>
*sdiff_v_early_late_drop sdiff_v_tr_comp_late_drop  sdiff_v_tr_comp_early_drop
local varlist "sdiff_v_tr_comp_early_drop sdiff_v_tr_comp_early_drop_lci sdiff_v_tr_comp_early_drop_uci sdiff_v_tr_comp_late_drop sdiff_v_tr_comp_late_drop_lci sdiff_v_tr_comp_late_drop_uci sdiff_v_early_late_drop sdiff_v_early_late_drop_lci sdiff_v_early_late_drop_uci"
foreach var of local varlist {
    local newvar = subinstr("`var'", "_drop", "", .)
    gen `newvar' = `var'
}

foreach var of varlist sdiff_v_tr_comp_early sdiff_v_tr_comp_early_lci sdiff_v_tr_comp_early_uci sdiff_v_tr_comp_late sdiff_v_tr_comp_late_lci sdiff_v_tr_comp_late_uci sdiff_v_early_late sdiff_v_early_late_lci sdiff_v_early_late_uci {
	scalar variable = "`var'"
					qui summarize `var' if inrange(times, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(times, 2.95, 3.05) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(times,  4.95, 5.05) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix ests_`var' = ( `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = "1 yr" "3 yrs" "5 yrs"
		}
	
matrix est_as23m1_alt = (ests_sdiff_v_tr_comp_early , ests_sdiff_v_tr_comp_early_lci , ests_sdiff_v_tr_comp_early_uci ,  ///
	ests_sdiff_v_tr_comp_late , ests_sdiff_v_tr_comp_late_lci, ests_sdiff_v_tr_comp_late_uci , ///
	ests_sdiff_v_early_late , ests_sdiff_v_early_late_lci , ests_sdiff_v_early_late_uci )
matrix colnames est_as23m1_alt = "Comp vs Early" "Comp vs Early(lci)" "Comp vs Early(uci)" "Comp vs Late" "Comp vs Late (lci)" "Comp vs Late (uci)" "Early vs Late" "Early vs Late (lci)" "Early vs Late (uci)"

esttab matrix(est_as23m1_alt) using "${pathdata2}prob_prison_m1_main_diff_alt.html", replace 

<</dd_do>>
~~~~


<<dd_include: "${pathdata2}prob_prison_m1_main_diff_alt.html" >>

~~~~
<<dd_do>>
local varlist "rmstdiff_tr_c_early_drop rmstdiff_tr_c_early_drop_lci rmstdiff_tr_c_early_drop_uci rmstdiff_tr_c_late_drop rmstdiff_tr_c_late_drop_lci rmstdiff_tr_c_late_drop_uci  rmstdiff_erl_late_drop rmstdiff_erl_late_drop_lci rmstdiff_erl_late_drop_uci"
foreach var of local varlist {
    local newvar = subinstr("`var'", "_drop", "", .)
	local newvar = subinstr("`newvar'", "rmstdiff", "rmstd", .)
    gen `newvar' = `var'
}

foreach var of varlist rmstd_tr_c_early rmstd_tr_c_early_lci rmstd_tr_c_early_uci rmstd_tr_c_late rmstd_tr_c_late_lci rmstd_tr_c_late_uci rmstd_erl_late rmstd_erl_late_lci rmstd_erl_late_uci {
	scalar variable = "`var'"
					qui summarize `var' if inrange(times, .95, 1.05) // tolerance of .10
					scalar e1y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(times, 2.95, 3.05) // tolerance of .30
					scalar e3y_`var' = round(round(r(mean),.0001),.001)
					qui summarize `var' if inrange(times,  4.95, 5.05) // tolerance of .30
					scalar e5y_`var' = round(round(r(mean),.0001),.001)
	 cap noi matrix ests_`var' = (`=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = "1 yr" "3 yrs" "5 yrs"
		}
	
matrix est_as24m1_alt = (ests_rmstd_tr_c_early, ests_rmstd_tr_c_early_lci, ests_rmstd_tr_c_early_uci, ///
					ests_rmstd_tr_c_late, ests_rmstd_tr_c_late_lci, ests_rmstd_tr_c_late_uci, ///
					ests_rmstd_erl_late, ests_rmstd_erl_late_lci, ests_rmstd_erl_late_uci )
matrix colnames est_as24m1_alt =  "Comp vs Early" "Comp vs Early(lci)" "Comp vs Early(uci)" "Comp vs Late" "Comp vs Late (lci)" "Comp vs Late (uci)" "Early vs Late" "Early vs Late (lci)" "Early vs Late (uci)"

esttab matrix(est_as24m1_alt) using "${pathdata2}rmst_prison_m1_main_diff_alt.html", replace 

<</dd_do>>
~~~~

<<dd_include: "${pathdata2}rmst_prison_m1_main_diff_alt.html" >>


~~~~
<<dd_do>>
**Perera M, Dwivedi AK. Statistical issues and methods in designing and analyzing survival studies. Cancer Rep (Hoboken). 2020;3(4):e1176. doi:10.1002/cnr2.1176
*https://www.statalist.org/forums/forum/general-stata-discussion/general/1461519-optimism-in-flexible-parametric-survival-analysis-by-stpm2-please
* https://sper.org/wp-content/uploads/2019/03/AMW-2012_Kaufman-Schempf_Absolute-Epidemiology.pdf
*https://journals.sagepub.com/doi/pdf/10.1177/1536867X1201200405
use mariel_feb_23_2_m1.dta, clear 
*estread using "mariel_feb_23_2_m1.sters"

generate times = .
replace times  = 1 if _n==1
replace times = 3 if _n==2
replace times = 5 if _n==3

global covs_3b_pre_dum "mot_egr_early mot_egr_late tr_mod2 sex_dum2 edad_ini_cons esc1 esc2 sus_prin2 sus_prin3 sus_prin4 sus_prin5 fr_cons_sus_prin2 fr_cons_sus_prin3 fr_cons_sus_prin4 fr_cons_sus_prin5 cond_ocu2 cond_ocu3 cond_ocu4 cond_ocu5 cond_ocu6 policonsumo num_hij2 tenviv1 tenviv2 tenviv4 tenviv5 mzone2 mzone3 n_off_vio n_off_acq n_off_sud n_off_oth psy_com2 dep2 rural2 rural3 porc_pobr susini2 susini3 susini4 susini5 ano_nac_corr cohab2 cohab3 cohab4 fis_com2 rc_x1 rc_x2 rc_x3"

capture program drop b_conc

program define b_conc, rclass
qui noi stpm2 $covs_3b_pre_dum , scale(hazard) df(6) eform tvc(mot_egr_early mot_egr_late) dftvc(1) 
stcstat2
return scalar c = r(C)
end
bootstrap c=r(c), reps(500): b_conc

<<dd_display: "Ended at= `c(current_time)' `c(current_date)'">>
