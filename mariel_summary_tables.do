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

matrix est_as11 = (ests_s_tr_comp , ests_s_tr_comp_lci, ests_s_tr_comp_uci ,  ///
	ests_s_early_drop , ests_s_early_drop_lci, ests_s_early_drop_uci , ///
	ests_s_late_drop , ests_s_late_drop_lci , ests_s_late_drop_uci )
matrix colnames est_as11 = Comp Comp_lci Comp_uci Early Early_lci Early_uci Late Late_lci Late_uci 

esttab matrix(est_as11) using "${pathdata2}prob_condsent_m1_main.html", replace 
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

matrix est_as12 = (ests_rmst_h0 , ests_rmst_h0_lci , ests_rmst_h0_uci ,  ///
	ests_rmst_h1 , ests_rmst_h1_lci , ests_rmst_h1_uci ,  ///
	ests_rmst_h2 , ests_rmst_h2_lci , ests_rmst_h2_uci )
matrix colnames est_as12 = Comp Comp_lci Comp_uci Early Early_lci Early_uci Late Late_lci Late_uci 

esttab matrix(est_as12) using "${pathdata2}rmst_condsent_m1_main.html", replace 
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
	
matrix est_as13 = (ests_diff_tr_comp_early_drop , ests_diff_tr_comp_early_drop_lci , ests_diff_tr_comp_early_drop_uci ,  ///
	ests_diff_tr_comp_late_drop , ests_diff_tr_comp_late_drop_lci, ests_diff_tr_comp_late_drop_uci , ///
	ests_diff_early_late_drop , ests_diff_early_late_drop_lci , ests_diff_early_late_drop_uci )
matrix colnames est_as13 = Comp_Early Comp_Early_lci Comp_Early_uci Comp_Late Comp_Late_lci Comp_Late_uci Early_Late Early_Late_lci Early_Late_uci 

esttab matrix(est_as13) using "${pathdata2}prob_condsent_m1_main_diff.html", replace 
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

esttab matrix(est_as14) using "${pathdata2}rmst_condsent_m1_main_diff.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}rmst_condsent_m1_main_diff.html" >>


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

matrix est_as11 = (ests_s_tr_comp , ests_s_tr_comp_lci, ests_s_tr_comp_uci ,  ///
	ests_s_early_drop , ests_s_early_drop_lci, ests_s_early_drop_uci , ///
	ests_s_late_drop , ests_s_late_drop_lci , ests_s_late_drop_uci )
matrix colnames est_as11 = Comp Comp_lci Comp_uci Early Early_lci Early_uci Late Late_lci Late_uci 

esttab matrix(est_as11) using "${pathdata2}prob_condsent_m2_main.html", replace 
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

matrix est_as12 = (ests_rmst_h0 , ests_rmst_h0_lci , ests_rmst_h0_uci ,  ///
	ests_rmst_h1 , ests_rmst_h1_lci , ests_rmst_h1_uci ,  ///
	ests_rmst_h2 , ests_rmst_h2_lci , ests_rmst_h2_uci )
matrix colnames est_as12 = Comp Comp_lci Comp_uci Early Early_lci Early_uci Late Late_lci Late_uci 

esttab matrix(est_as12) using "${pathdata2}rmst_condsent_m2_main.html", replace 
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
	
matrix est_as13 = (ests_diff_tr_comp_early_drop , ests_diff_tr_comp_early_drop_lci , ests_diff_tr_comp_early_drop_uci ,  ///
	ests_diff_tr_comp_late_drop , ests_diff_tr_comp_late_drop_lci, ests_diff_tr_comp_late_drop_uci , ///
	ests_diff_early_late_drop , ests_diff_early_late_drop_lci , ests_diff_early_late_drop_uci )
matrix colnames est_as13 = Comp_Early Comp_Early_lci Comp_Early_uci Comp_Late Comp_Late_lci Comp_Late_uci Early_Late Early_Late_lci Early_Late_uci 

esttab matrix(est_as13) using "${pathdata2}prob_condsent_m2_main_diff.html", replace 
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
	
matrix est_as14 = (ests_diff_tr_comp_early_drop , ests_diff_tr_comp_early_drop_lci , ests_diff_tr_comp_early_drop_uci ,  ///
	ests_diff_tr_comp_late_drop , ests_diff_tr_comp_late_drop_lci, ests_diff_tr_comp_late_drop_uci , ///
	ests_diff_early_late_drop , ests_diff_early_late_drop_lci , ests_diff_early_late_drop_uci )
matrix colnames est_as14 = Comp_Early Comp_Early_lci Comp_Early_uci Comp_Late Comp_Late_lci Comp_Late_uci Early_Late Early_Late_lci Early_Late_uci 

esttab matrix(est_as14) using "${pathdata2}rmst_condsent_m2_main_diff.html", replace 
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

matrix est_as22 = (ests_rmst_h0 , ests_rmst_h0_lci , ests_rmst_h0_uci ,  ///
	ests_rmst_h1 , ests_rmst_h1_lci , ests_rmst_h1_uci ,  ///
	ests_rmst_h2 , ests_rmst_h2_lci , ests_rmst_h2_uci )
matrix colnames est_as12 = Comp Comp_lci Comp_uci Early Early_lci Early_uci Late Late_lci Late_uci 

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

matrix est_as21 = (ests_s_tr_comp , ests_s_tr_comp_lci, ests_s_tr_comp_uci ,  ///
	ests_s_early_drop , ests_s_early_drop_lci, ests_s_early_drop_uci , ///
	ests_s_late_drop , ests_s_late_drop_lci , ests_s_late_drop_uci )
matrix colnames est_as21 = Comp Comp_lci Comp_uci Early Early_lci Early_uci Late Late_lci Late_uci 

esttab matrix(est_as21) using "${pathdata2}prob_prison_m1_main.html", replace 
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

matrix est_as22 = (ests_rmst_h0 , ests_rmst_h0_lci , ests_rmst_h0_uci ,  ///
	ests_rmst_h1 , ests_rmst_h1_lci , ests_rmst_h1_uci ,  ///
	ests_rmst_h2 , ests_rmst_h2_lci , ests_rmst_h2_uci )
matrix colnames est_as12 = Comp Comp_lci Comp_uci Early Early_lci Early_uci Late Late_lci Late_uci 

esttab matrix(est_as22) using "${pathdata2}rmst_prison_m1_main.html", replace 
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
	
matrix est_as23 = (ests_diff_tr_comp_early_drop , ests_diff_tr_comp_early_drop_lci , ests_diff_tr_comp_early_drop_uci ,  ///
	ests_diff_tr_comp_late_drop , ests_diff_tr_comp_late_drop_lci, ests_diff_tr_comp_late_drop_uci , ///
	ests_diff_early_late_drop , ests_diff_early_late_drop_lci , ests_diff_early_late_drop_uci )
matrix colnames est_as23 = Comp_Early Comp_Early_lci Comp_Early_uci Comp_Late Comp_Late_lci Comp_Late_uci Early_Late Early_Late_lci Early_Late_uci 

esttab matrix(est_as23) using "${pathdata2}prob_prison_m1_main_diff.html", replace 
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
	
matrix est_as24 = (ests_diff_tr_comp_early_drop , ests_diff_tr_comp_early_drop_lci , ests_diff_tr_comp_early_drop_uci ,  ///
	ests_diff_tr_comp_late_drop , ests_diff_tr_comp_late_drop_lci, ests_diff_tr_comp_late_drop_uci , ///
	ests_diff_early_late_drop , ests_diff_early_late_drop_lci , ests_diff_early_late_drop_uci )
matrix colnames est_as24 = Comp_Early Comp_Early_lci Comp_Early_uci Comp_Late Comp_Late_lci Comp_Late_uci Early_Late Early_Late_lci Early_Late_uci 

esttab matrix(est_as24) using "${pathdata2}rmst_prison_m1_main_diff.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}rmst_prison_m1_main_diff.html" >>



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

matrix est_as21 = (ests_s_tr_comp , ests_s_tr_comp_lci, ests_s_tr_comp_uci ,  ///
	ests_s_early_drop , ests_s_early_drop_lci, ests_s_early_drop_uci , ///
	ests_s_late_drop , ests_s_late_drop_lci , ests_s_late_drop_uci )
matrix colnames est_as21 = Comp Comp_lci Comp_uci Early Early_lci Early_uci Late Late_lci Late_uci 

esttab matrix(est_as21) using "${pathdata2}prob_prison_m2_main.html", replace 
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

matrix est_as22 = (ests_rmst_h0 , ests_rmst_h0_lci , ests_rmst_h0_uci ,  ///
	ests_rmst_h1 , ests_rmst_h1_lci , ests_rmst_h1_uci ,  ///
	ests_rmst_h2 , ests_rmst_h2_lci , ests_rmst_h2_uci )
matrix colnames est_as12 = Comp Comp_lci Comp_uci Early Early_lci Early_uci Late Late_lci Late_uci 

esttab matrix(est_as22) using "${pathdata2}rmst_prison_m2_main.html", replace 
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
	
matrix est_as23 = (ests_diff_tr_comp_early_drop , ests_diff_tr_comp_early_drop_lci , ests_diff_tr_comp_early_drop_uci ,  ///
	ests_diff_tr_comp_late_drop , ests_diff_tr_comp_late_drop_lci, ests_diff_tr_comp_late_drop_uci , ///
	ests_diff_early_late_drop , ests_diff_early_late_drop_lci , ests_diff_early_late_drop_uci )
matrix colnames est_as23 = Comp_Early Comp_Early_lci Comp_Early_uci Comp_Late Comp_Late_lci Comp_Late_uci Early_Late Early_Late_lci Early_Late_uci 

esttab matrix(est_as23) using "${pathdata2}prob_prison_m2_main_diff.html", replace 
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
	
matrix est_as24 = (ests_diff_tr_comp_early_drop , ests_diff_tr_comp_early_drop_lci , ests_diff_tr_comp_early_drop_uci ,  ///
	ests_diff_tr_comp_late_drop , ests_diff_tr_comp_late_drop_lci, ests_diff_tr_comp_late_drop_uci , ///
	ests_diff_early_late_drop , ests_diff_early_late_drop_lci , ests_diff_early_late_drop_uci )
matrix colnames est_as24 = Comp_Early Comp_Early_lci Comp_Early_uci Comp_Late Comp_Late_lci Comp_Late_uci Early_Late Early_Late_lci Early_Late_uci 

esttab matrix(est_as24) using "${pathdata2}rmst_prison_m2_main_diff.html", replace 
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}rmst_prison_m2_main_diff.html" >>




## #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
## #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

# E-values, main analyses

#### #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#### #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_


### Condemnatory Sentence


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

evalue hr `=scalar(HR_early)' , lcl(`=scalar(HR_early_lo)') ucl(`=scalar(HR_early_up)') true(1) common figure

<</dd_do>>
~~~~

<<dd_graph: saving("eval_early.svg") width(800) replace>>

~~~~
<<dd_do>>
evalue hr `=scalar(HR_late)' , lcl(`=scalar(HR_late_lo)') ucl(`=scalar(HR_late_up)') true(1) common figure
<</dd_do>>
~~~~

<<dd_graph: saving("eval_late.svg") width(800) replace>>


### Imprisonment


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

evalue hr `=scalar(HR_early2)' , lcl(`=scalar(HR_early_lo2)') ucl(`=scalar(HR_early_up2)') true(1) common figure

<</dd_do>>
~~~~

<<dd_graph: saving("eval_early_pris.svg") width(800) replace>>


~~~~
<<dd_do>>
evalue hr `=scalar(HR_late2)' , lcl(`=scalar(HR_late_lo2)') ucl(`=scalar(HR_late_up2)') true(1) common figure
<</dd_do>>
~~~~

<<dd_graph: saving("eval_late_pris.svg") width(800) replace>>


<<dd_display: "Ended at= `c(current_time)' `c(current_date)'">>