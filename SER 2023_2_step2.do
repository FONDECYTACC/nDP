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
## Transition probabilities & Lengths of Stay, Parametric Models
=============================================================================

# Tables of PRs & LOS at 3 years

## Probabilities

### Probabilities_First patient

#### Probabilities_First patient_First transition

~~~~
<<dd_do>>
foreach var of varlist fprob_from1a_2_2m fprob_from1a_2_lci_2m fprob_from1a_2_uci_2m fprob_from1b_2_2m fprob_from1b_2_lci_2m fprob_from1b_2_uci_2m {
				scalar variable = "`var'"
					qui summarize `var' if inrange(timevar0, .30, .40)
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(timevar0, .98, 1.02)
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(timevar0, 2.98, 3.02)
					scalar e2m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(timevar0,  4.98, 5.02)
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e1y_`var')'\ `=scalar(scalar(e2m_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 1_yr 3_yrs 5_yrs
 }

matrix est_bs12 = (ests_fprob_from1a_2_2m, ests_fprob_from1a_2_lci_2m, ests_fprob_from1a_2_uci_2m, ests_fprob_from1b_2_2m, ests_fprob_from1b_2_lci_2m, ests_fprob_from1b_2_uci_2m)
matrix colnames est_bs12 = NoPoly NoPoly_lci NoPoly_uci Poly Poly_lci Poly_uci

esttab matrix(est_bs12) using "${pathdata2}pr_1st_trans_ser23.html", replace 
<</dd_do>>
~~~~

The transition probabilities are presented here:

<<dd_include: "${pathdata2}pr_1st_trans_ser23.html" >>


#### Probabilities_First patient_Second_and_Third transition

~~~~
<<dd_do>>
foreach var of varlist fprob_from1a_3_2m fprob_from1a_3_lci_2m fprob_from1a_3_uci_2m fprob_from1b_3_2m fprob_from1b_3_lci_2m fprob_from1b_3_uci_2m fprob_from2a_3_2m fprob_from2a_3_lci_2m fprob_from2a_3_uci_2m fprob_from2b_3_2m fprob_from2b_3_lci_2m fprob_from2b_3_uci_2m {
				scalar variable = "`var'"
					qui summarize `var' if inrange(timevar0, .30, .40)
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(timevar0, .98, 1.02)
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(timevar0, 2.98, 3.02)
					scalar e2m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(timevar0,  4.98, 5.02)
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e1y_`var')'\ `=scalar(scalar(e2m_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 1_yr 3_yrs 5_yrs
 }

matrix est_bs32 = (ests_fprob_from1a_3_2m, ests_fprob_from1a_3_lci_2m, ests_fprob_from1a_3_uci_2m, ests_fprob_from1b_3_2m, ests_fprob_from1b_3_lci_2m, ests_fprob_from1b_3_uci_2m, ests_fprob_from2a_3_2m, ests_fprob_from2a_3_lci_2m, ests_fprob_from2a_3_uci_2m, ests_fprob_from2b_3_2m, ests_fprob_from2b_3_lci_2m, ests_fprob_from2b_3_uci_2m)
matrix colnames est_bs32 = NoPoly13 NoPoly13_lci NoPoly13_uci Poly13 Poly13_lci Poly13_uci NoPoly23 NoPoly23_lci NoPoly23_uci Poly23 Poly23_lci Poly23_uci

esttab matrix(est_bs32) using "${pathdata2}pr_3rd_trans_ser23.html", replace 
<</dd_do>>
~~~~

The transition probabilities are presented here:

<<dd_include: "${pathdata2}pr_3rd_trans_ser23.html" >>



## Lengths of Stay

### Lengths of Stay_First patient

#### Length_of_stay_First patient_Time_in_admission

~~~~
<<dd_do>>
foreach var of varlist flos_from1a_1_2m flos_from1a_1_lci_2m flos_from1a_1_uci_2m flos_from1b_1_2m flos_from1b_1_lci_2m flos_from1b_1_uci_2m {
	scalar variable = "`var'"
					qui summarize `var' if inrange(timevar0, .30, .40)
					scalar e3m_`var' = round(round(r(mean),.001),.01)
					qui summarize `var' if inrange(timevar0, .98, 1.02)
					scalar e1y_`var' = round(round(r(mean),.001),.01)
					qui summarize `var' if inrange(timevar0, 2.98, 3.02)
					scalar e2m_`var' = round(round(r(mean),.001),.01)
					qui summarize `var' if inrange(timevar0,  4.98, 5.02)
					scalar e5y_`var' = round(round(r(mean),.001),.01)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e1y_`var')'\ `=scalar(scalar(e2m_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 1_yr 3_yrs 5_yrs
		}

matrix est_cs12 = (ests_flos_from1a_1_2m, ests_flos_from1a_1_lci_2m, ests_flos_from1a_1_uci_2m, ests_flos_from1b_1_2m, ests_flos_from1b_1_lci_2m, ests_flos_from1b_1_uci_2m)
matrix colnames est_cs12 = NoPoly NoPoly_lci NoPoly_uci Poly Poly_lci Poly_uci

esttab matrix(est_cs12) using "${pathdata2}los_1st_trans_ser23.html", replace 
<</dd_do>>
~~~~

The lengths of stay are presented here:

<<dd_include: "${pathdata2}los_1st_trans_ser23.html" >>

#### Length_of_stay_First patient_Time_in_completion

~~~~
<<dd_do>>
foreach var of varlist flos_from2a_2_2m flos_from2a_2_lci_2m flos_from2a_2_uci_2m flos_from2b_2_2m flos_from2b_2_lci_2m flos_from2b_2_uci_2m {
	scalar variable = "`var'"
					qui summarize `var' if inrange(timevar0, .30, .40)
					scalar e3m_`var' = round(round(r(mean),.001),.01)
					qui summarize `var' if inrange(timevar0, .98, 1.02)
					scalar e1y_`var' = round(round(r(mean),.001),.01)
					qui summarize `var' if inrange(timevar0, 2.98, 3.02)
					scalar e2m_`var' = round(round(r(mean),.001),.01)
					qui summarize `var' if inrange(timevar0,  4.98, 5.02)
					scalar e5y_`var' = round(round(r(mean),.001),.01)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e1y_`var')'\ `=scalar(scalar(e2m_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 1_yr 3_yrs 5_yrs
		}

matrix est_cs22 = (ests_flos_from2a_2_2m, ests_flos_from2a_2_lci_2m, ests_flos_from2a_2_uci_2m, ests_flos_from2b_2_2m, ests_flos_from2b_2_lci_2m, ests_flos_from2b_2_uci_2m)
matrix colnames est_cs22 = "NoPoly2" "NoPoly2_lci" "NoPoly2_uci" "Poly2" "Poly2_lci" "Poly2_uci"

esttab matrix(est_cs22) using "${pathdata2}los_2nd_trans_ser23.html", replace 
<</dd_do>>
~~~~

The lengths of stay are presented here:

<<dd_include: "${pathdata2}los_2nd_trans_ser23.html" >>


### Difference_Probabilities

~~~~
<<dd_do>>
*list fdiff_prob_from1_2_2m fdiff_prob_from1_2_lci_2m fdiff_prob_from1_2_uci_2m if inrange(timevar0 , .32, .34)|inrange(timevar0 , .999,  1.02)|inrange(timevar0 , 2.99, 3.02)|inrange(timevar0 , 4.999, 5.02)

foreach var of varlist fdiff_prob_from1_2_2m  fdiff_prob_from1_2_lci_2m  fdiff_prob_from1_2_uci_2m fdiff_prob_from1_3_2m  fdiff_prob_from1_3_lci_2m  fdiff_prob_from1_3_uci_2m fdiff_prob_from2_3_2m fdiff_prob_from2_3_lci_2m fdiff_prob_from2_3_uci_2m {
				scalar variable = "`var'"
					qui summarize `var' if inrange(timevar0, .30, .40)
					scalar e3m_`var' = round(round(r(mean),.001)*100,.01)
					qui summarize `var' if inrange(timevar0, .98, 1.02)
					scalar e1y_`var' = round(round(r(mean),.001)*100,.01)
					qui summarize `var' if inrange(timevar0, 2.98, 3.02)
					scalar e2m_`var' = round(round(r(mean),.001)*100,.01)
					qui summarize `var' if inrange(timevar0,  4.98, 5.02)
					scalar e5y_`var' = round(round(r(mean),.001)*100,.01)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e1y_`var')'\ `=scalar(scalar(e2m_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 1_yr 3_yrs 5_yrs
 }

matrix est_diff11 = (ests_fdiff_prob_from1_2_2m, ests_fdiff_prob_from1_2_lci_2m, ests_fdiff_prob_from1_2_uci_2m, ests_fdiff_prob_from1_3_2m, ests_fdiff_prob_from1_3_lci_2m, ests_fdiff_prob_from1_3_uci_2m, ests_fdiff_prob_from2_3_2m, ests_fdiff_prob_from2_3_lci_2m, ests_fdiff_prob_from2_3_uci_2m)
matrix colnames est_diff11 =  "to Completion" "to Completion_lo" "to Completion_hi" "to Non-completion" "to Non-completion_lo" "to Non-completion_hi" "from Completion" "from Completion_lo" "from Completion_hi"

esttab matrix(est_diff11) using "${pathdata2}diff_probs_res_out_mod_t1_2m_22_corr3.html", replace 
<</dd_do>>
~~~~

The transition probabilities are presented here:

<<dd_include: "${pathdata2}diff_probs_res_out_mod_t1_2m_22_corr3.html" >>


### Difference_Length_of_Stay


#### Difference_Length_of_Stay_from_Admission_to_Completion_to_Non-completion

~~~~
<<dd_do>>
foreach var of varlist fdiff_los_from1_1_2m fdiff_los_from1_1_lci_2m fdiff_los_from1_1_uci_2m {
		scalar variable = "`var'"
					qui summarize `var' if inrange(timevar0, .30, .40)
					scalar e3m_`var' = round(round(r(mean),.001)*1,.01)
					qui summarize `var' if inrange(timevar0, .98, 1.02)
					scalar e1y_`var' = round(round(r(mean),.001)*1,.01)
					qui summarize `var' if inrange(timevar0, 2.98, 3.02)
					scalar e2m_`var' = round(round(r(mean),.001)*1,.01)
					qui summarize `var' if inrange(timevar0,  4.98, 5.02)
					scalar e5y_`var' = round(round(r(mean),.001)*1,.01)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e1y_`var')'\ `=scalar(scalar(e2m_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 1_yr 3_yrs 5_yrs
		}
matrix est_diff12_l = (ests_fdiff_los_from1_1_2m, ests_fdiff_los_from1_1_lci_2m, ests_fdiff_los_from1_1_uci_2m)
matrix colnames est_diff12_l =  "in_Admission" "in_Admission_lo" "in_Admission_hi"

esttab matrix(est_diff12_l) using "${pathdata2}diff_los_res_out_mod_t1_2m_22_corr3.html", replace 
<</dd_do>>
~~~~

The lengths of stay are presented here:

<<dd_include: "${pathdata2}diff_los_res_out_mod_t1_2m_22_corr3.html" >>


#### Difference_Length_of_Stay_from_Second_and_third_state

~~~~
<<dd_do>>
foreach var of varlist fdiff_los_from2_2_2m fdiff_los_from2_2_lci_2m fdiff_los_from2_2_uci_2m {
		scalar variable = "`var'"
					qui summarize `var' if inrange(timevar0, .30, .40)
					scalar e3m_`var' = round(round(r(mean),.001)*1,.01)
					qui summarize `var' if inrange(timevar0, .98, 1.02)
					scalar e1y_`var' = round(round(r(mean),.001)*1,.01)
					qui summarize `var' if inrange(timevar0, 2.98, 3.02)
					scalar e2m_`var' = round(round(r(mean),.001)*1,.01)
					qui summarize `var' if inrange(timevar0,  4.98, 5.02)
					scalar e5y_`var' = round(round(r(mean),.001)*1,.01)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e1y_`var')'\ `=scalar(scalar(e2m_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 1_yr 3_yrs 5_yrs
		}
matrix est_diff22_l = (ests_fdiff_los_from2_2_2m, ests_fdiff_los_from2_2_lci_2m, ests_fdiff_los_from2_2_uci_2m)
matrix colnames est_diff22_l =  "in Completion" "in Completion_lo" "in Completion_hi"

esttab matrix(est_diff22_l) using "${pathdata2}diff_los_res_out_mod_t2_2m_22_corr3.html", replace 
<</dd_do>>
~~~~

The lengths of stay are presented here:

<<dd_include: "${pathdata2}diff_los_res_out_mod_t2_2m_22_corr3.html" >>

   
<<dd_display: "Ended (not saved) at= `c(current_time)' `c(current_date)'">>


   
<<dd_do: nocommand>>
/*
FORMA DE EXPORTAR LOS DATOS Y EL MARKDOWN

cap rm "E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/an_ser_2023_step_2.html"
dyndoc "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\SER 2023_2_step2.do", saving("E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\an_ser_2023_step_2.html") replace nostop 
copy "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\an_ser_2023_step_2.html" "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\_outputs\an_ser_2023_step_2.html", replace

cap rm "C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/an_ser_2023_step_2.html"
dyndoc "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\SER 2023_2_step2.do", saving("C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\an_ser_2023_step_2.html") replace nostop 
copy "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\an_ser_2023_step_2.html" "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\_outputs\an_ser_2023_step_2.html", replace

_outputs

graph combine "transmat_ser23.gph" "pbal2_mod.gph", ///
colfirst iscale(*.6) imargin(tiny) graphregion(color(gs16))  /// // ycommon xcommon // l1(Differences in transition probabilities, size(medium)) b1(Time since admission (in years), size(medium)) ///
name(comb_ser23, replace)

*/
<</dd_do>>