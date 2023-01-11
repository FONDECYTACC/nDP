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
					qui summarize `var' if inrange(timevar0, .32, .34)
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(timevar0, .999, 1.02)
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(timevar0, 2.99, 3.02)
					scalar e2m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(timevar0,  4.999, 5.02)
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
					qui summarize `var' if inrange(timevar0, .32, .34)
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(timevar0, .999, 1.02)
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(timevar0, 2.99, 3.02)
					scalar e2m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(timevar0,  4.999, 5.02)
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

<<dd_include: "${pathdata2}pr_2nd_3rd_trans_ser23.html" >>



## Lengths of Stay

### Lengths of Stay_First patient

#### Length_of_stay_First patient_First transition

~~~~
<<dd_do>>
foreach var of varlist flos_from1a_1_2m flos_from1a_1_lci_2m flos_from1a_1_uci_2m flos_from1b_1_2m flos_from1b_1_lci_2m flos_from1b_1_uci_2m {
	scalar variable = "`var'"
					qui summarize `var' if inrange(timevar0, .32, .34)
					scalar e3m_`var' = round(round(r(mean),.001),.1)
					qui summarize `var' if inrange(timevar0, .999, 1.02)
					scalar e1y_`var' = round(round(r(mean),.001),.1)
					qui summarize `var' if inrange(timevar0, 2.99, 3.02)
					scalar e2m_`var' = round(round(r(mean),.001),.1)
					qui summarize `var' if inrange(timevar0,  4.999, 5.02)
					scalar e5y_`var' = round(round(r(mean),.001),.1)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e1y_`var')'\ `=scalar(scalar(e2m_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 1_yr 3_yrs 5_yrs
		}

matrix est_cs12 = (ests_flos_from1a_1_2m, ests_flos_from1a_1_lci_2m, ests_flos_from1a_1_uci_2m, ests_flos_from1b_1_2m, ests_flos_from1b_1_lci_2m, ests_flos_from1b_1_uci_2m)
matrix colnames est_cs12 = Out Out_lci Out_uci Res Res_lci Res_uci

esttab matrix(est_cs12) using "${pathdata2}los_1st_trans_ser23.html", replace 
<</dd_do>>
~~~~

The lengths of stay are presented here:

<<dd_include: "${pathdata2}los_1st_trans_ser23.html" >>

#### Length_of_stay_First patient_Second transition

~~~~
<<dd_do>>
foreach var of varlist flos_from2a_2_2m flos_from2a_2_lci_2m flos_from2a_2_uci_2m flos_from2b_2_2m flos_from2b_2_lci_2m flos_from2b_2_uci_2m {
	scalar variable = "`var'"
					qui summarize `var' if inrange(timevar0, .32, .34)
					scalar e3m_`var' = round(round(r(mean),.001),.1)
					qui summarize `var' if inrange(timevar0, .999, 1.02)
					scalar e1y_`var' = round(round(r(mean),.001),.1)
					qui summarize `var' if inrange(timevar0, 2.99, 3.02)
					scalar e2m_`var' = round(round(r(mean),.001),.1)
					qui summarize `var' if inrange(timevar0,  4.999, 5.02)
					scalar e5y_`var' = round(round(r(mean),.001),.1)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e1y_`var')'\ `=scalar(scalar(e2m_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 1_yr 3_yrs 5_yrs
		}

matrix est_cs22 = (ests_flos_from2a_2_2m, ests_flos_from2a_2_lci_2m, ests_flos_from2a_2_uci_2m, ests_flos_from2b_2_2m, ests_flos_from2b_2_lci_2m, ests_flos_from2b_2_uci_2m)
matrix colnames est_cs22 = Out Out_lci Out_uci Res Res_lci Res_uci

esttab matrix(est_cs22) using "${pathdata2}los_2nd_trans_ser23.html", replace 
<</dd_do>>
~~~~

The lengths of stay are presented here:

<<dd_include: "${pathdata2}los_2nd_trans_ser23.html" >>

~~~~
<<dd_do>>
foreach var of varlist flos_from3a_3_2m flos_from3a_3_lci_2m flos_from3a_3_uci_2m flos_from3b_3_2m flos_from3b_3_lci_2m flos_from3b_3_uci_2m {
	scalar variable = "`var'"
					qui summarize `var' if inrange(timevar0, .32, .34)
					scalar e3m_`var' = round(round(r(mean),.001),.1)
					qui summarize `var' if inrange(timevar0, .999, 1.02)
					scalar e1y_`var' = round(round(r(mean),.001),.1)
					qui summarize `var' if inrange(timevar0, 2.99, 3.02)
					scalar e2m_`var' = round(round(r(mean),.001),.1)
					qui summarize `var' if inrange(timevar0,  4.999, 5.02)
					scalar e5y_`var' = round(round(r(mean),.001),.1)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e1y_`var')'\ `=scalar(scalar(e2m_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 1_yr 3_yrs 5_yrs
		}

matrix est_cs22 = (ests_flos_from3a_3_2m, ests_flos_from3a_3_lci_2m, ests_flos_from3a_3_uci_2m, ests_flos_from3b_3_2m, ests_flos_from3b_3_lci_2m, ests_flos_from3b_3_uci_2m)
matrix colnames est_cs22 = Out Out_lci Out_uci Res Res_lci Res_uci

esttab matrix(est_cs32) using "${pathdata2}los_3rd_trans_ser23.html", replace 
<</dd_do>>
~~~~

The lengths of stay are presented here:

<<dd_include: "${pathdata2}los_3rd_trans_ser23.html" >>


### Difference_Probabilities

#### Difference_Probabilities_First_and_second_transition

~~~~
<<dd_do>>
*list fdiff_prob_from1_2_2m fdiff_prob_from1_2_lci_2m fdiff_prob_from1_2_uci_2m if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999,  1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02)

foreach var of varlist fdiff_prob_from1_2_2m  fdiff_prob_from1_2_lci_2m  fdiff_prob_from1_2_uci_2m fdiff_prob_from1_3_2m  fdiff_prob_from1_3_lci_2m  fdiff_prob_from1_3_uci_2m  {
				scalar variable = "`var'"
					qui summarize `var' if inrange(timevar01, .32, .34)
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(timevar01, .999, 1.02)
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(timevar01, 2.99, 3.02)
					scalar e2m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(timevar01,  4.999, 5.02)
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e1y_`var')'\ `=scalar(scalar(e2m_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 1_yr 3_yrs 5_yrs
 }

matrix est_diff11 = (ests_fdiff_prob_from1_2_2m, ests_fdiff_prob_from1_2_lci_2m, ests_fdiff_prob_from1_2_uci_2m, ests_fdiff_prob_from1_3_2m, ests_fdiff_prob_from1_3_lci_2m, ests_fdiff_prob_from1_3_uci_2m)
matrix colnames est_diff11 =  "Completion" "Completion_lo" "Completion_hi" "Non-completion" "Non-completion_lo" "Non-completion_hi" 

esttab matrix(est_diff11) using "${pathdata2}diff_probs_res_out_mod_t1_2m_22_corr3.html", replace 
<</dd_do>>
~~~~

The transition probabilities are presented here:

<<dd_include: "${pathdata2}diff_probs_res_out_mod_t1_2m_22_corr3.html" >>


#### Difference_Probabilities_First patient_Third_and_fourth_transition

~~~~
<<dd_do>>
foreach var of varlist fdiff_prob_from2_4_2m  fdiff_prob_from2_4_lci_2m  fdiff_prob_from2_4_uci_2m fdiff_prob_from3_5_2m  fdiff_prob_from3_5_lci_2m  fdiff_prob_from3_5_uci_2m  {
				scalar variable = "`var'"
					qui summarize `var' if inrange(timevar01, .32, .34)
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(timevar01, .999, 1.02)
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(timevar01, 2.99, 3.02)
					scalar e2m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if inrange(timevar01,  4.999, 5.02)
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e1y_`var')'\ `=scalar(scalar(e2m_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 1_yr 3_yrs 5_yrs
 }

matrix est_diff21 = (ests_fdiff_prob_from2_4_2m, ests_fdiff_prob_from2_4_lci_2m, ests_fdiff_prob_from2_4_uci_2m, ests_fdiff_prob_from3_5_2m, ests_fdiff_prob_from3_5_lci_2m, ests_fdiff_prob_from3_5_uci_2m)
matrix colnames est_diff21 =  "Completion" "Completion_lo" "Completion_hi" "Non-completion" "Non-completion_lo" "Non-completion_hi" 

esttab matrix(est_diff21) using "${pathdata2}diff_probs_res_out_mod_t2_2m_22_corr3.html", replace 
<</dd_do>>
~~~~

The transition probabilities are presented here:

<<dd_include: "${pathdata2}diff_probs_res_out_mod_t2_2m_22_corr3.html" >>


### Difference_Length_of_Stay


#### Difference_Length_of_Stay_from_Admission_to_Completion_to_Non-completion

~~~~
<<dd_do>>
foreach var of varlist fdiff_los_from1_2_2m fdiff_los_from1_2_lci_2m fdiff_los_from1_2_uci_2m fdiff_los_from1_3_2m fdiff_los_from1_3_lci_2m fdiff_los_from1_3_uci_2m {
		scalar variable = "`var'"
					qui summarize `var' if inrange(timevar01, .32, .34)
					scalar e3m_`var' = round(round(r(mean),.001)*1,.1)
					qui summarize `var' if inrange(timevar01, .999, 1.02)
					scalar e1y_`var' = round(round(r(mean),.001)*1,.1)
					qui summarize `var' if inrange(timevar01, 2.99, 3.02)
					scalar e2m_`var' = round(round(r(mean),.001)*1,.1)
					qui summarize `var' if inrange(timevar01,  4.999, 5.02)
					scalar e5y_`var' = round(round(r(mean),.001)*1,.1)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e1y_`var')'\ `=scalar(scalar(e2m_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 1_yr 3_yrs 5_yrs
		}
matrix est_diff12_l = (ests_fdiff_los_from1_2_2m, ests_fdiff_los_from1_2_lci_2m, ests_fdiff_los_from1_2_uci_2m, ests_fdiff_los_from1_3_2m, ests_fdiff_los_from1_3_lci_2m, ests_fdiff_los_from1_3_uci_2m)
matrix colnames est_diff12_l =  "to_Completion" "to_Completion_lo" "to_Completion_hi" "to_Non_Completion" "to_Non_Completion_lo" "to_Non_Completion_hi"

esttab matrix(est_diff12_l) using "${pathdata2}diff_los_res_out_mod_t1_2m_22_corr3.html", replace 
<</dd_do>>
~~~~

The lengths of stay are presented here:

<<dd_include: "${pathdata2}diff_los_res_out_mod_t1_2m_rp_22_corr.html" >>


#### Difference_Length_of_Stay_from_Second_and_third_state

~~~~
<<dd_do>>
foreach var of varlist fdiff_los_from2_2_2m fdiff_los_from2_2_lci_2m fdiff_los_from2_2_uci_2m fdiff_los_from3_3_2m fdiff_los_from3_3_lci_2m fdiff_los_from3_3_uci_2m {
		scalar variable = "`var'"
					qui summarize `var' if inrange(timevar01, .32, .34)
					scalar e3m_`var' = round(round(r(mean),.001)*1,.1)
					qui summarize `var' if inrange(timevar01, .999, 1.02)
					scalar e1y_`var' = round(round(r(mean),.001)*1,.1)
					qui summarize `var' if inrange(timevar01, 2.99, 3.02)
					scalar e2m_`var' = round(round(r(mean),.001)*1,.1)
					qui summarize `var' if inrange(timevar01,  4.999, 5.02)
					scalar e5y_`var' = round(round(r(mean),.001)*1,.1)
	 cap noi matrix ests_`var' = (`=scalar(e3m_`var')'\  `=scalar(e1y_`var')'\ `=scalar(scalar(e2m_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames ests_`var' = `var'
	 matrix rownames ests_`var' = 3_mths 1_yr 3_yrs 5_yrs
		}
matrix est_diff22_l = (ests_fdiff_los_from2_2_2m, ests_fdiff_los_from2_2_lci_2m, ests_fdiff_los_from2_2_uci_2m, ests_fdiff_los_from3_3_2m, ests_fdiff_los_from3_3_lci_2m, ests_fdiff_los_from3_3_uci_2m)
matrix colnames est_diff22_l =  "Completion" "Completion_lo" "Completion_hi" "Non-Completion" "Non-Completion_lo" "Non-Completion_hi" 

esttab matrix(est_diff22_l) using "${pathdata2}diff_los_res_out_mod_t2_2m_22_corr3.html", replace 
<</dd_do>>
~~~~

The lengths of stay are presented here:

<<dd_include: "${pathdata2}diff_los_res_out_mod_t2_2m_22_corr3.html" >>



## =============================================================================
## =============================================================================

#  Export tables

~~~~
<<dd_do>>
*https://www.statalist.org/forums/forum/general-stata-discussion/general/1594605-twoway-graph-serset


*####################################################################
*#  Probabilities (In the next state) (1st pat)
*###################################################################

local x `" "1a_2_3y"	"1a_2_lci_3y"	"1a_2_uci_3y"	"1b_2_3y"	"1b_2_lci_3y"	"1b_2_uci_3y""' 
local wx ""
foreach l of local x {
	local wx "`wx' fprob_from`l'"
	}
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("1pat_pr_corr3_3y", modify) cell(B1) firstrow(variables) //*save to Excel worksheetcell(start) cell(A1) 

local x `" "1a_3_3y"	"1a_3_lci_3y"	"1a_3_uci_3y"	"1b_3_3y"	"1b_3_lci_3y"	"1b_3_uci_3y""' 
local wx ""
foreach l of local x {
	local wx "`wx' fprob_from`l'"
	}
*di "`wx'"
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("1pat_pr_corr3_3y", modify) cell(B6) //*save to Excel worksheetcell(start)  cell(A6) 

local x `" "2a_4_3y"	"2a_4_lci_3y"	"2a_4_uci_3y"	"2b_4_3y"	"2b_4_lci_3y"	"2b_4_uci_3y""' 
local wx ""
foreach l of local x {
	local wx "`wx' fprob_from`l'"
	}
*di "`wx'"
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("1pat_pr_corr3_3y", modify) cell(B10) //*save to Excel worksheetcell(start)  cell(A6) 

local x `" "3a_5_3y"	"3a_5_lci_3y"	"3a_5_uci_3y"	"3b_5_3y"	"3b_5_lci_3y"	"3b_5_uci_3y""' 
local wx ""
foreach l of local x {
	local wx "`wx' fprob_from`l'"
	}
*di "`wx'"
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("1pat_pr_corr3_3y", modify) cell(B14) //*save to Excel worksheetcell(start)  cell(A6) 

local x `" "4a_6_3y"	"4a_6_lci_3y"	"4a_6_uci_3y"	"4b_6_3y"	"4b_6_lci_3y"	"4b_6_uci_3y""' 
local wx ""
foreach l of local x {
	local wx "`wx' fprob_from`l'"
	}
*di "`wx'"
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("1pat_pr_corr3_3y", modify) cell(B18) //*save to Excel worksheetcell(start)  cell(A6) 

local x `" "5a_7_3y"	"5a_7_lci_3y"	"5a_7_uci_3y"	"5b_7_3y"	"5b_7_lci_3y"	"5b_7_uci_3y""' 
local wx ""
foreach l of local x {
	local wx "`wx' fprob_from`l'"
	}
*di "`wx'"
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("1pat_pr_corr3_3y", modify) cell(B22) //*save to Excel worksheetcell(start)  cell(A6) 

local x `" "6a_8_3y"	"6a_8_lci_3y"	"6a_8_uci_3y"	"6b_8_3y"	"6b_8_lci_3y"	"6b_8_uci_3y""' 
local wx ""
foreach l of local x {
	local wx "`wx' fprob_from`l'"
	}
*di "`wx'"
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("1pat_pr_corr3_3y", modify) cell(B26) //*save to Excel worksheetcell(start)  cell(A6) 

local x `" "7a_9_3y"	"7a_9_lci_3y"	"7a_9_uci_3y"	"7b_9_3y"	"7b_9_lci_3y"	"7b_9_uci_3y""' 
local wx ""
foreach l of local x {
	local wx "`wx' fprob_from`l'"
	}
*di "`wx'"
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("1pat_pr_corr3_3y", modify) cell(B30) //*save to Excel worksheetcell(start)  cell(A6) 



*####################################################################
*#  Differences in Probabilities (In the next state)
*###################################################################
	
local x `""1_2_3y" "1_2_lci_3y" "1_2_uci_3y""' 
local wx ""
foreach l of local x {
	local wx "`wx' fdiff_prob_from`l'"
	}
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("diff_prob_corr3_3y", modify) cell(B1) firstrow(variables) //*save to Excel worksheetcell(start)  cell(A6) 

local x `" "1_3_3y" "1_3_lci_3y" "1_3_uci_3y" "' 
local wx ""
foreach l of local x {
	local wx "`wx' fdiff_prob_from`l'"
	}
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("diff_prob_corr3_3y", modify) cell(B6) //*save to Excel worksheetcell(start)  cell(A6) 

local x `" "2_4_3y" "2_4_lci_3y" "2_4_uci_3y" "' 
local wx ""
foreach l of local x {
	local wx "`wx' fdiff_prob_from`l'"
	}
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("diff_prob_corr3_3y", modify) cell(B10) //*save to Excel worksheetcell(start)  cell(A6) 

local x `" "3_5_3y" "3_5_lci_3y" "3_5_uci_3y" "' 
local wx ""
foreach l of local x {
	local wx "`wx' fdiff_prob_from`l'"
	}
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("diff_prob_corr3_3y", modify) cell(B14) //*save to Excel worksheetcell(start)  cell(A6) 

local x `" "4_6_3y" "4_6_lci_3y" "4_6_uci_3y" "' 
local wx ""
foreach l of local x {
	local wx "`wx' fdiff_prob_from`l'"
	}
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("diff_prob_corr3_3y", modify) cell(B18) //*save to Excel worksheetcell(start)  cell(A6) 

local x `" "5_7_3y" "5_7_lci_3y" "5_7_uci_3y" "' 
local wx ""
foreach l of local x {
	local wx "`wx' fdiff_prob_from`l'"
	}
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("diff_prob_corr3_3y", modify) cell(B22) //*save to Excel worksheetcell(start)  cell(A6) 

local x `" "6_8_3y" "6_8_lci_3y" "6_8_uci_3y" "' 
local wx ""
foreach l of local x {
	local wx "`wx' fdiff_prob_from`l'"
	}
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("diff_prob_corr3_3y", modify) cell(B26) //*save to Excel worksheetcell(start)  cell(A6) 

local x `" "7_9_3y" "7_9_lci_3y" "7_9_uci_3y" "' 
local wx ""
foreach l of local x {
	local wx "`wx' fdiff_prob_from`l'"
	}
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("diff_prob_corr3_3y", modify) cell(B30) //*save to Excel worksheetcell(start)  cell(A6) 


	
*####################################################################
*#  Length of Stay (In the actual state)
*###################################################################

local x `" "1a_1_3y"	"1a_1_lci_3y"	"1a_1_uci_3y"	"1b_1_3y"	"1b_1_lci_3y"	"1b_1_uci_3y""' 
local wx ""
foreach l of local x {
	local wx "`wx' flos_from`l'"
	}
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("1pat_los_act_st_corr3_3y", modify) cell(B1) firstrow(variables) //*save to Excel worksheetcell(start) cell(A1) 

local x `" "2a_2_3y"	"2a_2_lci_3y"	"2a_2_uci_3y"	"2b_2_3y"	"2b_2_lci_3y"	"2b_2_uci_3y""' 
local wx ""
foreach l of local x {
	local wx "`wx' flos_from`l'"
	}
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("1pat_los_act_st_corr3_3y", modify) cell(B6) //*save to Excel worksheetcell(start)  cell(A6) 

local x `" "3a_3_3y"	"3a_3_lci_3y"	"3a_3_uci_3y"	"3b_3_3y"	"3b_3_lci_3y"	"3b_3_uci_3y""' 
local wx ""
foreach l of local x {
	local wx "`wx' flos_from`l'"
	}
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("1pat_los_act_st_corr3_3y", modify) cell(B10) //*save to Excel worksheetcell(start)  cell(A6) 

local x `" "4a_4_3y"	"4a_4_lci_3y"	"4a_4_uci_3y"	"4b_4_3y"	"4b_4_lci_3y"	"4b_4_uci_3y""' 
local wx ""
foreach l of local x {
	local wx "`wx' flos_from`l'"
	}
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("1pat_los_act_st_corr3_3y", modify) cell(B14) //*save to Excel worksheetcell(start)  cell(A6) 

local x `" "5a_5_3y"	"5a_5_lci_3y"	"5a_5_uci_3y"	"5b_5_3y"	"5b_5_lci_3y"	"5b_5_uci_3y""' 
local wx ""
foreach l of local x {
	local wx "`wx' flos_from`l'"
	}
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("1pat_los_act_st_corr3_3y", modify) cell(B18) //*save to Excel worksheetcell(start)  cell(A6) 

local x `" "6a_6_3y"	"6a_6_lci_3y"	"6a_6_uci_3y"	"6b_6_3y"	"6b_6_lci_3y"	"6b_6_uci_3y""' 
local wx ""
foreach l of local x {
	local wx "`wx' flos_from`l'"
	}
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("1pat_los_act_st_corr3_3y", modify) cell(B22) //*save to Excel worksheetcell(start)  cell(A6) 

local x `" "7a_7_3y"	"7a_7_lci_3y"	"7a_7_uci_3y"	"7b_7_3y"	"7b_7_lci_3y"	"7b_7_uci_3y""' 
local wx ""
foreach l of local x {
	local wx "`wx' flos_from`l'"
	}
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("1pat_los_act_st_corr3_3y", modify) cell(B26) //*save to Excel worksheetcell(start)  cell(A6) 


*####################################################################
*#  Differences in Length of Stay (In the actual state)
*###################################################################

local x `""1_1_3y" "1_1_lci_3y" "1_1_uci_3y""' 
local wx ""
foreach l of local x {
	local wx "`wx' fdiff_los_from`l'"
	}
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("diff_los_corr3_3y", modify) cell(B1) firstrow(variables) //*save to Excel worksheetcell(start)  cell(A6) 

local x `" "2_2_3y" "2_2_lci_3y" "2_2_uci_3y" "' 
local wx ""
foreach l of local x {
	local wx "`wx' fdiff_los_from`l'"
	}
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("diff_los_corr3_3y", modify) cell(B6) //*save to Excel worksheetcell(start)  cell(A6) 

local x `" "3_3_3y" "3_3_lci_3y" "3_3_uci_3y" "' 
local wx ""
foreach l of local x {
	local wx "`wx' fdiff_los_from`l'"
	}
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("diff_los_corr3_3y", modify) cell(B10) //*save to Excel worksheetcell(start)  cell(A6) 

local x `" "4_4_3y" "4_4_lci_3y" "4_4_uci_3y" "' 
local wx ""
foreach l of local x {
	local wx "`wx' fdiff_los_from`l'"
	}
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("diff_los_corr3_3y", modify) cell(B14) //*save to Excel worksheetcell(start)  cell(A6) 

local x `" "5_5_3y" "5_5_lci_3y" "5_5_uci_3y" "' 
local wx ""
foreach l of local x {
	local wx "`wx' fdiff_los_from`l'"
	}
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("diff_los_corr3_3y", modify) cell(B18) //*save to Excel worksheetcell(start)  cell(A6) 

local x `" "6_6_3y" "6_6_lci_3y" "6_6_uci_3y" "' 
local wx ""
foreach l of local x {
	local wx "`wx' fdiff_los_from`l'"
	}
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("diff_los_corr3_3y", modify) cell(B22) //*save to Excel worksheetcell(start)  cell(A6) 

local x `" "7_7_3y" "7_7_lci_3y" "7_7_uci_3y" "' 
local wx ""
foreach l of local x {
	local wx "`wx' fdiff_los_from`l'"
	}
export excel timevar01  `wx' if inrange(timevar01 , .32, .34)|inrange(timevar01 , .999, 1.02)|inrange(timevar01 , 2.99, 3.02)|inrange(timevar01 , 4.999, 5.02) ///
using trans_prob_los_22_nov.xlsx, sheet("diff_los_corr3_3y", modify) cell(B26) //*save to Excel worksheetcell(start)  cell(A6) 


<</dd_do>>
~~~~
   
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