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

recode policonsumo (0=0 "No polysubstance use") (1=1 "Polysubstance use"), gen(poly)

lab var poly "Polysubstance use"

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
lab var age_at_censor_date "Age at censorship date (2019-11-13)" 

/*
label define country1 1 "Ukraine" 2 "Bulgaria" 3 "Georgia" 4 "Russia" 5 "Lithuania" 6 "Czech Republic" ///
7 "Hungary" 8 "Slovakia" 9 "Portugal" 10 "Croatia" 11 "Ireland" 12 "Estonia" 13 "France" 14 "Cyprus" ///
15 "Poland" 16 "Germany" 17 "Great Britain" 18 "Slovenia" 19 "Israel" 20 "Spain" 21 "Belgium" ///
22 "Netherlands" 23 "Switzerland" 24 "Sweden" 25 "Norway" 26 "Denmark" 27 "Finland" 
label values country country1
*/

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

// censorship
gen diff1a= age_at_censor_date-edad_al_ing_1 
// time to event
gen diff2b= age_offending_imp-edad_al_ing_1 
// conditional
gen diff2= cond(event==1, diff2b, diff1a) //age_offending_imp-edad_al_ing_1
// completion
gen diff1b= edad_al_egres_imp-edad_al_ing_1 // edad_al_egres_imp
gen comp= cond(strpos(motivodeegreso_mod_imp_rec,"Treatment completion"), 1, 0)
gen contact_js= event
// time to tr. completion
gen diff1 = cond(comp==1, diff1b, diff1a)

gen edad_al_egres_imp_rec = cond(comp==1, edad_al_egres_imp, age_at_censor_date)

lab var diff2 "Time to offending" 
lab var diff1 "Time to tr. completion" 
lab var edad_al_egres_imp_rec "Time to tr. completion (if not, censorship)" 

lab var contact_js "Contact with justice system status" 
lab var comp "Tr. completion status" 

drop comuna_residencia_cod_rec_str event 
drop diff1a diff1b diff2b

//id in numeric format
*encode hash_key, gen(id)
*gen id= encode(hash_key)
egen long id = group(hash_key)
lab var id "HASH (numeric)" 

//Age at admission, corrected when 0 days in treatment
gen edad_al_ing_1_mod = cond(diff1<0, edad_al_ing_1 -0.01,edad_al_ing_1)
lab var edad_al_ing_1_mod "Age at admission (corrected)" 

//Sort variables
order id hash_key edad_al_ing_1 edad_al_egres_imp age_offending_imp age_at_censor_date diff1 diff2 comp contact_js
/*
global covs_2_mod "i.policonsumo i.sus_prin_mod i.fr_sus_prin edad_al_ing_1 edad_ini_cons i.sex_enc i.esc_rec i.ten_viv i.dg_cie_10_rec i.sud_severity_icd10 n_off_vio n_off_acq n_off_sud i.clas porc_pobr i.macrozone"
global covs_3_mod "policonsumo sus_prin_mod fr_sus_prin edad_al_ing_1 edad_ini_cons sex_enc esc_rec ten_viv dg_cie_10_rec n_off_vio n_off_acq n_off_sud clas porc_pobr macrozone"
*/

//a string variable, so it is encoded
destring id_centro, replace

gen diff1c= cond(diff1<0.001, 0.01, diff1)

drop diff1

rename diff1c diff1
lab var diff1 "Time to tr. completion" 

//Sort variables
order id hash_key edad_al_ing_1 edad_al_egres_imp age_offending_imp age_at_censor_date diff1 diff2 comp contact_js
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

86% of patients showed complete data, and 2% had missing data due to tenure status of the household and age of onset of substance use.

=======================================
## Survival, descriptives & IPW
=======================================

**Time to tr. completion**

~~~~
<<dd_do>>
*comp contact_js
*stset edad_al_egres_imp, enter(edad_al_ing_1_mod) failure(comp==1) //*scale(12) 
cap drop  _st
cap drop _d
cap drop _t
cap drop _t0
cap drop _start

cap gen _start= 0
stset diff1, enter(_start) failure(comp==1) //*scale(12) 

stsum, by (poly)

<</dd_do>>
~~~~

We calculated the inverse probability weights

~~~~
<<dd_do>>
*https://www.statalist.org/forums/forum/general-stata-discussion/general/1600895-survival-model-stpm2-using-previously-estimated-weights
*pweights are Stata’s sampling weights—the inverse of the probability that the subject was chosen from the population.

stipw (logit poly sus_prin_mod fr_sus_prin edad_al_ing_1 edad_ini_cons sex_enc esc_rec ten_viv dg_cie_10_rec n_off_vio n_off_acq n_off_sud clas porc_pobr macrozone), distribution(weibull) ipwtype(stabilised) vce(robust) genweight(ipw_wgt) //*edad_al_ing_1 df(10)  ten_viv colinealidad
<</dd_do>>
~~~~

We calculated the incidence rate.

~~~~
<<dd_do>>
cap drop  _st
cap drop _d
cap drop _t
cap drop _t0
cap drop _start

cap gen _start= 0
stset diff1 [pw=ipw_wgt], enter(_start) failure(comp==1) //*scale(12) 

stsum, by (poly)
<</dd_do>>
~~~~

Given the possible errors pointed out by stata in the weights, we calculated the IPW manually

~~~~
<<dd_do>>
cap qui noi frame drop example_a
frame copy default example_a
frame example_a: logistic poly i.sus_prin_mod i.fr_sus_prin edad_al_ing_1 edad_ini_cons i.sex_enc i.esc_rec i.ten_viv i.dg_cie_10_rec i.n_off_vio i.n_off_acq i.n_off_sud i.clas porc_pobr i.macrozone, nolog 

frame example_a: predict double ps //ps = propensity score

frame example_a: gen double HAW = ((poly == 1) / ps) + ((poly == 0) / (1 - ps)) // 
*Compute the inverse probability Treatment weights (IPTW)
frame example_a: summarize HAW, detail
*keep if inrange(HAW, r(p05), r(p95))
frame example_a: keep if inrange(HAW, r(p1), r(p99)) //2023-01-08

frame example_a: cap drop  _st
frame example_a: cap drop _d
frame example_a: cap drop _t
frame example_a: cap drop _t0
frame example_a: cap drop _start

frame example_a: cap gen _start= 0
frame example_a: stset diff1 [pw=HAW], enter(_start) failure(comp==1) //*scale(12) 

frame example_a: stsum, by (poly)
~~~~
<<dd_do>>
frame example_a: stdescribe, weight
<</dd_do>>
~~~~

*###################
**Time to contact with the justice system**

We defined the incidence rates and weights for the time to contact with the justice system

~~~~
<<dd_do>>
*comp contact_js
*stset edad_al_egres_imp, enter(edad_al_ing_1_mod) failure(comp==1) //*scale(12) 
cap drop  _st
cap drop _d
cap drop _t
cap drop _t0
cap drop _start

cap gen _start= 0
stset diff2, enter(_start) failure(contact_js==1) //*scale(12) 

stsum, by (poly)

<</dd_do>>
~~~~

We then computed the data with weights

~~~~
<<dd_do>>
*https://www.statalist.org/forums/forum/general-stata-discussion/general/1600895-survival-model-stpm2-using-previously-estimated-weights
*pweights are Stata’s sampling weights—the inverse of the probability that the subject was chosen from the population.

stipw (logit poly sus_prin_mod fr_sus_prin edad_al_ing_1 edad_ini_cons sex_enc esc_rec ten_viv dg_cie_10_rec n_off_vio n_off_acq n_off_sud clas porc_pobr macrozone), distribution(weibull) ipwtype(stabilised) vce(robust) genweight(ipw_wgt2) //*edad_al_ing_1 df(10)  ten_viv
<</dd_do>>
~~~~

We calculated the incidence rate.

~~~~
<<dd_do>>
cap drop  _st
cap drop _d
cap drop _t
cap drop _t0
cap drop _start

cap gen _start= 0

stset diff2 [pw=ipw_wgt2], enter(_start) failure(contact_js==1) //*scale(12) 

stsum, by (poly)
<</dd_do>>
~~~~

~~~~
<<dd_do>>
stdescribe, weight
<</dd_do>>
~~~~

Given the possible errors pointed out by stata in the weights, we calculated the IPW manually


~~~~
<<dd_do>>
cap qui noi drop ps2
cap qui noi drop HAW2

cap qui noi frame drop example_b
frame copy default example_b
frame example_b: logistic poly i.sus_prin_mod i.fr_sus_prin edad_al_ing_1 edad_ini_cons i.sex_enc i.esc_rec i.ten_viv i.dg_cie_10_rec i.n_off_vio i.n_off_acq i.n_off_sud i.clas porc_pobr i.macrozone, nolog 

frame example_b: predict double ps2 //ps = propensity score

frame example_b: gen double HAW2 = ((poly == 1) / ps2) + ((poly == 0) / (1 - ps2)) // 
*Compute the inverse probability Treatment weights (IPTW)
frame example_b: summarize HAW2, detail
*keep if inrange(HAW, r(p05), r(p95))
frame example_b: keep if inrange(HAW2, r(p1), r(p99)) //2023-01-08

frame example_b: cap drop  _st
frame example_b: cap drop _d
frame example_b: cap drop _t
frame example_b: cap drop _t0
frame example_b: cap drop _start

frame example_b: cap gen _start= 0
frame example_b: stset diff2 [pw=HAW2], enter(_start) failure(contact_js==1) //*scale(12) 

frame example_b: stsum, by (poly)
<</dd_do>>
~~~~

~~~~
<<dd_do>>
frame example_b: stdescribe, weight
<</dd_do>>
~~~~


**##############################**
** GET WEIGHTED BALANCE TABLES**


~~~~
<<dd_do>>
logistic poly i.sus_prin_mod i.fr_sus_prin edad_al_ing_1 edad_ini_cons i.sex_enc i.esc_rec i.ten_viv i.dg_cie_10_rec i.n_off_vio i.n_off_acq i.n_off_sud i.clas porc_pobr i.macrozone, nolog 
predict double ps //ps = propensity score
gen double HAW = ((poly == 1) / ps) + ((poly == 0) / (1 - ps)) // 
*Compute the inverse probability Treatment weights (IPTW)
summarize HAW, detail
//ci mean HAW //2022-01-08 does not work well, many out
keep if inrange(HAW, r(p1), r(p99))
//keep if inrange(HAW, r(lb), r(ub))

cap drop  _st
cap drop _d
cap drop _t
cap drop _t0
cap drop _start

cap gen _start= 0
stset diff1 [pw=HAW], enter(_start) failure(comp==1) //*scale(12) 

stsum, by (poly)
<</dd_do>>
~~~~	

~~~~
<<dd_do>>

xi: pbalchk poly i.sus_prin_mod i.fr_sus_prin edad_al_ing_1 edad_ini_cons i.sex_enc i.esc_rec i.ten_viv i.dg_cie_10_rec i.n_off_vio i.n_off_acq i.n_off_sud i.clas porc_pobr i.macrozone, wt(HAW) p mahal sqrt xiprefix(string) graph xline(.15 -.15) //diag
graph save "Graph" "pbal2.gph", replace


// Change legends
gr_edit .legend.plotregion1.label[1].text = {}
gr_edit .legend.plotregion1.label[1].text.Arrpush Before Adj.
gr_edit .legend.plotregion1.label[2].text = {}
gr_edit .legend.plotregion1.label[2].text.Arrpush After Adj.

//change image background 
gr_edit style.editstyle boxstyle(shadestyle(color(gs16))) editcopy
gr_edit .yaxis1.style.editstyle majorstyle(tickstyle(textstyle(size(small)))) editcopy

//mod points in grayscale
gr_edit .plotregion1.plot1.style.editstyle marker(fillcolor(gs7%60)) editcopy
gr_edit .plotregion1.plot1.style.editstyle marker(linestyle(color(gs7%60))) editcopy
gr_edit .plotregion1.plot2.style.editstyle marker(fillcolor(gs3%60)) editcopy
gr_edit .plotregion1.plot2.style.editstyle marker(linestyle(color(gs3%60))) editcopy
// plot2 edits

graph save "Graph" "pbal2_mod.gph", replace
*You cannot give both f and p as options, only one.
*, strata(varname) wt(varname) f p mahal metric(matrix) diag sqrt xiprefix(string) graph xline(numlist) xlabel(numlist) nostandardize nocatstandardize sigrep ]
<</dd_do>>
~~~~

Cocaine paste base and cocaine hydrochloride were the only variables that were not well adjusted if resticted the sample to the 5% and 95%.

=======================================
## Multistate & IPW
=======================================

and transform the database in a long format	according to the specifications and the transition matrix.									
										
~~~~										
<<dd_do:nocommand>>	
cap drop  _st
cap drop _d
cap drop _t
cap drop _t0
cap drop _start
	
// https://reddooranalytics.se/2022/01/17/multistate-v4-4-0-semi-parametric-multi-state-modelling/
msset, id(id) states(comp contact_js) ///									
		times(diff1 diff2)  //* saqué tipo_de_plan_res_1 para que no se empiece a subdividir								

mat mat_obs_states = r(transmatrix)										
mat freq_trans = r(freqmatrix)   										
mat next_states = r(Nnextstates) 										

msboxes, transmatrix(mat_obs_states) id(id)                  ///
          xvalues(0.2 0.7 0.2)                   ///
          yvalues(0.7 0.7 0.2)                   ///
          statenames(Admission TC Contact)      ///
          boxheight(0.2) yrange(0.09 0.81) ysize(3)
*set graphics off

forvalues i = 1/3 {
	gr_edit .plotregion1.textbox`i'.style.editstyle size(small) editcopy
	gr_edit .plotregion1.textbox`i'.style.editstyle box_alignment(south) editcopy
	gr_edit .plotregion1.textbox`i'.style.editstyle vertical(bottom) editcopy
	gr_edit .plotregion1.textbox`i'.style.editstyle margin(bottom) editcopy
}

*mat li freq_trans
*file:///G:/Mi%20unidad/Alvacast/SISTRAT%202019%20(github)/_supp_mstates/stata/crowther2017%20(1).pdf										
gen _time = _stop - _start
lab var _time "Time to states (reset)" 

gen _start2 = edad_al_ing_1_mod + _start
gen _stop2 = edad_al_ing_1 + _stop

*age time
*stset age_offending_imp, fail(event ==1) enter(edad_al_egres_imp)

stset _stop [pw=ipw_wgt], enter(_start) failure(_status==1) //*scale(12) 
*list edad_al_ing_1 edad_al_egres_imp_rec age_offending_imp age_at_censor_date edad_al_egres_imp_rec _start _stop _stop2 if _stop2<9

// days in admission of 0
*browse if diff1<0

cap gen _time = _t	

*replace event=1 if !missing(sex)
range timevar0 0.3 5 90 //added before october 23 
<</dd_do>>										
~~~~


~~~~
<<dd_do>>
/*
=======================================
## IPW example
=======================================
*https://www.esmoopen.com/cms/10.1016/j.esmoop.2021.100363/attachment/276cb642-3654-4d16-ad04-7bf83cb6fb15/mmc1.pdf
logistic  motivodeegreso_mod_imp_rec2_inv edad_al_ing_1 edad_ini_cons i.sex_enc i.esc_rec i.sus_prin_mod i.fr_sus_prin i.comp_biosoc i.ten_viv i.dg_cie_10_rec i.sud_severity_icd10 i.policonsumo i.n_off_vio i.n_off_acq i.n_off_sud i.clas porc_pobr, nolog 
predict double ps //ps = propensity score
gen double HAW = ((motivodeegreso_mod_imp_rec2_inv == 1) / ps) + ((motivodeegreso_mod_imp_rec2_inv == 0) / (1 - ps)) // 
Compute the inverse probability Treatment weights (IPTW)
summarize HAW, detail
keep if inrange(HAW, r(p05), r(p95))
stset _stop [pw=HAW], enter(_start) failure(_status==1) scale(365.25)
*/
<</dd_do>>
~~~~



=============================================================================
## Aalen-Johanssen, Non-parametric transition probabilities
=============================================================================

Generated an Aalen-Johanssen estimator to obtain the transition probabilities of the data from the time 0 (from admission).
For this, we separated the transition probabilities according to the setting at baseline.

~~~~
<<dd_do>>
*http://fmwww.bc.edu/repec/bocode/m/msaj.ado
msaj, transmatrix(mat_obs_states) by(poly) ci
rename (P_AJ_*) (ajprob*)
<</dd_do>>
~~~~

To generate figures, we select the valid transitions


<<dd_do:quietly>>
	egen comb2 = concat(_from _to), punct(", ")
	cap gen comb_final = ""
		foreach i in "1, 2" "1, 3" "2, 3" {
		replace comb_final= " `i'"   if comb2== trim(" `i'") 
		}
	
encode comb_final, gen (comb_final_n)
label define mstatus 1 "Admission->TC" 2 "Admission->Contact_JS" ///
	3 "TC->Contact_JS" 

	label value comb_final_n mstatus

	cap label variable comb_final_n "Valid transitions"
	
twoway (rarea ajprob1_lci ajprob1_uci _t if poly==0 & _t <5, sort connect(stairstep stairstep) lcolor(gs8) color(gs8)) ///
	   (rarea ajprob1_lci ajprob1_uci _t if poly==1 & _t <5, sort connect(stairstep stairstep)lcolor(gs5)color(gs5)) ///
	   (line ajprob1 _t if poly==0 & _t <5, sort connect(stairstep stairstep) lcolor(gs8) color(gs8) lpattern("-")) ///
	   (line ajprob1 _t if poly==1 & _t <5, sort connect(stairstep stairstep) lcolor(gs3) lpattern("-")), ///
	 xtitle("Analysis time when record ends", size(small)) ///
	xlabel(0(.3)5, labsize(small)) ///	
	ylabel(0(.2)1, labsize(small)) ///	
	ytitle("Transition Probabilities", size(small)) scheme(sj) graphregion(color(white)) ///
	legend(pos(1) ring(0) col(1) symysize(zero) keygap(1) symxsize(large) order( 3 4) lab(3 "No polysubstance") lab(4 "Polysubstance") size(small)) ///
	note("{it:Note. Means and 95% CI's}",size(vsmall)) ///
	title(" Probability in State 1 (Admission) with confidence intervals", size(small)) name(msaj_1_23, replace) ///
	saving(msaj_1_23.gph, replace)
<</dd_do>>

<<dd_graph: saving(msaj_1_23.svg) width(800) replace>>

<<dd_do:quietly>>
twoway (rarea ajprob2_lci ajprob2_uci _t if poly==0 & _t <5, sort connect(stairstep stairstep) lcolor(gs8) color(gs8)) ///
	   (rarea ajprob2_lci ajprob2_uci _t if poly==1 & _t <5, sort connect(stairstep stairstep)lcolor(gs5) color(gs5)) ///
	   (line ajprob2 _t if poly==0 & _t <5, sort connect(stairstep stairstep) lcolor(gs8) color(gs8) lpattern("-")) ///
	   (line ajprob2 _t if poly==1 & _t <5, sort connect(stairstep stairstep) lcolor(gs3) lpattern("-")), ///
	 xtitle("Analysis time when record ends", size(small)) ///
	xlabel(0(.3)5, labsize(small)) ///	
	ylabel(0(.2)1, labsize(small)) ///	
	ytitle("Transition Probabilities", size(small)) scheme(sj) graphregion(color(white)) ///
	legend(pos(1) ring(0) col(1) symysize(zero) keygap(1) symxsize(large) order( 3 4) lab(3 "No polysubstance") lab(4 "Polysubstance") size(small)) ///
	note("{it:Note. Means and 95% CI's}",size(vsmall)) ///
	title(" Probability in State 2 (TC) with confidence intervals", size(small)) name(msaj_2_23, replace) ///
	saving(msaj_2_23.gph, replace)
<</dd_do>>

<<dd_graph: saving(msaj_2_23.svg) width(800) replace>>

<<dd_do:quietly>>
twoway (rarea ajprob3_lci ajprob3_uci _t if poly==0 & _t <5, sort connect(stairstep stairstep) lcolor(gs8) color(gs8)) ///
	   (rarea ajprob3_lci ajprob3_uci _t if poly==1 & _t <5, sort connect(stairstep stairstep)lcolor(gs5)color(gs5)) ///
	   (line ajprob3 _t if poly==0 & _t <5, sort connect(stairstep stairstep) lcolor(gs8) color(gs8) lpattern("-")) ///
	   (line ajprob3 _t if poly==1 & _t <5, sort connect(stairstep stairstep) lcolor(gs3) lpattern("-")), ///
	 xtitle("Analysis time when record ends", size(small)) ///
	xlabel(0(.3)5, labsize(small)) ///	
	ylabel(0(.2)1, labsize(small)) ///	
	ytitle("Transition Probabilities", size(small)) scheme(sj) graphregion(color(white)) ///
	legend(pos(1) ring(0) col(1) symysize(zero) keygap(1) symxsize(large) order( 3 4) lab(3 "No polysubstance") lab(4 "Polysubstance") size(small)) ///
	note("{it:Note. Means and 95% CI's}",size(vsmall)) ///
	title(" Probability in State 3 (TNC) with confidence intervals", size(small)) name(msaj_3_23, replace) ///
	saving(msaj_3_23.gph, replace) 
<</dd_do>>

<<dd_graph: saving(msaj_3_23.svg) width(800) replace>>


~~~~
<<dd_do:nocommand>>

cap noi drop trp_ajprob*
cap drop P_AJ*
 forvalues i = 1/2 {
	msaj, transmat(mat_obs_states) from(`i') ltruncated(.3) exit(5) by(poly) ci los
	rename (P_AJ_*) (trp_ajprob_3_5_`i'*)
	cap drop P_AJ*
 }

 *variable trp_ajprob* not found
<</dd_do>>
~~~~


<<dd_do:nocommand>>
*ADDRESS OVERLAY PROBLEM

cap noi drop _t2
cap clonevar _t2 = _t
replace _t2 = cond(poly==1, _t2 + .15, _t2 + 0)
gen _t3 = round(_t, 0.0001)
gen bar= 0 

cap drop ranges
gen ranges= 0
recode ranges 0=1 if inrange(_t, .295, .305) 
recode ranges 0=2 if _t ==1
recode ranges 0=3 if inrange(_t, 2.99, 3.01) 
recode ranges 0=4 if inrange(_t, 4.9, 5.1)
recode ranges 0=.

*===========
*CHANGE FRAME TO GET THE FIRST OBS BY COUNT

cap qui noi frame drop example1
frame copy default example1
frame example1: cap noi gen trp_ajprob_3_5_12_miss= cond(!missing(trp_ajprob_3_5_12),1,0)
*cap noi egen trp_ajprob_3_5_12_n=count(trp_ajprob_3_5_12) if !missing(trp_ajprob_3_5_12), by(ranges poly)
frame example1: sort trp_ajprob_3_5_12_miss ranges poly _t
frame example1: cap quietly noi by trp_ajprob_3_5_12_miss ranges poly:  gen dup2 = cond(_N==1,0,_n)


/*
*TO CHECK HOW MANY COUNTS THERE ARE FOR EVERY COMBINATION
cap qui noi frame drop example
frame copy default example
frame change default
cap qui noi frame change example
cap qui noi statsby p025=r(lb) p975=r(ub) mean=r(mean) n=r(N), by(ranges poly) subsets clear: ci mean trp_ajprob_3_5_12 trp_ajprob_3_5_12_lci trp_ajprob_3_5_12_uci
cap browse
*/

/*
graph bar (mean) trp_ajprob_3_5_12, over(ranges) by(poly) asyvars percent showyvars bargap(20) ///
  ytitle("Proportions") legend(off) scheme(s1mono) ///
  ylabel(0(10)100, labsize(small)) ///	
  ytitle("Transition Probabilities", size(small)) graphregion(color(white)) ///
  legend(pos(2) ring(0) col(1) symysize(zero) keygap(1) symxsize(large) order(1 2) lab(1 "No Polysubs") lab(2 "Polysubs") size(small)) ///
  note("{it:Note. Means }",size(vsmall)) ///
	name(msaj_12_23, replace) ///
	saving(msaj_12_23.gph, replace)
 */
 
frame example1: twoway (rbar bar trp_ajprob_3_5_12 _t2 if poly==0 & dup2==1 & _t2<5.17, color(gs3%70) lcolor(black%0) barw(.3)) ///   
	(rbar bar trp_ajprob_3_5_12 _t2 if poly==1 & dup2==1 & _t2<5.17,  color(gs7%70) lcolor(gs7%0) barw(.3)) ///
	(rcap trp_ajprob_3_5_12_lci trp_ajprob_3_5_12_uci _t2 if poly==0 & dup2==1 & _t2<5.17, color(gs3%70)) ///
	(rcap trp_ajprob_3_5_12_lci trp_ajprob_3_5_12_uci _t2 if poly==1 & dup2==1 & _t2<5.17, color(gs7%70)), /// //**0.8
	xtitle("Time since entering the study", size(small)) ///
	xlabel(0(.3)5, labsize(small)) ///	
	ylabel(0(.10)1, labsize(small)) ///	
	ytitle("Transition Probabilities", size(small)) scheme(sj) graphregion(color(white)) ///
	legend(pos(2) ring(0) col(1) symysize(zero) keygap(1) symxsize(large) order(3 4) lab(3 "No Polysubs") lab(4 "Polysubs") size(small) lcolor(gs4 gs7)) ///
	note("{it:Note. Means and 95% CI's}",size(vsmall)) ///
	title("Transition Probabilities from Admission to Treatment completion with CIs", size(small)) name(msaj_12_23, replace) ///
	saving(msaj_12_23.gph, replace)
<</dd_do>>

<<dd_graph: saving(msaj_12_23.svg) width(800) replace>>


<<dd_do:nocommand>>
cap qui noi frame drop example2
frame copy default example2
frame example2: cap noi gen trp_ajprob_3_5_13_miss= cond(!missing(trp_ajprob_3_5_13),1,0)
*cap noi egen trp_ajprob_3_5_12_n=count(trp_ajprob_3_5_12) if !missing(trp_ajprob_3_5_12), by(ranges poly)
frame example2: sort trp_ajprob_3_5_13_miss ranges poly _t
frame example2: cap quietly noi by trp_ajprob_3_5_13_miss ranges poly:  gen dup2 = cond(_N==1,0,_n)

frame example2: twoway (rbar bar trp_ajprob_3_5_13 _t2 if poly==0 & dup2==1 & _t2<5.17, color(gs3%70) lcolor(black%0) barw(.3)) ///   
	(rbar bar trp_ajprob_3_5_13 _t2 if poly==1 & dup2==1 & _t2<5.17,  color(gs7%70) lcolor(gs7%0) barw(.3)) ///
	(rcap trp_ajprob_3_5_13_lci trp_ajprob_3_5_13_uci _t2 if poly==0 & dup2==1 & _t2<5.17, color(gs3%70)) ///
	(rcap trp_ajprob_3_5_13_lci trp_ajprob_3_5_13_uci _t2 if poly==1 & dup2==1 & _t2<5.17, color(gs7%70)), /// //**0.8
	xtitle("Time since entering the study", size(small)) ///
	xlabel(0(.3)5, labsize(small)) ///	
	ylabel(0(.10)1, labsize(small)) ///	
	ytitle("Transition Probabilities", size(small)) scheme(sj) graphregion(color(white)) ///
	legend(pos(2) ring(0) col(1) symysize(zero) keygap(1) symxsize(large) order(3 4) lab(3 "No Polysubs") lab(4 "Polysubs") size(small) lcolor(gs4 gs7)) ///
	note("{it:Note. Means and 95% CI's}",size(vsmall)) ///
	title("Transition Probabilities from Admission to Contact with the justice system with CIs", size(small)) name(msaj_13_23, replace) ///
	saving(msaj_13_23.gph, replace)
<</dd_do>>

<<dd_graph: saving(msaj_13_23.svg) width(800) replace>>

<<dd_do:nocommand>>
cap qui noi frame drop example3
frame copy default example3
frame example3: cap noi gen trp_ajprob_3_5_23_miss= cond(!missing(trp_ajprob_3_5_23),1,0)
*cap noi egen trp_ajprob_3_5_12_n=count(trp_ajprob_3_5_12) if !missing(trp_ajprob_3_5_12), by(ranges poly)
frame example3: sort trp_ajprob_3_5_23_miss ranges poly _t
frame example3: cap quietly noi by trp_ajprob_3_5_23_miss ranges poly:  gen dup2 = cond(_N==1,0,_n)


frame example2: twoway (rbar bar trp_ajprob_3_5_23 _t2 if poly==0 & dup2==1 & _t2<5.17, color(gs3%70) lcolor(black%0) barw(.3)) ///   
	(rbar bar trp_ajprob_3_5_23 _t2 if poly==1 & dup2==1 & _t2<5.17,  color(gs7%70) lcolor(gs7%0) barw(.3)) ///
	(rcap trp_ajprob_3_5_23_lci trp_ajprob_3_5_23_uci _t2 if poly==0 & dup2==1 & _t2<5.17, color(gs3%70)) ///
	(rcap trp_ajprob_3_5_23_lci trp_ajprob_3_5_23_uci _t2 if poly==1 & dup2==1 & _t2<5.17, color(gs7%70)), /// //**0.8
	xtitle("Time since entering the study", size(small)) ///
	xlabel(0(.3)5, labsize(small)) ///	
	ylabel(0(.10)1, labsize(small)) ///	
	ytitle("Transition Probabilities", size(small)) scheme(sj) graphregion(color(white)) ///
	legend(pos(2) ring(0) col(1) symysize(zero) keygap(1) symxsize(large) order(3 4) lab(3 "No Polysubs") lab(4 "Polysubs") size(small) lcolor(gs4 gs7)) ///
	note("{it:Note. Means and 95% CI's}",size(vsmall)) ///
	title("Transition Probabilities from Treatment completion to Contact with the justice system with CIs", size(small)) name(msaj_23_23, replace) ///
	saving(msaj_23_23.gph, replace)

<</dd_do>>

<<dd_graph: saving(msaj_23_23.svg) width(800) replace>>



## Aalen-Johanssen, Probability Tables

**Transition Probabilities from Admission to Treatment completion**

~~~~
<<dd_do>>
foreach var of varlist trp_ajprob_3_5_12 trp_ajprob_3_5_12_lci trp_ajprob_3_5_12_uci {
				scalar variable = "`var'"
					qui summarize `var' if ranges==1 & poly==0 
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if ranges==2 & poly==0 
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if ranges==3 & poly==0 
					scalar e3y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if ranges==4  & poly==0 
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix e_a_`var' = (`=scalar(e3m_`var')'\  `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames e_a_`var' = `var'
	 matrix rownames e_a_`var' = 3_mths 1_yr 3_yrs 5_yrs
					qui summarize `var' if ranges==1  & poly==1 
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if ranges==2  & poly==1
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if ranges==3  & poly==1
					scalar e3y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if ranges==4  & poly==1
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix e_b_`var' = (`=scalar(e3m_`var')'\  `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames e_b_`var' = `var'
	 matrix rownames e_b_`var' = 3_mths 1_yr 3_yrs 5_yrs
 }

matrix est_msaj12 = (e_a_trp_ajprob_3_5_12, e_a_trp_ajprob_3_5_12_lci, e_a_trp_ajprob_3_5_12_uci, e_b_trp_ajprob_3_5_12, e_b_trp_ajprob_3_5_12_lci, e_b_trp_ajprob_3_5_12_uci)
matrix colnames est_msaj12 = Est_NoPoly LCI UCI Est_Poly LCI UCI

esttab matrix(est_msaj12) using "${pathdata2}pr_msaj12_23.html", replace 
<</dd_do>>
~~~~

The transition probabilities are presented here:

<<dd_include: "${pathdata2}pr_msaj12_23.html" >>

**Transition Probabilities from Admission to Contact with the justice system**

~~~~
<<dd_do>>
foreach var of varlist trp_ajprob_3_5_13 trp_ajprob_3_5_13_lci trp_ajprob_3_5_13_uci {
				scalar variable = "`var'"
					qui summarize `var' if ranges==1 & poly==0 
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if ranges==2 & poly==0 
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if ranges==3 & poly==0 
					scalar e3y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if ranges==4  & poly==0 
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix e_a_`var' = (`=scalar(e3m_`var')'\  `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames e_a_`var' = `var'
	 matrix rownames e_a_`var' = 3_mths 1_yr 3_yrs 5_yrs
					qui summarize `var' if ranges==1  & poly==1 
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if ranges==2  & poly==1
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if ranges==3  & poly==1
					scalar e3y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if ranges==4  & poly==1
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix e_b_`var' = (`=scalar(e3m_`var')'\  `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames e_b_`var' = `var'
	 matrix rownames e_b_`var' = 3_mths 1_yr 3_yrs 5_yrs
 }

matrix est_msaj13 = (e_a_trp_ajprob_3_5_13, e_a_trp_ajprob_3_5_13_lci, e_a_trp_ajprob_3_5_13_uci, e_b_trp_ajprob_3_5_13, e_b_trp_ajprob_3_5_13_lci, e_b_trp_ajprob_3_5_13_uci)
matrix colnames est_msaj13 = Est_NoPoly LCI UCI Est_Poly LCI UCI

esttab matrix(est_msaj13) using "${pathdata2}pr_msaj13_23.html", replace 
<</dd_do>>
~~~~

The transition probabilities are presented here:

<<dd_include: "${pathdata2}pr_msaj13_23.html" >>

**Transition Probabilities from Treatment Completion to Contact with the justice system**

~~~~
<<dd_do>>
foreach var of varlist trp_ajprob_3_5_23 trp_ajprob_3_5_23_lci trp_ajprob_3_5_23_uci {
				scalar variable = "`var'"
					qui summarize `var' if ranges==1 & poly==0 
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if ranges==2 & poly==0 
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if ranges==3 & poly==0 
					scalar e3y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if ranges==4  & poly==0 
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix e_a_`var' = (`=scalar(e3m_`var')'\  `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames e_a_`var' = `var'
	 matrix rownames e_a_`var' = 3_mths 1_yr 3_yrs 5_yrs
					qui summarize `var' if ranges==1  & poly==1 
					scalar e3m_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if ranges==2  & poly==1
					scalar e1y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if ranges==3  & poly==1
					scalar e3y_`var' = round(round(r(mean),.001)*100,.1)
					qui summarize `var' if ranges==4  & poly==1
					scalar e5y_`var' = round(round(r(mean),.001)*100,.1)
	 cap noi matrix e_b_`var' = (`=scalar(e3m_`var')'\  `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames e_b_`var' = `var'
	 matrix rownames e_b_`var' = 3_mths 1_yr 3_yrs 5_yrs
 }

matrix est_msaj23 = (e_a_trp_ajprob_3_5_23, e_a_trp_ajprob_3_5_23_lci, e_a_trp_ajprob_3_5_23_uci, e_b_trp_ajprob_3_5_23, e_b_trp_ajprob_3_5_23_lci, e_b_trp_ajprob_3_5_23_uci)
matrix colnames est_msaj23 = Est_NoPoly LCI UCI Est_Poly LCI UCI

esttab matrix(est_msaj23) using "${pathdata2}pr_msaj23_23.html", replace 
<</dd_do>>
~~~~

The transition probabilities are presented here:

<<dd_include: "${pathdata2}pr_msaj23_23.html" >>


## Aalen-Johanssen, Tables of Lengths of stay

**Lengths of stay in Admission (does not give CIs)**

~~~~
<<dd_do>>
* inrange(_t, .299, .301) | _t==1 | inrange(_t, 2.99, 3.01) | inrange(_t, 4.970, 5.001) 
foreach var of varlist LOS_AJ_1 LOS_AJ_2 LOS_AJ_3 {
				scalar variable = "`var'"
					qui summarize `var' if inrange(_t, .299, .301) & poly==0 
					scalar e3m_`var' = round(round(r(mean),.001),.1)
					qui summarize `var' if inrange(_t, 1, 1) & poly==0 
					scalar e1y_`var' = round(round(r(mean),.001),.1)
					qui summarize `var' if inrange(_t, 2.99, 3.01) & poly==0 
					scalar e3y_`var' = round(round(r(mean),.001),.1)
					qui summarize `var' if inrange(_t,  4.97, 5.001) & poly==0 
					scalar e5y_`var' = round(round(r(mean),.001),.1)
	 cap noi matrix e_a_`var' = (`=scalar(e3m_`var')'\  `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames e_a_`var' = `var'
	 matrix rownames e_a_`var' = 3_mths 1_yr 3_yrs 5_yrs
					qui summarize `var' if inrange(_t, .299, .301) & poly==1 
					scalar e3m_`var' = round(round(r(mean),.001),.1)
					qui summarize `var' if inrange(_t, 1, 1) & poly==1
					scalar e1y_`var' = round(round(r(mean),.001),.1)
					qui summarize `var' if inrange(_t, 2.99, 3.01) & poly==1
					scalar e3y_`var' = round(round(r(mean),.001),.1)
					qui summarize `var' if inrange(_t,  4.97, 5.001) & poly==1
					scalar e5y_`var' = round(round(r(mean),.001),.1)
	 cap noi matrix e_b_`var' = (`=scalar(e3m_`var')'\  `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames e_b_`var' = `var'
	 matrix rownames e_b_`var' = 3_mths 1_yr 3_yrs 5_yrs
 }

matrix est_msaj12los = (e_a_LOS_AJ_1, e_b_LOS_AJ_1, e_a_LOS_AJ_2, e_b_LOS_AJ_2, e_a_LOS_AJ_3, e_b_LOS_AJ_3)
matrix colnames est_msaj12los = LOS1_NoPoly LOS2_NoPoly LOS3_NoPoly LOS1_Poly LOS2_Poly LOS3_Poly 

esttab matrix(est_msaj12los) using "${pathdata2}los_msaj12_23.html", replace 
<</dd_do>>
~~~~

The lengths of stay are presented here:

<<dd_include: "${pathdata2}los_msaj12_23.html" >>


=============================================================================
## Parametric Models
=============================================================================


### Intercept-only

We generated a list of parametric and intercept-only survival models with different distributions (Exponential, Weibull, Gompertz, Log-logistic, Log-normal & Generalized gamma. Aditionally, we defined a series of Royston-Parmar models with a function of restricted cubic splines, in which the knots (#df -1) are defined in each percentile of the distribution. We saved the estimates in the file called `parmodels_m_23'.

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

Posteriorly, we compared models by adding the setting at baseline, with the outcome of previous treatments (lagged treatment completion, in 0/1 format) as users pass through each transition. Equally, we added the time of arrival as a covariate to each transition (excluding the first). We saved the estimates in `parmodels_m2_jun_22_corr3_cc'.

~~~~
<<dd_do>>
global covs_2 "i.poly i.sus_prin_mod i.fr_sus_prin edad_al_ing_1 edad_ini_cons i.sex_enc i.esc_rec i.ten_viv i.dg_cie_10_rec i.sud_severity_icd10 n_off_vio n_off_acq n_off_sud i.clas porc_pobr i.macrozone"
global covs_3 "poly sus_prin_mod fr_sus_prin edad_al_ing_1 edad_ini_cons sex_enc esc_rec ten_viv dg_cie_10_rec n_off_vio n_off_acq n_off_sud clas porc_pobr macrozone"

local stname `" "poly " "poly " "poly _start" "' 
forvalues i = 1/3 {
	gettoken covs stname : stname 
	// Cox w/tvc
	forvalues j=1/7 {
		di in yellow "{bf: ***********}"
		di in yellow "{bf: Transition `i': family Cox tvc `j'}"
		di in yellow "{bf: ***********}"
		set seed 2125
		qui cap noi stmerlin `covs' if _trans==`i', dist(cox) tvc(poly) dftvc(`j')
		estimates store m2_`i'_cox`j'	
	}
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

qui estread "${pathdata2}parmodels_m_illness_death_23_2.sters"

local labs `" "Est" "Cons" "Par1" "Par2" "Par3" "Time in days""' 
esttab m2_1_rp10 m2_2_rp5 m2_3_rp3 using tab1_ser_23.html, /// transform(1 exp(@) @ @ @) ///
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

qui estread "${pathdata2}parmodels_m2_jun_22_corr3.sters"


predictms, transmatrix(mat_nine_states) models(m2_1_gom m2_2_gom m2_3_logn m2_4_ggam m2_5_logn m2_6_ggam m2_7_ggam m2_8_ggam) ///
	timevar(timevar0) /// mint(0) maxt(1826) timevar() cannot be specified with mint()/maxt()/obs()
	seed(2125) n($sim) prob los diff ratio /// * si agrego bootstrap, at#() limit reached, or unrecognised option
	from(1) ci at1(tipo_de_plan_res_1 0  _start 731) at2(tipo_de_plan_res_1 1  _start 731) reset bootstrap m($boots)   //* es una forma de hacer bootstrap 

<</dd_do>>
~~~~

~~~~
<<dd_do>>
rename (_prob_at1_1_*lci) (fprob_from1a_*lci_3y)
rename (_prob_at1_1_*uci) (fprob_from1a_*uci_3y)
rename (_prob_at1_1_*) (fprob_from1a_*_3y)

rename (_los_at1_1_*lci) (flos_from1a_*lci_3y)
rename (_los_at1_1_*uci) (flos_from1a_*uci_3y)
rename (_los_at1_1_*) (flos_from1a_*_3y)

rename (_prob_at2_1_*lci) (fprob_from1b_*lci_3y)
rename (_prob_at2_1_*uci) (fprob_from1b_*uci_3y)
rename (_prob_at2_1_*) (fprob_from1b_*_3y)

rename (_los_at2_1_*lci) (flos_from1b_*lci_3y)
rename (_los_at2_1_*uci) (flos_from1b_*uci_3y)
rename (_los_at2_1_*) (flos_from1b_*_3y)

rename (_ratio_prob_at2_1_*_lci) (fratio_prob_from1_*_lci_3y)
rename (_ratio_prob_at2_1_*_uci) (fratio_prob_from1_*_uci_3y)
rename (_ratio_prob_at2_1_*) (fratio_prob_from1_*_3y)
rename (_diff_prob_at2_1_*_lci) (fdiff_prob_from1_*_lci_3y)
rename (_diff_prob_at2_1_*_uci) (fdiff_prob_from1_*_uci_3y)
rename (_diff_prob_at2_1_*) (fdiff_prob_from1_*_3y)  

rename (_ratio_los_at2_1_*_lci) (fratio_los_from1_*_lci_3y)
rename (_ratio_los_at2_1_*_uci) (fratio_los_from1_*_uci_3y)
rename (_ratio_los_at2_1_*) (fratio_los_from1_*_3y)
rename (_diff_los_at2_1_*_lci) (fdiff_los_from1_*_lci_3y)
rename (_diff_los_at2_1_*_uci) (fdiff_los_from1_*_uci_3y)
rename (_diff_los_at2_1_*) (fdiff_los_from1_*_3y)  

qui estread "${pathdata2}parmodels_m2_jun_22_corr3.sters"

predictms, transmatrix(mat_nine_states) models(m2_1_gom m2_2_gom m2_3_logn m2_4_ggam m2_5_logn m2_6_ggam m2_7_ggam m2_8_ggam) ///
	timevar(timevar0) /// mint(0) maxt(1826) timevar() cannot be specified with mint()/maxt()/obs()
	seed(2125) n($sim) prob los diff ratio /// * si agrego bootstrap, at#() limit reached, or unrecognised option
	from(2) ci at1(tipo_de_plan_res_1 0  _start 731) at2(tipo_de_plan_res_1 1  _start 731) reset bootstrap m($boots)   //* es una forma de hacer bootstrap 
<</dd_do>>
~~~~

~~~~
<<dd_do>>
rename (_prob_at1_2_*lci) (fprob_from2a_*lci_3y)
rename (_prob_at1_2_*uci) (fprob_from2a_*uci_3y)
rename (_prob_at1_2_*) (fprob_from2a_*_3y)

rename (_los_at1_2_*lci) (flos_from2a_*lci_3y)
rename (_los_at1_2_*uci) (flos_from2a_*uci_3y)
rename (_los_at1_2_*) (flos_from2a_*_3y)

rename (_prob_at2_2_*lci) (fprob_from2b_*lci_3y)
rename (_prob_at2_2_*uci) (fprob_from2b_*uci_3y)
rename (_prob_at2_2_*) (fprob_from2b_*_3y)

rename (_los_at2_2_*lci) (flos_from2b_*lci_3y)
rename (_los_at2_2_*uci) (flos_from2b_*uci_3y)
rename (_los_at2_2_*) (flos_from2b_*_3y)

rename (_ratio_prob_at2_2_*_lci) (fratio_prob_from2_*_lci_3y)
rename (_ratio_prob_at2_2_*_uci) (fratio_prob_from2_*_uci_3y)
rename (_ratio_prob_at2_2_*) (fratio_prob_from2_*_3y)
rename (_diff_prob_at2_2_*_lci) (fdiff_prob_from2_*_lci_3y)
rename (_diff_prob_at2_2_*_uci) (fdiff_prob_from2_*_uci_3y)
rename (_diff_prob_at2_2_*) (fdiff_prob_from2_*_3y)  

rename (_ratio_los_at2_2_*_lci) (fratio_los_from2_*_lci_3y)
rename (_ratio_los_at2_2_*_uci) (fratio_los_from2_*_uci_3y)
rename (_ratio_los_at2_2_*) (fratio_los_from2_*_3y)
rename (_diff_los_at2_2_*_lci) (fdiff_los_from2_*_lci_3y)
rename (_diff_los_at2_2_*_uci) (fdiff_los_from2_*_uci_3y)
rename (_diff_los_at2_2_*) (fdiff_los_from2_*_3y) 

qui estread "${pathdata2}parmodels_m2_jun_22_corr3.sters"

predictms, transmatrix(mat_nine_states) models(m2_1_gom m2_2_gom m2_3_logn m2_4_ggam m2_5_logn m2_6_ggam m2_7_ggam m2_8_ggam) ///
	timevar(timevar0) /// mint(0) maxt(1826) timevar() cannot be specified with mint()/maxt()/obs()
	seed(2125) n($sim) prob los diff ratio /// * si agrego bootstrap, at#() limit reached, or unrecognised option
	from(3) ci at1(tipo_de_plan_res_1 0  _start 731) at2(tipo_de_plan_res_1 1  _start 731) reset bootstrap m($boots)   //* es una forma de hacer bootstrap 

<</dd_do>>
~~~~

~~~~
<<dd_do>>
rename (_prob_at1_3_*lci) (fprob_from3a_*lci_3y)
rename (_prob_at1_3_*uci) (fprob_from3a_*uci_3y)
rename (_prob_at1_3_*) (fprob_from3a_*_3y)

rename (_los_at1_3_*lci) (flos_from3a_*lci_3y)
rename (_los_at1_3_*uci) (flos_from3a_*uci_3y)
rename (_los_at1_3_*) (flos_from3a_*_3y)

rename (_prob_at2_3_*lci) (fprob_from3b_*lci_3y)
rename (_prob_at2_3_*uci) (fprob_from3b_*uci_3y)
rename (_prob_at2_3_*) (fprob_from3b_*_3y)

rename (_los_at2_3_*lci) (flos_from3b_*lci_3y)
rename (_los_at2_3_*uci) (flos_from3b_*uci_3y)
rename (_los_at2_3_*) (flos_from3b_*_3y)

rename (_ratio_prob_at2_3_*_lci) (fratio_prob_from3_*_lci_3y)
rename (_ratio_prob_at2_3_*_uci) (fratio_prob_from3_*_uci_3y)
rename (_ratio_prob_at2_3_*) (fratio_prob_from3_*_3y)
rename (_diff_prob_at2_3_*_lci) (fdiff_prob_from3_*_lci_3y)
rename (_diff_prob_at2_3_*_uci) (fdiff_prob_from3_*_uci_3y)
rename (_diff_prob_at2_3_*) (fdiff_prob_from3_*_3y)  

rename (_ratio_los_at2_3_*_lci) (fratio_los_from3_*_lci_3y)
rename (_ratio_los_at2_3_*_uci) (fratio_los_from3_*_uci_3y)
rename (_ratio_los_at2_3_*) (fratio_los_from3_*_3y)
rename (_diff_los_at2_3_*_lci) (fdiff_los_from3_*_lci_3y)
rename (_diff_los_at2_3_*_uci) (fdiff_los_from3_*_uci_3y)
rename (_diff_los_at2_3_*) (fdiff_los_from3_*_3y) 

<</dd_do>>