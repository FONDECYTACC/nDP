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
	cap noi net install pbalchk from("http://personalpages.manchester.ac.uk/staff/mark.lunt/")
	}
cap noi which matselrc
if _rc==111 {
	ssc install matselrc
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
*https://www.statalist.org/forums/forum/general-stata-discussion/general/1635683-stata-stir-command-with-pweights
<</dd_do>>
~~~~

~~~~
<<dd_do>>
poisson _d i.poly , irr exposure(_t) vce(rob)
matrix pois_irr_t0_nowgt= r(table)
*matselrc pois_ir_t0_nowgt pois_ir_t0_nowgt, c(1/1) r(1, 5/6)

margins i.poly, predict(ir)
matrix pois_ir_t0_nowgt= r(table)
*matselrc pois_ir_t0_nowgt pois_ir_t0_nowgt, c(1/2) r(1, 5/6)
local ir_poly_0 : di %6.2f pois_ir_t0_nowgt[1,1]
local ir_poly_0_lo : di %6.2f pois_ir_t0_nowgt[2,1]
local ir_poly_0_up : di %6.2f pois_ir_t0_nowgt[3,1]
*"`=scalar(round(pois_ir_t0_nowgt[1,1]*1000,.01))' (95%CI: `=scalar(round(pois_ir_t0_nowgt[2,1]*1000,.01))',`=scalar(round(pois_ir_t0_nowgt[3,1]*1000,.01))')"
scalar ir_poly_00 = "`=scalar(round(pois_ir_t0_nowgt[1,1]*1000,.01))' (95%CI: `=scalar(round(pois_ir_t0_nowgt[5,1]*1000,.01))',`=scalar(round(pois_ir_t0_nowgt[6,1]*1000,.01))')"
local ir_poly_1 : di %6.2f pois_ir_t0_nowgt[1,2]
local ir_poly_1_lo : di %6.2f pois_ir_t0_nowgt[2,2]
local ir_poly_1_up : di %6.2f pois_ir_t0_nowgt[3,2]
*"`=scalar(round(pois_ir_t0_nowgt[1,2]*1000,.01))' (95%CI: `=scalar(round(pois_ir_t0_nowgt[2,2]*1000,.01))',`=scalar(round(pois_ir_t0_nowgt[3,2]*1000,.01))')"
scalar ir_poly_11 = "`=scalar(round(pois_ir_t0_nowgt[1,2]*1000,.01))' (95%CI: `=scalar(round(pois_ir_t0_nowgt[5,2]*1000,.01))',`=scalar(round(pois_ir_t0_nowgt[6,2]*1000,.01))')"

local irr_poly_1 : di %6.2f pois_irr_t0_nowgt[1,2]
local irr_poly_1_lo : di %6.2f pois_irr_t0_nowgt[2,2]
local irr_poly_1_up : di %6.2f pois_irr_t0_nowgt[3,2]
**"`=scalar(round(pois_irr_t0_nowgt[1,1]*1000,.01))' (95%CI: `=scalar(round(pois_irr_t0_nowgt[2,1]*1000,.01))',`=scalar(round(pois_irr_t0_nowgt[3,1]*1000,.01))')"
scalar irr_poly01 = "`=scalar(round(pois_irr_t0_nowgt[1,2],.01))' (95%CI: `=scalar(round(pois_irr_t0_nowgt[5,2],.01))',`=scalar(round(pois_irr_t0_nowgt[6,2],.01))')"

<</dd_do>>
~~~~

~~~~
<<dd_do>>
*set trace on
cap noi qui sts test poly, logrank
scalar logrank_chi= round(r(chi2),.01)
scalar logrank_df= r(df)
scalar logrank_p= round(chiprob(r(df),r(chi2)),.001)
local lr1: di %3.2f logrank_chi
local lr2: di %1.0f logrank_df
local lr3: di %5.4f logrank_p
scalar logrank_nowgt= " Chi^2(`lr2')=`lr1',p=`lr3'"
*di logrank_nowgt

matrix comb_irs = (0 \ 0 \ 0 \ 0)
matrix colnames comb_irs = "Tr.comp-no weight"
matrix rownames comb_irs = "No PSU: `=scalar(ir_poly_00)'" "PSU: `=scalar(ir_poly_11)'" "IRR: `=scalar(irr_poly01)'" "Logrank: `=scalar(logrank_nowgt)'"
esttab matrix(comb_irs) using "irrs_t0_nowgt_tr_comp.html", replace
*set trace off
<</dd_do>>
~~~~

<<dd_include: "irrs_t0_nowgt_tr_comp.html" >>

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
<</dd_do>>
~~~~


~~~~
<<dd_do>>
frame change example_a
poisson _d i.poly [pw=HAW], irr exposure(_t) vce(rob)
matrix pois_irr_t0_wgt= r(table)
*matselrc pois_ir_t0_nowgt pois_ir_t0_nowgt, c(1/1) r(1, 5/6)

margins i.poly, predict(ir)
matrix pois_ir_t0_wgt= r(table)
*matselrc pois_ir_t0_wgt pois_ir_t0_wgt, c(1/2) r(1, 5/6)
local ir_poly_0 : di %6.2f pois_ir_t0_wgt[1,1]
local ir_poly_0_lo : di %6.2f pois_ir_t0_wgt[2,1]
local ir_poly_0_up : di %6.2f pois_ir_t0_wgt[3,1]
*"`=scalar(round(pois_ir_t0_wgt[1,1]*1000,.01))' (95%CI: `=scalar(round(pois_ir_t0_wgt[2,1]*1000,.01))',`=scalar(round(pois_ir_t0_wgt[3,1]*1000,.01))')"
scalar ir_poly_001 = "`=scalar(round(pois_ir_t0_wgt[1,1]*1000,.01))' (95%CI: `=scalar(round(pois_ir_t0_wgt[5,1]*1000,.01))',`=scalar(round(pois_ir_t0_wgt[6,1]*1000,.01))')"
local ir_poly_1 : di %6.2f pois_ir_t0_wgt[1,2]
local ir_poly_1_lo : di %6.2f pois_ir_t0_wgt[2,2]
local ir_poly_1_up : di %6.2f pois_ir_t0_wgt[3,2]
*"`=scalar(round(pois_ir_t0_wgt[1,2]*1000,.01))' (95%CI: `=scalar(round(pois_ir_t0_wgt[2,2]*1000,.01))',`=scalar(round(pois_ir_t0_wgt[3,2]*1000,.01))')"
scalar ir_poly_111 = "`=scalar(round(pois_ir_t0_wgt[1,2]*1000,.01))' (95%CI: `=scalar(round(pois_ir_t0_wgt[5,2]*1000,.01))',`=scalar(round(pois_ir_t0_wgt[6,2]*1000,.01))')"

local irr_poly_1 : di %6.2f pois_irr_t0_wgt[1,2]
local irr_poly_1_lo : di %6.2f pois_irr_t0_wgt[2,2]
local irr_poly_1_up : di %6.2f pois_irr_t0_wgt[3,2]
**"`=scalar(round(pois_irr_t0_wgt[1,1]*1000,.01))' (95%CI: `=scalar(round(pois_irr_t0_wgt[2,1]*1000,.01))',`=scalar(round(pois_irr_t0_wgt[3,1]*1000,.01))')"
scalar irr_poly011 = "`=scalar(round(pois_irr_t0_wgt[1,2],.01))' (95%CI: `=scalar(round(pois_irr_t0_wgt[5,2],.01))',`=scalar(round(pois_irr_t0_wgt[6,2],.01))')"

<</dd_do>>
~~~~

~~~~
<<dd_do>>
*set trace on
cap noi qui sts test poly
scalar logrank_chi1= round(r(chi2),.01)
scalar logrank_df1= r(df)
scalar logrank_p1= round(chiprob(r(df),r(chi2)),.001)
local lr10: di %3.2f logrank_chi1
local lr20: di %1.0f logrank_df1
local lr30: di %5.4f logrank_p1
scalar logrank_wgt1 = "Cox regression-based test for equality of survival curves: Wald Chi^2(`lr20')=`lr10',p=`lr30'"
*di logrank_nowgt

matrix comb_irs2 = (0 \ 0 \ 0 \ 0)
matrix colnames comb_irs2 = "Tr.comp-weight"
matrix rownames comb_irs2 = "No PSU: `=scalar(ir_poly_001)'" "PSU: `=scalar(ir_poly_111)'" "IRR: `=scalar(irr_poly011)'" "Logrank: `=scalar(logrank_wgt1)'"
esttab matrix(comb_irs2) using "irrs_t0_wgt_tr_comp.html", replace
*set trace off
<</dd_do>>
~~~~

<<dd_include: "irrs_t0_wgt_tr_comp.html" >>

~~~~
<<dd_do>>
frame example_a: stdescribe, weight
frame change default

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

~~~~
<<dd_do>>
poisson _d i.poly , irr exposure(_t) vce(rob)
matrix pois_irr_t0_nowgt2= r(table)
*matselrc pois_ir_t0_nowgt pois_ir_t0_nowgt, c(1/1) r(1, 5/6)

margins i.poly, predict(ir)
matrix pois_ir_t0_nowgt2= r(table)
*matselrc pois_ir_t0_nowgt pois_ir_t0_nowgt, c(1/2) r(1, 5/6)
local ir_poly_0 : di %6.2f pois_ir_t0_nowgt2[1,1]
local ir_poly_0_lo : di %6.2f pois_ir_t0_nowgt2[2,1]
local ir_poly_0_up : di %6.2f pois_ir_t0_nowgt2[3,1]
*"`=scalar(round(pois_ir_t0_nowgt[1,1]*1000,.01))' (95%CI: `=scalar(round(pois_ir_t0_nowgt[2,1]*1000,.01))',`=scalar(round(pois_ir_t0_nowgt[3,1]*1000,.01))')"
scalar ir_poly_002 = "`=scalar(round(pois_ir_t0_nowgt2[1,1]*1000,.01))' (95%CI: `=scalar(round(pois_ir_t0_nowgt2[5,1]*1000,.01))',`=scalar(round(pois_ir_t0_nowgt2[6,1]*1000,.01))')"
local ir_poly_1 : di %6.2f pois_ir_t0_nowgt2[1,2]
local ir_poly_1_lo : di %6.2f pois_ir_t0_nowgt2[2,2]
local ir_poly_1_up : di %6.2f pois_ir_t0_nowgt2[3,2]
*"`=scalar(round(pois_ir_t0_nowgt[1,2]*1000,.01))' (95%CI: `=scalar(round(pois_ir_t0_nowgt[2,2]*1000,.01))',`=scalar(round(pois_ir_t0_nowgt[3,2]*1000,.01))')"
scalar ir_poly_112 = "`=scalar(round(pois_ir_t0_nowgt2[1,2]*1000,.01))' (95%CI: `=scalar(round(pois_ir_t0_nowgt2[5,2]*1000,.01))',`=scalar(round(pois_ir_t0_nowgt2[6,2]*1000,.01))')"

local irr_poly_1 : di %6.2f pois_irr_t0_nowgt2[1,2]
local irr_poly_1_lo : di %6.2f pois_irr_t0_nowgt2[2,2]
local irr_poly_1_up : di %6.2f pois_irr_t0_nowgt2[3,2]
**"`=scalar(round(pois_irr_t0_nowgt[1,1]*1000,.01))' (95%CI: `=scalar(round(pois_irr_t0_nowgt[2,1]*1000,.01))',`=scalar(round(pois_irr_t0_nowgt[3,1]*1000,.01))')"
scalar irr_poly012 = "`=scalar(round(pois_irr_t0_nowgt2[1,2],.01))' (95%CI: `=scalar(round(pois_irr_t0_nowgt2[5,2],.01))',`=scalar(round(pois_irr_t0_nowgt2[6,2],.01))')"

<</dd_do>>
~~~~

~~~~
<<dd_do>>
*set trace on
cap noi qui sts test poly, logrank
scalar logrank_chi2= round(r(chi2),.01)
scalar logrank_df2= r(df)
scalar logrank_p2= round(chiprob(r(df),r(chi2)),.001)
local lr12: di %3.2f logrank_chi2
local lr22: di %1.0f logrank_df2
local lr32: di %5.4f logrank_p2
scalar logrank_nowgt3= " Chi^2(`lr22')=`lr12',p=`lr32'"
*di logrank_nowgt

matrix comb_irs3 = (0 \ 0 \ 0 \ 0)
matrix colnames comb_irs3 = "Contact.js-no weight"
matrix rownames comb_irs3 = "No PSU: `=scalar(ir_poly_002)'" "PSU: `=scalar(ir_poly_112)'" "IRR: `=scalar(irr_poly012)'" "Logrank: `=scalar(logrank_nowgt3)'"
esttab matrix(comb_irs3) using "irrs_t0_nowgt_contact_js.html", replace
*set trace off
<</dd_do>>
~~~~

<<dd_include: "irrs_t0_nowgt_contact_js.html" >>

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

*The stset command is used for survival analysis to set the time variable and specify the structure of the dataset, while the [pw] option specifies the weights for each observation in the analysis.
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
frame example_b: stset diff2 [pw=HAW2], enter(_start) failure(contact_js==1)  id(id) //*scale(12) 

frame example_b: stsum, by (poly)
<</dd_do>>
~~~~

~~~~
<<dd_do>>
frame change example_b 
poisson _d i.poly [pw=HAW2], irr exposure(_t) vce(rob)
matrix pois_irr_t0_wgt3= r(table)
*matselrc pois_ir_t0_nowgt pois_ir_t0_nowgt, c(1/1) r(1, 5/6)

margins i.poly, predict(ir)
matrix pois_ir_t0_wgt3= r(table)
*matselrc pois_ir_t0_wgt pois_ir_t0_wgt, c(1/2) r(1, 5/6)
local ir_poly_0 : di %6.2f pois_ir_t0_wgt3[1,1]
local ir_poly_0_lo : di %6.2f pois_ir_t0_wgt3[2,1]
local ir_poly_0_up : di %6.2f pois_ir_t0_wgt3[3,1]
*"`=scalar(round(pois_ir_t0_wgt[1,1]*1000,.01))' (95%CI: `=scalar(round(pois_ir_t0_wgt[2,1]*1000,.01))',`=scalar(round(pois_ir_t0_wgt[3,1]*1000,.01))')"
scalar ir_poly_003 = "`=scalar(round(pois_ir_t0_wgt3[1,1]*1000,.01))' (95%CI: `=scalar(round(pois_ir_t0_wgt3[5,1]*1000,.01))',`=scalar(round(pois_ir_t0_wgt3[6,1]*1000,.01))')"
local ir_poly_1 : di %6.2f pois_ir_t0_wgt3[1,2]
local ir_poly_1_lo : di %6.2f pois_ir_t0_wgt3[2,2]
local ir_poly_1_up : di %6.2f pois_ir_t0_wgt3[3,2]
*"`=scalar(round(pois_ir_t0_wgt[1,2]*1000,.01))' (95%CI: `=scalar(round(pois_ir_t0_wgt[2,2]*1000,.01))',`=scalar(round(pois_ir_t0_wgt[3,2]*1000,.01))')"
scalar ir_poly_113 = "`=scalar(round(pois_ir_t0_wgt3[1,2]*1000,.01))' (95%CI: `=scalar(round(pois_ir_t0_wgt3[5,2]*1000,.01))',`=scalar(round(pois_ir_t0_wgt3[6,2]*1000,.01))')"

local irr_poly_1 : di %6.2f pois_irr_t0_wgt3[1,2]
local irr_poly_1_lo : di %6.2f pois_irr_t0_wgt3[2,2]
local irr_poly_1_up : di %6.2f pois_irr_t0_wgt3[3,2]
**"`=scalar(round(pois_irr_t0_wgt[1,1]*1000,.01))' (95%CI: `=scalar(round(pois_irr_t0_wgt[2,1]*1000,.01))',`=scalar(round(pois_irr_t0_wgt[3,1]*1000,.01))')"
scalar irr_poly013 = "`=scalar(round(pois_irr_t0_wgt3[1,2],.01))' (95%CI: `=scalar(round(pois_irr_t0_wgt3[5,2],.01))',`=scalar(round(pois_irr_t0_wgt3[6,2],.01))')"

<</dd_do>>
~~~~

~~~~
<<dd_do>>
*set trace on
cap noi qui sts test poly
scalar logrank_chi3= round(r(chi2),.01)
scalar logrank_df3= r(df)
scalar logrank_p3= round(chiprob(r(df),r(chi2)),.001)
local lr13: di %3.2f logrank_chi3
local lr23: di %1.0f logrank_df3
local lr33: di %5.4f logrank_p3
scalar logrank_wgt4 = "Cox regression-based test for equality of survival curves: Wald Chi^2(`lr23')=`lr13',p=`lr33'"
*di logrank_nowgt

matrix comb_irs4 = (0 \ 0 \ 0 \ 0)
matrix colnames comb_irs4 = "Contact.js-weight"
matrix rownames comb_irs4 = "No PSU: `=scalar(ir_poly_003)'" "PSU: `=scalar(ir_poly_113)'" "IRR: `=scalar(irr_poly013)'" "Logrank: `=scalar(logrank_wgt4)'"
esttab matrix(comb_irs4) using "irrs_t0_wgt_contact_js.html", replace
*set trace off
<</dd_do>>
~~~~

<<dd_include: "irrs_t0_wgt_contact_js.html" >>

~~~~
<<dd_do>>
frame example_b: stdescribe, weight
<</dd_do>>
~~~~


~~~~
<<dd_do>>
stptime, title(person-years) per(1000) by(poly)

stmh poly
scalar poly_rr= round( r(rratio), .01)
scalar poly_rr_lb= round(r(lb),.01) 
scalar poly_rr_ub= round(r(ub),.01) 
local ir1= poly_rr
local ir2= poly_rr_lb
local ir3= poly_rr_ub
global poly_irr " `title': IRR: `ir1' (95%IC `ir2' - `ir3') "
<</dd_do>>
~~~~

- The weighted IRR of treatment completion was <<dd_display: "$poly_irr">> per 1,000 person-years

~~~~
<<dd_do>>

*set trace on
local stname `" "2_1" "' 
local titl `" "Poly vs No-Poly" "' 
foreach s of local stname {
	gettoken title titl: titl
cap noi qui ir _d poly _t
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
global irr_`s' " `title': IRR (non weighted): `ir1' (IC 95% `ir2' - `ir3') "
global ir_`s' "`ir1' (IC 95% `ir2' - `ir3')"
}	
<</dd_do>>
~~~~

- <<dd_display: "$irr_2_1">>

~~~~
<<dd_do>>
matrix comb_irs5 = (0 \ 0 )
matrix colnames comb_irs5 = "Contact.js-weight2"
matrix rownames comb_irs5 = "IRR weighted: '$poly_irr'" "IRR non qweighted: '$irr_2_1'" 
esttab matrix(comb_irs5) using "irrs_t0_wgt_contact_js2.html", replace
*set trace off
<</dd_do>>
~~~~

<<dd_include: "irrs_t0_wgt_contact_js2.html" >>

~~~~
<<dd_do>>
frame change default
<</dd_do>>
~~~~

**##############################**

### GET WEIGHTED BALANCE TABLES AND PROPORTIONAL HAZARDS FROM THE SELECTED WEIGHTS

**##############################**


~~~~
<<dd_do>>
quietly tab sus_prin_mod, generate(sus_prin_mod_cat) 
quietly tab fr_sus_prin, gen(fr_sus_prin_cat)

quietly tab esc_rec, gen(esc_rec_tab) 
quietly tab ten_viv, gen(ten_viv_tab) 
quietly tab dg_cie_10_rec, gen(dg_cie_10_rec_tab) 
quietly tab clas, gen(clas) 
quietly tab macrozone, gen(macrozone_cat) 

lab var macrozone_cat2 "Macrozone (North)"  
lab var macrozone_cat3 "Macrozone (South)"  
lab var clas2 "Municipallity of Residence Classification (Mixed)"
lab var clas3 "Municipallity of Residence Classification (Rural)"
lab var sex_enc "Sex (Women)"
lab var n_off_vio "Pre-tr. criminality (Violent)"
lab var n_off_acq "Pre-tr. criminality (Acquisitve)"
lab var n_off_sud "Pre-tr. criminality (substance-related)"
lab var dg_cie_10_rec_tab2 "Psychiatric comorbidity ICD-10 (Diagnosis under study)"
lab var dg_cie_10_rec_tab3 "Psychiatric comorbidity ICD-10 (Diagnosis under study)"
lab var ten_viv_tab1 "Housing situation (Illegal Settlement)"
lab var ten_viv_tab3 "Housing situation (owner/Tr. dwellings/Pays divide)"
lab var ten_viv_tab4 "Housing situation (Renting)"
lab var ten_viv_tab5 "Housing situation (Stays temporary w/ a relative)"
lab var esc_rec_tab2 "Educational Attainment (Complete high-school or less)"
lab var esc_rec_tab3 "Educational Attainment (Complete high-school or less)"
lab var edad_al_ing_1 "Admission Age"
lab var edad_ini_cons "Substance use onset age"
lab var sus_prin_mod_cat1 "Primary`=char(9)'Subs`=char(9)'at`=char(9)'Adm.`=char(9)'(Alcohol)" //asdsa
lab var sus_prin_mod_cat2 "Primary Subs at Adm. (Snort cocaine)"
lab var sus_prin_mod_cat3 "Primary Subs at Adm. (Cocaine paste base)"
lab var sus_prin_mod_cat4 "Primary Subs at Adm .(Marijuana)"
lab var fr_sus_prin_cat1 "Primary Subs use Freq (1 day a week or more)"
lab var fr_sus_prin_cat2 "Primary Subs use Freq (2-3 days a week or more)"
lab var fr_sus_prin_cat3 "Primary Subs use Freq (4-6 days a week or more)"
lab var fr_sus_prin_cat4 "Primary Subs use Freq (Daily)"

														
logistic poly sus_prin_mod_cat1 sus_prin_mod_cat2 sus_prin_mod_cat3 sus_prin_mod_cat4 ///
							fr_sus_prin_cat1 fr_sus_prin_cat2 fr_sus_prin_cat3 fr_sus_prin_cat4 /// 
							edad_al_ing_1 edad_ini_cons ///
							sex_enc ///
							esc_rec_tab2 esc_rec_tab3 /// 
							ten_viv_tab1 ten_viv_tab3 ten_viv_tab4 ten_viv_tab5 /// 
							dg_cie_10_rec_tab2 dg_cie_10_rec_tab3 /// 
							n_off_vio  /// 
							n_off_acq  /// 
							n_off_sud  /// 
							clas2 clas3 /// 
							porc_pobr ///
							macrozone_cat2 macrozone_cat3, nolog 
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
stset diff1 [pw=HAW], enter(_start) failure(comp==1) id(id) //*scale(12) 

stsum, by (poly)
scalar ir_total= round(r(ir),.01) 
global ir_total= ir_total
<</dd_do>>
~~~~	


We explored the inicidence rate ratios (IRR) of polysubstance use.

~~~~
<<dd_do>>
stptime, title(person-years) per(1000) by(poly)

stmh poly
scalar poly_b_rr= round( r(rratio), .01)
scalar poly_b_rr_lb= round(r(lb),.01) 
scalar poly_b_rr_ub= round(r(ub),.01) 
local ir1b= poly_b_rr
local ir2b= poly_b_rr_lb
local ir3b= poly_b_rr_ub
global poly2_irr " `title': IRR: `ir1b' (95%IC `ir2b' - `ir3b') "
scalar exp1= "`poly2_irr'"
<</dd_do>>
~~~~

- The weighted IRR of treatment completion was <<dd_display: "$poly2_irr">> per 1,000 person-years

~~~~
<<dd_do>>

*set trace on
local stname `" "2_1" "' 
local titl `" "Poly vs No-Poly" "' 
foreach s of local stname {
	gettoken title titl: titl
cap noi qui ir _d poly _t
scalar ir2_`s' =round(r(irr),.01)
*di ir_`s'
scalar ir2_`s'_lb =round(r(lb_irr),.01) 
*di ir_`s'_lb
scalar ir2_`s'_ub =round(r(ub_irr),.01)
*di ir_`s'_ub
local ir12a= ir2_`s'
local ir22a= ir2_`s'_lb
local ir32a= ir2_`s'_ub
*di  in gr _col(13) " `title': IRR `ir1' (IC 95% `ir2' - `ir3') "
global irr2_`s' " `title': IRR (non weighted): `ir12a' (IC 95% `ir22a' - `ir32a') "
global ir2_`s' "`ir1' (IC 95% `ir2' - `ir3')"
}	
<</dd_do>>
~~~~

- <<dd_display: "$irr2_2_1">>

~~~~
<<dd_do>>
matrix comb_irs6 = (0 \ 0 )
matrix colnames comb_irs6 = IRRs_t0
matrix rownames comb_irs6 = "IRR weighted: '$poly2_irr'" "IRR non qweighted: '$irr2_2_1'" 
esttab matrix(comb_irs6) using "irrs_t0_wgt_tr_comp2.html", replace
*set trace off
<</dd_do>>
~~~~

<<dd_include: "irrs_t0_wgt_tr_comp2.html" >>


Get schoenfeld residuals

~~~~
<<dd_do>>
qui stcox  poly , robust nolog schoenfeld(sch*) scaledsch(sca*)
qui estat phtest, log detail
qui scalar chi2_scho_test = r(chi2)

qui mat mat_scho_test = r(phtest)

esttab matrix(mat_scho_test) using "mat_scho_test_ser23.html", replace

<</dd_do>>
~~~~

<<dd_include: "mat_scho_test_ser23.html" >>


~~~~
<<dd_do>>
pbalchk poly sus_prin_mod_cat1 sus_prin_mod_cat2 sus_prin_mod_cat3 sus_prin_mod_cat4 ///
							fr_sus_prin_cat1 fr_sus_prin_cat2 fr_sus_prin_cat3 fr_sus_prin_cat4 /// 
							edad_al_ing_1 edad_ini_cons sex_enc ///
							esc_rec_tab2 esc_rec_tab3 /// 
							ten_viv_tab1 ten_viv_tab3 ten_viv_tab4 ten_viv_tab5 /// 
							dg_cie_10_rec_tab2 dg_cie_10_rec_tab3 /// 
							n_off_vio  /// 
							n_off_acq  /// 
							n_off_sud  /// 
							clas2 clas3 /// 
							porc_pobr ///
							macrozone_cat2 macrozone_cat3, wt(HAW) p mahal sqrt diag graph xline(.15 -.15)
graph save "Graph" "`c(pwd)'\_figs\pbal2.gph", replace

mat smd_before = r(usmeandiff)

// Change legends
gr_edit .legend.plotregion1.label[1].text = {}
gr_edit .legend.plotregion1.label[1].text.Arrpush Before Adjustment
gr_edit .legend.plotregion1.label[2].text = {}
gr_edit .legend.plotregion1.label[2].text.Arrpush After Adjustment

//change image background 
gr_edit style.editstyle boxstyle(shadestyle(color(gs16))) editcopy
gr_edit .yaxis1.style.editstyle majorstyle(tickstyle(textstyle(size(vsmall)))) editcopy

//mod points in grayscale
gr_edit .plotregion1.plot1.style.editstyle marker(fillcolor(gs7%60)) editcopy
gr_edit .plotregion1.plot1.style.editstyle marker(linestyle(color(gs7%60))) editcopy
gr_edit .plotregion1.plot2.style.editstyle marker(fillcolor(gs3%60)) editcopy
gr_edit .plotregion1.plot2.style.editstyle marker(linestyle(color(gs3%60))) editcopy

// modify label
gr_edit .legend.style.editstyle boxstyle(linestyle(color(none))) editcopy //.legend.Edit , style(rows(2)) style(cols(0)) keepstyles 
// modify label
gr_edit .xaxis1.title.text = {}
gr_edit .xaxis1.title.text.Arrpush Standardardized differences
// note 
gr_edit .note.text = {}
gr_edit .note.text.Arrpush Note: Red lines depict standardized differences of -0.15 and +0.15
gr_edit .note.DragBy -.2314887155038407 -45

//title
gr_edit .title.style.editstyle size(medlarge) editcopy
gr_edit .title.text = {}
gr_edit .title.text.Arrpush Figure 1. Graphical Representation of SMDs
gr_edit .title.style.editstyle color(black) editcopy
gr_edit .title.style.editstyle box_alignment(nwest) editcopy
gr_edit .title.style.editstyle horizontal(left) editcopy
gr_edit .title.xoffset = -45
gr_edit .title.DragBy 2 0
//eliminate title 2023-04-23
gr_edit .title.draw_view.setstyle, style(no)

/*
gr_edit .yaxis1.major.num_rule_ticks = 26
gr_edit .yaxis1.edit_tick 27 26 `"Primary Subs Adm (Cocaine paste base)"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 26
gr_edit .yaxis1.edit_tick 24 24 `"Primary Subs Adm (Cocaine)"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 26
//gr_edit .yaxis1.edit_tick 24 24 `"Primary Subs Adm (Cocaine)"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 25
//gr_edit .yaxis1.edit_tick 23 23 `"Housing situation (Owner/Tr. dwellings/Pays divide)"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 25
gr_edit .yaxis1.edit_tick 23 23 `"Housing situation (Owner/Tr. dwellings/Pays divide)"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 24
gr_edit .yaxis1.edit_tick 22 22 `"Housing situation (Stays temporary w/ relative)"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 23
gr_edit .yaxis1.edit_tick 21 21 `"Macrozone (South)"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 22
gr_edit .yaxis1.edit_tick 20 20 `"Pre-tr. criminality (Acquisitve)"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 21
gr_edit .yaxis1.edit_tick 17 17 `"Macrozone (North)"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 19
gr_edit .yaxis1.edit_tick 16 16 `"Pre-tr. criminality (substance-related)"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 19
gr_edit .yaxis1.edit_tick 17 19 `"Educational Attainment (Complete primary school or less)"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 18
gr_edit .yaxis1.edit_tick 16 18 `"Municipallity of Residence Classification (Mixed)"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 17
gr_edit .yaxis1.edit_tick 12 12 `"Municipallity of Residence Classification (Urban)"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 16
gr_edit .yaxis1.edit_tick 13 14 `"Educational Attainment (Complete high-school or less)"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 14
gr_edit .yaxis1.edit_tick 12 15 `"Substance use frequency (primary subs)- Daily"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 13
gr_edit .yaxis1.edit_tick 10 10 `"Substance use frequency (primary subs)- < 1 day a week"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 4
gr_edit .yaxis1.edit_tick 2 2 `"Housing situation (Other)"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 3
gr_edit .yaxis1.edit_tick 1 1 `"Substance use frequency (primary subs)- 4-6 days a week"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 7
gr_edit .yaxis1.edit_tick 5 5 `"Pre-tr. criminality (Violent)"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 6
gr_edit .yaxis1.edit_tick 4 4 `"Primary Subs Adm (Other)"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 5
gr_edit .yaxis1.edit_tick 3 3 `"Housing situation (Renting)"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 8
gr_edit .yaxis1.edit_tick 6 6 `"Sex (Women)"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 9
gr_edit .yaxis1.edit_tick 7 7 `"Substance use frequency (primary subs)- 2-3 days a week"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 11
gr_edit .yaxis1.edit_tick 9 11 `"Primary substance at admission (Marijuana)"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 10
gr_edit .yaxis1.edit_tick 8 8 `"Poverty of municipallities of residence"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 6
gr_edit .yaxis1.edit_tick 4 24 `"Primary Subs Adm (Cocaine)"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 5
gr_edit .yaxis1.edit_tick 3 23 `"Housing situation (owner/Tr. dwellings/Pays divide)"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 4
gr_edit .yaxis1.edit_tick 2 13 `"Psychiatric comorbidity (ICD-10)- Under study"', tickset(major)
gr_edit .yaxis1.major.num_rule_ticks = 3
gr_edit .yaxis1.edit_tick 1 9 `"Psychiatric comorbidity (ICD-10)- Presence"', tickset(major)
*/

graph export "`c(pwd)'\_figs\pbal2_mod.jpg", as(jpg) replace width(2000) height(1333)
graph export "`c(pwd)'\_figs\pbal2_mod.png", as(png) replace width(1800) height(1000)
graph export "`c(pwd)'\_figs\pbal2_mod.eps", as(eps) replace
graph export "`c(pwd)'\_figs\pbal2_mod.pdf", as(pdf) replace //*width(2000) height(2000) orientation(landscape)
*graph export "_Appendix2_Graph_Mean_SE_g32.svg", as(svg) replace height(20000) fontface (Helvetica)
graph save "`c(pwd)'\_figs\pbal2_mod", replace
graph save "Graph" "`c(pwd)'\_figs\pbal2_mod.gph", replace
//graph use "pbal2_mod"

*You cannot give both f and p as options, only one.
*, strata(varname) wt(varname) f p mahal metric(matrix) diag sqrt xiprefix(string) graph xline(numlist) xlabel(numlist) nostandardize nocatstandardize sigrep ]
<</dd_do>>
~~~~

<<dd_graph: saving("pbal2_mod.svg") width(800) replace>>

Cocaine paste base and cocaine hydrochloride were the only variables that were not well adjusted if resticted the sample to the 5% and 95%.

=======================================
## Multistate & IPW
=======================================

and transform the database in a long format	according to the specifications and the transition matrix.									
										
~~~~										
<<dd_do>>	
cap drop  _st
cap drop _d
cap drop _t
cap drop _t0
cap drop _start
	
// https://reddooranalytics.se/2022/01/17/multistate-v4-4-0-semi-parametric-multi-state-modelling/
matrix tmat = (.,1,2 \ .,.,3 \ .,.,.)
matrix colnames tmat  = start TC Contact_JS
matrix rownames tmat   = start TC Contact_JS
matrix coleq tmat   = to to to
matrix roweq tmat  = from from from 
										

msset, id(id) states(comp contact_js) transm(tmat) ///									
		times(diff1 diff2)  //* saqué tipo_de_plan_res_1 para que no se empiece a subdividir								

mat mat_obs_states = r(transmatrix)										
mat freq_trans = r(freqmatrix)   										
mat next_states = r(Nnextstates) 										
<</dd_do>>										
~~~~
~~~~										
<<dd_do>>	
msboxes, transmatrix(tmat) id(id)                  ///
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

forvalues i = 5/7 {
gr_edit .plotregion1.plot`i'.style.editstyle area(linestyle(color(black))) editcopy
gr_edit .plotregion1.plot`i'.style.editstyle marker(fillcolor(black)) editcopy
gr_edit .plotregion1.plot`i'.style.editstyle marker(linestyle(color(black))) editcopy
}

gr_edit .plotregion1.textbox1.text = {}
gr_edit .plotregion1.textbox1.text.Arrpush `"Admission to"'
gr_edit .plotregion1.textbox1.text.Arrpush `"baseline treatment"'
gr_edit .plotregion1.textbox2.text = {}
gr_edit .plotregion1.textbox2.text.Arrpush `"Treatment"'
gr_edit .plotregion1.textbox2.text.Arrpush `"Completion"'
gr_edit .plotregion1.textbox3.text = {}
gr_edit .plotregion1.textbox3.text.Arrpush `"Contact with"'
gr_edit .plotregion1.textbox3.text.Arrpush `"the justice system"'
gr_edit .plotregion1.textbox3.text.Arrpush `"after baseline tr."'
//move down third label
gr_edit .plotregion1.textbox3.DragBy -.0030521185101971 0
gr_edit .plotregion1.textbox3.DragBy -.0030521185101971 0
gr_edit .plotregion1.textbox3.DragBy -.0030521185101971 0

//labels of transitions
gr_edit .plotregion1.textbox15.text = {}
gr_edit .plotregion1.textbox15.text.Arrpush `"{it:h(3)}{sub:23}"'
gr_edit .plotregion1.textbox13.text = {}
gr_edit .plotregion1.textbox13.text.Arrpush `"{it:h(1)}{sub:12}"'
gr_edit .plotregion1.textbox14.text = {}
gr_edit .plotregion1.textbox14.text.Arrpush `"{it:h(2)}{sub:13}"'

//warning: if the matrix or events change, change these numbers
gr_edit .plotregion1.textbox4.text = {}
gr_edit .plotregion1.textbox4.text.Arrpush 59,763
gr_edit .plotregion1.textbox7.text = {}
gr_edit .plotregion1.textbox7.text.Arrpush 28,957
gr_edit .plotregion1.textbox5.text = {}
gr_edit .plotregion1.textbox5.text.Arrpush 16,475
gr_edit .plotregion1.textbox8.text = {}
gr_edit .plotregion1.textbox8.text.Arrpush 13,539
gr_edit .plotregion1.textbox6.text = {}
gr_edit .plotregion1.textbox6.text.Arrpush 0
gr_edit .plotregion1.textbox9.text = {}
gr_edit .plotregion1.textbox9.text.Arrpush 17,267


graph export "`c(pwd)'\_figs\transmat_ser23.png", as(png) replace width(2000) height(1000)
graph export "`c(pwd)'\_figs\transmat_ser23.eps", as(eps) replace
graph export "`c(pwd)'\_figs\transmat_ser23.pdf", as(pdf) replace //*width(2000) height(2000) orientation(landscape)
*graph export "_Appendix2_Graph_Mean_SE_g32.svg", as(svg) replace height(20000) fontface (Helvetica)
graph save "`c(pwd)'\_figs\transmat_ser23", replace
graph save "`c(pwd)'\_figs\transmat_ser23_2", replace
<</dd_do>>										
~~~~

<<dd_graph: saving("transmat_ser23_2.svg") width(800) replace>>

~~~~
<<dd_do>>
*mat li freq_trans
*file:///G:/Mi%20unidad/Alvacast/SISTRAT%202019%20(github)/_supp_mstates/stata/crowther2017%20(1).pdf										
gen _time = _stop - _start
lab var _time "Time to states (reset)" 

gen _start2 = edad_al_ing_1_mod + _start
gen _stop2 = edad_al_ing_1 + _stop

*age time
*stset age_offending_imp, fail(event ==1) enter(edad_al_egres_imp)

stset _stop [pw=HAW], enter(_start) failure(_status==1) //*scale(12) 
*list edad_al_ing_1 edad_al_egres_imp_rec age_offending_imp age_at_censor_date edad_al_egres_imp_rec _start _stop _stop2 if _stop2<9

// days in admission of 0
*browse if diff1<0

cap gen _time = _t	

*replace event=1 if !missing(sex)
range timevar0 0.2 5 180
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
For this, we separated the transition probabilities according to polysubstance use at baseline.

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
	title("Probabilites in Admission with confidence intervals", size(small)) name(msaj_1, replace) ///
	saving("`c(pwd)'\_figs\msaj_1.gph", replace) //msaj_1.gph
<</dd_do>>

<<dd_graph: saving("msaj_1.svg") width(800) replace>>

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
	title("Probabilites in Tr. Completion with confidence intervals", size(small)) name(msaj_2, replace) ///
	saving("`c(pwd)'\_figs\msaj_2.gph", replace)
<</dd_do>>

<<dd_graph: saving("msaj_2.svg") width(800) replace>>

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
	title(" Probability in Contact with the justice system with confidence intervals", size(small)) name(msaj_3, replace) ///
	saving("`c(pwd)'\_figs\msaj_3.gph", replace) 
<</dd_do>>

<<dd_graph: saving("msaj_3.svg") width(800) replace>>


~~~~
<<dd_do:nocommand>>

cap noi drop trp_ajprob*
cap drop P_AJ*
cap drop LOS_AJ_*
 forvalues i = 1/2 {
	msaj, transmat(mat_obs_states) from(`i') ltruncated(.25) exit(5) by(poly) ci los
	rename (P_AJ_*) (trp_ajprob_3_5_`i'*)
	rename (LOS_AJ_*) (trp_ajlos_3_5_`i'*)
	cap drop P_AJ* 
	cap drop LOS_AJ_*
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
recode ranges 0=1 if inrange(_t, .49, .51) 
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
	saving("`c(pwd)'\_figs\msaj_12_23.gph", replace)
<</dd_do>>

<<dd_graph: saving("msaj_12_23.svg") width(800) replace>>


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
	saving("`c(pwd)'\_figs\msaj_13_23.gph", replace)
<</dd_do>>

<<dd_graph: saving("msaj_13_23.svg") width(800) replace>>

<<dd_do:nocommand>>
cap qui noi frame drop example3
frame copy default example3
frame example3: cap noi gen trp_ajprob_3_5_23_miss= cond(!missing(trp_ajprob_3_5_23),1,0)
*cap noi egen trp_ajprob_3_5_12_n=count(trp_ajprob_3_5_12) if !missing(trp_ajprob_3_5_12), by(ranges poly)
frame example3: sort trp_ajprob_3_5_23_miss ranges poly _t
frame example3: cap quietly noi by trp_ajprob_3_5_23_miss ranges poly:  gen dup2 = cond(_N==1,0,_n)


frame example3: twoway (rbar bar trp_ajprob_3_5_23 _t2 if poly==0 & dup2==1 & _t2<5.17, color(gs3%70) lcolor(black%0) barw(.3)) ///   
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
	saving("`c(pwd)'\_figs\msaj_23_23.gph", replace)

<</dd_do>>

<<dd_graph: saving("msaj_23_23.svg") width(800) replace>>



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
	 matrix rownames e_a_`var' = 6_mths 1_yr 3_yrs 5_yrs
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
	 matrix rownames e_b_`var' = 6_mths 1_yr 3_yrs 5_yrs
 }

matrix est_msaj12 = (e_a_trp_ajprob_3_5_12, e_a_trp_ajprob_3_5_12_lci, e_a_trp_ajprob_3_5_12_uci, e_b_trp_ajprob_3_5_12, e_b_trp_ajprob_3_5_12_lci, e_b_trp_ajprob_3_5_12_uci)
matrix colnames est_msaj12 = Est_NoPoly LCI UCI Est_Poly LCI UCI

esttab matrix(est_msaj12) using "pr_msaj12_23.html", replace 
<</dd_do>>
~~~~

The transition probabilities are presented here:

<<dd_include: "pr_msaj12_23.html" >>

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
	 matrix rownames e_a_`var' = 6_mths 1_yr 3_yrs 5_yrs
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
	 matrix rownames e_b_`var' = 6_mths 1_yr 3_yrs 5_yrs
 }

matrix est_msaj13 = (e_a_trp_ajprob_3_5_13, e_a_trp_ajprob_3_5_13_lci, e_a_trp_ajprob_3_5_13_uci, e_b_trp_ajprob_3_5_13, e_b_trp_ajprob_3_5_13_lci, e_b_trp_ajprob_3_5_13_uci)
matrix colnames est_msaj13 = Est_NoPoly LCI UCI Est_Poly LCI UCI

esttab matrix(est_msaj13) using "pr_msaj13_23.html", replace 
<</dd_do>>
~~~~

The transition probabilities are presented here:

<<dd_include: "pr_msaj13_23.html" >>

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
	 matrix rownames e_a_`var' = 6_mths 1_yr 3_yrs 5_yrs
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
	 matrix rownames e_b_`var' = 6_mths 1_yr 3_yrs 5_yrs
 }

matrix est_msaj23 = (e_a_trp_ajprob_3_5_23, e_a_trp_ajprob_3_5_23_lci, e_a_trp_ajprob_3_5_23_uci, e_b_trp_ajprob_3_5_23, e_b_trp_ajprob_3_5_23_lci, e_b_trp_ajprob_3_5_23_uci)
matrix colnames est_msaj23 = Est_NoPoly LCI UCI Est_Poly LCI UCI

esttab matrix(est_msaj23) using "pr_msaj23_23.html", replace 
<</dd_do>>
~~~~

The transition probabilities are presented here:

<<dd_include: "pr_msaj23_23.html" >>


## Aalen-Johanssen, Tables of Lengths of stay

**Lengths of stay in Admission (does not give CIs)**

~~~~
<<dd_do>>
* inrange(_t, .299, .301) | _t==1 | inrange(_t, 2.99, 3.01) | inrange(_t, 4.970, 5.001) 
foreach var of varlist trp_ajlos_3_5_11 trp_ajlos_3_5_22 {
				scalar variable = "`var'"
					qui summarize `var' if inrange(_t, .49, .51) & poly==0 
					scalar e3m_`var' = round(round(r(mean),.001),.1)
					qui summarize `var' if inrange(_t, 1, 1) & poly==0 
					scalar e1y_`var' = round(round(r(mean),.001),.1)
					qui summarize `var' if inrange(_t, 2.99, 3.01) & poly==0 
					scalar e3y_`var' = round(round(r(mean),.001),.1)
					qui summarize `var' if inrange(_t,  4.97, 5.001) & poly==0 
					scalar e5y_`var' = round(round(r(mean),.001),.1)
	 cap noi matrix e_a_`var' = (`=scalar(e3m_`var')'\  `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames e_a_`var' = `var'
	 matrix rownames e_a_`var' = 6_mths 1_yr 3_yrs 5_yrs
					qui summarize `var' if inrange(_t, .49, .51) & poly==1 
					scalar e3m_`var' = round(round(r(mean),.001),.1)
					qui summarize `var' if inrange(_t, 1, 1) & poly==1
					scalar e1y_`var' = round(round(r(mean),.001),.1)
					qui summarize `var' if inrange(_t, 2.99, 3.01) & poly==1
					scalar e3y_`var' = round(round(r(mean),.001),.1)
					qui summarize `var' if inrange(_t,  4.97, 5.001) & poly==1
					scalar e5y_`var' = round(round(r(mean),.001),.1)
	 cap noi matrix e_b_`var' = (`=scalar(e3m_`var')'\  `=scalar(e1y_`var')'\ `=scalar(scalar(e3y_`var'))'\ `=scalar(scalar(e5y_`var'))')
	 matrix colnames e_b_`var' = `var'
	 matrix rownames e_b_`var' = 6_mths 1_yr 3_yrs 5_yrs
 }

matrix est_msaj12los = (e_a_trp_ajlos_3_5_11, e_b_trp_ajlos_3_5_11, e_a_trp_ajlos_3_5_22, e_b_trp_ajlos_3_5_22)
matrix colnames est_msaj12los = LOS1_NoPoly LOS2_NoPoly LOS1_Poly LOS2_Poly

esttab matrix(est_msaj12los) using "los_msaj12_23.html", replace 
<</dd_do>>
~~~~

The lengths of stay are presented here:

<<dd_include: "los_msaj12_23.html" >>


<<dd_display: "Saved at= `c(current_time)' `c(current_date)'">>

~~~~
<<dd_do:nocommand>>
	cap qui noi frame drop example1
	cap qui noi frame drop example2
	cap qui noi frame drop example3
	cap qui noi frame drop example_a
	cap qui noi frame drop example_b
	cap qui save "ser_2023_0.dta", all replace emptyok /* Before it was b2, but i want to keep it in case an error*/
<</dd_do>>
~~~~

   
<<dd_do: nocommand>>
/*
FORMA DE EXPORTAR LOS DATOS Y EL MARKDOWN

cap rm "E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/an_ser_2023_step_0.html"
dyndoc "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\SER 2023_2_step0.do", saving("E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\an_ser_2023_step_0.html") replace nostop 
copy "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\an_ser_2023_step_0.html" "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\_outputs\an_ser_2023_step_0.html", replace

cap rm "C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/an_ser_2023_step_0.html"
dyndoc "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\SER 2023_2_step0.do", saving("C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\an_ser_2023_step_0.html") replace nostop 
copy "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\an_ser_2023_step_0.html" "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\_outputs\an_ser_2023_step_0.html", replace

_outputs
*/
<</dd_do>>
