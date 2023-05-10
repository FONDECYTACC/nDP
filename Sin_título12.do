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
scalar irr_poly01 = "`=scalar(round(pois_irr_t0_nowgt[1,2]*1000,.01))' (95%CI: `=scalar(round(pois_irr_t0_nowgt[5,2]*1000,.01))',`=scalar(round(pois_irr_t0_nowgt[6,2]*1000,.01))')"

<</dd_do>>
~~~~

~~~~
<<dd_do>>
*set trace on
cap noi qui sts test poly, logrank
scalar logrank_chi= round(r(chi2),.01)
scalar logrank_df= r(df)
scalar logrank_p_= round(chiprob(r(df),r(chi2)),.001)
local lr1: di %3.2f logrank_chi
local lr2: di %1.0f logrank_df
local lr3: di %5.4f logrank_p_
scalar logrank_nowgt= " Chi^2(`lr2')=`lr1',p=`lr3'"
*di logrank_nowgt

matrix comb_irs = (0 \ 0 \ 0 \ 0)
matrix colnames comb_irs = IRs_t0
matrix rownames comb_irs = "No poly: `=scalar(ir_poly_0)'" "Poly: `=scalar(ir_poly_1)'" "IRR: `=scalar(irr_poly01)'" "Logrank: `=scalar(logrank_nowgt)'"
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
scalar irr_poly011 = "`=scalar(round(pois_irr_t0_wgt[1,2]*1000,.01))' (95%CI: `=scalar(round(pois_irr_t0_wgt[5,2]*1000,.01))',`=scalar(round(pois_irr_t0_wgt[6,2]*1000,.01))')"

<</dd_do>>
~~~~

~~~~
<<dd_do>>
*set trace on
cap noi qui sts test poly
scalar logrank_chi= round(r(chi2),.01)
scalar logrank_df= r(df)
scalar logrank_p_= round(chiprob(r(df),r(chi2)),.001)
local lr11: di %3.2f logrank_chi
local lr21: di %1.0f logrank_df
local lr31: di %5.4f logrank_p_
scalar logrank_wgt1 = "Cox regression-based test for equality of survival curves: Wald Chi^2(`lr21')=`lr11',p=`lr31'"
*di logrank_nowgt

matrix comb_irs2 = (0 \ 0 \ 0 \ 0)
matrix colnames comb_irs2 = IRs_t0
matrix rownames comb_irs2 = "No poly: `=scalar(ir_poly_01)'" "Poly: `=scalar(ir_poly_11)'" "IRR: `=scalar(irr_poly011)'" "Logrank: `=scalar(logrank_wgt1)'"
esttab matrix(comb_irs) using "irrs_t0_wgt_tr_comp.html", replace
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
scalar irr_poly012 = "`=scalar(round(pois_irr_t0_nowgt2[1,2]*1000,.01))' (95%CI: `=scalar(round(pois_irr_t0_nowgt2[5,2]*1000,.01))',`=scalar(round(pois_irr_t0_nowgt2[6,2]*1000,.01))')"

<</dd_do>>
~~~~

~~~~
<<dd_do>>
*set trace on
cap noi qui sts test poly, logrank
scalar logrank_chi= round(r(chi2),.01)
scalar logrank_df= r(df)
scalar logrank_p_= round(chiprob(r(df),r(chi2)),.001)
local lr12: di %3.2f logrank_chi
local lr22: di %1.0f logrank_df
local lr32: di %5.4f logrank_p_
scalar logrank_nowgt2= " Chi^2(`lr22')=`lr12',p=`lr32'"
*di logrank_nowgt

matrix comb_irs2 = (0 \ 0 \ 0 \ 0)
matrix colnames comb_irs2 = IRs_t0
matrix rownames comb_irs2 = "No poly: `=scalar(ir_poly_002)'" "Poly: `=scalar(ir_poly_112)'" "IRR: `=scalar(irr_poly012)'" "Logrank: `=scalar(logrank_nowgt2)'"
esttab matrix(comb_irs2) using "irrs_t0_nowgt_contact_js.html", replace
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
frame example_b: stset diff2 [pw=HAW2], enter(_start) failure(contact_js==1) //*scale(12) 

frame example_b: stsum, by (poly)
<</dd_do>>
~~~~

~~~~
<<dd_do>>
frame change example_a
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
scalar irr_poly013 = "`=scalar(round(pois_irr_t0_wgt3[1,2]*1000,.01))' (95%CI: `=scalar(round(pois_irr_t0_wgt3[5,2]*1000,.01))',`=scalar(round(pois_irr_t0_wgt3[6,2]*1000,.01))')"

<</dd_do>>
~~~~

~~~~
<<dd_do>>
*set trace on
cap noi qui sts test poly
scalar logrank_chi= round(r(chi2),.01)
scalar logrank_df= r(df)
scalar logrank_p_= round(chiprob(r(df),r(chi2)),.001)
local lr13: di %3.2f logrank_chi
local lr23: di %1.0f logrank_df
local lr33: di %5.4f logrank_p_
scalar logrank_wgt3 = "Cox regression-based test for equality of survival curves: Wald Chi^2(`lr23')=`lr13',p=`lr33'"
*di logrank_nowgt

matrix comb_irs3 = (0 \ 0 \ 0 \ 0)
matrix colnames comb_irs3 = IRs_t0
matrix rownames comb_irs3 = "No poly: `=scalar(ir_poly_003)'" "Poly: `=scalar(ir_poly_113)'" "IRR: `=scalar(irr_poly013)'" "Logrank: `=scalar(logrank_wgt3)'"
esttab matrix(comb_irs3) using "irrs_t0_wgt_contact_js.html", replace
*set trace off
<</dd_do>>
~~~~

<<dd_include: "irrs_t0_wgt_contact_js.html" >>

~~~~
<<dd_do>>
frame example_b: stdescribe, weight
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
scalar poly_rr= round( r(rratio), .01)
scalar poly_rr_lb= round(r(lb),.01) 
scalar poly_rr_ub= round(r(ub),.01) 
local ir1= poly_rr
local ir2= poly_rr_lb
local ir3= poly_rr_ub
global poly_irr " `title': IRR `ir1' (95%IC `ir2' - `ir3') "
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
global irr_`s' " `title': IRR (non weighted) `ir1' (IC 95% `ir2' - `ir3') "
global ir_`s' "`ir1' (IC 95% `ir2' - `ir3')"
}	
<</dd_do>>
~~~~

- <<dd_display: "$irr_2_1">>


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

<<dd_do: nocommand>>
/*
FORMA DE EXPORTAR LOS DATOS Y EL MARKDOWN

cap rm "E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/an_ser_2023_step_01.html"
dyndoc "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\SER Sin_título12.do", saving("E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\an_ser_2023_step_0.html") replace nostop 
copy "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\an_ser_2023_step_01.html" "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\_outputs\an_ser_2023_step_01.html", replace

cap rm "C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/an_ser_2023_step_01.html"
dyndoc "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\Sin_título12.do.do", saving("C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\an_ser_2023_step_01.html") replace nostop 
copy "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\an_ser_2023_step_01.html" "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\_outputs\an_ser_2023_step_01.html", replace

_outputs
*/
<</dd_do>>
