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


The file is located and named as: <<dd_display: "`c(pwd)'fiscalia_mariel_jan_2023_match_SENDA.dta" >>
 

=============================================================================
## Structure database and survival
=============================================================================

 
We open the files

~~~~
<<dd_do>>
use "an_grant_23_24_miss.dta", clear
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
replace clas = 1 if strpos(clasificacion,"Mixta")>0
replace clas = 2 if strpos(clasificacion,"Rural")>0
recode clas (0=0 "Urban") (1=1 "Mixed") (2=2 "Rural"), gen(clas2) 
drop clas

rename clas2 clas

lab var clas "Classification of Municipallities of Residence into Rural-Urban" 
lab var offender_d "Offenders (proxy of event; missing not event)" 

lab var porc_pobr "Poverty of municipallities of Residence (numeric)" 
lab var comuna_residencia_cod_rec "Municipallity of Residence (code)" 

drop clasificacion

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

lab var event_offense "Offense after admission" 
lab var event_dropout "Dropout after admission" 
 

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


//id in numeric format
*encode hash_key, gen(id)
*gen id= encode(hash_key)
egen long id = group(hash_key)
lab var id "HASH (numeric)" 

//Sort variables
order id hash_key time_to_drop_from_adm event_dropout time_to_off_from_adm event_offense edad_al_ing_1 age_dropout_imp age_offending_imp
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
stset time_to_off_from_adm, enter(_start) failure(event_offense==1) //*scale(12) 

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

//IR for no PSU, from mat ir
scalar ir_poly_00= string(`=scalar(round(pois_ir_t0_nowgt[1,1]*1000,.01))',"%9.2f")+" (95%CI: "+string(`=scalar(round(pois_ir_t0_nowgt[5,1]*1000,.01))',"%9.2f")+", "+string(`=scalar(round(pois_ir_t0_nowgt[6,1]*1000,.01))',"%9.2f")+")" 
//IR for PSU, from mat ir
scalar ir_poly_11= string(`=scalar(round(pois_ir_t0_nowgt[1,2]*1000,.01))',"%9.2f")+" (95%CI: "+string(`=scalar(round(pois_ir_t0_nowgt[5,2]*1000,.01))',"%9.2f")+", "+string(`=scalar(round(pois_ir_t0_nowgt[6,2]*1000,.01))',"%9.2f")+")" 
//IRR for PSU, from mat irr (no * 1000)
scalar irr_poly01= string(`=scalar(round(pois_irr_t0_nowgt[1,2],.01))',"%9.2f")+" (95%CI: "+string(`=scalar(round(pois_irr_t0_nowgt[5,2],.01))',"%9.2f")+", "+string(`=scalar(round(pois_irr_t0_nowgt[6,2],.01))',"%9.2f")+")" 
<</dd_do>>
~~~~

~~~~
<<dd_do>> 
*set trace on //string(exp(`m_m9'),"%9.2f")+
cap noi qui sts test poly, logrank
scalar logrank_chi= string(round(r(chi2),.01),"%9.2f") 
scalar logrank_df= r(df)
scalar logrank_p=  round(chiprob(r(df),r(chi2)),.001)  // round(chiprob(r(df),r(chi2)),.001)
local lr1: di %3.2f logrank_chi
local lr2: di %1.0f logrank_df
local lr3: di %5.4f logrank_p
scalar logrank_nowgt= " Chi^2(`lr2')=`lr1',p=`lr3'"
*di logrank_nowgt

matrix comb_irs = (0 \ 0 \ 0 \ 0)
matrix colnames comb_irs = "Tr.comp-no weight"
matrix rownames comb_irs = "No PSU: `=scalar(ir_poly_00)'" "PSU: `=scalar(ir_poly_11)'" "IRR: `=scalar(irr_poly01)'" "Logrank: `=scalar(logrank_nowgt)'"
esttab matrix(comb_irs) using "irrs_t0_nowgt_tr_comp_jul_2023.html", replace
*set trace off
<</dd_do>>
~~~~

<<dd_include: "irrs_t0_nowgt_tr_comp_jul_2023.html" >>

We calculated the inverse probability weights

~~~
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
stset time_to_off_from_adm [pw=ipw_wgt], enter(_start) failure(event_offense==1) //*scale(12) 

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
frame example_a: stset time_to_off_from_adm [pw=HAW], enter(_start) failure(event_offense==1) //*scale(12) 

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

//IR for no PSU, from mat ir
scalar ir_poly_001= string(`=scalar(round(pois_ir_t0_wgt[1,1]*1000,.01))',"%9.2f")+" (95%CI: "+string(`=scalar(round(pois_ir_t0_wgt[5,1]*1000,.01))',"%9.2f")+", "+string(`=scalar(round(pois_ir_t0_wgt[6,1]*1000,.01))',"%9.2f")+")" 
//IR for PSU, from mat ir
scalar ir_poly_111= string(`=scalar(round(pois_ir_t0_wgt[1,2]*1000,.01))',"%9.2f")+" (95%CI: "+string(`=scalar(round(pois_ir_t0_wgt[5,2]*1000,.01))',"%9.2f")+", "+string(`=scalar(round(pois_ir_t0_wgt[6,2]*1000,.01))',"%9.2f")+")" 
//IRR for PSU, from mat irr (no * 1000)
scalar irr_poly011= string(`=scalar(round(pois_irr_t0_wgt[1,2],.01))',"%9.2f")+" (95%CI: "+string(`=scalar(round(pois_irr_t0_wgt[5,2],.01))',"%9.2f")+", "+string(`=scalar(round(pois_irr_t0_wgt[6,2],.01))',"%9.2f")+")" 

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
esttab matrix(comb_irs2) using "irrs_t0_wgt_tr_comp_jul_2023.html", replace
*set trace off
<</dd_do>>
~~~~

<<dd_include: "irrs_t0_wgt_tr_comp_jul_2023.html" >>

~~~~
<<dd_do>>
frame example_a: stdescribe, weight
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
stset time_to_off_from_adm [pw=HAW], enter(_start) failure(event_offense==1) id(id) //*scale(12) 

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
scalar poly_b_rr= string(round(r(rratio),.01),"%9.2f")  // round( r(rratio), .01)
scalar poly_b_rr_lb= string(round(r(lb),.01),"%9.2f")  //  
scalar poly_b_rr_ub= string(round(r(ub),.01),"%9.2f")  //  
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
scalar ir2_`s' = string(round(r(irr),.01),"%9.2f") // round(r(irr),.01)
*di ir_`s'
scalar ir2_`s'_lb = string(round(r(lb_irr),.01),"%9.2f") 
*di ir_`s'_lb
scalar ir2_`s'_ub = string(round(r(ub_irr),.01),"%9.2f")  
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
esttab matrix(comb_irs6) using "irrs_t0_wgt_tr_comp2_jul_2023.html", replace
*set trace off
<</dd_do>>
~~~~

<<dd_include: "irrs_t0_wgt_tr_comp2.html" >>


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
graph save "Graph" "`c(pwd)'\_figs\pbal2_jul_2023.gph", replace

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

graph export "`c(pwd)'\_figs\pbal2_mod_jul_2023.jpg", as(jpg) replace width(2000) height(1333)
graph export "`c(pwd)'\_figs\pbal2_mod_jul_2023.png", as(png) replace width(1800) height(1000)
graph export "`c(pwd)'\_figs\pbal2_mod_jul_2023.eps", as(eps) replace
graph export "`c(pwd)'\_figs\pbal2_mod_jul_2023.pdf", as(pdf) replace //*width(2000) height(2000) orientation(landscape)
*graph export "_Appendix2_Graph_Mean_SE_g32.svg", as(svg) replace height(20000) fontface (Helvetica)
graph save "`c(pwd)'\_figs\pbal2_mod_jul_2023", replace
graph save "Graph" "`c(pwd)'\_figs\pbal2_mod_jul_2023.gph", replace
//graph use "pbal2_mod"

<</dd_do>>
~~~~

<<dd_graph: saving("pbal2_mod_jul_2023.svg") width(800) replace>>

Cocaine paste base and cocaine hydrochloride were the only variables that were not well adjusted if resticted the sample to the 5% and 95%.


<<dd_display: "Saved at= `c(current_time)' `c(current_date)'">>

~~~~
<<dd_do:nocommand>>
	cap qui noi frame drop example1
	cap qui noi frame drop example2
	cap qui noi frame drop example3
	cap qui noi frame drop example_a
	cap qui noi frame drop example_b
	cap qui save "ser_2023_3_0.dta", all replace emptyok /* Before it was b2, but i want to keep it in case an error*/
<</dd_do>>
~~~~

   
<<dd_do: nocommand>>
/*
FORMA DE EXPORTAR LOS DATOS Y EL MARKDOWN

cap rm "E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/an_ser_2023_step_0_aug_23.html"
dyndoc "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\SER_2023_3_step0.do", saving("E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\an_ser_2023_step_0_aug_23.html") replace nostop 
copy "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\an_ser_2023_step_0_aug_23.html" "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\_outputs\an_ser_2023_step_0_aug_23.html", replace

cap rm "C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/an_ser_2023_step_0_aug_23.html"
dyndoc "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\SER_2023_3_step0.do", saving("C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\an_ser_2023_step_0_aug_23.html") replace nostop 
copy "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\an_ser_2023_step_0_aug_23.html" "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\_outputs\an_ser_2023_step_0_aug_23.html", replace



_outputs
*/
<</dd_do>>
