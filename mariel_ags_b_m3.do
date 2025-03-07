<<dd_version: 2>>
<<dd_include: "E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/header.txt" >>
<<dd_include: "H:/Mi unidad/Alvacast/SISTRAT 2022 (github)/header.txt" >>
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
cap noi which estwrite 
if _rc==111 {		
	ssc install estwrite
	}

cap noi ssc install moremata 

cap noi which esttab
if _rc==111 {		
	ssc install estout
	}
<</dd_do>>
~~~~

# Exercise

Date created: <<dd_display: "`c(current_time)' `c(current_date)'">>.

Get the folder

~~~~
<<dd_do: nocommand>>
* codebook, compact

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


The file is located and named as: <<dd_display: "`c(pwd)'fiscalia_mariel_feb_2023_match_SENDA_miss.dta" >>
 

=============================================================================
## Structure database
=============================================================================

 
We open the files

~~~~
<<dd_do>>
use "fiscalia_mariel_feb_2023_match_SENDA_miss.dta", clear

**What Mariel requested

frame create original
frame original: use "fiscalia_mariel_jan_2023_match_SENDA.dta", clear
frame original: 

frlink 1:1 hash_key, frame(original)
frget 	origen_ingreso_mod, from(original)

drop if origen_ingreso_mod ==4

*b) select 10% of the data
/*
set seed 2125
sample 10
*/


fs mariel_ags_*.do
di "`r(dofile)'"

*tostring tr_modality, gen(tr_modality_str)

cap noi encode tr_modality_str, gen(newtr_modality)
cap confirm variable newtr_modality
    if !_rc {		
cap noi drop tr_modality
cap noi rename newtr_modality tr_modality
	}

cap noi encode condicion_ocupacional_cor, gen(newcondicion_ocupacional_cor)
cap confirm variable newcondicion_ocupacional_cor
    if !_rc {		
cap noi drop condicion_ocupacional_cor
cap noi rename newcondicion_ocupacional_cor condicion_ocupacional_cor
	}

cap noi encode tipo_centro, gen(newtipo_centro)
cap confirm variable newtipo_centro
    if !_rc {		
cap noi drop tipo_centro
cap noi rename newtipo_centro tipo_centro
	}

cap noi encode sus_ini_mod_mvv, gen(newsus_ini_mod_mvv)
cap confirm variable newsus_ini_mod_mvv
    if !_rc {		
cap noi drop sus_ini_mod_mvv
cap noi rename newsus_ini_mod_mvv sus_ini_mod_mvv
	}	
	
cap noi encode dg_trs_cons_sus_or, gen(newdg_trs_cons_sus_or)
cap confirm variable newdg_trs_cons_sus_or
    if !_rc {		
cap noi drop dg_trs_cons_sus_or
cap noi rename newdg_trs_cons_sus_or dg_trs_cons_sus_or
	}

cap noi encode con_quien_vive_joel, gen(newcon_quien_vive_joel)
cap confirm variable newcon_quien_vive_joel
    if !_rc {		
cap noi drop con_quien_vive_joel
cap noi rename newcon_quien_vive_joel con_quien_vive_joel
	}	

	
*order and encode	
cap noi decode freq_cons_sus_prin, gen(str_freq_cons_sus_prin)
cap confirm variable str_freq_cons_sus_prin
    if !_rc {	
cap noi drop freq_cons_sus_prin
label def freq_cons_sus_prin2 1 "Less than 1 day a week" 2 "1 day a week or more" 3 "2 to 3 days a week" 4 "4 to 6 days a week" 5 "Daily"
encode str_freq_cons_sus_prin, gen(freq_cons_sus_prin) label (freq_cons_sus_prin2)
	}
cap noi decode dg_trs_cons_sus_or, gen(str_dg_trs_cons_sus_or)
cap confirm variable str_dg_trs_cons_sus_or
    if !_rc {	
cap noi drop dg_trs_cons_sus_or
cap label def dg_trs_cons_sus_or2 1 "Hazardous consumption" 2 "Drug dependence"
encode str_dg_trs_cons_sus_or, gen(dg_trs_cons_sus_or) label (dg_trs_cons_sus_or2)
	}	
 
 
cap noi encode escolaridad_rec, gen(esc_rec)
cap noi encode sex, generate(sex_enc)
cap noi encode sus_principal_mod, gen(sus_prin_mod)
cap noi encode freq_cons_sus_prin, gen(fr_sus_prin)
cap noi encode compromiso_biopsicosocial, gen(comp_biosoc)
cap noi encode tenencia_de_la_vivienda_mod, gen(ten_viv)
*encode dg_cie_10_rec, generate(dg_cie_10_mental_h) *already numeric
cap noi encode dg_trs_cons_sus_or, gen(sud_severity_icd10)
cap noi encode macrozona, gen(macrozone)

/*
*2023-02-28, not done in R
cap noi recode numero_de_hijos_mod  (0=0 "No children") (1/10=1 "Children"), gen(newnumero_de_hijos_mod) 
cap confirm variable newnumero_de_hijos_mod
    if !_rc {	
drop numero_de_hijos_mod  
cap noi rename newnumero_de_hijos_mod numero_de_hijos_mod 
	}
*/
mkspline2 rc_x = edad_al_ing_1, cubic nknots(4) displayknots

*not necessary: 2023-02-28
*gen     motivodeegreso_mod_imp_rec3 = 1
*replace motivodeegreso_mod_imp_rec3 = 2 if strpos(motivodeegreso_mod_imp_rec,"Early")>0
*replace motivodeegreso_mod_imp_rec3 = 3 if strpos(motivodeegreso_mod_imp_rec,"Late")>0

*encode policonsumo, generate(policon) *already numeric
// Generate a restricted cubic spline variable for a variable "x" with 4 knots
*https://chat.openai.com/chat/4a9396cd-2caa-4a2e-b5f4-ed2c2d0779b3
*https://www.stata.com/meeting/nordic-and-baltic15/abstracts/materials/sweden15_oskarsson.pdf
*mkspline xspline = edad_al_ing_1, cubic nknots(4)
*gen rcs_x = xspline1*xspline2 xspline3 xspline4

*https://www.statalist.org/forums/forum/general-stata-discussion/general/1638622-comparing-cox-proportional-hazard-linear-and-non-linear-restricted-cubic-spline-models-using-likelihood-ratio-test

<</dd_do>>
~~~~

=============================================================================
## Survival
=============================================================================

**Reset-time**

~~~~
<<dd_do>>
*if missing offender_d (status) , means that there was a record and the time is the time of offense

*set the indicator
gen event=0
replace event=1 if !missing(offender_d)
*replace event=1 if !missing(sex)

*correct time to event if _st=0
gen diff= age_offending_imp-edad_al_egres_imp
gen diffc= cond(diff<0.001, 0.001, diff)
drop diff
rename diffc diff
lab var diff "Time to offense leading to condemnatory sentence" 

*age time
*stset age_offending_imp, fail(event ==1) enter(edad_al_egres_imp)
*reset time
stset diff, failure(event ==1) 

stdescribe, weight
<</dd_do>>
~~~~

We calculate the incidence rate.

~~~~
<<dd_do>>
stsum, by (motivodeegreso_mod_imp_rec)
<</dd_do>>
~~~~


~~~~
<<dd_do>>
*Micki Hill & Paul C Lambert & Michael J Crowther, 2021. "Introducing stipw: inverse probability weighted parametric survival models," London Stata Conference 2021 15, Stata Users Group.
*https://view.officeapps.live.com/op/view.aspx?src=http%3A%2F%2Ffmwww.bc.edu%2Frepec%2Fusug2021%2Fusug21_hill.pptx&wdOrigin=BROWSELINK

*Treatment variable should be a binary variable with values 0 and 1.
gen     motivodeegreso_mod_imp_rec2 = 0
replace motivodeegreso_mod_imp_rec2 = 1 if motivodeegreso_mod_imp_rec==2
replace motivodeegreso_mod_imp_rec2 = 1 if motivodeegreso_mod_imp_rec==3

recode motivodeegreso_mod_imp_rec2 (0=1 "Tr Completion") (1=0 "Tr Non-completion (Late & Early)"), gen(caus_disch_mod_imp_rec) 

cap noi gen motegr_dum3= motivodeegreso_mod_imp_rec2 
replace motegr_dum3 = 0 if motivodeegreso_mod_imp_rec==2
cap noi gen motegr_dum2= motivodeegreso_mod_imp_rec2 
replace motegr_dum2 = 0 if motivodeegreso_mod_imp_rec==3
lab var motegr_dum3 "Baseline treatment outcome(dich, 1= Late Dropout)" 
lab var motegr_dum2 "Baseline treatment outcome(dich, 1= Early Dropout)" 
lab var caus_disch_mod_imp_rec "Baseline treatment outcome(dich)" 


*Factor variables not allowed for tvc() option. Create your own dummy varibles.
gen     motivodeegreso_mod_imp_rec_earl = 1
replace motivodeegreso_mod_imp_rec_earl  = 0 if motivodeegreso_mod_imp_rec==1
replace motivodeegreso_mod_imp_rec_earl  = 0 if motivodeegreso_mod_imp_rec==3

gen     motivodeegreso_mod_imp_rec_late = 1
replace motivodeegreso_mod_imp_rec_late  = 0 if motivodeegreso_mod_imp_rec==1
replace motivodeegreso_mod_imp_rec_late  = 0 if motivodeegreso_mod_imp_rec==2

*recode motivodeegreso_mod_imp_rec_earl (1=1 "Early dropout") (0=0 "Tr. comp & Late dropout"), gen(newmotivodeegreso_mod_imp_rec_e)
*recode motivodeegreso_mod_imp_rec_late (1=1 "Late dropout") (0=0 "Tr. comp & Early dropout"), gen(newmotivodeegreso_mod_imp_rec_l)

lab var motivodeegreso_mod_imp_rec_earl "Baseline treatment outcome- Early dropout(dich)" 
lab var motivodeegreso_mod_imp_rec_late "Baseline treatment outcome- Late dropout(dich)" 

cap noi rename motivodeegreso_mod_imp_rec_late mot_egr_late
cap noi rename motivodeegreso_mod_imp_rec_earl mot_egr_early
<</dd_do>>
~~~~


=============================================================================
## Graph
=============================================================================

We generated a graph with every type of treatment and the Nelson-Aalen estimate.

~~~~
<<dd_do>>
sts graph, na by (motivodeegreso_mod_imp_rec) ci ///
title("Comission of an offense (cond. sentence)") /// 
subtitle("Nelson-Aalen Cum Hazards w/ Confidence Intervals 95%") ///
risktable(, size(*.5) order(1 "Tr Completion" 2 "Early Disch" 3 "Late Disch")) ///
ytitle("Cum. Hazards") ylabel(#8) ///
xtitle("Years since tr. outcome") xlabel(#8) ///
note("Source: nDP, SENDA's SUD Treatments & POs Office Data period 2010-2019 ") ///
legend(rows(3)) ///
legend(cols(4)) ///
graphregion(color(white) lwidth(large)) bgcolor(white) ///
plotregion(fcolor(white)) graphregion(fcolor(white) ) /// //text(.5 1 "IR = <0.001") ///
legend(order(1 "95CI Tr Completion" 2 "Tr Completion" 3 "95CI Early Tr Disch" 4 "Early Tr Disch " 5 "95CI Late Tr Disch" 6 "Late Tr Disch" )size(*.5)region(lstyle(none)) region(c(none)) nobox)
graph save "`c(pwd)'\_figs\tto_2023_m3.gph", replace
<</dd_do>>
~~~~
		
<<dd_graph: saving("tto_2023_m3.svg") width(800) replace>>



=============================================================================
## Survival Analyses
=============================================================================

~~~~
<<dd_do>>
/*
vars_cov<-c("motivodeegreso_mod_imp_rec", "tr_modality", "edad_al_ing_1", "sex", "edad_ini_cons", "escolaridad_rec", "sus_principal_mod", "freq_cons_sus_prin", "condicion_ocupacional_corr", "policonsumo", "num_hijos_mod_joel_bin", "tenencia_de_la_vivienda_mod", "macrozona", "n_off_vio", "n_off_acq",  "n_off_sud", "n_off_oth", "dg_cie_10_rec", "dg_trs_cons_sus_or", "clas_r", "porc_pobr", "sus_ini_mod_mvv", "ano_nac_corr", "con_quien_vive_joel", "fis_comorbidity_icd_10")
*/

global covs "i.motivodeegreso_mod_imp_rec i.tr_modality i.sex_enc edad_ini_cons i.escolaridad_rec i.sus_principal_mod i.freq_cons_sus_prin i.condicion_ocupacional_cor i.policonsumo i.num_hijos_mod_joel_bin i.tenencia_de_la_vivienda_mod i.macrozona i.n_off_vio i.n_off_acq i.n_off_sud i.n_off_oth i.dg_cie_10_rec i.dg_trs_cons_sus_or i.clas_r porc_pobr i.sus_ini_mod_mvv ano_nac_corr i.con_quien_vive_joel i.fis_comorbidity_icd_10"


<</dd_do>>
~~~~

~~~~
<<dd_do>>
// VERIFY FIRST SPLINE VARIABLE IS THE ORIGINAL VARIABLE
assert float(edad_al_ing_1) == float(rc_x1)

// MODEL WITH FULL SPLINE
qui noi stcox  $covs rc*
estat ic
estimates store full_spline
scalar  ll_1= e(ll) 
// MODEL WITH ONLY LINEAR TERM
qui noi stcox  $covs rc_x1
estat ic
scalar  ll_2= e(ll) 
estimates store linear_term

lrtest full_spline linear_term

scalar ll_diff= round(`=scalar(ll_1)'-`=scalar(ll_2)',.01) 
di "Log-likelihood difference (spline - linear): `=scalar(ll_diff)'"

* the presence of censored observations makes it difficult to decide further among them. (This is partly due to the fact that both the Cox model and the parametric survival models assume that censoring is orthogonal to survival time, a mathematically handy assumption that is often demonstrably and seriously in error, and the actual data generation process for survival is often too unknown or too messy to simulate.) So in this context, reliance on LR tests or IC statistics is a fallback position.
<</dd_do>>
~~~~

<<dd_display: "Log-likelihood difference (spline - linear): `=scalar(ll_diff)'">>


=============================================================================
## Adjusted Survival Analyses
=============================================================================

In view of nonproportional hazards, we explored different shapes of time-dependent effects and baseline hazards.

~~~~
<<dd_do>>
*______________________________________________
*______________________________________________
* ADJUSTED ROYSTON PARMAR - NO STAGGERED ENTRY, BINARY TREATMENT (1-DROPOUT VS. 0-COMPLETION)

/*
vars_cov<-c("motivodeegreso_mod_imp_rec", "tr_modality", "edad_al_ing_1", "sex", "edad_ini_cons", "escolaridad_rec", "sus_principal_mod", "freq_cons_sus_prin", "condicion_ocupacional_corr", "policonsumo", "num_hijos_mod_joel_bin", "tenencia_de_la_vivienda_mod", "macrozona", "n_off_vio", "n_off_acq",  "n_off_sud", "n_off_oth", "dg_cie_10_rec", "dg_trs_cons_sus_or", "clas_r", "porc_pobr", "sus_ini_mod_mvv", "ano_nac_corr", "con_quien_vive_joel", "fis_comorbidity_icd_10")
*/

cap noi tab tr_modality, gen(tr_mod)
cap noi tab sex_enc, gen(sex_dum)
cap noi tab escolaridad_rec, gen(esc)
cap noi tab sus_principal_mod, gen(sus_prin)
cap noi tab freq_cons_sus_prin, gen(fr_cons_sus_prin)
cap noi tab condicion_ocupacional_cor, gen(cond_ocu)
cap noi tab num_hijos_mod_joel_bin, gen(num_hij)
cap noi tab tenencia_de_la_vivienda_mod, gen(tenviv)
cap noi tab macrozona, gen(mzone)
cap noi tab clas_r, gen(rural)
cap noi tab sus_ini_mod_mvv, gen(susini)
cap noi tab con_quien_vive_joel, gen(cohab)
cap noi tab fis_comorbidity_icd_10, gen(fis_com)
cap noi tab dg_cie_10_rec, gen(psy_com)
cap noi tab dg_trs_cons_sus_or, gen(dep)

/*
*NO LONGER USEFUL
local varslab "dg_fis_anemia dg_fis_card dg_fis_in_study dg_fis_enf_som dg_fis_ets dg_fis_hep_alc dg_fis_hep_b dg_fis_hep_cro dg_fis_inf dg_fis_otr_cond_fis_ries_vit dg_fis_otr_cond_fis dg_fis_pat_buc dg_fis_pat_ges_intrau dg_fis_trau_sec"
forvalues i = 1/14 {
	local v : word `i' of `varslab'
	di "`v'"
	gen `v'2= 0
	replace `v'2 =1 if `v'==2
}
*/

global covs_3b "mot_egr_early mot_egr_late i.tr_modality i.sex_enc edad_ini_cons i.escolaridad_rec i.sus_principal_mod i.freq_cons_sus_prin i.condicion_ocupacional_cor i.policonsumo i.num_hijos_mod_joel_bin i.tenencia_de_la_vivienda_mod i.macrozona i.n_off_vio i.n_off_acq i.n_off_sud i.n_off_oth i.dg_cie_10_rec i.dg_trs_cons_sus_or i.clas_r porc_pobr i.sus_ini_mod_mvv ano_nac_corr i.con_quien_vive_joel i.fis_comorbidity_icd_10 rc_x1 rc_x2 rc_x3"

*REALLY NEEDS DUMMY VARS
global covs_3b_pre_dum "mot_egr_early mot_egr_late tr_mod2 sex_dum2 edad_ini_cons esc1 esc2 sus_prin2 sus_prin3 sus_prin4 sus_prin5 fr_cons_sus_prin2 fr_cons_sus_prin3 fr_cons_sus_prin4 fr_cons_sus_prin5 cond_ocu2 cond_ocu3 cond_ocu4 cond_ocu5 cond_ocu6 policonsumo num_hij2 tenviv1 tenviv2 tenviv4 tenviv5 mzone2 mzone3 n_off_vio n_off_acq n_off_sud n_off_oth psy_com2 psy_com3 dep2 rural2 rural3 porc_pobr susini2 susini3 susini4 susini5 ano_nac_corr cohab2 cohab3 cohab4 fis_com2 fis_com3 rc_x1 rc_x2 rc_x3"

forvalues i=1/10 {
	forvalues j=1/7 {
qui noi stpm2 $covs_3b_pre_dum , scale(hazard) df(`i') eform tvc(mot_egr_early mot_egr_late) dftvc(`j') 
estimates  store m_nostag_rp`i'_tvc_`j'
	}
}

<</dd_do>>
~~~~

We obtained a summary of distributions by AICs and BICs.

~~~~
<<dd_do>>
*file:///G:/Mi%20unidad/Alvacast/SISTRAT%202019%20(github)/_supp_mstates/stata/1806.01615.pdf
*rcs - restricted cubic splines on log hazard scale
*rp - Royston-Parmar model (restricted cubic spline on log cumulative hazard scale)
qui count if _d == 1
	// we count the amount of cases with the event in the strata
	//we call the estimates stored, and the results...
estimates stat m_nostag_rp*, n(`r(N)')
	//we store in a matrix de survival
matrix stats_1=r(S)

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

mata : st_sort_matrix("stats_1", 5) // 5 AIC, 6 BIC
global st_rownames : rownames stats_1
*di "$st_rownames"
esttab matrix(stats_1) using "testreg_aic_bic_mrl_23_1_m3.csv", replace
esttab matrix(stats_1) using "testreg_aic_bic_mrl_23_1_m3.html", replace

*weibull: Log cumulative hazard is linear in log t: ln⁡𝐻(𝑡)=𝑘 ln⁡〖𝑡−〖k ln〗⁡𝜆 〗
*Splines generalize to (almost) any baseline hazard shape.
*Stable estimates on the log cumulative hazard scale.
*ln⁡𝐻(𝑡)=𝑠(ln⁡〖𝑡)−〖k ln〗⁡𝜆 〗

*corey979 (https://stats.stackexchange.com/users/72352/corey979), How to compare models on the basis of AIC?, URL (version: 2016-08-30): https://stats.stackexchange.com/q/232494
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}testreg_aic_bic_mrl_23_1_m3.html" >>


In the case of the more flexible parametric models (non-standard), we selected the models that showed the best trade-off between lower complexity and better fit. This is why we also considered the BIC. If a model with fewer parameters had greater or equal AIC (or differences lower than 4) but also had better BIC (<=3), we favoured the model with fewer parameters.

~~~~
<<dd_do>>

*The per(1000) option multiplies the hazard rate by 1000 as it is easier to interpret the rate per 1000 years than per person per year.

range tt 0 7 28

estimates replay m_nostag_rp8_tvc_1, eform
estimates restore m_nostag_rp8_tvc_1

predict h0, hazard timevar(tt) at(mot_egr_early 0 mot_egr_late 0) zeros ci per(1000)

predict h1, hazard timevar(tt) at(mot_egr_early 1 mot_egr_late 0) zeros ci per(1000)

predict h2, hazard timevar(tt) at(mot_egr_early 0 mot_egr_late 1) zeros ci per(1000)

<</dd_do>>
~~~~

~~~~
<<dd_do>>

sts gen km=s, by(motivodeegreso_mod_imp_rec)

gen zero=0

estimates restore m_nostag_rp8_tvc_1

// Marginal survival 
predict ms0, meansurv timevar(tt) at(mot_egr_early 0 mot_egr_late 0) ci 

predict ms1, meansurv timevar(tt) at(mot_egr_early 1 mot_egr_late 0) ci 

predict ms2, meansurv timevar(tt) at(mot_egr_early 0 mot_egr_late 1) ci 

twoway  (rarea ms0_lci ms0_uci tt, color(gs2%35)) ///             
                 (rarea ms1_lci ms1_uci tt, color(gs6%35)) ///
				 (rarea ms2_lci ms2_uci tt, color(gs10%25)) ///
				 (line km _t if motivodeegreso_mod_imp_rec==1 , sort connect(stairstep) lpattern(dash) lwidth(medthick) lcolor(gs2%50)) ///
				 (line km _t if motivodeegreso_mod_imp_rec==2 , sort connect(stairstep) lpattern(dash) lwidth(medthick) lcolor(gs6%50)) ///
				 (line km _t if motivodeegreso_mod_imp_rec==3 , sort connect(stairstep) lpattern(dash) lwidth(medthick) lcolor(gs10%50)) ///
                 (line ms0 tt, lcolor(gs2) lwidth(thick)) ///
                 (line ms1 tt, lcolor(gs6) lwidth(thick)) ///
				 (line ms2 tt, lcolor(gs10) lwidth(thick)) ///
                 ,xtitle("Years from treatment outcome") ///
                 ytitle("Probibability of avoiding sentence (standardized)") ///
                 legend(order( 4 "Tr. completion" 5 "Early dropout" 6 "Late dropout") ring(0) pos(1) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) )  /// //text(.5 1 "IR = <0.001") ///
                 name(km_vs_standsurv_pre, replace)
graph save "`c(pwd)'\_figs\h_m_ns_rp6tvc2_m3.gph", replace

<</dd_do>>
~~~~

<<dd_graph: saving("h_m_ns_rp6tvc2_m3.svg") width(800) replace>>

~~~~
<<dd_do>>
*https://www.pauldickman.com/software/stata/sex-differences/

estimates restore m_nostag_rp8_tvc_1

predictnl diff_ms = predict(meansurv timevar(tt)) - ///
                  predict(meansurv at(mot_egr_early 1 mot_egr_late 0) timevar(tt)) ///
                  if mot_egr_early==0, ci(diff_ms_l diff_ms_u)

predictnl diff_ms2 = predict(meansurv timevar(tt)) - ///
                  predict(meansurv at(mot_egr_early 0 mot_egr_late 1) timevar(tt)) ///
                  if mot_egr_late==0, ci(diff_ms2_l diff_ms2_u)
				  
predictnl diff_ms3 = predict(meansurv at(mot_egr_early 1 mot_egr_late 0) timevar(tt)) - ///
                  predict(meansurv at(mot_egr_early 0 mot_egr_late 1) timevar(tt)) ///
                  if mot_egr_late==0, ci(diff_ms3_l diff_ms3_u)

				  
twoway  (rarea diff_ms_l diff_ms_u tt, color(gs7%35)) ///     
				  (line diff_ms tt, lcolor(gs7) lwidth(thick)) ///
		(rarea diff_ms2_l diff_ms2_u tt, color(gs2%35)) ///     
				  (line diff_ms2 tt, lcolor(gs2) lwidth(thick)) ///		
		(rarea diff_ms3_l diff_ms3_u tt, color(gs10%25)) ///     				  
				  (line diff_ms3 tt, lcolor(gs10) lwidth(thick)) ///					  
				  (line zero tt, lcolor(black%20) lwidth(thick)) ///
				   ,xtitle("Years from treatment outcome") ///
                 ytitle("Differences  of avoiding sentence (standardized)") ///
                 legend(order( 2 "Early vs. tr. completion" 4 "Late dropout vs. tr. completion" 6 "Late vs. early dropout") ring(2) pos(1) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) /// //text(.5 1 "IR = <0.001") ///
                 name(surv_diffs, replace)
graph save "`c(pwd)'\_figs\h_m_ns_rp6_stddif_s_m3.gph", replace
				  /*
*https://pclambert.net/software/stpm2_standsurv/standardized_survival/
*https://pclambert.net/software/stpm2_standsurv/standardized_survival_rmst/
stpm2_standsurv, at1(male 0 stage2m 0 stage3m 0) ///
                  at2(male 1 stage2m = stage2 stage3m = stage3) timevar(temptime) ci contrast(difference)
				  */
<</dd_do>>
~~~~

<<dd_graph: saving("h_m_ns_rp6_stddiff_s_m3.svg") width(800) replace>>

~~~~
<<dd_do>>
/*
vars_cov<-c("motivodeegreso_mod_imp_rec", "tr_modality", "edad_al_ing_1", "sex", "edad_ini_cons", "escolaridad_rec", "sus_principal_mod", "freq_cons_sus_prin", "condicion_ocupacional_corr", "policonsumo", "num_hijos_mod_joel_bin", "tenencia_de_la_vivienda_mod", "macrozona", "n_off_vio", "n_off_acq",  "n_off_sud", "n_off_oth", "dg_cie_10_rec", "dg_trs_cons_sus_or", "clas_r", "porc_pobr", "sus_ini_mod_mvv", "ano_nac_corr", "con_quien_vive_joel", "fis_comorbidity_icd_10")
*/

*REALLY NEEDS DUMMY VARS
global covs_3b_dum "mot_egr_early mot_egr_late tr_mod2 sex_dum2 edad_ini_cons esc1 esc2 sus_prin2 sus_prin3 sus_prin4 sus_prin5 fr_cons_sus_prin2 fr_cons_sus_prin3 fr_cons_sus_prin4 fr_cons_sus_prin5 cond_ocu2 cond_ocu3 cond_ocu4 cond_ocu5 cond_ocu6 policonsumo num_hij2 tenviv1 tenviv2 tenviv4 tenviv5 mzone2 mzone3 n_off_vio n_off_acq n_off_sud n_off_oth psy_com2 psy_com3 dep2 rural2 rural3 porc_pobr susini2 susini3 susini4 susini5 ano_nac_corr cohab2 cohab3 cohab4 fis_com2 fis_com3 rc_x1 rc_x2 rc_x3"


estimates restore m_nostag_rp8_tvc_1

stpm2_standsurv, at1(mot_egr_early 0 mot_egr_late 0) at2(mot_egr_early 1 mot_egr_late 0) timevar(tt) ci contrast(difference) ///
     atvar(s_tr_comp s_early_drop) contrastvar(sdiff_tr_comp_early_drop)

stpm2_standsurv, at1(mot_egr_early 0 mot_egr_late 0) at2(mot_egr_early 0 mot_egr_late 1) timevar(tt) ci contrast(difference) ///
     atvar(s_tr_comp0 s_late_drop) contrastvar(sdiff_tr_comp_late_drop)

stpm2_standsurv, at1(mot_egr_early 1 mot_egr_late 0) at2(mot_egr_early 0 mot_egr_late 1) timevar(tt) ci contrast(difference) ///
     atvar(s_early_drop0 s_late_drop0) contrastvar(sdiff_early_late_drop)	

cap noi drop s_tr_comp0 s_early_drop0 s_late_drop0
twoway  (rarea s_tr_comp_lci s_tr_comp_uci tt, color(gs2%35)) ///             
                 (rarea s_early_drop_lci s_early_drop_uci tt, color(gs6%35)) ///
				 (rarea s_late_drop_lci s_late_drop_uci tt, color(gs10%35)) ///
				 (line km _t if motivodeegreso_mod_imp_rec==1 , sort connect(stairstep) lpattern(dash) lwidth(medthick) lcolor(gs2%35)) ///
				 (line km _t if motivodeegreso_mod_imp_rec==2 , sort connect(stairstep) lpattern(dash) lwidth(medthick) lcolor(gs6%35)) ///
				 (line km _t if motivodeegreso_mod_imp_rec==3 , sort connect(stairstep) lpattern(dash) lwidth(medthick) lcolor(gs10%50)) ///
                 (line s_tr_comp tt, lcolor(gs2) lwidth(thick)) ///
                 (line s_early_drop tt, lcolor(gs6) lwidth(thick)) ///
				 (line s_late_drop tt, lcolor(gs10) lwidth(thick)) ///
                 ,xtitle("Years from treatment outcome") ///
                 ytitle("Probibability of avoiding sentence (standardized)") ///
                 legend(order( 4 "Tr. completion" 5 "Early dropout" 6 "Late dropout") ring(0) pos(1) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) /// //text(.5 1 "IR = <0.001") ///
                 name(km_vs_standsurv, replace)
graph save "`c(pwd)'\_figs\h_m_ns_rp6_s_m3.gph", replace

<</dd_do>>
~~~~

<<dd_graph: saving("h_m_ns_rp6_s_m3.svg") width(800) replace>>

~~~~
<<dd_do>>

twoway  (rarea sdiff_tr_comp_early_drop_lci sdiff_tr_comp_early_drop_uci tt, color(gs2%35)) ///
                 (line sdiff_tr_comp_early_drop tt, lcolor(gs2)) ///
		(rarea sdiff_tr_comp_late_drop_lci sdiff_tr_comp_late_drop_uci tt, color(gs6%35)) ///
                 (line sdiff_tr_comp_late_drop tt, lcolor(gs6)) ///
		(rarea sdiff_early_late_drop_lci sdiff_early_late_drop_uci tt, color(gs10%35)) ///
                 (line sdiff_early_late_drop tt, lcolor(gs10)) ///				 
         				  (line zero tt, lcolor(black%20) lwidth(thick)) ///
         , ylabel(, format(%3.1f)) ///
         ytitle("Difference in Survival (years)") ///
         xtitle("Years from baseline treatment outcome") ///
		 legend(order( 1 "Early vs. Tr. completion" 3 "Late vs. Tr. completion" 5 "Late vs. Early dropout") ring(0) pos(7) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) /// //text(.5 1 "IR = <0.001") ///
                 name(s_diff, replace)
		gr_edit yaxis1.major.label_format = `"%9.2f"'

graph save "`c(pwd)'\_figs\h_m_ns_rp6_stdif_s2_m3.gph", replace
<</dd_do>>
~~~~

<<dd_graph: saving("h_m_ns_rp6_stdif_s2_m3.svg") width(800) replace>>

~~~~
<<dd_do>>

estimates restore m_nostag_rp8_tvc_1

stpm2_standsurv, at1(mot_egr_early 0 mot_egr_late 0) at2(mot_egr_early 1 mot_egr_late 0) timevar(tt) rmst ci contrast(difference) ///
     atvar(rmst_h0 rmst_h1) contrastvar(rmstdiff_tr_comp_early_drop)

stpm2_standsurv, at1(mot_egr_early 0 mot_egr_late 0) at2(mot_egr_early 0 mot_egr_late 1) timevar(tt) rmst ci contrast(difference) ///
     atvar(rmst_h00 rmst_h2) contrastvar(rmstdiff_tr_comp_late_drop)

stpm2_standsurv, at1(mot_egr_early 1 mot_egr_late 0) at2(mot_egr_early 0 mot_egr_late 1) timevar(tt) rmst ci contrast(difference) ///
     atvar(rmst_h11 rmst_h22) contrastvar(rmstdiff_early_late_drop)	
	 
cap noi drop rmst_h00 rmst_h11 rmst_h22
twoway  (rarea rmstdiff_tr_comp_early_drop_lci rmstdiff_tr_comp_early_drop_uci tt, color(gs2%35)) ///
                 (line rmstdiff_tr_comp_early_drop tt, lcolor(gs2)) ///
		(rarea rmstdiff_tr_comp_late_drop_lci rmstdiff_tr_comp_late_drop_uci tt, color(gs6%35)) ///
                 (line rmstdiff_tr_comp_late_drop tt, lcolor(gs6)) ///
		(rarea rmstdiff_early_late_drop_lci rmstdiff_early_late_drop_uci tt, color(gs10%35)) ///
                 (line rmstdiff_early_late_drop tt, lcolor(gs10)) ///				 
         				  (line zero tt, lcolor(black%20) lwidth(thick)) ///
         , ylabel(, format(%3.1f)) ///
         ytitle("Difference in RMST (years)") ///
         xtitle("Years from baseline treatment outcome") ///
		 legend(order( 1 "Early vs. Tr. completion" 3 "Late vs. Tr. completion" 5 "Late vs. Early dropout") ring(0) pos(7) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) /// //text(.5 1 "IR = <0.001") ///
                 name(RMSTdiff, replace)
graph save "`c(pwd)'\_figs\h_m_ns_rp6_stdif_rmst_m3.gph", replace
<</dd_do>>
~~~~

<<dd_graph: saving("h_m_ns_rp6_stdif_rmst_m3.svg") width(800) replace>>


~~~~
<<dd_do:nocommand>>
	estwrite _all using "mariel_feb_23_m3.sters", replace

	cap qui save "mariel_feb_23_m3.dta", all replace emptyok
<</dd_do>>
~~~~

