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


The file is located and named as: <<dd_display: "`c(pwd)'fiscalia_mariel_oct_2022_match_SENDA.dta" >>
 

=============================================================================
## Structure database
=============================================================================

 
We open the files

~~~~
<<dd_do>>
use "fiscalia_mariel_feb_2023_match_SENDA_miss.dta", clear

*b) select 5% of the data
/*
set seed 2125
sample 5
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
	
cap noi decode freq_cons_sus_prin, gen(str_freq_cons_sus_prin)
cap confirm variable str_freq_cons_sus_prin
    if !_rc {	
cap noi drop freq_cons_sus_prin
label def freq_cons_sus_prin2 1 "Less than 1 day a week" 2 "1 day a week or more" 3 "2 to 3 days a week" 4 "4 to 6 days a week" 5 "Daily"
encode str_freq_cons_sus_prin, gen(freq_cons_sus_prin) label (freq_cons_sus_prin2)
	}

cap noi encode numero_de_hijos_mod, gen(newnumero_de_hijos_mod)
*cap confirm variable newnumero_de_hijos_mod
    if _rc {		
cap noi gen newnumero_de_hijos_mod= cond(numero_de_hijos_mod>.5,1,0)
cap noi recode newnumero_de_hijos_mod  (0=0 "No children") (1/10=1 "Children"), gen(new2numero_de_hijos_mod) 

cap noi drop numero_de_hijos_mod newnumero_de_hijos_mod
cap noi rename new2numero_de_hijos_mod numero_de_hijos_mod
*similar to  table(ifelse(CONS_C1_df_dup_SEP_2020$numero_de_hijos_mod>0,1,0))
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

*encode policonsumo, generate(policon) *already numeric

*similar to table(ifelse(CONS_C1_df_dup_SEP_2020$numero_de_hijos_mod>0,1,0))
<</dd_do>>
~~~~

We show a table of missing values

~~~~
<<dd_do>>
misstable sum motivodeegreso_mod_imp_rec tr_modality edad_al_ing_1 sex_enc edad_ini_cons escolaridad_rec sus_principal_mod freq_cons_sus_prin condicion_ocupacional_cor policonsumo numero_de_hijos_mod tenencia_de_la_vivienda_mod macrozona n_off_vio n_off_acq n_off_sud n_off_oth clas_r porc_pobr sus_ini_mod_mvv dg_fis_anemia dg_fis_card dg_fis_in_study dg_fis_enf_som dg_fis_ets dg_fis_hep_alc dg_fis_hep_b dg_fis_hep_cro dg_fis_inf dg_fis_otr_cond_fis_ries_vit dg_fis_otr_cond_fis dg_fis_pat_buc dg_fis_pat_ges_intrau dg_fis_trau_sec
<</dd_do>>
~~~~

And missing patterns

~~~~
<<dd_do>>
misstable pat motivodeegreso_mod_imp_rec tr_modality edad_al_ing_1 sex_enc edad_ini_cons escolaridad_rec sus_principal_mod freq_cons_sus_prin condicion_ocupacional_cor policonsumo numero_de_hijos_mod tenencia_de_la_vivienda_mod macrozona n_off_vio n_off_acq n_off_sud n_off_oth clas_r porc_pobr sus_ini_mod_mvv dg_fis_anemia dg_fis_card dg_fis_in_study dg_fis_enf_som dg_fis_ets dg_fis_hep_alc dg_fis_hep_b dg_fis_hep_cro dg_fis_inf dg_fis_otr_cond_fis_ries_vit dg_fis_otr_cond_fis dg_fis_pat_buc dg_fis_pat_ges_intrau dg_fis_trau_sec
<</dd_do>>
~~~~

=============================================================================
## Survival
=============================================================================

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
stset age_offending_imp, fail(event ==1) enter(edad_al_egres_imp)

stdescribe, weight
<</dd_do>>
~~~~

We calculate the incidence rate.

~~~~
<<dd_do>>
stsum, by (motivodeegreso_mod_imp_rec)
<</dd_do>>
~~~~


=============================================================================
## Graph
=============================================================================

We generated a graph with every type of treatment and the Nelson-Aalen estimate.

~~~~
<<dd_do>>
sts graph, na by (motivodeegreso_mod_imp_rec) ci ///
title("Comission of an offense (end with cond. sent.)") /// 
subtitle("Nelson-Aalen Cum Hazards w/ Confidence Intervals 95%") ///
risktable(, size(*.5) order(1 "Tr Completion" 2 "Early Disch" 3 "Late Disch")) ///
ytitle("Cum. Hazards") ylabel(#8) ///
xtitle("Years of age") xlabel(#8) ///
note("Source: nDP, SENDA's SUD Treatments & POs Office Data period 2010-2019 ") ///
legend(rows(3)) ///
legend(cols(4)) ///
graphregion(color(white) lwidth(large)) bgcolor(white) ///
plotregion(fcolor(white)) graphregion(fcolor(white) ) /// //text(.5 1 "IR = <0.001") ///
legend(order(1 "95CI Tr Completion" 2 "Tr Completion" 3 "95CI Early Tr Disch" 4 "Early Tr Disch " 5 "95CI Late Tr Disch" 6 "Late Tr Disch" )size(*.5)region(lstyle(none)) region(c(none)) nobox)
graph save "`c(pwd)'\_figs\tto_2023_miss.gph", replace
<</dd_do>>
~~~~
		
<<dd_graph: saving("tto_2023_miss.svg") width(800) replace>>



=============================================================================
## Survival Analyses
=============================================================================

**Staggered entry**

We tested the schoefeld residuals.

~~~~
<<dd_do>>
global sim 1e5 //5e1 1e5 
global boots 1e3 //5e1 2e3
global times 0 90 365 1096 1826
range timevar0 90 1826 90

/*
vars_cov<-c("tr_modality", "edad_al_ing_1", "sex", "edad_ini_cons", "dias_treat_imp_sin_na_1", "escolaridad_rec", "sus_principal_mod", "freq_cons_sus_prin", "condicion_ocupacional_corr", "via_adm_sus_prin_act", "policonsumo", "origen_ingreso_mod", "numero_de_hijos_mod", "tenencia_de_la_vivienda_mod", "dg_cie_10_rec", "dg_trs_cons_sus_or", "macrozona", "n_off_vio", "n_off_acq", "n_off_sud", "n_off_oth", "clas_centers_r", "clas_r", "porc_pobr")
*sex_enc sud_severity_icd10 
*/

global covs "i.motivodeegreso_mod_imp_rec i.tr_modality i.sex_enc edad_ini_cons i.escolaridad_rec i.sus_principal_mod i.freq_cons_sus_prin i.condicion_ocupacional_cor i.policonsumo i.numero_de_hijos_mod i.tenencia_de_la_vivienda_mod i.macrozona i.n_off_vio i.n_off_acq i.n_off_sud i.n_off_oth i.clas_r porc_pobr i.sus_ini_mod_mvv dg_fis_anemia dg_fis_card dg_fis_enf_som dg_fis_ets dg_fis_hep_alc dg_fis_hep_b dg_fis_hep_cro dg_fis_inf dg_fis_otr_cond_fis_ries_vit dg_fis_otr_cond_fis dg_fis_pat_buc dg_fis_pat_ges_intrau dg_fis_trau_sec"


qui noi stcox  $covs , efron robust nolog schoenfeld(sch*) scaledsch(sca*)

qui noi estat phtest, log detail
mat mat_scho_test = r(phtest)
scalar chi2_scho_test = r(chi2)
scalar chi2_scho_test_df = r(df)
scalar chi2_scho_test_p = r(p)


esttab matrix(mat_scho_test) using "mat_scho_test_02_2023_miss.csv", replace
esttab matrix(mat_scho_test) using "mat_scho_test_02_2023_miss.html", replace

<</dd_do>>
~~~~

<<dd_display: "Chi^2(`=round(chi2_scho_test_df,.01)')= `=round(chi2_scho_test,.01)', p= `=round(chi2_scho_test_p,.0001)'">>

<<dd_include: "${pathdata2}mat_scho_test_02_2023_miss.html" >>

**Reset-time**

~~~~
<<dd_do>>
*reset time, only compatible with stteffects (same entry times)
stset diff, failure(event ==1) 
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

/*
vars_cov<-c("tr_modality", "edad_al_ing_1", "sex", "edad_ini_cons", "dias_treat_imp_sin_na_1", "escolaridad_rec", "sus_principal_mod", "freq_cons_sus_prin", "condicion_ocupacional_corr", "via_adm_sus_prin_act", "policonsumo", "origen_ingreso_mod", "numero_de_hijos_mod", "tenencia_de_la_vivienda_mod", "dg_cie_10_rec", "dg_trs_cons_sus_or", "macrozona", "n_off_vio", "n_off_acq", "n_off_sud", "n_off_oth", "clas_centers_r", "clas_r", "porc_pobr")
*sex_enc sud_severity_icd10 

*dias_treat_imp_sin_na_1 
*i.clas_centers_r 
*i.origen_ingreso_mod 
*i.via_adm_sus_prin_act 
*i.sud_severity_icd10
*/

global covs_3 "i.motivodeegreso_mod_imp_rec i.tr_modality edad_al_ing_1 i.sex_enc edad_ini_cons i.escolaridad_rec i.sus_principal_mod i.freq_cons_sus_prin i.condicion_ocupacional_cor i.policonsumo i.numero_de_hijos_mod i.tenencia_de_la_vivienda_mod i.macrozona i.n_off_vio i.n_off_acq i.n_off_sud i.n_off_oth i.clas_r porc_pobr i.sus_ini_mod_mvv dg_fis_anemia dg_fis_card dg_fis_enf_som dg_fis_ets dg_fis_hep_alc dg_fis_hep_b dg_fis_hep_cro dg_fis_inf dg_fis_otr_cond_fis_ries_vit dg_fis_otr_cond_fis dg_fis_pat_buc dg_fis_pat_ges_intrau dg_fis_trau_sec"

qui noi stcox  $covs_3 , efron robust nolog schoenfeld(sch_b*) scaledsch(sca_b*)
qui noi estat phtest, log detail
mat mat_scho_test2 = r(phtest)
scalar chi2_scho_test2 = r(chi2)
scalar chi2_scho_test2_df = r(df)
scalar chi2_scho_test2_p = r(p)
 
esttab matrix(mat_scho_test2) using "mat_scho_test_02_2023_2_miss.csv", replace
esttab matrix(mat_scho_test2) using "mat_scho_test_02_2023_2_miss.html", replace

<</dd_do>>
~~~~

<<dd_display: "Chi^2(`=round(chi2_scho_test2_df,.01)')= `=round(chi2_scho_test2,.01)', p= `=round(chi2_scho_test2_p,.0001)'">>

<<dd_include: "${pathdata2}mat_scho_test_02_2023_2_miss.html" >>


=============================================================================
## Adjusted Survival Analyses
=============================================================================

In view of nonproportional hazards, we explored different shapes of time-dependent effects and baseline hazards.

~~~~
<<dd_do>>
*______________________________________________
*______________________________________________
* ADJUSTED ROYSTON PARMAR - NO STAGGERED ENTRY, BINARY TREATMENT (1-DROPOUT VS. 0-COMPLETION)


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

global covs_3b "mot_egr_early mot_egr_late i.tr_modality edad_al_ing_1 i.sex_enc edad_ini_cons i.escolaridad_rec i.sus_principal_mod i.freq_cons_sus_prin i.condicion_ocupacional_cor i.policonsumo i.numero_de_hijos_mod i.tenencia_de_la_vivienda_mod i.macrozona i.n_off_vio i.n_off_acq i.n_off_sud i.n_off_oth i.clas_r porc_pobr i.sus_ini_mod_mvv dg_fis_anemia dg_fis_card dg_fis_enf_som dg_fis_ets dg_fis_hep_alc dg_fis_hep_b dg_fis_hep_cro dg_fis_inf dg_fis_otr_cond_fis_ries_vit dg_fis_otr_cond_fis dg_fis_pat_buc dg_fis_pat_ges_intrau dg_fis_trau_sec"

forvalues i=1/10 {
	forvalues j=1/7 {
qui noi stpm2 $covs_3b , scale(hazard) df(`i') eform tvc(mot_egr_early mot_egr_late) dftvc(`j') 
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
esttab matrix(stats_1) using "testreg_aic_bic_mrl_23_1.csv", replace
esttab matrix(stats_1) using "testreg_aic_bic_mrl_23_1.html", replace

*weibull: Log cumulative hazard is linear in log t: ln⁡𝐻(𝑡)=𝑘 ln⁡〖𝑡−〖k ln〗⁡𝜆 〗
*Splines generalize to (almost) any baseline hazard shape.
*Stable estimates on the log cumulative hazard scale.
*ln⁡𝐻(𝑡)=𝑠(ln⁡〖𝑡)−〖k ln〗⁡𝜆 〗

<</dd_do>>
~~~~

<<dd_include: "${pathdata2}testreg_aic_bic_mrl_23_1.html" >>


In case of the more flexible parametric models (non-standard), we selected the models that showed the best trade-off between lower complexity and better fit, and this is why we also considered the BIC. If a model with less parameters had greater or equal AIC (or differences lower than 2) but also had better BIC (<=2), we favoured the model with less parameters.

The baseline hazard function was fitted using restricted cubic splines with 6 degrees of freedom, generating 5 interior knots placed at equally-spaced percentiles (17, 33, 50, 67 & 83). To allow for non-proportional hazards, the time-dependent effect of treatment outcome was fitted using restricted cubic splines with 1 degrees of freedom.

~~~~
<<dd_do>>

*The per(1000) option multiplies the hazard rate by 1000 as it is easier to interpret the rate per 1000 years than per person per year.

range tt 0 7 28

estimates replay m_nostag_rp6_tvc_1
estimates restore m_nostag_rp6_tvc_1

predict h0, hazard timevar(tt) at(mot_egr_early 0 mot_egr_late 0) zeros ci per(1000)

predict h1, hazard timevar(tt) at(mot_egr_early 1 mot_egr_late 0) zeros ci per(1000)

predict h2, hazard timevar(tt) at(mot_egr_early 0 mot_egr_late 1) zeros ci per(1000)

twoway  (rarea h0_lci h0_uci tt, color(red%25)) ///             
                 (rarea h1_lci h1_uci tt, color(blue%25)) ///
				 (rarea h2_lci h2_uci tt, color(green%25)) ///
                 (line h0 tt, lcolor(red) lwidth(thick)) ///
                 (line h1 tt, lcolor(blue) lwidth(thick)) ///
				 (line h2 tt, lcolor(green) lwidth(thick)) ///
                 ,xtitle("Years from treatment outcome") ///
                 ytitle("Condemnatory sentence rate (per 1000 py) (covariates at 0)") ///
                 legend(order( 4 "Tr. completion" 5 "Early dropout" 6 "Late dropout") ring(0) pos(1) cols(1)region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) /// //text(.5 1 "IR = <0.001") ///
                 name(hazards_in_zeros, replace)
				 
graph save "`c(pwd)'\_figs\h_m_ns_rp6tvc1.gph", replace
<</dd_do>>
~~~~

<<dd_graph: saving("h_m_ns_rp6tvc1.svg") width(800) replace>>

~~~~
<<dd_do>>

sts gen km=s, by(motivodeegreso_mod_imp_rec)

gen zero=0

estimates restore m_nostag_rp6_tvc_1

// Marginal survival 
predict ms0, meansurv timevar(tt) at(mot_egr_early 0 mot_egr_late 0) ci 

predict ms1, meansurv timevar(tt) at(mot_egr_early 1 mot_egr_late 0) ci 

predict ms2, meansurv timevar(tt) at(mot_egr_early 0 mot_egr_late 1) ci 

twoway  (rarea ms0_lci ms0_uci tt, color(red%25)) ///             
                 (rarea ms1_lci ms1_uci tt, color(blue%25)) ///
				 (rarea ms2_lci ms2_uci tt, color(green%25)) ///
				 (line km _t if motivodeegreso_mod_imp_rec==1 , sort connect(stairstep) lpattern(dash) lwidth(medthick) lcolor(red%50)) ///
				 (line km _t if motivodeegreso_mod_imp_rec==2 , sort connect(stairstep) lpattern(dash) lwidth(medthick) lcolor(blue%50)) ///
				 (line km _t if motivodeegreso_mod_imp_rec==3 , sort connect(stairstep) lpattern(dash) lwidth(medthick) lcolor(green%50)) ///
                 (line ms0 tt, lcolor(red) lwidth(thick)) ///
                 (line ms1 tt, lcolor(blue) lwidth(thick)) ///
				 (line ms2 tt, lcolor(green) lwidth(thick)) ///
                 ,xtitle("Years from treatment outcome") ///
                 ytitle("Probibability of avoiding sentence (standardized)") ///
                 legend(order( 4 "Tr. completion" 5 "Early dropout" 6 "Late dropout") ring(0) pos(1) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) )  /// //text(.5 1 "IR = <0.001") ///
                 name(km_vs_standsurv, replace)
graph save "`c(pwd)'\_figs\h_m_ns_rp6tvc2.gph", replace

<</dd_do>>
~~~~

<<dd_graph: saving("h_m_ns_rp6tvc2.svg") width(800) replace>>

~~~~
<<dd_do>>
*https://www.pauldickman.com/software/stata/sex-differences/

estimates restore m_nostag_rp6_tvc_1

predictnl diff_ms = predict(meansurv timevar(tt)) - ///
                  predict(meansurv at(mot_egr_early 1 mot_egr_late 0) timevar(tt)) ///
                  if mot_egr_early==0, ci(diff_ms_l diff_ms_u)

predictnl diff_ms2 = predict(meansurv timevar(tt)) - ///
                  predict(meansurv at(mot_egr_early 0 mot_egr_late 1) timevar(tt)) ///
                  if mot_egr_late==0, ci(diff_ms2_l diff_ms2_u)
				  
predictnl diff_ms3 = predict(meansurv at(mot_egr_early 1 mot_egr_late 0) timevar(tt)) - ///
                  predict(meansurv at(mot_egr_early 0 mot_egr_late 1) timevar(tt)) ///
                  if mot_egr_late==0, ci(diff_ms3_l diff_ms3_u)

				  
twoway  (rarea diff_ms_l diff_ms_u tt, color(red%25)) ///     
				  (line diff_ms tt, lcolor(red) lwidth(thick)) ///
		(rarea diff_ms2_l diff_ms2_u tt, color(blue%25)) ///     
				  (line diff_ms2 tt, lcolor(blue) lwidth(thick)) ///		
		(rarea diff_ms3_l diff_ms3_u tt, color(green%25)) ///     				  
				  (line diff_ms3 tt, lcolor(green) lwidth(thick)) ///					  
				  (line zero tt, lcolor(black%20) lwidth(thick)) ///
				   ,xtitle("Years from treatment outcome") ///
                 ytitle("Differences  of avoiding sentence (standardized)") ///
                 legend(order( 2 "Early vs. tr. completion" 4 "Late dropout vs. tr. completion" 6 "Late vs. early dropout") ring(2) pos(1) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) /// //text(.5 1 "IR = <0.001") ///
                 name(surv_diffs, replace)
graph save "`c(pwd)'\_figs\h_m_ns_rp6_stddif_s.gph", replace

				  /*
*https://pclambert.net/software/stpm2_standsurv/standardized_survival/
*https://pclambert.net/software/stpm2_standsurv/standardized_survival_rmst/
stpm2_standsurv, at1(male 0 stage2m 0 stage3m 0) ///
                  at2(male 1 stage2m = stage2 stage3m = stage3) timevar(temptime) ci contrast(difference)
				  */
<</dd_do>>
~~~~

<<dd_graph: saving("h_m_ns_rp6_stddiff_s.svg") width(800) replace>>

~~~~
<<dd_do>>

cap noi tab tr_modality, gen(tr_mod)
cap noi tab sex_enc, gen(sex_dum)
cap noi tab escolaridad_rec, gen(esc)
cap noi tab sus_principal_mod, gen(sus_prin)
cap noi tab freq_cons_sus_prin, gen(fr_cons_sus_prin)
cap noi tab condicion_ocupacional_cor, gen(cond_ocu)
cap noi tab numero_de_hijos_mod, gen(num_hij)
cap noi tab tenencia_de_la_vivienda_mod, gen(tenviv)
cap noi tab macrozona, gen(mzone)
cap noi tab clas_r, gen(rural)
cap noi tab sus_ini_mod_mvv, gen(susini)

local varslab "dg_fis_anemia dg_fis_card dg_fis_in_study dg_fis_enf_som dg_fis_ets dg_fis_hep_alc dg_fis_hep_b dg_fis_hep_cro dg_fis_inf dg_fis_otr_cond_fis_ries_vit dg_fis_otr_cond_fis dg_fis_pat_buc dg_fis_pat_ges_intrau dg_fis_trau_sec"
forvalues i = 1/14 {
	local v : word `i' of `varslab'
	di "`v'"
	gen `v'2= 0
	replace `v'2 =1 if `v'==2
}

*REALLY NEEDS DUMMY VARS
global covs_3b_dum "mot_egr_early mot_egr_late tr_mod2 edad_al_ing_1 sex_dum2 edad_ini_cons esc1 esc2 sus_prin2 sus_prin3 sus_prin4 sus_prin5 fr_cons_sus_prin2 fr_cons_sus_prin3 fr_cons_sus_prin4 fr_cons_sus_prin5 cond_ocu2 cond_ocu3 cond_ocu4 cond_ocu5 cond_ocu6 policonsumo num_hij2 tenviv1 tenviv2 tenviv4 tenviv5 mzone2 mzone3 n_off_vio n_off_acq n_off_sud n_off_oth rural2 rural3 porc_pobr susini2 susini3 susini4 susini5 dg_fis_anemia2 dg_fis_card2 dg_fis_enf_som2 dg_fis_ets2 dg_fis_hep_alc2 dg_fis_hep_b2 dg_fis_hep_cro2 dg_fis_inf2 dg_fis_otr_cond_fis_ries_vit2 dg_fis_otr_cond_fis2 dg_fis_pat_buc2 dg_fis_pat_ges_intrau2 dg_fis_trau_sec2"

qui noi stpm2 $covs_3b_dum , scale(hazard) df(6) eform tvc(mot_egr_early mot_egr_late) dftvc(1) 
estimates store m_nostag_rp6_tvc_1_dum

estimates restore m_nostag_rp6_tvc_1_dum

stpm2_standsurv, at1(mot_egr_early 0 mot_egr_late 0) at2(mot_egr_early 1 mot_egr_late 0) timevar(tt) ci contrast(difference) ///
     atvar(s_tr_comp s_early_drop) contrastvar(sdiff_tr_comp_early_drop)

stpm2_standsurv, at1(mot_egr_early 0 mot_egr_late 0) at2(mot_egr_early 0 mot_egr_late 1) timevar(tt) ci contrast(difference) ///
     atvar(s_tr_comp0 s_late_drop) contrastvar(sdiff_tr_comp_late_drop)

stpm2_standsurv, at1(mot_egr_early 1 mot_egr_late 0) at2(mot_egr_early 0 mot_egr_late 1) timevar(tt) ci contrast(difference) ///
     atvar(s_early_drop0 s_late_drop0) contrastvar(sdiff_early_late_drop)	

cap noi drop s_tr_comp0 s_early_drop0 s_late_drop0
twoway  (rarea s_tr_comp_lci s_tr_comp_uci tt, color(red%25)) ///             
                 (rarea s_early_drop_lci s_early_drop_uci tt, color(blue%25)) ///
				 (rarea s_late_drop_lci s_late_drop_uci tt, color(green%25)) ///
				 (line km _t if motivodeegreso_mod_imp_rec==1 , sort connect(stairstep) lpattern(dash) lwidth(medthick) lcolor(red%50)) ///
				 (line km _t if motivodeegreso_mod_imp_rec==2 , sort connect(stairstep) lpattern(dash) lwidth(medthick) lcolor(blue%50)) ///
				 (line km _t if motivodeegreso_mod_imp_rec==3 , sort connect(stairstep) lpattern(dash) lwidth(medthick) lcolor(green%50)) ///
                 (line s_tr_comp tt, lcolor(red) lwidth(thick)) ///
                 (line s_early_drop tt, lcolor(blue) lwidth(thick)) ///
				 (line s_late_drop tt, lcolor(green) lwidth(thick)) ///
                 ,xtitle("Years from treatment outcome") ///
                 ytitle("Probibability of avoiding sentence (standardized)") ///
                 legend(order( 4 "Tr. completion" 5 "Early dropout" 6 "Late dropout") ring(0) pos(1) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) /// //text(.5 1 "IR = <0.001") ///
                 name(km_vs_standsurv, replace)
graph save "`c(pwd)'\_figs\h_m_ns_rp6_s.gph", replace

<</dd_do>>
~~~~

<<dd_graph: saving("h_m_ns_rp6_s.svg") width(800) replace>>

~~~~
<<dd_do>>

twoway  (rarea sdiff_tr_comp_early_drop_lci sdiff_tr_comp_early_drop_uci tt, color(blue%20)) ///
                 (line sdiff_tr_comp_early_drop tt, lcolor(blue)) ///
		(rarea sdiff_tr_comp_late_drop_lci sdiff_tr_comp_late_drop_uci tt, color(red%20)) ///
                 (line sdiff_tr_comp_late_drop tt, lcolor(red)) ///
		(rarea sdiff_early_late_drop_lci sdiff_early_late_drop_uci tt, color(green%20)) ///
                 (line sdiff_early_late_drop tt, lcolor(green)) ///				 
         				  (line zero tt, lcolor(black%20) lwidth(thick)) ///
         , ylabel(, format(%3.1f)) ///
         ytitle("Difference in Survival (years)") ///
         xtitle("Years from baseline treatment outcome") ///
		 legend(order( 1 "Early vs. Tr. completion" 3 "Late vs. Tr. completion" 5 "Late vs. Early dropout") ring(0) pos(7) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) /// //text(.5 1 "IR = <0.001") ///
                 name(s_diff, replace)
		gr_edit yaxis1.major.label_format = `"%9.2f"'

graph save "`c(pwd)'\_figs\h_m_ns_rp6_stdif_s2.gph", replace
<</dd_do>>
~~~~

<<dd_graph: saving("h_m_ns_rp6_stdif_s2.svg") width(800) replace>>

~~~~
<<dd_do>>

estimates restore m_nostag_rp6_tvc_1_dum

stpm2_standsurv, at1(mot_egr_early 0 mot_egr_late 0) at2(mot_egr_early 1 mot_egr_late 0) timevar(tt) rmst ci contrast(difference) ///
     atvar(rmst_h0 rmst_h1) contrastvar(rmstdiff_tr_comp_early_drop)

stpm2_standsurv, at1(mot_egr_early 0 mot_egr_late 0) at2(mot_egr_early 0 mot_egr_late 1) timevar(tt) rmst ci contrast(difference) ///
     atvar(rmst_h00 rmst_h2) contrastvar(rmstdiff_tr_comp_late_drop)

stpm2_standsurv, at1(mot_egr_early 1 mot_egr_late 0) at2(mot_egr_early 0 mot_egr_late 1) timevar(tt) rmst ci contrast(difference) ///
     atvar(rmst_h11 rmst_h22) contrastvar(rmstdiff_early_late_drop)	
	 
cap noi drop rmst_h00 rmst_h11 rmst_h22
twoway  (rarea rmstdiff_tr_comp_early_drop_lci rmstdiff_tr_comp_early_drop_uci tt, color(blue%20)) ///
                 (line rmstdiff_tr_comp_early_drop tt, lcolor(blue)) ///
		(rarea rmstdiff_tr_comp_late_drop_lci rmstdiff_tr_comp_late_drop_uci tt, color(red%20)) ///
                 (line rmstdiff_tr_comp_late_drop tt, lcolor(red)) ///
		(rarea rmstdiff_early_late_drop_lci rmstdiff_early_late_drop_uci tt, color(green%20)) ///
                 (line rmstdiff_early_late_drop tt, lcolor(green)) ///				 
         				  (line zero tt, lcolor(black%20) lwidth(thick)) ///
         , ylabel(, format(%3.1f)) ///
         ytitle("Difference in RMST (years)") ///
         xtitle("Years from baseline treatment outcome") ///
		 legend(order( 1 "Early vs. Tr. completion" 3 "Late vs. Tr. completion" 5 "Late vs. Early dropout") ring(0) pos(7) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) /// //text(.5 1 "IR = <0.001") ///
                 name(RMSTdiff, replace)
graph save "`c(pwd)'\_figs\h_m_ns_rp6_stdif_rmst.gph", replace
<</dd_do>>
~~~~

<<dd_graph: saving("_figs/h_m_ns_rp6_stdif_rmst.svg") width(800) replace>>


=============================================================================
### IPTW Royston-Parmar
=============================================================================

First we calculated the difference between those patients who did and did not complete baseline treatment, given that the analysis of stipw is restricted to 2 values and does not allow multi-valued treatments.

~~~~
<<dd_do>>
*______________________________________________
*______________________________________________
* NO STAGGERED ENTRY, BINARY TREATMENT (1-DROPOUT VS. 0-COMPLETION)

global covs_4_dum "motivodeegreso_mod_imp_rec2 tr_mod2 edad_al_ing_1 sex_dum2 edad_ini_cons esc1 esc2 sus_prin2 sus_prin3 sus_prin4 sus_prin5 fr_cons_sus_prin2 fr_cons_sus_prin3 fr_cons_sus_prin4 fr_cons_sus_prin5 cond_ocu2 cond_ocu3 cond_ocu4 cond_ocu5 cond_ocu6 policonsumo num_hij2 tenviv1 tenviv2 tenviv4 tenviv5 mzone2 mzone3 n_off_vio n_off_acq n_off_sud n_off_oth rural2 rural3 porc_pobr susini2 susini3 susini4 susini5 dg_fis_anemia dg_fis_card dg_fis_enf_som dg_fis_ets dg_fis_hep_alc dg_fis_hep_b dg_fis_hep_cro dg_fis_inf dg_fis_otr_cond_fis_ries_vit dg_fis_otr_cond_fis dg_fis_pat_buc dg_fis_pat_ges_intrau dg_fis_trau_sec"

*  tvar must be a binary variable with 1 = treatment/exposure and 0 = control.
rename motivodeegreso_mod_imp_rec2 mot_egr_imp_rec2

gen mot_egr_imp_rec= motivodeegreso_mod_imp_rec

tab mot_egr_imp_rec, gen(mot_egr_imp_rec_dum)

cap noi drop mot_egr_imp_rec

*mot_egr_imp_rec_dum2 = early; mot_egr_imp_rec_dum3= late

*exponential weibull gompertz lognormal loglogistic
*10481 observations have missing treatment and/or missing confounder values and/or _st = 0.
forvalues i=1/10 {
	forvalues j=1/7 {
qui noi stipw (logit mot_egr_imp_rec2 tr_mod2 edad_al_ing_1 sex_dum2 edad_ini_cons esc1 esc2 sus_prin2 sus_prin3 sus_prin4 sus_prin5 fr_cons_sus_prin2 fr_cons_sus_prin3 fr_cons_sus_prin4 fr_cons_sus_prin5 cond_ocu2 cond_ocu3 cond_ocu4 cond_ocu5 cond_ocu6 policonsumo num_hij2 tenviv1 tenviv2 tenviv4 tenviv5 mzone2 mzone3 n_off_vio n_off_acq n_off_sud n_off_oth rural2 rural3 porc_pobr susini2 susini3 susini4 susini5 dg_fis_anemia2 dg_fis_card2 dg_fis_enf_som2 dg_fis_ets2 dg_fis_hep_alc2 dg_fis_hep_b2 dg_fis_hep_cro2 dg_fis_inf2 dg_fis_otr_cond_fis_ries_vit2 dg_fis_otr_cond_fis2 dg_fis_pat_buc2 dg_fis_pat_ges_intrau2 dg_fis_trau_sec2), distribution(rp) df(`i') dftvc(`j') genw(rpdf`i'_m_nostag_tvcdf`j') ipwtype(stabilised) vce(mestimation) eform
estimates  store m_stipw_nostag_rp`i'_tvcdf`j'
	}
}

*https://core.ac.uk/download/pdf/6990318.pdf

*The following options are not permitted with streg models:
*bknots, bknotstvc, df, dftvc, failconvlininit, knots, knotstvc knscale, noorthorg, eform, alleq, keepcons, showcons, lininit
*forvalues j=1/7 {
local vars "exponential weibull gompertz lognormal loglogistic"
local varslab "exp wei gom logn llog"
forvalues i = 1/5 {
 local v : word `i' of `vars'
 local v2 : word `i' of `varslab'

qui noi stipw (logit mot_egr_imp_rec2 tr_mod2 edad_al_ing_1 sex_dum2 edad_ini_cons esc1 esc2 sus_prin2 sus_prin3 sus_prin4 sus_prin5 fr_cons_sus_prin2 fr_cons_sus_prin3 fr_cons_sus_prin4 fr_cons_sus_prin5 cond_ocu2 cond_ocu3 cond_ocu4 cond_ocu5 cond_ocu6 policonsumo num_hij2 tenviv1 tenviv2 tenviv4 tenviv5 mzone2 mzone3 n_off_vio n_off_acq n_off_sud n_off_oth rural2 rural3 porc_pobr susini2 susini3 susini4 susini5 dg_fis_anemia2 dg_fis_card2 dg_fis_enf_som2 dg_fis_ets2 dg_fis_hep_alc2 dg_fis_hep_b2 dg_fis_hep_cro2 dg_fis_inf2 dg_fis_otr_cond_fis_ries_vit2 dg_fis_otr_cond_fis2 dg_fis_pat_buc2 dg_fis_pat_ges_intrau2 dg_fis_trau_sec2), distribution(`v') genw(`v2'_m2_nostag) ipwtype(stabilised) vce(mestimation)
estimates  store m_stipw_nostag_`v2'
	}
*}

qui count if _d == 1
	// we count the amount of cases with the event in the strata
	//we call the estimates stored, and the results...
estimates stat m_stipw_nostag_*, n(`r(N)')
	//we store in a matrix de survival
matrix stats_2=r(S)
mata : st_sort_matrix("stats_2", 5) // 5 AIC, 6 BIC
esttab matrix(stats_2) using "testreg_aic_bic_mrl_23_2.csv", replace
esttab matrix(stats_2) using "testreg_aic_bic_mrl_23_2.html", replace

*m_stipw_nostag_rp5_tvcdf1 m_stipw_nostag_rp5_tvcdf1 confirmed
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}testreg_aic_bic_mrl_23_2.html" >>

~~~~
<<dd_do>>

estimates restore m_stipw_nostag_rp5_tvcdf1

stpm2_standsurv, at1(mot_egr_imp_rec2 0 ) at2(mot_egr_imp_rec2 1 ) timevar(tt) ci contrast(difference) ///
     atvar(s_comp_a s_nocomp_a) contrastvar(sdiff_comp_vs_nocomp)

stpm2_standsurv, at1(mot_egr_imp_rec2 0 ) at2(mot_egr_imp_rec2 1 ) timevar(tt) rmst ci contrast(difference) ///
     atvar(rmst_comp_a rmst_nocomp_a) contrastvar(rmstdiff_comp_vs_nocomp)

sts gen km_a=s, by(mot_egr_imp_rec2)
	 
twoway  (rarea s_comp_a_lci s_comp_a_uci tt, color(red%25)) ///             
                 (rarea s_nocomp_a_lci s_nocomp_a_uci tt, color(blue%25)) ///
				 (line km_a _t if mot_egr_imp_rec2==0 , sort connect(stairstep) lpattern(dash) lwidth(medthick) lcolor(red%50)) ///
				 (line km_a _t if mot_egr_imp_rec2==1 , sort connect(stairstep) lpattern(dash) lwidth(medthick) lcolor(blue%50)) ///
                 (line s_comp_a tt, lcolor(red) lwidth(thick)) ///
                 (line s_nocomp_a tt, lcolor(blue) lwidth(thick)) ///
                 ,xtitle("Years from treatment outcome") ///
                 ytitle("Probibability of avoiding sentence (standardized)") ///
                 legend(order(5 "Tr. completion" 6 "No completion") ring(0) pos(1) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) /// //text(.5 1 "IR = <0.001") ///
                 name(km_vs_standsurv_fin_a, replace)
graph save "`c(pwd)'\_figs\h_m_ns_rp5_a.gph", replace

<</dd_do>>
~~~~

<<dd_graph: saving("h_m_ns_rp5_a.svg") width(800) replace>>


~~~~
<<dd_do>>

twoway  (rarea rmst_comp_a_lci rmst_comp_a_uci tt, color(red%25)) ///             
                 (rarea rmst_nocomp_a_lci rmst_nocomp_a_uci tt, color(blue%25)) ///
                 (line rmst_comp_a tt, lcolor(red) lwidth(thick)) ///
                 (line rmst_nocomp_a tt, lcolor(blue) lwidth(thick)) ///
                 ,xtitle("Years from treatment outcome") ///
                 ytitle("Restricted Mean Survival Times (standardized)") ///
                 legend(order(1 "Tr. completion" 2 "No completion") ring(0) pos(5) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) /// //text(.5 1 "IR = <0.001") ///
                 name(rmst_std_fin_a, replace)	 
graph save "`c(pwd)'\_figs\h_m_ns_rp5_stdiff_rmst_a.gph", replace
<</dd_do>>
~~~~

<<dd_graph: saving("h_m_ns_rp5_stdiff_rmst_a.svg") width(800) replace>>

**Late dropout**

~~~~
<<dd_do>>
*______________________________________________
*______________________________________________
* NO STAGGERED ENTRY, BINARY TREATMENT (1-LATE VS. 0-COMPLETION & EARLY DROP)

forvalues i=1/10 {
	forvalues j=1/7 {
qui noi stipw (logit motegr_dum3 tr_mod2 edad_al_ing_1 sex_dum2 edad_ini_cons esc1 esc2 sus_prin2 sus_prin3 sus_prin4 sus_prin5 fr_cons_sus_prin2 fr_cons_sus_prin3 fr_cons_sus_prin4 fr_cons_sus_prin5 cond_ocu2 cond_ocu3 cond_ocu4 cond_ocu5 cond_ocu6 policonsumo num_hij2 tenviv1 tenviv2 tenviv4 tenviv5 mzone2 mzone3 n_off_vio n_off_acq n_off_sud n_off_oth rural2 rural3 porc_pobr susini2 susini3 susini4 susini5 dg_fis_anemia2 dg_fis_card2 dg_fis_enf_som2 dg_fis_ets2 dg_fis_hep_alc2 dg_fis_hep_b2 dg_fis_hep_cro2 dg_fis_inf2 dg_fis_otr_cond_fis_ries_vit2 dg_fis_otr_cond_fis2 dg_fis_pat_buc2 dg_fis_pat_ges_intrau2 dg_fis_trau_sec2), distribution(rp) df(`i') dftvc(`j') genw(rpdf`i'_m2_nostag_tvcdf`j') ipwtype(stabilised) vce(mestimation) eform
estimates  store m2_stipw_nostag_rp`i'_tvcdf`j'
	}
}

*https://core.ac.uk/download/pdf/6990318.pdf

*The following options are not permitted with streg models:
*bknots, bknotstvc, df, dftvc, failconvlininit, knots, knotstvc knscale, noorthorg, eform, alleq, keepcons, showcons, lininit
*forvalues j=1/7 {
local vars "exponential weibull gompertz lognormal loglogistic"
local varslab "exp wei gom logn llog"
forvalues i = 1/5 {
 local v : word `i' of `vars'
 local v2 : word `i' of `varslab'
qui noi stipw (logit motegr_dum3 tr_mod2 edad_al_ing_1 sex_dum2 edad_ini_cons esc1 esc2 sus_prin2 sus_prin3 sus_prin4 sus_prin5 fr_cons_sus_prin2 fr_cons_sus_prin3 fr_cons_sus_prin4 fr_cons_sus_prin5 cond_ocu2 cond_ocu3 cond_ocu4 cond_ocu5 cond_ocu6 policonsumo num_hij2 tenviv1 tenviv2 tenviv4 tenviv5 mzone2 mzone3 n_off_vio n_off_acq n_off_sud n_off_oth rural2 rural3 porc_pobr susini2 susini3 susini4 susini5 dg_fis_anemia2 dg_fis_card2 dg_fis_enf_som2 dg_fis_ets2 dg_fis_hep_alc2 dg_fis_hep_b2 dg_fis_hep_cro2 dg_fis_inf2 dg_fis_otr_cond_fis_ries_vit2 dg_fis_otr_cond_fis2 dg_fis_pat_buc2 dg_fis_pat_ges_intrau2 dg_fis_trau_sec2), distribution(`v') genw(`v2'_m2_nostag) ipwtype(stabilised) vce(mestimation)
estimates  store m2_stipw_nostag_`v2'
	}
*}
*
*Just a workaround: I dropped the colinear variables from the regressions manually. I know this sounds like a solution, but it was an issue because I was looping over subsamples, so I didn't know what would be colinear before running.


qui count if _d == 1
	// we count the amount of cases with the event in the strata
	//we call the estimates stored, and the results...
estimates stat m2_stipw_nostag_*, n(`r(N)')
	//we store in a matrix de survival
matrix stats_3=r(S)
mata : st_sort_matrix("stats_3", 5) // 5 AIC, 6 BIC
esttab matrix(stats_3) using "testreg_aic_bic_mrl_23_3.csv", replace
esttab matrix(stats_3) using "testreg_aic_bic_mrl_23_3.html", replace

*m2_stipw_nostag_rp5_tvcdf1

<</dd_do>>
~~~~

<<dd_include: "${pathdata2}testreg_aic_bic_mrl_23_3.html" >>

~~~~
<<dd_do>>

estimates restore m2_stipw_nostag_rp5_tvcdf1

sts gen km_b=s, by(motegr_dum3)


stpm2_standsurv, at1(motegr_dum3 0 ) at2(motegr_dum3 1 ) timevar(tt) ci contrast(difference) ///
     atvar(s_tr_comp_early_b s_late_drop_b) contrastvar(sdiff_tr_comp_early_vs_late)

* s_tr_comp_early_b s_tr_comp_early_b_lci s_tr_comp_early_b_uci s_late_drop_b s_late_drop_b_lci s_late_drop_b_uci sdiff_tr_comp_early_vs_late sdiff_tr_comp_early_vs_late_lci sdiff_tr_comp_early_vs_late_uci	 

twoway  (rarea s_tr_comp_early_b_lci s_tr_comp_early_b_uci tt, color(red%25)) ///             
                 (rarea s_late_drop_b_lci s_late_drop_b_uci tt, color(blue%25)) ///
				 (line km_b _t if motegr_dum3==0 , sort connect(stairstep) lpattern(dash) lwidth(medthick) lcolor(red%50)) ///
				 (line km_b _t if motegr_dum3==1 , sort connect(stairstep) lpattern(dash) lwidth(medthick) lcolor(blue%50)) ///
                 (line s_tr_comp_early_b tt, lcolor(red) lwidth(thick)) ///
                 (line s_late_drop_b tt, lcolor(blue) lwidth(thick)) ///
                 ,xtitle("Years from treatment outcome") ///
                 ytitle("Probibability of avoiding sentence (standardized)") ///
                 legend(order(5 "Tr. completion & Early dropout" 6 "Late dropout") ring(0) pos(1) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) /// //text(.5 1 "IR = <0.001") ///
                 name(km_vs_standsurv_fin_b, replace)
graph save "`c(pwd)'\_figs\h_m_ns_rp5_22_b.gph", replace

<</dd_do>>
~~~~

<<dd_graph: saving("h_m_ns_rp5_22_b.svg") width(800) replace>>

~~~~
<<dd_do>>
estimates restore m2_stipw_nostag_rp5_tvcdf1

stpm2_standsurv, at1(motegr_dum3 0 ) at2(motegr_dum3 1 ) timevar(tt) rmst ci contrast(difference) ///
     atvar(rmst_comp_early_b rmst_late_drop_b) contrastvar(rmstdiff_comp_early_vs_late)

twoway  (rarea rmst_comp_early_b_lci rmst_comp_early_b_uci tt, color(red%25)) ///             
                 (rarea rmst_late_drop_b_lci rmst_late_drop_b_uci tt, color(blue%25)) ///
                 (line rmst_comp_early_b tt, lcolor(red) lwidth(thick)) ///
                 (line rmst_late_drop_b tt, lcolor(blue) lwidth(thick)) ///
                 ,xtitle("Years from treatment outcome") ///
                 ytitle("Restricted Mean Survival Times (standardized)") ///
                 legend(order(1 "Tr. completion & Early dropout" 2 "Late dropout") ring(0) pos(5) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) /// //text(.5 1 "IR = <0.001") ///
                 name(rmst_std_fin_b, replace)	 
graph save "`c(pwd)'\_figs\h_m_ns_rp5_stdif_rmst_b.gph", replace
<</dd_do>>
~~~~

<<dd_graph: saving("h_m_ns_rp5_stdif_rmst_b.svg") width(800) replace>>




**Early dropout**

~~~~
<<dd_do>>
*______________________________________________
*______________________________________________
* NO STAGGERED ENTRY, BINARY TREATMENT (1-EARLY VS. 0-COMPLETION & LATE DROP)

forvalues i=1/10 {
	forvalues j=1/7 {
qui noi stipw (logit motegr_dum2 tr_mod2 edad_al_ing_1 sex_dum2 edad_ini_cons esc1 esc2 sus_prin2 sus_prin3 sus_prin4 sus_prin5 fr_cons_sus_prin2 fr_cons_sus_prin3 fr_cons_sus_prin4 fr_cons_sus_prin5 cond_ocu2 cond_ocu3 cond_ocu4 cond_ocu5 cond_ocu6 policonsumo num_hij2 tenviv1 tenviv2 tenviv4 tenviv5 mzone2 mzone3 n_off_vio n_off_acq n_off_sud n_off_oth rural2 rural3 porc_pobr susini2 susini3 susini4 susini5 dg_fis_anemia2 dg_fis_card2 dg_fis_enf_som2 dg_fis_ets2 dg_fis_hep_alc2 dg_fis_hep_b2 dg_fis_hep_cro2 dg_fis_inf2 dg_fis_otr_cond_fis_ries_vit2 dg_fis_otr_cond_fis2 dg_fis_pat_buc2 dg_fis_pat_ges_intrau2 dg_fis_trau_sec2), distribution(rp) df(`i') dftvc(`j') genw(rpdf`i'_m3_nostag_tvcdf`j') ipwtype(stabilised) vce(mestimation) eform
estimates  store m3_stipw_nostag_rp`i'_tvcdf`j'
	}
}

*https://core.ac.uk/download/pdf/6990318.pdf

*The following options are not permitted with streg models:
*bknots, bknotstvc, df, dftvc, failconvlininit, knots, knotstvc knscale, noorthorg, eform, alleq, keepcons, showcons, lininit
*forvalues j=1/7 {
local vars "exponential weibull gompertz lognormal loglogistic"
local varslab "exp wei gom logn llog"
forvalues i = 1/5 {
 local v : word `i' of `vars'
 local v2 : word `i' of `varslab'
qui noi stipw (logit motegr_dum2 tr_mod2 edad_al_ing_1 sex_dum2 edad_ini_cons esc1 esc2 sus_prin2 sus_prin3 sus_prin4 sus_prin5 fr_cons_sus_prin2 fr_cons_sus_prin3 fr_cons_sus_prin4 fr_cons_sus_prin5 cond_ocu2 cond_ocu3 cond_ocu4 cond_ocu5 cond_ocu6 policonsumo num_hij2 tenviv1 tenviv2 tenviv4 tenviv5 mzone2 mzone3 n_off_vio n_off_acq n_off_sud n_off_oth rural2 rural3 porc_pobr susini2 susini3 susini4 susini5 dg_fis_anemia2 dg_fis_card2 dg_fis_enf_som2 dg_fis_ets2 dg_fis_hep_alc2 dg_fis_hep_b2 dg_fis_hep_cro2 dg_fis_inf2 dg_fis_otr_cond_fis_ries_vit2 dg_fis_otr_cond_fis2 dg_fis_pat_buc2 dg_fis_pat_ges_intrau2 dg_fis_trau_sec2), distribution(`v') genw(`v2'_m3_nostag) ipwtype(stabilised) vce(mestimation)
estimates  store m3_stipw_nostag_`v2'
	}
*}
*
*Just a workaround: I dropped the colinear variables from the regressions manually. I know this sounds like a solution, but it was an issue because I was looping over subsamples, so I didn't know what would be colinear before running.


qui count if _d == 1
	// we count the amount of cases with the event in the strata
	//we call the estimates stored, and the results...
estimates stat m3_stipw_nostag_*, n(`r(N)')
	//we store in a matrix de survival
matrix stats_4=r(S)
mata : st_sort_matrix("stats_4", 5) // 5 AIC, 6 BIC
esttab matrix(stats_4) using "testreg_aic_bic_mrl_23_4.csv", replace
esttab matrix(stats_4) using "testreg_aic_bic_mrl_23_4.html", replace

<</dd_do>>
~~~~

<<dd_include: "${pathdata2}testreg_aic_bic_mrl_23_4.html" >>

~~~~
<<dd_do>>

estimates restore m3_stipw_nostag_rp5_tvcdf1

sts gen km_c=s, by(motegr_dum2)

stpm2_standsurv, at1(motegr_dum2 0 ) at2(motegr_dum2 1 ) timevar(tt) ci contrast(difference) ///
     atvar(s_tr_comp_late_c s_early_drop_c) contrastvar(sdiff_tr_comp_late_vs_early)

* s_tr_comp_early_b s_tr_comp_early_b_lci s_tr_comp_early_b_uci s_late_drop_b s_late_drop_b_lci s_late_drop_b_uci sdiff_tr_comp_early_vs_late sdiff_tr_comp_early_vs_late_lci sdiff_tr_comp_early_vs_late_uci	 

twoway  (rarea s_tr_comp_late_c_lci s_tr_comp_late_c_uci tt, color(red%25)) ///             
                 (rarea s_early_drop_c_lci s_early_drop_c_uci tt, color(blue%25)) ///
				 (line km_c _t if motegr_dum2==0 , sort connect(stairstep) lpattern(dash) lwidth(medthick) lcolor(red%50)) ///
				 (line km_c _t if motegr_dum2==1 , sort connect(stairstep) lpattern(dash) lwidth(medthick) lcolor(blue%50)) ///
                 (line s_tr_comp_late_c tt, lcolor(red) lwidth(thick)) ///
                 (line s_early_drop_c tt, lcolor(blue) lwidth(thick)) ///
                 ,xtitle("Years from treatment outcome") ///
                 ytitle("Probibability of avoiding sentence (standardized)") ///
                 legend(order(5 "Tr. completion & Late dropout" 6 "Early dropout") ring(0) pos(1) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) /// //text(.5 1 "IR = <0.001") ///
                 name(km_vs_standsurv_fin_b, replace)
graph save "`c(pwd)'\_figs\h_m_ns_rp5_22_c.gph", replace

<</dd_do>>
~~~~

<<dd_graph: saving("h_m_ns_rp5_22_c.svg") width(800) replace>>

~~~~
<<dd_do>>
estimates restore m3_stipw_nostag_rp5_tvcdf1

stpm2_standsurv, at1(motegr_dum2 0 ) at2(motegr_dum2 1 ) timevar(tt) rmst ci contrast(difference) ///
     atvar(rmst_comp_late_c rmst_earl_drop_c) contrastvar(rmstdiff_comp_late_vs_earl)

twoway  (rarea rmst_comp_late_c_lci rmst_comp_late_c_uci tt, color(red%25)) ///             
                 (rarea rmst_earl_drop_c_lci rmst_earl_drop_c_uci tt, color(blue%25)) ///
                 (line rmst_comp_late_c tt, lcolor(red) lwidth(thick)) ///
                 (line rmst_earl_drop_c tt, lcolor(blue) lwidth(thick)) ///
                 ,xtitle("Years from treatment outcome") ///
                 ytitle("Restricted Mean Survival Times (standardized)") ///
                 legend(order(1 "Tr. completion & Latedropout" 2 "Early dropout") ring(0) pos(5) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) /// //text(.5 1 "IR = <0.001") ///
                 name(rmst_std_fin_b, replace)	 
graph save "`c(pwd)'\_figs\h_m_ns_rp5_stdif_rmst_c.gph", replace
<</dd_do>>
~~~~

<<dd_graph: saving("h_m_ns_rp5_stdif_rmst_c.svg") width(800) replace>>

**Summary**

~~~~
<<dd_do>>

twoway  (rarea sdiff_tr_comp_early_vs_late_lci sdiff_tr_comp_early_vs_late_uci tt, color(blue%20)) ///
                 (line sdiff_tr_comp_early_vs_late tt, lcolor(blue)) ///
		(rarea sdiff_tr_comp_late_vs_early_lci sdiff_tr_comp_late_vs_early_uci tt, color(green%20)) ///
                 (line sdiff_tr_comp_late_vs_early tt, lcolor(green)) ///		 
				 (line zero tt, lcolor(black%20) lwidth(thick)) ///
		(rarea sdiff_comp_vs_nocomp_lci sdiff_comp_vs_nocomp_uci tt, color(red%20)) ///
                 (line sdiff_comp_vs_nocomp tt, lcolor(red)) ///		 
				 (line zero tt, lcolor(black%20) lwidth(thick)) ///				 
         , ylabel(, format(%3.1f)) ///
         ytitle("Difference in Survival (years)") ///
         xtitle("Years from baseline treatment outcome") ///
		 legend(order( 1 "Late dropout vs. Early & Tr. completion" 3 "Early dropout vs. Late & Tr. completion" 6 "Noncompletion vs. completion") ring(0) pos(1) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) /// //text(.5 1 "IR = <0.001") ///
                 name(s_diff_fin_abc, replace)
		gr_edit yaxis1.major.label_format = `"%9.2f"'
graph save "`c(pwd)'\_figs\h_m_ns_rp5_stdif_s_abc.gph", replace
			
<</dd_do>>
~~~~

<<dd_graph: saving("h_m_ns_rp5_stdif_s_abc.svg") width(800) replace>>

~~~~
<<dd_do>>

twoway  (rarea rmstdiff_comp_early_vs_late_lci rmstdiff_comp_early_vs_late_uci tt, color(blue%20)) ///
                 (line rmstdiff_comp_early_vs_late tt, lcolor(blue)) ///
		(rarea rmstdiff_comp_late_vs_earl_lci rmstdiff_comp_late_vs_earl_uci tt, color(green%20)) ///
                 (line rmstdiff_comp_late_vs_earl tt, lcolor(green)) ///					 
		 (rarea rmstdiff_comp_vs_nocomp_lci rmstdiff_comp_vs_nocomp_uci tt, color(red%20)) ///
                 (line rmstdiff_comp_vs_nocomp tt, lcolor(red)) ///		 
         				  (line zero tt, lcolor(black%20) lwidth(thick)) ///
         , ylabel(, format(%3.1f)) ///
         ytitle("Difference in RMST (years)") ///
         xtitle("Years from baseline treatment outcome") ///
		 legend(order( 1 "Late dropout vs. Early & Tr. completion" 3 "Early dropout vs. Late & Tr. completion" 5 "Noncompletion vs. completion") ring(0) pos(7) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) /// //text(.5 1 "IR = <0.001") ///
                 name(RMSTdiff_fin_abc, replace)
		gr_edit yaxis1.major.label_format = `"%9.2f"'
graph save "`c(pwd)'\_figs\h_m_ns_rp5_stdif_rmst_abc.gph", replace
<</dd_do>>
~~~~

<<dd_graph: saving("h_m_ns_rp5_stdif_rmst_abc.svg") width(800) replace>>


   
<<dd_display: "Saved at= `c(current_time)' `c(current_date)'">>

~~~~
<<dd_do:nocommand>>
	estwrite _all using "mariel_feb_23.sters", replace

	cap qui save "mariel_feb_23_miss.dta", all replace emptyok
<</dd_do>>
~~~~


   
<<dd_do: nocommand>>
/*
FORMA DE EXPORTAR LOS DATOS Y EL MARKDOWN

cap rm "E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/analisis_mariel_feb_2023_stata.html"
dyndoc "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\mariel_ags_b.do", saving("E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\analisis_mariel_feb_2023_stata.html") replace nostop 
copy "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\analisis_mariel_feb_2023_stata.html" "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\_outputs\analisis_mariel_feb_2023_stata.html", replace

cap rm "C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2012 (github)/analisis_mariel_feb_2023_stata.html"
dyndoc "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\mariel_ags_b.do", saving("C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\analisis_mariel_feb_2023_stata.html") replace nostop 
copy "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\analisis_mariel_feb_2023_stata.html" "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\_outputs\analisis_mariel_feb_2023_stata.html", replace

_outputs
*/
<</dd_do>>