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

qui cap noi stmerlin poly  sus_prin_mod_cat1 sus_prin_mod_cat2 sus_prin_mod_cat3 sus_prin_mod_cat4 ///
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
							macrozone_cat2 macrozone_cat3, dist(rp) df(5) tvc(poly) dftvc(5)


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






stipw (logit poly sus_prin_mod fr_sus_prin edad_al_ing_1 edad_ini_cons sex_enc esc_rec ten_viv dg_cie_10_rec n_off_vio n_off_acq n_off_sud clas porc_pobr macrozone), distribution(rp) df(5)  dft() ipwtype(stabilised) vce(robust) //*edad_al_ing_1 df(10)  ten_viv
<</dd_do>>
~~~~

m2_1_rp5_tvc5 m2_2_rp4_tvc1 m2_3_gom