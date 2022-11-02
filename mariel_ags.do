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
cap noi which pathutil
if _rc==111 {	
	ssc install project
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


The file is located and named as: <<dd_display: "`c(pwd)'fiscalia_mariel_oct_2022_match_SENDA.dta" >>
 

=============================================================================
## Structure database and survival
=============================================================================

 
We open the files

~~~~
<<dd_do>>
use "fiscalia_mariel_oct_2022_match_SENDA.dta", clear

<</dd_do>>
~~~~

Luego configuramos la base de datos en formato de supervivencia.

~~~~
<<dd_do>>
*si no está perdido cod_region, significa que hubo un registro (0/1) y el tiempo es el tiempo desde 
*set the indicator
gen event=0
replace event=1 if !missing(offender_d)
*replace event=1 if !missing(sex)

gen diff= age_offending_imp-edad_al_egres_1

stset diff, failure(event ==1) 
*stset, id(id) failure(!miss(cod_region) enter(edad_al_ing_fmt) exit 

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
## Descriptives
=============================================================================
 


~~~~
<<dd_do>>
gen     motivodeegreso_mod_imp_rec3 = 1
replace motivodeegreso_mod_imp_rec3 = 2 if strpos(motivodeegreso_mod_imp_rec,"Early")>0
replace motivodeegreso_mod_imp_rec3 = 3 if strpos(motivodeegreso_mod_imp_rec,"Late")>0

local ttl `" "Tr Completion" "Tr Disch (Early)" "Tr Disch (Late)" "' 
forvalues i = 1/3 {
cap drop sum_*
	gettoken title ttl: ttl
cap qui egen sum_`i' = total(_t) if motivodeegreso_mod_imp_rec3==`i'
cap qui tab _d motivodeegreso_mod_imp_rec3 if _d==1 & motivodeegreso_mod_imp_rec3==`i'
scalar n_eventos`i' =r(N)
qui tabstat sum_`i', save
scalar sum_`i'_total =r(StatTotal)[1,1]
scalar ir_t = round((n_eventos`i'/sum_`i'_total)*1000,.1)
*di ir_t
*es numérico, para no generar incompatibilidad, se pasa a escalar
global dens_inc`i' "Incidence rate ratio for `title' (`i') (x1,000 person-days):  `=scalar(ir_t)'"
*di %9.1f (n_eventos`i'/sum_`i'_total)*1000
}
<</dd_do>>
~~~~

- <<dd_display: "$dens_inc1">>

- <<dd_display: "$dens_inc2">>

- <<dd_display: "$dens_inc3">>


We recode the discharge cause to contrast them

~~~~
<<dd_do>>
cap noi recode motivodeegreso_mod_imp_rec3 (1=0 "Tr Completion" ) ///
			 (2=1 "Early Disch") ///
			 (else=.), gen(tto_2_1)
cap noi recode motivodeegreso_mod_imp_rec3 (1=0 "Tr Completion" ) ///
			 (3=1 "Late Disch") ///
			 (else=.), gen(tto_3_1)
cap noi recode motivodeegreso_mod_imp_rec3 (2=0 "Early Disch" ) ///
			 (3=1 "Late Disch") ///
			 (else=.), gen(tto_3_2)
<</dd_do>>
~~~~


We explored the inicidence rate ratios IRR (razón de tasa de densidad de incidencia) of each cause of discharge.

~~~~
<<dd_do>>

*set trace on
local stname `" "2_1" "3_1" "3_2" "' 
local titl `" "Early Disch vs Tr Completion" "Late Disch vs Tr Completion" "Late vs Early Disch" "' 
foreach s of local stname {
	gettoken title titl: titl
cap noi qui ir _d tto_`s' _t
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
global irr_`s' " `title': IRR `ir1' (IC 95% `ir2' - `ir3') "
global ir_`s' "`ir1' (IC 95% `ir2' - `ir3')"
}	
<</dd_do>>
~~~~

We generated a log-rank test to compare the expected to the observed result, obtaining that:

- <<dd_display: "$irr_2_1">>, so patients with an Early Discharge had a greater incidence rate than patients in Tr Completion

- <<dd_display: "$irr_3_1">>, so patients with a Late Discharge had a greater incidence rate than patients in Tr Completion

- <<dd_display: "$irr_3_2">>, so patients with a Late Discharge had a lower incidence rate than patients with an Early Discharge


~~~~
<<dd_do>>
*set trace on
local stname `" "2_1" "3_1" "3_2" "' 
local titl `" "Early Disch vs Tr Completion" "Late Disch vs Tr Completion" "Late vs Early Disch" "' 
foreach s of local stname {
	gettoken title titl: titl
cap noi qui sts test tto_`s', logrank
scalar logrank_chi`s'= round(r(chi2),.01)
scalar logrank_df`s'= r(df)
scalar logrank_p_`s'= round(chiprob(r(df),r(chi2)),.001)
local lr1= logrank_chi`s'
local lr2= logrank_df`s'
local lr3= logrank_p_`s'
global logrank_`s' " Chi^2(`lr2')=`lr1',p=`lr3'"
}
*set trace off
<</dd_do>>
~~~~

- With a value of <<dd_display: "$logrank_2_1">>, survival curves between patients that had an Early Disch y Tr Completion were significantly different.

- With a value of <<dd_display: "$logrank_3_1">>, survival curves between patients that had an Late Disch y Tr Completion were significantly different.

- With a value of <<dd_display: "$logrank_3_2">>, survival curves between patients that had an Late y Early Disch were significantly different.


=============================================================================
## Graph
=============================================================================

We generated a graph with every type of treatment and the Nelson-Aalen estimate.

~~~~
<<dd_do>>
sts graph, na by (motivodeegreso_mod_imp_rec) ci ///
title("Comission of an offense") /// 
subtitle("Nelson-Aalen Cum Hazards w/ Confidence Intervals 95%") ///
risktable(, size(*.5) order(1 "Tr Completion" 2 "Early Disch" 3 "Late Disch")) ///
ytitle("Cum. Hazards") ylabel(#8) ///
xtitle("Years Follow-up") xlabel(#8) ///
note("Source: nDP, SENDA's SUD Treatments & POs Office Data period 2010-2019 ") ///
legend(rows(3)) ///
legend(cols(4)) ///
graphregion(color(white) lwidth(large)) bgcolor(white) ///
plotregion(fcolor(white)) graphregion(fcolor(white) ) ///
text(.5 1 "IR = 0.655") ///
legend(order(1 "95CI Tr Completion" 2 "Tr Completion" 3 "95CI Early Tr Disch" 4 "Early Tr Disch " 5 "95CI Late Tr Disch" 6 "Late Tr Disch" )size(*.5)region(lstyle(none)) region(c(none)) nobox)
graph save "tto.gph", replace
<</dd_do>>
~~~~

<<dd_graph: saving(tto.svg) width(800) replace>>


<<dd_do: nocommand>>
/*
*#n: Como p-valor es menor a 0.05, hay evidencia suficiente para rechazar la 
*#hipótesis, por lo tanto, las curvas de sobrevida de hombres y mujeres son significativamente 
*#distintas.

cap rm "C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2012 (github)/analisis_mariel_nov_2022_stata.html"
dyndoc "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\mariel_ags.do", saving("E:\Mi unidad\Alvacast/SISTRAT 2022 (github)\analisis_mariel_nov_2022_stata.html") replace nostop 


*/
<</dd_do>>
