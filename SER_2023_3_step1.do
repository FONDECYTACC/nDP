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
cap noi which med4way	
if _rc==111 {
	cap noi net install med4way, from("https://raw.githubusercontent.com/anddis/med4way/master/") replace
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
*#https://www.pauldickman.com/software/stata/mediation_meansurv.do
*#https://www.pauldickman.com/software/stata/mediation_standsurv.do

use "ser_2023_3_0.dta", clear
<</dd_do>>
~~~~


~~~~
<<dd_do>>
*https://www.pauldickman.com/software/stata/mediation_meansurv.do

stpm2 poly event_dropout, df(5) tvc(poly) dftvc(3) scale(hazard) eform

range temptime 0.5 3 30

/*survival (women)*/
predict s0m0 if poly==0, meansurv timevar(temptime)

/*survival (men)*/
predict s1m1 if poly==1, meansurv timevar(temptime)

/* Survival of males if they had the stage distribution of females */
predict s1m0 if poly==0, meansurv at(poly 1) timevar(temptime)

/* Survival of females if they had the stage distribution of males */
/* This is not used in the mediation analysis */
predict s0m1 if poly==1, meansurv at(poly 0) timevar(temptime)

// Estimate NDE, NIE, TE, and PM
generate TE = (s0m0 - s1m1)*100 if temptime != .
generate NIE = (s1m0 - s1m1)*100 if temptime != .
generate NDE = (s0m0 - s1m0)*100 if temptime != .
generate PM = (NIE / TE)*100 if temptime != .

label variable TE "Total effect"
label variable NIE "Natural indirect effect"
label variable NDE "Natural direct effect"
label variable PM "Proportion mediated"

format TE NDE NIE PM %6.2f 

gen temptime_round= round(temptime,0.01)

list temptime TE NDE NIE PM if inlist(temptime_round,0.5, 1.02,3)

twoway	(line s1m0 temptime, sort lcolor(blue) lpattern(dash)) ///
		(line s0m1 temptime, sort lcolor(red) lpattern(dash)) ///
		(line s1m1 temptime, sort lcolor(blue)) ///
		(line s0m0 temptime, sort lcolor(red)) ///
		, ///
		graphregion(color(white)) ///
		xlabel(0(5)15, labsize(*1.0)) ///s
		ylab(0.5(0.1)1, nogrid angle(0) labsize(*1.0) format(%04.2f)) ///
			legend(cols(2) size(*0.6) region(lcolor(white)) bmargin(zero) order(4 3 2 1) position(6) ring(0) ///
			label(1 "S1M0 (men if stage dist. of women)") label(2 "S0M1 (women if stage dist. of men)") label(3 "S1M1 (men)") label(4 "S0M0 (women)")) ///
			title (Survival) name(survival, replace) ///
			xtitle("Years since diagnosis") ///
			ytitle("Cause-specific survival", size(*1.0)) 
			
twoway	(line PM temptime, sort lcolor(blue) lpattern(solid)) ///
		, ///
		graphregion(color(white)) ///
		xlabel(0(5)15, labsize(*1.0)) ///
		ylab(0(10)100, nogrid angle(0) labsize(*1.0) format(%4.0f)) ///
			title (Percentage of polysubstance use difference mediated by dropout) name(pm, replace) ///
			xtitle("Years since diagnosis") legend(off) ///
			ytitle("Percentage mediated", size(*1.0)) 
<</dd_do>>
~~~~

~~~~
<<dd_do>>
*#Discacciati, A., Bellavia, A., Lee, J.J., Mazumdar, M., Valeri, L. Med4way: a Stata command to investigate mediating and interactive mechanisms using the four-way effect decomposition. International Journal of Epidemiology. 2019 Feb;48(1):15-20. doi: 10.1093/ije/dyy236
*https://github.com/anddis/med4way

med4way poly event_dropout, a0(0) a1(1) m(0) yreg(aft, e) mreg(logistic) c(1) /// // yreg permite aft, w para weibull; también puede ser cox
 cformat(%6.4f) /// //* using four decimal places 
 boot reps(100) seed(2125) //By default, normal-based CIs are calculated.
 
 
//Show the legend of the coefficients
med4way, coeflegend

*Test whether the component of the total excess mean survival-time ratio due to controlled direct effect (ereri_cde) is statistically different from the component of the total excess mean survival-time ratio due to pure indirect effect (ereri_pie)

test _b[ereri_cde] = _b[ereri_pie]

*Given the 4 basic components of the total effect, additional derived quantities can be estimated with the post-estimation commands lincom or nlcom, as appropriate. For example, to calculate the overall proportion mediated (op_m)
nlcom (_b[ereri_pie]+_b[ereri_intmed])/(_b[ereri_cde]+_b[ereri_intref]+_b[ereri_intmed]+_b[ereri_pie]), noheader
 
 
 *Mediation analysis was performed using the “med4way” command in STATA.17, 18 This method decomposes the total effect (TE, the effect of SES on overall survival) into four components: controlled direct effect (CDE, the effect neither due to the mediator nor to exposure-mediator interaction), reference interaction (INTref, the effect only due to interaction), mediated interaction (INTmed, the effect due to interaction only active when mediation is present) and the pure indirect effect (PIE, the effect due to mediation alone). The decomposition can be explained by the following equation
 
 *Sensitivity analyses for unmeasured confounding from smoking status were subsequently carried out by producing a bias factor using the following equation (8
 *https://onlinelibrary.wiley.com/doi/full/10.1002/cam4.3418
 
 *A bootstrapped analysis was used to empirically evaluate the difference in the mediation results between male and female infants for first trimester hCG only. We resampled the dataset 1,000 times for male and females separately, did mediation analysis in each dataset, then pooled the results together to calculate the difference and variance in the 5 decomposition quantities. Of the 1,000 estimates, we used the values at the 2.5th, 50th, and 97.5th percentiles to evaluate differences.

*https://uu.diva-portal.org/smash/get/diva2:1177117/FULLTEXT01.pdf
 
 
*#https://www.statalist.org/forums/forum/general-stata-discussion/general/1706954-help-with-interpretation-of-mediation-results-from-med4way-command
*https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6206666/
*https://www.stata.com/symposiums/biostatistics-and-epidemiology23/slides/Bio23_Valeri.pdf
<</dd_do>>
~~~~




<<dd_display: "Saved at= `c(current_time)' `c(current_date)'">>

~~~~
<<dd_do:nocommand>>
	cap qui save "ser_2023_3_1.dta", all replace emptyok /* Before it was b2, but i want to keep it in case an error*/
<</dd_do>>
~~~~

   
<<dd_do: nocommand>>
/*
FORMA DE EXPORTAR LOS DATOS Y EL MARKDOWN

cap rm "E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/an_ser_2023_step_1.html"
dyndoc "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\SER 2023_2_step1.do", saving("E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\an_ser_2023_step_1.html") replace nostop 
copy "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\an_ser_2023_step_1.html" "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\_outputs\an_ser_2023_step_1.html", replace

cap rm "C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/an_ser_2023_step_1.html"
dyndoc "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\SER 2023_2_step1.do", saving("C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\an_ser_2023_step_1.html") replace nostop 
copy "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\an_ser_2023_step_1.html" "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\_outputs\an_ser_2023_step_1.html", replace

_outputs

*di 

graph combine "`c(pwd)'\_figs\transmat_ser23.gph" "`c(pwd)'\_figs\pbal2_mod.gph", ///
colfirst iscale(*.6) imargin(tiny) graphregion(color(gs16))  /// // ycommon xcommon // l1(Differences in transition probabilities, size(medium)) b1(Time since admission (in years), size(medium)) ///
name(comb_ser23, replace)


// File created by Graph Editor Recorder.
// Edit only if you know what you are doing.
gr_edit .plotregion1.graph1.title.style.editstyle color(black) editcopy
gr_edit .plotregion1.graph1.title.text = {}
gr_edit .plotregion1.graph1.title.text.Arrpush A
// title edits
gr_edit .plotregion1.graph1.title.text = {}
gr_edit .plotregion1.graph1.title.text.Arrpush `"A) Multistate scheme"'
// title edits
gr_edit .plotregion1.graph2.title.xoffset = 0
gr_edit .plotregion1.graph2.title.yoffset = 0
// title edits
gr_edit .plotregion1.graph2.title.text = {}
gr_edit .plotregion1.graph2.title.text.Arrpush B
// title edits
gr_edit .plotregion1.graph2.title.text = {}
gr_edit .plotregion1.graph2.title.text.Arrpush `"B) Graphical representation of SMDs"'
// title edits
gr_edit .plotregion1.graph2.note.xoffset = 0
// note edits
gr_edit .plotregion1.graph1.plotregion1.style.editstyle inner_boxstyle(linestyle(align(outside))) editcopy
// plotregion1 edits
gr_edit .plotregion1.graph1.plotregion1.style.editstyle inner_boxstyle(linestyle(align(inside))) editcopy
// plotregion1 edits
// plotregion1 edits
gr_edit .plotregion1.graph1.plotregion1.style.editstyle margin(zero) editcopy
// plotregion1 margin
// plotregion1 margin
gr_edit .plotregion1.graph2.dragable = 1
// graph2 edits
gr_edit .plotregion1.graph2.style.editstyle margin(zero) editcopy
gr_edit .plotregion1.graph2.style.editstyle declared_xsize(6) editcopy
// graph2 edits
gr_edit .plotregion1.graph2.style.editstyle declared_xsize(7) editcopy
// graph2 size
gr_edit .plotregion1.graph1.dragable = 1
// graph1 edits
gr_edit .plotregion1.graph1.style.editstyle declared_xsize(6) editcopy
// graph1 size
gr_edit .plotregion1.graph1.style.editstyle declared_xsize(9) editcopy
// graph1 size
gr_edit .plotregion1.graph2.yaxis1.style.editstyle majorstyle(tickstyle(textstyle(size(small)))) editcopy
// yaxis1 size
gr_edit .plotregion1.graph2.title.style.editstyle margin(small) editcopy
// title margin
gr_edit .plotregion1.graph2.style.editstyle declared_xsize(9) editcopy
// graph2 size
gr_edit .plotregion1.graph2.xoffset = -20
// graph2 edits
gr_edit .plotregion1.graph2.xoffset = 20
// graph2 edits
gr_edit .plotregion1.graph2.DragBy 0 -17.59314237829193
// graph2 reposition
gr_edit .plotregion1.graph2.xoffset = -5
// graph2 edits
gr_edit .plotregion1.graph2.style.editstyle declared_xsize(11) editcopy
// graph2 size
gr_edit .plotregion1.graph1.style.editstyle declared_xsize(11) editcopy
// graph1 size
gr_edit .plotregion1.graph2.style.editstyle boxstyle(linestyle(width(none))) editcopy
// graph2 width
gr_edit .plotregion1.graph2.yaxis1.style.editstyle majorstyle(tickstyle(textstyle(size(medsmall)))) editcopy
// yaxis1 size
gr_edit .plotregion1.graph2.title.style.editstyle size(large) editcopy
// title size
gr_edit .plotregion1.graph2.note.style.editstyle size(medsmall) editcopy
// note size
gr_edit .plotregion1.graph2.note.DragBy 0 -4.861263025580612
// note reposition
gr_edit .plotregion1.graph2.note.xoffset = -20
// note edits
gr_edit .plotregion1.graph2.xaxis1.title.DragBy -.4629774310076796 0
// title reposition
gr_edit .plotregion1.graph2.xaxis1.title.style.editstyle size(medium) editcopy
// title size
gr_edit .plotregion1.graph2.legend.plotregion1.label[1].style.editstyle size(medium) editcopy
// label[1] size
gr_edit .plotregion1.graph2.legend.plotregion1.label[2].style.editstyle size(medium) editcopy
// label[2] size

graph save "grapph_comb_ser23", replace
graph export "grapph_comb_ser23.jpg", as(jpg) replace width(2000) height(1333)
graph export "grapph_comb_ser23.png", as(png) replace width(1800) height(1000)
*/
<</dd_do>>