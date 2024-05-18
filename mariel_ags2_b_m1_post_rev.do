<<dd_version: 2>>
<<dd_include: "E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/header.txt" >>
<<dd_include: "H:/Mi unidad/Alvacast/SISTRAT 2022 (github)/header.txt" >>
<<dd_include: "C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/header.txt">>


~~~~
<<dd_do>>
//cd "E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/"
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
cap noi which merlin
if _rc==111 {		
	ssc install merlin
	}

mata: st_numscalar("installed", _stata("mata: mata which mm_quantile()",1) == 0)
if installed == 1 {
    // do nothing
}
else {
    	ssc install estwrite
}

cap noi which esttab
if _rc==111 {		
	ssc install estout
	}
	
cap noi which stpm3
if _rc==111 {		
	ssc install stpm3
	}
cap noi which standsurv
if _rc==111 {		
	ssc install standsurv
	}
cap noi which gensplines
if _rc==111 {		
	ssc install gensplines
	}
<</dd_do>>
~~~~

# Exercise (post-review)

Date created: <<dd_display: "`c(current_time)' `c(current_date)'">>.

Explore the .do files in the folder

~~~~
<<dd_do: nocommand>>
fs mariel_ags_*.do
di "`r(dofile)'"
<</dd_do>>
~~~~

Get the folder

~~~~
<<dd_do: nocommand>>
* codebook, compact

*para poner la carpeta que aloja a tu proyecto
pathutil split "`c(filename)'"
	cap qui noi cd `"`dir'"'
	global pathdata `"`dir'"' 
	di "Date: `c(current_date)', considering an OS `c(os)' for the user: `c(username)'"
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}header.txt" >>

<<dd_display: "Path data= ${pathdata};">>

<<dd_display: "Time: `c(current_date)', using an OS `c(os)'">>


The file is located and named as: <<dd_display: "`c(pwd)'fiscalia_mariel_feb_2023_match_SENDA_miss.dta" >>
 

=============================================================================
## Structure database
=============================================================================

 
We open the files

~~~~
<<dd_do>>
use "fiscalia_mariel_feb_2023_match_SENDA_miss_pris.dta", clear

estread using "mariel_feb_23_2_m1.sters"

*b) select 10% of the data
/*
set seed 2125
sample 10
*/

<</dd_do>>
~~~~

We add the municipallity/commune name for every patient

~~~~
<<dd_do>>
*nos traemos los modelos estimados en la abse de mariel

*creamos el marco temporal y nos traemos datos antiguos que tienen la comuna
cap qui noi frame create temp
frame temp: use "fiscalia_mariel_oct_2022_match_SENDA_pris.dta", clear

*recodificamos las comunas y sus códigos
frame temp: gen str20 comuna = ustrregexs(1) if ustrregexm(comuna_residencia_cod,"([\d,]+)")

frame temp: tab comuna 
*recodificar las comunas con códigos antiguos
frame temp: replace comuna= "16101" if strpos(strlower(comuna),"8401")>0
frame temp: replace comuna= "16102" if strpos(strlower(comuna),"8402")>0
frame temp: replace comuna= "16103" if strpos(strlower(comuna),"8406")>0
frame temp: replace comuna= "16104" if strpos(strlower(comuna),"8407")>0
frame temp: replace comuna= "16105" if strpos(strlower(comuna),"8410")>0
frame temp: replace comuna= "16106" if strpos(strlower(comuna),"8411")>0
frame temp: replace comuna= "16107" if strpos(strlower(comuna),"8413")>0
frame temp: replace comuna= "16108" if strpos(strlower(comuna),"8418")>0
frame temp: replace comuna= "16109" if strpos(strlower(comuna),"8421")>0
frame temp: replace comuna= "16201" if strpos(strlower(comuna),"8414")>0
frame temp: replace comuna= "16202" if strpos(strlower(comuna),"8403")>0
frame temp: replace comuna= "16203" if strpos(strlower(comuna),"8404")>0
frame temp: replace comuna= "16204" if strpos(strlower(comuna),"8408")>0
frame temp: replace comuna= "16205" if strpos(strlower(comuna),"8412")>0
frame temp: replace comuna= "16206" if strpos(strlower(comuna),"8415")>0
frame temp: replace comuna= "16207" if strpos(strlower(comuna),"8420")>0
frame temp: replace comuna= "16301" if strpos(strlower(comuna),"8416")>0
frame temp: replace comuna= "16302" if strpos(strlower(comuna),"8405")>0
frame temp: replace comuna= "16303" if strpos(strlower(comuna),"8409")>0
frame temp: replace comuna= "16304" if strpos(strlower(comuna),"8417")>0
frame temp: replace comuna= "16305" if strpos(strlower(comuna),"8419")>0

frame temp: destring comuna, replace


frame temp: tab comuna 

frame temp: codebook comuna
*nos quedamos sólo con las columnas de interés del temporal para que no introduzcamos ruido al vincular
frame temp: keep hash_key comuna
*traigo la base principal al default
cap qui noi use "mariel_feb_23_2_m1.dta", clear
*vinculo las bases que tienen comunas con la más nueva que noi
frlink m:1 hash_key, frame(temp) //* comuna
*traemos la comuna
frame default: frget comuna, from(temp)
<</dd_do>>
~~~~

Now we bring the center ID

~~~~
<<dd_do>>
*creamos el marco temporal y nos traemos datos antiguos que tienen el id centro
cap qui noi frame create temp2
frame temp2: cap use "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\_ig_borquez\fiscalia_ig_bo_feb_2023_SENDA.dta" , clear
frame temp2: cap use "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\_ig_borquez\fiscalia_ig_bo_feb_2023_SENDA.dta" , clear
frame temp2: tab id_centro
frame temp2: codebook hash_key id_centro
frame temp2: keep hash_key id_centro
frlink m:1 hash_key, frame(temp2) //* comuna
frame default: frget id_centro, from(temp2)

<</dd_do>>
~~~~

=============================================================================
## Preparation
=============================================================================

We define the previous covariates

~~~~
<<dd_do>>
*definimos las covariables fijas
global covs_3b_pre_dum "mot_egr_early mot_egr_late tr_mod2 sex_dum2 edad_ini_cons esc1 esc2 sus_prin2 sus_prin3 sus_prin4 sus_prin5 fr_cons_sus_prin2 fr_cons_sus_prin3 fr_cons_sus_prin4 fr_cons_sus_prin5 cond_ocu2 cond_ocu3 cond_ocu4 cond_ocu5 cond_ocu6 policonsumo num_hij2 tenviv1 tenviv2 tenviv4 tenviv5 mzone2 mzone3 n_off_vio n_off_acq n_off_sud n_off_oth psy_com2 dep2 rural2 rural3 porc_pobr susini2 susini3 susini4 susini5 ano_nac_corr cohab2 cohab3 cohab4 fis_com2 rc_x1 rc_x2 rc_x3"
<</dd_do>>
~~~~

Define factor variables

~~~~
<<dd_do>>
*commune/municipallity
tostring comuna, gen(comuna_str) 
encode comuna_str, gen(comuna_factor)
*center ID
tostring id_centro, gen(id_centro_str) 
encode id_centro_str, gen(id_centro_factor)
<</dd_do>>
~~~~

And add a range to predict different time-to-events in different followup times (grids)

~~~~
<<dd_do>>
*definir un ¿a variable de tiempo con 112 puntos igualmente distribuidos de 0 a 7
range tt3 0 7 112

// Generate a vector of times of interest: 1 year, 3 years and 5 years
cap gen times = .
replace times  = 1 if _n==1
replace times = 3 if _n==2
replace times = 5 if _n==3

<</dd_do>>
~~~~ 

Given a memory limit, we drop all of the STIPW models

~~~~
<<dd_do>>
cap estimates drop m_stipw*
cap estimates drop m2_stipw*
cap estimates drop m3_stipw*
<</dd_do>>
~~~~ 

Saved database

~~~~
<<dd_do>>
cap qui save "mariel_ene_24_2_m1pre.dta", all replace emptyok
<</dd_do>>
~~~~ 


=============================================================================
## Fixed terms for municipallity/commune
=============================================================================

Test different specifications for the outcome  (time-varying effects)

~~~~
<<dd_do>>
forvalues i=1/7 {
	forvalues j=1/7 {
di as txt "{bf:*¨**********************}"
di as txt "{bf:Model stored in m_24_rp`i'_tvc_`j'}"
di as txt "{bf:*¨**********************}"
set seed 2125
qui noi stpm2 $covs_3b_pre_dum i.comuna_factor, scale(hazard) df(`i') eform tvc(mot_egr_early mot_egr_late) dftvc(`j') 
estimates  store m_24_rp`i'_tvc_`j'
di as txt "{bf:*¨**********************}"
di as txt "{bf:*¨**********************}"
	}
}
<</dd_do>>
~~~~

We obtained a summary of distributions by AICs and BICs.

~~~~
<<dd_do>>
cap estimates drop m_24_rp8_*
cap estimates drop m_24_rp9_*
cap estimates drop m_24_rp10_*

*file:///G:/Mi%20unidad/Alvacast/SISTRAT%202019%20(github)/_supp_mstates/stata/1806.01615.pdf
*rcs - restricted cubic splines on log hazard scale
*rp - Royston-Parmar model (restricted cubic spline on log cumulative hazard scale)
qui count if _d == 1
	// we count the amount of cases with the event in the strata
	//we call the estimates stored, and the results...
estimates stat m_24_rp*, n(`r(N)')
	//we store in a matrix de survival
matrix stats_126=r(S)

** to order AICs
*https://www.statalist.org/forums/forum/general-stata-discussion/general/1665263-sorting-matrix-including-rownames

cap mata: mata drop st_sort_matrix()

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

// display function to order models by lowest AICs
mata: st_sort_matrix("stats_126", 5) // 5 AIC, 6 BIC
global st_rownames : rownames stats_126
*di "$st_rownames"
esttab matrix(stats_126) using "testreg_aic_bic_mrl_24_pris_m1.csv", replace
esttab matrix(stats_126) using "testreg_aic_bic_mrl_24_pris_m1.html", replace

*estwrite m_24_rp* using "mariel_ene_24_m1_fxd_stpm2.sters", replace

*weibull: Log cumulative hazard is linear in log t: ln⁡𝐻(𝑡)=𝑘 ln⁡〖𝑡−〖k ln〗⁡𝜆 〗
*Splines generalize to (almost) any baseline hazard shape.
*Stable estimates on the log cumulative hazard scale.
*ln⁡𝐻(𝑡)=𝑠(ln⁡〖𝑡)−〖k ln〗⁡𝜆 〗

*corey979 (https://stats.stackexchange.com/users/72352/corey979), How to compare models on the basis of AIC?, URL (version: 2016-08-30): https://stats.stackexchange.com/q/232494
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}testreg_aic_bic_mrl_24_pris_m1.html" >>

We selected the models that showed the best trade-off between lower complexity and better fit. This is why we also considered the BIC. If a model with fewer parameters had greater or equal AIC (or differences lower than 4) but also had better BIC (<=3), we favoured the model with fewer parameters.

~~~~
<<dd_do>>
*The per(1000) option multiplies the hazard rate by 1000 as it is easier to interpret the rate per 1000 years than per person per year.

estimates replay m_24_rp6_tvc_1, eform
estimates restore m_24_rp6_tvc_1
<</dd_do>>
~~~~

~~~~
<<dd_do:nocommand>>
*si no queremos perder tiempo probando cosas
*use "mariel_ene_24_m1pre.dta", clear
*estread using "mariel_ene_24_m1_fxd_stpm2.sters"
<</dd_do>>
~~~~ 

We plot the standardized probabilities of this model

~~~~
<<dd_do>>
//Generate a kaplan-meier curve
sts gen km_pr=s, by(motivodeegreso_mod_imp_rec)


estimates restore m_24_rp6_tvc_1

// Generate a variable with a value 0, to establish an horizontal line
cap gen zero=0
cap gen times2 = times -0.07
cap gen times3 = times +0.07
cap gen time3=3

/*
// Obtain marginal survival probabilities of interest
predict ms0_pr2, meansurv timevar(times) at(mot_egr_early 0 mot_egr_late 0) ci 

predict ms1_pr2, meansurv timevar(times) at(mot_egr_early 1 mot_egr_late 0) ci 

predict ms2_pr2, meansurv timevar(times) at(mot_egr_early 0 mot_egr_late 1) ci 

twoway  (rcap ms0_pr2_lci ms0_pr2_uci times, color(gs2%75) lcolor(gs2%75) ) ///             
                 (rcap ms1_pr2_lci ms1_pr2_uci times2, color(gs6%75) lcolor(gs6%75) ) ///
				 (rcap ms2_pr2_lci ms2_pr2_uci times3, color(gs10%75) lcolor(gs10%75) ) ///
                 (scatter ms0_pr2 times, mcolor(gs2) msymbol(O) mlcolor(gs2)) ///
                 (scatter ms1_pr2 times2, mcolor(gs6) msymbol(O) mlcolor(gs2)) ///
				 (scatter ms2_pr2 times3, mcolor(gs10) msymbol(O) mlcolor(gs2)) ///
                 ,xtitle("Years from treatment outcome") ///
                 ytitle("Probibability of avoiding sentence (standardized)") ///
                 legend(order( 4 "Tr. completion" 5 "Early dropout" 6 "Late dropout") ring(0) pos(7) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) 
                 name(km_vs_standsurv_m1_post_rev, replace)
graph save "`c(pwd)'\_figs\h_m_ns_rp6tvc1_m1_post_rev.gph", replace
*/

// Obtain marginal survival differences in probabilities: SE DEMORA MUCHO Y ME PEGA EL COMPUTADOR
predictnl diff_ms_pr = predict(meansurv at(mot_egr_early 0 mot_egr_late 0) timevar(times)) - ///
                  predict(meansurv at(mot_egr_early 1 mot_egr_late 0) timevar(times)) ///
                  , ci(diff_ms_pr_l diff_ms_pr_u)
predictnl diff_ms2_pr = predict(meansurv at(mot_egr_early 0 mot_egr_late 0) timevar(times)) - ///
                  predict(meansurv at(mot_egr_early 0 mot_egr_late 1) timevar(times)) ///
                  , ci(diff_ms2_pr_l diff_ms2_pr_u) //iter(500) force 
				  //Maximum number of iterations exceeded.
predictnl diff_ms3_pr = predict(meansurv at(mot_egr_early 0 mot_egr_late 1) timevar(times)) - ///
                  predict(meansurv at(mot_egr_early 1 mot_egr_late 0) timevar(times)) ///
                  , ci(diff_ms3_pr_l diff_ms3_pr_u) //iter(500) force  
<</dd_do>>
~~~~

~~~~
<<dd_do>>
twoway  (rcap diff_ms_pr_l diff_ms_pr_u times, color(gs2%75) lcolor(gs2%75) ) ///             
                 (rcap diff_ms2_pr_l diff_ms2_pr_u times2, color(gs6%75) lcolor(gs6%75) ) ///
				 (rcap diff_ms3_pr_l diff_ms3_pr_u times3, color(gs10%75) lcolor(gs10%75) ) ///
                 (scatter diff_ms_pr times, mcolor(gs2) msymbol(O) mlcolor(gs2)) ///
                 (scatter diff_ms2_pr times2, mcolor(gs6) msymbol(O) mlcolor(gs2)) ///
				 (scatter diff_ms3_pr times3, mcolor(gs10) msymbol(O) mlcolor(gs2)), ///
				 xtitle("Years from treatment outcome") ///
                 ytitle("Standardized differences in the probability" "of imprisonment") ///
                 legend(order( 4 "Early dropout vs. Tr. completion" 5 "Late dropout vs. Tr. completion" 6 "Late vs. early dropout") ring(0) pos(11) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white)) 
                 name(km_vs_standsurv_m1_pos_rev_diff, replace)
graph save "`c(pwd)'\_figs\h_m_ns_rp6tvc1_pris_m1_pos_rev_diff.gph", replace
<</dd_do>>
~~~~

<<dd_graph: saving("h_m_ns_rp6tvc1_pris_m1_pos_rev_diff.svg") width(800) replace>>


=============================================================================
## Fixed terms for center ID
=============================================================================
~~~~
<<dd_do>>
*eliminate previous plots 
cap estimates drop m_24_rp*
<</dd_do>>
~~~~

Test different specifications for the outcome  (time-varying effects)

~~~~
<<dd_do>>
forvalues i=1/7 {
	forvalues j=1/7 {
di as txt "{bf:*¨**********************}"
di as txt "{bf:Model stored in m_24_rp`i'_tvc_`j'}"
di as txt "{bf:*¨**********************}"
set seed 2125
qui noi stpm2 $covs_3b_pre_dum i.id_centro_factor, scale(hazard) df(`i') eform tvc(mot_egr_early mot_egr_late) dftvc(`j') 
estimates  store m_24_2rp`i'_tvc_`j'
di as txt "{bf:*¨**********************}"
di as txt "{bf:*¨**********************}"
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
estimates stat m_24_2rp*, n(`r(N)')
	//we store in a matrix de survival
matrix stats_127=r(S)

** to order AICs
*https://www.statalist.org/forums/forum/general-stata-discussion/general/1665263-sorting-matrix-including-rownames

cap mata: mata drop st_sort_matrix()

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

// display function to order models by lowest AICs
mata: st_sort_matrix("stats_127", 5) // 5 AIC, 6 BIC
global st_rownames : rownames stats_127
*di "$st_rownames"
esttab matrix(stats_127) using "testreg_aic_bic_mrl_24_pris2_m1.csv", replace
esttab matrix(stats_127) using "testreg_aic_bic_mrl_24_pris2_m1.html", replace

estwrite m_24_2rp* using "mariel_ene_24_m1_fxd2_pris2_stpm2.sters", replace

*weibull: Log cumulative hazard is linear in log t: ln⁡𝐻(𝑡)=𝑘 ln⁡〖𝑡−〖k ln〗⁡𝜆 〗
*Splines generalize to (almost) any baseline hazard shape.
*Stable estimates on the log cumulative hazard scale.
*ln⁡𝐻(𝑡)=𝑠(ln⁡〖𝑡)−〖k ln〗⁡𝜆 〗

*corey979 (https://stats.stackexchange.com/users/72352/corey979), How to compare models on the basis of AIC?, URL (version: 2016-08-30): https://stats.stackexchange.com/q/232494
<</dd_do>>
~~~~

<<dd_include: "${pathdata2}testreg_aic_bic_mrl_24_pris2_m1.html" >>

We selected the models that showed the best trade-off between lower complexity and better fit. This is why we also considered the BIC. If a model with fewer parameters had greater or equal AIC (or differences lower than 4) but also had better BIC (<=3), we favoured the model with fewer parameters.

~~~~
<<dd_do>>
*The per(1000) option multiplies the hazard rate by 1000 as it is easier to interpret the rate per 1000 years than per person per year.

estimates replay m_24_2rp7_tvc_2, eform
estimates restore m_24_2rp7_tvc_2
<</dd_do>>
~~~~

We plot the standardized probabilities of this model

~~~~
<<dd_do>>
//Restore selected model
estimates restore m_24_2rp7_tvc_2

// Obtain marginal survival differences in probabilities: SE DEMORA MUCHO Y ME PEGA EL COMPUTADOR
predictnl diff2_ms_pr = predict(meansurv at(mot_egr_early 0 mot_egr_late 0) timevar(times)) - ///
                  predict(meansurv at(mot_egr_early 1 mot_egr_late 0) timevar(times)) ///
                  , ci(diff2_ms_pr_l diff2_ms_pr_u)
predictnl diff2_ms2_pr = predict(meansurv at(mot_egr_early 0 mot_egr_late 0) timevar(times)) - ///
                  predict(meansurv at(mot_egr_early 0 mot_egr_late 1) timevar(times)) ///
                  , ci(diff2_ms2_pr_l diff2_ms2_pr_u) //iter(500) force 
				  //Maximum number of iterations exceeded.
predictnl diff2_ms3_pr = predict(meansurv at(mot_egr_early 0 mot_egr_late 1) timevar(times)) - ///
                  predict(meansurv at(mot_egr_early 1 mot_egr_late 0) timevar(times)) ///
                  , ci(diff2_ms3_pr_l diff2_ms3_pr_u) //iter(500) force 	  
<</dd_do>>
~~~~

~~~~
<<dd_do>>
twoway  (rcap diff2_ms_pr_l diff2_ms_pr_u times, color(gs2%75) lcolor(gs2%75) ) ///             
                 (rcap diff2_ms2_pr_l diff2_ms2_pr_u times2, color(gs6%75) lcolor(gs6%75) ) ///
				 (rcap diff2_ms3_pr_l diff2_ms3_pr_u times3, color(gs10%75) lcolor(gs10%75) ) ///
                 (scatter diff2_ms_pr times, mcolor(gs2) msymbol(O) mlcolor(gs2)) ///
                 (scatter diff2_ms2_pr times2, mcolor(gs6) msymbol(O) mlcolor(gs2)) ///
				 (scatter diff2_ms3_pr times3, mcolor(gs10) msymbol(O) mlcolor(gs2)) ///
                 ,xtitle("Years from treatment outcome") ///
                 ytitle("Standardized differences in the probability" "of contact with the justice system") ///
                 legend(order( 4 "Early dropout vs. Tr. completion" 5 "Late dropout vs. Tr. completion" 6 "Early vs. Late dropout") ring(0) pos(7) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) ///
                 name(km_vs_standsurv_m1_pos_rev_diff2, replace)
graph save "`c(pwd)'\_figs\h_m_ns_rp6tvc1_pris_m1_pos_rev_diff2.gph", replace
<</dd_do>>
~~~~

<<dd_graph: saving("h_m_ns_rp6tvc1_pris_m1_pos_rev_diff2.svg") width(800) replace>>


=============================================================================
## Cluster-robust SEs
=============================================================================

~~~~
<<dd_do>>
/*
use "E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/mariel_ene_24_2_m1.dta", clear
global covs_3b_pre_dum "mot_egr_early mot_egr_late tr_mod2 sex_dum2 edad_ini_cons esc1 esc2 sus_prin2 sus_prin3 sus_prin4 sus_prin5 fr_cons_sus_prin2 fr_cons_sus_prin3 fr_cons_sus_prin4 fr_cons_sus_prin5 cond_ocu2 cond_ocu3 cond_ocu4 cond_ocu5 cond_ocu6 policonsumo num_hij2 tenviv1 tenviv2 tenviv4 tenviv5 mzone2 mzone3 n_off_vio n_off_acq n_off_sud n_off_oth psy_com2 dep2 rural2 rural3 porc_pobr susini2 susini3 susini4 susini5 ano_nac_corr cohab2 cohab3 cohab4 fis_com2 rc_x1 rc_x2 rc_x3"
*/

qui noi stpm2 $covs_3b_pre_dum , scale(hazard) df(6) eform tvc(mot_egr_early mot_egr_late) dftvc(1) vce(cluster id_centro_factor)

stpm2_standsurv, at1(mot_egr_early 1 mot_egr_late 0) at2(mot_egr_early 0 mot_egr_late 0) timevar(times) ci contrast(difference) ///
     atvar(s_243_tr_comp s_243_early_drop) contrastvar(sdiff_243_tr_comp_early_drop)

stpm2_standsurv, at1(mot_egr_early 0 mot_egr_late 1) at2(mot_egr_early 0 mot_egr_late 0) timevar(times) ci contrast(difference) ///
     atvar(s_243_tr_comp0 s_243_late_drop) contrastvar(sdiff_243_tr_comp_late_drop)

stpm2_standsurv, at1(mot_egr_early 1 mot_egr_late 0) at2(mot_egr_early 0 mot_egr_late 1) timevar(times) ci contrast(difference) ///
     atvar(s_243_early_drop0 s_243_late_drop0) contrastvar(sdiff_243_early_late_drop)	

qui noi stpm2 $covs_3b_pre_dum , scale(hazard) df(6) eform tvc(mot_egr_early mot_egr_late) dftvc(1) vce(cluster comuna_factor)

stpm2_standsurv, at1(mot_egr_early 1 mot_egr_late 0) at2(mot_egr_early 0 mot_egr_late 0) timevar(times) ci contrast(difference) ///
     atvar(s_244_tr_comp s_244_early_drop) contrastvar(sdiff_244_tr_comp_early_drop)

stpm2_standsurv, at1(mot_egr_early 0 mot_egr_late 1) at2(mot_egr_early 0 mot_egr_late 0) timevar(times) ci contrast(difference) ///
     atvar(s_244_tr_comp0 s_244_late_drop) contrastvar(sdiff_244_tr_comp_late_drop)

stpm2_standsurv, at1(mot_egr_early 1 mot_egr_late 0) at2(mot_egr_early 0 mot_egr_late 1) timevar(times) ci contrast(difference) ///
     atvar(s_244_early_drop0 s_244_late_drop0) contrastvar(sdiff_244_early_late_drop)	

/*
//get estimates  compute statistic marginally with respect to the latent variables
cap noi predict pr_mln_mixstd_rly_lte, marginal stand sdifference ci at1(mot_egr_early 1 mot_egr_late 0) at2(mot_egr_early 0 mot_egr_late 1) timevar(times) reps(200) 
cap noi predict pr_mln_mixstd_rly_cmp, marginal stand sdifference ci at1(mot_egr_early 1 mot_egr_late 0) at2(mot_egr_early 0 mot_egr_late 0) timevar(times) reps(200) 
cap noi predict pr_mln_mixstd_lte_cmp, marginal stand sdifference ci at1(mot_egr_early 0 mot_egr_late 1) at2(mot_egr_early 0 mot_egr_late 0) timevar(times) reps(200) 
//standardise not supported with multilevel models
*/
<</dd_do>>
~~~~
   
~~~~
<<dd_do>>
*center ID
twoway  (rcap sdiff_243_tr_comp_early_drop_lci sdiff_243_tr_comp_early_drop_uci times, color(gs2%75) lcolor(gs2%75) ) ///             
                 (rcap sdiff_243_tr_comp_late_drop_lci sdiff_243_tr_comp_late_drop_uci times2, color(gs6%75) lcolor(gs6%75) ) ///
				 (rcap sdiff_243_early_late_drop_lci sdiff_243_early_late_drop_uci  times3, color(gs10%75) lcolor(gs10%75) ) ///
                 (scatter sdiff_243_tr_comp_early_drop times, mcolor(gs2) msymbol(O) mlcolor(gs2)) ///
                 (scatter sdiff_243_tr_comp_late_drop times2, mcolor(gs6) msymbol(O) mlcolor(gs2)) ///
				 (scatter sdiff_243_early_late_drop times3, mcolor(gs10) msymbol(O) mlcolor(gs2)) ///
                 ,xtitle("Years from treatment outcome") ///
                 ytitle("Standardized differences in the probability" "of contact with the justice system") ///
                 legend(order( 4 "Early dropout vs. Tr. completion" 5 "Late dropout vs. Tr. completion" 6 "Early vs. Late dropout") ring(0) pos(11) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) ///
                 name(clus_std2_m1_pos_rev_pris_diff2, replace)
graph save "`c(pwd)'\_figs\h_m_ns_rp6tvc1_2pris_m1_pos_rev_diff2_clus.gph", replace
<</dd_do>>
~~~~

<<dd_graph: saving("h_m_ns_rp6tvc1_2pris_m1_pos_rev_diff2_clus.svg") width(800) replace>>

~~~~
<<dd_do>>
*municipallity
twoway  (rcap sdiff_244_tr_comp_early_drop_lci sdiff_244_tr_comp_early_drop_uci times, color(gs2%75) lcolor(gs2%75) ) ///             
                 (rcap sdiff_244_tr_comp_late_drop_lci sdiff_244_tr_comp_late_drop_uci times2, color(gs6%75) lcolor(gs6%75) ) ///
				 (rcap sdiff_244_early_late_drop_lci sdiff_244_early_late_drop_uci  times3, color(gs10%75) lcolor(gs10%75) ) ///
                 (scatter sdiff_244_tr_comp_early_drop times, mcolor(gs2) msymbol(O) mlcolor(gs2)) ///
                 (scatter sdiff_244_tr_comp_late_drop times2, mcolor(gs6) msymbol(O) mlcolor(gs2)) ///
				 (scatter sdiff_244_early_late_drop times3, mcolor(gs10) msymbol(O) mlcolor(gs2)) ///
                 ,xtitle("Years from treatment outcome") ///
                 ytitle("Standardized differences in the probability" "of contact with the justice system") ///
                 legend(order( 4 "Early dropout vs. Tr. completion" 5 "Late dropout vs. Tr. completion" 6 "Early vs. Late dropout") ring(0) pos(11) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) ///
                 name(clus_std2_m1_pos_rev_pris_diff2, replace)
graph save "`c(pwd)'\_figs\h_m_ns_rp6tvc1_2pris_m1_pos_rev_diff2_clus2.gph", replace
<</dd_do>>
~~~~

<<dd_graph: saving("h_m_ns_rp6tvc1_2pris_m1_pos_rev_diff2_clus2.svg") width(800) replace>>

<<dd_do: nocommand>>
/*
FORMA DE EXPORTAR LOS DATOS Y EL MARKDOWN

cap rm "E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/analisis_mariel_ene_2024_stata_pris_m1.html"
dyndoc "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\mariel_ags2_b_m1_post_rev.do", saving("E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\analisis_mariel_ene_2024_stata_pris_m1.html") replace nostop 
copy "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\analisis_mariel_ene_2024_stata_pris_m1.html" "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\_outputs\analisis_mariel_ene_2024_stata_pris_m1.html", replace

cap rm "C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/analisis_mariel_ene_2024_stata_pris_m1.html"
dyndoc "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\mariel_ags2_b_m1_post_rev.do", saving("C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\analisis_mariel_ene_2024_stata_pris_m1.html") replace nostop 
copy "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\analisis_mariel_ene_2024_stata_pris_m1.html" "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\_outputs\analisis_mariel_ene_2024_stata_pris_m1.html", replace


cap rm "H:/Mi unidad/Alvacast/SISTRAT 2022 (github)/analisis_mariel_ene_2024_pris_stata_m1.html"
dyndoc "H:\Mi unidad\Alvacast\SISTRAT 2022 (github)\mariel_ags2_b_m1_post_rev.do", saving("H:\Mi unidad\Alvacast\SISTRAT 2022 (github)\analisis_mariel_ene_2024_stata_pris_m1.html") replace nostop 
copy "H:\Mi unidad\Alvacast\SISTRAT 2022 (github)\analisis_mariel_ene_2024_stata_pris_m1.html" "H:\Mi unidad\Alvacast\SISTRAT 2022 (github)\_outputs\analisis_mariel_ene_2024_stata_pris_m1.html", replace

_outputs
*/
<</dd_do>>

<<dd_display: "Saved at= `c(current_time)' `c(current_date)'">>


~~~~
<<dd_do>>
	estwrite _all using "mariel_ene_24_pris_m1_fxd_vce.sters", replace

	cap qui save "mariel_ene_24_2_m1.dta", all replace emptyok
<</dd_do>>
~~~~





