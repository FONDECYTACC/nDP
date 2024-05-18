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

# Prop Grantt 23-24

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


The file is located and named as: <<dd_display: "`c(pwd)'an_grant_23_24_3_miss.dta" >>
 

=============================================================================
## 
=============================================================================

 
We open the files

~~~~
<<dd_do>>
use "C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/an_grant_23_24_3_miss_240411.dta", clear

*estread using "mariel_feb_23_2_m1.sters"

*b) select 10% of the data
/*
set seed 2125
sample 10
*/

<</dd_do>>
~~~~

~~~~
<<dd_do>>
/*
frame temp: gen str20 comuna = ustrregexs(1) if ustrregexm(comuna_residencia_cod,"([\d,]+)")
frame temp: destring comuna, replace
frame temp: tab comuna 


*Surv(time.lag,time,event)~ event.lag+ comp_bpsc_y2_moderate.lag+ comp_bpsc_y3_severe.lag+ less_90d_tr1.lag+ policonsumo2.lag+ policonsumo2+
*cluster(hash_key),
*/

merlin (time                             /// rehosp. times
              male                        /// male
              M1[hash_key]@1              /// random intercept
              , family(rp, df(3)          /// distribution
                         failure(event))) ///
        (stime                            /// survival time
              male                        /// male
              M1[id]                      /// random effect & association
              , family(weibull,           /// distribution
                         failure(death))) //

<</dd_do>>
~~~~

Now we bring the center ID
