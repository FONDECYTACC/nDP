cd "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)"

clear all

//mariel_ags_b_m1
graph use "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\_figs\h_m_ns_rp6_stdif_s2_m1.gph"
//graph use "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\_figs\h_m_ns_rp6_stdif_rmst_m1.gph"
serset dir
serset use

gen sdiff_early_late_drop_lci_c= -sdiff_early_late_drop_lci
gen sdiff_early_late_drop_uci_c= -sdiff_early_late_drop_uci
gen sdiff_early_late_drop_c= -sdiff_early_late_drop

twoway  (rarea sdiff_tr_comp_early_drop_lci sdiff_tr_comp_early_drop_uci tt, color(gs2%35)) ///
                 (line sdiff_tr_comp_early_drop tt, lcolor(gs2)) ///
		(rarea sdiff_tr_comp_late_drop_lci sdiff_tr_comp_late_drop_uci tt, color(gs6%35)) ///
                 (line sdiff_tr_comp_late_drop tt, lcolor(gs6)) ///
		(rarea sdiff_early_late_drop_lci_c sdiff_early_late_drop_uci_c tt, color(gs10%35)) ///
                 (line sdiff_early_late_drop_c tt, lcolor(gs10)) ///				 
         				  (line zero tt, lcolor(black%20) lwidth(thick)) ///
         , ylabel(, format(%3.1f)) ///
         ytitle("Difference in survival probabilities") ///
         xtitle("Years from baseline substance use treatment completion status") ///
		 legend(order( 1 "Early dropout vs. Tr. completion" 3 "Late dropout vs. Tr. completion" 5 "Early vs. Late dropout") ring(0) pos(7) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) /// //text(.5 1 "IR = <0.001") ///
                 name(s_diff_corr, replace)
		gr_edit yaxis1.major.label_format = `"%9.2f"'
		gr_edit .legend.plotregion1.label[1].style.editstyle size(small) editcopy
		gr_edit .legend.plotregion1.label[2].style.editstyle size(small) editcopy
		gr_edit .legend.plotregion1.label[3].style.editstyle size(small) editcopy

graph save "`c(pwd)'\_figs\h_m_ns_rp6_stdif_s2_m1_corr.gph", replace
graph export "_figs/h_m_ns_rp6_stdif_s2_m1_corr.pdf", as(pdf) name("s_diff_corr") replace
graph set eps fontface Arial
graph export "_figs/h_m_ns_rp6_stdif_s2_m1_corr.eps", as(eps) name("s_diff_corr") replace

clear all

//mariel_ags2_b_m1
graph use "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\_figs\h_m_ns_rp6_stdif_s2_pris_m1.gph"
//graph use "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\_figs\h_m_ns_rp6_stdif_s2_pris_m1.gph"
serset dir
serset use

gen sdiff_early_late_drop_lci_c= -sdiff_early_late_drop_lci
gen sdiff_early_late_drop_uci_c= -sdiff_early_late_drop_uci
gen sdiff_early_late_drop_c= -sdiff_early_late_drop

twoway  (rarea sdiff_tr_comp_early_drop_lci sdiff_tr_comp_early_drop_uci tt, color(gs2%35)) ///
                 (line sdiff_tr_comp_early_drop tt, lcolor(gs2)) ///
		(rarea sdiff_tr_comp_late_drop_lci sdiff_tr_comp_late_drop_uci tt, color(gs6%35)) ///
                 (line sdiff_tr_comp_late_drop tt, lcolor(gs6)) ///
		(rarea sdiff_early_late_drop_lci_c sdiff_early_late_drop_uci_c tt, color(gs10%35)) ///
                 (line sdiff_early_late_drop_c tt, lcolor(gs10)) ///				 
         				  (line zero tt, lcolor(black%20) lwidth(thick)) ///
         , ylabel(, format(%3.1f)) ///
         ytitle("Difference in survival probabilities") ///
         xtitle("Years from baseline substance use treatment completion status") ///
		 legend(order( 1 "Early dropout vs. Tr. completion" 3 "Late dropout vs. Tr. completion" 5 "Early dropout vs. Late dropout") ring(0) pos(7) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) /// //text(.5 1 "IR = <0.001") ///
                 name(s_diff_pris_corr, replace)
		gr_edit yaxis1.major.label_format = `"%9.2f"'
		gr_edit .legend.plotregion1.label[1].style.editstyle size(small) editcopy
		gr_edit .legend.plotregion1.label[2].style.editstyle size(small) editcopy
		gr_edit .legend.plotregion1.label[3].style.editstyle size(small) editcopy

graph save "`c(pwd)'\_figs\h_m_ns_rp6_stdif_s2_pris_m1_corr.gph", replace
graph export "_figs/h_m_ns_rp6_stdif_s2_pris_m1_corr.pdf", as(pdf) name("s_diff_pris_corr") replace
graph set eps fontface Arial
graph export "_figs/h_m_ns_rp6_stdif_s2_pris_m1_corr.eps", as(eps) name("s_diff_pris_corr") replace




/*
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
# Cluster, center ID, time to contact to the justice system
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
*/

clear all

//ID centro
cap graph use "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\_figs\h_m_ns_rp6tvc1_m1_pos_rev_diff2_clus.gph"
cap graph use "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\_figs\h_m_ns_rp6tvc1_m1_pos_rev_diff2_clus.gph"
//graph use "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\_figs\h_m_ns_rp6_stdif_rmst_m1.gph"
serset dir
serset use

gen zero=0
replace sdiff_24_tr_comp_early_drop_uci = -sdiff_24_tr_comp_early_drop_uci 
replace sdiff_24_tr_comp_early_drop_lci = -sdiff_24_tr_comp_early_drop_lci 
replace sdiff_24_tr_comp_early_drop = -sdiff_24_tr_comp_early_drop

replace sdiff_24_tr_comp_late_drop_uci = -sdiff_24_tr_comp_late_drop_uci
replace sdiff_24_tr_comp_late_drop_lci = -sdiff_24_tr_comp_late_drop_lci
replace sdiff_24_tr_comp_late_drop = -sdiff_24_tr_comp_late_drop

replace sdiff_24_early_late_drop_lci = -sdiff_24_early_late_drop_lci 
replace sdiff_24_early_late_drop_uci = -sdiff_24_early_late_drop_uci 
replace sdiff_24_early_late_drop = -sdiff_24_early_late_drop

frame create original 
frame original: use mariel_feb_23_m1.dta, clear 
*estread using "mariel_feb_23_2_m1.sters"

frame original: generate times = .
frame original: replace times  = 1 if _n==1
frame original: replace times = 3 if _n==2
frame original: replace times = 5 if _n==3

frame original: global covs_3b_pre_dum "mot_egr_early mot_egr_late tr_mod2 sex_dum2 edad_ini_cons esc1 esc2 sus_prin2 sus_prin3 sus_prin4 sus_prin5 fr_cons_sus_prin2 fr_cons_sus_prin3 fr_cons_sus_prin4 fr_cons_sus_prin5 cond_ocu2 cond_ocu3 cond_ocu4 cond_ocu5 cond_ocu6 policonsumo num_hij2 tenviv1 tenviv2 tenviv4 tenviv5 mzone2 mzone3 n_off_vio n_off_acq n_off_sud n_off_oth psy_com2 dep2 rural2 rural3 porc_pobr susini2 susini3 susini4 susini5 ano_nac_corr cohab2 cohab3 cohab4 fis_com2 rc_x1 rc_x2 rc_x3"

frame original: qui noi stpm2 $covs_3b_pre_dum , scale(hazard) df(8) eform tvc(mot_egr_early mot_egr_late) dftvc(1) 

frame original: stpm2_standsurv, at1(mot_egr_early 0 mot_egr_late 0) at2(mot_egr_early 1 mot_egr_late 0) timevar(times) ci contrast(difference) ///
     atvar(s_v_tr_comp s_v_early_drop) contrastvar(sdiff_v_tr_comp_early_drop)

frame original: stpm2_standsurv, at1(mot_egr_early 0 mot_egr_late 0) at2(mot_egr_early 0 mot_egr_late 1) timevar(times) ci contrast(difference) ///
     atvar(s_v_tr_comp0 s_v_late_drop) contrastvar(sdiff_v_tr_comp_late_drop)

frame original: stpm2_standsurv, at1(mot_egr_early 1 mot_egr_late 0) at2(mot_egr_early 0 mot_egr_late 1) timevar(times) ci contrast(difference) ///
     atvar(s_v_early_drop0 s_v_late_drop0) contrastvar(sdiff_v_early_late_drop)	

frame original: drop if missing(times)

*vinculo las bases que tienen tiempos con la más nueva que noi
frlink m:1 times, frame(original) //* 
*traemos las diferencias de supervivencia originales
frget sdiff_v_tr_comp_early_drop sdiff_v_tr_comp_early_drop_lci sdiff_v_tr_comp_early_drop_uci sdiff_v_tr_comp_late_drop sdiff_v_tr_comp_late_drop_lci sdiff_v_tr_comp_late_drop_uci sdiff_v_early_late_drop sdiff_v_early_late_drop_lci sdiff_v_early_late_drop_uci, from(original)

gen neg_sdiff_v_early_late_drop = - sdiff_v_early_late_drop 
gen neg_sdiff_v_early_late_drop_lci = - sdiff_v_early_late_drop_lci 
gen neg_sdiff_v_early_late_drop_uci = - sdiff_v_early_late_drop_uci
 
gen times4= times+.1
gen times5= times2-.1
gen times6= times3+.1 
 
*https://www.stata.com/manuals/ppystataintegration.pdf
*https://www.statalist.org/forums/forum/general-stata-discussion/general/1656976-trouble-with-python-in-stata-17
*https://stackoverflow.com/questions/64071200/stata-16-0-integration-with-python
/*
https://blog.stata.com/2020/09/01/stata-python-integration-part-3-how-to-install-python-packages/
shell

*/

cap set python_exec  "C:\Users\CISS Fondecyt\AppData\Local\Programs\Python\Python38\python.exe", perm
cap set python_userpath	 "C:\Users\CISS Fondecyt\AppData\Local\Programs\Python\Python38\"
*cap set python_exec  "C:\Users\CISS Fondecyt\anaconda32\python.exe", perm

*cap set python_userpath	 "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\python"
*cap set python_userpath	 "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\python"
python query
!"C:\Users\CISS Fondecyt\AppData\Local\Programs\Python\Python38\python.exe"  pip -V
!"C:\Users\CISS Fondecyt\AppData\Local\Programs\Python\Python38\python.exe" -m pip install --upgrade pip

!"C:\Users\CISS Fondecyt\AppData\Local\Programs\Python\Python38\python.exe" -m pip install numpy --force-reinstall --upgrade --target="C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/python"
!"C:\Users\CISS Fondecyt\AppData\Local\Programs\Python\Python38\python.exe" -m pip install pandas --upgrade --target="C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/python"

*!"C:\Users\CISS Fondecyt\AppData\Local\Programs\Python\Python38\python.exe"  -m venv stata_python_env
*!"C:\Users\CISS Fondecyt\AppData\Local\Programs\Python\Python38\python.exe"  -m stata_python_env\Scripts\activate
*#python: os.system("python3 -m venv stata_python_env")
*#python: os.system("stata_python_env\Scripts\activate")
*#python: os.system("(venv)$ pip install --upgrade pip")
*#filedir= "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\python"
python:
import os
original_value = os.environ.get('ENV_VAR_NAME')

filedir= "C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/python"
end
python:
import os
filedir= "C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/python"
try:  
	import numpy
except ImportError:
	os.system('"C:/Users/CISS Fondecyt/AppData/Local/Programs/Python/Python38/python.exe" -m pip install pandas --upgrade ')
	import numpy
try:
	import pandas as pd
except ImportError:
	os.system('"C:/Users/CISS Fondecyt/AppData/Local/Programs/Python/Python38/python.exe" -m pip install matplotlib --upgrade ')
	import pandas as pd
try:
	import matplotlib.pyplot as plt
	import matplotlib.image as mpimg
except ImportError:
	os.system('"C:/Users/CISS Fondecyt/AppData/Local/Programs/Python/Python38/python.exe" -m pip install matplotlib  --upgrade ')
	import matplotlib.pyplot as plt	
	import matplotlib.image as mpimg
try:
	import statsmodels as sm
except ImportError:
	os.system('"C:/Users/CISS Fondecyt/AppData/Local/Programs/Python/Python38/python.exe" -m pip install statsmodels  --upgrade ')
	import statsmodels as sm	
end

python:
import sfi	
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.image as mpimg
from sfi import Data,Scalar,SFIToolkit
data= sfi.Data.get()
df = pd.DataFrame(data)
df
print(df.head())	
# eliminate fourth row
df = df.drop(3)
print(df.head())	

end

unab varlist: _all
file open myfile using "varlist.txt", write replace
file write myfile `"`varlist'"'
file close myfile
python
with open("varlist.txt", "r") as file:
    varlist = file.read().split()
end

python
if len(varlist) == len(df.columns):
    df.columns = varlist
else:
    print("The length of varlist does not match the number of columns in df")
end

python: print(df.head())

python:
plt.figure(figsize=(10, 6))
import matplotlib.pyplot as plt 
yerr_lower = abs(df['sdiff_24_tr_comp_early_drop'] - df['sdiff_24_tr_comp_early_drop_lci'])
yerr_lower2 = abs(df['sdiff_24_tr_comp_late_drop'] - df['sdiff_24_tr_comp_late_drop_lci'])
yerr_lower3 = abs(df['sdiff_24_early_late_drop'] - df['sdiff_24_early_late_drop_lci'])

yerr_lower4 = abs(df['neg_sdiff_v_early_late_drop'] - df['neg_sdiff_v_early_late_drop_lci'])
yerr_lower5 = abs(df['sdiff_v_tr_comp_early_drop'] - df['sdiff_v_tr_comp_early_drop_lci'])
yerr_lower6 = abs(df['sdiff_v_tr_comp_late_drop'] - df['sdiff_v_tr_comp_late_drop_lci'])

plt.errorbar(df['times'], df['sdiff_24_tr_comp_early_drop'], 
             yerr=yerr_lower, 
             fmt='D', color='gray', ecolor='gray', alpha=0.7, elinewidth=3, capsize=0, label='Early dropout vs. Tr. completion')
plt.errorbar(df['times2'], df['sdiff_24_tr_comp_late_drop'], 
             yerr=yerr_lower2, 
             fmt='D', color='gray', ecolor='gray', alpha=0.3, elinewidth=3, capsize=0, label='Late dropout vs. Tr. completion')
plt.errorbar(df['times3'], df['sdiff_24_early_late_drop'], 
             yerr=yerr_lower3, 
             fmt='D', color='black', ecolor='black', alpha=0.7, elinewidth=3, capsize=0, label='Early vs.Late dropout')			 
			 
plt.errorbar(df['times4'], df['sdiff_v_tr_comp_early_drop'], 
             yerr=yerr_lower, 
             fmt='o', color='gray', ecolor='gray', alpha=0.7, elinewidth=3, capsize=0, label='Original: Early dropout vs. Tr. completion')
plt.errorbar(df['times5'], df['sdiff_v_tr_comp_late_drop'], 
             yerr=yerr_lower5, 
             fmt='o', color='gray', ecolor='gray', alpha=0.3, elinewidth=3, capsize=0, label='Original: Late dropout vs. Tr. completion')
plt.errorbar(df['times6'], df['neg_sdiff_v_early_late_drop'], 
             yerr=yerr_lower6, 
             fmt='o', color='black', ecolor='black', alpha=0.7, elinewidth=3, capsize=0, label='Original: Early vs.Late dropout')					 
			 
plt.xlabel("Years from baseline substance use treatment completion status")
plt.ylabel("Difference in survival probabilities")
plt.legend(frameon=False, framealpha=0)
plt.grid(False)
plt.savefig("_figs/h_m_ns_rp6tvc1_m1_pos_rev_diff2_clus22.png", dpi=500, bbox_inches="tight", pad_inches=0.1)
plt.savefig("_figs/h_m_ns_rp6tvc1_m1_pos_rev_diff2_clus22.pdf", format="pdf", bbox_inches="tight", pad_inches=0.1)
plt.savefig()
plt.show() 
end

/*
lpattern(dash) changes the line pattern to dashed.
Stata offers various line patterns like solid, dash, dot, dash_dot, shortdash, longdash, 
*/
twoway  (rcap sdiff_24_tr_comp_early_drop_lci sdiff_24_tr_comp_early_drop_uci times, color(gs2%75) lcolor(gs2%75) legend(label(1 "Early dropout vs. Tr. completion"))) ///             
		(rcap sdiff_v_tr_comp_early_drop_lci sdiff_v_tr_comp_early_drop_uci times4, color(gs2%75) lcolor(gs2%75) lpattern(dash) legend(label(3 "Late dropout vs. Tr. completion"))) ///
        (rcap sdiff_24_tr_comp_late_drop_lci sdiff_24_tr_comp_late_drop_uci times2, color(gs6%75) lcolor(gs6%75) legend(label(5 "Early vs. Late dropout"))) ///
		(rcap sdiff_v_tr_comp_late_drop_lci sdiff_v_tr_comp_late_drop_uci times5, color(gs6%75) lcolor(gs6%75) lpattern(dash) legend(label(2 "Original: Early dropout vs. Tr. completion"))) ///
		(rcap sdiff_24_early_late_drop_lci sdiff_24_early_late_drop_uci times3, color(gs10%75) lcolor(gs10%75) legend(label(4 "Original: Late dropout vs. Tr. completion"))) ///
		(rcap neg_sdiff_v_early_late_drop_lci neg_sdiff_v_early_late_drop_uci times6, color(gs10%75) lcolor(gs10%75) lpattern(dash) legend(label(6 "Original: Early vs. Late dropout"))) ///
                 (connect  sdiff_24_tr_comp_early_drop times, mcolor(gs2) msymbol(O) mlcolor(gs2) legend(label(1 "Early dropout vs. Tr. completion"))) ///             
                 (connect  sdiff_v_tr_comp_early_drop times4, mcolor(gs2) msymbol(O) mlcolor(gs2) msymbol(diamond) legend(label(3 "Late dropout vs. Tr. completion"))) ///
                 (connect  sdiff_24_tr_comp_late_drop times2, mcolor(gs6) msymbol(O) mlcolor(gs2) legend(label(5 "Early vs. Late dropout"))) ///
                 (connect  sdiff_v_tr_comp_late_drop times5, mcolor(gs6) msymbol(O) mlcolor(gs2) msymbol(diamond) legend(label(2 "Original: Early dropout vs. Tr. completion"))) ///
				 (connect  sdiff_24_early_late_drop times3, mcolor(gs10) msymbol(O) mlcolor(gs2)  legend(label(4 "Original: Late dropout vs. Tr. completion"))) ///
				 (connect  neg_sdiff_v_early_late_drop times6, mcolor(gs10) msymbol(O) mlcolor(gs2) msymbol(diamond) legend(label(6 "Original: Early vs. Late dropout"))) ///
         				  (line zero times, lcolor(black%20) lwidth(thick) legend(label(6 ""))) ///
                 ,xtitle("Years from baseline substance use treatment completion status") ///
				 legend(order( 1 "Early dropout vs. Tr. completion" 2 "Original: Early dropout vs. Tr. completion" 3 "Late dropout vs. Tr. completion" 4 "Original: Late dropout vs. Tr. completion" 5 "Early vs. Late dropout" 6 "Original: Early vs. Late dropout" ) ring(0) pos(7) cols(2) region(lstyle(none)) region(c(none)) nobox  size(2.5)) ///
                 ytitle("Difference in survival probabilities") ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) ///
                 name(s_diff_cjs_clus_corr_idcen, replace)
		 			 
forvalues i=7/12 {
  gr_edit .plotregion1.plot`i'.style.editstyle line(width(none)) editcopy
}
forvalues i=1/6 {
  gr_edit legend.plotregion1.key[`i'].xsz.editstyle 9 editcopy
}
				 
graph save "`c(pwd)'\_figs\h_m_ns_rp6tvc1_m1_pos_rev_diff2_clus_corr.gph", replace
graph export "_figs/h_m_ns_rp6tvc1_m1_pos_rev_diff2_clus_corr.pdf", as(pdf) name("s_diff_cjs_clus_corr_idcen") replace
graph set eps fontface Arial
graph export "_figs/h_m_ns_rp6tvc1_m1_pos_rev_diff2_clus_corr.eps", as(eps) name("s_diff_cjs_clus_corr_idcen") replace

/*
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
# Cluster, municipallity, time to contact to the justice system
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
*/

clear all

//ID centro
graph use "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\_figs\h_m_ns_rp6tvc1_m1_pos_rev_diff2_clus2.gph"
//graph use "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\_figs\h_m_ns_rp6_stdif_rmst_m1.gph"
serset dir
serset use

gen zero=0
replace sdiff_242_tr_comp_early_drop_uci = -sdiff_242_tr_comp_early_drop_uci 
replace sdiff_242_tr_comp_early_drop_lci = -sdiff_242_tr_comp_early_drop_lci 
replace sdiff_242_tr_comp_early_drop = -sdiff_242_tr_comp_early_drop

replace sdiff_242_tr_comp_late_drop_uci = -sdiff_242_tr_comp_late_drop_uci
replace sdiff_242_tr_comp_late_drop_lci = -sdiff_242_tr_comp_late_drop_lci
replace sdiff_242_tr_comp_late_drop = -sdiff_242_tr_comp_late_drop

replace sdiff_242_early_late_drop_lci = -sdiff_242_early_late_drop_lci 
replace sdiff_242_early_late_drop_uci = -sdiff_242_early_late_drop_uci 
replace sdiff_242_early_late_drop = -sdiff_242_early_late_drop

 
twoway  (rcap sdiff_242_tr_comp_early_drop_lci sdiff_242_tr_comp_early_drop_uci times, color(gs2%75) lcolor(gs2%75) ) ///             
                 (rcap sdiff_242_tr_comp_late_drop_lci sdiff_242_tr_comp_late_drop_uci times2, color(gs6%75) lcolor(gs6%75) ) ///
				 (rcap sdiff_242_early_late_drop_lci sdiff_242_early_late_drop_uci  times3, color(gs10%75) lcolor(gs10%75) ) ///
                 (scatter sdiff_242_tr_comp_early_drop times, mcolor(gs2) msymbol(O) mlcolor(gs2)) ///
                 (scatter sdiff_242_tr_comp_late_drop times2, mcolor(gs6) msymbol(O) mlcolor(gs2)) ///
				 (scatter sdiff_242_early_late_drop times3, mcolor(gs10) msymbol(O) mlcolor(gs2)) ///				 
         				  (line zero times, lcolor(black%20) lwidth(thick)) ///
                 ,xtitle("Years from baseline substance use treatment completion status") ///
                 ytitle("Difference in survival probabilities") ///
                 legend(order( 4 "Early dropout vs. Tr. completion" 5 "Late dropout vs. Tr. completion" 6 "Early vs. Late dropout") ring(0) pos(7) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) ///
                 name(s_diff_cjs_clus_corr_muni, replace)

graph save "`c(pwd)'\_figs\h_m_ns_rp6tvc1_m1_pos_rev_diff2_clus2_corr.gph", replace
graph export "_figs/h_m_ns_rp6tvc1_m1_pos_rev_diff2_clus2_corr.pdf", as(pdf) name("s_diff_cjs_clus_corr_muni") replace
graph set eps fontface Arial
graph export "_figs/h_m_ns_rp6tvc1_m1_pos_rev_diff2_clus2_corr.eps", as(eps) name("s_diff_cjs_clus_corr_muni") replace

/*
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
# Cluster, center ID, time to imprisonment
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
*/

clear all

//ID centro

cap graph use "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\_figs\h_m_ns_rp6tvc1_2pris_m1_pos_rev_diff2_clus.gph"
cap graph use "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\_figs\h_m_ns_rp6tvc1_2pris_m1_pos_rev_diff2_clus.gph"
serset dir
serset use

cap gen zero=0
replace sdiff_243_tr_comp_early_drop_uci = -sdiff_243_tr_comp_early_drop_uci 
replace sdiff_243_tr_comp_early_drop_lci = -sdiff_243_tr_comp_early_drop_lci 
replace sdiff_243_tr_comp_early_drop = -sdiff_243_tr_comp_early_drop

replace sdiff_243_tr_comp_late_drop_uci = -sdiff_243_tr_comp_late_drop_uci
replace sdiff_243_tr_comp_late_drop_lci = -sdiff_243_tr_comp_late_drop_lci
replace sdiff_243_tr_comp_late_drop = -sdiff_243_tr_comp_late_drop

replace sdiff_243_early_late_drop_lci = -sdiff_243_early_late_drop_lci 
replace sdiff_243_early_late_drop_uci = -sdiff_243_early_late_drop_uci 
replace sdiff_243_early_late_drop = -sdiff_243_early_late_drop


frame create original 
frame original: use mariel_feb_23_2_m1.dta, clear 
*estread using "mariel_feb_23_2_m1.sters"

frame original: generate times = .
frame original: replace times  = 1 if _n==1
frame original: replace times = 3 if _n==2
frame original: replace times = 5 if _n==3

frame original: global covs_3b_pre_dum "mot_egr_early mot_egr_late tr_mod2 sex_dum2 edad_ini_cons esc1 esc2 sus_prin2 sus_prin3 sus_prin4 sus_prin5 fr_cons_sus_prin2 fr_cons_sus_prin3 fr_cons_sus_prin4 fr_cons_sus_prin5 cond_ocu2 cond_ocu3 cond_ocu4 cond_ocu5 cond_ocu6 policonsumo num_hij2 tenviv1 tenviv2 tenviv4 tenviv5 mzone2 mzone3 n_off_vio n_off_acq n_off_sud n_off_oth psy_com2 dep2 rural2 rural3 porc_pobr susini2 susini3 susini4 susini5 ano_nac_corr cohab2 cohab3 cohab4 fis_com2 rc_x1 rc_x2 rc_x3"

frame original: qui noi stpm2 $covs_3b_pre_dum , scale(hazard) df(6) eform tvc(mot_egr_early mot_egr_late) dftvc(1) 

frame original: stpm2_standsurv, at1(mot_egr_early 0 mot_egr_late 0) at2(mot_egr_early 1 mot_egr_late 0) timevar(times) ci contrast(difference) ///
     atvar(s_v_tr_comp s_v_early_drop) contrastvar(sdiff_v_tr_comp_early_drop)

frame original: stpm2_standsurv, at1(mot_egr_early 0 mot_egr_late 0) at2(mot_egr_early 0 mot_egr_late 1) timevar(times) ci contrast(difference) ///
     atvar(s_v_tr_comp0 s_v_late_drop) contrastvar(sdiff_v_tr_comp_late_drop)

frame original: stpm2_standsurv, at1(mot_egr_early 1 mot_egr_late 0) at2(mot_egr_early 0 mot_egr_late 1) timevar(times) ci contrast(difference) ///
     atvar(s_v_early_drop0 s_v_late_drop0) contrastvar(sdiff_v_early_late_drop)	

frame original: drop if missing(times)

*vinculo las bases que tienen tiempos con la más nueva que noi
frlink m:1 times, frame(original) //* 
*traemos las diferencias de supervivencia originales
frget sdiff_v_tr_comp_early_drop sdiff_v_tr_comp_early_drop_lci sdiff_v_tr_comp_early_drop_uci sdiff_v_tr_comp_late_drop sdiff_v_tr_comp_late_drop_lci sdiff_v_tr_comp_late_drop_uci sdiff_v_early_late_drop sdiff_v_early_late_drop_lci sdiff_v_early_late_drop_uci, from(original)

gen neg_sdiff_v_early_late_drop = - sdiff_v_early_late_drop 
gen neg_sdiff_v_early_late_drop_lci = - sdiff_v_early_late_drop_lci 
gen neg_sdiff_v_early_late_drop_uci = - sdiff_v_early_late_drop_uci
 
gen times4= times+.1
gen times5= times2-.1
gen times6= times3+.1 

cap set python_exec  "C:\Users\CISS Fondecyt\AppData\Local\Programs\Python\Python38\python.exe", perm
cap set python_userpath	 "C:\Users\CISS Fondecyt\AppData\Local\Programs\Python\Python38\"
python query
!"C:\Users\CISS Fondecyt\AppData\Local\Programs\Python\Python38\python.exe"  pip -V
!"C:\Users\CISS Fondecyt\AppData\Local\Programs\Python\Python38\python.exe" -m pip install --upgrade pip

!"C:\Users\CISS Fondecyt\AppData\Local\Programs\Python\Python38\python.exe" -m pip install numpy --force-reinstall --upgrade --target="C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/python"
!"C:\Users\CISS Fondecyt\AppData\Local\Programs\Python\Python38\python.exe" -m pip install pandas --upgrade --target="C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/python"

python:
import os
original_value = os.environ.get('ENV_VAR_NAME')

filedir= "C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/python"
end
python:
import os
filedir= "C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/python"
try:  
	import numpy
except ImportError:
	os.system('"C:/Users/CISS Fondecyt/AppData/Local/Programs/Python/Python38/python.exe" -m pip install pandas --upgrade ')
	import numpy
try:
	import pandas as pd
except ImportError:
	os.system('"C:/Users/CISS Fondecyt/AppData/Local/Programs/Python/Python38/python.exe" -m pip install matplotlib --upgrade ')
	import pandas as pd
try:
	import matplotlib.pyplot as plt
	import matplotlib.image as mpimg
except ImportError:
	os.system('"C:/Users/CISS Fondecyt/AppData/Local/Programs/Python/Python38/python.exe" -m pip install matplotlib  --upgrade ')
	import matplotlib.pyplot as plt	
	import matplotlib.image as mpimg
try:
	import statsmodels as sm
except ImportError:
	os.system('"C:/Users/CISS Fondecyt/AppData/Local/Programs/Python/Python38/python.exe" -m pip install statsmodels  --upgrade ')
	import statsmodels as sm	
end

python:
import sfi	
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.image as mpimg
from sfi import Data,Scalar,SFIToolkit
data= sfi.Data.get()
df = pd.DataFrame(data)
df
print(df.head())	
# eliminate fourth row
df = df.drop(3)
print(df.head())	

end

unab varlist: _all
file open myfile using "varlist_pris.txt", write replace
file write myfile `"`varlist'"'
file close myfile
python
with open("varlist_pris.txt", "r") as file:
    varlist = file.read().split()
end

python
if len(varlist) == len(df.columns):
    df.columns = varlist
else:
    print("The length of varlist does not match the number of columns in df")
end

python: print(df.head())


python:
plt.figure(figsize=(10, 6))
import matplotlib.pyplot as plt 
yerr_lower = abs(df['sdiff_243_tr_comp_early_drop'] - df['sdiff_243_tr_comp_early_drop_lci'])
yerr_lower2 = abs(df['sdiff_243_tr_comp_late_drop'] - df['sdiff_243_tr_comp_late_drop_lci'])
yerr_lower3 = abs(df['sdiff_243_early_late_drop'] - df['sdiff_243_early_late_drop_lci'])

yerr_lower4 = abs(df['neg_sdiff_v_early_late_drop'] - df['neg_sdiff_v_early_late_drop_lci'])
yerr_lower5 = abs(df['sdiff_v_tr_comp_early_drop'] - df['sdiff_v_tr_comp_early_drop_lci'])
yerr_lower6 = abs(df['sdiff_v_tr_comp_late_drop'] - df['sdiff_v_tr_comp_late_drop_lci'])

plt.errorbar(df['times'], df['sdiff_243_tr_comp_early_drop'], 
             yerr=yerr_lower, 
             fmt='D', color='gray', ecolor='gray', alpha=0.7, elinewidth=3, capsize=0, label='Early dropout vs. Tr. completion')
plt.errorbar(df['times2'], df['sdiff_243_tr_comp_late_drop'], 
             yerr=yerr_lower2, 
             fmt='D', color='gray', ecolor='gray', alpha=0.3, elinewidth=3, capsize=0, label='Late dropout vs. Tr. completion')
plt.errorbar(df['times3'], df['sdiff_243_early_late_drop'], 
             yerr=yerr_lower3, 
             fmt='D', color='black', ecolor='black', alpha=0.7, elinewidth=3, capsize=0, label='Early vs.Late dropout')			 
			 
plt.errorbar(df['times4'], df['sdiff_v_tr_comp_early_drop'], 
             yerr=yerr_lower, 
             fmt='o', color='gray', ecolor='gray', alpha=0.7, elinewidth=3, capsize=0, label='Original: Early dropout vs. Tr. completion')
plt.errorbar(df['times5'], df['sdiff_v_tr_comp_late_drop'], 
             yerr=yerr_lower5, 
             fmt='o', color='gray', ecolor='gray', alpha=0.3, elinewidth=3, capsize=0, label='Original: Late dropout vs. Tr. completion')
plt.errorbar(df['times6'], df['neg_sdiff_v_early_late_drop'], 
             yerr=yerr_lower6, 
             fmt='o', color='black', ecolor='black', alpha=0.7, elinewidth=3, capsize=0, label='Original: Early vs.Late dropout')					 
			 
plt.xlabel("Years from baseline substance use treatment completion status")
plt.ylabel("Difference in survival probabilities")
plt.legend(frameon=False, framealpha=0)
plt.grid(False)
plt.savefig("_figs/h_m_ns_rp6tvc1_pris_m1_pos_rev_diff2_clus22.png", dpi=500, bbox_inches="tight", pad_inches=0.1)
plt.savefig("_figs/h_m_ns_rp6tvc1_pris_m1_pos_rev_diff2_clus22.pdf", format="pdf", bbox_inches="tight", pad_inches=0.1)
plt.show() 
end

twoway  (rcap sdiff_243_tr_comp_early_drop_lci sdiff_243_tr_comp_early_drop_uci times, color(gs2%75) lcolor(gs2%75) legend(label(1 "Early dropout vs. Tr. completion"))) ///             
		(rcap sdiff_v_tr_comp_early_drop_lci sdiff_v_tr_comp_early_drop_uci times4, color(gs2%75) lcolor(gs2%75) lpattern(dash) legend(label(3 "Late dropout vs. Tr. completion"))) ///
        (rcap sdiff_243_tr_comp_late_drop_lci sdiff_243_tr_comp_late_drop_uci times2, color(gs6%75) lcolor(gs6%75) legend(label(5 "Early vs. Late dropout"))) ///
		(rcap sdiff_v_tr_comp_late_drop_lci sdiff_v_tr_comp_late_drop_uci times5, color(gs6%75) lcolor(gs6%75) lpattern(dash) legend(label(2 "Original: Early dropout vs. Tr. completion"))) ///
		(rcap sdiff_243_early_late_drop_lci sdiff_243_early_late_drop_uci times3, color(gs10%75) lcolor(gs10%75) legend(label(4 "Original: Late dropout vs. Tr. completion"))) ///
		(rcap neg_sdiff_v_early_late_drop_lci neg_sdiff_v_early_late_drop_uci times6, color(gs10%75) lcolor(gs10%75) lpattern(dash) legend(label(6 "Original: Early vs. Late dropout"))) ///
                 (connect  sdiff_243_tr_comp_early_drop times, mcolor(gs2) msymbol(O) mlcolor(gs2) legend(label(1 "Early dropout vs. Tr. completion"))) ///             
                 (connect  sdiff_v_tr_comp_early_drop times4, mcolor(gs2) msymbol(O) mlcolor(gs2) msymbol(diamond) legend(label(3 "Late dropout vs. Tr. completion"))) ///
                 (connect  sdiff_243_tr_comp_late_drop times2, mcolor(gs6) msymbol(O) mlcolor(gs2) legend(label(5 "Early vs. Late dropout"))) ///
                 (connect  sdiff_v_tr_comp_late_drop times5, mcolor(gs6) msymbol(O) mlcolor(gs2) msymbol(diamond) legend(label(2 "Original: Early dropout vs. Tr. completion"))) ///
				 (connect  sdiff_243_early_late_drop times3, mcolor(gs10) msymbol(O) mlcolor(gs2)  legend(label(4 "Original: Late dropout vs. Tr. completion"))) ///
				 (connect  neg_sdiff_v_early_late_drop times6, mcolor(gs10) msymbol(O) mlcolor(gs2) msymbol(diamond) legend(label(6 "Original: Early vs. Late dropout"))) ///
         				  (line zero times, lcolor(black%20) lwidth(thick) legend(label(6 ""))) ///
                 ,xtitle("Years from baseline substance use treatment completion status") ///
				 legend(order( 1 "Early dropout vs. Tr. completion" 2 "Original: Early dropout vs. Tr. completion" 3 "Late dropout vs. Tr. completion" 4 "Original: Late dropout vs. Tr. completion" 5 "Early vs. Late dropout" 6 "Original: Early vs. Late dropout" ) ring(0) pos(7) cols(2) region(lstyle(none)) region(c(none)) nobox  size(2.5)) ///
                 ytitle("Difference in survival probabilities") ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) ) ///
                 name(s_diff_pris_clus_corr_idcen, replace)
				 
forvalues i=7/12 {
  gr_edit .plotregion1.plot`i'.style.editstyle line(width(none)) editcopy
}
forvalues i=1/6 {
  gr_edit legend.plotregion1.key[`i'].xsz.editstyle 9 editcopy
}

graph save "`c(pwd)'\_figs\h_m_ns_rp6tvc1_2pris_m1_pos_rev_diff2_clus_corr.gph", replace
graph export "_figs/h_m_ns_rp6tvc1_2pris_m1_pos_rev_diff2_clus_corr.pdf", as(pdf) name("s_diff_pris_clus_corr_idcen") replace
graph set eps fontface Arial
graph export "_figs/h_m_ns_rp6tvc1_2pris_m1_pos_rev_diff2_clus_corr.eps", as(eps) name("s_diff_pris_clus_corr_idcen") replace



/*
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
# Cluster, municipallity, time to imprisonment
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
*/

clear all

//ID centro
graph use "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\_figs\h_m_ns_rp6tvc1_2pris_m1_pos_rev_diff2_clus2.gph"
//graph use "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2022 (github)\_figs\h_m_ns_rp6_stdif_rmst_m1.gph"
serset dir
serset use

cap gen zero=0
replace sdiff_244_tr_comp_early_drop_uci = -sdiff_244_tr_comp_early_drop_uci 
replace sdiff_244_tr_comp_early_drop_lci = -sdiff_244_tr_comp_early_drop_lci 
replace sdiff_244_tr_comp_early_drop = -sdiff_244_tr_comp_early_drop

replace sdiff_244_tr_comp_late_drop_uci = -sdiff_244_tr_comp_late_drop_uci
replace sdiff_244_tr_comp_late_drop_lci = -sdiff_244_tr_comp_late_drop_lci
replace sdiff_244_tr_comp_late_drop = -sdiff_244_tr_comp_late_drop

replace sdiff_244_early_late_drop_lci = -sdiff_244_early_late_drop_lci 
replace sdiff_244_early_late_drop_uci = -sdiff_244_early_late_drop_uci 
replace sdiff_244_early_late_drop = -sdiff_244_early_late_drop

 
twoway  (rcap sdiff_244_tr_comp_early_drop_lci sdiff_244_tr_comp_early_drop_uci times, color(gs2%75) lcolor(gs2%75) ) ///             
                 (rcap sdiff_244_tr_comp_late_drop_lci sdiff_244_tr_comp_late_drop_uci times2, color(gs6%75) lcolor(gs6%75) ) ///
				 (rcap sdiff_244_early_late_drop_lci sdiff_244_early_late_drop_uci  times3, color(gs10%75) lcolor(gs10%75) ) ///
                 (scatter sdiff_244_tr_comp_early_drop times, mcolor(gs2) msymbol(O) mlcolor(gs2)) ///
                 (scatter sdiff_244_tr_comp_late_drop times2, mcolor(gs6) msymbol(O) mlcolor(gs2)) ///
				 (scatter sdiff_244_early_late_drop times3, mcolor(gs10) msymbol(O) mlcolor(gs2)) ///				 
         				  (line zero times, lcolor(black%20) lwidth(thick)) ///
                 ,xtitle("Years from baseline substance use treatment completion status") ///
                 ytitle("Difference in survival probabilities") ///
                 legend(order( 4 "Early dropout vs. Tr. completion" 5 "Late dropout vs. Tr. completion" 6 "Early vs. Late dropout") ring(0) pos(7) cols(1) region(lstyle(none)) region(c(none)) nobox) ///
				 graphregion(color(white) lwidth(large)) bgcolor(white) ///
				 plotregion(fcolor(white)) graphregion(fcolor(white) )  ///
                 name(s_diff_pris_clus_corr_muni, replace)
graph save "`c(pwd)'\_figs\h_m_ns_rp6tvc1_2pris_m1_pos_rev_diff2_clus2_corr.gph", replace
graph export "_figs/h_m_ns_rp6tvc1_2pris_m1_pos_rev_diff2_clus2_corr.pdf", as(pdf) name("s_diff_pris_clus_corr_muni") replace
graph set eps fontface Arial
graph export "_figs/h_m_ns_rp6tvc1_2pris_m1_pos_rev_diff2_clus2_corr.eps", as(eps) name("s_diff_pris_clus_corr_muni") replace

