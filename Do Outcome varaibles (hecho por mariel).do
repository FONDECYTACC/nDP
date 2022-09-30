
**Step 1: Identifying the valid cases (those with a FINAL JUDGMENT GUILTY; N=150,598)**

gen validcases=.
replace validcases=1 if agrupa_terminos=="SENTENCIA DEFINITIVA CONDENATORIA"
replace  validcases=0 if validcases==.

label define dico 0 "No" 1 "Yes"
label values validcases dico
table validcases

**Step 2: Generate Outcome variable option 1: imprisonment (N=65008 cases, valid cases)**
gen imprisonment=.
replace imprisonment=1 if clasificacion_pena_47=="Presidio Mayor grado medio"
replace imprisonment=1 if clasificacion_pena_47=="Presidio Mayor grado máximo"
replace imprisonment=1 if clasificacion_pena_47=="Presidio Mayor grado mínimo"
replace imprisonment=1 if clasificacion_pena_47=="Presidio Menor grado medio"
replace imprisonment=1 if clasificacion_pena_47=="Presidio Menor grado máximo"
replace imprisonment=1 if clasificacion_pena_47=="Presidio Menor grado mínimo"
replace imprisonment=1 if clasificacion_pena_47=="Presidio Perpetuo calificado"
replace imprisonment=1 if clasificacion_pena_47=="Presidio Perpetuo simple"
replace imprisonment=1 if clasificacion_pena_47=="Prisión"

replace  imprisonment=0 if imprisonment==.
label values imprisonment dico
table imprisonment if validcases==1

*Actually, I realize that imprisonment variable is exactly the same that marca_pena_44*

***Step 3:  Generate Outcome variable option 2: violent crimes (conservative measure, only considering homicide and violent thefts) (N=32450, valid cases)****
***I am following the chilean prosecutor office to operationalize "violent crime", but I am not considering abortion nor Quasi-delicts committed by health professionals. Also I am not sure if I should add OFFENCES OF TORTURE, ILL-TREATMENT, GENOCIDE, ETC, because the prosecutor Office does not consider it, but also because many crimes on this group are "the intent of"(intent of genocide, give the order of torture, etc.)*** 

**Sobre la familia "malos tratos, genocidio y tortura" se considera por la fiscalia 4 delitos: 1. crimenes de lesa humanidad,2. obtencion de declaraciones forzadas, 3. tormentos y apremios cometidos por empleados publicosñ 4. tormentos y apremios cometidos por paticulares.// Considerar que en el codigo penal: "ART. 150 A.    El empleado público que, abusando de su cargo o sus funciones, aplicare, ordenare o consintiere en que se aplique tortura, será penado con presidio mayor en su grado mínimo. Igual sanción se impondrá al empleado público que, conociendo de la ocurrencia de estas conductas, no impidiere o no hiciere cesar la aplicación de tortura, teniendo la facultad o autoridad necesaria para ello o estando en posición para hacerlo.La misma pena se aplicará al particular que, en el ejercicio de funciones públicas, o a instigación de un empleado público, o con el consentimiento o aquiescencia de éste, ejecutare los actos a que se refiere este artículo." ***

gen violent=.
replace violent=1 if familia_delito=="HOMICIDIOS"
replace violent=1 if familia_delito=="ROBOS"
replace violent=1 if familia_delito=="LESIONES"
replace violent=1 if gls_materia=="SECUESTRO CON HOMICIDIO, VIOLACIÓN O LE"
replace violent=1 if gls_materia=="SECUESTRO CON LESIONES"
replace violent=1 if gls_materia=="SECUESTRO CON VIOLACIÓN"
replace violent=1 if gls_materia=="SUSTRACCIÓN DE MENORES. ART. 142"
replace  violent=0 if violent==.
label values violent dico
table violent if validcases==1

****Step 4:Generate Outcome variable option 2: Type of crime 2(violent as a relaxed measure, considering all crimes that may involve violence) (N=DRUG:4938;VIOLENT:32450;OTHER:113210)
****
gen t_crime3groups=.
replace t_crime3groups=1 if familia_delito=="DELITOS LEY DE DROGAS"
replace t_crime3groups=1 if gls_materia=="HALLAZGO DE DROGAS"
replace t_crime3groups=2 if violent==1
replace t_crime3groups=3 if t_crime3groups==.

label define  t_crime3groups 1 "DRUG RELATED CRIMES" 2 "VIOLENT CRIMES" 3 "OTHER CHARGES"
label values t_crime3groups t_crime3groups
table t_crime3groups if validcases==1

********Step 5: Generate treatment and control groups (SENDA data base)****
gen comp_groups4=.
replace  comp_groups4=1 if  motivodeegreso_mod_imp==2
replace  comp_groups4=2 if  motivodeegreso_mod_imp==1
replace  comp_groups4=3 if  motivodeegreso_mod_imp==4
replace  comp_groups4=4 if  motivodeegreso_mod_imp==5
replace  comp_groups4=4 if  motivodeegreso_mod_imp==3
replace  comp_groups4=4 if  motivodeegreso_mod_imp==6

label define  comp_groups4 1 "CONTROL" 2 "SOME TREATMENT" 3 "FULL TREATMENT" 4"OTHER CONDITIONS"
label values comp_groups4 comp_groups4
table comp_groups4 if validcases==1





