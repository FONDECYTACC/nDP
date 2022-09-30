// para crear la base principal
use "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\fiscalia_mariel_sep_2022_match_SENDA.dta" , clear

// You can create frames, and delete them, and rename them. The commands are
// . frame create framename
// . frame drop framename
// . frame rename oldname newname
// Stata will list the names of all the existing frames if you type
//
// . frames dir

// cambias a una nueva base pero temporal
frame create temp
//en esa ventana, cargamos la data, eliminarmos los casos que no son sentencia definitiva condenatoria, 
frame temp: use "E:\Mi unidad\Alvacast\SISTRAT 2022 (github)\fiscalia_mariel_sep_2022_match_SENDA.dta" , clear
//filtramos en agrupa terminos aquellos valores que llevados a mayúscula no contienen la palabra condenatoria
frame temp: drop if strpos(upper(agrupa_terminos),"CONDENATORIA")<1
frame temp: tab agrupa_terminos
//filtramos a los que 
frame temp: drop if encontrado_como_imputado=="NO"
frame temp: tab encontrado_como_imputado

********
*0c. Ordenar base de datos
** Ordena primero por hash key, y después por fecha de comisión delito, 
** desde el mas reciente al más nuevo.
*dependiendo del signo que se le asigne a fecha de comisión delito, 
*es si será de menor (más antiguo) a mayor (más reciente) (+), o mayor a menor (-)

frame temp: sort rut_enc_saf edad_comision_imp // de menor edad hasta mayor edad
//Vemos la base
frame temp: edit rut_enc_saf edad_comision_imp imp_birth_date  

*0.d. Contar cuántas veces se repite cada HASH
frame temp: cap quietly by rut_enc_saf:  gen dup_po = cond(_N==1,0,_n)
frame temp: label variable dup_po "Número de registros por HASH (posición a medida que aparecen, menor es más reciente)"
//Vemos la base
frame temp: edit rut_enc_saf edad_comision_imp imp_birth_date dup_po

*La ventaja de duplicates_filtered es que considera los perdidos
frame temp: egen duplicates_filtered_po=count(rut_enc_saf), by(rut_enc_saf)
frame temp: label variable duplicates_filtered_po "Número de registros por HASH (Total)"
//Vemos la base
frame temp: edit rut_enc_saf edad_comision_imp imp_birth_date dup_po duplicates_filtered_po

*me quedo con la primera observación de cada caso, y con los casos con único tratamiento
frame temp: gen a_botar2=.
frame temp: recode a_botar2 .=1 if dup_po==0 & duplicates_filtered_po==1
*siempre el primer tratamiento, si tiene más de uno
frame temp: recode a_botar2 .=1 if dup_po==1 & duplicates_filtered_po>1
frame temp: label variable a_botar2 "Descarta tratamientos que no permitan ver la duración del usuario entre 2010-2019"
//Vemos la base
frame temp: edit rut_enc_saf edad_comision_imp imp_birth_date dup_po duplicates_filtered_po a_botar2

*para ver los casos que eliminaría (missing)
frame temp: tab a_botar2, miss
********
*Botar tratamientos posteriores
frame temp: drop if missing(a_botar2) //*95,051 se eliminan
frame temp: edit rut_enc_saf edad_comision_imp imp_birth_date dup_po duplicates_filtered_po a_botar2
********
*0a. ¿Hay duplicados en términos de hash y fecha de ingreso?
frame temp: duplicates drop rut_enc_saf edad_comision_imp, force
*Ojo: esto es para un primer caso de clasificación pena. Los siguientes no t
frame temp: tab clasificacion_pena_47, miss
frame temp: edit rut_enc_saf edad_comision_imp imp_birth_date dup_po clasificacion_pena_47