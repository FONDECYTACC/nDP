 *orden y patria
 import delimited "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\_Encriptacion y SISTRAT 2010\0.ejemplo_tab.txt"
 
* Assuming the RUN values are in a variable named "run"

* Eliminar espacios y puntos
gen cleaned_run = subinstr(rut, " ", "", .)
replace cleaned_run = subinstr(cleaned_run, ".", "", .)

* Eliminar todos los caracteres especiales distintos de números o hyphens
gen final_run = regexm(cleaned_run, "([0-9\-]+)")

* Drop the intermediate variable
drop cleaned_run

* Assuming the RUN values are in a variable named "run"
gen numeric_run = subinstr(final_run, "\\.", "", .) // Eliminar puntos
replace numeric_run = substr(numeric_run, 1, length(numeric_run)-2) // Remove hyphen and verification digit




****************************************************************************

import delimited "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\_Encriptacion y SISTRAT 2010\1.plantilla_hon_lo_barne.txt", clear 

* Eliminar espacios y puntos de la columna "rut" (asumiendo que tiene ese nombre)
gen cleaned_run = subinstr(rut, " ", "", .)
replace cleaned_run = subinstr(cleaned_run, ".", "", .)

* Descartar todos los caracteres especiales distintos de números o hyphens, fuera de números o K's
gen final_run = regexr(cleaned_run, "[^0-9\\K\\k\\-]+", "")

* Eliminar puntos
gen numeric_run = subinstr(final_run, "\\.", "", .) // Eliminar puntos

* Si tiene un hypen y un número al lado, se eliminan los últimos 2 números
replace numeric_run = substr(numeric_run, 1, length(numeric_run)-2) if substr(numeric_run, length(numeric_run)-1, 1) == "-" & regexm(substr(numeric_run, length(numeric_run), 1), "[0-9|K]")

*comparar
list numeric_run final_run rut // siempre y cuando se llame rut la columna inicial


****************************************************************************

import delimited "C:\Users\CISS Fondecyt\Mi unidad\Alvacast\_Encriptacion y SISTRAT 2010\2.Plantilla_honorarios.txt", varnames(1) clear 

*Drop aberrant cases
drop if regexm(rut, "BARNECHEA 2017")
drop if regexm(rut, "2017")
drop if regexm(rut, "COMUNITARIAS DE LO BARNECHEA")
drop if regexm(rut, "COMUNICACIÓN EXTERNA PARA LOS VECINOS")
drop if regexm(rut, "CALIDAD DE VIDA")
drop if regexm(rut, "APOYO Y FORMACIÓN PARAR EL DESARROLLO DE LA MUJER")
drop if regexm(rut, "INFORMACIÓN EN EL USO ESPACIOS EN VÍA PUBLICA")

 
* Eliminar espacios y puntos de la columna "rut" (asumiendo que tiene ese nombre)
gen cleaned_run = subinstr(rut, " ", "", .)
replace cleaned_run = subinstr(cleaned_run, ".", "", .)

* Descartar todos los caracteres especiales distintos de números o hyphens, fuera de números o K's
gen final_run = regexr(cleaned_run, "[^0-9\\K\\k\\-]+", "")

* Eliminar puntos
gen numeric_run = subinstr(final_run, "\\.", "", .) // Eliminar puntos

* Si tiene un hypen y un número al lado, se eliminan los últimos 2 números
replace numeric_run = substr(numeric_run, 1, length(numeric_run)-2) if substr(numeric_run, length(numeric_run)-1, 1) == "-" & regexm(substr(numeric_run, length(numeric_run), 1), "[0-9|K]")

*comparar
list numeric_run final_run rut in 1/50 // siempre y cuando se llame rut la columna inicial, primeras 50 observaciones
