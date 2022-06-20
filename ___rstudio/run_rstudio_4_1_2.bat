@ECHO OFF
:: Este es el programa para correr Rstudio con distintas versiones de R. Este programa requiere tener instalado rig (https://github.com/r-lib/rig)
TITLE Desplegar version de R
==========================ECHO PROGRAMA PARA DESPLEGAR LA VERSION DE R 4.1.2============================
ECHO Si no existe, bajaremos la versión de RIG para poder desplegar este codigo. Dejar en el escritorio
IF EXIST "C:\Program Files\rig\rig.exe" (color 0C && echo EL PROGRAMA YA EXISTE) else (color 0A && echo DESPLEGAR LA INSTALACION && START https://github.com/r-lib/rig/releases/download/v0.4.2/rig-windows-0.4.2.exe && pause && echo INSTALAR Y VOLVER A DESPLEGAR)
color 0E && ECHO Ya esta instalado: DESPLEGAR EL PROGRAMA
pause 
start cmd /c "rig rstudio 4.1.2"
