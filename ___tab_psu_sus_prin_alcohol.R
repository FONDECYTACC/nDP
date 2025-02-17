invisible("Adicionales en E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/_proposal_grant/2023/para_pres_cum_mean.R")
folder_path <- ifelse(dir.exists("E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/_proposal_grant/2023/"),
                      "E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/_proposal_grant/2023/",
                      "C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2022 (github)/_proposal_grant/2023/")
load(paste0(folder_path,"an_grant_23_24_4.RData"))

if(!require(geepack)){install.packages("geepack");library(geepack)}
if(!require(geeM)){install.packages("geeM");library(geeM)}
if(!require(tidyverse)){install.packages("tidyverse");library(tidyverse)}
if(!require(MASS)){install.packages("MASS");library(MASS)}
if(!require(geeasy)){install.packages("geeasy");library(geeasy)}
if(!require(MuMIn)){remotes::install_version("MuMIn", "1.46.0");library(MuMIn)}
if(!require(tableone)){install.packages("tableone");library(tableone)}
if(!require(knitr)){install.packages("knitr");library(knitr)}
if(!require(emmeans)){install.packages("emmeans");library(emmeans)}
if(!require(biostat3)){install.packages("biostat3");library(biostat3)}
if(!require(rms)){install.packages("rms");library(rms)}
if(!require(meta)){install.packages("meta");library(meta)}
try(if(!require(dmetar)){install.packages("dmetar");library(dmetar)})
if(!require(metafor)){install.packages("metafor");library(metafor)}
try(if(!require(extrafont)){install.packages("extrafont");library(extrafont)})
try(if(!require(showtext)){install.packages("showtext");library(showtext)})
try(if(!require(mice)){install.packages("mice");library(mice)})


table(data_mine_miss_restr_proc2exp2$tipo_de_plan_2_mod)
# 
# basic ambulatory GP intensive ambulatory          GP residential WO intensive ambulatory          WO residential 
# 9993                   11497                    5204                    1760                    2534 


subset(data_mine_miss_restr_proc2exp2, tipo_de_plan_2_mod == "GP residential")


#broom::tidy(nocorr_nowgt_int_list[[5]], exponentiate=T, conf.int=T)[1:3,]

#    model                         term          RR               p.value
#    <chr>                         <chr>         <chr>            <chr>  
#  1 model_basic_ambulatory        rec1.only PSU 1.01 (0.98-1.04) 0.4849 
#  2 model_basic_ambulatory        rec2.both     1.08 (1.05-1.12) 0.0000 
#  3 model_GP_intensive_ambulatory rec1.only PSU 1.02 (0.99-1.05) 0.1599 
#  4 model_GP_intensive_ambulatory rec2.both     1.10 (1.07-1.14) 0.0000 
#  5 model_GP_residential          rec1.only PSU 0.99 (0.94-1.04) 0.6761 
#  6 model_GP_residential          rec2.both     0.89 (0.83-0.94) 0.0002 
#  7 model_WO_intensive_ambulatory rec1.only PSU 0.96 (0.89-1.02) 0.2034 
#  8 model_WO_intensive_ambulatory rec2.both     1.07 (0.99-1.15) 0.0767 
#  9 model_WO_residential          rec1.only PSU 1.14 (1.06-1.23) 0.0008 
# 10 model_WO_residential          rec2.both     1.14 (1.05-1.24) 0.0030 
# 
# 

round(prop.table(table(
            subset(data_mine_miss_restr_proc2exp2, tipo_de_plan_2_mod == "GP residential")[["policonsumo2_rec"]],
            subset(data_mine_miss_restr_proc2exp2, tipo_de_plan_2_mod == "GP residential")[["sus_principal_mod"]]
        ),1),2)

round(prop.table(table(
  subset(data_mine_miss_restr_proc2exp2, tipo_de_plan_2_mod == "basic ambulatory")[["policonsumo2_rec"]],
  subset(data_mine_miss_restr_proc2exp2, tipo_de_plan_2_mod == "basic ambulatory")[["sus_principal_mod"]]
),1),2)

round(prop.table(table(
       subset(data_mine_miss_restr_proc2exp2, tipo_de_plan_2_mod == "GP intensive ambulatory")[["policonsumo2_rec"]],
       subset(data_mine_miss_restr_proc2exp2, tipo_de_plan_2_mod == "GP intensive ambulatory")[["sus_principal_mod"]]
   ),1),2) 

round(prop.table(table(
       subset(data_mine_miss_restr_proc2exp2, tipo_de_plan_2_mod == "WO intensive ambulatory")[["policonsumo2_rec"]],
       subset(data_mine_miss_restr_proc2exp2, tipo_de_plan_2_mod == "WO intensive ambulatory")[["sus_principal_mod"]]
   ),1),2)

round(prop.table(table(
  subset(data_mine_miss_restr_proc2exp2, tipo_de_plan_2_mod == "WO residential")[["policonsumo2_rec"]],
  subset(data_mine_miss_restr_proc2exp2, tipo_de_plan_2_mod == "WO residential")[["sus_principal_mod"]]
),1),2)
invisible("En residential, el cocaine paste es de 77% en both, y 35% en alcohol-noPSU")
invisible("Tienen menos cloridrato de cocaina y consumo de marihuana en porcentajes")
#
# Entonces, tengo un modelo que ajusta por sustancia principal, pero al ajustar por policonsumo
# introduzco un sesgo porque el policonsumo-alcohol, asume específicamente que no tiene both, 
# quien tiene sus principal por alcohool. Y en el modelo está la sustancia principal como predictor
# Por tanto, hay un estrato de sustancia principal en el que no habrá casos
# 
# WO intensive ambulatory  WO residential
prop.table(table(
    subset(data_mine_miss_restr_proc2exp2, tipo_de_plan_2_mod == "GP residential")[["tr_outcome"]],
    subset(data_mine_miss_restr_proc2exp2, tipo_de_plan_2_mod == "GP residential")[["policonsumo2_rec"]]
        ),2)
# GP residential both 63,8% = no completa
# GP residential no PSU 68.7% = no completa
#
# en basic ambulatory both 84,8% = no completa
# en basic ambulatory no psu 74,4% ) no PSU
# 
# GO intensive ambulatory both 83,7%
# GP intensive ambulatory no PSU 71,6%  = no completa
# 
# WO intensive ambulatory both 83,9%
# WO intensive ambulatory no PSU 72,6% = no completa
# 
# WO residential PSU both 74,7%
# WO residential no PSU  62,7% = no completa
# 

invisible("Ver la interacción de todos los niveles: resultado tratamiento, sustancia, nivel PSU y setting"))
xtabs(~ tr_outcome + sus_principal_mod + policonsumo2_rec+ tipo_de_plan_2_mod, data = data_mine_miss_restr_proc2exp2)


xtabs(~ tr_outcome + comp_bpsc_y3_severe + policonsumo2_rec+ tipo_de_plan_2_mod, data = data_mine_miss_restr_proc2exp2)
invisible("elo sque tienen both tienen un perfil super severo respecto de los no PSU, como 79% vs. 55%")