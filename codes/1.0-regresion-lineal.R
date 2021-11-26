# Regresión lineal múltiple ----
# ¿Qué tan importante es la equidad de género en los resultados financieros de las empresas?

library(readr)  # Read csv
library(haven)  # Read dta
library(dplyr)  # database manipulation
library (stargazer)

setwd("D:/Desktop/MACC/VII Semestre/Análisis Estadístico de Datos/Proyecto")
EAS_2019 = EAS_2019 %>% select(Division, intio, ocgtot, CONINTER, SULSAL, PRESTA, VALAGRE, TOTG, idaio, insertot, potpsfr, potpau, pottot, pomtot, gpptpot, ocgrps, gmesetot, gsptot, ocgid, ocgoi, opcomex)

EAS_2019$sector = NA
EAS_2019$sector = ifelse(EAS_2019$Division >= 49,"trans_alma",EAS_2019$sector)
EAS_2019$sector = ifelse(EAS_2019$Division >= 55,"hot_rest",EAS_2019$sector)
EAS_2019$sector = ifelse(EAS_2019$Division >= 58,"inf_com",EAS_2019$sector)
EAS_2019$sector = ifelse(EAS_2019$Division >= 68,"inmobiliario",EAS_2019$sector)
EAS_2019$sector = ifelse(EAS_2019$Division >= 69,"cient_tec",EAS_2019$sector)
EAS_2019$sector = ifelse(EAS_2019$Division >= 77,"administrativos",EAS_2019$sector)
EAS_2019$sector = ifelse(EAS_2019$Division >= 85,"educacion",EAS_2019$sector)
EAS_2019$sector = ifelse(EAS_2019$Division >= 86,"salud_asis",EAS_2019$sector)
EAS_2019$sector = ifelse(EAS_2019$Division >= 90,"entretenimiento",EAS_2019$sector)
EAS_2019$sector = ifelse(EAS_2019$Division >= 94,"otros",EAS_2019$sector)

EAS_2019$sector = as.factor(EAS_2019$sector)

EAS_2019 = EAS_2019 %>% select(-Division)

#Modificamos la variable de años
EAS_2019$idaio = 2019 - EAS_2019$idaio
na.omit(EAS_2019)

#Normalizando los datos
EAS_2019$intio = scale(EAS_2019$intio)
EAS_2019$ocgtot = scale(EAS_2019$ocgtot)
EAS_2019$CONINTER = scale(EAS_2019$CONINTER)
EAS_2019$SULSAL = scale(EAS_2019$SULSAL)
EAS_2019$PRESTA = scale(EAS_2019$PRESTA)
EAS_2019$VALAGRE = scale(EAS_2019$VALAGRE)
EAS_2019$TOTG = scale(EAS_2019$TOTG)
EAS_2019$idaio = scale(EAS_2019$idaio)
EAS_2019$insertot = scale(EAS_2019$insertot)
EAS_2019$potpsfr = scale(EAS_2019$potpsfr)
EAS_2019$potpau = scale(EAS_2019$potpau)
EAS_2019$pottot = scale(EAS_2019$pottot)
EAS_2019$pomtot = scale(EAS_2019$pomtot)
EAS_2019$gpptpot = scale(EAS_2019$gpptpot)
EAS_2019$ocgrps = scale(EAS_2019$ocgrps)
EAS_2019$gmesetot = scale(EAS_2019$gmesetot)
EAS_2019$gsptot = scale(EAS_2019$gsptot)
EAS_2019$ocgid = scale(EAS_2019$ocgid)
EAS_2019$ocgoi = scale(EAS_2019$ocgoi)

#Modelos 
mod_gen=lm(intio ~ idaio+potpsfr+potpau+pomtot, data=EAS_2019)
mod_trans_alma=lm(intio ~ idaio+potpsfr+potpau+pomtot, filter(EAS_2019, sector == "trans_alma"))
mod_hot_rest=lm(intio ~ idaio+potpsfr+potpau+pomtot, filter(EAS_2019, sector == "hot_rest"))
mod_inf_com=lm(intio ~ idaio+potpsfr+potpau+pomtot, filter(EAS_2019, sector == "inf_com"))
mod_inmobiliario=lm(intio ~ idaio+potpsfr+potpau+pomtot, filter(EAS_2019, sector == "inmobiliario"))
mod_cient_tec=lm(intio ~ idaio+potpsfr+potpau+pomtot, filter(EAS_2019, sector == "cient_tec"))
mod_administrativos=lm(intio ~ idaio+potpsfr+potpau+pomtot, filter(EAS_2019, sector == "administrativos"))
mod_educacion=lm(intio ~ idaio+potpsfr+potpau+pomtot, filter(EAS_2019, sector == "educacion"))
mod_salud_asis=lm(intio ~ idaio+potpsfr+potpau+pomtot, filter(EAS_2019, sector == "salud_asis"))
mod_entretenimiento=lm(intio ~ idaio+potpsfr+potpau+pomtot, filter(EAS_2019, sector == "entretenimiento"))
mod_otros=lm(intio ~ idaio+potpsfr+potpau+pomtot, filter(EAS_2019, sector == "otros"))

