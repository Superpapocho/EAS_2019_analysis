# MANOVA ----
# ¿Es igual el comportamiento de la proporción de mujeres y márgenes brutos entre sectores?

library(readr)      # Read csv
library(haven)      # Read dta
library(dplyr)      # database manipulation
library(tidyverse)  # database manipulation
library(heplots)    # boxM test

# Limpieza de la base ----

setwd("D:/Windows/Desktop/Sexto semestre/Analisis estadistico de datos/proyecto/")

EAS_2019 = read_dta("raw/EAS_2019.dta")

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
# Se transforma la variable division en una mas agregada "sector"
EAS_2019$sector = as.factor(EAS_2019$sector)
EAS_2019 = EAS_2019 %>% select(-Division)

str(EAS_2019)

# Se crean las variables margen y eq_genero que representan los margenes de 
# ganancia que tiene la empresa y la proporción que representan las mujeres 
# en el total de personal ocupado
EAS_2019 = EAS_2019 %>% mutate(margen = (intio - ocgtot) / intio, eq_genero = pomtot / pottot)
EAS_2019 = EAS_2019 %>% filter_all(all_vars(!is.infinite(.)))
# Elimina margen infinito
EAS_2019 = na.omit(EAS_2019)
# Elimina na's

# Se realiza un analisis manova one-way para verificar si estas 
# dos variables se comportan de manera distinta entre sectores.

margen = EAS_2019$margen
eq_genero = EAS_2019$eq_genero
sector = EAS_2019$sector

# Verificar supuestos ----

# Normalidad multivariada - Se cumple

# Independencia - Se cumple

# Varianzas iguales: Realizamos una prueba box
res <- boxM(cbind(margen,eq_genero),sector); res

# Presencia de outliers - Mahalanobis Distance
verify_outliers = data.frame(margen,eq_genero)
verify_outliers$mahal = mahalanobis(verify_outliers, colMeans(verify_outliers), cov(verify_outliers))
# Create new column in data frame to hold p-value for each Mahalanobis distance
verify_outliers$p <- pchisq(verify_outliers$mahal, df=1, lower.tail=FALSE)
table(verify_outliers$p > 0.001)
# Se considera outlier aquellas observaciones que tengan un p-valor menor a 0.001
# Existen dos outliers para estas variables
EAS_2019 = EAS_2019[verify_outliers$p > 0.001,]
# Las eliminamos de nuestro dataframe

# Reference: https://www.statology.org/manova-assumptions/

# Manova ----
res.man = manova(cbind(margen, eq_genero)~sector, data = EAS_2019)
summary.aov(res.man)

# Proporcion de mujeres por sector ----
# Con la proporcion de trabajadores que son mujeres si se ve afectado significativamente
# por el sector al que pertenece la empresa.

EA = EAS_2019 %>% group_by(sector) %>% summarize(meq_genero = mean(eq_genero))
ggplot(EA, aes(x=sector,y = meq_genero))+geom_col(fill = "#FF6666")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  scale_x_discrete(labels = c('Administrativos','Cientificos y tecnicos','Educacion',
                              'Entretenimiento','Hoteles y restaurantes','Informacion y comunicación',
                              'Inmobiliario','Otros','Salud y asistencia social','Transporte y almacenamiento'))+
  ylab("Proporcion de mujeres con respecto al total de trabajadores") +
  xlab("Sector de servicios")

ggsave("./images/gender_prop.jpeg",width = 8,
       height = 8)
# ----