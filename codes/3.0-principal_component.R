# Componentes principales ---- 
# ¿Es posible una reduccion de dimensionalidad, cómo se interpreta?
library(factoextra)   # Principal component
library(ggplot2)      # Gráficas

# Limpieza de la base ----

setwd("D:/Windows/Desktop/Sexto semestre/Analisis estadistico de datos/proyecto")

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

EAS_2019$sector = as.numeric(as.factor(EAS_2019$sector))
# La variable categorica se convierte a numerica para realizar el analisis de 
# componentes principales

EAS_2019 = EAS_2019 %>% mutate(edad = 2019 - idaio)
EAS_2019 = EAS_2019 %>% select(-Division, -intio, -idaio)
# Se elimina la variable division (ahora se utilizara sector) y la variable 
# dependiente ingreso.

EAS_2019 = na.omit(EAS_2019)

# Principal component ----
res.pca <- prcomp(EAS_2019, center = TRUE, scale = TRUE)

fviz_eig(res.pca)

# Eigenvalues and proportion
eig.val <- get_eigenvalue(res.pca)
ggplot(data = eig.val, aes(x = 1:nrow(eig.val), y = cumulative.variance.percent)) + theme_minimal() +
  geom_bar(stat = 'identity', fill = "lightblue") + xlab("Valor propio") + ylab("Porcentaje acumulado de varianza explicado")
ggsave("./images/accumulated.jpeg", width = 8, height = 6)

# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord         # Contributions to the PCs of each variable

# ----
