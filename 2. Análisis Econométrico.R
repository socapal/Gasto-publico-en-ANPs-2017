# Definición de Variables y Estadística Descriptiva --------------------

# 1. Librerías y base de datos ------------------------------------------------------------
library(pastecs)
library(ggplot2)
library(dplyr)
library(formattable)
library(stargazer)
library(lmtest)

#Colores
VDemograficas = "#C05555"
VANPs = "#59886B"
VFiscales = "#FFC85C"
Complementario = "#FFF8C1"


#Base de Datos
Gasto_Estatal=read.csv("Bases de Datos/Generadas/Gasto_Estatal.csv")
Gasto_Estatal=Gasto_Estatal[,2:14]
names(Gasto_Estatal)


#Agreagamos mean to median ratio
m2m=Gasto_Estatal$PIB/119.0 
View(m2m)
#
Gasto_Estatal$PIB=m2m

colnames(Gasto_Estatal) <- c("Clave Federal", "Porcentaje Población Estatal",
                         "Porcentaje Población 60+", "Densidad de Población (km²)", 
                         "Estimador Viviendas Particulares",
                         "Población Autoadscrita Indígena","Población Aut. Afrodescendiente", 
                         "Índice Étnico", "Aprox. Distancia Mínima", "Gasto Secretaría M. Ambiente",
                         "Gasto Ajustado al Ingreso", "Escolaridad", "PIB")


#2. Revisión de estadística descriptiva y correlación------------------------------------------------------------

#Estadística Descriptiva
formattable(Gasto_Estatal[1:5,]%>% select(-`Gasto Secretaría M. Ambiente`),5, align =c("l","c","c","c","c", "c", "c", "c", "c", "r"), list(
  `Clave Federal` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  `Porcentaje Población Estatal` = color_bar(Complementario),
  `Porcentaje Población 60+` = color_bar(Complementario),
  `Densidad de Población (km²)` = color_bar(Complementario),
  `Estimador Viviendas Particulares` = color_bar(Complementario),
  `Población Autoadscrita Indígena` = color_bar(VDemograficas),
  `Población Aut. Afrodescendiente` = color_bar(VDemograficas),
  `Índice Étnico` = color_bar(VDemograficas),
  `Aprox. Distancia Mínima` = color_bar(VDemograficas),
  `Gasto Ajustado al Ingreso` = color_bar(VFiscales),
  `Escolaridad` = color_bar(VDemograficas),
  `PIB` = color_bar(VFiscales)
))

#Tabla de correlaciones
##Eliminamos los datos nacionales.
Gasto_Estatal=Gasto_Estatal[2:32,]
##Por alguna razón, R interpreta las últimas dos columnas como argumentos
##"character" en lugar del tipo "numeric" necesario para las funciones
sapply(Gasto_Estatal, class)
Gasto_Estatal=as.data.frame(apply(Gasto_Estatal, 2, as.numeric))
sapply(Gasto_Estatal, class) 


cor_Gasto=as.data.frame(cor(
  remove_missing(Gasto_Estatal%>% select(-`Clave Federal`))
))

#Perdemos información de Coahuila y Tabasco si incluimos los controles de PIB per cápita y Escolaridad.

colnames(cor_Gasto) <- c("Porcentaje Población Estatal",
                         "Porcentaje Población 60+", "Densidad de Población (km²)", 
                         "Estimador Viviendas Particulares",
                         "Población Autoadscrita Indígena","Población Aut. Afrodescendiente", 
                         "Índice Étnico", "Aprox. Distancia Mínima", "Gasto Secretaría M. Ambiente",
                         "Gasto Ajustado al Ingreso", "Escolaridad", "PIB")


formattable(cor_Gasto,
            align =c("l","c","c","c","c", "c", "c", "c","c","c","r"), list(
              ` ` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
              `Porcentaje Población Estatal` = color_tile(Complementario, VDemograficas),
              `Porcentaje Población 60+` = color_tile(Complementario, VDemograficas),
              `Densidad de Población (km²)` = color_tile(Complementario, VDemograficas),
              `Estimador Viviendas Particulares` = color_tile(Complementario, VDemograficas),
              `Población Autoadscrita Indígena` = color_tile(Complementario, VDemograficas),
              `Población Aut. Afrodescendiente` = color_tile(Complementario, VDemograficas),
              `Índice Étnico` = color_tile(Complementario, VDemograficas),
              `Aprox. Distancia Mínima` = color_tile(Complementario, VDemograficas),
              `Gasto Secretaría M. Ambiente` = color_tile(Complementario, VDemograficas),
              `Gasto Ajustado al Ingreso` = color_tile(Complementario, VDemograficas),
              `Escolaridad` = color_tile(Complementario, VDemograficas),
              `PIB` = color_tile(Complementario, VDemograficas)
            ))

#De nuestras variables, destaca la correlación hacia el Gasto Ajustado al Ingreso para:
##Densidad de Población, Población Autodscita Indígena, Población Aut. Afrodescendiente,
##Aprox. Dist. Min, Escolaridad y PIB
##Consideramos lo siguiente: las cuatro variables pob. indígena, pob. afrodescendiente,
## aprox. dist. min y prop. del gasto son proporciones, contra la escolaridad,
## densidad de población y PIB que son variables establecidas.  
 

# 3. Análisis Inicial de Regresión ------------------------------------------------------------

#Regresión Ideal. 
##Está mal por no considerar a las variables como proporción.

# 3.1.1 - Regresión Ideal sin el uso de el índice  étnico -----------------------------------


lm(`Gasto Ajustado al Ingreso` ~ `Densidad de Población (km²)` +
           `Población Aut. Afrodescendiente` + `Población Autoadscrita Indígena`+
           `Aprox. Distancia Mínima` + `Escolaridad`+ `PIB`,
          data=Gasto_Estatal)



# 3.1.2 - Regresión ideal haciendo uso del índice étnico ----------------------------------------------

summary(lm(`Gasto Ajustado al Ingreso` ~ `Densidad de Población (km²)` +
           `Índice Étnico`+
           `Aprox. Distancia Mínima` + `Escolaridad`+ `PIB`,
         data=Gasto_Estatal))

# 3.2 - Regresión de Proporciones-----------------------------------------
#Para esto, utilizaremos un modelo de mínimos cuadrados simples.

# 3.2.1 - Regresión de Proporciones sin utilizar índice étnico ------------
m321=lm(`Gasto Ajustado al Ingreso` ~ `Población Aut. Afrodescendiente` + 
        `Población Autoadscrita Indígena`, data=Gasto_Estatal)
m322=lm(`Gasto Ajustado al Ingreso` ~ `Población Aut. Afrodescendiente` + 
        `Población Autoadscrita Indígena`+`Aprox. Distancia Mínima`,
      data=Gasto_Estatal)
m323=lm(`Gasto Ajustado al Ingreso` ~ `Población Aut. Afrodescendiente` + 
         `Población Autoadscrita Indígena`+`Aprox. Distancia Mínima` + `PIB`,
       data=Gasto_Estatal)

stargazer(m321,m322,m323, type="text", dep.var.labels = "Gasto Ambiental Ajustado al Ingreso Estatal", 
          title="Resultados de Regresión Inicial", digits=1, 
          covariate.labels=c("Población Afrodescendiente", "Población Indígena", "Distancia Mínima", "Media a la Mediana"))


# 3.3.3 -  Modelo  para la Regresión en Proporciones---------------------

me0=lm(`Gasto Ajustado al Ingreso` ~ `Población Aut. Afrodescendiente`, data=Gasto_Estatal)
me1=lm(`Gasto Ajustado al Ingreso` ~ `Población Aut. Afrodescendiente` + 
         `Población Autoadscrita Indígena`, data=Gasto_Estatal)
me2=lm(`Gasto Ajustado al Ingreso` ~ `Población Aut. Afrodescendiente` + 
         `Población Autoadscrita Indígena` +`Porcentaje Población 60+`,
       data=Gasto_Estatal)
me3=lm(`Gasto Ajustado al Ingreso` ~ `Población Aut. Afrodescendiente` + 
         `Población Autoadscrita Indígena`+ `Porcentaje Población 60+`+
         `Aprox. Distancia Mínima`, data=Gasto_Estatal)
me4=lm(`Gasto Ajustado al Ingreso` ~ `Población Aut. Afrodescendiente` + 
         `Población Autoadscrita Indígena`+ `Porcentaje Población 60+`+
         `Aprox. Distancia Mínima` + `PIB`, data=Gasto_Estatal)

stargazer(me0, me1,me2, me3, me4, type="latex", dep.var.labels = "Gasto Ambiental Ajustado al Ingreso Estatal", 
          title="Resultados de Regresión Inicial", digits=1, 
          covariate.labels=c("Población Afrodescendiente", "Población Indígena", 
                             "Porcentaje Población 60+", "Distancia Mínima", "Proporción Ingreso a la Media"),float.env = "sidewaystable")

# 4. Comprobación de supuestos MCO ----------------------------------------

# 4.1 Colinealidad --------------------------------------------------------

datos=data.frame(`Gasto Ajustado al Ingreso`=Gasto_Estatal$`Gasto Ajustado al Ingreso`,
                 `Población Aut. Afrodescendiente`=Gasto_Estatal$`Población Aut. Afrodescendiente`,
                 `Población Autoadscrita Indígena`=Gasto_Estatal$`Población Autoadscrita Indígena`,
                 `Porcentaje Población 60+`=Gasto_Estatal$`Porcentaje Población 60+`,
                 `Aprox. Distancia Mínima`=Gasto_Estatal$`Aprox. Distancia Mínima`,
                 `PIB`=Gasto_Estatal$PIB)

colnames(datos) <- c("Porcentaje Población 60+", 
                       "Población Autoadscrita Indígena",
                       "Población Aut. Afrodescendiente", 
                       "Aprox. Distancia Mínima", 
                       "Gasto Ajustado al Ingreso",
                       "Proporción Ingreso a la Mediana")


cor_Col=as.data.frame(cor(remove_missing(datos)))



colnames(cor_Col) <- c("Porcentaje Población 60+", 
                         "Población Autoadscrita Indígena",
                         "Población Aut. Afrodescendiente", 
                         "Aprox. Distancia Mínima", 
                         "Gasto Ajustado al Ingreso",
                        "Proporción Ingreso a la Mediana")


formattable(cor_Col,
            align =c("l","c","c","c","c","r"), list(
              ` ` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
              `Porcentaje Población 60+` = color_tile(Complementario, VDemograficas),
              `Población Autoadscrita Indígena` = color_tile(Complementario, VDemograficas),
              `Población Aut. Afrodescendiente` = color_tile(Complementario, VDemograficas),
              `Aprox. Distancia Mínima` = color_tile(Complementario, VDemograficas),
              `Gasto Ajustado al Ingreso` = color_tile(Complementario, VDemograficas),
              `Proporción Ingreso a la Mediana`=color_tile(Complementario, VDemograficas)
              
            ))

#Ninguna correlación es mayor a 0.7, pero observamos que nuestra R^2 podría ser menor
##al eliminar a la población anciana. Lo decidimos más adelante.


# 4.2 No heterosticidad -------------------------------------------------------
bp_me4=bptest(me4)
print(bp_me4)

#Tal que p-value de 0.09  <   0.1 
#Esto es, con un 90% de confianza puede descartarse la hipótesis nula. Hay evidencia
#estadística de la existencia de heterosticidad en el modelo.

#Redefinición del índice étnico.
Etnico=(1-((datos$`Población Autoadscrita Indígena`)^2+(datos$`Población Aut. Afrodescendiente`)^2 +
  (1-datos$`Población Autoadscrita Indígena`-datos$`Población Aut. Afrodescendiente`)^2))
datos$Ethnic=Etnico


colnames(datos) <- c("Porcentaje Población 60+", 
                     "Población Autoadscrita Indígena",
                     "Población Aut. Afrodescendiente", 
                     "Aprox. Distancia Mínima", 
                     "Gasto Ajustado al Ingreso",
                     "Proporción Ingreso a la Mediana",
                     "Diversidad Étnica")

cor_ColE=as.data.frame(cor(remove_missing(datos)))

colnames(cor_ColE) <-   c("Porcentaje Población 60+", 
                       "Población Autoadscrita Indígena",
                       "Población Aut. Afrodescendiente", 
                       "Aprox. Distancia Mínima", 
                       "Gasto Ajustado al Ingreso",
                       "Proporción Ingreso a la Mediana",
                       "Diversidad Étnica")


formattable(cor_ColE,
            align =c("l","c","c","c","c","c","r"), list(
              ` ` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
              `Porcentaje Población 60+` = color_tile(Complementario, VDemograficas),
              `Población Autoadscrita Indígena` = color_tile(Complementario, VDemograficas),
              `Población Aut. Afrodescendiente` = color_tile(Complementario, VDemograficas),
              `Aprox. Distancia Mínima` = color_tile(Complementario, VDemograficas),
              `Gasto Ajustado al Ingreso` = color_tile(Complementario, VDemograficas),
              `Proporción Ingreso a la Mediana`=color_tile(Complementario, VDemograficas),
              `Diversidad Étnica` = color_tile(Complementario, VDemograficas)
            ))


me_0=lm(`Gasto Ajustado al Ingreso` ~ `Diversidad Étnica`, data=datos)
me_1=lm(`Gasto Ajustado al Ingreso` ~ `Diversidad Étnica` + 
          `Aprox. Distancia Mínima`, data=datos)
me_2=lm(`Gasto Ajustado al Ingreso` ~ `Diversidad Étnica` + 
          `Aprox. Distancia Mínima` +`Porcentaje Población 60+`,
       data=datos)
me_3=lm(`Gasto Ajustado al Ingreso` ~ `Diversidad Étnica` + 
          `Aprox. Distancia Mínima` +`Porcentaje Población 60+`+`Proporción Ingreso a la Mediana`,
        data=datos)


stargazer(me_0, me_1,me_2,me_3, type="latex", dep.var.labels = "Gasto Ambiental Ajustado al Ingreso Estatal", 
          title="Resultados de Regresión Inicial", digits=1, 
          covariate.labels=c("Diversidad Étnica", 
                             "Aprox. Distancia Mínima", "Porcentaje Población 60+", "Proporción Ingreso a la Mediana"))



#Prueba de no heterosticidad.

#Prueba Breusch-Pagan
bp=bptest(me_3)
print(bp)

#Entendemos que no hay evidencia estadísticamente significativa de heterosticidad en el modelo.

#Prueba White Términos Cruzados
bptest(me_3,  ~ fitted(me_3)+ I(fitted(me_3)^2) )


# 4.3 Autocorrelación contemporánea ---------------------------------------

#Prueba Dubin-Watson
dw=dwtest(me_3)
dw
#No hay evidencia 
#est. significativa para descartar la hipótesis nula 
#sobre autocorrelación contemporánea

#Prueba Breush-Godofrey
bgtest(me_3)
#No podemos descartar la hipótesis nula sobre la 
#no existencia de autocorrelación contemporánea

# 4.4 Normalidad
library(tseries)
jarque.bera.test(residuals(me_3)) 


shapiro.test(residuals(me_3))


# 4.5 Forma Funcional  ----------------------------------------------------
resettest(me_3)

#Podemos rechazar la hipótesis nula con una significancia del 0.99.Mala especificación.

summary(datos)


  