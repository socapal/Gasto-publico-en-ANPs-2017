# Definición de Variables y Estadística Descriptiva --------------------

GEOS=read.csv("Bases de datos/Generadas/GEOS.csv") #Datos Geoestadísticos

# 1. Librerías ------------------------------------------------------------

library(pastecs)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(formattable)

#Definimos los colores para la identidad gráfica del reporte.
VDemograficas = "#C05555"
VANPs = "#59886B"
VFiscales = "#FFC85C"
Complementario = "#FFF8C1"

#Renombraremos las variables :(

#2. Variables a nivel distrito-------------------------------------------------------------------------

#Ajustes sobre la base de datos geoestadística.

stat.desc(GEOS)

##Distritos por entidad
HD_T=GEOS %>%
    ggplot( aes(x=CVE_ENT)) +
    ggtitle("Distritos por entidad")+
    theme(plot.title = element_text(size=12))+
    geom_histogram(bins=32, fill=VDemograficas, color=Complementario, show.legend = FALSE)+
    theme_classic()

##Distritos con muestra insuficiente por entidad
HD_MI=subset(GEOS,GEOS$MI=="*") %>% 
      ggplot(aes(x=CVE_ENT)) +
      ggtitle("Distritos con muestra insuficiente por entidad") +
      theme(plot.title = element_text(size=12))+
      geom_histogram(bins=32, fill='VDemograficas', color='#f1f1e8',show.legend = TRUE)+
      theme_classic()

##Distritos identificados como indígenas por entidad.
HD_I=subset(GEOS, GEOS$Indigena=="SI") %>%
      ggplot( aes(x=CVE_ENT)) +
      ggtitle("Distritos identificados como índigenas por entidad") +
      theme(plot.title = element_text(size=12))+
      geom_histogram(bins=32, fill='#206a5d', color='#f1f1e8',show.legend = TRUE)+
      theme_classic()

HD=grid.arrange(HD_T, HD_MI, HD_I, ncol=1, nrow=3)

#Distritos con muestra insuficiente.
GEOS_MI=subset(GEOS, GEOS$MI=="*")
stat.desc(GEOS_MI)

GEOS=subset(GEOS, GEOS$MI!="*")  #Removemos nueve distritos con una muestra insuficiente. 


# 2.1 Índice étnico -------------------------------------------------------


#El coeficiente étnico está generado con base en el paper de Alesina, Baqir y Easterly (1999).
#Bajo la consideración de los autores, "representa la probabilidad de encontrarte con una
#persona de tu misma etnia. Por lo tanto, lo interpretamos como que una población con 
#índice étnico de 1 es perfectamente homogénea.

#Fórmula:
#    100-%Pob. Índigena - %Pob. Afrodescendiente [iND_141 índigena, 128_afrodescendiente]
GEOS$Ethnic= (100-GEOS$IND_141-GEOS$IND_128)/100
summary(GEOS$Ethnic)

#Histograma

GEOS %>%
  ggplot( aes(x=Ethnic)) +
  ggtitle("Coeficiente étnico") +
  theme(plot.title = element_text(size=12))+
  geom_histogram(bins=60, fill="#206a5d", color='#f1f1e8')+
  theme_classic()+
  geom_vline(aes(xintercept = mean(Ethnic)),col='black',size=0)

mean(GEOS$Ethnic)
mean(GEOS_MI$Ethnic)

#Correlaciones entre variables.
#Entre variables del índice 
cor(GEOS$IND_141,GEOS$IND_128) #0.28


# 2.2 Distancia -----------------------------------------------------------

# Distancia mínima euclediana desde la estimación del 
# centroide de los polígonos de ambos tipos.
##Denotada bajo c_ANPCercana y c_distmin.


#En ambos casos escalamos la distancia para que su valor 
## máximo sea de uno.

EscalaMinMax <- function(x){
  return((x-min(x))/(max(x)-min(x)))}

#Un estadístico más bello, la verdad.
dist_GEOS %>%
  ggplot( aes(x=EmM_cdistmin)) +
  ggtitle("Distancia estandarizada") +
  theme(plot.title = element_text(size=12))+
  geom_histogram(bins=60, fill="#206a5d", color='#f1f1e8')+
  theme_classic()+
  geom_vline(aes(xintercept = mean(EmM_cdistmin)),col='black',size=0)

#Desechamos el análisis de la distancia euclediana de 
#gran círculo entre cada frontera. Es decir, trabajamos 
#sólo en GEOS.

# 2.3 Relación índice étnico y distancia mínima ------------------------


#Removemos temporalmente valores límite, media y desv. est.
names(GEOS)

# [1] "X"            "ï..CVE_FED"   "CVE_ENT"      "distrito"    
# [5] "MI"           "Indigena"     "Complejidad"  "IND_002"     
# [9] "IND_002_LI"   "IND_002_LS"   "IND_002_EE"   "IND_002_CV"  
# [13] "IND_824"      "IND_824_LI"   "IND_824_LS"   "IND_824_EE"  
# [17] "IND_824_CV"   "IND_047"      "IND_047_LI"   "IND_047_LS"  
# [21] "IND_047_EE"   "IND_047_CV"   "IND_055"      "IND_055_LI"  
# [25] "IND_055_LS"   "IND_055_EE"   "IND_055_CV"   "IND_141"     
# [29] "IND_141_LI"   "IND_141_LS"   "IND_141_EE"   "IND_141_CV"  
# [33] "IND_128"      "IND_128_LI"   "IND_128_LS"   "IND_128_EE"  
# [37] "IND_128_CV"   "Ethnic"       "ANP_Cercana"  "dist_min"    
# [41] "c_ANPCercana" "c_distmin" 
#
# No nos interesan las columnas 1 a 7 ni ANP_cercana
## en sus dos versiones por ser categóricas. Nos interesan:
# "IND_002", "IND_824", "IND_047" , "IND_055", "IND_141", 
# "IND_128", "Ethnic", "dist_min", "c_distmin" 
#

cordf_GEOS=data.frame(CVE_FED=GEOS$ï..CVE_FED, IND_002=GEOS$IND_002, IND_824=GEOS$IND_824, 
                      IND_047=GEOS$IND_047, IND_055=GEOS$IND_055, IND_141=GEOS$IND_141,
                      IND_128=GEOS$IND_128, Ethnic=GEOS$Ethnic, c_distmin=GEOS$c_distmin)


#Función de redondeo de Jeromy Anglim.
redondear_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}


#Generamos un vector de nombres para nuestras columnas.
names(cordf_GEOS)

colnames(cordf_GEOS) <- c("Clave Federal","Porcentaje Población Estatal",
                        "Porcentaje Población 60+", "Densidad de Población (km²)", "Estimador Viviendas Particulares",
                        "Población Autoadscrita Indígena","Población Aut. Afrodescendiente", 
                        "Índice Étnico", "Aprox. Distancia Mínima")
View(cordf_GEOS)

#Rescalamos nuestras variables.
cordf_GEOS$`Porcentaje Población Estatal`=(cordf_GEOS$`Porcentaje Población Estatal`)/100
cordf_GEOS$`Porcentaje Población 60+`=(cordf_GEOS$`Porcentaje Población 60+`/100)
cordf_GEOS$`Población Autoadscrita Indígena`=(cordf_GEOS$`Población Autoadscrita Indígena`)/100
cordf_GEOS$`Población Aut. Afrodescendiente`=(cordf_GEOS$`Población Aut. Afrodescendiente`)/100
cordf_GEOS$`Aprox. Distancia Mínima` =EscalaMinMax(cordf_GEOS$`Aprox. Distancia Mínima`)


#Tabla de variables.
formattable(redondear_df(cordf_GEOS, 3), align =c("l","c","c","c","c", "c", "c", "c", "r"), list(
  `Clave Federal` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  `Porcentaje Población Estatal` = color_bar(Complementario),
  `Porcentaje Población 60+` = color_bar(Complementario),
  `Densidad de Población (km²)` = color_bar(Complementario),
  `Estimador Viviendas Particulares` = color_bar(Complementario),
  `Población Autoadscrita Indígena` = color_bar(VDemograficas),
  `Población Aut. Afrodescendiente` = color_bar(VDemograficas),
  `Índice Étnico` = color_bar(VDemograficas),
  `Aprox. Distancia Mínima` = color_bar(VDemograficas)))


#Anáisis de Correlación para la primera tabla 
png('Visualizaciones/Preprocesadas/Tabla 1 - Scatter de cor.png', pointsize=10, width=1800, height=1800, res=300)
pairs(cordf_GEOS) #Scatterplot
dev.off()

#Generamos un dataframe de correlación.
cor_GEOS=data.frame(cor(cordf_GEOS[,2:9])) 

colnames(cor_GEOS) <- c("Porcentaje Población Estatal",
                          "Porcentaje Población 60+", "Densidad de Población (km²)", 
                          "Estimador Viviendas Particulares",
                          "Población Autoadscrita Indígena","Población Aut. Afrodescendiente", 
                          "Índice Étnico", "Aprox. Distancia Mínima")


#Tabla de correlación.
formattable(redondear_df(cor_GEOS,4), align =c("l","c","c","c","c", "c", "c", "c", "r"), list(
  ` ` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  `Porcentaje Población Estatal` = color_tile(Complementario, VDemograficas),
  `Porcentaje Población 60+` = color_tile(Complementario, VDemograficas),
  `Densidad de Población (km²)` = color_tile(Complementario, VDemograficas),
  `Estimador Viviendas Particulares` = color_tile(Complementario, VDemograficas),
  `Población Autoadscrita Indígena` = color_tile(Complementario, VDemograficas),
  `Población Aut. Afrodescendiente` = color_tile(Complementario, VDemograficas),
  `Índice Étnico` = color_tile(Complementario, VDemograficas),
  `Aprox. Distancia Mínima` = color_tile(Complementario, VDemograficas)))

#Tabla de estadística descriptiva
formattable(redondear_df(
                stat.desc(redondear_df(
                           cordf_GEOS[,2:9],5)
                                  )[4:14,], 4), 
            align =c("l","c","c","c","c", "c", "c", "c", "r"),
            list( ` ` = formatter("span", style = ~ style(color = "grey",font.weight = "bold"))))


View(redondear_df(
  cordf_GEOS[,2:9],5))

#Análisis y Gráficas
plot(cordf_GEOS$`Índice Étnico`,cordf_GEOS$`Aprox. Distancia Mínima`)
plot(cordf_GEOS$`Población Autoadscrita Indígena`, cordf_GEOS$`Población Aut. Afrodescendiente`)

lm(cordf_GEOS$`Aprox. Distancia Mínima` ~ cordf_GEOS$`Índice Étnico`) #est. insignificante
lm(cordf_GEOS$`Índice Étnico` ~ cordf_GEOS$`Aprox. Distancia Mínima`) #ídem.

# 3. Variables estatales ----------------------------------------------

#Datos Geoestadísticos a nivel estatal
 GEOS_EST=read.csv("Bases de datos/Generadas/GEOS_EST.csv")
 GEOS_EST$ï..CVE_FED=GEOS_EST$CVE_FED
#Reducimos a variables de interés
 #GEOS_EST=data.frame(CVE_FED=GEOS_EST$ï..CVE_FED, IND_002=GEOS_EST$IND_002, 
 #          IND_824=GEOS_EST$IND_824, IND_047=GEOS_EST$IND_047, 
 #          IND_055=GEOS_EST$IND_055, IND_141=GEOS_EST$IND_141,
 #          IND_128=GEOS_EST$IND_128)

#Índice étnico
GEOS_EST$Ethnic= (100-GEOS_EST$IND_141-GEOS_EST$IND_128)/100


cordf_GEOS$CVE_ENT=GEOS$CVE_ENT

colnames(cor_GEOS) <- c("Porcentaje Población Estatal",
                        "Porcentaje Población 60+", "Densidad de Población (km²)", 
                        "Estimador Viviendas Particulares",
                        "Población Autoadscrita Indígena","Población Aut. Afrodescendiente", 
                        "Índice Étnico", "Aprox. Distancia Mínima")

#Generamos la distancia media por entidad.
V=tapply(cordf_GEOS$`Aprox. Distancia Mínima`, cordf_GEOS$CVE_ENT, mean)
mean(cordf_GEOS$`Aprox. Distancia Mínima`)
GEOS_EST$c_distmin=c("0.269648",V)

GEOS_EST=redondear_df(GEOS_EST, 3)
#Cargamos la base de datos sobre gasto y presupuesto.
Gasto_Amb=read.csv("Bases de datos/Generadas/Gasto Público ANPs.csv")
Gasto_Amb=Gasto_Amb[1:33,1:8]
rownames(Gasto_Amb)=Gasto_Amb$ï..CVE_FED

#Generamos la base de datos de geoestadística estatal y pres/gasto.

Gasto_Estatal=left_join(GEOS_EST, Gasto_Amb, by="ï..CVE_FED")

# 3.1 Proporción del presupuesto estatal en materia ambiental -------------
Prop_Pres= (Gasto_Estatal$PRES_AMB/Gasto_Estatal$PRES_INICIAL)
# 3.2 Proporción del gasto total anual en materia ambiental para  --------
Gasto_Estatal$Prop_Gasto=(Gasto_Estatal$Secret_MAmb/Gasto_Estatal$CuP_TOTAL)


#Reconstruimos y limpiamos.
names(Gasto_Estatal)
Gasto_Estatal=Gasto_Estatal%>% select(-c("X","ï..CVE_FED",  "Entidad"))
Gasto_Estatal=Gasto_Estatal%>% select(-c( "PRES_INICIAL", "PRES_AMB" ,    
                                         "CuP_TOTAL",     "Prop_Pres" ))


names(Gasto_Estatal)
colnames(Gasto_Estatal) <- c("Clave Federal", "Porcentaje Población Estatal",
                        "Porcentaje Población 60+", "Densidad de Población (km²)", 
                        "Estimador Viviendas Particulares",
                        "Población Autoadscrita Indígena","Población Aut. Afrodescendiente", 
                        "Índice Étnico", "Aprox. Distancia Mínima", "Gasto Secretaría M. Ambiente",
                        "Gasto Ajustado al Ingreso")
View(Gasto_Estatal)

#Rescalamos variables.
Gasto_Estatal$`Porcentaje Población Estatal`=Gasto_Estatal$`Porcentaje Población Estatal`/100
Gasto_Estatal$`Porcentaje Población 60+`=Gasto_Estatal$`Porcentaje Población 60+`/100
Gasto_Estatal$`Población Autoadscrita Indígena`=Gasto_Estatal$`Población Autoadscrita Indígena`/100
Gasto_Estatal$`Población Aut. Afrodescendiente`=Gasto_Estatal$`Población Aut. Afrodescendiente`/100

#Tabla de variables.
formattable(redondear_df(Gasto_Estatal[1:5,]%>% select(-`Gasto Secretaría M. Ambiente`),5), align =c("l","c","c","c","c", "c", "c", "c", "c", "r"), list(
  `Clave Federal` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  `Porcentaje Población Estatal` = color_bar(Complementario),
  `Porcentaje Población 60+` = color_bar(Complementario),
  `Densidad de Población (km²)` = color_bar(Complementario),
  `Estimador Viviendas Particulares` = color_bar(Complementario),
  `Población Autoadscrita Indígena` = color_bar(VDemograficas),
  `Población Aut. Afrodescendiente` = color_bar(VDemograficas),
  `Índice Étnico` = color_bar(VDemograficas),
  `Aprox. Distancia Mínima` = color_bar(VDemograficas),
  `Gasto Ajustado al Ingreso` = color_bar(VFiscales)
  ))

#Nos sigue dando NA la proporción...
Gasto_Estatal=remove_missing(Gasto_Estatal) #Quita datos para la CMDX 

#Por alguna razón, R interpreta las últimas dos columnas como argumentos
#"character" en lugar del tipo "numeric" necesario para las funciones
sapply(Gasto_Estatal, class)
Gasto_Estatal=as.data.frame(apply(Gasto_Estatal, 2, as.numeric))
sapply(Gasto_Estatal, class) 

cor_Gasto=(data.frame(cor(Gasto_Estatal%>% select(-`Clave Federal`))))

names(cor_Gasto)
colnames(cor_Gasto) <- c("Porcentaje Población Estatal",
                             "Porcentaje Población 60+", "Densidad de Población (km²)", 
                             "Estimador Viviendas Particulares",
                             "Población Autoadscrita Indígena","Población Aut. Afrodescendiente", 
                             "Índice Étnico", "Aprox. Distancia Mínima", "Gasto Secretaría M. Ambiente",
                             "Gasto Ajustado al Ingreso")
  
#Tabla de correlación.
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
  `Gasto Ajustado al Ingreso` = color_tile(Complementario, VDemograficas)
  ))


#Tabla de estadística descriptiva
formattable(stat.desc(Gasto_Estatal)[4:14,], 
  align =c("l","c","c","c","c", "c", "c", "c", "r"),
  list( ` ` = formatter("span", style = ~ style(color = "grey",font.weight = "bold"))))



#Correlación

png('Visualizaciones/Preprocesadas/Tabla II - Scatter de cor.png', pointsize=10, width=1800, height=1800, res=300)
pairs(Gasto_Estatal) #Scatterplot
dev.off()



fig <- plotly::plot_ly(Gasto_Estatal, x = ~Ethnic, y = ~c_distmin, z = ~Prop_Gasto, color = ~Prop_Gasto, colors = c('#BF382A', '#0C4B8E'))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Índice Étnic'),
                                   yaxis = list(title = 'Aproximación Dist. Mínima'),
                                   zaxis = list(title = 'Porporción de Gasto Ambiental')))


head(Controles)
Controles= read.csv("Bases de datos/Generadas/Controles.csv")
Controles=Controles[,1:3]
Controles$PIB_per_cápita=Controles$X
Controles=Controles%>% select(-c( "X" ))


colnames(Controles)= c("Clave Federal",
                                "Escolaridad Promedio 2016",
                                "PIB per Cápita Entidad")
head(Controles)
Controles=Controles[-c(10),]
View(Controles)
Gasto_Estatal$Escolaridad=Controles$`Escolaridad Promedio 2016`
Gasto_Estatal$PIB=Controles$`PIB per Cápita Entidad`

remove_missing(Gasto_Estatal)

# 4. Análisis Econométrico ------------------------------------------------
Reg_PGxSAMb=lm(`Gasto Ajustado al Ingreso`~ `Gasto Secretaría M. Ambiente`, data= Gasto_Estatal)
summary(Reg_PGxSAMb)

Reg_PG=lm(`Gasto Ajustado al Ingreso` ~`Aprox. Distancia Mínima` +
                        `Porcentaje Población 60+` + `Población Aut. Afrodescendiente` +
                        `Población Autoadscrita Indígena` + `Densidad de Población (km²)`, 
                         data=Gasto_Estatal)
summary(Reg_PG)

#Quitar la distancia elimina la significancia del coeficiente Afr.
Reg_PG=lm(`Gasto Ajustado al Ingreso` ~ `Porcentaje Población 60+` + `Población Aut. Afrodescendiente` +
            `Población Autoadscrita Indígena`, 
          data=Gasto_Estatal)
summary(Reg_PG)

head(Gasto_Estatal)

Reg_PG=lm(`Gasto Ajustado al Ingreso` ~`Aprox. Distancia Mínima` +
             `Población Aut. Afrodescendiente` +
            `Población Autoadscrita Indígena`+`Gasto Secretaría M. Ambiente`, 
          data=Gasto_Estatal)
summary(Reg_PG) #Intercepto est. significativo al eliminar `Porcentaje Población 60+` 


Reg_PG=lm(`Gasto Ajustado al Ingreso` ~`Aprox. Distancia Mínima` +
            `Porcentaje Población 60+` + `Población Aut. Afrodescendiente` +
            `Población Autoadscrita Indígena` + `Densidad de Población (km²)` + PIB +Escolaridad, 
          data=Gasto_Estatal)
summary(Reg_PG)

