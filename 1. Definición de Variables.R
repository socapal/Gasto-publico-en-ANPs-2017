# Definición de Variables y Estadística Descriptiva --------------------

GEOS=read.csv("Bases de datos/Generadas/GEOS.csv") #Datos Geoestadísticos


# 1. Librerías ------------------------------------------------------------

library(pastecs)
library(ggplot2)
library(gridExtra)
library(dplyr)


#2. Variables Independientes-------------------------------------------------------------------------

#Ajustes sobre la base de datos geoestadística.

stat.desc(GEOS)

##Distritos por entidad
HD_T=GEOS %>%
    ggplot( aes(x=CVE_ENT)) +
    ggtitle("Distritos por entidad")+
    theme(plot.title = element_text(size=12))+
    geom_histogram(bins=32, fill='#206a5d', color='#f1f1e8', show.legend = FALSE)+
    theme_classic()

##Distritos con muestra insuficiente por entidad
HD_MI=subset(GEOS,GEOS$MI=="*") %>% 
      ggplot(aes(x=CVE_ENT)) +
      ggtitle("Distritos con muestra insuficiente por entidad") +
      theme(plot.title = element_text(size=12))+
      geom_histogram(bins=32, fill='#206a5d', color='#f1f1e8',show.legend = TRUE)+
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
GEOS$Ethnic= 100-GEOS$IND_141-GEOS$IND_128
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

cordf_GEOS=data.frame(GEOS$IND_002, GEOS$IND_824, GEOS$IND_047,
                      GEOS$IND_055, GEOS$IND_141, GEOS$IND_128, 
                      GEOS$Ethnic, GEOS$dist_min, GEOS$c_distmin)
pairs(cordf_GEOS)
cor_GEOS=data.frame(cor(cordf_GEOS))

# 2.2 Distancia -----------------------------------------------------------
dist_GEOS=remove_missing(GEOS)
#Analizamos dos tipos de distancia.
# Distancia euclediana de grandes círculos entre
# los polígonos de ambos tipos más cercanos (sf)
##Denotadas bajo las variables ANP_Cercana, dist_min.
#
# Distancia mínima euclediana desde la estimación del 
# centroide de los polígonos de ambos tipos.
##Denotada bajo c_ANPCercana y c_distmin.

##Para el primer caso, no contamos con observaciones para 
# BC, Sin y Son.

#En ambos casos escalamos la distancia para que su valor 
## máximo sea de uno.

EscalaMinMax <- function(x){
  return((x-min(x))/(max(x)-min(x)))}

dist_GEOS$EmM_distmin=EscalaMinMax(dist_GEOS$dist_min)
dist_GEOS$EmM_cdistmin=EscalaMinMax(dist_GEOS$c_distmin)

#únicamente valores extremos para el primer caso.
dist_GEOS %>%
  ggplot( aes(x=EmM_distmin)) +
  ggtitle("Distancia estandarizada") +
  theme(plot.title = element_text(size=12))+
  geom_histogram(bins=60, fill="#206a5d", color='#f1f1e8')+
  theme_classic()+
  geom_vline(aes(xintercept = mean(EmM_distmin)),col='black',size=0)

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

plot(dist_GEOS$Ethnic,dist_GEOS$EmM_dist)
plot(dist_GEOS$Ethnic,dist_GEOS$EmM_cdistmin)

grid.arrange(plot(dist_GEOS$Ethnic,dist_GEOS$EmM_dist),
             plot(dist_GEOS$Ethnic,dist_GEOS$EmM_cdistmin), ncol=2, nrow=1)

cor(dist_GEOS$Ethnic,dist_GEOS$EmM_dist)  #Corr -0.13
cor(dist_GEOS$Ethnic,dist_GEOS$EmM_cdist) #Corr -0.02

lm(dist_GEOS$EmM_dist ~ dist_GEOS$Ethnic) #est. insignificante
lm(dist_GEOS$EmM_cdistmin ~ dist_GEOS$Ethnic) #ídem.

# 3. Variables dependientes ----------------------------------------------


# 3.1 Proporción del presupuesto estatal en materia ambiental -------------





# 3.2 Proporción del gasto total anual en materia ambiental para c --------




