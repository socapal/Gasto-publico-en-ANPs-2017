# 4. Estadísticas a Geoescala para regiones de las anp --------
setwd("C:/Users/socap/OneDrive/Documentos/GitHub/Gasto-publico-en-ANPs-2017")
library(sf)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(viridis)
library(rgdal)
library(stringr)

VDemograficas= "#C05555"
VANPs="#59886B"
VFiscales="#FFC85C"
VComplementario="#FFF8C1"

#Cargamos bases de datos, 
##ANPs tiene los polígonos de las Áreas Naturales Protegidas (ANPs)
##Reg contiene los polígonos de las nueve regiones de ANPs
##GEOS almacena la información geoestadística de los distritos electorales
##D{Ent} son los polígonos electorales para cada entidad.

#SHPs de Áreas Naturales Protegidas
ANPs= st_read("Bases de datos/SHP/SHAPE_ANPS/182ANP_Geo_ITRF08_Agosto_2020.shp")
Reg= st_read("Bases de datos/SHP/Regionalizacion_22052017/Regionalización_22052017.shp")

#Geoestadística
GEOS=read.csv("Bases de datos/Generadas/GEOS.CSV")
names(GEOS)
#Recodificamos las variables de la base de datos GEOS.
GEOS=data.frame(CVE_ENT=GEOS$Entidad, distrito=GEOS$distrito,
                CVE_FED=GEOS$Clave.Federal,
                IND_002=GEOS$Porcentaje.Población.Estatal,
                IND_824=GEOS$Porcentaje.Población.60.,
                IND_047=GEOS$`Densidad.de.Población..km².`,
                IND_055=GEOS$Estimador.Viviendas.Particulares, 
                IND_141=GEOS$Población.Autoadscrita.Indígena,
                IND_128=GEOS$Población.Aut..Afrodescendiente,
                Ethnic=GEOS$Índice.Étnico, 
                c_distmin=GEOS$Aprox..Distancia.Mínima,
                c_ANPCercana=GEOS$ANP.Cercana)
names(GEOS)



#SHP Distritos
DBC= read_sf("Bases de datos/SHP/disfed2018/bc/DISTRITO.shp")
DBCs=read_sf("Bases de datos/SHP/disfed2018/bcs/DISTRITO.shp")
DSin= read_sf("Bases de datos/SHP/disfed2018/sin/DISTRITO.shp")
DSon=read_sf("Bases de datos/SHP/disfed2018/son/DISTRITO.shp")
DCua=read_sf("Bases de datos/SHP/disfed2018/cua/DISTRITO.shp")
DDur=read_sf("Bases de datos/SHP/disfed2018/dgo/DISTRITO.shp")
DZac=read_sf("Bases de datos/SHP/disfed2018/zac/DISTRITO.shp")
DNL=read_sf("Bases de datos/SHP/disfed2018/nl/DISTRITO.shp")
DCoa=read_sf("Bases de datos/SHP/disfed2018/coa/DISTRITO.shp")
DTam=read_sf("Bases de datos/SHP/disfed2018/tam/DISTRITO.shp")
DSLP=read_sf("Bases de datos/SHP/disfed2018/san/DISTRITO.shp")
DVer=read_sf("Bases de datos/SHP/disfed2018/ver/DISTRITO.shp")
DPue=read_sf("Bases de datos/SHP/disfed2018/pue/DISTRITO.shp")
DCam=read_sf("Bases de datos/SHP/disfed2018/cam/DISTRITO.shp")
DTab=read_sf("Bases de datos/SHP/disfed2018/tab/DISTRITO.shp")
DHid=read_sf("Bases de datos/SHP/disfed2018/hgo/DISTRITO.shp")
DQui=read_sf("Bases de datos/SHP/disfed2018/qui/DISTRITO.shp")
DYuc=read_sf("Bases de datos/SHP/disfed2018/yuc/DISTRITO.shp")
DCps=read_sf("Bases de datos/SHP/disfed2018/cps/DISTRITO.shp")
DOax=read_sf("Bases de datos/SHP/disfed2018/oax/DISTRITO.shp")
DCmx=read_sf("Bases de datos/SHP/disfed2018/df/DISTRITO.shp")
DEdo=read_sf("Bases de datos/SHP/disfed2018/mex/DISTRITO.shp")
DQue=read_sf("Bases de datos/SHP/disfed2018/que/DISTRITO.shp")
DGue=read_sf("Bases de datos/SHP/disfed2018/gue/DISTRITO.shp")
DMor=read_sf("Bases de datos/SHP/disfed2018/mor/DISTRITO.shp")
DMic=read_sf("Bases de datos/SHP/disfed2018/mic/DISTRITO.shp")
DTla=read_sf("Bases de datos/SHP/disfed2018/tla/DISTRITO.shp")
DGua=read_sf("Bases de datos/SHP/disfed2018/gua/DISTRITO.shp")
DJal=read_sf("Bases de datos/SHP/disfed2018/jal/DISTRITO.shp")
DCol=read_sf("Bases de datos/SHP/disfed2018/col/DISTRITO.shp")
DNay=read_sf("Bases de datos/SHP/disfed2018/nay/DISTRITO.shp")
DAgs=read_sf("Bases de datos/SHP/disfed2018/ags/DISTRITO.shp")


#Graficamos la var. de diversidad étnica
GEOS %>%
  ggplot( aes(x=Ethnic)) +
  geom_histogram(bins=10, fill='skyblue', color='#69b3a2')


#REGIONES.

# 4.1 | Región Uno - Península de Baja California y Pacífico Norte ----------

# Únicamente contiene distritos de Baja California y Baja California Sur.

###Generamos un subset para cada entidad federativa o estado.
GEOSBC=subset(GEOS, GEOS$CVE_ENT==2 & GEOS$distrito>0)
GEOSBCs=subset(GEOS, GEOS$CVE_ENT==3 & GEOS$distrito>0)

###Unimos la información geoestadística con los polígonos de distritación.
DISTBC=merge(DBC, GEOSBC, by="distrito")
DISTBCs=merge(DBCs, GEOSBCs, by="distrito")


###Obtenemos la geoestadística distrital para el distrito uno
### Península de Baja California y Pacífico Norte
data=subset(ANPs, ANPs$REGION=="Península de Baja California y Pacífico Norte")
ANPsi=st_centroid(data)


GDREG1= ggplot()+
  geom_sf(data=subset(Reg, 
                      Reg$REGION=="Península de Baja California y Pacífico Norte"), 
          fill="#88B5D7", size=0, color = "White") + 
  geom_sf(data=DISTBC, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTBCs, aes(fill=Ethnic), size=0, color = "White")+
  scale_fill_continuous(low =VDemograficas, high=VComplementario, guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  geom_sf(data=ANPsi, color="#0F4B78")+
  labs(title="Distribución étnica por región", 
       subtitle="Península de Baja California y Pacífico Norte",
       caption="Datos: CONANP, INE-INEGI | @sebasdepapel")+
  
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 15, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 10, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=10, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.7, 0.09))+
  coord_sf(datum=NA)
GDREG1


# 4.2 | Región Dos - Noroeste y Alto Golfo de California -------------------

#Únicamente contiene distritos de Sonora y Sinaloa.
###Generamos un subset para cada entidad federativa o estado.
GEOSSin=subset(GEOS, GEOS$CVE_ENT==25& GEOS$distrito>0)
GEOSSon=subset(GEOS, GEOS$CVE_ENT==26 & GEOS$distrito>0)

###Unimos la información geoestadística con los polígonos de distritación.
DISTSin=merge(DSin, GEOSSin, by="distrito")
DISTSon=merge(DSon, GEOSSon, by="distrito")


###Obtenemos la geoestadística distrital para la región dos
### Noroeste y Alto Golfo de California
data=subset(ANPs, ANPs$REGION=="Noroeste y Alto Golfo de California")
ANPsi=st_centroid(data)


GDREG2= ggplot()+
  geom_sf(data=subset(Reg, 
                      Reg$REGION=="Noroeste y Alto Golfo de California"), 
          fill="#88B5D7", size=0, color = "White") + 
  geom_sf(data=DISTSin, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTSon, aes(fill=Ethnic), size=0, color = "White")+
  scale_fill_continuous(low ="#ffeda0", high="#f03b20", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  geom_sf(data=ANPsi, color="#0F4B78")+
  labs(title="Distribución étnica por región", 
       subtitle="Noroeste y Alto Golfo de California",
       caption="Datos: CONANP, INE-INEGI | @sebasdepapel")+
  
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 15, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 10, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=10, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.3, 0.09))+
  coord_sf(datum=NA)
GDREG2 


# 4.3 | Región Tres - Norte y Sierra Madre Occidental -----------------------

#Únicamente contiene distritos de Chihuahua, Durango y Zacatecas.
##El distrito tres de Zacatecas abarca dos regiones, su distrito 4
##no está en la región.

###Generamos un subset para cada entidad federativa o estado.
GEOSCua=subset(GEOS, GEOS$CVE_ENT==8 & GEOS$distrito>0)
GEOSDur=subset(GEOS, GEOS$CVE_ENT==10 & GEOS$distrito>0)
GEOSZac=subset(GEOS, GEOS$CVE_ENT==32 & GEOS$distrito>0)

###Unimos la informaci?n geoestad?stica con los pol. de distritaci?n.
DISTCua=merge(DCua, GEOSCua, by="distrito")
DISTDur=merge(DDur, GEOSDur, by="distrito")
DISTZac=merge(DZac, GEOSZac, by="distrito")


###Obtenemos la geoestadística distrital para el distrito tres
### Norte y Sierra Madre Occidental
data=subset(ANPs, ANPs$REGION=="Norte y Sierra Madre Occidental")
ANPsi=st_centroid(data)


GDREG3= ggplot()+
  geom_sf(data=subset(Reg, 
                      Reg$REGION=="Norte y Sierra Madre Occidental"), 
          fill="#88B5D7", size=0, color = "White") + 
  geom_sf(data=DISTCua, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTDur, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=subset(DISTZac,distrito<4), aes(fill=Ethnic), size=0, color = "White")+
  scale_fill_continuous(low ="#ffeda0", high="#f03b20", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  geom_sf(data=ANPsi, color="#0F4B78")+
  labs(title="Distribución étnica por región", 
       subtitle="Norte y Sierra Madre Occidental",
       caption="Datos: CONANP, INE-INEGI | @sebasdepapel")+
  
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 15, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 10, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=10, color = "#4e4d47", margin = margin(b = 0.25, r=-99, unit = "cm") ),
    
    legend.position = c(0.26, 0.085))+
  coord_sf(datum=NA)
GDREG3 

#
##Contiene distritos de Chihuahua, Coahuila, Nuevo Le?n,
##San Luis Potos?, Zacatecas, Tamaulipas, Durango
###Los distritos de Zacatecas son el 3 y 4.
###Se excluyen los distritos 4, 7 y 8 de Tamaulipas.
###Chihuahua tiene ?nicamente el distrito 5, sobrespuesto.
###Durango ?nicamente tiene el distrito 2, sobrepuesto.

# 4.4 | Región Cuatro - Noreste y Sierra Madre Oriental ---------------------

###Generamos un subset para cada entidad federativa o estado.
GEOSNL=subset(GEOS, GEOS$CVE_ENT==19 & GEOS$distrito>0)
GEOSCoa=subset(GEOS, GEOS$CVE_ENT==5 & GEOS$distrito>0)
GEOSSLP=subset(GEOS, GEOS$CVE_ENT==24 & GEOS$distrito>0)
GEOSTam=subset(GEOS, GEOS$CVE_ENT==28 & GEOS$distrito>0)
###Unimos la informaci?n geoestad?stica con los pol. de distritaci?n.
DISTNL=merge(DNL, GEOSNL, by="distrito")
DISTCoa=merge(DCoa, GEOSCoa, by="distrito")
DISTSLP=merge(DSLP, GEOSSLP, by="distrito")
DISTTam=merge(DTam, GEOSTam, by="distrito")


###Obtenemos la geoestadística distrital para el distrito tres
### Norte y Sierra Madre Occidental
data=subset(ANPs, ANPs$REGION=="Noreste y Sierra Madre Oriental")
ANPsi=st_centroid(data)


GDREG4= ggplot()+
  geom_sf(data=subset(Reg, 
                      Reg$REGION=="Noreste y Sierra Madre Oriental"), 
          fill="#88B5D7", size=0, color = "White") + 
  geom_sf(data=DISTNL, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTSLP, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTCoa, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=subset(DISTCua, distrito==5), aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=subset(DISTZac, distrito>2), aes(fill=Ethnic), size=0, color ="White")+
  geom_sf(data=subset(DISTDur, distrito==2), aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=filter(DISTTam, (distrito %in% c(1,2,3,5,6,9))), aes(fill=Ethnic), size=0, color = "White")+
  scale_fill_continuous(low ="#ffeda0", high="#f03b20", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  geom_sf(data=ANPsi, color="#0F4B78")+
  labs(title="Distribución étnica por región", 
       subtitle="Noreste y Sierra Madre Oriental",
       caption="Datos: CONANP, INE-INEGI | @sebasdepapel")+
  
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 15, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 10, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=10, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.3, 0.06))+
  coord_sf(datum=NA)

GDREG4


# 4.5 | Región Cinco - Planicie Costera y Golfo de México -------------------

#Contiene distritos de Veracruz, Puebla, Campeche,Tabasco, Tamaulipas, e Hidalgo.
###Sólo incluye el distrito dos de Campeche, aunque 
###una parte minúscula del primero está en la Región.
###Se incluyen los distritos 3, 4, 7 y 8 de Tamaulipas (3 y 4 sobrepuestos).

###Generamos un subset para cada entidad federativa o estado.
GEOSVer=subset(GEOS, GEOS$CVE_ENT==30 & GEOS$distrito>0)
GEOSPue=subset(GEOS, GEOS$CVE_ENT==21 & GEOS$distrito>0)
GEOSCam=subset(GEOS, GEOS$CVE_ENT==4 & GEOS$distrito>0)
GEOSTab=subset(GEOS, GEOS$CVE_ENT==27 & GEOS$distrito>0)
GEOSHid=subset(GEOS, GEOS$CVE_ENT==13 & GEOS$distrito>0)


###Unimos la información geoestadística con los pol. de distritación.
DISTVer=merge(DVer, GEOSVer, by="distrito")
DISTPue=merge(DPue, GEOSPue, by="distrito")
DISTCam=merge(DCam, GEOSCam, by="distrito")
DISTTab=merge(DTab, GEOSTab, by="distrito")
DISTHid=merge(DHid, GEOSHid, by="distrito")

###Obtenemos la geoestadística distrital para la región cinco
### Planicie Costera y Golfo de México
data=subset(ANPs, ANPs$REGION=="Planicie Costera y Golfo de México")
ANPsi=st_centroid(data)


GDREG5= ggplot()+
  geom_sf(data=subset(Reg, 
                      Reg$REGION=="Planicie Costera y Golfo de México"), 
          fill="#88B5D7", size=0, color = "White") + 
  geom_sf(data=DISTVer, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTTab, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=subset(DISTHid, DISTHid$distrito==4), aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=filter(DISTPue, (distrito%in%c(1,2,3,8))), aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=filter(DISTTam, (distrito%in%c(3,4,6,7,8))), aes(fill=Ethnic), size=0, color ="White")+
  geom_sf(data=subset(DISTCam, DISTCam$distrito==2), aes(fill=Ethnic), size=0, color = "White")+
  scale_fill_continuous(low ="#ffeda0", high="#f03b20", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  geom_sf(data=ANPsi, color="#0F4B78")+
  labs(title="Distribución étnica por región", 
       subtitle="Planicie Costera y Golfo de México",
       caption="Datos: CONANP, INE-INEGI | @sebasdepapel")+
  
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 15, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 10, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=10, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    legend.position = c(0.09, 0.08))+
  coord_sf(datum=NA)
GDREG5


# 4.6 | Región Seis - Península de Yucatán y Caribe Mexicano --------------

##Contiene distritos de Quintana Roo, Yucatán y Campeche,
###Recordamos que el distrito 2 de Campeche se sobrepone con la R5.

###Generamos un subset para cada entidad federativa o estado.
GEOSQui=subset(GEOS, GEOS$CVE_ENT==23 & GEOS$distrito>0)
GEOSYuc=subset(GEOS, GEOS$CVE_ENT==30 & GEOS$distrito>0)

###Unimos la informaci+on geoestadística con los pol. de distritación.
DISTQui=merge(DQui, GEOSQui, by="distrito")
DISTYuc=merge(DYuc, GEOSYuc, by="distrito")

###Obtenemos la geoestadística distrital para la región cinco
### Planicie Costera y Golfo de M?xico
data=subset(ANPs, ANPs$REGION=="Península de Yucatán y Caribe Mexicano")
ANPsi=st_centroid(data)


GDREG6= ggplot()+
  geom_sf(data=subset(Reg, 
                      Reg$REGION=="Península de Yucatán y Caribe Mexicano"), 
          fill="#88B5D7", size=0, color = "White") + 
  geom_sf(data=DISTQui, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTYuc, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTCam, aes(fill=Ethnic), size=0, color = "White")+
  scale_fill_continuous(low ="#ffeda0", high="#f03b20", guide = guide_legend( keyheight = unit(4, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=4, reverse = TRUE) ) +
  geom_sf(data=ANPsi, color="#0F4B78")+
  labs(title="Distribución étnica por región", 
       subtitle="Península de Yucatán y Caribe Mexicano",
       caption="Datos: CONANP, INE-INEGI | @sebasdepapel")+
  
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 15, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 10, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=10, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.10,0.70))+
  coord_sf(datum=NA)
GDREG6


# 4.7 | Región Siete - Frontera Sur, Istmo y Pacífico Sur -----------------


###Generamos un subset para cada entidad federativa o estado.
GEOSCps=subset(GEOS, GEOS$CVE_ENT==7 & GEOS$distrito>0)
GEOSOax=subset(GEOS, GEOS$CVE_ENT==20 & GEOS$distrito>0)

###Unimos la información geoestadística con los polígonos de distritación.
DISTCps=merge(DCps, GEOSCps, by="distrito")
DISTOax=merge(DOax, GEOSOax, by="distrito")

###Obtenemos la geoestadística distrital para el distrito siete
### Frontera Sur, Istmo y Pacífico Sur
data=subset(ANPs, ANPs$REGION=="Frontera Sur, Istmo y Pacífico Sur")
ANPsi=st_centroid(data)


GDREG7= ggplot()+
  geom_sf(data=subset(Reg, 
                      Reg$REGION=="Frontera Sur, Istmo y Pacífico Sur"), 
          fill="#88B5D7", size=0, color = "White") + 
  geom_sf(data=DISTOax, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTCps, aes(fill=Ethnic), size=0, color = "White")+
  scale_fill_continuous(low ="#ffeda0", high="#f03b20", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  geom_sf(data=ANPsi, color="#0F4B78")+
  labs(title="Distribución étnica por región", 
       subtitle="Frontera Sur, Istmo y Pacífico Sur",
       caption="Datos: CONANP, INE-INEGI | @sebasdepapel")+
  
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 15, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 10, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=10, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.8, 0.08))+
  coord_sf(datum=NA)
GDREG7



# 4.8 | Región Ocho - Centro y Eje Neovolcánico ---------------------------


##Contiene distritos de Ciudad de México, Quéretaro,
##Edo. México, Morelos, Hidalgo, Michoacán
##Guerrero y finalmente Tlaxcala.
###Se sobreponen los distritos 3 y 6 de Michoacán.
###El distrito 1 Guanajuato se sobrepone.
###También están sobrepuestos los distritos 2,3 y 8 de Oaxaca.
###Sólo introducimos los distritos 1,2,3 de Puebla.

###Generamos un subset para cada entidad federativa o estado.
GEOSCmx=subset(GEOS, GEOS$CVE_ENT==9 & GEOS$distrito>0)
GEOSEdo=subset(GEOS, GEOS$CVE_ENT==15 & GEOS$distrito>0)
GEOSQue=subset(GEOS, GEOS$CVE_ENT==22 & GEOS$distrito>0)
GEOSGue=subset(GEOS, GEOS$CVE_ENT==12 & GEOS$distrito>0)
GEOSMor=subset(GEOS, GEOS$CVE_ENT==17 & GEOS$distrito>0)
GEOSMic=subset(GEOS, GEOS$CVE_ENT==16 & GEOS$distrito>0)
GEOSTla=subset(GEOS, GEOS$CVE_ENT==29 & GEOS$distrito>0)
GEOSGua=subset(GEOS, GEOS$CVE_ENT==11 & GEOS$distrito>0)


###Unimos la información geoestadística con los pol. de distritación.
DISTCmx=merge(DCmx, GEOSCmx, by="distrito")
DISTEdo=merge(DEdo, GEOSEdo, by="distrito")
DISTQue=merge(DQue, GEOSQue, by="distrito")
DISTMor=merge(DMor, GEOSMor, by="distrito")
DISTMic=merge(DMic, GEOSMic, by="distrito")
DISTTla=merge(DTla, GEOSTla, by="distrito")
DISTGua=merge(DGua, GEOSGua, by="distrito")
DISTGue=merge(DGue, GEOSGue, by="distrito")

###Obtenemos la geoestadística distrital para la región nueve
### Centro y Eje Neovolcánico
data=subset(ANPs, ANPs$REGION=="Centro y Eje Neovolcánico")
ANPsi=st_centroid(data)


GDREG8= ggplot()+
  geom_sf(data=subset(Reg, 
                      Reg$REGION=="Centro y Eje Neovolcánico"), 
          fill="#88B5D7", size=0, color = "White") + 
  geom_sf(data=DISTCmx, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTEdo, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTQue, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTGue, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTMor, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTHid, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTTla, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=filter(DISTOax, (distrito%in%c(2,3))), aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=filter(DISTPue, !(distrito%in%c(1,2,3))), aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=subset(DISTGua, DISTGua$distrito==1), aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=filter(DISTMic, (distrito%in%c(3,6))), aes(fill=Ethnic), size=0, color = "White")+
  scale_fill_continuous(low ="#ffeda0", high="#f03b20", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(9, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  geom_sf(data=ANPsi, color="#0F4B78")+
  labs(title="Distribución étnica por región", 
       subtitle="Centro y Eje Neovolcánico",
       caption="Datos: CONANP, INE-INEGI | @sebasdepapel")+
  
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 15, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 10, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=10, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.22, 0.015))+
  coord_sf(datum=NA)
GDREG8

# 4.9 | Región Nueve - Occidente y Pacífico Centro ------------------------


##Contiene distritos de Colima, Jalisco, Nayarit, 
##Michoacán, Guerrero, Aguascalientes y Zacatecas.


###Generamos un subset para cada entidad federativa o estado.
GEOSJal=subset(GEOS, GEOS$CVE_ENT==14 & GEOS$distrito>0)
GEOSCol=subset(GEOS, GEOS$CVE_ENT==6 & GEOS$distrito>0)
GEOSNay=subset(GEOS, GEOS$CVE_ENT==18 & GEOS$distrito>0)
GEOSAgs=subset(GEOS, GEOS$CVE_ENT==1 & GEOS$distrito>0)

###Unimos la informaci?n geoestadística con los pol. de distritación.
DISTJal=merge(DJal, GEOSJal, by="distrito")
DISTCol=merge(DCol, GEOSCol, by="distrito")
DISTNay=merge(DNay, GEOSNay, by="distrito")
DISTAgs=merge(DAgs, GEOSAgs, by="distrito")

###Obtenemos la geoestad?ística distrital para la región nueve 
### Occidente y Pacífico Centro
data=subset(ANPs, ANPs$REGION=="Occidente y Pacífico Centro")
ANPsi=st_centroid(data)


GDREG9= ggplot()+
  geom_sf(data=subset(Reg, 
                      Reg$REGION=="Occidente y Pacífico Centro"), 
          fill="#88B5D7", size=0, color = "White") + 
  geom_sf(data=DISTAgs, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTJal, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTCol, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTNay, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=subset(DISTZac, DISTZac$distrito==2), aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTMic, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTGua, aes(fill=Ethnic), size=0, color = "White")+
  scale_fill_continuous(low ="#ffeda0", high="#f03b20", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  geom_sf(data=ANPsi, color="#0F4B78")+
  labs(title="Distribución étnica por región", 
       subtitle=" Occidente y Pacífico Centro",
       caption="Datos: CONANP, INE-INEGI | @sebasdepapel")+
  
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 15, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 10, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=10, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.3, 0.04))+
  coord_sf(datum=NA)
GDREG9



# 4.10 | Mapa de Geoestadísticas Distritales a Escala Federal -------------

ANPsc=st_centroid(ANPs)

GDF=ggplot()+
  geom_sf(data=Reg, 
          fill="#88B5D7", size=0, color = "White") + 
  geom_sf(data=DISTAgs, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTBC, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTBCs, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTCam, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTCmx, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTCoa, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTCol, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTCps, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTCua, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTDur, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTEdo, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTGua, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTGue, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTHid, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTJal, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTMic, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTMor, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTNay, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTNL, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTOax, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTPue, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTQue, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTQui, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTSin, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTSLP, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTSon, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTTab, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTTam, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTTla, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTVer, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTYuc, aes(fill=Ethnic), size=0, color = "White")+
  geom_sf(data=DISTZac, aes(fill=Ethnic), size=0, color = "White")+
  scale_fill_continuous(low ="#ffeda0", high="#f03b20", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  geom_sf(data=ANPsc, color="#0F4B78")+
  labs(title="Distribución étnica y ANPs", 
       caption="Datos: CONANP, INE-INEGI | @sebasdepapel")+
  
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 15, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.caption = element_text( size=10, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.3, 0.03))+
  coord_sf(datum=NA)

GDF



# 4.11 Impresión de Objetos -----------------------------------------------

png('Visualizaciones/Preprocesadas/GDREG4.png', pointsize=10, width=1800, height=1800, res=300)
GDF
dev.off()

png('Visualizaciones/Preprocesadas/GDREG4.png', pointsize=10, width=2200, height=1800, res=300)
GDREG5
dev.off()

png('Visualizaciones/Preprocesadas/GDREG4.png', pointsize=10, width=1800, height=1900, res=300)
GDREG9
dev.off()

# 5. Distancia mínima de distritos electorales a ANPs más cercana ---------








# 6. Bases de datos -------------------------------------------------------
library(formattable)
#Rescalamos la distancia mínima.
EscalaMinMax <- function(x){
  return((x-min(x))/(max(x)-min(x)))}

load("cordf_GEOS.RData")
#Aplicamos la función.
cordf_GEOS$c_distmin=EscalaMinMax(cordf_GEOS$c_distmin)

Tabla_1=cordf_GEOS[1:5,] #Reducimos la tabla
#Función de redondeo de Jeromy Anglim.
redondear_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}


formattable(redondear_df(Tabla_1, 3), align =c("l","c","c","c","c", "c", "c", "c", "r"), list(
  `CVE_ENT` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  `IND_002` = color_bar(Complementario),
  `IND_824` = color_bar(Complementario),
  `IND_047` = color_bar(Complementario),
  `IND_055` = color_bar(Complementario),
  `IND_141` = color_bar(VDemograficas),
  `IND_128` = color_bar(VDemograficas),
  `Ethnic` = color_bar(VDemograficas),
  `c_distmin` = color_bar(VDemograficas)
  ))

