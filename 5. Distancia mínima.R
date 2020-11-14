
# 5. Cálculo de la distancia mínima --------
#Calculamos la distancia mínima entre los polígonos de distrito electoral y 
#los polígonos de áreas naturales protegidas. 
###¡ALERTA! El código es muy pesado y no es necesario de ejectutar, pero
### está incluido para poder ser replicado. TIEMPO DE EJECUCIÓN:

# 1. Libraries -----------------------------------------------------------
setwd("C:/Users/socap/OneDrive/Documentos/R/Gasto P?blico en ?reas Naturales Protegidas/distance_example")
library(dplyr)
library(ggplot2)
library(gridExtra)

# 2. Ejemplo: Aguascalientes ("DAgs") ----------------------------------------------
# 2.1 Utilizando rgeos -----------------------------------------------
library(rgeos)

#Traemos los polígonos para Aguascalientes y las ANPs.
DAgs=st_read("Bases de datos/SHP/disfed2018/ags/DISTRITO.shp")
ANPs=read_sf("Bases de datos/SHP/SHAPE_ANPS/182ANP_Geo_ITRF08_Agosto_2020.shp")
st_crs(DAgs) # crs=WGS 84 / UTM zone 13N
st_crs(ANPs) # crs=MEXICO_ITRF_2008

## Transforming into 4326
ANPs_84=st_transform(ANPs, 4326)
ANPs_84$IND=1:182 #INDEX

DAgs_84=st_transform(DAgs, 4326)
DAgs$IND=101:103  #INDEX

ANPs_84p=as(ANPs_84,"Spatial")
DAgs_84p=as(DAgs_84,"Spatial")

# start=Sys.time()
# distance=apply(gDistance(DAgs_84p, ANPs_84p, byid=TRUE), 2, min)
# end=Sys.time()
# end-start           #9.377 minutes for 3x182 for ALL features!


# 3. Fuerza Bruta: Calculando la distancia para todo. ---------------------

###ORDEN!!!!! índice?

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



DIST= bind_rows(DAgs, DBC, DBCs,DCam,DCoa,
                   DCol, DCps,DCua, DCmx, DDur, DGua,
                   DGue, DHid, DJal,DEdo, DMic, DMor, DNay,
                   DNL, DOax,DPue, DQue, DQui,DSLP, DSin, 
                   DSon, DTab, DTam, DTla, DVer,
                   DYuc,DZac)





# 4.El orígen de GEOS$min_dist --------------------------------------------





