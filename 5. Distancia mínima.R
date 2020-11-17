# 5. Cálculo de la distancia mínima --------

#Calculamos la distancia mínima entre los polígonos de distrito electoral y 
#los polígonos de áreas naturales protegidas. 
###¡ALERTA! El código es muy pesado y no es necesario de ejecutar, pero
### está incluido para poder ser replicado. TIEMPO DE EJECUCIÓN:

# 1. Libraries -----------------------------------------------------------
setwd("C:/Users/socap/OneDrive/Documentos/GitHub/Gasto-publico-en-ANPs-2017/")
library(dplyr)
library(ggplot2)
library(gridExtra)
library(sf)

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

# Orden de entidades, para anexar fácilmente a la base GEOS.

#               (DAgs, DBC, DBCs,DCam,DCoa,
#                DCol, DCps,DCua, DCmx, DDur, DGua,
#                DGue, DHid, DJal,DEdo, DMic, DMor, DNay,
#                DNL, DOax,DPue, DQue, DQui,DSLP, DSin, 
#                DSon, DTab, DTam, DTla, DVer,
#                DYuc,DZac)

#Cargamos el archivo shape para las Áreas Naturales Protegidas.
ANPs=read_sf("Bases de datos/SHP/SHAPE_ANPS/182ANP_Geo_ITRF08_Agosto_2020.shp")

## Transformamos a WGS 84 y en un objeto espacial.
ANPs_84=st_transform(ANPs, 4326)
ANPs_84$IND=1:182 #INDEX
#ANPs_84=as(ANPs_84,"Spatial")

#sp_ANPs_84=SpatialPoints(ANPs_84)

# 3.1 Primeras 48 distancias -----------------------------------------------

#   DAgs, DBC, DBCs, DCam, DCoa, DCol. 
#   1.73 segundos en rgeos

DAgs=st_read("Bases de datos/SHP/disfed2018/ags/DISTRITO.shp")
DBC= read_sf("Bases de datos/SHP/disfed2018/bc/DISTRITO.shp")
DBCs=read_sf("Bases de datos/SHP/disfed2018/bcs/DISTRITO.shp")
DCam=read_sf("Bases de datos/SHP/disfed2018/cam/DISTRITO.shp")
DCoa=read_sf("Bases de datos/SHP/disfed2018/coa/DISTRITO.shp")
DCol=read_sf("Bases de datos/SHP/disfed2018/col/DISTRITO.shp")

DAgs_84=st_transform(DAgs, 4326)
DBC_84=st_transform(DBC, 4326)
DBCs_84=st_transform(DBCs, 4326)
DCam_84=st_transform(DCam, 4326)
DCoa_84=st_transform(DCoa, 4326)
DCol_84=st_transform(DCol, 4326)


# 3.1.1 Utilizando simple features (sf) -----------------------------------

inicio.compilación = Sys.time()
#DAgs, DBC, DBCs, DCam, DCoa, DCol.

#Closest polygon      #5.57 min

cercana_Ags=st_nearest_feature(DAgs_84, ANPs_84)
cercana_BC=st_nearest_feature(DBC_84, ANPs_84)
cercana_BCs=st_nearest_feature(DBCs_84, ANPs_84)
cercana_Cam=st_nearest_feature(DCam_84, ANPs_84)
cercana_Coa=st_nearest_feature(DCoa_84, ANPs_84)
cercana_Col=st_nearest_feature(DCol_84, ANPs_84)

#Generamos un índice utilizando la cercanía. #0.18 seg.

cercana_Ags=ANPs_84%>%slice(cercana_Ags) 
cercana_BC=ANPs_84%>%slice(cercana_BC)
cercana_BCs=ANPs_84%>%slice(cercana_BCs)
cercana_Cam=ANPs_84%>%slice(cercana_Cam)
cercana_Coa=ANPs_84%>%slice(cercana_Coa)
cercana_Col=ANPs_84%>%slice(cercana_Col)

#Distance to closest polygon   #
start=Sys.time()
distancia_Ags=st_distance(DAgs_84, cercana_Ags, by_element=TRUE)
distancia_BC=st_distance(DBC_84, cercana_BC, by_element=TRUE)
distancia_BCs=st_distance(DBCs_84, cercana_BCs, by_element=TRUE)
distancia_Cam=st_distance(DCam_84, cercana_Cam, by_element=TRUE)
distancia_Coa=st_distance(DCoa_84, cercana_Coa, by_element=TRUE)
distancia_Col=st_distance(DCol_84, cercana_Col, by_element=TRUE)
end=Sys.time()
end-start


#Generamos un objeto que concatene los resultados de las operaciones.
cercana_1=c(cercana_Ags,cercana_BC,cercana_BCs,cercana_Cam,cercana_Coa,cercana_Col)
dist_min_1=c(distancia_Ags,distancia_BC,distancia_BCs,distancia_Cam,distancia_Coa,distancia_Col)


#Un dataframe para los distritos
DIST_1=rbind(DAgs_84, DBC_84, DBCs_84,DCam_84,DCoa_84,DCol_84)

dist_1_sf=data.frame(CVE_FED=(DIST_1$entidad *100+DIST_1$distrito),  
                     entidad=DIST_1$entidad, distrito=DIST_1$distrito, ANP_Cercana=cercana_1,
                     dist_min=dist_min_1)

final.compilación = Sys.time()
final.compilación - start    #19.554 mins (24x182 for CLOSEST features)

# 3.1.2 Utilizando rgeos --------------------------------------------------

DAgs_84=as(DAgs_84,"Spatial")
DBC_84=as(DBC_84,"Spatial")
DBCs_84=as(DBCs_84,"Spatial")
DCam_84=as(DCam_84,"Spatial")
DCoa_84=as(DCoa_84, "Spatial")
DCol_84=as(DCol_84,"Spatial")

sp_DAgs_84=SpatialPoints(DAgs_84)
sp_DBC_84=SpatialPoints(DBC_84)
sp_DBCs_84=SpatialPoints(DBCs_84)
sp_DCam_84=SpatialPoints(DCam_84)
sp_DCoa_84=SpatialPoints(DCoa_84)
sp_DCol_84=SpatialPoints(DCol_84)

#Calcuamos la distancia mínima entre cada uno y su polígono.

inicio_compilación=Sys.time()
#Ags
start=Sys.time()
distance_Ags=apply(gDistance(sp_DAgs_84, sp_ANPs_84, byid=TRUE), 2, min)
end=Sys.time()
end-start           

#BC
start=Sys.time()
distance_BC=apply(gDistance(sp_DBC_84, sp_ANPs_84, byid=TRUE), 2, min)
end=Sys.time()
end-start 
#BCs
start=Sys.time()
distance_BCs=apply(gDistance(sp_DBCs_84, sp_ANPs_84, byid=TRUE), 2, min)
end=Sys.time()
end-start 
#Cam
start=Sys.time()
distance_Cam=apply(gDistance(sp_DCam_84, sp_ANPs_84, byid=TRUE), 2, min)
end=Sys.time()
end-start 
#Coa
start=Sys.time()
distance_Coa=apply(gDistance(sp_DCoa_84, sp_ANPs_84, byid=TRUE), 2, min)
end=Sys.time()
end-start 
#Col
start=Sys.time()
distance_Col=apply(gDistance(sp_DCol_84, sp_ANPs_84, byid=TRUE), 2, min)
end=Sys.time()
end-start 

final_compilación=Sys.time()
final_compilación - inicio_compilación
system('CMD /C "ECHO The R process has finished running && PAUSE"',   
       invisible=FALSE, wait=FALSE)

#Generamos un objeto que concatene los resultados de la operación.
dist_min_1=c(distance_Ags,distance_BC,distance_BCs,distance_Cam,distance_Coa,distance_Col)
dist_min_1

#Un dataframe para los distritos
DIST_1=rbind(DAgs_84, DBC_84, DBCs_84,DCam_84,DCoa_84,DCol_84)



dist_1=data.frame(CVE_FED=(DIST_1$entidad *100+DIST_1$distrito),  
                  entidad=DIST_1$entidad, distrito=DIST_1$distrito,
                  dist_min=min_dist_1)
head(dist_1)
# write.csv(dist_1, "Bases de Datos/Generadas/dist_1.csv")

# 3.2 Siguientes 50 distancias ---------------------------------------------

#   DCps,DCua, DCmx, DDur, DGua.
#

DCps=read_sf("Bases de datos/SHP/disfed2018/cps/DISTRITO.shp")
DCua=read_sf("Bases de datos/SHP/disfed2018/cua/DISTRITO.shp")
DCmx=read_sf("Bases de datos/SHP/disfed2018/df/DISTRITO.shp")
DDur=read_sf("Bases de datos/SHP/disfed2018/dgo/DISTRITO.shp")
DGua=read_sf("Bases de datos/SHP/disfed2018/gua/DISTRITO.shp")


DCps_84=st_transform(DCps, 4326)
DCua_84=st_transform(DCua, 4326)
DCmx_84=st_transform(DCmx, 4326)
DDur_84=st_transform(DDur, 4326)
DGua_84=st_transform(DGua, 4326)


DCps_84=as(DCps_84,"Spatial")
DCua_84=as(DCua_84,"Spatial")
DCmx_84=as(DCmx_84,"Spatial")
DDur_84=as(DDur_84,"Spatial")
DGua_84=as(DGua_84, "Spatial")

sp_DCps_84=SpatialPoints(DCps_84)
sp_DCua_84=SpatialPoints(DCua_84)
sp_DCmx_84=SpatialPoints(DCmx_84)
sp_DDur_84=SpatialPoints(DDur_84)
sp_DGua_84=SpatialPoints(DGua_84)


#Calcuamos la distancia mínima entre cada uno y su polígono.

inicio_compilación=Sys.time()
#Cps
start=Sys.time()
distance_Cps=apply(gDistance(sp_DCps_84, sp_ANPs_84, byid=TRUE), 2, min)
end=Sys.time()
end-start           

#Cua
start=Sys.time()
distance_Cua=apply(gDistance(sp_DCua_84, sp_ANPs_84, byid=TRUE), 2, min)
end=Sys.time()
end-start 
#Cmx
start=Sys.time()
distance_Cmx=apply(gDistance(sp_DCmx_84, sp_ANPs_84, byid=TRUE), 2, min)
end=Sys.time()
end-start 
#Dur
start=Sys.time()
distance_Dur=apply(gDistance(sp_DDur_84, sp_ANPs_84, byid=TRUE), 2, min)
end=Sys.time()
end-start 
##Gua
start=Sys.time()
distance_Gua=apply(gDistance(sp_DGua_84, sp_ANPs_84, byid=TRUE), 2, min)
end=Sys.time()
end-start 

final_compilación=Sys.time()
final_compilación - inicio_compilación #1. 46 segundos para 50x182 figuras.
system('CMD /C "ECHO The R process has finished running && PAUSE"',   
       invisible=FALSE, wait=FALSE)

#Generamos un objeto que concatene los resultados de la operación.
dist_min_2=c(distance_Cps, distance_Cua, distance_Cmx, distance_Dur, distance_Gua)
dist_min_2

#Un dataframe para los distritos
DIST_2=rbind(DCps_84, DCua_84, DCmx_84,DDur_84,DGua_84)

dist_2=data.frame(CVE_FED=(DIST_2$entidad *100+DIST_2$distrito),  
                  entidad=DIST_2$entidad, distrito=DIST_2$distrito,
                  dist_min=dist_min_2)
head(dist_2)
write.csv(dist_2, "Bases de Datos/Generadas/dist_2.csv")

# 3.3 ¡Aún más! n variables a medir distancia.  ---------------------------


#   DGue, DHid, DJal,DEdo, DMic,
#
DGue=read_sf("Bases de datos/SHP/disfed2018/gue/DISTRITO.shp")
DHid=read_sf("Bases de datos/SHP/disfed2018/hgo/DISTRITO.shp")
DJal=read_sf("Bases de datos/SHP/disfed2018/jal/DISTRITO.shp")
DEdo=read_sf("Bases de datos/SHP/disfed2018/mex/DISTRITO.shp")
DMic=read_sf("Bases de datos/SHP/disfed2018/mic/DISTRITO.shp")





# 3.4 Uffas... La distancia de las penúltimas n figuras. ------------------

#  DMor, DNay,DNL, DOax, DPue, DQue, DQui, DSLP, DSin.
#
DMor=read_sf("Bases de datos/SHP/disfed2018/mor/DISTRITO.shp")
DNay=read_sf("Bases de datos/SHP/disfed2018/nay/DISTRITO.shp")
DNL=read_sf("Bases de datos/SHP/disfed2018/nl/DISTRITO.shp")
DOax=read_sf("Bases de datos/SHP/disfed2018/oax/DISTRITO.shp")
DPue=read_sf("Bases de datos/SHP/disfed2018/pue/DISTRITO.shp")
DQue=read_sf("Bases de datos/SHP/disfed2018/que/DISTRITO.shp")
DQui=read_sf("Bases de datos/SHP/disfed2018/qui/DISTRITO.shp")
DSLP=read_sf("Bases de datos/SHP/disfed2018/san/DISTRITO.shp")
DSin= read_sf("Bases de datos/SHP/disfed2018/sin/DISTRITO.shp")


# 3.5 Al fin: distancia de los últimos n polígonos. -----------------------

#  DSon, DTab, DTam, DTla, DVer, DYuc,DZac.
#               
DSon=read_sf("Bases de datos/SHP/disfed2018/son/DISTRITO.shp")
DTab=read_sf("Bases de datos/SHP/disfed2018/tab/DISTRITO.shp")
DTam=read_sf("Bases de datos/SHP/disfed2018/tam/DISTRITO.shp")
DTla=read_sf("Bases de datos/SHP/disfed2018/tla/DISTRITO.shp")
DVer=read_sf("Bases de datos/SHP/disfed2018/ver/DISTRITO.shp")
DYuc=read_sf("Bases de datos/SHP/disfed2018/yuc/DISTRITO.shp")
DZac=read_sf("Bases de datos/SHP/disfed2018/zac/DISTRITO.shp")


distancia_2=data.frame()


# 4.El orígen de GEOS$min_dist --------------------------------------------







