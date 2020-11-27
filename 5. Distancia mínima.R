# 5. Cálculo de la distancia mínima --------

#Calculamos la distancia mínima entre los polígonos de distrito electoral y 
#los polígonos de áreas naturales protegidas. 
###¡ALERTA! El código es muy pesado y no es necesario de ejecutar, pero
### está incluido para poder ser replicado. 
#TIEMPO DE EJECUCIÓN: 3. Más de ocho horas. 4. Pocos minutos.

# 1. Libraries -----------------------------------------------------------
library(dplyr)
library(ggplot2)
library(gridExtra)
library(sf)
library(nngeo)

# 2. Ejemplo: Aguascalientes ("DAgs") ----------------------------------------------
# 2.1 Utilizando sf -----------------------------------------------
# 2.2 Utilizando nngeo -----------------------------------------------


# 3. Fuerza Bruta: Calculando la distancia para todo (sf). ---------------------

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


nearest_feature=function(x){
  st_nearest_feature(st_transform(x, 4326))
}

NF=c(DAgs, DBC, DBCs,DCam,DCoa,
                  DCol, DCps,DCua, DCmx, DDur, DGua,
                  DGue, DHid, DJal,DEdo, DMic, DMor, DNay,
                  DNL, DOax,DPue, DQue, DQui,DSLP, DSin, 
                  DSon, DTab, DTam, DTla, DVer,
                  DYuc,DZac)


# 3.1 Primeras 48 distancias -----------------------------------------------

#   DAgs, DBC, DBCs, DCam, DCoa, DCol. 
#   1.73 segundos en rgeos

DAgs=st_read("Bases de datos/SHP/disfed2018/ags/DISTRITO.shp")
DBC= read_sf("Bases de datos/SHP/disfed2018/bc/DISTRITO.shp")  #troubleshoot DBC
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


# Utilizando simple features (sf) 

inicio.compilación = Sys.time()
#DAgs, NOTDBC, DBCs, DCam, DCoa, DCol.

#Closest polygon      #5.57 min

cercana_Ags=st_nearest_feature(DAgs_84, ANPs_84)
cercana_BC=st_nearest_feature(DBC_84, ANPs_84)
cercana_BCs=st_nearest_feature(DBCs_84, ANPs_84)
cercana_Cam=st_nearest_feature(DCam_84, ANPs_84)
cercana_Coa=st_nearest_feature(DCoa_84, ANPs_84)
cercana_Col=st_nearest_feature(DCol_84, ANPs_84)

#Generamos un índice utilizando la cercanía. #0.18 seg.

s_cercana_Ags=ANPs_84%>%slice(cercana_Ags) 
s_cercana_BC=ANPs_84%>%slice(cercana_BC)
s_cercana_BCs=ANPs_84%>%slice(cercana_BCs)
s_cercana_Cam=ANPs_84%>%slice(cercana_Cam)
s_cercana_Coa=ANPs_84%>%slice(cercana_Coa)
s_cercana_Col=ANPs_84%>%slice(cercana_Col)

#Distance to closest polygon   #
start=Sys.time()
distancia_Ags=st_distance(DAgs_84, s_cercana_Ags, by_element=TRUE)
distancia_BC=st_distance(DBC_84, s_cercana_BC, by_element=TRUE)
distancia_BCs=st_distance(DBCs_84, s_cercana_BCs, by_element=TRUE)
distancia_Cam=st_distance(DCam_84, s_cercana_Cam, by_element=TRUE)
distancia_Coa=st_distance(DCoa_84, s_cercana_Coa, by_element=TRUE)
distancia_Col=st_distance(DCol_84, s_cercana_Col, by_element=TRUE)
end=Sys.time()
end-start



#Generamos un objeto que concatene los resultados de las operaciones.
cercana_1=c(cercana_Ags, cercana_BC, cercana_BCs,cercana_Cam,cercana_Coa,cercana_Col)
dist_min_1=c(distancia_Ags,distancia_BC,distancia_BCs,distancia_Cam,distancia_Coa,distancia_Col)


#Un dataframe para los distritos
DIST_1=rbind(DAgs_84, DBC_84, DBCs_84,DCam_84,DCoa_84,DCol_84)

dist_1_sf=data.frame(CVE_FED=(DIST_1$entidad *100+DIST_1$distrito),  
                     entidad=DIST_1$entidad, distrito=DIST_1$distrito, ANP_Cercana=cercana_1, dist_min=dist_min_1)

final.compilación = Sys.time()
final.compilación - inicio.compilación    #8.49 mins (sin incluir a BC)

system('CMD /C "ECHO The R process has finished running && PAUSE"', 
       invisible=FALSE, wait=FALSE)

write.csv(dist_1_sf_SBC, "Bases de Datos/Generadas/dist_1_sf.csv")

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

#Utilizando simple features (sf)

inicio.compilación = Sys.time()
#DCps,DCua, DCmx, DDur, DGua.

#Closest polygon      # min

cercana_Cps=st_nearest_feature(DCps_84, ANPs_84)
cercana_Cua=st_nearest_feature(DCua_84, ANPs_84)
cercana_Cmx=st_nearest_feature(DCmx_84, ANPs_84)
cercana_Dur=st_nearest_feature(DDur_84, ANPs_84)
cercana_Gua=st_nearest_feature(DGua_84, ANPs_84)

#Generamos un índice utilizando la cercanía. #0.18 seg.

cercana_Cps=ANPs_84%>%slice(cercana_Cps) 
cercana_Cua=ANPs_84%>%slice(cercana_Cua)
cercana_Cmx=ANPs_84%>%slice(cercana_Cmx)
cercana_Dur=ANPs_84%>%slice(cercana_Dur)
cercana_Gua=ANPs_84%>%slice(cercana_Gua)

#Distance to closest polygon   #

distancia_Cps=st_distance(DCps_84, cercana_Cps, by_element=TRUE)
distancia_Cua=st_distance(DCua_84, cercana_Cua, by_element=TRUE)
distancia_Cmx=st_distance(DCmx_84, cercana_Cmx, by_element=TRUE)
distancia_Dur=st_distance(DDur_84, cercana_Dur, by_element=TRUE)
distancia_Gua=st_distance(DGua_84, cercana_Gua, by_element=TRUE)

#Generamos un objeto que concatene los resultados de las operaciones.
cercana_2=c(cercana_Cps,cercana_Cua,cercana_Cmx,cercana_Dur,cercana_Gua)
dist_min_2=c(distancia_Cps,distancia_Cua,distancia_Cmx,distancia_Dur,distancia_Gua)


#Un dataframe para los distritos
DIST_2=rbind(DCps_84, DCua_84, DCmx_84,DDur_84,DGua_84)

dist_2_sf=data.frame(CVE_FED=(DIST_2$entidad *100+DIST_2$distrito),  
                     entidad=DIST_2$entidad, distrito=DIST_2$distrito, 
                     ANP_Cercana=cercana_2, dist_min=dist_min_2)

final.compilación = Sys.time()

write.csv(dist_2_sf, "Bases de Datos/Generadas/dist_2_sf.csv")



# 3.3 ¡Aún más! 89 variables a medir distancia.  ---------------------------

#   DGue, DHid, DJal,DEdo, DMic,
#
DGue=read_sf("Bases de datos/SHP/disfed2018/gue/DISTRITO.shp")
DHid=read_sf("Bases de datos/SHP/disfed2018/hgo/DISTRITO.shp")
DJal=read_sf("Bases de datos/SHP/disfed2018/jal/DISTRITO.shp")
DEdo=read_sf("Bases de datos/SHP/disfed2018/mex/DISTRITO.shp")
DMic=read_sf("Bases de datos/SHP/disfed2018/mic/DISTRITO.shp")

DGue_84=st_transform(DGue, 4326)
DHid_84=st_transform(DHid, 4326)
DJal_84=st_transform(DJal, 4326)
DEdo_84=st_transform(DEdo, 4326)
DMic_84=st_transform(DMic, 4326)

# Utilizando simple features (sf) 

inicio.compilación = Sys.time()
#DGue, DHid, DJal, DEdo, DMic.

#Closest polygon      #5.57 min

cercana_Gue=st_nearest_feature(DGue_84, ANPs_84)
cercana_Hid=st_nearest_feature(DHid_84, ANPs_84)
cercana_Jal=st_nearest_feature(DJal_84, ANPs_84)
cercana_Edo=st_nearest_feature(DEdo_84, ANPs_84)
cercana_Mic=st_nearest_feature(DMic_84, ANPs_84)


#Generamos un índice utilizando la cercanía. #0.18 seg.

s_cercana_Gue=ANPs_84%>%slice(cercana_Gue) 
s_cercana_Hid=ANPs_84%>%slice(cercana_Hid)
s_cercana_Jal=ANPs_84%>%slice(cercana_Jal)
s_cercana_Edo=ANPs_84%>%slice(cercana_Edo)
s_cercana_Mic=ANPs_84%>%slice(cercana_Mic)


#Distance to closest polygon   #
start=Sys.time()
distancia_Gue=st_distance(DGue_84, s_cercana_Gue, by_element=TRUE)
distancia_Hid=st_distance(DHid_84, s_cercana_Hid, by_element=TRUE)
distancia_Jal=st_distance(DJal_84, s_cercana_Jal, by_element=TRUE)
distancia_Edo=st_distance(DEdo_84, s_cercana_Edo, by_element=TRUE)
distancia_Mic=st_distance(DMic_84, s_cercana_Mic, by_element=TRUE)

end=Sys.time()
end-start

#Generamos un objeto que concatene los resultados de las operaciones.
cercana_3=c(cercana_Gue, cercana_Hid, cercana_Jal, cercana_Edo, cercana_Mic)
dist_min_3=c(distancia_Gue,distancia_Hid,distancia_Jal, distancia_Edo, distancia_Mic)


#Un dataframe para los distritos
DIST_3=rbind(DGue_84, DHid_84, DJal_84, DEdo_84, DMic_84)

dist_3_sf=data.frame(CVE_FED=(DIST_3$entidad *100+DIST_3$distrito),  
                     entidad=DIST_3$entidad, distrito=DIST_3$distrito, ANP_Cercana=cercana_3, dist_min=dist_min_3)

final.compilación = Sys.time()
final.compilación - inicio.compilación    #43.78 mins

system('CMD /C "ECHO The R process has finished running && PAUSE"', 
       invisible=FALSE, wait=FALSE)

write.csv(dist_3_sf, "Bases de Datos/Generadas/dist_3_sf.csv")


# 3.4 Uffas... La distancia de las penúltimas 65 figuras. ------------------

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

 Calculando la distancia por medio de sf.
#DMor, DNay,DNL, DOax, DPue, DQue, DQui, DSLP, DSin.
#

DMor_84=st_transform(DMor, 4326)
DNay_84=st_transform(DNay, 4326)
DNL_84=st_transform(DNL, 4326)
DOax_84=st_transform(DOax, 4326)
DPue_84=st_transform(DPue, 4326)
DQue_84=st_transform(DQue, 4326)
DQui_84=st_transform(DQui, 4326)
DSLP_84=st_transform(DSLP, 4326)
DSin_84=st_transform(DSin, 4326)


inicio.compilación = Sys.time()
# DMor, DNay,DNL, DOax, DPue,
# DQue, DQui, DSLP, DSin.

#Closest polygon      #

cercana_Mor=st_nearest_feature(DMor_84, ANPs_84)
cercana_Nay=st_nearest_feature(DNay_84, ANPs_84)
cercana_NL=st_nearest_feature(DNL_84, ANPs_84)
cercana_Oax=st_nearest_feature(DOax_84, ANPs_84)
cercana_Pue=st_nearest_feature(DPue_84, ANPs_84)
cercana_Que=st_nearest_feature(DQue_84, ANPs_84)
cercana_Qui=st_nearest_feature(DQui_84, ANPs_84)
cercana_SLP=st_nearest_feature(DSLP_84, ANPs_84)
cercana_Sin=st_nearest_feature(DSin_84, ANPs_84)

#Generamos un índice utilizando la cercanía. #0.18 seg.

s_cercana_Mor=ANPs_84%>%slice(cercana_Mor) 
s_cercana_Nay=ANPs_84%>%slice(cercana_Nay)
s_cercana_NL=ANPs_84%>%slice(cercana_NL)
s_cercana_Oax=ANPs_84%>%slice(cercana_Oax)
s_cercana_Pue=ANPs_84%>%slice(cercana_Pue)
s_cercana_Que=ANPs_84%>%slice(cercana_Que)
s_cercana_Qui=ANPs_84%>%slice(cercana_Qui)
s_cercana_SLP=ANPs_84%>%slice(cercana_SLP)
s_cercana_Sin=ANPs_84%>%slice(cercana_Sin)

#Distance to closest polygon   #

distancia_Mor=st_distance(DMor_84, s_cercana_Mor, by_element=TRUE)
distancia_Nay=st_distance(DNay_84, s_cercana_Nay, by_element=TRUE)
distancia_NL=st_distance(DNL_84, s_cercana_NL, by_element=TRUE)
distancia_Oax=st_distance(DOax_84, s_cercana_Oax, by_element=TRUE)
distancia_Pue=st_distance(DPue_84, s_cercana_Pue, by_element=TRUE)
distancia_Que=st_distance(DQue_84, s_cercana_Que, by_element=TRUE)
distancia_Qui=st_distance(DQui_84, s_cercana_Qui, by_element=TRUE)
distancia_SLP=st_distance(DSLP_84, s_cercana_SLP, by_element=TRUE)
#distancia_Sin=st_distance(DSin_84, s_cercana_Sin, by_element=TRUE)

#Generamos un objeto que concatene los resultados de las operaciones.
cercana_4=c(cercana_Mor,cercana_Nay,cercana_NL,cercana_Oax, cercana_Pue,
            cercana_Que, cercana_Qui, cercana_SLP, cercana_Sin)

dist_min_4=c(distancia_Mor,distancia_Nay,distancia_NL,distancia_Oax, distancia_Pue,
             distancia_Que, distancia_Qui, distancia_SLP, distancia_Sin)


# DMor, DNay,DNL, DOax, DPue,
# DQue, DQui, DSLP, DSin.

#Un dataframe para los distritos
DIST_4=rbind(DMor_84, DNay_84,DNL_84, DOax_84, 
             DPue_84,DQue_84, DQui_84, DSLP_84, DSin_84)

dist_4_sf_1=data.frame(CVE_FED=(DIST_4$entidad *100+DIST_4$distrito),  
                     entidad=DIST_4$entidad, distrito=DIST_4$distrito, ANP_Cercana=cercana_4,
                     dist_min=dist_min_4)

write.csv(dist_4_sf_1, "Bases de Datos/Generadas/dist_4_sf_2.csv")


final.compilación = Sys.time()
final.compilación - start    


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

DSon_84=st_transform(DSon, 4326)
DTab_84=st_transform(DTab, 4326)
DTam_84=st_transform(DTam, 4326)
DTla_84=st_transform(DTla, 4326)
DVer_84=st_transform(DVer, 4326)
DYuc_84=st_transform(DYuc, 4326)
DZac_84=st_transform(DZac, 4326)


# 3.5.1 Utilizando simple features (sf) -----------------------------------

#  DSon, DTab, DTam, DTla, DVer, DYuc,DZac.
#
inicio.compilación = Sys.time()

#Closest polygon      # min
start=Sys.time()
cercana_Son=st_nearest_feature(DSon_84, ANPs_84)
cercana_Tab=st_nearest_feature(DTab_84, ANPs_84)
cercana_Tam=st_nearest_feature(DTam_84, ANPs_84)
cercana_Tla=st_nearest_feature(DTla_84, ANPs_84)
cercana_Ver=st_nearest_feature(DVer_84, ANPs_84)
cercana_Yuc=st_nearest_feature(DYuc_84, ANPs_84)
cercana_Zac=st_nearest_feature(DZac_84, ANPs_84)

end=Sys.time()
end-start
#Generamos un índice utilizando la cercanía. #0.18 seg.

#DSon, DTab, DTam, DTla, DVer, DYuc,DZac.
#

s_cercana_Son=ANPs_84%>%slice(cercana_Son) 
s_cercana_Tab=ANPs_84%>%slice(cercana_Tab)
s_cercana_Tam=ANPs_84%>%slice(cercana_Tam)
s_cercana_Tla=ANPs_84%>%slice(cercana_Tla)
s_cercana_Ver=ANPs_84%>%slice(cercana_Ver)
s_cercana_Yuc=ANPs_84%>%slice(cercana_Yuc)
s_cercana_Zac=ANPs_84%>%slice(cercana_Zac)


#Distance to closest polygon   #
start=Sys.time()
distancia_Son=st_distance(DSon_84, s_cercana_Son,by_element=TRUE)
distancia_Tab=st_distance(DTab_84, s_cercana_Tab, by_element=TRUE)
distancia_Tam=st_distance(DTam_84, s_cercana_Tam, by_element=TRUE)
distancia_Tla=st_distance(DTla_84, s_cercana_Tla, by_element=TRUE)
distancia_Ver=st_distance(DVer_84, s_cercana_Ver, by_element=TRUE)
distancia_Yuc=st_distance(DYuc_84, s_cercana_Yuc, by_element=TRUE)
distancia_Zac=st_distance(DZac_84, s_cercana_Zac, by_element=TRUE)

end=Sys.time()
end-start

system('CMD /C "ECHO The R process has finished running && PAUSE"',   
       invisible=FALSE, wait=FALSE)


#Generamos un objeto que concatene los resultados de las operaciones.

#DSon, DTab, DTam, DTla, DVer, DYuc,DZac.

cercana_5=c(cercana_Son,cercana_Tab, cercana_Tam, cercana_Tla, 
            cercana_Ver, cercana_Yuc, cercana_Zac)

dist_min_5=c(distancia_Son, distancia_Tab, distancia_Tam, distancia_Tla, distancia_Ver,  distancia_Yuc, distancia_Zac)

#Un dataframe para los distritos
DIST_5=rbind(DSon_84, DTab_84,DTam_84, DTla_84,DVer_84,DYuc_84, DZac_84)

dist_5_sf=data.frame(CVE_FED=(DIST_5$entidad *100+DIST_5$distrito),  
                     entidad=DIST_5$entidad, distrito=DIST_5$distrito, ANP_NEAR=cercana_5, dist_min=dist_min_5)

final.compilación = Sys.time()

write.csv(dist_5_sf, "Bases de Datos/Generadas/dist_5_sf.csv")

system('CMD /C "ECHO The R process has finished running && PAUSE"',   
       invisible=FALSE, wait=FALSE)



# 4.Cálculo cauteloso (nngeo) --------------------------------------------
#Ahora calcularemos la distancia entre el centroide de los 
#polígonos más cercanos.

ANPs=read_sf("Bases de datos/SHP/SHAPE_ANPS/182ANP_Geo_ITRF08_Agosto_2020.shp")

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


#Función para obtener centroides en WGS 84.
Centroides_84=function(x){ 
        return(st_centroid(st_transform(x, 4326)))
        }

#Función para calcular distancia entre centroides y cercanos
distancia=function(x){
        st_nn(Centroides_84(x), Centroides_84(ANPs), k=1, parallel = 3, returnDist = TRUE)
}

        #                c(DAgs, DBC, DBCs,DCam,DCoa,
        #                DCol, DCps,DCua, DCmx, DDur, DGua,
        #                DGue, DHid, DJal,DEdo, DMic, DMor, DNay,
        #                DNL, DOax,DPue, DQue, DQui,DSLP, DSin, 
        #                DSon, DTab, DTam, DTla, DVer,
        #                DYuc,DZac)

Centroides_84(ANPs)
start=Sys.time()
distancia_Ags_nn=distancia(DAgs)
distancia_BC_nn=distancia(DBC)
distancia_BCs_nn=distancia(DBCs)
distancia_Cam_nn=distancia(DCam)
distancia_Coa_nn=distancia(DCoa)
distancia_Col_nn=distancia(DCol)
distancia_Cps_nn=distancia(DCps)
distancia_Cua_nn=distancia(DCua)
distancia_Cmx_nn=distancia(DCmx)
distancia_Dur_nn=distancia(DDur)
distancia_Gua_nn=distancia(DGua)
distancia_Gue_nn=distancia(DGue)
distancia_Hid_nn=distancia(DHid)
distancia_Jal_nn=distancia(DJal)
distancia_Edo_nn=distancia(DEdo)
distancia_Mic_nn=distancia(DMic)
distancia_Mor_nn=distancia(DMor)
distancia_Nay_nn=distancia(DNay)
distancia_NL_nn=distancia(DNL)
distancia_Oax_nn=distancia(DOax)
distancia_Pue_nn=distancia(DPue)
distancia_Que_nn=distancia(DQue)
distancia_Qui_nn=distancia(DQui)
distancia_SLP_nn=distancia(DSLP)
distancia_Sin_nn=distancia(DSin)
distancia_Son_nn=distancia(DSon)
distancia_Tab_nn=distancia(DTab)
distancia_Tam_nn=distancia(DTam)
distancia_Tla_nn=distancia(DTla)
distancia_Ver_nn=distancia(DVer)
distancia_Yuc_nn=distancia(DYuc)
distancia_Zac_nn=distancia(DZac)
end=Sys.time()
end-start

#Convertimos el resultado en dos columnas, 
# ANP cercana en centroides  (c_ANPCercana)
# distancia entre centroides (c_distmin)

# 5. Base de datos DIST ---------------------------------------------------

read.csv("dist_1_sf.csv")
read.csv("dist_2_sf_SC.csv")
read.csv("dist_3_sf.csv")
read.csv("dist_4_sf.csv")
read.csv("dist_5_sf_sSon.csv")

#Función de transformación WGS84.

DIST= rbind.data.frame(to_84(DAgs), to_84(DBC), to_84(DBCs),to_84(DCam),
            to_84(DCoa),to_84(DCol), to_84(DCps),to_84(DCua),
            to_84(DCmx), to_84(DDur), to_84(DGua),
            to_84(DGue), to_84(DHid), to_84(DJal),to_84(DEdo), 
            to_84(DMic), to_84(DMor), to_84(DNay),
            to_84(DNL), to_84(DOax), to_84(DPue), to_84(DQue),
            to_84(DQui), to_84(DSLP), to_84(DSin), to_84(DSon),
            to_84(DTab), to_84(DTam), to_84(DTla), to_84(DVer),
            to_84(DYuc),to_84(DZac))

#Queremos automatizar esto
x = data.frame(matrix(unlist(distancia_Cam_nn), nrow=max(DCam$distrito), byrow=F),stringsAsFactors=FALSE)
x
#xdistancia,ydistrito
transformacion=function(x,y){
 s=data.frame(matrix(unlist(x), nrow=max(y$distrito), 
                   byrow=F),stringsAsFactors=FALSE)
      
         return(s)    
}

c_distmin=rbind.data.frame(transformacion(distancia_Ags_nn, to_84(DAgs)),
            transformacion(distancia_BC_nn, to_84(DBC)), 
            transformacion(distancia_BCs_nn, to_84(DBCs)),
            transformacion(distancia_Cam_nn, to_84(DCam)),
            transformacion(distancia_Coa_nn, to_84(DCoa)),
            transformacion(distancia_Col_nn, to_84(DCol)),
            transformacion(distancia_Cps_nn, to_84(DCps)),
            transformacion(distancia_Cua_nn, to_84(DCua)),
            transformacion(distancia_Cmx_nn, to_84(DCmx)),
            transformacion(distancia_Dur_nn, to_84(DDur)),
            transformacion(distancia_Gua_nn, to_84(DGua)),
            transformacion(distancia_Gue_nn, to_84(DGue)),
            transformacion(distancia_Hid_nn, to_84(DHid)),
            transformacion(distancia_Jal_nn, to_84(DJal)),
            transformacion(distancia_Edo_nn, to_84(DEdo)),
            transformacion(distancia_Mic_nn, to_84(DMic)),
            transformacion(distancia_Mor_nn, to_84(DMor)),
            transformacion(distancia_Nay_nn, to_84(DNay)),
            transformacion(distancia_NL_nn, to_84(DNL)), 
            transformacion(distancia_Oax_nn, to_84(DOax)),
            transformacion(distancia_Pue_nn, to_84(DPue)),
            transformacion(distancia_Que_nn, to_84(DQue)),
            transformacion(distancia_Qui_nn, to_84(DQui)),
            transformacion(distancia_SLP_nn, to_84(DSLP)),
            transformacion(distancia_Sin_nn, to_84(DSin)),
            transformacion(distancia_Son_nn, to_84(DSon)),
            transformacion(distancia_Tab_nn, to_84(DTab)),
            transformacion(distancia_Tam_nn, to_84(DTam)),
            transformacion(distancia_Tla_nn, to_84(DTla)),
            transformacion(distancia_Ver_nn, to_84(DVer)),
            transformacion(distancia_Yuc_nn, to_84(DYuc)),
            transformacion(distancia_Zac_nn, to_84(DZac)))


DIST$CVE_FED=CVE_FED=(DIST$entidad *100+DIST$distrito)
DIST$c_ANPCercana= c_distmin$X1
DIST$c_distmin = c_distmin$X2



#Faltan también las variables de distancia por medio de sf. 
#Por ahora el código no lo cubre, porque que hueva.

GEOS=read.csv("Bases de datos/Generadas/GEOS.csv") #Datos Geoestadísticos
GEOS$c_ANPCercana=c_distmin$X1
GEOS$c_distmin=c_distmin$X2

#Recuerda primero eliminar la columna de geometría, aunque 
#ten en consideración que se utiliza para la visualización de
#la sección 6.
#    write.csv(DIST, "Bases de Datos/Generadas/DIST.csv")
write.csv(GEOS, "Bases de Datos/Generadas/GEOS_Respaldo.csv")



# 6. Visualización --------------------------------------------------------
#Requiere de la geometría conjunta DIST, la función de 
#centroides, la operación st_connect (librería nngeo).

png('Pares.png', pointsize=10, width=1800, height=1200, res=300)
ggplot()+
        geom_sf(data=Centroides_84(DIST), color="#C05555")+
        geom_sf(data=Centroides_84(ANPs), color="#59886B")+
        geom_sf(data=st_connect(Centroides_84(DIST), Centroides_84(ANPs), DIST$c_ANPCercana), add = TRUE)+
        labs(title="Distritaciones y área natural protegidas",
             subtitle="Pares de distritos electorales y ANPs con menor distancia",
             caption="Datos: CONANP, INE-INEGI | @sebasdepapel")+
  
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 15, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 12, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.caption = element_text( size=10, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.7, 0.09))+
  coord_sf(datum=NA)
dev.off()