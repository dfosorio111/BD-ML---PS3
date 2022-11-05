rm(list=ls())

#Samuel
setwd("~/Desktop/Big Data/Repositorios/BD-ML---PS3")

train_sf <- readRDS("data/conBañosyMetros.rds")

setwd("~/Desktop/Big Data/PS3")

# Medellín

## unzip file
unzip(zipfile="05Antioquia/05_Antioquia_CSV.zip" , exdir="input/." , overwrite=T) 

## data manzanas
mgnmed <- import("05Antioquia/05_Antioquia_CSV/CNPV2018_MGN_A2_05.CSV")
colnames(mgnmed)
mgnmed <- mgnmed %>% select(COD_DANE_ANM,UA_CLASE,COD_ENCUESTAS,U_VIVIENDA)

## data hogar
hogmed <- import("05Antioquia/05_Antioquia_CSV/CNPV2018_2HOG_A2_05.CSV")
colnames(hogmed)
hogmed <- hogmed %>% select(UA_CLASE,COD_ENCUESTAS,U_VIVIENDA,H_NROHOG,H_NRO_CUARTOS,HA_TOT_PER)

## data vivienda
vivmed <- import("05Antioquia/05_Antioquia_CSV/CNPV2018_1VIV_A2_05.CSV")
colnames(vivmed)
vivmed <- vivmed %>% select(COD_ENCUESTAS,UA_CLASE,U_VIVIENDA,V_TOT_HOG,VA1_ESTRATO)

## join hogar-vivienda
viv_hog_med <- left_join(hogmed,vivmed,by=c("COD_ENCUESTAS","U_VIVIENDA","UA_CLASE"))

## joing mnz-hogar-vivienda
viv_hog_mgn_med <- left_join(viv_hog_med,mgnmed,by=c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA"))

##=== collapse data ===##
dbmed <- viv_hog_mgn_med %>%
  group_by(COD_DANE_ANM) %>% 
  summarise(med_H_NRO_CUARTOS=median(H_NRO_CUARTOS,na.rm=T), 
            sum_HA_TOT_PER=sum(HA_TOT_PER,na.rm=T), 
            med_V_TOT_HOG=median(V_TOT_HOG,na.rm=T),
            med_VA1_ESTRATO=median(VA1_ESTRATO,na.rm=T))

## export data
export(db,"output/mnz_censo_2018.rds")

## delete files
unlink("input/censo_2018/",recursive = T)
unlink("input/__MACOSX/",recursive = T)






mnz <- readRDS("data/mnz_censo_2018.rds")

mnz_bog <- mnz%>%subset(MPIO_CCDGO == "11001")
mnz_med <- mnz%>%subset(MPIO_CCDGO == "05001")
mnz_cal <- mnz%>%subset(MPIO_CCDGO == "76001")


#Cargar censo
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning/BD-ML---PS3/data/Censo")
## unzip file
unzip(zipfile="11Bogota/11_BOGOTA_CSV.zip", overwrite=T) 
## data manzanas
mgn_Bog <- import("11Bogota/CNPV2018_MGN_A2_11.CSV")
#Selección de variables
mgn_Bog <- mgn_Bog %>% dplyr::select(COD_DANE_ANM,UA_CLASE,COD_ENCUESTAS,U_VIVIENDA)
## data hogar
hog_bog <- import("11Bogota/CNPV2018_2HOG_A2_11.CSV")
hog_bog <- hog_bog %>% dplyr::select(UA_CLASE,COD_ENCUESTAS,U_VIVIENDA,H_NROHOG,H_NRO_CUARTOS,HA_TOT_PER)
## data vivienda
viv_bog <- import("11Bogota/CNPV2018_1VIV_A2_11.CSV") 
colnames(viv)
viv_bog <- viv_bog %>% dplyr::select(COD_ENCUESTAS,UA_CLASE,U_VIVIENDA,V_TOT_HOG,VA1_ESTRATO, V_MAT_PARED, V_MAT_PISO, VE_RECBAS)
## join hogar-vivienda
viv_hog_bog <- left_join(hog_bog,viv_bog,by=c("COD_ENCUESTAS","U_VIVIENDA","UA_CLASE"))
## joing mnz-hogar-vivienda
viv_hog_mgn_bog <- left_join(viv_hog_bog,mgn_Bog,by=c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA"))
#Ver las variables que tenemos
names(viv_hog_mgn_bog)

#Exportamos los datos 
export(viv_hog_mgn_bog,"Censo_Bogota.rds")
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning/BD-ML---PS3/data/Censo")
Censo <- readRDS("Censo_Bogota.rds")
names(Censo)

#Crear función moda
moda <- function(codes){
  which.max(tabulate(codes))
}

Censo%>%count(VE_RECBAS)

Censo$H_NRO_CUARTOS[which(Censo$H_NRO_CUARTOS == 99)] <- NA
##=== collapse data ===##
db <- Censo %>%
  group_by(COD_DANE_ANM) %>% 
  summarise(mean_H_NRO_CUARTOS=mean(H_NRO_CUARTOS,na.rm=T),
            med_H_NRO_CUARTOS=median(H_NRO_CUARTOS,na.rm=T),
            sum_HA_TOT_PER=sum(HA_TOT_PER,na.rm=T), 
            med_V_TOT_HOG=median(V_TOT_HOG,na.rm=T),
            mod_VA1_ESTRATO=moda(VA1_ESTRATO),
            mod_V_MAT_PARED=moda(V_MAT_PARED),
            mod_V_MAT_PISO=moda(V_MAT_PISO),
            mod_VE_RECBAS=moda(VE_RECBAS)
  )

#Unir Censo con Manzanas para Bogotá


#Se cambia el crs de las manzanas para que se pueda hacer el pegue
mnz_bog <- st_transform(mnz_bog, 4326)
mnz_bog <- mnz_bog%>%select(COD_DANE)
house <- st_join(x=train_sf , y=mnz_bog)

names(house)
names(Censo)

house_def <- left_join(house, db,by=c("COD_DANE"="COD_DANE_ANM"))

sapply(house_def%>%subset(city == "Bogotá D.C"), function(y) sum(length(which(is.na(y)))))

nrow(house_def%>%subset(city == "Bogotá D.C"))
