###Limpiar 
rm(list = ls())
cat("\014")

## llamar pacman (contiene la función p_load)
require(pacman)

## llamar y/o instalar librerias
p_load(tidyverse,rio,skimr,viridis,
       gstat, ## variogram
       sf, ## datos espaciales
       leaflet, ## visualizaciones
       tmaptools, ## geocodificar
       ggsn, ## map scale bar 
       osmdata,  ## Get OSM's data
       class,
       nngeo, ## st_nn function
       spdep, ## Construct neighbours list from polygon list 
       MASS) ## packages with census data
#Directorio Daniel
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning/BD-ML---PS3/data")
#Crear la base de test que contiene los datos de Cali
test <- readRDS("test.Rds")

#Contar NA's en las variables del test
sapply(test, function(y) sum(length(which(is.na(y)))))

#Como primer paso se lleva toda la descripción a "minúscula sostenida"
test$description <- tolower(test$description)


#################1mer filtro: utilizar regex

#Patrones
x1 <- "[:digit:]+[:punct:]+[:digit:]+[:space:]+"
x2 <- "[:digit:]+[:punct:]+[:digit:]+"
x3 <- "[:digit:]+[:space:]+"
x4 <- "[:digit:]+"
x5 <- "[:word:]+[:space:]+"


#Se crea la variable y se llena con NA's que luego se reemplazan con los valores correctos
test$baños <- NA

## replace values para baños
for (i in c("baño","bano", "bao")){
  test <- test %>% 
    mutate(baños = ifelse(is.na(baños)==T,str_extract(string=description , pattern=paste0(x1,i)),baños),
           baños = ifelse(is.na(baños)==T,str_extract(string=description , pattern=paste0(x2,i)),baños),
           baños = ifelse(is.na(baños)==T,str_extract(string=description , pattern=paste0(x3,i)),baños),
           baños = ifelse(is.na(baños)==T,str_extract(string=description , pattern=paste0(x4,i)),baños),
           baños = ifelse(is.na(baños)==T,str_extract(string=description , pattern=paste0(x5,i)),baños))
}

## clean var
for (i in c("baño","bano", "bao"," ","\n\n")){
  test$baños <- gsub(i,"",test$baños)
}

#Aquellas que se pueden convertir a numéricas directamente se convierten
test$baños2 <- as.numeric(test$baños)

#Se crea una lista de posibles textos de resultado
list_ori <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "uno", "dos",
              "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez",
              "01", "02", "03", "04", "05", "06", "07", "08", "09", "1.5", "2.5",
              "3.5", "4.5", "5.5", "6.5", "7.5", "8.5", "9.5")
#Se crea una lista de valores para reemplazar
list_rep <- c(1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,
              1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5)


#Los valores de texto se reemplazan por números
test$baños3 <- NA
for (i in 1:length(list_ori)) {
  test$baños3[which(test$baños == list_ori[i])] <- list_rep[i]
}

#Creamos una variable definitiva con las 2 anteriores
test$baños_def <- test$baños2
test$baños_def <- ifelse(is.na(test$baños_def)==TRUE, test$baños3, test$baños_def)
#Se arman los baños juntando originales con regex
test$baños_def2 <- ifelse(is.na(test$bathrooms)==TRUE, test$baños_def, test$bathrooms)

sum(is.na(test$baños_def) & is.na(test$bathrooms))


#Se utiliza knn para aquellos sin valores originales (esto se debe hacer antes de la transformación a sf)
knn_houses <- test%>%dplyr::select(bedrooms, lat, lon, bathrooms)%>%subset(is.na(bathrooms) == FALSE)
knn_houses2 <- test%>%subset(is.na(bathrooms) == FALSE)%>%dplyr::select(bedrooms, lat, lon)
x_train <- scale(knn_houses2[,1:3]) 
knn_houses_test <- test%>%subset(is.na(bathrooms) == TRUE)%>%dplyr::select(bedrooms, lat, lon)
x_test <- scale(knn_houses_test[,1:3]) 
nearest5 <- knn(train=x_train, test=x_test, cl=knn_houses$bathrooms, k=5)

#Con esto se va a llenar solo aquellos que al final queden definitivamente en NA
contador <- 0
test$bathrooms_knn <- NA
for (i in 1:nrow(test)) {
  if (is.na(test$bathrooms[i]) == TRUE) {
    contador <- contador + 1
    test$bathrooms_knn[i] <- as.numeric(nearest5[contador])
  }
}

#Se replica el proceso de regex para los metros
#Se crea la variable y se llena con NA's que luego se reemplazan con los valores correctos
test$new_surface <- NA

for (i in c("m2","mt2", "m ", "metros","cuadrad","mtr", "mts",  "metrs", "meters","area", "área","espacio de")){
  test <- test %>% 
    mutate(new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x1,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x2,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x3,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x4,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x5,i)),new_surface),
    )
}

## clean var
for (i in c("m2","mt2", "m ", "metros","cuadrad","mtr", "mts", "metrs", "meters","area", "área","espacio de")){
  test$new_surface <- gsub(i,"",test$new_surface)
}

#Si algo quedó con , se reemplaza por punto para convertirlo así a numérico
test$new_surface2 <- as.numeric(gsub(pattern = ",", replacement = ".", x = test$new_surface))

#Note que los que son chiquiticos la gran mayoría están divididos por mil porque usan el punto de separador de miles
test$new_surface2[which(test$new_surface2 < 5)] <- 1000*test$new_surface2[which(test$new_surface2 < 5)]

#Se utiliza el surface total, pero si no tiene dato se llena con surface covered
test$new_surface_def <- ifelse(is.na(test$surface_total),test$surface_covered,test$surface_total)
is.na(test$new_surface_def)%>%table() #2838 NA


#Ahora, los que queden vacío ahí se llenan con la del regex
test$new_surface_def2 <- ifelse(is.na(test$new_surface_def),test$new_surface2,test$new_surface_def)
is.na(test$new_surface_def2)%>%table() #1636 NA

#########################2do filtro: vecinos espaciales



#Convertir dataframe a sf
test_sf <- st_as_sf(x = test, ## datos
                     coords=c("lon","lat"), ## coordenadas
                     crs=4326) ## CRS

#Armado de buffers de 200 metros a la redonda
test_sp <- test_sf %>% st_buffer(200) %>% as_Spatial() # poligonos

## obtener vecinos
test_nb <- poly2nb(pl=test_sp , queen=T) # opcion reina

#Crear variables
test_sf$bathrooms_Def <- NA
test_sf$metros_Def <- NA
test_sf$metros_Def2 <- NA
for (i in 1:nrow(test_sf)) {
  test_sf$bathrooms_Def[i] <- mean(test_sf$baños_def2[test_nb[[i]]],na.rm=TRUE)
  temporal <- as.data.frame(test_sf[test_nb[[i]],])
  test_sf$metros_Def [i] <- mean(temporal$new_surface_def2[which(temporal$bedrooms == test_sf$bedrooms[i])], na.rm = TRUE)
  test_sf$metros_Def2[i] <- mean(test_sf$new_surface_def2[test_nb[[i]]],na.rm=TRUE)
}

#Revisión de NA
is.na(test_sf$bathrooms_Def)%>%table()
is.na(test_sf$metros_Def)%>%table()
is.na(test_sf$metros_Def2)%>%table()

#Llenar los baños
test_sf$Final_Bathrooms <- ifelse(is.na(test_sf$bathrooms) == TRUE, ifelse(is.na(test_sf$baños_def2) == TRUE, test_sf$bathrooms_Def, test_sf$baños_def2), test_sf$bathrooms)
sum(is.na(test_sf$Final_Bathrooms))

#Completar los metros
test_sf$Final_Metros <- ifelse(is.na(test_sf$new_surface_def2) == TRUE, test_sf$metros_Def, test_sf$new_surface_def2)
sum(is.na(test_sf$Final_Metros))

#Exportar la base 
write_rds(test_sf, "Cali_sf.rds")


#######################################Censo y Manzanas
#Cargar manzanas
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning")
mnz <- st_read("MGN_URB_MANZANA.shp") #%>%dplyr::select(MANZ_CCNCT)
#Manzanas de Cali
mnz_cal <- mnz%>%subset(COD_MPIO == "76001")
#Cargar censo Bogotá
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning/BD-ML---PS3/data/Censo")
## data manzanas
mgn_cal <- import("76ValleDelCauca/CNPV2018_MGN_A2_76.CSV")
#Selección de variables
mgn_cal <- mgn_cal %>% dplyr::select(COD_DANE_ANM,UA_CLASE,COD_ENCUESTAS,U_VIVIENDA)
## data hogar
hog_cal <- import("76ValleDelCauca/CNPV2018_2HOG_A2_76.CSV")
hog_cal <- hog_cal %>% dplyr::select(UA_CLASE,COD_ENCUESTAS,U_VIVIENDA,H_NROHOG,H_NRO_CUARTOS,HA_TOT_PER)
## data vivienda
viv_cal <- import("76ValleDelCauca/CNPV2018_1VIV_A2_76.CSV") 
viv_cal <- viv_cal %>% dplyr::select(COD_ENCUESTAS,UA_CLASE,U_VIVIENDA,V_TOT_HOG,VA1_ESTRATO, V_MAT_PARED, V_MAT_PISO, VE_RECBAS)
## join hogar-vivienda
viv_hog_cal <- left_join(hog_cal,viv_cal,by=c("COD_ENCUESTAS","U_VIVIENDA","UA_CLASE"))
## joing mnz-hogar-vivienda
viv_hog_mgn_cal <- left_join(viv_hog_cal,mgn_cal,by=c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA"))
#Ver las variables que tenemos
names(viv_hog_mgn_cal)

#Exportamos los datos 
export(viv_hog_mgn_cal,"Censo_Cali.rds")

#Crear función moda
moda <- function(codes){
  which.max(tabulate(codes))
}

viv_hog_mgn_cal$H_NRO_CUARTOS[which(viv_hog_mgn_cal$H_NRO_CUARTOS == 99)] <- NA
##=== collapse data ===##
db <- viv_hog_mgn_cal %>%
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


#Unir Censo con Manzanas
#Se cambia el crs de las manzanas para que se pueda hacer el pegue
mnz_cal <- st_transform(mnz_cal, 4326)
mnz_cal <- mnz_cal%>%dplyr::select(COD_DANE)
house_cal <- st_join(x=test_sf , y=mnz_cal)

#Base definitiva con las variables del censo, unido por las manzanas
house_def_cal <- left_join(house_cal, db,by=c("COD_DANE"="COD_DANE_ANM"))

#Vecinos cercanos
house_sp_cal <- house_def_cal %>% st_buffer(200) %>% as_Spatial() # poligonos

## obtener vecinos
house_nb_cal <- poly2nb(pl=house_sp_cal , queen=T) # opcion reina

#Crear variables
house_def_cal$mean_H_NRO_CUARTOS2 <- NA
house_def_cal$med_H_NRO_CUARTOS2 <- NA
house_def_cal$sum_HA_TOT_PER2 <- NA
house_def_cal$med_V_TOT_HOG2 <- NA
house_def_cal$mod_VA1_ESTRATO2 <- NA
house_def_cal$mod_V_MAT_PARED2 <- NA
house_def_cal$mod_V_MAT_PISO2 <- NA
house_def_cal$mod_VE_RECBAS2 <- NA
for (i in 1:nrow(house_def_cal)) {
  house_def_cal$mean_H_NRO_CUARTOS2[i] <- mean(house_def_cal$mean_H_NRO_CUARTOS[house_nb_cal[[i]]],na.rm=TRUE)
  house_def_cal$med_H_NRO_CUARTOS2[i] <- mean(house_def_cal$med_H_NRO_CUARTOS[house_nb_cal[[i]]],na.rm=TRUE)
  house_def_cal$sum_HA_TOT_PER2[i] <- mean(house_def_cal$sum_HA_TOT_PER[house_nb_cal[[i]]],na.rm=TRUE)
  house_def_cal$med_V_TOT_HOG2[i] <- mean(house_def_cal$med_V_TOT_HOG[house_nb_cal[[i]]],na.rm=TRUE)
  house_def_cal$mod_VA1_ESTRATO2[i] <- moda(house_def_cal$mod_VA1_ESTRATO[house_nb_cal[[i]]])
  house_def_cal$mod_V_MAT_PARED2[i] <- moda(house_def_cal$mod_V_MAT_PARED[house_nb_cal[[i]]])
  house_def_cal$mod_V_MAT_PISO2[i] <- moda(house_def_cal$mod_V_MAT_PISO[house_nb_cal[[i]]])
  house_def_cal$mod_VE_RECBAS2[i] <- moda(house_def_cal$mod_VE_RECBAS[house_nb_cal[[i]]])
}

#Revisar NA
sapply(house_def_cal, function(y) sum(length(which(is.na(y)))))

#Nombres de la base
names(house_def_cal)

lista_definitivas <- c("property_id","city", "price", "surface_total", "surface_covered", "bedrooms",
                       "title", "description", "property_type", "operation_type", "Final_Bathrooms", 
                       "Final_Metros", "geometry", "COD_DANE", "mean_H_NRO_CUARTOS2", "med_H_NRO_CUARTOS2",
                       "sum_HA_TOT_PER2", "med_V_TOT_HOG2", "mod_VA1_ESTRATO2", "mod_V_MAT_PARED2",
                       "mod_V_MAT_PISO2", "mod_VE_RECBAS2")

#Guardar la base definitiva
houses_Cali <- house_def_cal[,(names(house_def_cal) %in% lista_definitivas)]

#Exportar la base
write_rds(houses_Cali,"BaseCali.rds")

cali <- readRDS("Censo/BaseCali.rds")

sapply(cali, function(y) sum(length(which(is.na(y)))))
