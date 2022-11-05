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

#Establacer directorios
#Directorio Daniel
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning/BD-ML---PS3/data")
#Crear la base de test
test <- readRDS("test.Rds")
#Crear la base de train
train <- readRDS("train.RDS")

#Se inicia completando las variables de la base original

#Ver cuáles son las variables que tienen Na's
sapply(train, function(y) sum(length(which(is.na(y)))))
sapply(test, function(y) sum(length(which(is.na(y)))))

#El tipo de propiedad y las habitaciones (bedrooms) no tienen ningún NA
#Primero vamos a rellenar los espacios con NA's de los baños

#Como primer paso se lleva toda la descripción a "minúscula sostenida"
train$description <- tolower(train$description)

#Patrones
x1 <- "[:digit:]+[:punct:]+[:digit:]+[:space:]+"
x2 <- "[:digit:]+[:punct:]+[:digit:]+"
x3 <- "[:digit:]+[:space:]+"
x4 <- "[:digit:]+"
x5 <- "[:word:]+[:space:]+"





#Se crea la variable y se llena con NA's que luego se reemplazan con los valores correctos
train$baños <- NA


## replace values para baños
for (i in c("baño","bano", "bao")){
  train <- train %>% 
    mutate(baños = ifelse(is.na(baños)==T,str_extract(string=description , pattern=paste0(x1,i)),baños),
           baños = ifelse(is.na(baños)==T,str_extract(string=description , pattern=paste0(x2,i)),baños),
           baños = ifelse(is.na(baños)==T,str_extract(string=description , pattern=paste0(x3,i)),baños),
           baños = ifelse(is.na(baños)==T,str_extract(string=description , pattern=paste0(x4,i)),baños),
           baños = ifelse(is.na(baños)==T,str_extract(string=description , pattern=paste0(x5,i)),baños))
}



## clean var
for (i in c("baño","bano", "bao"," ","\n\n")){
  train$baños <- gsub(i,"",train$baños)
}

#Se crea la variable y se llena con NA's que luego se reemplazan con los valores correctos
train$new_surface <- NA

for (i in c("mts","m2","mt2","metros","cuadrad","mtr", "metrs", "meters","area", "área","espacio de")){
  train <- train %>% 
    mutate(new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x1,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x2,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x3,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x4,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x5,i)),new_surface),
           )
}

## clean var
for (i in c("mts","m2","mt2","metros","cuadrad","mtr", "metrs", "meters","area", "área","espacio de")){
  train$new_surface <- gsub(i,"",train$new_surface)
}

View(train%>%dplyr::select(surface_total, surface_covered, new_surface, description))

train$new_surface_def <- ifelse(is.na(train$surface_covered) == TRUE, train$new_surface, train$surface_covered)

train$new_surface_def2 <- as.numeric(gsub(pattern = ",", replacement = ".", x = train$new_surface_def ))
  
  
table(is.na(train$surface_covered))
table(is.na(train$new_surface))
table(is.na(train$new_surface_def))

#Ver qué valores tiene 
View(train%>%count(baños))

#Aquellas que se pueden convertir a numéricas directamente se convierten
train$baños2 <- as.numeric(train$baños)

#Se crea una lista de posibles textos de resultado
list_ori <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "uno", "dos",
              "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez",
              "01", "02", "03", "04", "05", "06", "07", "08", "09", "1.5", "2.5",
              "3.5", "4.5", "5.5", "6.5", "7.5", "8.5", "9.5")
#Se crea una lista de valores para reemplazar
list_rep <- c(1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,
              1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5)



#Los valores de texto se reemplazan por números
train$baños3 <- NA
for (i in 1:length(list_ori)) {
  train$baños3[which(train$baños == list_ori[i])] <- list_rep[i]
}

View(train%>%dplyr::select(baños,baños2,baños3))

#Creamos una variable definitiva con las 2 anteriores
train$baños_def <- train$baños2
train$baños_def <- ifelse(is.na(train$baños_def)==TRUE, train$baños3, train$baños_def)

####stand-by####
#Cargar manzanas
mnz1 <- st_read("mgn/MGN_URB_MANZANA.shp")%>%dplyr::select(MANZ_CCNCT)

setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning")

mnz <- st_read("MGN_URB_MANZANA.shp") #%>%dplyr::select(MANZ_CCNCT)

mnz2 <- mnz%>%subset(COD_MPIO == "05001" | COD_MPIO == "11001")

mnz2 <- st_as_sf(mnz2)

class(mnz2)

leaflet() %>% addTiles() %>% addPolygons(data=mnz2[1:1000,])

#Cargar censo
censo <- import("mnz_censo_2018.rds")

#Convertir datafram a sf
train_sf <- st_as_sf(x = train, ## datos
                     coords=c("lon","lat"), ## coordenadas
                     crs=4326) ## CRS

mnz2 <- st_transform(mnz2, 4326)

## unir dos conjuntos de datos basados en la geometría
house <- st_join(x=train_sf, y=mnz2)

house_prueba <- house%>%subset(city == "Bogotá D.C")

#Quitar los NA's de las manzanas (Acá se muere)
house_sin_na <- st_join(x=house, y=mnz2, join=st_nn , maxdist=20 , k=1 , progress=F)
 


######Vecinos espaciales#####
#Convertir datafram a sf
train_sf <- st_as_sf(x = train, ## datos
                     coords=c("lon","lat"), ## coordenadas
                     crs=4326) ## CRS




train_sp <- train_sf %>% st_buffer(100) %>% as_Spatial() # poligonos

## obtener vecinos
train_nb <- poly2nb(pl=train_sp , queen=T) # opcion reina

#Crear variables
train_sf$bathrooms_Def <- NA
train_sf$metros_Def <- NA
train_sf$metros_Def2 <- NA
for (i in 1:nrow(train_sf)) {
  train_sf$bathrooms_Def[i] <- mean(train_sf$bathrooms[train_nb[[i]]],na.rm=TRUE)
  temporal <- as.data.frame(train_sf[train_nb[[i]],])
  train_sf$metros_Def [i] <- mean(temporal$new_surface_def2[which(temporal$bedrooms == train_sf$bedrooms[i])], na.rm = TRUE)
  train_sf$metros_Def2[i] <- mean(train_sf$new_surface_def2[train_nb[[i]]],na.rm=TRUE)
}

Prueba <- train_sf%>%subset(is.na(new_surface_def2) == FALSE & is.na(metros_Def) == FALSE)%>%mutate(MSE = (new_surface_def2-metros_Def)^2 )
mean(Prueba$MSE)

Prueba2 <- train_sf%>%subset(is.na(new_surface_def2) == FALSE & is.na(metros_Def2) == FALSE)%>%mutate(MSE = (new_surface_def2-metros_Def2)^2 )
mean(Prueba2$MSE)

#Llenar los baños
train_sf$Final_Bathrooms <- ifelse(is.na(train_sf$bathrooms) == TRUE, ifelse(is.na(train_sf$baños_def) == TRUE, train_sf$bathrooms_Def, train_sf$baños_def), train_sf$bathrooms)
sum(is.na(train_sf$Final_Bathrooms))

#KNN para los que faltan (En stand-by para completar los 117 baños que faltan)
knn_houses <- train_sf%>%dplyr::select(bedrooms, property_type, metros_Def2, Final_Bathrooms)%>%subset(is.na(Final_Bathrooms) == FALSE)
knn_houses2 <- train_sf%>%subset(is.na(Final_Bathrooms) == FALSE)%>%dplyr::select(bedrooms, property_type, metros_Def2)
knn_houses <- as.data.frame(knn_houses)
knn_houses2 <- as.data.frame(knn_houses2)
knn_houses$property_type <- ifelse(knn_houses$property_type == "Casa", 1, 0)
knn_houses2$property_type <- ifelse(knn_houses2$property_type == "Casa", 1, 0)
x_train <- scale(knn_houses2[,1:3]) 
knn_houses_test <- train_sf%>%subset(is.na(Final_Bathrooms) == TRUE)%>%dplyr::select(bedrooms, property_type, metros_Def2)
knn_houses_test <- as.data.frame(knn_houses_test)
knn_houses_test$property_type <- ifelse(knn_houses_test$property_type == "Casa", 1, 0)
x_test <- scale(knn_houses_test[,1:3]) 
nearest <- knn(train=x_train, test=x_test, cl=knn_houses$Final_Bathrooms, k=5)

#En últimas igualarlo al número de cuartos
#Los que faltan, vamos a suponer que tienen el mismo número de baños que de cuartos
train_sf$Final_Bathrooms <- ifelse(is.na(train_sf$Final_Bathrooms)== TRUE, train_sf$bedrooms, train_sf$Final_Bathrooms)
sum(is.na(train_sf$Final_Bathrooms))

#Completar los metros
train_sf$Final_Metros <- ifelse(is.na(train_sf$new_surface_def2) == TRUE, train_sf$metros_Def, train_sf$new_surface_def2)
sum(is.na(train_sf$Final_Metros))
train_sf%>%count(is.na(Final_Metros))


write_rds(train_sf, "conBaños.rds")
write_rds(train_sf, "conBañosyMetros.rds")
con_baños <- readRDS("conBaños.rds")


#Cargar base de datos
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning/BD-ML---PS3/data")
train_sf <- readRDS("conBañosyMetros.rds")

#Cargar manzanas
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning")
mnz <- st_read("MGN_URB_MANZANA.shp") #%>%dplyr::select(MANZ_CCNCT)
mnz2 <- mnz%>%subset(COD_MPIO == "05001" | COD_MPIO == "11001")
mnz_bog <- mnz%>%subset(COD_MPIO == "11001")


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

#Vecinos cercanos
house_sp <- house_def%>%subset(city == "Bogotá D.C") %>% st_buffer(200) %>% as_Spatial() # poligonos

## obtener vecinos
house_nb <- poly2nb(pl=house_sp , queen=T) # opcion reina

#Crear variables
house_def$mean_H_NRO_CUARTOS2 <- NA
house_def$med_H_NRO_CUARTOS2 <- NA
house_def$sum_HA_TOT_PER2 <- NA
house_def$med_V_TOT_HOG2 <- NA
house_def$mod_VA1_ESTRATO2 <- NA
house_def$mod_V_MAT_PARED2 <- NA
house_def$mod_V_MAT_PISO2 <- NA
house_def$mod_VE_RECBAS2 <- NA

for (i in 1:nrow(house_def)) {
  house_def$mean_H_NRO_CUARTOS2[i] <- mean(house_def$mean_H_NRO_CUARTOS[house_nb[[i]]],na.rm=TRUE)
  house_def$med_H_NRO_CUARTOS2[i] <- mean(house_def$med_H_NRO_CUARTOS[house_nb[[i]]],na.rm=TRUE)
  house_def$sum_HA_TOT_PER2[i] <- mean(house_def$sum_HA_TOT_PER2[house_nb[[i]]],na.rm=TRUE)
  house_def$med_V_TOT_HOG2[i] <- mean(house_def$med_V_TOT_HOG[house_nb[[i]]],na.rm=TRUE)
  house_def$mod_VA1_ESTRATO2[i] <- moda(house_def$mod_VA1_ESTRATO[house_nb[[i]]])
  house_def$mod_V_MAT_PARED2[i] <- moda(house_def$mod_V_MAT_PARED[house_nb[[i]]])
  house_def$mod_V_MAT_PISO2[i] <- moda(house_def$mod_V_MAT_PISO[house_nb[[i]]])
  house_def$mod_VE_RECBAS2[i] <- moda(house_def$mod_VE_RECBAS[house_nb[[i]]])
}


train_sf$bathrooms_Def <- NA
train_sf$metros_Def <- NA
train_sf$metros_Def2 <- NA
for (i in 1:nrow(train_sf)) {
  train_sf$bathrooms_Def[i] <- mean(train_sf$bathrooms[train_nb[[i]]],na.rm=TRUE)
  temporal <- as.data.frame(train_sf[train_nb[[i]],])
  train_sf$metros_Def [i] <- mean(temporal$new_surface_def2[which(temporal$bedrooms == train_sf$bedrooms[i])], na.rm = TRUE)
  train_sf$metros_Def2[i] <- mean(train_sf$new_surface_def2[train_nb[[i]]],na.rm=TRUE)
}





#Para sacar la moda intentar con esto:
browseURL("https://stackoverflow.com/questions/30385626/how-to-get-the-mode-of-a-group-in-summarize-in-r")

#Toca: 1. hacer el collapse, juntar el censo con las manzanas, juntar las manzanas y demás variables con la base original

## censo data
browseURL("https://microdatos.dane.gov.co//catalog/643/get_microdata")









### Pruebas con pocos datos

set.seed(1000)
aleatorio <- sample(1:nrow(train_sf),200)
probando <- train_sf[aleatorio,]
probando_sp <- probando %>% st_buffer(1000) %>% as_Spatial() 
## obtener vecinos
train_nb <- poly2nb(pl=probando_sp , queen=T) # opcion reina

probando$metros_Def <- NA
probando$metros_Def2 <- NA
for (i in 1:nrow(probando)) {
  temporal <- as.data.frame(probando[train_nb[[i]],])
  probando$metros_Def [i] <- mean(temporal$surface_covered[which(temporal$bedrooms == probando$bedrooms[i])], na.rm = TRUE)
  probando$metros_Def2[i] <- mean(probando$surface_covered[train_nb[[i]]],na.rm=T)
}

View(probando%>%dplyr::select(surface_covered, metros_Def, metros_Def2, description))

prueba <- probando%>%dplyr::select(surface_covered, metros_Def, metros_Def2, description)%>%subset(is.na(surface_covered) == FALSE & is.na(metros_Def) == FALSE & is.na(metros_Def2) == FALSE)

mean((prueba$surface_covered-prueba$metros_Def)^2)
mean((prueba$surface_covered-prueba$metros_Def2)^2)


probando$bedrooms[32]
probando$bedrooms[train_nb[[32]]]
probando$surface_covered[train_nb[[32]]]
temporal <- as.data.frame(probando[train_nb[[32]],])
mean(temporal$surface_covered[which(temporal$bedrooms == probando$bedrooms[32])], na.rm = TRUE)
