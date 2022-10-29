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
x1 <- "[:digit:]+[:space:]+"
x2 <- "[:digit:]+[:punct:]+[:digit:]+[:space:]+"
x3 <- "[:word:]+[:space:]+"
#Se crea la variable y se llena con NA's que luego se reemplazan con los valores correctos
train$baños <- NA


## replace values
for (i in c("baño","bano", "bao")){
  train <- train %>% 
    mutate(baños = ifelse(is.na(baños)==T,str_extract(string=description , pattern=paste0(x1,i)),baños),
           baños = ifelse(is.na(baños)==T,str_extract(string=description , pattern=paste0(x2,i)),baños),
           baños = ifelse(is.na(baños)==T,str_extract(string=description , pattern=paste0(x3,i)),baños))
}



## clean var
for (i in c("baño","bano", "bao"," ","\n\n")){
  train$baños <- gsub(i,"",train$baños)
}


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


## cargar manzanas
mnz <- st_read("mgn/MGN_URB_MANZANA.shp")%>%dplyr::select(MANZ_CCNCT)

#Convertir datafram a sf
train_sf <- st_as_sf(x = train, ## datos
                     coords=c("lon","lat"), ## coordenadas
                     crs=4326) ## CRS

## unir dos conjuntos de datos basados en la geometría
house <- st_join(x=train_sf, y=mnz)

#Quitar los NA's de las manzanas
house <- st_join(x=house , y=mnz , join=st_nn , maxdist=20 , k=1 , progress=F)


