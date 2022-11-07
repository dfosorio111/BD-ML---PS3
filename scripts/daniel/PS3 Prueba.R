###Limpiar 
rm(list = ls())
cat("\014")

## llamar pacman (contiene la función p_load)
require(pacman)

## llamar y/o instalar librerias
p_load(tidyverse,rio,skimr,
       sf, ## datos espaciales
       leaflet, ## visualizaciones
       tmaptools, ## geocodificar
       ggsn, ## map scale bar 
       osmdata,
       class,
       MASS) ## packages with census data

#Establacer directorios
#Directorio Daniel
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning/BD-ML---PS3/data")
#Crear la base de test
test <- readRDS("test.Rds")
#Crear la base de train
train <- readRDS("train.RDS")

#Ciudades en el test
test%>%count(city)
#Ciudades en el train
train%>%count(city)

#Ver Na's
sapply(train, function(y) sum(length(which(is.na(y)))))
sapply(test, function(y) sum(length(which(is.na(y)))))

train%>%count(operation_type) #Todos son de venta

#Intento 1 (Con mt2)
train$mt2 <- gsub("([0-9]+) mt2.*$", "\\1", train$description)
train$mt2 <- ifelse(nchar(train$mt2) == nchar(train$description), NA, substr(train$mt2,nchar(train$mt2)-5,nchar(train$mt2)))
train$mt2 <- as.numeric(gsub(".* ([0-9]+).*$", "\\1", train$mt2))

#(Con metros)
train$metros <- gsub("([0-9]+) metros.*$", "\\1", train$description)
train$metros <- ifelse(nchar(train$metros) == nchar(train$description), NA, substr(train$metros,nchar(train$metros)-5,nchar(train$metros)))
train$metros <- as.numeric(gsub(".* ([0-9]+).*$", "\\1", train$metros))

#Intento 1 (Con m2)
train$m2 <- gsub("([0-9]+) m2.*$", "\\1", train$description)
train$m2 <- ifelse(nchar(train$m2) == nchar(train$description), NA, substr(train$m2,nchar(train$m2)-5,nchar(train$m2)))
train$m2 <- as.numeric(gsub(".* ([0-9]+).*$", "\\1", train$m2))

#(Con metros cuadrados)
train$metros_cuad <- gsub("([0-9]+) metros cuadrados.*$", "\\1", train$description)
train$metros_cuad <- ifelse(nchar(train$metros_cuad) == nchar(train$description), NA, substr(train$metros_cuad,nchar(train$metros_cuad)-5,nchar(train$metros_cuad)))
train$metros_cuad <- as.numeric(gsub(".* ([0-9]+).*$", "\\1", train$metros_cuad))

#Toca ver cuáles más hay y seguir así para completar lo máximo posible

#Una opción que puede servir para los baños (Intento con las que queden cerca en longitud y latitud y tengan número de cuartos similar)

x <- scale(fgl[,1:9]) # column 10 is the class label
apply(x,2,sd) # see ?apply

#Como en clase
names(train)
knn_houses <- train%>%dplyr::select(bedrooms, lat, lon, bathrooms)%>%subset(is.na(bathrooms) == FALSE)
knn_houses2 <- train%>%subset(is.na(bathrooms) == FALSE)%>%dplyr::select(bedrooms, lat, lon)
x <- scale(knn_houses2[,1:3]) 
apply(x,2,sd) 
set.seed(1010101)
test <- sample(1:nrow(knn),0.2*nrow(knn))
knn_houses$bathrooms <- as.factor(knn_houses$bathrooms)
nearest5 <- knn(train=x[-test,], test=x[test,], cl=knn_houses$bathrooms[-test], k=5)

#Prueba para predecir en los que faltan
knn_houses <- train%>%dplyr::select(bedrooms, lat, lon, bathrooms)%>%subset(is.na(bathrooms) == FALSE)
knn_houses2 <- train%>%subset(is.na(bathrooms) == FALSE)%>%dplyr::select(bedrooms, lat, lon)
x_train <- scale(knn_houses2[,1:3]) 
knn_houses_test <- train%>%subset(is.na(bathrooms) == TRUE)%>%dplyr::select(bedrooms, lat, lon)
x_test <- scale(knn_houses_test[,1:3]) 
nearest5 <- knn(train=x, test=x_test, cl=knn_houses$bathrooms, k=5)

contador <- 0
for (i in 1:nrow(train)) {
  if (is.na(train$bathrooms[i]) == TRUE) {
    contador <- contador + 1
    train$bathrooms[i] <- as.numeric(nearest5[contador])
  }
}


#Convertir datafram a sf
train_sf <- st_as_sf(x = train, ## datos
                     coords=c("lon","lat"), ## coordenadas
                     crs=4326) ## CRS

## obtener la caja de coordenada que contiene el polígono de Cali
opq(bbox = getbb("Cali Colombia"))

## objeto osm
available_tags("amenity")
#Creamos una lista con los ammenities que a priori consideramos más importantes
lista_amenities <- c("restaurant", "police", "school", "theatre", "university", "arts_centre", "bank", "bicycle_parking",
                     "bus_station","cafe", "casino", "childcare", "cinema",
                     "clinic", "college", "community_centre", "conference_centre", "dentist", "doctors",
                     "events_venue", "fast_food", "hospital",
                     "kindergarten", "library", "love_hotel", "marketplace", "monastery",
                     "parking", "pharmacy", "place_of_worship", "post_office",
                     "pub", "recycling",
                     "shelter", "social_facility",
                     "veterinary")

lista_amenities <- c("bank", "restaurant")


#El loop crea un objeto osm para cada amenitie y extrae los simple features
for (amenitie in lista_amenities) {
  #Crear objetos osm
  #assign(paste("osm", amenitie, sep = "_"), opq(bbox = getbb("Cali Colombia")) %>%
   #        add_osm_feature(key="amenity" , value= amenitie))
  ## extraer Simple Features Collection (creando objetos sf)
  #assign(paste("osm", amenitie, "sf", sep = "_"), opq(bbox = getbb("Cali Colombia")) %>%
   #        add_osm_feature(key="amenity" , value= amenitie) %>% osmdata_sf())
  assign(paste("osm", amenitie, "Bogota", "sf", sep = "_"), opq(bbox = getbb("Bogotá Colombia")) %>%
           add_osm_feature(key="amenity" , value= amenitie) %>% osmdata_sf() %>% 
           .$osm_polygons)
  assign(paste("osm", amenitie, "Medellin", "sf", sep = "_"), opq(bbox = getbb("Medellín Colombia")) %>%
           add_osm_feature(key="amenity" , value= amenitie) %>% osmdata_sf() %>% 
           .$osm_polygons)
}

#Parece que tocará hacerlo manual (1 por 1, a veces funciona, a veces no)



#Gráficos de las casas de Bogotá
housesBogota <- train_sf %>% subset(city=="Bogotá D.C")
leaflet() %>% addTiles() %>% addCircles(data=housesBogota)

#Paso siguiente para que queden
Bog <- getbb(place_name = "Bogotá Colombia", 
             featuretype = "boundary:administrative", 
             format_out = "sf_polygon") %>% .$multipolygon

leaflet() %>% addTiles() %>% addPolygons(data=Bog)

house_Bog <- st_intersection(x = housesBogota , y = Bog)

leaflet() %>% addTiles() %>% addPolygons(data=Bog,col="red") %>% addCircles(data=house_Bog)


## Distancia a muchos polígonos

#Se crea la matriz de distancias con los bancos
matrix_dist_banco <- st_distance(x=house_Bog , y= osm_bank_Bogota_sf)
min_dist_banco <- apply(matrix_dist_banco, 1 , min)
min_dist_banco %>% head()
house_Bog$dist_banco <- min_dist_banco

#Se crea la matriz de distancias con las estaciones de bus
matrix_dist_bus <- st_distance(x=house_Bog , y= osm_bus_station_Bogota_sf)
min_dist_bus <- apply(matrix_dist_bus, 1 , min)
min_dist_bus %>% head()
house_Bog$dist_bus <- min_dist_bus

#Sería útil ver en qué barrio quedan las casas, porque Ignacio dice que no solo importa estar cerca a algún 
#amenitie, sino que la "interacción" con la zona importa: como el ejemplo de los parques de Chicago
#Estar cerca al transmi es bueno, pero si es cerca a una estación peligrosa, pues es peor.


#Gráficos de las casas de medellín
housesMedallo <- train_sf %>% subset(city=="Medellín")
leaflet() %>% addTiles() %>% addCircles(data=housesMedallo)

#Paso siguiente para que queden
Med <- getbb(place_name = "Medellín Colombia", 
             featuretype = "boundary:administrative", 
             format_out = "sf_polygon") %>% .$multipolygon

leaflet() %>% addTiles() %>% addPolygons(data=Med)

house_Bog <- st_intersection(x = housesBogota , y = Bog)

leaflet() %>% addTiles() %>% addPolygons(data=Bog,col="red") %>% addCircles(data=house_Bog)