#####Código que ordena el regex y vecinos cercanos de la base de train
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
#Crear la base de train con los datos de Bogotá y Medellín
train <- readRDS("train.Rds")

#Contar NA's en las variables del test
sapply(train, function(y) sum(length(which(is.na(y)))))

#Como primer paso se lleva toda la descripción a "minúscula sostenida"
train$description <- tolower(train$description)


#################1mer filtro: utilizar regex

#Patrones
x1 <- "[:digit:]+[:punct:]+[:digit:]+[:space:]+"
x2 <- "[:digit:]+[:punct:]+[:digit:]+"
x3 <- "[:digit:]+[:space:]+"
x4 <- "[:digit:]+"
x5 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+"
x6 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+"
x7 <- "[:space:]+[:digit:]+[:space:]+"
x8 <- "[:space:]+[:digit:]+"
x9 <- "[:word:]+[:space:]+"
x10 <- "[:word:]+"

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

#Creamos una variable definitiva con las 2 anteriores
train$baños_def <- train$baños2
train$baños_def <- ifelse(is.na(train$baños_def)==TRUE, train$baños3, train$baños_def)
#Se arman los baños juntando originales con regex
train$baños_def2 <- ifelse(is.na(train$bathrooms)==TRUE, train$baños_def, train$bathrooms)

is.na(train$baños_def2)%>%table()

#Se utiliza knn para aquellos sin valores originales (esto se debe hacer antes de la transformación a sf)
knn_houses <- train%>%dplyr::select(bedrooms, lat, lon, bathrooms)%>%subset(is.na(bathrooms) == FALSE)
knn_houses2 <- train%>%subset(is.na(bathrooms) == FALSE)%>%dplyr::select(bedrooms, lat, lon)
x_train <- scale(knn_houses2[,1:3]) 
knn_houses_test <- train%>%subset(is.na(bathrooms) == TRUE)%>%dplyr::select(bedrooms, lat, lon)
x_test <- scale(knn_houses_test[,1:3]) 
nearest5 <- knn(train=x_train, test=x_test, cl=knn_houses$bathrooms, k=5)

#Con esto se va a llenar solo aquellos que al final queden definitivamente en NA
contador <- 0
train$bathrooms_knn <- NA
for (i in 1:nrow(train)) {
  if (is.na(train$bathrooms[i]) == TRUE) {
    contador <- contador + 1
    train$bathrooms_knn[i] <- as.numeric(nearest5[contador])
  }
}


#Se replica el proceso de regex para los metros
#Se crea la variable y se llena con NA's que luego se reemplazan con los valores correctos
train$new_surface <- NA

for (i in c("m2","mt2", "m ", "metros","cuadrad","mtr", "mts",  "metrs", "meters","area", "área","espacio de")){
  train <- train %>% 
    mutate(new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x1,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x2,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x3,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x4,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x5,i)),new_surface),
    )
}

## clean var
for (i in c("m2","mt2", "m ", "metros","cuadrad","mtr", "mts", "metrs", "meters","area", "área","espacio de")){
  train$new_surface <- gsub(i,"",train$new_surface)
}

#Si algo quedó con , se reemplaza por punto para convertirlo así a numérico
train$new_surface2 <- as.numeric(gsub(pattern = ",", replacement = ".", x = train$new_surface))

#Note que los que son chiquiticos la gran mayoría están divididos por mil porque usan el punto de separador de miles
train$new_surface2[which(train$new_surface2 < 5)] <- 1000*train$new_surface2[which(train$new_surface2 < 5)]

#Se utiliza el surface total, pero si no tiene dato se llena con surface covered
train$new_surface_def <- ifelse(is.na(train$surface_total),train$surface_covered,train$surface_total)
is.na(train$new_surface_def)%>%table() #34600 NA


#Ahora, los que queden vacío ahí se llenan con la del regex
train$new_surface_def2 <- ifelse(is.na(train$new_surface_def),train$new_surface2,train$new_surface_def)
is.na(train$new_surface_def2)%>%table() #19360 NA

#########################2do filtro: vecinos espaciales

#Convertir dataframe a sf
train_sf <- st_as_sf(x = train, ## datos
                    coords=c("lon","lat"), ## coordenadas
                    crs=4326) ## CRS

#Armado de buffers de 200 metros a la redonda
train_sp <- train_sf %>% st_buffer(200) %>% as_Spatial() # poligonos

## obtener vecinos
train_nb <- poly2nb(pl=train_sp , queen=T) # opcion reina

#Crear variables
train_sf$bathrooms_Def <- NA
train_sf$metros_Def <- NA
train_sf$metros_Def2 <- NA
for (i in 1:nrow(train_sf)) {
  train_sf$bathrooms_Def[i] <- mean(train_sf$baños_def2[train_nb[[i]]],na.rm=TRUE)
  temporal <- as.data.frame(train_sf[train_nb[[i]],])
  train_sf$metros_Def [i] <- mean(temporal$new_surface_def2[which(temporal$bedrooms == train_sf$bedrooms[i])], na.rm = TRUE)
  train_sf$metros_Def2[i] <- mean(train_sf$new_surface_def2[train_nb[[i]]],na.rm=TRUE)
}

#Revisión de NA
is.na(train_sf$bathrooms_Def)%>%table()
is.na(train_sf$metros_Def)%>%table()
is.na(train_sf$metros_Def2)%>%table()

#Llenar los baños
train_sf$Final_Bathrooms <- ifelse(is.na(train_sf$bathrooms) == TRUE, ifelse(is.na(train_sf$baños_def2) == TRUE, train_sf$bathrooms_Def, train_sf$baños_def2), train_sf$bathrooms)
sum(is.na(train_sf$Final_Bathrooms))

#Completar los metros
train_sf$Final_Metros <- ifelse(is.na(train_sf$new_surface_def2) == TRUE, train_sf$metros_Def, train_sf$new_surface_def2)
sum(is.na(train_sf$Final_Metros))

#Exportar la base 
write_rds(train_sf, "BogMed_sf.rds")


#Polígonos de los mapas
#Medellín
Medallo <- getbb(place_name = "Medellín, Colombia", 
                 featuretype = "boundary:administrative", 
                 format_out = "sf_polygon") 

#Graficado
leaflet() %>%
  addTiles() %>%
  addPolygons(data=Medallo)

#Exportar
write_rds(Medallo, "MedellínPoligono.rds")

#Bogotá
Bog <- getbb(place_name = "Bogotá Colombia", 
             featuretype = "boundary:administrative", 
             format_out = "sf_polygon") %>% .$multipolygon
#Graficado
leaflet() %>%
  addTiles() %>%
  addPolygons(data=Bog)

#Exportar
write_rds(Bog, "BogotáPoligono.rds")


#Cali
Cali <- getbb(place_name = "Cali, Colombia", 
                 featuretype = "boundary:administrative", 
                 format_out = "sf_polygon") 

#Graficado
leaflet() %>%
  addTiles() %>%
  addPolygons(data=Cali)

#Exportar
write_rds(Cali, "CaliPoligono.rds")

################Para obtener divisiones dentro de las ciudades

#Bogotá
## get Bogota-UPZ 
bog <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key="boundary", value="administrative") %>% 
  osmdata_sf()
bog <- bog$osm_multipolygons %>% subset(admin_level==9)

## basic plot
ggplot() + geom_sf(data=bog)

#Exportar
write_rds(bog, "BogotáUPZ.rds")

## get Medellín 
med <- opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="boundary", value="administrative") %>% 
  osmdata_sf()
med <- med$osm_multipolygons %>% subset(admin_level==8)

## basic plot
ggplot() + geom_sf(data=med)

#Exportar
write_rds(med, "MedellínComunas.rds")

## get Cali
cal <- opq(bbox = getbb("Cali Colombia")) %>%
  add_osm_feature(key="boundary", value="administrative") %>% 
  osmdata_sf()
cal <- cal$osm_multipolygons %>% subset(admin_level==8)

#Exportar
write_rds(cal, "CaliComunas.rds")

## basic plot
ggplot() + geom_sf(data=cal)




###############Pruebas más desagregadas
## get Medellín 
med9 <- opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="boundary", value="administrative") %>% 
  osmdata_sf()
med9 <- med9$osm_multipolygons %>% subset(admin_level==9)

## basic plot
ggplot() + geom_sf(data = med) + geom_sf(data=med9)

#Exportar
write_rds(med9, "MedellínBarrios.rds")


## get Cali
cal9 <- opq(bbox = getbb("Cali Colombia")) %>%
  add_osm_feature(key="boundary", value="administrative") %>% 
  osmdata_sf()
cal9 <- cal9$osm_multipolygons %>% subset(admin_level==9)

## basic plot
ggplot() + geom_sf(data=cal9)

#Exportar
write_rds(cal9, "CaliBarrios.rds")

#Bogotá localidades
## get Bogota
bog8 <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key="boundary", value="administrative") %>% 
  osmdata_sf()
bog8 <- bog8$osm_multipolygons %>% subset(admin_level==8)

## basic plot
ggplot() + geom_sf(data=bog8)

#Exportar
write_rds(bog8, "BogotáLocalidades.rds")


#Bogotá barrios (Sale un mapa de Colombia muy raro)
bog10 <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key="boundary", value="administrative") %>% 
  osmdata_sf()
bog10 <- bog10$osm_multipolygons %>% subset(admin_level=10)

## basic plot
ggplot() + geom_sf(data=bog10)