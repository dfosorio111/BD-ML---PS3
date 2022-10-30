
# limpiar ambiente
rm(list=ls())


# instalar paquetes
install.packages("packages")

require("pacman")

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



# set wd
setwd("C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS3/scripts/diego/data")


# cargar bases de entrenamiento y prueba
train <- readRDS("train.Rds")
test <- readRDS("test.Rds")

# variables disponibles
available_features()%>%head(20)

# keys disponibles
available_tags("amenity")



### **4.4. Descargar features**

## crear la caja de coordenada que contiene el polígono de Cali
cali <- opq(bbox = getbb("Cali Colombia"))

## crear la caja de coordenada que contiene el polígono de Bogotá, DC
bog <- opq(bbox = getbb("Bogotá Colombia"))

## crear la caja de coordenada que contiene el polígono de Medellín
med <- opq(bbox = getbb("Medellín Colombia"))



# crear lista de amenities

amenities <- available_tags("amenity")
# Creamos una lista con los ammenities que a priori consideramos más importantes
# para predecir el precio de las casas
lista_amenities <- c("restaurant", "cafe ", "bank", "bus_station", "police", "casino", "childcare", "cinema",
                     "clinic", "college",  "bicycle_parking", "community_centre", "conference_centre", "dentist", "doctors",
                     "events_venue", "fast_food", "hospital",
                     "kindergarten", "library", "love_hotel", "marketplace", "monastery",
                     "parking", "pharmacy", "place_of_worship",  "post_office",
                     "pub", "recycling", "shelter", "social_facility",
                     "theatre", "veterinary", "school", "university", "arts_centre")


lista_amenities2 <- c("bank", "bus_station", "police", "casino", "childcare", "cinema",
                     "clinic", "college",  "bicycle_parking", "community_centre", "conference_centre", "dentist", "doctors",
                     "events_venue", "fast_food", "hospital",
                     "kindergarten", "library", "love_hotel", "marketplace", "monastery",
                     "parking", "pharmacy", "place_of_worship",  "post_office",
                     "pub", "recycling", "shelter", "social_facility",
                     "theatre", "veterinary", "school", "university", "arts_centre")


# para cada amenitie
# crear objetos OSM para cada amenitie
# extraer los Simple Features 
# tipo de dato geometrico polygons


#El loop crea un objeto OSM para cada amenitie y extrae los Simple Features
for (amenitie in lista_amenities) {
  #Crear objetos osm
  #assign(paste("osm", amenitie, sep = "_"), opq(bbox = getbb("Cali Colombia")) %>%
  #        add_osm_feature(key="amenity" , value= amenitie))
  ## extraer Simple Features Collection (creando objetos sf)
  #assign(paste("osm", amenitie, "sf", sep = "_"), opq(bbox = getbb("Cali Colombia")) %>%
  #        add_osm_feature(key="amenity" , value= amenitie) %>% osmdata_sf())
  
  
  
  #assign(paste("osm", amenitie, "sf", sep = "_"), opq(bbox = getbb("Cali Colombia")) %>%
  #         add_osm_feature(key="amenity" , value= amenitie) %>% osmdata_sf() %>% 
  #         .$osm_polygons)
  
  assign(paste("osm", amenitie,"Bogota" ,"sf", sep = "_"), opq(bbox = getbb("Bogotá Colombia")) %>%
           add_osm_feature(key="amenity" , value= amenitie) %>% osmdata_sf() %>% 
           .$osm_polygons)
  
  assign(paste("osm", amenitie,"Medellin" ,"sf", sep = "_"), opq(bbox = getbb("Medellín Colombia")) %>%
           add_osm_feature(key="amenity" , value= amenitie) %>% osmdata_sf() %>% 
           .$osm_polygons)
  
}

### ESTO HACE EL LOOP PARA CADA AMENITIE


  ## extraer Simple Features Collection
  osm_sf = osm %>% osmdata_sf()
  osm_sf
  
  ## crear un objeto sf
  bus_station = osm_sf$osm_points %>% select(osm_id,amenity) 
  bus_station
  
  ## Pintar las estaciones de autobus
  #leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_station , col="red")


## objeto osm
osm = opq(bbox = getbb("Cali Colombia")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
class(osm)


## extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf

## crear un objeto sf
bus_station = osm_sf$osm_points %>% select(osm_id,amenity) 
bus_station

## Pintar las estaciones de autobus
leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_station , col="red")


## **[5.] Operaciones geometricas**

### **5.1 Importar conjuntos de datos**

## Inmuebles
#train_house <- train
#class(train_house)
#skim(train_house)

## convetir DataFrame de casas en datos de entrenamiento a sf
train_house <- st_as_sf(x = train, ## datos
                   coords=c("lon","lat"), ## coordenadas
                   crs=4326) ## CRS

class(train_house)

# graficar casas de datos de entrenamiento
leaflet() %>% addTiles() %>% addCircleMarkers(data=train_house) #[1:10,]


### **5.2 help:** `sf` 

## Help
vignette("sf3")
vignette("sf4")

### **5.3 Afine transformations**
st_crs(train_house) == st_crs(parques) 




### **5.4 Filtros de datos**

# grafico casas Bogota

# opcion 1: grafico general de casas de datos de entrenamiento en la ciudad de Bogota DC
# SIN FILTROS


# casas de Bogota
houses_bogota <- train_house %>% subset(city== "Bogotá D.C")

leaflet() %>% addTiles() %>% addCircles(data=houses_bogota)


# casas de Medellín
houses_medellin <- train_house %>% subset(city== "Medellín")

leaflet() %>% addTiles() %>% addCircles(data=houses_medellin)



### Usar la geometría de la ciudad

## crear  bb de las ciudades

# crear bb de la ciudad Bogota
# crear sf tipo de datos polygon de la ciudad de Bogota
bog_sf_polygon <- getbb(place_name = "Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon

# graficar sf_polygon de la ciudad de Bogota sf_polygon
leaflet() %>% addTiles() %>% addPolygons(data=bog_sf_polygon)


# crear sf_polygon de la ciudad Medellin
med_sf_polygon <- getbb(place_name = "Medellín", 
                     featuretype = "boundary:administrative", 
                     format_out = "sf_polygon") %>% .$multipolygon

# graficar sf_polygon de la ciudad de Medellin sf_polygon
leaflet() %>% addTiles() %>% addPolygons(data=med_sf_polygon)

# crear sf_polygon de la ciudad Cali
cali_sf_polygon <- getbb(place_name = "Cali", 
                        featuretype = "boundary:administrative", 
                        format_out = "sf_polygon") %>% .$multipolygon

# graficar sf_polygon de la ciudad de Bogota sf_polygon
leaflet() %>% addTiles() %>% addPolygons(data=cali_sf_polygon)



### Usar la geometría de UPZ de la Ciudad
## crear bb sf_polygon de UPZ's


# obtener bb de UPZ 
# featuretype = "boundary:administrative"
# format_out = sf polygon
bogota_sf <- getbb(place_name = "Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon

leaflet() %>% addTiles() %>% addPolygons(data=usaquen)


## crear interseccion entre x base de casas sf, y sf_polygon   
houses_inter <- st_intersection(x = bog_sf_polygon , y = bogota_sf)
# graficar sf_polygon de la interseccion entre ambos sf
leaflet() %>% addTiles() %>% addPolygons(data=bogota_sf,col="red") %>% addCircles(data=houses_inter)




## crop puntos con poligono (opcion 3)
#house_chapi <- houses[chapinero,]
#leaflet() %>% addTiles() %>% addPolygons(data=chapinero,col="red") %>% addCircles(data=house_chapi)




### **5.5. Distancia a amenities**
## Calcular Distancia a muchos polygonos

## Distancia a amenities Bogota

# arts_centre_bog
matrix_dist_arts_centre_bog <- st_distance(x=houses_bogota  , y=osm_arts_centre_Bogota_sf)

#matrix_dist_parque[1:5,1:5]

min_dist_arts_centre_bog <- apply(matrix_dist_arts_centre_bog , 1 , min)

min_dist_arts_centre_bog %>% head()

house_chapi$arts_centre_bog = min_dist_arts_centre_bog



# restaurants

# arts_centre_bog
matrix_dist_restaurant_bog <- st_distance(x=houses_bogota  , y=osm_arts_centre_Bogota_sf)

#matrix_dist_parque[1:5,1:5]

min_dist_arts_centre_bog <- apply(matrix_dist_arts_centre_bog , 1 , min)

min_dist_arts_centre_bog %>% head()

house_chapi$arts_centre_bog = min_dist_arts_centre_bog




### restaurants


set.seed(1000)

muestra <- sample(1:nrow(train_house), 100)
prueba <- train_house[muestra,]



base_aux <- bind_rows(prueba, osm_restaurant_Bogota_sf%>%select(osm_id,name,amenity))


base_aux_sp <- base_aux%>%st_buffer(100)%>%as_Spatial()



### Vecinos Espaciales

# obtener vecinos espaciales 

base_nb <- poly2nb(pl=base_aux_sp, queen= TRUE) #opcion reina


base_aux$num_res <- NA
base_aux$valor_res <- NA

#base_aux$res_final <- NA

for (i in 1:length(base_nb)) {
  base_aux$num_res[i] <- size_sum(base_nb[[i]])
  base_aux$valor_res[i] <- base_nb[[i]]==0
}

base_aux <- base_aux%>%mutate(res_final = ifelse(valor_res == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_res)))) 

#base_aux <- base_aux%>%subset(is.na(amenity) == TRUE)
drop <- c("osm_id", "name","amenity","num_res", "valor_res")
base_aux <- base_aux[,!names(base_aux)%in%drop]



### cafe

base_aux <- bind_rows(base_aux, osm_cafe_Bogota_sf%>%dplyr::select(osm_id,name,amenity))


base_aux_sp <- base_aux%>%st_buffer(100)%>%as_Spatial()



### Vecinos Espaciales

# obtener vecinos espaciales 

base_nb <- poly2nb(pl=base_aux_sp, queen= TRUE) #opcion reina


base_aux$num_cafe <- NA
base_aux$valor_cafe <- NA

#base_aux$res_final <- NA

for (i in 1:length(base_nb)) {
  base_aux$num_cafe[i] <- size_sum(base_nb[[i]])
  base_aux$valor_cafe[i] <- base_nb[[i]]==0
}

base_aux <- base_aux%>%mutate(cafe_final = ifelse(valor_cafe == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_cafe)))) 


#base_aux <- base_aux%>%subset(is.na(amenity) == TRUE)
drop <- c("osm_id", "name","amenity","num_cafe", "valor_cafe")
base_aux <- base_aux[,!names(base_aux)%in%drop]







### TODAS LAS AMENITIES

do.call(rbind.data.frame, mget(ls(pattern = "^osm\\s+\\")))
do.call(rbind.data.frame, mget(paste0("osm", seq_along(file_list), ".csv")))


base_amen <- do.call("rbind", lista_amenities2)

for (amenitie in lista_amenities2){
  
  #assign(paste("osm", amenitie,"Bogota" ,"sf", sep = "_"), opq(bbox = getbb("Bogotá Colombia")) %>%
  #         add_osm_feature(key="amenity" , value= amenitie) %>% osmdata_sf() %>% 
  #         .$osm_polygons)
  
  
  #base_amen <- do.call("rbind", paste("osm", amenitie,"Bogota" ,"sf", sep = "_") )
  base_amen <- bind_rows(base_aux, paste("osm", amenitie,"Bogota" ,"sf", sep = "_"))   #%>%dplyr::select(osm_id,name,amenity))
  
}






base_aux <- bind_rows(base_aux, osm_cafe_Bogota_sf%>%dplyr::select(osm_id,name,amenity))


base_aux_sp <- base_aux%>%st_buffer(100)%>%as_Spatial()



### Vecinos Espaciales

# obtener vecinos espaciales 

base_nb <- poly2nb(pl=base_aux_sp, queen= TRUE) #opcion reina


base_aux$num_cafe <- NA
base_aux$valor_cafe <- NA

#base_aux$res_final <- NA

for (i in 1:length(base_nb)) {
  base_aux$num_cafe[i] <- size_sum(base_nb[[i]])
  base_aux$valor_cafe[i] <- base_nb[[i]]==0
}

base_aux <- base_aux%>%mutate(cafe_final = ifelse(valor_cafe == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_cafe)))) 


#base_aux <- base_aux%>%subset(is.na(amenity) == TRUE)
drop <- c("osm_id", "name","amenity","num_cafe", "valor_cafe")
base_aux <- base_aux[,!names(base_aux)%in%drop]

















#for 
## vecinos del inmueble 32
nb_house[[32]]


## visualizar
leaflet() %>% addTiles() %>% 
  addCircles(data=new_house[nb_house[[32]],]) %>% 
  addCircles(data=new_house[32,],col="red")









## Distancia a amenities Medellín




### MANZANAS

# set wd
setwd("C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS3/data/mgn")


## cargar manzanas Bogota
mnz <- st_read("MGN_URB_MANZANA.shp")%>%dplyr::select(MANZ_CCNCT)

#Convertir datafram a sf
train_sf <- st_as_sf(x = train, ## datos
                     coords=c("lon","lat"), ## coordenadas
                     crs=4326) ## CRS

## unir dos conjuntos de datos basados en la geometría
house <- st_join(x=train_sf, y=mnz)

#Quitar los NA's de las manzanas
house <- st_join(x=house , y=mnz , join=st_nn , maxdist=20 , k=1 , progress=F)




### CENSO

# cargara censo de Bogota
censo <- import("mnz_censo_2018.rds")
censo








### VECINOS ESPACIALES




