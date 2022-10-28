
# limpiar ambiente
rm(list=ls())


# instalar paquetes
install.packages("packages")

require("pacman")

## llamar y/o instalar librerias
p_load(tidyverse,rio,skimr,
       sf, ## datos espaciales
       leaflet, ## visualizaciones
       tmaptools, ## geocodificar
       ggsn, ## map scale bar 
       osmdata,dplyr) ## packages with census data



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

## obtener la caja de coordenada que contiene el polígono de Cali
opq(bbox = getbb("Cali Colombia"))

## obtener la caja de coordenada que contiene el polígono de Bogotá, DC
opq(bbox = getbb("Bogotá Colombia"))

## obtener la caja de coordenada que contiene el polígono de Medellín
opq(bbox = getbb("Medellín Colombia"))



# crear lista de amenities

amenities <- available_tags("amenity")
#Creamos una lista con los ammenities que a priori consideramos más importantes
lista_amenities <- c("bank","restaurant", "bus_station", "police", "bank", "cafe", "casino", "childcare", "cinema",
                     "clinic", "college",  "bicycle_parking", "community_centre", "conference_centre", "dentist", "doctors",
                     "events_venue", "fast_food", "hospital",
                     "kindergarten", "library", "love_hotel", "marketplace", "monastery",
                     "parking", "pharmacy", "place_of_worship",  "post_office",
                     "pub", "recycling", "shelter", "social_facility",
                     "theatre", "veterinary", "school", "university", "arts_centre")



#Crear objetos OSM para cada amenitie y extraer los Simple Features tipo Poligonos
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

  ## extraer Simple Features Collection
  osm_sf = osm %>% osmdata_sf()
  osm_sf
  
  ## Obtener un objeto sf
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

## Obtener un objeto sf
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

## convetir DataFrame de train-set a sf
train_house <- st_as_sf(x = train, ## datos
                   coords=c("lon","lat"), ## coordenadas
                   crs=4326) ## CRS

class(train_house)

# graficar casas de train-set
leaflet() %>% addTiles() %>% addCircleMarkers(data=train_house[1:10,])


### **5.2 help:** `sf` 

## Help
vignette("sf3")
vignette("sf4")

### **5.3 Afine transformations**
st_crs(train_house) == st_crs(parques) 



### **5.4 Filtrar datos**
## usando los valores de una variable



# grafico casas Bogota

# opcion 1: grafico general de casas de Bogota con filtro de variable city
# casas de Bogota
houses_bogota <- train_house %>% subset(city= "Bogotá D.C")

leaflet() %>% addTiles() %>% addCircles(data=houses_bogota)

# casas de Medellín
houses_medellin <- train_house %>% subset(city= "Medellín")

leaflet() %>% addTiles() %>% addCircles(data=houses_medellin)





### Usar la geometría de la ciudad

## crear sf_polygon de las ciudades
# crear sf_polygon de la ciudad Bogota
bogota_city <- getbb(place_name = "Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon

# graficar sf_polygon de la ciudad de Bogota sf_polygon
leaflet() %>% addTiles() %>% addPolygons(data=bogota_city)


# crear sf_polygon de la ciudad Medellin
medellin_city <- getbb(place_name = "Medellín", 
                     featuretype = "boundary:administrative", 
                     format_out = "sf_polygon") %>% .$multipolygon

# graficar sf_polygon de la ciudad de Bogota sf_polygon
leaflet() %>% addTiles() %>% addPolygons(data=medellin_city)



## crear interseccion entre x base de casas sf, y sf_polygon   
house_chapi <- st_intersection(x = houses_bogota , y = bogota_city)
# graficar sf_polygon de la interseccion entre ambos sf
leaflet() %>% addTiles() %>% addPolygons(data=bogota_city,col="red") %>% addCircles(data=house_chapi)




## crop puntos con poligono (opcion 3)
#house_chapi <- houses[chapinero,]
#leaflet() %>% addTiles() %>% addPolygons(data=chapinero,col="red") %>% addCircles(data=house_chapi)




### **5.5. Distancia a amenities**
## Calcular Distancia a muchos polygonos

## Distancia a amenities Bogota


# arts_centre_bog
matrix_dist_arts_centre_bog <- st_distance(x=house_chapi , y=osm_arts_centre_Bogota_sf)

#matrix_dist_parque[1:5,1:5]

min_dist_arts_centre_bog <- apply(matrix_dist_arts_centre_bog , 1 , min)

min_dist_arts_centre_bog %>% head()

house_chapi$arts_centre_bog = min_dist_arts_centre_bog




## Distancia a amenities Medellín



