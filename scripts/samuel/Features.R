rm(list=ls())

#Samuel
setwd("~/Desktop/Big Data/Repositorios/BD-ML---PS3/scripts/samuel/data")

require(pacman) 
p_load(tidyverse,rio,skimr,viridis,
       gstat, ## variogram
       sf, ## leer/escribir/manipular datos espaciales
       leaflet, ## Visualizaciones dinámicas
       nngeo, ## st_nn function
       spdep, ## Construct neighbours list from polygon list
       tmaptools, ## geocodificar
       ggsn, ## map scale bar 
       osmdata) ## Get OSM's data

train <- read_rds("train.Rds")
test <- readRDS("test.Rds")

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

available_features("kindergarten")

lista_amenities2 <- c("kindergarten", "library", "love_hotel", "marketplace", "monastery",
                      "parking", "pharmacy", "place_of_worship",  "post_office",
                      "pub", "recycling", "shelter", "social_facility",
                      "theatre", "veterinary", "school", "university", "arts_centre")

# para cada amenitie
# crear objetos OSM para cada amenitie
# extraer los Simple Features 
# tipo de dato geometrico polygons

#El loop crea un objeto OSM para cada amenitie y extrae los Simple Features
for (amenitie in lista_amenities2) {
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


assign(paste("osm", "busway","Bogota" ,"sf", sep = "_"), opq(bbox = getbb("Bogotá Colombia")) %>%
         add_osm_feature(key="highway" , value= "busway") %>% osmdata_sf() %>% 
         .$osm_polygons)

## objeto osm
osm = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="highway" , value="road") 
class(osm)


## extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf

## crear un objeto sf
lanes = osm_sf$osm_lines
lanes

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
bus_station = osm_sf$osm_points %>% select(osm_id,highway) 
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





















# variables disponibles
available_features()

# keys disponibles
available_tags("highway")

# Bogotá

osm = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="highway" , value="road") 
class(osm)

osm1 = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="lanes:bus") 
class(osm)

osm2 = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="financial_advisor") 
class(osm)



# Extraer sf
osm_sf = osm %>% osmdata_sf()
osm_sf

osm_sf2 = osm1 %>% osmdata_sf()
osm_sf2

osm_sf2 = osm2 %>% osmdata_sf()
osm_sf2

## Obtener un objeto sf
roads = osm_sf$osm_lines
roads

## Pintar las estaciones de autobus
leaflet() %>% addTiles() %>% addCircleMarkers(data=supermarket , col="red")



# Intento semivariograma

#Convertir datafram a sf
train_sf <- st_as_sf(x = train, ## datos
                     coords=c("lon","lat"), ## coordenadas
                     crs=4326) ## CRS

## sf to sp
house_sp <- train_sf %>% as_Spatial()
house_sp

## price
variogram(price/1000000 ~ 1, house_sp, cloud = F , cressie=T) %>% plot()

## variable aleatoria
house_sp$normal <- rnorm(n = nrow(house_sp),
                         mean = mean(house_sp$price/1000000),
                         sd = 1000)

## variogramas
v_price = variogram(price/1000000 ~ 1, house_sp, cloud = F , cressie=T) %>% mutate(estimate=gamma) %>% select(dist,estimate)
v_ramdon = variogram(normal ~ 1, house_sp, cloud = F , cressie=T) %>% mutate(normal=gamma) %>% select(dist,normal)

## join db
db_plot = left_join(x=v_price , y=v_ramdon ,"dist")
db_plot %>% head()

## plot
ggplot(db_plot) + 
  geom_point(aes(x=dist, y=normal , fill="Datos aleatorios (Dist. Normal)"), shape=21, alpha=0.5, size=5 ) +
  geom_point(aes(x=dist, y=estimate , fill="Precio de la vivienda (properati)"), shape=21, alpha=0.5, size=5 ) +
  labs(caption = "Fuente: Properati", y = "Semivariograma", x = "Distancia de separación entre inmuebles", fill = "") + theme_test()


### Distancia a centro internacional y vías principales

# arts_centre_bog
matrix_dist_arts_centre_bog <- st_distance(x=houses_bogota  , y=osm_arts_centre_Bogota_sf)

#matrix_dist_parque[1:5,1:5]

min_dist_arts_centre_bog <- apply(matrix_dist_arts_centre_bog , 1 , min)

min_dist_arts_centre_bog %>% head()

house_chapi$arts_centre_bog = min_dist_arts_centre_bog
