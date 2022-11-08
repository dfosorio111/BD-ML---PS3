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
       nngeo, ## st_nn function
       spdep,
       osmdata, 
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
  
  
  assign(paste("osm", amenitie,"Bogota" ,"sf", sep = "_"), opq(bbox = getbb("Bogotá Colombia")) %>%
           add_osm_feature(key="amenity" , value= amenitie) %>% osmdata_sf() %>% 
           .$osm_polygons)
  
  assign(paste("osm", amenitie,"Medellin" ,"sf", sep = "_"), opq(bbox = getbb("Medellín Colombia")) %>%
           add_osm_feature(key="amenity" , value= amenitie) %>% osmdata_sf() %>% 
           .$osm_polygons)
  
  assign(paste("osm", amenitie,"Cali" ,"sf", sep = "_"), opq(bbox = getbb("Cali Colombia")) %>%
           add_osm_feature(key="amenity" , value= amenitie) %>% osmdata_sf() %>% 
           .$osm_polygons)
  
  
}


### GUARDAR LOS OSM_SF DE CADA AMENITY

write_rds(osm_arts_centre_Bogota_sf,'osm_arts_centre_Bogota_sf.RDS')
write_rds(osm_arts_centre_Medellin_sf,'osm_arts_centre_Medellin_sf.RDS')

write_rds(osm_bank_Bogota_sf,'osm_bank_Bogota_sf.RDS')
write_rds(osm_bank_Medellin_sf,'osm_bank_Medellin_sf.RDS')
write_rds(osm_bank_Cali_sf,'osm_bank_Cali_sf.RDS')

write_rds(osm_bicycle_parking_Bogota_sf,'osm_bicycle_parking_Bogota_sf.RDS')
write_rds(osm_bicycle_parking_Medellin_sf,'osm_bicycle_parking_Medellin_sf.RDS')
write_rds(osm_bicycle_parking_Medellin_sf,'osm_bicycle_parking_Medellin_sf.RDS')

write_rds(osm_bus_station_Bogota_sf,'osm_bus_station_Bogota_sf.RDS')
write_rds(osm_bus_station_Medellin_sf,'osm_bus_station_Medellin_sf.RDS')
write_rds(osm_bus_station_Cali_sf,'osm_bus_station_Cali_sf.RDS')

### LAS OBSERVACIONES DE AMENITIES DE CAFE SON 0



###
write_rds(osm_casino_Bogota_sf,'osm_casino_Bogota_sf.RDS')
write_rds(osm_casino_Medellin_sf,'osm_casino_Medellin_sf.RDS')
write_rds(osm_casino_Cali_sf,'osm_casino_Cali_sf.RDS')

write_rds(osm_childcare_Bogota_sf,'osm_childcare_Bogota_sf.RDS')
write_rds(osm_childcare_Medellin_sf,'osm_childcare_Medellin_sf.RDS')
write_rds(osm_childcare_Cali_sf,'osm_childcare_Cali_sf.RDS')

write_rds(osm_cinema_Bogota_sf,'osm_cinema_Bogota_sf.RDS')
write_rds(osm_cinema_Medellin_sf,'osm_cinema_Medellin_sf.RDS')
write_rds(osm_cinema_Cali_sf,'osm_cinema_Cali_sf.RDS')

write_rds(osm_clinic_Bogota_sf,'osm_clinic_Bogota_sf.RDS')
write_rds(osm_clinic_Medellin_sf,'osm_clinic_Medellin_sf.RDS')
write_rds(osm_clinic_Cali_sf,'osm_clinic_Cali_sf.RDS')


write_rds(osm_college_Bogota_sf,'osm_college_Bogota_sf.RDS')
write_rds(osm_college_Medellin_sf,'osm_college_Medellin_sf.RDS')
write_rds(osm_college_Cali_sf,'osm_college_Cali_sf.RDS')


write_rds(osm_community_centre_Bogota_sf,'osm_community_centre_Bogota_sf.RDS')
write_rds(osm_community_centre_Medellin_sf,'osm_community_centre_Medellin_sf.RDS')
write_rds(osm_community_centre_Cali_sf,'osm_community_centre_Cali_sf.RDS')

write_rds(osm_conference_centre_Bogota_sf,'osm_conference_centre_Bogota_sf.RDS')
write_rds(osm_conference_centre_Medellin_sf,'osm_conference_centre_Medellin_sf.RDS')
write_rds(osm_conference_centre_Cali_sf,'osm_conference_centre_Cali_sf.RDS')


write_rds(osm_dentist_Bogota_sf,'osm_dentist_Bogota_sf.RDS')
write_rds(osm_dentist_Medellin_sf,'osm_dentist_Medellin_sf.RDS')
write_rds(osm_dentist_Cali_sf,'osm_dentist_Cali_sf.RDS')


write_rds(osm_doctors_Bogota_sf,'osm_doctors_Bogota_sf.RDS')
write_rds(osm_doctors_Medellin_sf,'osm_doctors_Medellin_sf.RDS')
write_rds(osm_doctors_Cali_sf,'osm_doctors_Cali_sf.RDS')


write_rds(osm_events_venue_Bogota_sf,'osm_events_venue_Bogota_sf.RDS')
write_rds(osm_events_venue_Medellin_sf,'osm_events_venue_Medellin_sf.RDS')
write_rds(osm_events_venue_Cali_sf,'osm_events_venue_Cali_sf.RDS')


write_rds(osm_fast_food_Bogota_sf,'osm_fast_food_Bogota_sf.RDS')
write_rds(osm_fast_food_Medellin_sf,'osm_fast_food_Medellin_sf.RDS')
write_rds(osm_fast_food_Cali_sf,'osm_fast_food_Cali_sf.RDS')


write_rds(osm_hospital_Bogota_sf,'osm_hospital_Bogota_sf.RDS')
write_rds(osm_hospital_Medellin_sf,'osm_hospital_Medellin_sf.RDS')
write_rds(osm_hospital_Cali_sf,'osm_hospital_Cali_sf.RDS')


write_rds(osm_kindergarten_Bogota_sf,'osm_kindergarten_Bogota_sf.RDS')
write_rds(osm_kindergarten_Medellin_sf,'osm_kindergarten_Medellin_sf.RDS')
write_rds(osm_kindergarten_Cali_sf,'osm_kindergarten_Cali_sf.RDS')

write_rds(osm_library_Bogota_sf,'osm_library_Bogota_sf.RDS')
write_rds(osm_library_Medellin_sf,'osm_library_Medellin_sf.RDS')
write_rds(osm_library_Cali_sf,'osm_library_Cali_sf.RDS')

write_rds(osm_love_hotel_Bogota_sf,'osm_love_hotel_Bogota_sf.RDS')
write_rds(osm_love_hotel_Medellin_sf,'osm_love_hotel_Medellin_sf.RDS')
write_rds(osm_love_hotel_Cali_sf,'osm_love_hotel_Cali_sf.RDS')

write_rds(osm_marketplace_Bogota_sf,'osm_marketplace_Bogota_sf.RDS')
write_rds(osm_marketplace_Medellin_sf,'osm_marketplace_Medellin_sf.RDS')
write_rds(osm_marketplace_Cali_sf,'osm_marketplace_Cali_sf.RDS')


write_rds(osm_monastery_Bogota_sf,'osm_monastery_Bogota_sf.RDS')
write_rds(osm_monastery_Medellin_sf,'osm_monastery_Medellin_sf.RDS')
write_rds(osm_monastery_Cali_sf,'osm_monastery_Cali_sf.RDS')


write_rds(osm_parking_Bogota_sf,'osm_parking_Bogota_sf.RDS')
write_rds(osm_parking_Medellin_sf,'osm_parking_Medellin_sf.RDS')
write_rds(osm_parking_Cali_sf,'osm_parking_Cali_sf.RDS')


### FARMACIAS NO HAY PARA CALI
write_rds(osm_pharmacy_Bogota_sf,'osm_pharmacy_Bogota_sf.RDS')
write_rds(osm_pharmacy_Medellin_sf,'osm_pharmacy_Medellin_sf.RDS')


### PLACE OF WORSHIP NO HAY PARA CALI
write_rds(osm_place_of_worship_Bogota_sf, 'osm_place_of_worship_Bogota_sf.RDS')
write_rds(osm_place_of_worship_Medellin_sf, 'osm_place_of_worship_Medellin_sf.RDS')


write_rds(osm_police_Bogota_sf, 'osm_police_Bogota_sf.RDS')
write_rds(osm_police_Medellin_sf, 'osm_police_Medellin_sf.RDS')
write_rds(osm_police_Cali_sf, 'osm_police_Cali_sf.RDS')


write_rds(osm_post_office_Bogota_sf, 'osm_post_office_Bogota_sf.RDS')
write_rds(osm_post_office_Medellin_sf, 'osm_post_office_Medellin_sf.RDS')

### PUB NO HAY PARA MEDELLIN NI PARA CALI

### RECYCLING NO HAY PARA MEDELLIN NI PARA CALI

write_rds(osm_recycling_Bogota_sf, 'osm_recycling_Bogota_sf.RDS')
write_rds(osm_recycling_Medellin_sf, 'osm_recycling_Medellin_sf.RDS')

write_rds(osm_restaurant_Bogota_sf, 'osm_restaurant_Bogota_sf.RDS')
write_rds(osm_restaurant_Medellin_sf, 'osm_restaurant_Medellin_sf.RDS')
write_rds(osm_restaurant_Cali_sf, 'osm_restaurant_Cali_sf.RDS')

### SCHOOL NO HAY PARA CALI
write_rds(osm_school_Bogota_sf, 'osm_school_Bogota_sf.RDS')
write_rds(osm_school_Medellin_sf, 'osm_school_Medellin_sf.RDS')

### SHELTER NO HAY PARA MEDELLIN NI CALI


write_rds(osm_social_facility_Bogota_sf, 'osm_social_facility_Bogota_sf.RDS')
write_rds(osm_social_facility_Medellin_sf, 'osm_social_facility_Medellin_sf.RDS')

write_rds(osm_theatre_Bogota_sf, 'osm_theatre_Bogota_sf.RDS')
write_rds(osm_theatre_Medellin_sf, 'osm_theatre_Medellin_sf.RDS')


write_rds(osm_university_Bogota_sf, 'osm_university_Bogota_sf.RDS')
write_rds(osm_university_Medellin_sf, 'osm_university_Medellin_sf.RDS')


write_rds(osm_veterinary_Bogota_sf, 'osm_veterinary_Bogota_sf.RDS')
write_rds(osm_veterinary_Medellin_sf, 'osm_veterinary_Medellin_sf.RDS')


####


### CARGAR OSM_SF DE CADA AMENITY
osm_arts_centre_Bogota_sf <- readRDS('osm_arts_centre_Bogota_sf.RDS')
osm_arts_centre_Medellin_sf <- readRDS('osm_arts_centre_Medellin_sf.RDS')

osm_bank_Bogota_sf <- readRDS('osm_bank_Bogota_sf.RDS')
osm_bank_Medellin_sf <- readRDS('osm_bank_Medellin_sf.RDS')
osm_bank_Cali_sf <- readRDS('osm_bank_Cali_sf.RDS')

osm_bicycle_parking_Bogota_sf <- readRDS('osm_bicycle_parking_Bogota_sf.RDS')
osm_bicycle_parking_Medellin_sf <- readRDS('osm_bicycle_parking_Medellin_sf.RDS')
osm_bicycle_parking_Medellin_sf <- readRDS('osm_bicycle_parking_Medellin_sf.RDS')


osm_bus_station_Bogota_sf <- readRDS('osm_bus_station_Bogota_sf.RDS')
osm_bus_station_Medellin_sf <- readRDS('osm_bus_station_Medellin_sf.RDS')
osm_bus_station_Cali_sf <- readRDS('osm_bus_station_Cali_sf.RDS')

### DE CAFE SON 0

osm_casino_Bogota_sf <- readRDS('osm_casino_Bogota_sf.RDS')
osm_casino_Medellin_sf <- readRDS('osm_casino_Medellin_sf.RDS')
osm_casino_Cali_sf <- readRDS('osm_casino_Cali_sf.RDS')

osm_childcare_Bogota_sf <- readRDS('osm_childcare_Bogota_sf.RDS')
osm_childcare_Medellin_sf <- readRDS('osm_childcare_Medellin_sf.RDS')
osm_childcare_Cali_sf <- readRDS('osm_childcare_Cali_sf.RDS')

osm_cinema_Bogota_sf <- readRDS('osm_cinema_Bogota_sf.RDS')
osm_cinema_Medellin_sf <- readRDS('osm_cinema_Medellin_sf.RDS')
osm_cinema_Cali_sf <- readRDS('osm_cinema_Cali_sf.RDS')


osm_clinic_Bogota_sf <- readRDS('osm_clinic_Bogota_sf.RDS')
osm_clinic_Medellin_sf <- readRDS('osm_clinic_Medellin_sf.RDS')
osm_clinic_Cali_sf <- readRDS('osm_clinic_Cali_sf.RDS')


osm_college_Bogota_sf <- readRDS('osm_college_Bogota_sf.RDS')
osm_college_Medellin_sf <- readRDS('osm_college_Medellin_sf.RDS')
osm_college_Cali_sf <- readRDS('osm_college_Cali_sf.RDS')


osm_community_centre_Bogota_sf <- readRDS('osm_community_centre_Bogota_sf.RDS')
osm_community_centre_Medellin_sf <- readRDS('osm_community_centre_Medellin_sf.RDS')
osm_community_centre_Cali_sf <- readRDS('osm_community_centre_Cali_sf.RDS')


osm_conference_centre_Bogota_sf <- readRDS('osm_conference_centre_Bogota_sf.RDS')
osm_conference_centre_Medellin_sf <- readRDS('osm_conference_centre_Medellin_sf.RDS')
osm_conference_centre_Cali_sf <- readRDS('osm_conference_centre_Cali_sf.RDS')



osm_dentist_Bogota_sf <- readRDS('osm_dentist_Bogota_sf.RDS')
osm_dentist_Medellin_sf <- readRDS('osm_dentist_Medellin_sf.RDS')
osm_dentist_Cali_sf <- readRDS('osm_dentist_Cali_sf.RDS')


osm_doctors_Bogota_sf <- readRDS('osm_doctors_Bogota_sf.RDS')
osm_doctors_Medellin_sf <- readRDS('osm_doctors_Medellin_sf.RDS')
osm_doctors_Cali_sf <- readRDS('osm_doctors_Cali_sf.RDS')


osm_events_venue_Bogota_sf <- readRDS('osm_events_venue_Bogota_sf.RDS')
osm_events_venue_Cali_sf <- readRDS('osm_events_venue_Cali_sf.RDS')
osm_events_venue_Medellin_sf <- readRDS('osm_events_venue_Medellin_sf.RDS')


osm_fast_food_Bogota_sf <- readRDS('osm_fast_food_Bogota_sf.RDS')
osm_fast_food_Medellin_sf <- readRDS('osm_fast_food_Medellin_sf.RDS')
osm_fast_food_Cali_sf <- readRDS('osm_fast_food_Cali_sf.RDS') 

osm_hospital_Bogota_sf <- readRDS('osm_hospital_Bogota_sf.RDS')
osm_hospital_Medellin_sf <- readRDS('osm_hospital_Medellin_sf.RDS')
osm_hospital_Cali_sf <- readRDS('osm_hospital_Cali_sf.RDS')


osm_kindergarten_Bogota_sf <- readRDS('osm_kindergarten_Bogota_sf.RDS')
osm_kindergarten_Medellin_sf <- readRDS('osm_kindergarten_Medellin_sf.RDS')
osm_kindergarten_Cali_sf <- readRDS('osm_kindergarten_Cali_sf.RDS')


osm_library_Bogota_sf <- readRDS('osm_library_Bogota_sf.RDS')
osm_library_Medellin_sf <- readRDS('osm_library_Medellin_sf.RDS')
osm_library_Cali_sf <- readRDS('osm_library_Cali_sf.RDS')


osm_love_hotel_Bogota_sf <- readRDS('osm_love_hotel_Bogota_sf.RDS')
osm_love_hotel_Medellin_sf <- readRDS('osm_love_hotel_Medellin_sf.RDS')
osm_love_hotel_Cali_sf <- readRDS('osm_love_hotel_Cali_sf.RDS')


osm_marketplace_Bogota_sf <- readRDS('osm_marketplace_Bogota_sf.RDS')
osm_marketplace_Cali_sf <- readRDS('osm_marketplace_Cali_sf.RDS')
osm_marketplace_Medellin_sf <- readRDS('osm_marketplace_Medellin_sf.RDS')


osm_monastery_Bogota_sf <- readRDS('osm_monastery_Bogota_sf.RDS')
osm_monastery_Medellin_sf <- readRDS('osm_monastery_Medellin_sf.RDS')
osm_monastery_Cali_sf <- readRDS('osm_monastery_Cali_sf.RDS')


osm_parking_Bogota_sf <- readRDS('osm_parking_Bogota_sf.RDS')
osm_parking_Medellin_sf <- readRDS('osm_parking_Medellin_sf.RDS')
osm_parking_Cali_sf <- readRDS('osm_parking_Cali_sf.RDS')


osm_pharmacy_Bogota_sf <- readRDS('osm_pharmacy_Bogota_sf.RDS')
osm_pharmacy_Medellin_sf <- readRDS('osm_pharmacy_Medellin_sf.RDS')


osm_place_of_worship_Bogota_sf <- readRDS('osm_place_of_worship_Bogota_sf.RDS')
osm_place_of_worship_Medellin_sf <- readRDS('osm_place_of_worship_Medellin_sf.RDS')


osm_police_Bogota_sf <- readRDS('osm_police_Bogota_sf.RDS')
osm_police_Medellin_sf <- readRDS('osm_police_Medellin_sf.RDS')
osm_police_Cali_sf <- readRDS('osm_police_Cali_sf.RDS')


osm_post_office_Bogota_sf <- readRDS('osm_post_office_Bogota_sf.RDS')
osm_post_office_Medellin_sf <- readRDS('osm_post_office_Medellin_sf.RDS')

osm_recycling_Bogota_sf <- readRDS('osm_recycling_Bogota_sf.RDS')
osm_recycling_Medellin_sf <- readRDS('osm_recycling_Medellin_sf.RDS')

osm_restaurant_Bogota_sf <- readRDS('osm_restaurant_Bogota_sf.RDS')
osm_restaurant_Medellin_sf <- readRDS('osm_restaurant_Medellin_sf.RDS')
osm_restaurant_Cali_sf <- readRDS('osm_restaurant_Cali_sf.RDS')

osm_school_Bogota_sf <- readRDS('osm_school_Bogota_sf.RDS')
osm_school_Medellin_sf <- readRDS('osm_school_Medellin_sf.RDS')


osm_social_facility_Bogota_sf <- readRDS('osm_social_facility_Bogota_sf.RDS')
osm_social_facility_Medellin_sf <- readRDS('osm_social_facility_Medellin_sf.RDS')


osm_theatre_Bogota_sf <- readRDS('osm_theatre_Bogota_sf.RDS')
osm_theatre_Medellin_sf <- readRDS('osm_theatre_Medellin_sf.RDS')


osm_university_Bogota_sf <- readRDS('osm_university_Bogota_sf.RDS')
osm_university_Medellin_sf <- readRDS('osm_university_Medellin_sf.RDS')


osm_veterinary_Bogota_sf <- readRDS('osm_veterinary_Bogota_sf.RDS')
osm_veterinary_Medellin_sf <-readRDS('osm_veterinary_Medellin_sf.RDS')




### ESTO HACE EL LOOP PARA CADA AMENITIE ###

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


### ESTO HACE EL LOOP PARA CADA AMENITIE ###






## **[5.] Operaciones geometricas**

### **5.1 Importar conjuntos de datos**

## Inmuebles
#train_house <- train
#class(train_house)
#skim(train_house)

## convetir DataFrame de casas en datos de entrenamiento a sf
train_sf <- st_as_sf(x = train, ## datos
                        coords=c("lon","lat"), ## coordenadas
                        crs=4326) ## CRS

class(train_sf)

# graficar casas de datos de entrenamiento en formato sf
leaflet() %>% addTiles() %>% addCircleMarkers(data=train_sf) #[1:10,]


## convetir DataFrame de casas en datos de entrenamiento a sf
test_sf <- st_as_sf(x = test, ## datos
                     coords=c("lon","lat"), ## coordenadas
                     crs=4326) ## CRS

class(test_sf)

# graficar casas de datos de entrenamiento en formato sf
leaflet() %>% addTiles() %>% addCircleMarkers(data=test_sf) #[1:10,]


### **5.2 help:** `sf` 

## Help
vignette("sf3")
vignette("sf4")

### **5.3 Afine transformations**
# tiene que ser el mismo dato
#st_crs(train_sf) == st_crs(parques) 



### **5.4 Filtros de datos**

# grafico casas Bogota

# opcion 1: grafico general de casas de datos de entrenamiento en la ciudad de Bogota DC
# SIN FILTROS
# casas de Bogota
houses_bogota1 <- train_sf %>% subset(city== "Bogotá D.C")

leaflet() %>% addTiles() %>% addCircles(data=houses_bogota1)


# casas de Medellín
houses_medellin1 <- train_sf %>% subset(city== "Medellín")

leaflet() %>% addTiles() %>% addCircles(data=houses_medellin1)


# casas de Cali
houses_cali1 <- test_sf %>% subset(city== "Cali")

leaflet() %>% addTiles() %>% addCircles(data=houses_cali1)




### Usar la geometría de la ciudad

## crear  caja de coordenadas  de las ciudades

# crear caja de coordenadas de la ciudad Bogota
# crear sf tipo de datos polygon de la ciudad de Bogota
bog_sf_polygon <- getbb(place_name = "Bogota", 
                        featuretype = "boundary:administrative", 
                        format_out = "sf_polygon") %>% .$multipolygon

# graficar sf_polygon de la ciudad de Bogota sf_polygon
leaflet() %>% addTiles() %>% addPolygons(data=bog_sf_polygon)


### GUARDAR polygon de ciduad de Bogota  
write_rds(bog_sf_polygon,'bog_sf_polygon.RDS')

### CARGAR polygon 
bog_sf_polygon <- readRDS('bog_sf_polygon.RDS')



### NO FUCIONA EL POLYGON DE MEDELLIN NI CALI
# crear sf_polygon de la ciudad Medellin
med_sf_polygon <- getbb(place_name = "Medellín", 
                        featuretype = "boundary:administrative", 
                        format_out = "sf_polygon") %>% .$multipolygon

# graficar sf_polygon de la ciudad de Medellin sf_polygon
leaflet() %>% addTiles() %>% addPolygons(data=med_sf_polygon)


### GUARDAR polygon de ciduad de Bogota  
write_rds(med_sf_polygon,'med_sf_polygon.RDS')

### CARGAR polygon 
med_sf_polygon <- readRDS('med_sf_polygon.RDS')



# crear sf_polygon de la ciudad Cali
cali_sf_polygon <- getbb(place_name = "Cali", 
                         featuretype = "boundary:administrative", 
                         format_out = "sf_polygon") %>% .$multipolygon

# graficar sf_polygon de la ciudad de Bogota sf_polygon
leaflet() %>% addTiles() %>% addPolygons(data=cali_sf_polygon)

### GUARDAR polygon de ciduad de Bogota  
write_rds(cali_sf_polygon,'cali_sf_polygon.RDS')

### CARGAR polygon 
cali_sf_polygon <- readRDS('cali_sf_polygon.RDS')





### Usar la geometría de UPZ de la Ciudad
## crear bb sf_polygon de UPZ's


# obtener bb de UPZ 
# featuretype = "boundary:administrative"
# format_out = sf polygon
bogota_sf <- getbb(place_name = "Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon

leaflet() %>% addTiles() %>% addPolygons(data=bogota_sf)


### opcion 2
## crear interseccion entre x base de casas sf, y sf_polygon   
houses_inter1 <- st_intersection(x = bog_sf_polygon , y = train_sf)
# graficar sf_polygon de la interseccion entre sf de datos de entrenamiento e interseccion bog_sf_polygon,train_sf
leaflet() %>% addTiles() %>% addPolygons(data=bogota_sf,col="red") %>% addCircles(data=houses_inter1)




### opcion 3
#houses_inter2 <- train_sf[,]
# graficar sf_polygon de la interseccion entre sf de datos de entrenamiento  e interseccion bogota_sf, 
#leaflet() %>% addTiles() %>% addPolygons(data=bogota_sf,col="red") %>% addCircles(data=houses_inter2)

## crop puntos con poligono (opcion 3)
#house_chapi <- houses[chapinero,]
#leaflet() %>% addTiles() %>% addPolygons(data=chapinero,col="red") %>% addCircles(data=house_chapi)


### **5.5. Distancia a amenities**
## Calcular Distancia a muchos polygonos


### Distancia a amenities Bogota

# arts_centre_bog
#matrix_dist_arts_centre_bog <- st_distance(x=houses_bogota1  , y=osm_arts_centre_Bogota_sf)

#min_dist_arts_centre_bog <- apply(matrix_dist_arts_centre_bog , 1 , min)

#min_dist_arts_centre_bog %>% head()

#houses_bogota1$arts_centre_bog = min_dist_arts_centre_bog


# bank_bog
matrix_dist_bank_bog <- st_distance(x=houses_bogota1  , y=osm_bank_Bogota_sf)

min_dist_bank_bog <- apply(matrix_dist_bank_bog , 1 , min)

min_dist_bank_bog %>% head()

houses_bogota1$bank_bog = min_dist_bank_bog



# bicycle_parking_bog
#matrix_dist_bicycle_parking_bog <- st_distance(x=houses_bogota1  , y=osm_bicycle_parking_Bogota_sf)

#min_dist_bicycle_parking_bog <- apply(matrix_dist_bicycle_parking_bog , 1 , min)

#min_dist_bicycle_parking_bog %>% head()

#houses_bogota1$bicycle_parking_bog = min_dist_bicycle_parking_bog


# bus_station_bog
matrix_dist_bus_station_bog <- st_distance(x=houses_bogota1  , y=osm_bus_station_Bogota_sf)

min_dist_bus_station_bog <- apply(matrix_dist_bus_station_bog , 1 , min)

min_dist_bus_station_bog %>% head()

houses_bogota1$bus_station_bog = min_dist_bus_station_bog


# casino_bog
matrix_dist_casino_bog <- st_distance(x=houses_bogota1  , y=osm_casino_Bogota_sf)

min_dist_casino_bog <- apply(matrix_dist_casino_bog , 1 , min)

min_dist_casino_bog %>% head()

houses_bogota1$casino_bog = min_dist_casino_bog


# childcare_bog
matrix_dist_childcare_bog <- st_distance(x=houses_bogota1  , y=osm_childcare_Bogota_sf)

min_dist_childcare_bog <- apply(matrix_dist_childcare_bog , 1 , min)

min_dist_childcare_bog %>% head()

houses_bogota1$childcare_bog = min_dist_childcare_bog


# cinema_bog
matrix_dist_cinema_bog <- st_distance(x=houses_bogota1  , y=osm_cinema_Bogota_sf)

min_dist_cinema_bog <- apply(matrix_dist_cinema_bog , 1 , min)

min_dist_cinema_bog %>% head()

houses_bogota1$cinema_bog = min_dist_cinema_bog



# clinic_bog
matrix_dist_clinic_bog <- st_distance(x=houses_bogota1  , y=osm_clinic_Bogota_sf)

min_dist_clinic_bog <- apply(matrix_dist_clinic_bog , 1 , min)

min_dist_clinic_bog %>% head()

houses_bogota1$clinic_bog = min_dist_clinic_bog


# college_bog
matrix_dist_college_bog <- st_distance(x=houses_bogota1  , y=osm_college_Bogota_sf)

min_dist_college_bog <- apply(matrix_dist_college_bog , 1 , min)

min_dist_college_bog %>% head()

houses_bogota1$college_bog = min_dist_college_bog


# community_centre_bog
matrix_dist_community_centre_bog <- st_distance(x=houses_bogota1  , y=osm_community_centre_Bogota_sf)

min_dist_community_centre_bog <- apply(matrix_dist_community_centre_bog , 1 , min)

min_dist_community_centre_bog %>% head()

houses_bogota1$community_centre_bog = min_dist_community_centre_bog


# conference_centre_bog
matrix_dist_conference_centre_bog <- st_distance(x=houses_bogota1  , y=osm_conference_centre_Bogota_sf)

min_dist_conference_centre_bog <- apply(matrix_dist_conference_centre_bog , 1 , min)

min_dist_conference_centre_bog %>% head()

houses_bogota1$conference_centre_bog = min_dist_conference_centre_bog



# dentist_bog
matrix_dist_dentist_bog <- st_distance(x=houses_bogota1  , y=osm_dentist_Bogota_sf)

min_dist_dentist_bog <- apply(matrix_dist_dentist_bog , 1 , min)

min_dist_dentist_bog %>% head()

houses_bogota1$dentist_bog = min_dist_dentist_bog


# doctors_bog
matrix_dist_doctors_bog <- st_distance(x=houses_bogota1  , y=osm_doctors_Bogota_sf)

min_dist_doctors_bog <- apply(matrix_dist_doctors_bog , 1 , min)

min_dist_doctors_bog %>% head()

houses_bogota1$doctors_bog = min_dist_doctors_bog


# events_bog
matrix_dist_events_bog <- st_distance(x=houses_bogota1  , y=osm_events_venue_Bogota_sf)

min_dist_events_bog <- apply(matrix_dist_events_bog , 1 , min)

min_dist_events_bog %>% head()

houses_bogota1$events_bog = min_dist_events_bog


# fast_food_bog
matrix_dist_fast_food_bog <- st_distance(x=houses_bogota1  , y=osm_fast_food_Bogota_sf)

min_dist_fast_food_bog <- apply(matrix_dist_fast_food_bog , 1 , min)

min_dist_fast_food_bog %>% head()

houses_bogota1$fast_food_bog = min_dist_fast_food_bog


# hospital_bog
matrix_dist_hospital_bog <- st_distance(x=houses_bogota1  , y=osm_hospital_Bogota_sf)

min_dist_hospital_bog <- apply(matrix_dist_hospital_bog , 1 , min)

min_dist_hospital_bog %>% head()

houses_bogota1$hospital_bog = min_dist_hospital_bog


# kindergarten_bog
matrix_dist_kindergarten_bog <- st_distance(x=houses_bogota1  , y=osm_kindergarten_Bogota_sf)

min_dist_kindergarten_bog <- apply(matrix_dist_kindergarten_bog , 1 , min)

min_dist_kindergarten_bog %>% head()

houses_bogota1$kindergarten_bog = min_dist_kindergarten_bog




# library_bog
matrix_dist_library_bog <- st_distance(x=houses_bogota1  , y=osm_library_Bogota_sf)

min_dist_library_bog <- apply(matrix_dist_library_bog , 1 , min)

min_dist_library_bog %>% head()

houses_bogota1$library_bog = min_dist_library_bog


# love_hotel_bog
matrix_dist_love_hotel_bog <- st_distance(x=houses_bogota1  , y=osm_love_hotel_Bogota_sf)

min_dist_love_hotel_bog <- apply(matrix_dist_love_hotel_bog , 1 , min)

min_dist_love_hotel_bog %>% head()

houses_bogota1$love_hotel_bog = min_dist_love_hotel_bog



# marketplace_bog
matrix_dist_marketplace_bog <- st_distance(x=houses_bogota1  , y=osm_marketplace_Bogota_sf)

min_dist_marketplace_bog <- apply(matrix_dist_marketplace_bog , 1 , min)

min_dist_marketplace_bog %>% head()

houses_bogota1$marketplace_bog = min_dist_marketplace_bog


# monastery_bog
matrix_dist_monastery_bog <- st_distance(x=houses_bogota1  , y=osm_monastery_Bogota_sf)

min_dist_monastery_bog <- apply(matrix_dist_monastery_bog , 1 , min)

min_dist_monastery_bog %>% head()

houses_bogota1$monastery_bog = min_dist_monastery_bog


# parking_bog
matrix_dist_parking_bog <- st_distance(x=houses_bogota1  , y=osm_parking_Bogota_sf)

min_dist_parking_bog <- apply(matrix_dist_parking_bog , 1 , min)

min_dist_parking_bog %>% head()

houses_bogota1$parking_bog = min_dist_parking_bog



# pharmacy_bog
#matrix_dist_pharmacy_bog <- st_distance(x=houses_bogota1  , y=osm_pharmacy_Bogota_sf)

#min_dist_pharmacy_bog <- apply(matrix_dist_pharmacy_bog , 1 , min)

#min_dist_pharmacy_bog %>% head()

#houses_bogota1$pharmacy_bog = min_dist_pharmacy_bog


# worship_bog
#matrix_dist_worship_bog <- st_distance(x=houses_bogota1  , y=osm_place_of_worship_Bogota_sf)

#min_dist_worship_bog <- apply(matrix_dist_worship_bog , 1 , min)

#min_dist_worship_bog %>% head()

#houses_bogota1$worship_bog = min_dist_worship_bog


# police_bog
matrix_dist_police_bog <- st_distance(x=houses_bogota1  , y=osm_police_Bogota_sf)

min_dist_police_bog <- apply(matrix_dist_police_bog , 1 , min)

min_dist_police_bog %>% head()

houses_bogota1$police_bog = min_dist_police_bog



# post_office_bog
#matrix_dist_post_office_bog <- st_distance(x=houses_bogota1  , y=osm_post_office_Bogota_sf)

#min_dist_post_office_bog <- apply(matrix_dist_post_office_bog , 1 , min)

#min_dist_post_office_bog %>% head()

#houses_bogota1$post_office_bog = min_dist_post_office_bog


# recycling_bog
#matrix_dist_recycling_bog <- st_distance(x=houses_bogota1  , y=osm_recycling_Bogota_sf)

#min_dist_recycling_bog <- apply(matrix_dist_recycling_bog , 1 , min)

#min_dist_recycling_bog %>% head()

#houses_bogota1$recycling_bog = min_dist_recycling_bog



### restaurant_bog
matrix_dist_restaurant_bog <- st_distance(x=houses_bogota1  , y=osm_restaurant_Bogota_sf)

min_dist_restaurant_bog <- apply(matrix_dist_restaurant_bog , 1 , min)

min_dist_restaurant_bog %>% head()

houses_bogota1$restaurant_bog = min_dist_restaurant_bog


# school_bog
#matrix_dist_school_bog <- st_distance(x=houses_bogota1  , y=osm_school_Bogota_sf)

#min_dist_school_bog <- apply(matrix_dist_school_bog , 1 , min)

#min_dist_school_bog %>% head()

#houses_bogota1$post_school_bog = min_dist_school_bog


# social_facility_bog
#matrix_dist_social_facility_bog <- st_distance(x=houses_bogota1  , y=osm_social_facility_Bogota_sf )

#min_dist_social_facility_bog <- apply(matrix_dist_social_facility_bog , 1 , min)

#min_dist_social_facility_bog %>% head()

#houses_bogota1$social_facility_bog = min_dist_social_facility_bog


# theatre_bog
#matrix_dist_theatre_bog <- st_distance(x=houses_bogota1  , y=osm_theatre_Bogota_sf )

#min_dist_theatre_bog <- apply(matrix_dist_theatre_bog , 1 , min)

#min_dist_theatre_bog %>% head()

#houses_bogota1$theatre_bog = min_dist_theatre_bog



# university_bog
#matrix_dist_university_bog <- st_distance(x=houses_bogota1  , y=osm_university_Bogota_sf )

#min_dist_university_bog <- apply(matrix_dist_university_bog , 1 , min)

#min_dist_university_bog %>% head()

#houses_bogota1$university_bog = min_dist_university_bog


# veterinary_bog
#matrix_dist_veterinary_bog <- st_distance(x=houses_bogota1  , y=osm_veterinary_Bogota_sf )

#min_dist_veterinary_bog <- apply(matrix_dist_veterinary_bog , 1 , min)

#min_dist_veterinary_bog %>% head()

#houses_bogota1$veterinary_bog = min_dist_veterinary_bog



### GUARDAR BASES DE DATOS DE ENTRENAMIENTO CON AMENITIES BOGOTA

write_rds(houses_bogota1,'amenities_bogota.RDS')


### CARGAR BASES DE DATOS DE ENTRENAMIENTO CON AMENITIES BOGOTA

houses_bogota1 <- readRDS('amenities_bogota.RDS')



### Distancia a amenities Medellin


# arts_centre_med
#matrix_dist_arts_centre_med <- st_distance(x=houses_medellin1  , y=osm_arts_centre_medota_sf)

#min_dist_arts_centre_med <- apply(matrix_dist_arts_centre_med , 1 , min)

#min_dist_arts_centre_med %>% head()

#houses_medellin1$arts_centre_med = min_dist_arts_centre_med


# bank_med
matrix_dist_bank_med <- st_distance(x=houses_medellin1  , y=osm_bank_Medellin_sf)

min_dist_bank_med <- apply(matrix_dist_bank_med , 1 , min)

min_dist_bank_med %>% head()

houses_medellin1$bank_med = min_dist_bank_med



# bicycle_parking_med
#matrix_dist_bicycle_parking_med <- st_distance(x=houses_medellin1  , y=osm_bicycle_parking_Medellin_sf)

#min_dist_bicycle_parking_med <- apply(matrix_dist_bicycle_parking_med , 1 , min)

#min_dist_bicycle_parking_med %>% head()

#houses_medellin1$bicycle_parking_med = min_dist_bicycle_parking_med


# bus_station_med
matrix_dist_bus_station_med <- st_distance(x=houses_medellin1  , y=osm_bus_station_Medellin_sf)

min_dist_bus_station_med <- apply(matrix_dist_bus_station_med , 1 , min)

min_dist_bus_station_med %>% head()

houses_medellin1$bus_station_med = min_dist_bus_station_med


# casino_med
matrix_dist_casino_med <- st_distance(x=houses_medellin1  , y=osm_casino_Medellin_sf)

min_dist_casino_med <- apply(matrix_dist_casino_med , 1 , min)

min_dist_casino_med %>% head()

houses_medellin1$casino_med = min_dist_casino_med


# childcare_med
matrix_dist_childcare_med <- st_distance(x=houses_medellin1  , y=osm_childcare_Medellin_sf)

min_dist_childcare_med <- apply(matrix_dist_childcare_med , 1 , min)

min_dist_childcare_med %>% head()

houses_medellin1$childcare_med = min_dist_childcare_med


# cinema_med
matrix_dist_cinema_med <- st_distance(x=houses_medellin1  , y=osm_cinema_Medellin_sf)

min_dist_cinema_med <- apply(matrix_dist_cinema_med , 1 , min)

min_dist_cinema_med %>% head()

houses_medellin1$cinema_med = min_dist_cinema_med



# clinic_med
matrix_dist_clinic_med <- st_distance(x=houses_medellin1  , y=osm_clinic_Medellin_sf)

min_dist_clinic_med <- apply(matrix_dist_clinic_med , 1 , min)

min_dist_clinic_med %>% head()

houses_medellin1$clinic_med = min_dist_clinic_med


# college_med
matrix_dist_college_med <- st_distance(x=houses_medellin1  , y=osm_college_Medellin_sf)

min_dist_college_med <- apply(matrix_dist_college_med , 1 , min)

min_dist_college_med %>% head()

houses_medellin1$college_med = min_dist_college_med


# community_centre_med
matrix_dist_community_centre_med <- st_distance(x=houses_medellin1  , y=osm_community_centre_Medellin_sf)

min_dist_community_centre_med <- apply(matrix_dist_community_centre_med , 1 , min)

min_dist_community_centre_med %>% head()

houses_medellin1$community_centre_med = min_dist_community_centre_med


# conference_centre_med
matrix_dist_conference_centre_med <- st_distance(x=houses_medellin1  , y=osm_conference_centre_Medellin_sf)

min_dist_conference_centre_med <- apply(matrix_dist_conference_centre_med , 1 , min)

min_dist_conference_centre_med %>% head()

houses_medellin1$conference_centre_med = min_dist_conference_centre_med



# dentist_med
matrix_dist_dentist_med <- st_distance(x=houses_medellin1  , y=osm_dentist_Medellin_sf)

min_dist_dentist_med <- apply(matrix_dist_dentist_med , 1 , min)

min_dist_dentist_med %>% head()

houses_medellin1$dentist_med = min_dist_dentist_med


# doctors_med
matrix_dist_doctors_med <- st_distance(x=houses_medellin1  , y=osm_doctors_Medellin_sf)

min_dist_doctors_med <- apply(matrix_dist_doctors_med , 1 , min)

min_dist_doctors_med %>% head()

houses_medellin1$doctors_med = min_dist_doctors_med


# events_med
matrix_dist_events_med <- st_distance(x=houses_medellin1  , y=osm_events_venue_Medellin_sf)

min_dist_events_med <- apply(matrix_dist_events_med , 1 , min)

min_dist_events_med %>% head()

houses_medellin1$events_med = min_dist_events_med


# fast_food_med
matrix_dist_fast_food_med <- st_distance(x=houses_medellin1  , y=osm_fast_food_Medellin_sf)

min_dist_fast_food_med <- apply(matrix_dist_fast_food_med , 1 , min)

min_dist_fast_food_med %>% head()

houses_medellin1$fast_food_med = min_dist_fast_food_med


# hospital_med
matrix_dist_hospital_med <- st_distance(x=houses_medellin1  , y=osm_hospital_Medellin_sf)

min_dist_hospital_med <- apply(matrix_dist_hospital_med , 1 , min)

min_dist_hospital_med %>% head()

houses_medellin1$hospital_med = min_dist_hospital_med


# kindergarten_med
matrix_dist_kindergarten_med <- st_distance(x=houses_medellin1  , y=osm_kindergarten_Medellin_sf)

min_dist_kindergarten_med <- apply(matrix_dist_kindergarten_med , 1 , min)

min_dist_kindergarten_med %>% head()

houses_medellin1$kindergarten_med = min_dist_kindergarten_med




# library_med
matrix_dist_library_med <- st_distance(x=houses_medellin1  , y=osm_library_Medellin_sf)

min_dist_library_med <- apply(matrix_dist_library_med , 1 , min)

min_dist_library_med %>% head()

houses_medellin1$library_med = min_dist_library_med


# love_hotel_med
matrix_dist_love_hotel_med <- st_distance(x=houses_medellin1  , y=osm_love_hotel_Medellin_sf)

min_dist_love_hotel_med <- apply(matrix_dist_love_hotel_med , 1 , min)

min_dist_love_hotel_med %>% head()

houses_medellin1$love_hotel_med = min_dist_love_hotel_med



# marketplace_med
matrix_dist_marketplace_med <- st_distance(x=houses_medellin1  , y=osm_marketplace_Medellin_sf)

min_dist_marketplace_med <- apply(matrix_dist_marketplace_med , 1 , min)

min_dist_marketplace_med %>% head()

houses_medellin1$marketplace_med = min_dist_marketplace_med


# monastery_med
matrix_dist_monastery_med <- st_distance(x=houses_medellin1  , y=osm_monastery_Medellin_sf)

min_dist_monastery_med <- apply(matrix_dist_monastery_med , 1 , min)

min_dist_monastery_med %>% head()

houses_medellin1$monastery_med = min_dist_monastery_med


# parking_med
matrix_dist_parking_med <- st_distance(x=houses_medellin1  , y=osm_parking_Medellin_sf)

min_dist_parking_med <- apply(matrix_dist_parking_med , 1 , min)

min_dist_parking_med %>% head()

houses_medellin1$parking_med = min_dist_parking_med



# pharmacy_med
#matrix_dist_pharmacy_med <- st_distance(x=houses_medellin1  , y=osm_pharmacy_medota_sf)

#min_dist_pharmacy_med <- apply(matrix_dist_pharmacy_med , 1 , min)

#min_dist_pharmacy_med %>% head()

#houses_medellin1$pharmacy_med = min_dist_pharmacy_med


# worship_med
#matrix_dist_worship_med <- st_distance(x=houses_medellin1  , y=osm_place_of_worship_medota_sf)

#min_dist_worship_med <- apply(matrix_dist_worship_med , 1 , min)

#min_dist_worship_med %>% head()

#houses_medellin1$worship_med = min_dist_worship_med


# police_med
matrix_dist_police_med <- st_distance(x=houses_medellin1  , y=osm_police_Medellin_sf)

min_dist_police_med <- apply(matrix_dist_police_med , 1 , min)

min_dist_police_med %>% head()

houses_medellin1$police_med = min_dist_police_med



# post_office_med
#matrix_dist_post_office_med <- st_distance(x=houses_medellin1  , y=osm_post_office_medota_sf)

#min_dist_post_office_med <- apply(matrix_dist_post_office_med , 1 , min)

#min_dist_post_office_med %>% head()

#houses_medellin1$post_office_med = min_dist_post_office_med


# recycling_med
#matrix_dist_recycling_med <- st_distance(x=houses_medellin1  , y=osm_recycling_medota_sf)

#min_dist_recycling_med <- apply(matrix_dist_recycling_med , 1 , min)

#min_dist_recycling_med %>% head()

#houses_medellin1$recycling_med = min_dist_recycling_med



### restaurant_med
matrix_dist_restaurant_med <- st_distance(x=houses_medellin1  , y=osm_restaurant_Medellin_sf)

min_dist_restaurant_med <- apply(matrix_dist_restaurant_med , 1 , min)

min_dist_restaurant_med %>% head()

houses_medellin1$restaurant_med = min_dist_restaurant_med


# school_med
#matrix_dist_school_med <- st_distance(x=houses_medellin1  , y=osm_school_medota_sf)

#min_dist_school_med <- apply(matrix_dist_school_med , 1 , min)

#min_dist_school_med %>% head()

#houses_medellin1$post_school_med = min_dist_school_med


# social_facility_med
#matrix_dist_social_facility_med <- st_distance(x=houses_medellin1  , y=osm_social_facility_medota_sf )

#min_dist_social_facility_med <- apply(matrix_dist_social_facility_med , 1 , min)

#min_dist_social_facility_med %>% head()

#houses_medellin1$social_facility_med = min_dist_social_facility_med


# theatre_med
#matrix_dist_theatre_med <- st_distance(x=houses_medellin1  , y=osm_theatre_medota_sf )

#min_dist_theatre_med <- apply(matrix_dist_theatre_med , 1 , min)

#min_dist_theatre_med %>% head()

#houses_medellin1$theatre_med = min_dist_theatre_med



# university_med
#matrix_dist_university_med <- st_distance(x=houses_medellin1  , y=osm_university_medota_sf )

#min_dist_university_med <- apply(matrix_dist_university_med , 1 , min)

#min_dist_university_med %>% head()

#houses_medellin1$university_med = min_dist_university_med


# veterinary_med
#matrix_dist_veterinary_med <- st_distance(x=houses_medellin1  , y=osm_veterinary_medota_sf )

#min_dist_veterinary_med <- apply(matrix_dist_veterinary_med , 1 , min)

#min_dist_veterinary_med %>% head()

#houses_medellin1$veterinary_med = min_dist_veterinary_med



### GUARDAR BASES DE DATOS DE ENTRENAMIENTO CON AMENITIES BOGOTA

write_rds(houses_medellin1,'amenities_medellin.RDS')


### CARGAR BASES DE DATOS DE ENTRENAMIENTO CON AMENITIES BOGOTA

houses_medellin1 <- readRDS('amenities_medellin.RDS')



### Distancia a amenities Cali


# arts_centre_cali
#matrix_dist_arts_centre_cali <- st_distance(x=houses_cali1  , y=osm_arts_centre_caliota_sf)

#min_dist_arts_centre_cali <- apply(matrix_dist_arts_centre_cali , 1 , min)

#min_dist_arts_centre_cali %>% head()

#houses_cali1$arts_centre_cali = min_dist_arts_centre_cali


# bank_cali
matrix_dist_bank_cali <- st_distance(x=houses_cali1  , y=osm_bank_Cali_sf)

min_dist_bank_cali <- apply(matrix_dist_bank_cali , 1 , min)

min_dist_bank_cali %>% head()

houses_cali1$bank_cali = min_dist_bank_cali



# bicycle_parking_cali
#matrix_dist_bicycle_parking_cali <- st_distance(x=houses_cali1  , y=osm_bicycle_parking_caliellin_sf)

#min_dist_bicycle_parking_cali <- apply(matrix_dist_bicycle_parking_cali , 1 , min)

#min_dist_bicycle_parking_cali %>% head()

#houses_cali1$bicycle_parking_cali = min_dist_bicycle_parking_cali


# bus_station_cali
matrix_dist_bus_station_cali <- st_distance(x=houses_cali1  , y=osm_bus_station_Cali_sf)

min_dist_bus_station_cali <- apply(matrix_dist_bus_station_cali , 1 , min)

min_dist_bus_station_cali %>% head()

houses_cali1$bus_station_cali = min_dist_bus_station_cali


# casino_cali
matrix_dist_casino_cali <- st_distance(x=houses_cali1  , y=osm_casino_Cali_sf)

min_dist_casino_cali <- apply(matrix_dist_casino_cali , 1 , min)

min_dist_casino_cali %>% head()

houses_cali1$casino_cali = min_dist_casino_cali


# childcare_cali
matrix_dist_childcare_cali <- st_distance(x=houses_cali1  , y=osm_childcare_Cali_sf)

min_dist_childcare_cali <- apply(matrix_dist_childcare_cali , 1 , min)

min_dist_childcare_cali %>% head()

houses_cali1$childcare_cali = min_dist_childcare_cali


# cinema_cali
matrix_dist_cinema_cali <- st_distance(x=houses_cali1  , y=osm_cinema_Cali_sf)

min_dist_cinema_cali <- apply(matrix_dist_cinema_cali , 1 , min)

min_dist_cinema_cali %>% head()

houses_cali1$cinema_cali = min_dist_cinema_cali



# clinic_cali
matrix_dist_clinic_cali <- st_distance(x=houses_cali1  , y=osm_clinic_Cali_sf)

min_dist_clinic_cali <- apply(matrix_dist_clinic_cali , 1 , min)

min_dist_clinic_cali %>% head()

houses_cali1$clinic_cali = min_dist_clinic_cali


# college_cali
matrix_dist_college_cali <- st_distance(x=houses_cali1  , y=osm_college_Cali_sf)

min_dist_college_cali <- apply(matrix_dist_college_cali , 1 , min)

min_dist_college_cali %>% head()

houses_cali1$college_cali = min_dist_college_cali


# community_centre_cali
matrix_dist_community_centre_cali <- st_distance(x=houses_cali1  , y=osm_community_centre_Cali_sf)

min_dist_community_centre_cali <- apply(matrix_dist_community_centre_cali , 1 , min)

min_dist_community_centre_cali %>% head()

houses_cali1$community_centre_cali = min_dist_community_centre_cali


# conference_centre_cali
matrix_dist_conference_centre_cali <- st_distance(x=houses_cali1  , y=osm_conference_centre_Cali_sf)

min_dist_conference_centre_cali <- apply(matrix_dist_conference_centre_cali , 1 , min)

min_dist_conference_centre_cali %>% head()

houses_cali1$conference_centre_cali = min_dist_conference_centre_cali



# dentist_cali
matrix_dist_dentist_cali <- st_distance(x=houses_cali1  , y=osm_dentist_Cali_sf)

min_dist_dentist_cali <- apply(matrix_dist_dentist_cali , 1 , min)

min_dist_dentist_cali %>% head()

houses_cali1$dentist_cali = min_dist_dentist_cali


# doctors_cali
matrix_dist_doctors_cali <- st_distance(x=houses_cali1  , y=osm_doctors_Cali_sf)

min_dist_doctors_cali <- apply(matrix_dist_doctors_cali , 1 , min)

min_dist_doctors_cali %>% head()

houses_cali1$doctors_cali = min_dist_doctors_cali


# events_cali
matrix_dist_events_cali <- st_distance(x=houses_cali1  , y=osm_events_venue_Cali_sf)

min_dist_events_cali <- apply(matrix_dist_events_cali , 1 , min)

min_dist_events_cali %>% head()

houses_cali1$events_cali = min_dist_events_cali


# fast_food_cali
matrix_dist_fast_food_cali <- st_distance(x=houses_cali1  , y=osm_fast_food_Cali_sf)

min_dist_fast_food_cali <- apply(matrix_dist_fast_food_cali , 1 , min)

min_dist_fast_food_cali %>% head()

houses_cali1$fast_food_cali = min_dist_fast_food_cali


# hospital_cali
matrix_dist_hospital_cali <- st_distance(x=houses_cali1  , y=osm_hospital_Cali_sf)

min_dist_hospital_cali <- apply(matrix_dist_hospital_cali , 1 , min)

min_dist_hospital_cali %>% head()

houses_cali1$hospital_cali = min_dist_hospital_cali


# kindergarten_cali
matrix_dist_kindergarten_cali <- st_distance(x=houses_cali1  , y=osm_kindergarten_Cali_sf)

min_dist_kindergarten_cali <- apply(matrix_dist_kindergarten_cali , 1 , min)

min_dist_kindergarten_cali %>% head()

houses_cali1$kindergarten_cali = min_dist_kindergarten_cali




# library_cali
matrix_dist_library_cali <- st_distance(x=houses_cali1  , y=osm_library_Cali_sf)

min_dist_library_cali <- apply(matrix_dist_library_cali , 1 , min)

min_dist_library_cali %>% head()

houses_cali1$library_cali = min_dist_library_cali


# love_hotel_cali
matrix_dist_love_hotel_cali <- st_distance(x=houses_cali1  , y=osm_love_hotel_Cali_sf)

min_dist_love_hotel_cali <- apply(matrix_dist_love_hotel_cali , 1 , min)

min_dist_love_hotel_cali %>% head()

houses_cali1$love_hotel_cali = min_dist_love_hotel_cali



# marketplace_cali
matrix_dist_marketplace_cali <- st_distance(x=houses_cali1  , y=osm_marketplace_Cali_sf)

min_dist_marketplace_cali <- apply(matrix_dist_marketplace_cali , 1 , min)

min_dist_marketplace_cali %>% head()

houses_cali1$marketplace_cali = min_dist_marketplace_cali


# monastery_cali
matrix_dist_monastery_cali <- st_distance(x=houses_cali1  , y=osm_monastery_Cali_sf)

min_dist_monastery_cali <- apply(matrix_dist_monastery_cali , 1 , min)

min_dist_monastery_cali %>% head()

houses_cali1$monastery_cali = min_dist_monastery_cali


# parking_cali
matrix_dist_parking_cali <- st_distance(x=houses_cali1  , y=osm_parking_Cali_sf)

min_dist_parking_cali <- apply(matrix_dist_parking_cali , 1 , min)

min_dist_parking_cali %>% head()

houses_cali1$parking_cali = min_dist_parking_cali



# pharmacy_cali
#matrix_dist_pharmacy_cali <- st_distance(x=houses_cali1  , y=osm_pharmacy_caliota_sf)

#min_dist_pharmacy_cali <- apply(matrix_dist_pharmacy_cali , 1 , min)

#min_dist_pharmacy_cali %>% head()

#houses_cali1$pharmacy_cali = min_dist_pharmacy_cali


# worship_cali
#matrix_dist_worship_cali <- st_distance(x=houses_cali1  , y=osm_place_of_worship_caliota_sf)

#min_dist_worship_cali <- apply(matrix_dist_worship_cali , 1 , min)

#min_dist_worship_cali %>% head()

#houses_cali1$worship_cali = min_dist_worship_cali


# police_cali
matrix_dist_police_cali <- st_distance(x=houses_cali1  , y=osm_police_Cali_sf)

min_dist_police_cali <- apply(matrix_dist_police_cali , 1 , min)

min_dist_police_cali %>% head()

houses_cali1$police_cali = min_dist_police_cali



# post_office_cali
#matrix_dist_post_office_cali <- st_distance(x=houses_cali1  , y=osm_post_office_caliota_sf)

#min_dist_post_office_cali <- apply(matrix_dist_post_office_cali , 1 , min)

#min_dist_post_office_cali %>% head()

#houses_cali1$post_office_cali = min_dist_post_office_cali


# recycling_cali
#matrix_dist_recycling_cali <- st_distance(x=houses_cali1  , y=osm_recycling_caliota_sf)

#min_dist_recycling_cali <- apply(matrix_dist_recycling_cali , 1 , min)

#min_dist_recycling_cali %>% head()

#houses_cali1$recycling_cali = min_dist_recycling_cali



### restaurant_cali
matrix_dist_restaurant_cali <- st_distance(x=houses_cali1  , y=osm_restaurant_Cali_sf)

min_dist_restaurant_cali <- apply(matrix_dist_restaurant_cali , 1 , min)

min_dist_restaurant_cali %>% head()

houses_cali1$restaurant_cali = min_dist_restaurant_cali


# school_cali
#matrix_dist_school_cali <- st_distance(x=houses_cali1  , y=osm_school_caliota_sf)

#min_dist_school_cali <- apply(matrix_dist_school_cali , 1 , min)

#min_dist_school_cali %>% head()

#houses_cali1$post_school_cali = min_dist_school_cali


# social_facility_cali
#matrix_dist_social_facility_cali <- st_distance(x=houses_cali1  , y=osm_social_facility_caliota_sf )

#min_dist_social_facility_cali <- apply(matrix_dist_social_facility_cali , 1 , min)

#min_dist_social_facility_cali %>% head()

#houses_cali1$social_facility_cali = min_dist_social_facility_cali


# theatre_cali
#matrix_dist_theatre_cali <- st_distance(x=houses_cali1  , y=osm_theatre_caliota_sf )

#min_dist_theatre_cali <- apply(matrix_dist_theatre_cali , 1 , min)

#min_dist_theatre_cali %>% head()

#houses_cali1$theatre_cali = min_dist_theatre_cali



# university_cali
#matrix_dist_university_cali <- st_distance(x=houses_cali1  , y=osm_university_caliota_sf )

#min_dist_university_cali <- apply(matrix_dist_university_cali , 1 , min)

#min_dist_university_cali %>% head()

#houses_cali1$university_cali = min_dist_university_cali


# veterinary_cali
#matrix_dist_veterinary_cali <- st_distance(x=houses_cali1  , y=osm_veterinary_caliota_sf )

#min_dist_veterinary_cali <- apply(matrix_dist_veterinary_cali , 1 , min)

#min_dist_veterinary_cali %>% head()

#houses_cali1$veterinary_cali = min_dist_veterinary_cali




### GUARDAR BASES DE DATOS DE ENTRENAMIENTO CON AMENITIES BOGOTA

write_rds(houses_cali1,'amenities_cali.RDS')


### CARGAR BASES DE DATOS DE ENTRENAMIENTO CON AMENITIES BOGOTA

houses_cali1 <- readRDS('amenities_cali.RDS')


