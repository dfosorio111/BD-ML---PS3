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
matrix_dist_arts_centre_bog <- st_distance(x=houses_inter1  , y=osm_arts_centre_Bogota_sf)

min_dist_arts_centre_bog <- apply(matrix_dist_arts_centre_bog , 1 , min)

min_dist_arts_centre_bog %>% head()

houses_inter1$arts_centre_bog = min_dist_arts_centre_bog


# bank_bog
matrix_dist_bank_bog <- st_distance(x=houses_inter1  , y=osm_bank_Bogota_sf)

min_dist_bank_bog <- apply(matrix_dist_bank_bog , 1 , min)

min_dist_bank_bog %>% head()

houses_inter1$bank_bog = min_dist_bank_bog



# bicycle_parking_bog
matrix_dist_bicycle_parking_bog <- st_distance(x=houses_inter1  , y=osm_bicycle_parking_Bogota_sf)

min_dist_bicycle_parking_bog <- apply(matrix_dist_bicycle_parking_bog , 1 , min)

min_dist_bicycle_parking_bog %>% head()

houses_inter1$bicycle_parking_bog = min_dist_bicycle_parking_bog


# bus_station_bog
matrix_dist_bus_station_bog <- st_distance(x=houses_inter1  , y=osm_bus_station_Bogota_sf)

min_dist_bus_station_bog <- apply(matrix_dist_bus_station_bog , 1 , min)

min_dist_bus_station_bog %>% head()

houses_inter1$bus_station_bog = min_dist_bus_station_bog


# casino_bog
matrix_dist_casino_bog <- st_distance(x=houses_inter1  , y=osm_casino_Bogota_sf)

min_dist_casino_bog <- apply(matrix_dist_casino_bog , 1 , min)

min_dist_casino_bog %>% head()

houses_inter1$casino_bog = min_dist_casino_bog


# childcare_bog
matrix_dist_childcare_bog <- st_distance(x=houses_inter1  , y=osm_childcare_Bogota_sf)

min_dist_childcare_bog <- apply(matrix_dist_childcare_bog , 1 , min)

min_dist_childcare_bog %>% head()

houses_inter1$childcare_bog = min_dist_childcare_bog


# cinema_bog
matrix_dist_cinema_bog <- st_distance(x=houses_inter1  , y=osm_cinema_Bogota_sf)

min_dist_cinema_bog <- apply(matrix_dist_cinema_bog , 1 , min)

min_dist_cinema_bog %>% head()

houses_inter1$cinema_bog = min_dist_cinema_bog



# clinic_bog
matrix_dist_clinic_bog <- st_distance(x=houses_inter1  , y=osm_clinic_Bogota_sf)

min_dist_clinic_bog <- apply(matrix_dist_clinic_bog , 1 , min)

min_dist_clinic_bog %>% head()

houses_inter1$clinic_bog = min_dist_clinic_bog


# college_bog
matrix_dist_college_bog <- st_distance(x=houses_inter1  , y=osm_college_Bogota_sf)

min_dist_college_bog <- apply(matrix_dist_college_bog , 1 , min)

min_dist_college_bog %>% head()

houses_inter1$college_bog = min_dist_college_bog


# community_centre_bog
matrix_dist_community_centre_bog <- st_distance(x=houses_inter1  , y=osm_community_centre_Bogota_sf)

min_dist_community_centre_bog <- apply(matrix_dist_community_centre_bog , 1 , min)

min_dist_community_centre_bog %>% head()

houses_inter1$community_centre_bog = min_dist_community_centre_bog


# conference_centre_bog
matrix_dist_conference_centre_bog <- st_distance(x=houses_inter1  , y=osm_conference_centre_Bogota_sf)

min_dist_conference_centre_bog <- apply(matrix_dist_conference_centre_bog , 1 , min)

min_dist_conference_centre_bog %>% head()

houses_inter1$conference_centre_bog = min_dist_conference_centre_bog



# dentist_bog
matrix_dist_dentist_bog <- st_distance(x=houses_inter1  , y=osm_dentist_Bogota_sf)

min_dist_dentist_bog <- apply(matrix_dist_dentist_bog , 1 , min)

min_dist_dentist_bog %>% head()

houses_inter1$dentist_bog = min_dist_dentist_bog


# doctors_bog
matrix_dist_doctors_bog <- st_distance(x=houses_inter1  , y=osm_doctors_Bogota_sf)

min_dist_doctors_bog <- apply(matrix_dist_doctors_bog , 1 , min)

min_dist_doctors_bog %>% head()

houses_inter1$doctors_bog = min_dist_doctors_bog


# events_bog
matrix_dist_events_bog <- st_distance(x=houses_inter1  , y=osm_events_venue_Bogota_sf)

min_dist_events_bog <- apply(matrix_dist_events_bog , 1 , min)

min_dist_events_bog %>% head()

houses_inter1$events_bog = min_dist_events_bog


# fast_food_bog
matrix_dist_fast_food_bog <- st_distance(x=houses_inter1  , y=osm_fast_food_Bogota_sf)

min_dist_fast_food_bog <- apply(matrix_dist_fast_food_bog , 1 , min)

min_dist_fast_food_bog %>% head()

houses_inter1$fast_food_bog = min_dist_fast_food_bog


# hospital_bog
matrix_dist_hospital_bog <- st_distance(x=houses_inter1  , y=osm_hospital_Bogota_sf)

min_dist_hospital_bog <- apply(matrix_dist_hospital_bog , 1 , min)

min_dist_hospital_bog %>% head()

houses_inter1$hospital_bog = min_dist_hospital_bog


# kindergarten_bog
matrix_dist_kindergarten_bog <- st_distance(x=houses_inter1  , y=osm_kindergarten_Bogota_sf)

min_dist_kindergarten_bog <- apply(matrix_dist_kindergarten_bog , 1 , min)

min_dist_kindergarten_bog %>% head()

houses_inter1$kindergarten_bog = min_dist_kindergarten_bog




# library_bog
matrix_dist_library_bog <- st_distance(x=houses_inter1  , y=osm_library_Bogota_sf)

min_dist_library_bog <- apply(matrix_dist_library_bog , 1 , min)

min_dist_library_bog %>% head()

houses_inter1$library_bog = min_dist_library_bog


# love_hotel_bog
matrix_dist_love_hotel_bog <- st_distance(x=houses_inter1  , y=osm_love_hotel_Bogota_sf)

min_dist_love_hotel_bog <- apply(matrix_dist_love_hotel_bog , 1 , min)

min_dist_love_hotel_bog %>% head()

houses_inter1$love_hotel_bog = min_dist_love_hotel_bog



# marketplace_bog
matrix_dist_marketplace_bog <- st_distance(x=houses_inter1  , y=osm_marketplace_Bogota_sf)

min_dist_marketplace_bog <- apply(matrix_dist_marketplace_bog , 1 , min)

min_dist_marketplace_bog %>% head()

houses_inter1$marketplace_bog = min_dist_marketplace_bog


# monastery_bog
matrix_dist_monastery_bog <- st_distance(x=houses_inter1  , y=osm_monastery_Bogota_sf)

min_dist_monastery_bog <- apply(matrix_dist_monastery_bog , 1 , min)

min_dist_monastery_bog %>% head()

houses_inter1$monastery_bog = min_dist_monastery_bog


# parking_bog
matrix_dist_parking_bog <- st_distance(x=houses_inter1  , y=osm_parking_Bogota_sf)

min_dist_parking_bog <- apply(matrix_dist_parking_bog , 1 , min)

min_dist_parking_bog %>% head()

houses_inter1$parking_bog = min_dist_parking_bog



# pharmacy_bog
matrix_dist_pharmacy_bog <- st_distance(x=houses_inter1  , y=osm_pharmacy_Bogota_sf)

min_dist_pharmacy_bog <- apply(matrix_dist_pharmacy_bog , 1 , min)

min_dist_pharmacy_bog %>% head()

houses_inter1$pharmacy_bog = min_dist_pharmacy_bog


# worship_bog
matrix_dist_worship_bog <- st_distance(x=houses_inter1  , y=osm_place_of_worship_Bogota_sf)

min_dist_worship_bog <- apply(matrix_dist_worship_bog , 1 , min)

min_dist_worship_bog %>% head()

houses_inter1$worship_bog = min_dist_worship_bog


# police_bog
matrix_dist_police_bog <- st_distance(x=houses_inter1  , y=osm_police_Bogota_sf)

min_dist_police_bog <- apply(matrix_dist_police_bog , 1 , min)

min_dist_police_bog %>% head()

houses_inter1$police_bog = min_dist_police_bog



# post_office_bog
matrix_dist_post_office_bog <- st_distance(x=houses_inter1  , y=osm_post_office_Bogota_sf)

min_dist_post_office_bog <- apply(matrix_dist_post_office_bog , 1 , min)

min_dist_post_office_bog %>% head()

houses_inter1$post_office_bog = min_dist_post_office_bog


# recycling_bog
matrix_dist_recycling_bog <- st_distance(x=houses_inter1  , y=osm_recycling_Bogota_sf)

min_dist_recycling_bog <- apply(matrix_dist_recycling_bog , 1 , min)

min_dist_recycling_bog %>% head()

houses_inter1$recycling_bog = min_dist_recycling_bog



### restaurant_bog
matrix_dist_restaurant_bog <- st_distance(x=houses_inter1  , y=osm_restaurant_Bogota_sf)

min_dist_restaurant_bog <- apply(matrix_dist_restaurant_bog , 1 , min)

min_dist_restaurant_bog %>% head()

houses_inter1$restaurant_bog = min_dist_restaurant_bog


# school_bog
matrix_dist_school_bog <- st_distance(x=houses_inter1  , y=osm_school_Bogota_sf)

min_dist_school_bog <- apply(matrix_dist_school_bog , 1 , min)

min_dist_school_bog %>% head()

houses_inter1$post_school_bog = min_dist_school_bog


# social_facility_bog
matrix_dist_social_facility_bog <- st_distance(x=houses_inter1  , y=osm_social_facility_Bogota_sf )

min_dist_social_facility_bog <- apply(matrix_dist_social_facility_bog , 1 , min)

min_dist_social_facility_bog %>% head()

houses_inter1$social_facility_bog = min_dist_social_facility_bog


# theatre_bog
matrix_dist_theatre_bog <- st_distance(x=houses_inter1  , y=osm_theatre_Bogota_sf )

min_dist_theatre_bog <- apply(matrix_dist_theatre_bog , 1 , min)

min_dist_theatre_bog %>% head()

houses_inter1$theatre_bog = min_dist_theatre_bog



# university_bog
matrix_dist_university_bog <- st_distance(x=houses_inter1  , y=osm_university_Bogota_sf )

min_dist_university_bog <- apply(matrix_dist_university_bog , 1 , min)

min_dist_university_bog %>% head()

houses_inter1$university_bog = min_dist_university_bog


# veterinary_bog
matrix_dist_veterinary_bog <- st_distance(x=houses_inter1  , y=osm_veterinary_Bogota_sf )

min_dist_veterinary_bog <- apply(matrix_dist_veterinary_bog , 1 , min)

min_dist_veterinary_bog %>% head()

houses_inter1$veterinary_bog = min_dist_veterinary_bog



### GUARDAR BASES DE DATOS DE ENTRENAMIENTO CON AMENITIES BOGOTA

write_rds(houses_inter1,'amenities_bogota.RDS')



### CARGAR BASES DE DATOS DE ENTRENAMIENTO CON AMENITIES BOGOTA

houses_inter1 <- readRDS('amenities_bogota.RDS')









### Distancia a amenities Medellin





### GUARDAR BASES DE DATOS DE ENTRENAMIENTO CON AMENITIES BOGOTA

write_rds(houses_inter2,'amenities_medellin.RDS')


### CARGAR BASES DE DATOS DE ENTRENAMIENTO CON AMENITIES BOGOTA

houses_inter2 <- readRDS('amenities_medellin.RDS')






### Distancia a amenities Cali



### GUARDAR BASES DE DATOS DE ENTRENAMIENTO CON AMENITIES BOGOTA

write_rds(houses_inter3,'amenities_cali.RDS')


### CARGAR BASES DE DATOS DE ENTRENAMIENTO CON AMENITIES BOGOTA

houses_inter3 <- readRDS('amenities_cali.RDS')


