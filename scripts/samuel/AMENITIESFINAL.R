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


## convetir DataFrame de casas en datos de entrenamiento a sf
train_sf <- st_as_sf(x = train, ## datos
                     coords=c("lon","lat"), ## coordenadas
                     crs=4326) ## CRS

cali_houses <- st_as_sf(x = test, ## datos
                     coords=c("lon","lat"), ## coordenadas
                     crs=4326) ## CRS

## Separar ciudades

bog_houses <- train_sf %>% subset(city== "Bogotá D.C")

med_houses <- train_sf %>% subset(city== "Medellín")

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

## Loop

lista_amenities_cal <- list("bank", "bus_station", "casino", "childcare", "cinema",
                         "clinic", "college", "community_centre", "conference_centre", "dentist", 
                         "doctors", "events_venue", "fast_food", "hospital",
                         "kindergarten", "library", "love_hotel", "marketplace", "monastery",
                         "parking", "police", "restaurant")

### MEDELLÍN

med_houses_sp <- med_houses%>%st_buffer(300)%>%as_Spatial()

med_houses_nb <- poly2nb(pl=med_houses_sp, queen= TRUE)

med_houses$num_vecinos <- NA
med_houses$val_vecinos <- NA

for (i in 1:length(med_houses_nb)) {
  
  med_houses$num_vecinos[i] <- size_sum(med_houses_nb[[i]])
  med_houses$val_vecinos[i] <- med_houses_nb[[i]]==0
  
}

med_houses <- med_houses %>% mutate(vecinos_final = ifelse(val_vecinos == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_vecinos)))) 

drop <- c("num_vecinos", "val_vecinos")
med_houses <- med_houses[,!names(med_houses)%in%drop]

# Bancos

med_houses <- bind_rows(med_houses, osm_bank_Medellin_sf%>%dplyr::select(osm_id,amenity))

med_houses_sp <- med_houses%>%st_buffer(300)%>%as_Spatial()

med_houses_nb <- poly2nb(pl=med_houses_sp, queen= TRUE)

med_houses$num_bank <- NA
med_houses$val_bank <- NA

for (i in 1:length(med_houses_nb)) {
  
  med_houses$num_bank[i] <- size_sum(med_houses_nb[[i]])
  med_houses$val_bank[i] <- med_houses_nb[[i]]==0
  
}

med_houses <- med_houses %>% mutate(bank_final = ifelse(val_bank == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_bank)))) 

drop <- c("num_bank", "val_bank")
med_houses <- med_houses[,!names(med_houses)%in%drop]

med_houses <- med_houses %>% mutate(bank = bank_final - vecinos_final) 

# Bus Station

med_houses <- bind_rows(med_houses, osm_bus_station_Medellin_sf%>%dplyr::select(osm_id,amenity))

med_houses_sp <- med_houses%>%st_buffer(300)%>%as_Spatial()

med_houses_nb <- poly2nb(pl=med_houses_sp, queen= TRUE)

med_houses$num_bus_station <- NA
med_houses$val_bus_station <- NA

for (i in 1:length(med_houses_nb)) {
  
  med_houses$num_bus_station[i] <- size_sum(med_houses_nb[[i]])
  med_houses$val_bus_station[i] <- med_houses_nb[[i]]==0
  
}

med_houses <- med_houses %>% mutate(bus_station_final = ifelse(val_bus_station == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_bus_station)))) 

drop <- c("num_bus_station", "val_bus_station")
med_houses <- med_houses[,!names(med_houses)%in%drop]

med_houses <- med_houses %>% mutate(bus_station = bus_station_final - vecinos_final) 

# Casino

med_houses <- bind_rows(med_houses, osm_casino_Medellin_sf%>%dplyr::select(osm_id,amenity))

med_houses_sp <- med_houses%>%st_buffer(300)%>%as_Spatial()

med_houses_nb <- poly2nb(pl=med_houses_sp, queen= TRUE)

med_houses$num_casino <- NA
med_houses$val_casino <- NA

for (i in 1:length(med_houses_nb)) {
  
  med_houses$num_casino[i] <- size_sum(med_houses_nb[[i]])
  med_houses$val_casino[i] <- med_houses_nb[[i]]==0
  
}

med_houses <- med_houses %>% mutate(casino_final = ifelse(val_casino == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_casino)))) 

drop <- c("num_casino", "val_casino")
med_houses <- med_houses[,!names(med_houses)%in%drop]

med_houses <- med_houses %>% mutate(casino = casino_final - vecinos_final) 


# Childcare

med_houses <- bind_rows(med_houses, osm_childcare_Medellin_sf%>%dplyr::select(osm_id,amenity))

med_houses_sp <- med_houses%>%st_buffer(300)%>%as_Spatial()

med_houses_nb <- poly2nb(pl=med_houses_sp, queen= TRUE)

med_houses$num_childcare <- NA
med_houses$val_childcare <- NA

for (i in 1:length(med_houses_nb)) {
  
  med_houses$num_childcare[i] <- size_sum(med_houses_nb[[i]])
  med_houses$val_childcare[i] <- med_houses_nb[[i]]==0
  
}

med_houses <- med_houses %>% mutate(childcare_final = ifelse(val_childcare == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_childcare)))) 

drop <- c("num_childcare", "val_childcare")
med_houses <- med_houses[,!names(med_houses)%in%drop]

med_houses <- med_houses %>% mutate(childcare = childcare_final - vecinos_final) 


# Cinema

med_houses <- bind_rows(med_houses, osm_cinema_Medellin_sf%>%dplyr::select(osm_id,amenity))

med_houses_sp <- med_houses%>%st_buffer(300)%>%as_Spatial()

med_houses_nb <- poly2nb(pl=med_houses_sp, queen= TRUE)

med_houses$num_cinema <- NA
med_houses$val_cinema <- NA

for (i in 1:length(med_houses_nb)) {
  
  med_houses$num_cinema[i] <- size_sum(med_houses_nb[[i]])
  med_houses$val_cinema[i] <- med_houses_nb[[i]]==0
  
}

med_houses <- med_houses %>% mutate(cinema_final = ifelse(val_cinema == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_cinema)))) 

drop <- c("num_cinema", "val_cinema")
med_houses <- med_houses[,!names(med_houses)%in%drop]

med_houses <- med_houses %>% mutate(cinema = cinema_final - vecinos_final) 


# clinic

med_houses <- bind_rows(med_houses, osm_clinic_Medellin_sf%>%dplyr::select(osm_id,amenity))

med_houses_sp <- med_houses%>%st_buffer(300)%>%as_Spatial()

med_houses_nb <- poly2nb(pl=med_houses_sp, queen= TRUE)

med_houses$num_clinic <- NA
med_houses$val_clinic <- NA

for (i in 1:length(med_houses_nb)) {
  
  med_houses$num_clinic[i] <- size_sum(med_houses_nb[[i]])
  med_houses$val_clinic[i] <- med_houses_nb[[i]]==0
  
}

med_houses <- med_houses %>% mutate(clinic_final = ifelse(val_clinic == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_clinic)))) 

drop <- c("num_clinic", "val_clinic")
med_houses <- med_houses[,!names(med_houses)%in%drop]

med_houses <- med_houses %>% mutate(clinic = clinic_final - vecinos_final) 


# college

med_houses <- bind_rows(med_houses, osm_college_Medellin_sf%>%dplyr::select(osm_id,amenity))

med_houses_sp <- med_houses%>%st_buffer(300)%>%as_Spatial()

med_houses_nb <- poly2nb(pl=med_houses_sp, queen= TRUE)

med_houses$num_college <- NA
med_houses$val_college <- NA

for (i in 1:length(med_houses_nb)) {
  
  med_houses$num_college[i] <- size_sum(med_houses_nb[[i]])
  med_houses$val_college[i] <- med_houses_nb[[i]]==0
  
}

med_houses <- med_houses %>% mutate(college_final = ifelse(val_college == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_college)))) 

drop <- c("num_college", "val_college")
med_houses <- med_houses[,!names(med_houses)%in%drop]

med_houses <- med_houses %>% mutate(college = college_final - vecinos_final) 


# community_centre

med_houses <- bind_rows(med_houses, osm_community_centre_Medellin_sf%>%dplyr::select(osm_id,amenity))

med_houses_sp <- med_houses%>%st_buffer(300)%>%as_Spatial()

med_houses_nb <- poly2nb(pl=med_houses_sp, queen= TRUE)

med_houses$num_community_centre <- NA
med_houses$val_community_centre <- NA

for (i in 1:length(med_houses_nb)) {
  
  med_houses$num_community_centre[i] <- size_sum(med_houses_nb[[i]])
  med_houses$val_community_centre[i] <- med_houses_nb[[i]]==0
  
}

med_houses <- med_houses %>% mutate(community_centre_final = ifelse(val_community_centre == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_community_centre)))) 

drop <- c("num_community_centre", "val_community_centre")
med_houses <- med_houses[,!names(med_houses)%in%drop]

med_houses <- med_houses %>% mutate(community_centre = community_centre_final - vecinos_final) 



# conference_centre

med_houses <- bind_rows(med_houses, osm_conference_centre_Medellin_sf%>%dplyr::select(osm_id,amenity))

med_houses_sp <- med_houses%>%st_buffer(300)%>%as_Spatial()

med_houses_nb <- poly2nb(pl=med_houses_sp, queen= TRUE)

med_houses$num_conference_centre <- NA
med_houses$val_conference_centre <- NA

for (i in 1:length(med_houses_nb)) {
  
  med_houses$num_conference_centre[i] <- size_sum(med_houses_nb[[i]])
  med_houses$val_conference_centre[i] <- med_houses_nb[[i]]==0
  
}

med_houses <- med_houses %>% mutate(conference_centre_final = ifelse(val_conference_centre == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_conference_centre)))) 

drop <- c("num_conference_centre", "val_conference_centre")
med_houses <- med_houses[,!names(med_houses)%in%drop]

med_houses <- med_houses %>% mutate(conference_centre = conference_centre_final - vecinos_final) 



# dentist

med_houses <- bind_rows(med_houses, osm_dentist_Medellin_sf%>%dplyr::select(osm_id,amenity))

med_houses_sp <- med_houses%>%st_buffer(300)%>%as_Spatial()

med_houses_nb <- poly2nb(pl=med_houses_sp, queen= TRUE)

med_houses$num_dentist <- NA
med_houses$val_dentist <- NA

for (i in 1:length(med_houses_nb)) {
  
  med_houses$num_dentist[i] <- size_sum(med_houses_nb[[i]])
  med_houses$val_dentist[i] <- med_houses_nb[[i]]==0
  
}

med_houses <- med_houses %>% mutate(dentist_final = ifelse(val_dentist == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_dentist)))) 

drop <- c("num_dentist", "val_dentist")
med_houses <- med_houses[,!names(med_houses)%in%drop]

med_houses <- med_houses %>% mutate(dentist = dentist_final - vecinos_final) 


# doctors

med_houses <- bind_rows(med_houses, osm_doctors_Medellin_sf%>%dplyr::select(osm_id,amenity))

med_houses_sp <- med_houses%>%st_buffer(300)%>%as_Spatial()

med_houses_nb <- poly2nb(pl=med_houses_sp, queen= TRUE)

med_houses$num_doctors <- NA
med_houses$val_doctors <- NA

for (i in 1:length(med_houses_nb)) {
  
  med_houses$num_doctors[i] <- size_sum(med_houses_nb[[i]])
  med_houses$val_doctors[i] <- med_houses_nb[[i]]==0
  
}

med_houses <- med_houses %>% mutate(doctors_final = ifelse(val_doctors == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_doctors)))) 

drop <- c("num_doctors", "val_doctors")
med_houses <- med_houses[,!names(med_houses)%in%drop]

med_houses <- med_houses %>% mutate(doctors = doctors_final - vecinos_final) 


# events_venue

med_houses <- bind_rows(med_houses, osm_events_venue_Medellin_sf%>%dplyr::select(osm_id,amenity))

med_houses_sp <- med_houses%>%st_buffer(300)%>%as_Spatial()

med_houses_nb <- poly2nb(pl=med_houses_sp, queen= TRUE)

med_houses$num_events_venue <- NA
med_houses$val_events_venue <- NA

for (i in 1:length(med_houses_nb)) {
  
  med_houses$num_events_venue[i] <- size_sum(med_houses_nb[[i]])
  med_houses$val_events_venue[i] <- med_houses_nb[[i]]==0
  
}

med_houses <- med_houses %>% mutate(events_venue_final = ifelse(val_events_venue == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_events_venue)))) 

drop <- c("num_events_venue", "val_events_venue")
med_houses <- med_houses[,!names(med_houses)%in%drop]

med_houses <- med_houses %>% mutate(events_venue = events_venue_final - vecinos_final) 


# fast_food

med_houses <- bind_rows(med_houses, osm_fast_food_Medellin_sf%>%dplyr::select(osm_id,amenity))

med_houses_sp <- med_houses%>%st_buffer(300)%>%as_Spatial()

med_houses_nb <- poly2nb(pl=med_houses_sp, queen= TRUE)

med_houses$num_fast_food <- NA
med_houses$val_fast_food <- NA

for (i in 1:length(med_houses_nb)) {
  
  med_houses$num_fast_food[i] <- size_sum(med_houses_nb[[i]])
  med_houses$val_fast_food[i] <- med_houses_nb[[i]]==0
  
}

med_houses <- med_houses %>% mutate(fast_food_final = ifelse(val_fast_food == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_fast_food)))) 

drop <- c("num_fast_food", "val_fast_food")
med_houses <- med_houses[,!names(med_houses)%in%drop]

med_houses <- med_houses %>% mutate(fast_food = fast_food_final - vecinos_final) 


# hospital

med_houses <- bind_rows(med_houses, osm_hospital_Medellin_sf%>%dplyr::select(osm_id,amenity))

med_houses_sp <- med_houses%>%st_buffer(300)%>%as_Spatial()

med_houses_nb <- poly2nb(pl=med_houses_sp, queen= TRUE)

med_houses$num_hospital <- NA
med_houses$val_hospital <- NA

for (i in 1:length(med_houses_nb)) {
  
  med_houses$num_hospital[i] <- size_sum(med_houses_nb[[i]])
  med_houses$val_hospital[i] <- med_houses_nb[[i]]==0
  
}

med_houses <- med_houses %>% mutate(hospital_final = ifelse(val_hospital == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_hospital)))) 

drop <- c("num_hospital", "val_hospital")
med_houses <- med_houses[,!names(med_houses)%in%drop]

med_houses <- med_houses %>% mutate(hospital = hospital_final - vecinos_final) 


# kindergarten

med_houses <- bind_rows(med_houses, osm_kindergarten_Medellin_sf%>%dplyr::select(osm_id,amenity))

med_houses_sp <- med_houses%>%st_buffer(300)%>%as_Spatial()

med_houses_nb <- poly2nb(pl=med_houses_sp, queen= TRUE)

med_houses$num_kindergarten <- NA
med_houses$val_kindergarten <- NA

for (i in 1:length(med_houses_nb)) {
  
  med_houses$num_kindergarten[i] <- size_sum(med_houses_nb[[i]])
  med_houses$val_kindergarten[i] <- med_houses_nb[[i]]==0
  
}

med_houses <- med_houses %>% mutate(kindergarten_final = ifelse(val_kindergarten == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_kindergarten)))) 

drop <- c("num_kindergarten", "val_kindergarten")
med_houses <- med_houses[,!names(med_houses)%in%drop]

med_houses <- med_houses %>% mutate(kindergarten = kindergarten_final - vecinos_final)


# library

med_houses <- bind_rows(med_houses, osm_library_Medellin_sf%>%dplyr::select(osm_id,amenity))

med_houses_sp <- med_houses%>%st_buffer(300)%>%as_Spatial()

med_houses_nb <- poly2nb(pl=med_houses_sp, queen= TRUE)

med_houses$num_library <- NA
med_houses$val_library <- NA

for (i in 1:length(med_houses_nb)) {
  
  med_houses$num_library[i] <- size_sum(med_houses_nb[[i]])
  med_houses$val_library[i] <- med_houses_nb[[i]]==0
  
}

med_houses <- med_houses %>% mutate(library_final = ifelse(val_library == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_library)))) 

drop <- c("num_library", "val_library")
med_houses <- med_houses[,!names(med_houses)%in%drop]

med_houses <- med_houses %>% mutate(library = library_final - vecinos_final) 



# love_hotel

med_houses <- bind_rows(med_houses, osm_love_hotel_Medellin_sf%>%dplyr::select(osm_id,amenity))

med_houses_sp <- med_houses%>%st_buffer(300)%>%as_Spatial()

med_houses_nb <- poly2nb(pl=med_houses_sp, queen= TRUE)

med_houses$num_love_hotel <- NA
med_houses$val_love_hotel <- NA

for (i in 1:length(med_houses_nb)) {
  
  med_houses$num_love_hotel[i] <- size_sum(med_houses_nb[[i]])
  med_houses$val_love_hotel[i] <- med_houses_nb[[i]]==0
  
}

med_houses <- med_houses %>% mutate(love_hotel_final = ifelse(val_love_hotel == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_love_hotel)))) 

drop <- c("num_love_hotel", "val_love_hotel")
med_houses <- med_houses[,!names(med_houses)%in%drop]

med_houses <- med_houses %>% mutate(love_hotel = love_hotel_final - vecinos_final) 


# marketplace

med_houses <- bind_rows(med_houses, osm_marketplace_Medellin_sf%>%dplyr::select(osm_id,amenity))

med_houses_sp <- med_houses%>%st_buffer(300)%>%as_Spatial()

med_houses_nb <- poly2nb(pl=med_houses_sp, queen= TRUE)

med_houses$num_marketplace <- NA
med_houses$val_marketplace <- NA

for (i in 1:length(med_houses_nb)) {
  
  med_houses$num_marketplace[i] <- size_sum(med_houses_nb[[i]])
  med_houses$val_marketplace[i] <- med_houses_nb[[i]]==0
  
}

med_houses <- med_houses %>% mutate(marketplace_final = ifelse(val_marketplace == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_marketplace)))) 

drop <- c("num_marketplace", "val_marketplace")
med_houses <- med_houses[,!names(med_houses)%in%drop]

med_houses <- med_houses %>% mutate(marketplace = marketplace_final - vecinos_final) 



# monastery

med_houses <- bind_rows(med_houses, osm_monastery_Medellin_sf%>%dplyr::select(osm_id,amenity))

med_houses_sp <- med_houses%>%st_buffer(300)%>%as_Spatial()

med_houses_nb <- poly2nb(pl=med_houses_sp, queen= TRUE)

med_houses$num_monastery <- NA
med_houses$val_monastery <- NA

for (i in 1:length(med_houses_nb)) {
  
  med_houses$num_monastery[i] <- size_sum(med_houses_nb[[i]])
  med_houses$val_monastery[i] <- med_houses_nb[[i]]==0
  
}

med_houses <- med_houses %>% mutate(monastery_final = ifelse(val_monastery == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_monastery)))) 

drop <- c("num_monastery", "val_monastery")
med_houses <- med_houses[,!names(med_houses)%in%drop]

med_houses <- med_houses %>% mutate(monastery = monastery_final - vecinos_final) 


# parking

med_houses <- bind_rows(med_houses, osm_parking_Medellin_sf%>%dplyr::select(osm_id,amenity))

med_houses_sp <- med_houses%>%st_buffer(300)%>%as_Spatial()

med_houses_nb <- poly2nb(pl=med_houses_sp, queen= TRUE)

med_houses$num_parking <- NA
med_houses$val_parking <- NA

for (i in 1:length(med_houses_nb)) {
  
  med_houses$num_parking[i] <- size_sum(med_houses_nb[[i]])
  med_houses$val_parking[i] <- med_houses_nb[[i]]==0
  
}

med_houses <- med_houses %>% mutate(parking_final = ifelse(val_parking == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_parking)))) 

drop <- c("num_parking", "val_parking")
med_houses <- med_houses[,!names(med_houses)%in%drop]

med_houses <- med_houses %>% mutate(parking = parking_final - vecinos_final) 


# police

med_houses <- bind_rows(med_houses, osm_police_Medellin_sf%>%dplyr::select(osm_id,amenity))

med_houses_sp <- med_houses%>%st_buffer(300)%>%as_Spatial()

med_houses_nb <- poly2nb(pl=med_houses_sp, queen= TRUE)

med_houses$num_police <- NA
med_houses$val_police <- NA

for (i in 1:length(med_houses_nb)) {
  
  med_houses$num_police[i] <- size_sum(med_houses_nb[[i]])
  med_houses$val_police[i] <- med_houses_nb[[i]]==0
  
}

med_houses <- med_houses %>% mutate(police_final = ifelse(val_police == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_police)))) 

drop <- c("num_police", "val_police")
med_houses <- med_houses[,!names(med_houses)%in%drop]

med_houses <- med_houses %>% mutate(police = police_final - vecinos_final) 


# restaurant

med_houses <- bind_rows(med_houses, osm_restaurant_Medellin_sf%>%dplyr::select(osm_id,amenity))

med_houses_sp <- med_houses%>%st_buffer(300)%>%as_Spatial()

med_houses_nb <- poly2nb(pl=med_houses_sp, queen= TRUE)

med_houses$num_restaurant <- NA
med_houses$val_restaurant <- NA

for (i in 1:length(med_houses_nb)) {
  
  med_houses$num_restaurant[i] <- size_sum(med_houses_nb[[i]])
  med_houses$val_restaurant[i] <- med_houses_nb[[i]]==0
  
}

med_houses <- med_houses %>% mutate(restaurant_final = ifelse(val_restaurant == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_restaurant)))) 

drop <- c("num_restaurant", "val_restaurant")
med_houses <- med_houses[,!names(med_houses)%in%drop]

med_houses <- med_houses %>% mutate(restaurant = restaurant_final - vecinos_final) 

write_rds(med_houses, "med_houses_1.RDS")

med_houses_2 <- med_houses %>% select(property_id, bank, bus_station, casino, childcare, cinema,
                                      clinic, college, community_centre, conference_centre, dentist, 
                                      doctors, events_venue, fast_food, hospital,
                                      kindergarten, library, love_hotel, marketplace, monastery,
                                      parking, police, restaurant)

write_rds(med_houses_2, "med_houses_2.RDS")

write_csv(med_houses_2, "med_houses_2.csv")


### Cali

cali_houses_sp <- cali_houses%>%st_buffer(300)%>%as_Spatial()

cali_houses_nb <- poly2nb(pl=cali_houses_sp, queen= TRUE)

cali_houses$num_vecinos <- NA
cali_houses$val_vecinos <- NA

for (i in 1:length(cali_houses_nb)) {
  
  cali_houses$num_vecinos[i] <- size_sum(cali_houses_nb[[i]])
  cali_houses$val_vecinos[i] <- cali_houses_nb[[i]]==0
  
}

cali_houses <- cali_houses %>% mutate(vecinos_final = ifelse(val_vecinos == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_vecinos)))) 

drop <- c("num_vecinos", "val_vecinos")
cali_houses <- cali_houses[,!names(cali_houses)%in%drop]

# Bancos

cali_houses <- bind_rows(cali_houses, osm_bank_Cali_sf%>%dplyr::select(osm_id,amenity))

cali_houses_sp <- cali_houses%>%st_buffer(300)%>%as_Spatial()

cali_houses_nb <- poly2nb(pl=cali_houses_sp, queen= TRUE)

cali_houses$num_bank <- NA
cali_houses$val_bank <- NA

for (i in 1:length(cali_houses_nb)) {
  
  cali_houses$num_bank[i] <- size_sum(cali_houses_nb[[i]])
  cali_houses$val_bank[i] <- cali_houses_nb[[i]]==0
  
}

cali_houses <- cali_houses %>% mutate(bank_final = ifelse(val_bank == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_bank)))) 

drop <- c("num_bank", "val_bank")
cali_houses <- cali_houses[,!names(cali_houses)%in%drop]

cali_houses <- cali_houses %>% mutate(bank = bank_final - vecinos_final) 

# Bus Station

cali_houses <- bind_rows(cali_houses, osm_bus_station_Cali_sf%>%dplyr::select(osm_id,amenity))

cali_houses_sp <- cali_houses%>%st_buffer(300)%>%as_Spatial()

cali_houses_nb <- poly2nb(pl=cali_houses_sp, queen= TRUE)

cali_houses$num_bus_station <- NA
cali_houses$val_bus_station <- NA

for (i in 1:length(cali_houses_nb)) {
  
  cali_houses$num_bus_station[i] <- size_sum(cali_houses_nb[[i]])
  cali_houses$val_bus_station[i] <- cali_houses_nb[[i]]==0
  
}

cali_houses <- cali_houses %>% mutate(bus_station_final = ifelse(val_bus_station == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_bus_station)))) 

drop <- c("num_bus_station", "val_bus_station")
cali_houses <- cali_houses[,!names(cali_houses)%in%drop]

cali_houses <- cali_houses %>% mutate(bus_station = bus_station_final - vecinos_final) 

# Casino

cali_houses <- bind_rows(cali_houses, osm_casino_Cali_sf%>%dplyr::select(osm_id,amenity))

cali_houses_sp <- cali_houses%>%st_buffer(300)%>%as_Spatial()

cali_houses_nb <- poly2nb(pl=cali_houses_sp, queen= TRUE)

cali_houses$num_casino <- NA
cali_houses$val_casino <- NA

for (i in 1:length(cali_houses_nb)) {
  
  cali_houses$num_casino[i] <- size_sum(cali_houses_nb[[i]])
  cali_houses$val_casino[i] <- cali_houses_nb[[i]]==0
  
}

cali_houses <- cali_houses %>% mutate(casino_final = ifelse(val_casino == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_casino)))) 

drop <- c("num_casino", "val_casino")
cali_houses <- cali_houses[,!names(cali_houses)%in%drop]

cali_houses <- cali_houses %>% mutate(casino = casino_final - vecinos_final) 


# Childcare

cali_houses <- bind_rows(cali_houses, osm_childcare_Cali_sf%>%dplyr::select(osm_id,amenity))

cali_houses_sp <- cali_houses%>%st_buffer(300)%>%as_Spatial()

cali_houses_nb <- poly2nb(pl=cali_houses_sp, queen= TRUE)

cali_houses$num_childcare <- NA
cali_houses$val_childcare <- NA

for (i in 1:length(cali_houses_nb)) {
  
  cali_houses$num_childcare[i] <- size_sum(cali_houses_nb[[i]])
  cali_houses$val_childcare[i] <- cali_houses_nb[[i]]==0
  
}

cali_houses <- cali_houses %>% mutate(childcare_final = ifelse(val_childcare == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_childcare)))) 

drop <- c("num_childcare", "val_childcare")
cali_houses <- cali_houses[,!names(cali_houses)%in%drop]

cali_houses <- cali_houses %>% mutate(childcare = childcare_final - vecinos_final) 


# Cinema

cali_houses <- bind_rows(cali_houses, osm_cinema_Cali_sf%>%dplyr::select(osm_id,amenity))

cali_houses_sp <- cali_houses%>%st_buffer(300)%>%as_Spatial()

cali_houses_nb <- poly2nb(pl=cali_houses_sp, queen= TRUE)

cali_houses$num_cinema <- NA
cali_houses$val_cinema <- NA

for (i in 1:length(cali_houses_nb)) {
  
  cali_houses$num_cinema[i] <- size_sum(cali_houses_nb[[i]])
  cali_houses$val_cinema[i] <- cali_houses_nb[[i]]==0
  
}

cali_houses <- cali_houses %>% mutate(cinema_final = ifelse(val_cinema == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_cinema)))) 

drop <- c("num_cinema", "val_cinema")
cali_houses <- cali_houses[,!names(cali_houses)%in%drop]

cali_houses <- cali_houses %>% mutate(cinema = cinema_final - vecinos_final) 


# clinic

cali_houses <- bind_rows(cali_houses, osm_clinic_Cali_sf%>%dplyr::select(osm_id,amenity))

cali_houses_sp <- cali_houses%>%st_buffer(300)%>%as_Spatial()

cali_houses_nb <- poly2nb(pl=cali_houses_sp, queen= TRUE)

cali_houses$num_clinic <- NA
cali_houses$val_clinic <- NA

for (i in 1:length(cali_houses_nb)) {
  
  cali_houses$num_clinic[i] <- size_sum(cali_houses_nb[[i]])
  cali_houses$val_clinic[i] <- cali_houses_nb[[i]]==0
  
}

cali_houses <- cali_houses %>% mutate(clinic_final = ifelse(val_clinic == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_clinic)))) 

drop <- c("num_clinic", "val_clinic")
cali_houses <- cali_houses[,!names(cali_houses)%in%drop]

cali_houses <- cali_houses %>% mutate(clinic = clinic_final - vecinos_final) 


# college

cali_houses <- bind_rows(cali_houses, osm_college_Cali_sf%>%dplyr::select(osm_id,amenity))

cali_houses_sp <- cali_houses%>%st_buffer(300)%>%as_Spatial()

cali_houses_nb <- poly2nb(pl=cali_houses_sp, queen= TRUE)

cali_houses$num_college <- NA
cali_houses$val_college <- NA

for (i in 1:length(cali_houses_nb)) {
  
  cali_houses$num_college[i] <- size_sum(cali_houses_nb[[i]])
  cali_houses$val_college[i] <- cali_houses_nb[[i]]==0
  
}

cali_houses <- cali_houses %>% mutate(college_final = ifelse(val_college == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_college)))) 

drop <- c("num_college", "val_college")
cali_houses <- cali_houses[,!names(cali_houses)%in%drop]

cali_houses <- cali_houses %>% mutate(college = college_final - vecinos_final) 


# community_centre

cali_houses <- bind_rows(cali_houses, osm_community_centre_Cali_sf%>%dplyr::select(osm_id,amenity))

cali_houses_sp <- cali_houses%>%st_buffer(300)%>%as_Spatial()

cali_houses_nb <- poly2nb(pl=cali_houses_sp, queen= TRUE)

cali_houses$num_community_centre <- NA
cali_houses$val_community_centre <- NA

for (i in 1:length(cali_houses_nb)) {
  
  cali_houses$num_community_centre[i] <- size_sum(cali_houses_nb[[i]])
  cali_houses$val_community_centre[i] <- cali_houses_nb[[i]]==0
  
}

cali_houses <- cali_houses %>% mutate(community_centre_final = ifelse(val_community_centre == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_community_centre)))) 

drop <- c("num_community_centre", "val_community_centre")
cali_houses <- cali_houses[,!names(cali_houses)%in%drop]

cali_houses <- cali_houses %>% mutate(community_centre = community_centre_final - vecinos_final) 



# conference_centre

cali_houses <- bind_rows(cali_houses, osm_conference_centre_Cali_sf%>%dplyr::select(osm_id,amenity))

cali_houses_sp <- cali_houses%>%st_buffer(300)%>%as_Spatial()

cali_houses_nb <- poly2nb(pl=cali_houses_sp, queen= TRUE)

cali_houses$num_conference_centre <- NA
cali_houses$val_conference_centre <- NA

for (i in 1:length(cali_houses_nb)) {
  
  cali_houses$num_conference_centre[i] <- size_sum(cali_houses_nb[[i]])
  cali_houses$val_conference_centre[i] <- cali_houses_nb[[i]]==0
  
}

cali_houses <- cali_houses %>% mutate(conference_centre_final = ifelse(val_conference_centre == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_conference_centre)))) 

drop <- c("num_conference_centre", "val_conference_centre")
cali_houses <- cali_houses[,!names(cali_houses)%in%drop]

cali_houses <- cali_houses %>% mutate(conference_centre = conference_centre_final - vecinos_final) 



# dentist

cali_houses <- bind_rows(cali_houses, osm_dentist_Cali_sf%>%dplyr::select(osm_id,amenity))

cali_houses_sp <- cali_houses%>%st_buffer(300)%>%as_Spatial()

cali_houses_nb <- poly2nb(pl=cali_houses_sp, queen= TRUE)

cali_houses$num_dentist <- NA
cali_houses$val_dentist <- NA

for (i in 1:length(cali_houses_nb)) {
  
  cali_houses$num_dentist[i] <- size_sum(cali_houses_nb[[i]])
  cali_houses$val_dentist[i] <- cali_houses_nb[[i]]==0
  
}

cali_houses <- cali_houses %>% mutate(dentist_final = ifelse(val_dentist == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_dentist)))) 

drop <- c("num_dentist", "val_dentist")
cali_houses <- cali_houses[,!names(cali_houses)%in%drop]

cali_houses <- cali_houses %>% mutate(dentist = dentist_final - vecinos_final) 


# doctors

cali_houses <- bind_rows(cali_houses, osm_doctors_Cali_sf%>%dplyr::select(osm_id,amenity))

cali_houses_sp <- cali_houses%>%st_buffer(300)%>%as_Spatial()

cali_houses_nb <- poly2nb(pl=cali_houses_sp, queen= TRUE)

cali_houses$num_doctors <- NA
cali_houses$val_doctors <- NA

for (i in 1:length(cali_houses_nb)) {
  
  cali_houses$num_doctors[i] <- size_sum(cali_houses_nb[[i]])
  cali_houses$val_doctors[i] <- cali_houses_nb[[i]]==0
  
}

cali_houses <- cali_houses %>% mutate(doctors_final = ifelse(val_doctors == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_doctors)))) 

drop <- c("num_doctors", "val_doctors")
cali_houses <- cali_houses[,!names(cali_houses)%in%drop]

cali_houses <- cali_houses %>% mutate(doctors = doctors_final - vecinos_final) 


# events_venue

cali_houses <- bind_rows(cali_houses, osm_events_venue_Cali_sf%>%dplyr::select(osm_id,amenity))

cali_houses_sp <- cali_houses%>%st_buffer(300)%>%as_Spatial()

cali_houses_nb <- poly2nb(pl=cali_houses_sp, queen= TRUE)

cali_houses$num_events_venue <- NA
cali_houses$val_events_venue <- NA

for (i in 1:length(cali_houses_nb)) {
  
  cali_houses$num_events_venue[i] <- size_sum(cali_houses_nb[[i]])
  cali_houses$val_events_venue[i] <- cali_houses_nb[[i]]==0
  
}

cali_houses <- cali_houses %>% mutate(events_venue_final = ifelse(val_events_venue == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_events_venue)))) 

drop <- c("num_events_venue", "val_events_venue")
cali_houses <- cali_houses[,!names(cali_houses)%in%drop]

cali_houses <- cali_houses %>% mutate(events_venue = events_venue_final - vecinos_final) 


# fast_food

cali_houses <- bind_rows(cali_houses, osm_fast_food_Cali_sf%>%dplyr::select(osm_id,amenity))

cali_houses_sp <- cali_houses%>%st_buffer(300)%>%as_Spatial()

cali_houses_nb <- poly2nb(pl=cali_houses_sp, queen= TRUE)

cali_houses$num_fast_food <- NA
cali_houses$val_fast_food <- NA

for (i in 1:length(cali_houses_nb)) {
  
  cali_houses$num_fast_food[i] <- size_sum(cali_houses_nb[[i]])
  cali_houses$val_fast_food[i] <- cali_houses_nb[[i]]==0
  
}

cali_houses <- cali_houses %>% mutate(fast_food_final = ifelse(val_fast_food == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_fast_food)))) 

drop <- c("num_fast_food", "val_fast_food")
cali_houses <- cali_houses[,!names(cali_houses)%in%drop]

cali_houses <- cali_houses %>% mutate(fast_food = fast_food_final - vecinos_final) 


# hospital

cali_houses <- bind_rows(cali_houses, osm_hospital_Cali_sf%>%dplyr::select(osm_id,amenity))

cali_houses_sp <- cali_houses%>%st_buffer(300)%>%as_Spatial()

cali_houses_nb <- poly2nb(pl=cali_houses_sp, queen= TRUE)

cali_houses$num_hospital <- NA
cali_houses$val_hospital <- NA

for (i in 1:length(cali_houses_nb)) {
  
  cali_houses$num_hospital[i] <- size_sum(cali_houses_nb[[i]])
  cali_houses$val_hospital[i] <- cali_houses_nb[[i]]==0
  
}

cali_houses <- cali_houses %>% mutate(hospital_final = ifelse(val_hospital == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_hospital)))) 

drop <- c("num_hospital", "val_hospital")
cali_houses <- cali_houses[,!names(cali_houses)%in%drop]

cali_houses <- cali_houses %>% mutate(hospital = hospital_final - vecinos_final) 


# kindergarten

cali_houses <- bind_rows(cali_houses, osm_kindergarten_Cali_sf%>%dplyr::select(osm_id,amenity))

cali_houses_sp <- cali_houses%>%st_buffer(300)%>%as_Spatial()

cali_houses_nb <- poly2nb(pl=cali_houses_sp, queen= TRUE)

cali_houses$num_kindergarten <- NA
cali_houses$val_kindergarten <- NA

for (i in 1:length(cali_houses_nb)) {
  
  cali_houses$num_kindergarten[i] <- size_sum(cali_houses_nb[[i]])
  cali_houses$val_kindergarten[i] <- cali_houses_nb[[i]]==0
  
}

cali_houses <- cali_houses %>% mutate(kindergarten_final = ifelse(val_kindergarten == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_kindergarten)))) 

drop <- c("num_kindergarten", "val_kindergarten")
cali_houses <- cali_houses[,!names(cali_houses)%in%drop]

cali_houses <- cali_houses %>% mutate(kindergarten = kindergarten_final - vecinos_final)


# library

cali_houses <- bind_rows(cali_houses, osm_library_Cali_sf%>%dplyr::select(osm_id,amenity))

cali_houses_sp <- cali_houses%>%st_buffer(300)%>%as_Spatial()

cali_houses_nb <- poly2nb(pl=cali_houses_sp, queen= TRUE)

cali_houses$num_library <- NA
cali_houses$val_library <- NA

for (i in 1:length(cali_houses_nb)) {
  
  cali_houses$num_library[i] <- size_sum(cali_houses_nb[[i]])
  cali_houses$val_library[i] <- cali_houses_nb[[i]]==0
  
}

cali_houses <- cali_houses %>% mutate(library_final = ifelse(val_library == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_library)))) 

drop <- c("num_library", "val_library")
cali_houses <- cali_houses[,!names(cali_houses)%in%drop]

cali_houses <- cali_houses %>% mutate(library = library_final - vecinos_final) 



# love_hotel

cali_houses <- bind_rows(cali_houses, osm_love_hotel_Cali_sf%>%dplyr::select(osm_id,amenity))

cali_houses_sp <- cali_houses%>%st_buffer(300)%>%as_Spatial()

cali_houses_nb <- poly2nb(pl=cali_houses_sp, queen= TRUE)

cali_houses$num_love_hotel <- NA
cali_houses$val_love_hotel <- NA

for (i in 1:length(cali_houses_nb)) {
  
  cali_houses$num_love_hotel[i] <- size_sum(cali_houses_nb[[i]])
  cali_houses$val_love_hotel[i] <- cali_houses_nb[[i]]==0
  
}

cali_houses <- cali_houses %>% mutate(love_hotel_final = ifelse(val_love_hotel == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_love_hotel)))) 

drop <- c("num_love_hotel", "val_love_hotel")
cali_houses <- cali_houses[,!names(cali_houses)%in%drop]

cali_houses <- cali_houses %>% mutate(love_hotel = love_hotel_final - vecinos_final) 


# marketplace

cali_houses <- bind_rows(cali_houses, osm_marketplace_Cali_sf%>%dplyr::select(osm_id,amenity))

cali_houses_sp <- cali_houses%>%st_buffer(300)%>%as_Spatial()

cali_houses_nb <- poly2nb(pl=cali_houses_sp, queen= TRUE)

cali_houses$num_marketplace <- NA
cali_houses$val_marketplace <- NA

for (i in 1:length(cali_houses_nb)) {
  
  cali_houses$num_marketplace[i] <- size_sum(cali_houses_nb[[i]])
  cali_houses$val_marketplace[i] <- cali_houses_nb[[i]]==0
  
}

cali_houses <- cali_houses %>% mutate(marketplace_final = ifelse(val_marketplace == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_marketplace)))) 

drop <- c("num_marketplace", "val_marketplace")
cali_houses <- cali_houses[,!names(cali_houses)%in%drop]

cali_houses <- cali_houses %>% mutate(marketplace = marketplace_final - vecinos_final) 



# monastery

cali_houses <- bind_rows(cali_houses, osm_monastery_Cali_sf%>%dplyr::select(osm_id,amenity))

cali_houses_sp <- cali_houses%>%st_buffer(300)%>%as_Spatial()

cali_houses_nb <- poly2nb(pl=cali_houses_sp, queen= TRUE)

cali_houses$num_monastery <- NA
cali_houses$val_monastery <- NA

for (i in 1:length(cali_houses_nb)) {
  
  cali_houses$num_monastery[i] <- size_sum(cali_houses_nb[[i]])
  cali_houses$val_monastery[i] <- cali_houses_nb[[i]]==0
  
}

cali_houses <- cali_houses %>% mutate(monastery_final = ifelse(val_monastery == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_monastery)))) 

drop <- c("num_monastery", "val_monastery")
cali_houses <- cali_houses[,!names(cali_houses)%in%drop]

cali_houses <- cali_houses %>% mutate(monastery = monastery_final - vecinos_final) 


# parking

cali_houses <- bind_rows(cali_houses, osm_parking_Cali_sf%>%dplyr::select(osm_id,amenity))

cali_houses_sp <- cali_houses%>%st_buffer(300)%>%as_Spatial()

cali_houses_nb <- poly2nb(pl=cali_houses_sp, queen= TRUE)

cali_houses$num_parking <- NA
cali_houses$val_parking <- NA

for (i in 1:length(cali_houses_nb)) {
  
  cali_houses$num_parking[i] <- size_sum(cali_houses_nb[[i]])
  cali_houses$val_parking[i] <- cali_houses_nb[[i]]==0
  
}

cali_houses <- cali_houses %>% mutate(parking_final = ifelse(val_parking == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_parking)))) 

drop <- c("num_parking", "val_parking")
cali_houses <- cali_houses[,!names(cali_houses)%in%drop]

cali_houses <- cali_houses %>% mutate(parking = parking_final - vecinos_final) 


# police

cali_houses <- bind_rows(cali_houses, osm_police_Cali_sf%>%dplyr::select(osm_id,amenity))

cali_houses_sp <- cali_houses%>%st_buffer(300)%>%as_Spatial()

cali_houses_nb <- poly2nb(pl=cali_houses_sp, queen= TRUE)

cali_houses$num_police <- NA
cali_houses$val_police <- NA

for (i in 1:length(cali_houses_nb)) {
  
  cali_houses$num_police[i] <- size_sum(cali_houses_nb[[i]])
  cali_houses$val_police[i] <- cali_houses_nb[[i]]==0
  
}

cali_houses <- cali_houses %>% mutate(police_final = ifelse(val_police == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_police)))) 

drop <- c("num_police", "val_police")
cali_houses <- cali_houses[,!names(cali_houses)%in%drop]

cali_houses <- cali_houses %>% mutate(police = police_final - vecinos_final) 


# restaurant

cali_houses <- bind_rows(cali_houses, osm_restaurant_Cali_sf%>%dplyr::select(osm_id,amenity))

cali_houses_sp <- cali_houses%>%st_buffer(300)%>%as_Spatial()

cali_houses_nb <- poly2nb(pl=cali_houses_sp, queen= TRUE)

cali_houses$num_restaurant <- NA
cali_houses$val_restaurant <- NA

for (i in 1:length(cali_houses_nb)) {
  
  cali_houses$num_restaurant[i] <- size_sum(cali_houses_nb[[i]])
  cali_houses$val_restaurant[i] <- cali_houses_nb[[i]]==0
  
}

cali_houses <- cali_houses %>% mutate(restaurant_final = ifelse(val_restaurant == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_restaurant)))) 

drop <- c("num_restaurant", "val_restaurant")
cali_houses <- cali_houses[,!names(cali_houses)%in%drop]

cali_houses <- cali_houses %>% mutate(restaurant = restaurant_final - vecinos_final) 

write_rds(cali_houses, "cali_houses_1.RDS")

cali_houses_2 <- cali_houses %>% select(property_id, bank, bus_station, casino, childcare, cinema,
                                      clinic, college, community_centre, conference_centre, dentist, 
                                      doctors, events_venue, fast_food, hospital,
                                      kindergarten, library, love_hotel, marketplace, monastery,
                                      parking, police, restaurant)

write_rds(cali_houses_2, "cali_houses_2.RDS")

write_csv(cali_houses_2, "cali_houses_2.csv")

## Loop

lista_amenities_cal <- list("bank", "bus_station", "casino", "childcare", "cinema",
                            "clinic", "college", "community_centre", "conference_centre", "dentist", 
                            "doctors", "events_venue", "fast_food", "hospital",
                            "kindergarten", "library", "love_hotel", "marketplace", "monastery",
                            "parking", "police", "restaurant")

bog_houses_1 <- bog_houses[1:5620,]

bog_houses_2 <- bog_houses[5621:10720,]

bog_houses_3 <- bog_houses[10721:15840,]

bog_houses_4 <- bog_houses[15841:20355,]

bog_houses_5 <- bog_houses[20356:25640,]

bog_houses_6 <- bog_houses[25641:30710,]

bog_houses_7 <- bog_houses[30711:34120,]

bog_houses_8 <- bog_houses[34121:37985,]


### Bogota 1

bog_houses_1_sp <- bog_houses_1%>%st_buffer(300)%>%as_Spatial()

bog_houses_1_nb <- poly2nb(pl=bog_houses_1_sp, queen= TRUE)

bog_houses_1$num_vecinos <- NA
bog_houses_1$val_vecinos <- NA

for (i in 1:length(bog_houses_1_nb)) {
  
  bog_houses_1$num_vecinos[i] <- size_sum(bog_houses_1_nb[[i]])
  bog_houses_1$val_vecinos[i] <- bog_houses_1_nb[[i]]==0
  
}

bog_houses_1 <- bog_houses_1 %>% mutate(vecinos_final = ifelse(val_vecinos == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_vecinos)))) 

drop <- c("num_vecinos", "val_vecinos")
bog_houses_1 <- bog_houses_1[,!names(bog_houses_1)%in%drop]

# Bancos

bog_houses_1 <- bind_rows(bog_houses_1, osm_bank_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_1_sp <- bog_houses_1%>%st_buffer(300)%>%as_Spatial()

bog_houses_1_nb <- poly2nb(pl=bog_houses_1_sp, queen= TRUE)

bog_houses_1$num_bank <- NA
bog_houses_1$val_bank <- NA

for (i in 1:length(bog_houses_1_nb)) {
  
  bog_houses_1$num_bank[i] <- size_sum(bog_houses_1_nb[[i]])
  bog_houses_1$val_bank[i] <- bog_houses_1_nb[[i]]==0
  
}

bog_houses_1 <- bog_houses_1 %>% mutate(bank_final = ifelse(val_bank == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_bank)))) 

drop <- c("num_bank", "val_bank")
bog_houses_1 <- bog_houses_1[,!names(bog_houses_1)%in%drop]

bog_houses_1 <- bog_houses_1 %>% mutate(bank = bank_final - vecinos_final) 

# Bus Station

bog_houses_1 <- bind_rows(bog_houses_1, osm_bus_station_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_1_sp <- bog_houses_1%>%st_buffer(300)%>%as_Spatial()

bog_houses_1_nb <- poly2nb(pl=bog_houses_1_sp, queen= TRUE)

bog_houses_1$num_bus_station <- NA
bog_houses_1$val_bus_station <- NA

for (i in 1:length(bog_houses_1_nb)) {
  
  bog_houses_1$num_bus_station[i] <- size_sum(bog_houses_1_nb[[i]])
  bog_houses_1$val_bus_station[i] <- bog_houses_1_nb[[i]]==0
  
}

bog_houses_1 <- bog_houses_1 %>% mutate(bus_station_final = ifelse(val_bus_station == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_bus_station)))) 

drop <- c("num_bus_station", "val_bus_station")
bog_houses_1 <- bog_houses_1[,!names(bog_houses_1)%in%drop]

bog_houses_1 <- bog_houses_1 %>% mutate(bus_station = bus_station_final - vecinos_final) 

# Casino

bog_houses_1 <- bind_rows(bog_houses_1, osm_casino_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_1_sp <- bog_houses_1%>%st_buffer(300)%>%as_Spatial()

bog_houses_1_nb <- poly2nb(pl=bog_houses_1_sp, queen= TRUE)

bog_houses_1$num_casino <- NA
bog_houses_1$val_casino <- NA

for (i in 1:length(bog_houses_1_nb)) {
  
  bog_houses_1$num_casino[i] <- size_sum(bog_houses_1_nb[[i]])
  bog_houses_1$val_casino[i] <- bog_houses_1_nb[[i]]==0
  
}

bog_houses_1 <- bog_houses_1 %>% mutate(casino_final = ifelse(val_casino == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_casino)))) 

drop <- c("num_casino", "val_casino")
bog_houses_1 <- bog_houses_1[,!names(bog_houses_1)%in%drop]

bog_houses_1 <- bog_houses_1 %>% mutate(casino = casino_final - vecinos_final) 


# Childcare

bog_houses_1 <- bind_rows(bog_houses_1, osm_childcare_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_1_sp <- bog_houses_1%>%st_buffer(300)%>%as_Spatial()

bog_houses_1_nb <- poly2nb(pl=bog_houses_1_sp, queen= TRUE)

bog_houses_1$num_childcare <- NA
bog_houses_1$val_childcare <- NA

for (i in 1:length(bog_houses_1_nb)) {
  
  bog_houses_1$num_childcare[i] <- size_sum(bog_houses_1_nb[[i]])
  bog_houses_1$val_childcare[i] <- bog_houses_1_nb[[i]]==0
  
}

bog_houses_1 <- bog_houses_1 %>% mutate(childcare_final = ifelse(val_childcare == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_childcare)))) 

drop <- c("num_childcare", "val_childcare")
bog_houses_1 <- bog_houses_1[,!names(bog_houses_1)%in%drop]

bog_houses_1 <- bog_houses_1 %>% mutate(childcare = childcare_final - vecinos_final) 


# Cinema

bog_houses_1 <- bind_rows(bog_houses_1, osm_cinema_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_1_sp <- bog_houses_1%>%st_buffer(300)%>%as_Spatial()

bog_houses_1_nb <- poly2nb(pl=bog_houses_1_sp, queen= TRUE)

bog_houses_1$num_cinema <- NA
bog_houses_1$val_cinema <- NA

for (i in 1:length(bog_houses_1_nb)) {
  
  bog_houses_1$num_cinema[i] <- size_sum(bog_houses_1_nb[[i]])
  bog_houses_1$val_cinema[i] <- bog_houses_1_nb[[i]]==0
  
}

bog_houses_1 <- bog_houses_1 %>% mutate(cinema_final = ifelse(val_cinema == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_cinema)))) 

drop <- c("num_cinema", "val_cinema")
bog_houses_1 <- bog_houses_1[,!names(bog_houses_1)%in%drop]

bog_houses_1 <- bog_houses_1 %>% mutate(cinema = cinema_final - vecinos_final) 


# clinic

bog_houses_1 <- bind_rows(bog_houses_1, osm_clinic_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_1_sp <- bog_houses_1%>%st_buffer(300)%>%as_Spatial()

bog_houses_1_nb <- poly2nb(pl=bog_houses_1_sp, queen= TRUE)

bog_houses_1$num_clinic <- NA
bog_houses_1$val_clinic <- NA

for (i in 1:length(bog_houses_1_nb)) {
  
  bog_houses_1$num_clinic[i] <- size_sum(bog_houses_1_nb[[i]])
  bog_houses_1$val_clinic[i] <- bog_houses_1_nb[[i]]==0
  
}

bog_houses_1 <- bog_houses_1 %>% mutate(clinic_final = ifelse(val_clinic == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_clinic)))) 

drop <- c("num_clinic", "val_clinic")
bog_houses_1 <- bog_houses_1[,!names(bog_houses_1)%in%drop]

bog_houses_1 <- bog_houses_1 %>% mutate(clinic = clinic_final - vecinos_final) 


# college

bog_houses_1 <- bind_rows(bog_houses_1, osm_college_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_1_sp <- bog_houses_1%>%st_buffer(300)%>%as_Spatial()

bog_houses_1_nb <- poly2nb(pl=bog_houses_1_sp, queen= TRUE)

bog_houses_1$num_college <- NA
bog_houses_1$val_college <- NA

for (i in 1:length(bog_houses_1_nb)) {
  
  bog_houses_1$num_college[i] <- size_sum(bog_houses_1_nb[[i]])
  bog_houses_1$val_college[i] <- bog_houses_1_nb[[i]]==0
  
}

bog_houses_1 <- bog_houses_1 %>% mutate(college_final = ifelse(val_college == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_college)))) 

drop <- c("num_college", "val_college")
bog_houses_1 <- bog_houses_1[,!names(bog_houses_1)%in%drop]

bog_houses_1 <- bog_houses_1 %>% mutate(college = college_final - vecinos_final) 


# community_centre

bog_houses_1 <- bind_rows(bog_houses_1, osm_community_centre_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_1_sp <- bog_houses_1%>%st_buffer(300)%>%as_Spatial()

bog_houses_1_nb <- poly2nb(pl=bog_houses_1_sp, queen= TRUE)

bog_houses_1$num_community_centre <- NA
bog_houses_1$val_community_centre <- NA

for (i in 1:length(bog_houses_1_nb)) {
  
  bog_houses_1$num_community_centre[i] <- size_sum(bog_houses_1_nb[[i]])
  bog_houses_1$val_community_centre[i] <- bog_houses_1_nb[[i]]==0
  
}

bog_houses_1 <- bog_houses_1 %>% mutate(community_centre_final = ifelse(val_community_centre == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_community_centre)))) 

drop <- c("num_community_centre", "val_community_centre")
bog_houses_1 <- bog_houses_1[,!names(bog_houses_1)%in%drop]

bog_houses_1 <- bog_houses_1 %>% mutate(community_centre = community_centre_final - vecinos_final) 



# conference_centre

bog_houses_1 <- bind_rows(bog_houses_1, osm_conference_centre_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_1_sp <- bog_houses_1%>%st_buffer(300)%>%as_Spatial()

bog_houses_1_nb <- poly2nb(pl=bog_houses_1_sp, queen= TRUE)

bog_houses_1$num_conference_centre <- NA
bog_houses_1$val_conference_centre <- NA

for (i in 1:length(bog_houses_1_nb)) {
  
  bog_houses_1$num_conference_centre[i] <- size_sum(bog_houses_1_nb[[i]])
  bog_houses_1$val_conference_centre[i] <- bog_houses_1_nb[[i]]==0
  
}

bog_houses_1 <- bog_houses_1 %>% mutate(conference_centre_final = ifelse(val_conference_centre == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_conference_centre)))) 

drop <- c("num_conference_centre", "val_conference_centre")
bog_houses_1 <- bog_houses_1[,!names(bog_houses_1)%in%drop]

bog_houses_1 <- bog_houses_1 %>% mutate(conference_centre = conference_centre_final - vecinos_final) 



# dentist

bog_houses_1 <- bind_rows(bog_houses_1, osm_dentist_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_1_sp <- bog_houses_1%>%st_buffer(300)%>%as_Spatial()

bog_houses_1_nb <- poly2nb(pl=bog_houses_1_sp, queen= TRUE)

bog_houses_1$num_dentist <- NA
bog_houses_1$val_dentist <- NA

for (i in 1:length(bog_houses_1_nb)) {
  
  bog_houses_1$num_dentist[i] <- size_sum(bog_houses_1_nb[[i]])
  bog_houses_1$val_dentist[i] <- bog_houses_1_nb[[i]]==0
  
}

bog_houses_1 <- bog_houses_1 %>% mutate(dentist_final = ifelse(val_dentist == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_dentist)))) 

drop <- c("num_dentist", "val_dentist")
bog_houses_1 <- bog_houses_1[,!names(bog_houses_1)%in%drop]

bog_houses_1 <- bog_houses_1 %>% mutate(dentist = dentist_final - vecinos_final) 


# doctors

bog_houses_1 <- bind_rows(bog_houses_1, osm_doctors_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_1_sp <- bog_houses_1%>%st_buffer(300)%>%as_Spatial()

bog_houses_1_nb <- poly2nb(pl=bog_houses_1_sp, queen= TRUE)

bog_houses_1$num_doctors <- NA
bog_houses_1$val_doctors <- NA

for (i in 1:length(bog_houses_1_nb)) {
  
  bog_houses_1$num_doctors[i] <- size_sum(bog_houses_1_nb[[i]])
  bog_houses_1$val_doctors[i] <- bog_houses_1_nb[[i]]==0
  
}

bog_houses_1 <- bog_houses_1 %>% mutate(doctors_final = ifelse(val_doctors == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_doctors)))) 

drop <- c("num_doctors", "val_doctors")
bog_houses_1 <- bog_houses_1[,!names(bog_houses_1)%in%drop]

bog_houses_1 <- bog_houses_1 %>% mutate(doctors = doctors_final - vecinos_final) 


# events_venue

bog_houses_1 <- bind_rows(bog_houses_1, osm_events_venue_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_1_sp <- bog_houses_1%>%st_buffer(300)%>%as_Spatial()

bog_houses_1_nb <- poly2nb(pl=bog_houses_1_sp, queen= TRUE)

bog_houses_1$num_events_venue <- NA
bog_houses_1$val_events_venue <- NA

for (i in 1:length(bog_houses_1_nb)) {
  
  bog_houses_1$num_events_venue[i] <- size_sum(bog_houses_1_nb[[i]])
  bog_houses_1$val_events_venue[i] <- bog_houses_1_nb[[i]]==0
  
}

bog_houses_1 <- bog_houses_1 %>% mutate(events_venue_final = ifelse(val_events_venue == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_events_venue)))) 

drop <- c("num_events_venue", "val_events_venue")
bog_houses_1 <- bog_houses_1[,!names(bog_houses_1)%in%drop]

bog_houses_1 <- bog_houses_1 %>% mutate(events_venue = events_venue_final - vecinos_final) 


# fast_food

bog_houses_1 <- bind_rows(bog_houses_1, osm_fast_food_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_1_sp <- bog_houses_1%>%st_buffer(300)%>%as_Spatial()

bog_houses_1_nb <- poly2nb(pl=bog_houses_1_sp, queen= TRUE)

bog_houses_1$num_fast_food <- NA
bog_houses_1$val_fast_food <- NA

for (i in 1:length(bog_houses_1_nb)) {
  
  bog_houses_1$num_fast_food[i] <- size_sum(bog_houses_1_nb[[i]])
  bog_houses_1$val_fast_food[i] <- bog_houses_1_nb[[i]]==0
  
}

bog_houses_1 <- bog_houses_1 %>% mutate(fast_food_final = ifelse(val_fast_food == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_fast_food)))) 

drop <- c("num_fast_food", "val_fast_food")
bog_houses_1 <- bog_houses_1[,!names(bog_houses_1)%in%drop]

bog_houses_1 <- bog_houses_1 %>% mutate(fast_food = fast_food_final - vecinos_final) 


# hospital

bog_houses_1 <- bind_rows(bog_houses_1, osm_hospital_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_1_sp <- bog_houses_1%>%st_buffer(300)%>%as_Spatial()

bog_houses_1_nb <- poly2nb(pl=bog_houses_1_sp, queen= TRUE)

bog_houses_1$num_hospital <- NA
bog_houses_1$val_hospital <- NA

for (i in 1:length(bog_houses_1_nb)) {
  
  bog_houses_1$num_hospital[i] <- size_sum(bog_houses_1_nb[[i]])
  bog_houses_1$val_hospital[i] <- bog_houses_1_nb[[i]]==0
  
}

bog_houses_1 <- bog_houses_1 %>% mutate(hospital_final = ifelse(val_hospital == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_hospital)))) 

drop <- c("num_hospital", "val_hospital")
bog_houses_1 <- bog_houses_1[,!names(bog_houses_1)%in%drop]

bog_houses_1 <- bog_houses_1 %>% mutate(hospital = hospital_final - vecinos_final) 


# kindergarten

bog_houses_1 <- bind_rows(bog_houses_1, osm_kindergarten_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_1_sp <- bog_houses_1%>%st_buffer(300)%>%as_Spatial()

bog_houses_1_nb <- poly2nb(pl=bog_houses_1_sp, queen= TRUE)

bog_houses_1$num_kindergarten <- NA
bog_houses_1$val_kindergarten <- NA

for (i in 1:length(bog_houses_1_nb)) {
  
  bog_houses_1$num_kindergarten[i] <- size_sum(bog_houses_1_nb[[i]])
  bog_houses_1$val_kindergarten[i] <- bog_houses_1_nb[[i]]==0
  
}

bog_houses_1 <- bog_houses_1 %>% mutate(kindergarten_final = ifelse(val_kindergarten == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_kindergarten)))) 

drop <- c("num_kindergarten", "val_kindergarten")
bog_houses_1 <- bog_houses_1[,!names(bog_houses_1)%in%drop]

bog_houses_1 <- bog_houses_1 %>% mutate(kindergarten = kindergarten_final - vecinos_final)


# library

bog_houses_1 <- bind_rows(bog_houses_1, osm_library_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_1_sp <- bog_houses_1%>%st_buffer(300)%>%as_Spatial()

bog_houses_1_nb <- poly2nb(pl=bog_houses_1_sp, queen= TRUE)

bog_houses_1$num_library <- NA
bog_houses_1$val_library <- NA

for (i in 1:length(bog_houses_1_nb)) {
  
  bog_houses_1$num_library[i] <- size_sum(bog_houses_1_nb[[i]])
  bog_houses_1$val_library[i] <- bog_houses_1_nb[[i]]==0
  
}

bog_houses_1 <- bog_houses_1 %>% mutate(library_final = ifelse(val_library == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_library)))) 

drop <- c("num_library", "val_library")
bog_houses_1 <- bog_houses_1[,!names(bog_houses_1)%in%drop]

bog_houses_1 <- bog_houses_1 %>% mutate(library = library_final - vecinos_final) 



# love_hotel

bog_houses_1 <- bind_rows(bog_houses_1, osm_love_hotel_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_1_sp <- bog_houses_1%>%st_buffer(300)%>%as_Spatial()

bog_houses_1_nb <- poly2nb(pl=bog_houses_1_sp, queen= TRUE)

bog_houses_1$num_love_hotel <- NA
bog_houses_1$val_love_hotel <- NA

for (i in 1:length(bog_houses_1_nb)) {
  
  bog_houses_1$num_love_hotel[i] <- size_sum(bog_houses_1_nb[[i]])
  bog_houses_1$val_love_hotel[i] <- bog_houses_1_nb[[i]]==0
  
}

bog_houses_1 <- bog_houses_1 %>% mutate(love_hotel_final = ifelse(val_love_hotel == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_love_hotel)))) 

drop <- c("num_love_hotel", "val_love_hotel")
bog_houses_1 <- bog_houses_1[,!names(bog_houses_1)%in%drop]

bog_houses_1 <- bog_houses_1 %>% mutate(love_hotel = love_hotel_final - vecinos_final) 


# marketplace

bog_houses_1 <- bind_rows(bog_houses_1, osm_marketplace_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_1_sp <- bog_houses_1%>%st_buffer(300)%>%as_Spatial()

bog_houses_1_nb <- poly2nb(pl=bog_houses_1_sp, queen= TRUE)

bog_houses_1$num_marketplace <- NA
bog_houses_1$val_marketplace <- NA

for (i in 1:length(bog_houses_1_nb)) {
  
  bog_houses_1$num_marketplace[i] <- size_sum(bog_houses_1_nb[[i]])
  bog_houses_1$val_marketplace[i] <- bog_houses_1_nb[[i]]==0
  
}

bog_houses_1 <- bog_houses_1 %>% mutate(marketplace_final = ifelse(val_marketplace == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_marketplace)))) 

drop <- c("num_marketplace", "val_marketplace")
bog_houses_1 <- bog_houses_1[,!names(bog_houses_1)%in%drop]

bog_houses_1 <- bog_houses_1 %>% mutate(marketplace =  marketplace_final - vecinos_final) 



# monastery

bog_houses_1 <- bind_rows(bog_houses_1, osm_monastery_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_1_sp <- bog_houses_1%>%st_buffer(300)%>%as_Spatial()

bog_houses_1_nb <- poly2nb(pl=bog_houses_1_sp, queen= TRUE)

bog_houses_1$num_monastery <- NA
bog_houses_1$val_monastery <- NA

for (i in 1:length(bog_houses_1_nb)) {
  
  bog_houses_1$num_monastery[i] <- size_sum(bog_houses_1_nb[[i]])
  bog_houses_1$val_monastery[i] <- bog_houses_1_nb[[i]]==0
  
}

bog_houses_1 <- bog_houses_1 %>% mutate(monastery_final = ifelse(val_monastery == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_monastery)))) 

drop <- c("num_monastery", "val_monastery")
bog_houses_1 <- bog_houses_1[,!names(bog_houses_1)%in%drop]

bog_houses_1 <- bog_houses_1 %>% mutate(monastery = monastery_final - vecinos_final) 


# parking

bog_houses_1 <- bind_rows(bog_houses_1, osm_parking_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_1_sp <- bog_houses_1%>%st_buffer(300)%>%as_Spatial()

bog_houses_1_nb <- poly2nb(pl=bog_houses_1_sp, queen= TRUE)

bog_houses_1$num_parking <- NA
bog_houses_1$val_parking <- NA

for (i in 1:length(bog_houses_1_nb)) {
  
  bog_houses_1$num_parking[i] <- size_sum(bog_houses_1_nb[[i]])
  bog_houses_1$val_parking[i] <- bog_houses_1_nb[[i]]==0
  
}

bog_houses_1 <- bog_houses_1 %>% mutate(parking_final = ifelse(val_parking == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_parking)))) 

drop <- c("num_parking", "val_parking")
bog_houses_1 <- bog_houses_1[,!names(bog_houses_1)%in%drop]

bog_houses_1 <- bog_houses_1 %>% mutate(parking = parking_final - vecinos_final) 


# police

bog_houses_1 <- bind_rows(bog_houses_1, osm_police_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_1_sp <- bog_houses_1%>%st_buffer(300)%>%as_Spatial()

bog_houses_1_nb <- poly2nb(pl=bog_houses_1_sp, queen= TRUE)

bog_houses_1$num_police <- NA
bog_houses_1$val_police <- NA

for (i in 1:length(bog_houses_1_nb)) {
  
  bog_houses_1$num_police[i] <- size_sum(bog_houses_1_nb[[i]])
  bog_houses_1$val_police[i] <- bog_houses_1_nb[[i]]==0
  
}

bog_houses_1 <- bog_houses_1 %>% mutate(police_final = ifelse(val_police == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_police)))) 

drop <- c("num_police", "val_police")
bog_houses_1 <- bog_houses_1[,!names(bog_houses_1)%in%drop]

bog_houses_1 <- bog_houses_1 %>% mutate(police = police_final - vecinos_final) 


# restaurant

bog_houses_1 <- bind_rows(bog_houses_1, osm_restaurant_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_1_sp <- bog_houses_1%>%st_buffer(300)%>%as_Spatial()

bog_houses_1_nb <- poly2nb(pl=bog_houses_1_sp, queen= TRUE)

bog_houses_1$num_restaurant <- NA
bog_houses_1$val_restaurant <- NA

for (i in 1:length(bog_houses_1_nb)) {
  
  bog_houses_1$num_restaurant[i] <- size_sum(bog_houses_1_nb[[i]])
  bog_houses_1$val_restaurant[i] <- bog_houses_1_nb[[i]]==0
  
}

bog_houses_1 <- bog_houses_1 %>% mutate(restaurant_final = ifelse(val_restaurant == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_restaurant)))) 

drop <- c("num_restaurant", "val_restaurant")
bog_houses_1 <- bog_houses_1[,!names(bog_houses_1)%in%drop]

bog_houses_1 <- bog_houses_1 %>% mutate(restaurant =  restaurant_final - vecinos_final) 

write_rds(bog_houses_1, "bog_houses_1_1.RDS")

bog_houses_1_2 <- bog_houses_1 %>% select(property_id, bank, bus_station, casino, childcare, cinema,
                                        clinic, college, community_centre, conference_centre, dentist, 
                                        doctors, events_venue, fast_food, hospital,
                                        kindergarten, library, love_hotel, marketplace, monastery,
                                        parking, police, restaurant)

write_rds(bog_houses_1_2, "bog_houses_1_2.RDS")

write_csv(bog_houses_1_2, "bog_houses_1_2.csv")


### Bogota 2

bog_houses_2_sp <- bog_houses_2%>%st_buffer(300)%>%as_Spatial()

bog_houses_2_nb <- poly2nb(pl=bog_houses_2_sp, queen= TRUE)

bog_houses_2$num_vecinos <- NA
bog_houses_2$val_vecinos <- NA

for (i in 1:length(bog_houses_2_nb)) {
  
  bog_houses_2$num_vecinos[i] <- size_sum(bog_houses_2_nb[[i]])
  bog_houses_2$val_vecinos[i] <- bog_houses_2_nb[[i]]==0
  
}

bog_houses_2 <- bog_houses_2 %>% mutate(vecinos_final = ifelse(val_vecinos == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_vecinos)))) 

drop <- c("num_vecinos", "val_vecinos")
bog_houses_2 <- bog_houses_2[,!names(bog_houses_2)%in%drop]

# Bancos

bog_houses_2 <- bind_rows(bog_houses_2, osm_bank_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_2_sp <- bog_houses_2%>%st_buffer(300)%>%as_Spatial()

bog_houses_2_nb <- poly2nb(pl=bog_houses_2_sp, queen= TRUE)

bog_houses_2$num_bank <- NA
bog_houses_2$val_bank <- NA

for (i in 1:length(bog_houses_2_nb)) {
  
  bog_houses_2$num_bank[i] <- size_sum(bog_houses_2_nb[[i]])
  bog_houses_2$val_bank[i] <- bog_houses_2_nb[[i]]==0
  
}

bog_houses_2 <- bog_houses_2 %>% mutate(bank_final = ifelse(val_bank == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_bank)))) 

drop <- c("num_bank", "val_bank")
bog_houses_2 <- bog_houses_2[,!names(bog_houses_2)%in%drop]

bog_houses_2 <- bog_houses_2 %>% mutate(bank = bank_final - vecinos_final) 

# Bus Station

bog_houses_2 <- bind_rows(bog_houses_2, osm_bus_station_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_2_sp <- bog_houses_2%>%st_buffer(300)%>%as_Spatial()

bog_houses_2_nb <- poly2nb(pl=bog_houses_2_sp, queen= TRUE)

bog_houses_2$num_bus_station <- NA
bog_houses_2$val_bus_station <- NA

for (i in 1:length(bog_houses_2_nb)) {
  
  bog_houses_2$num_bus_station[i] <- size_sum(bog_houses_2_nb[[i]])
  bog_houses_2$val_bus_station[i] <- bog_houses_2_nb[[i]]==0
  
}

bog_houses_2 <- bog_houses_2 %>% mutate(bus_station_final = ifelse(val_bus_station == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_bus_station)))) 

drop <- c("num_bus_station", "val_bus_station")
bog_houses_2 <- bog_houses_2[,!names(bog_houses_2)%in%drop]

bog_houses_2 <- bog_houses_2 %>% mutate(bus_station = bus_station_final - vecinos_final) 

# Casino

bog_houses_2 <- bind_rows(bog_houses_2, osm_casino_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_2_sp <- bog_houses_2%>%st_buffer(300)%>%as_Spatial()

bog_houses_2_nb <- poly2nb(pl=bog_houses_2_sp, queen= TRUE)

bog_houses_2$num_casino <- NA
bog_houses_2$val_casino <- NA

for (i in 1:length(bog_houses_2_nb)) {
  
  bog_houses_2$num_casino[i] <- size_sum(bog_houses_2_nb[[i]])
  bog_houses_2$val_casino[i] <- bog_houses_2_nb[[i]]==0
  
}

bog_houses_2 <- bog_houses_2 %>% mutate(casino_final = ifelse(val_casino == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_casino)))) 

drop <- c("num_casino", "val_casino")
bog_houses_2 <- bog_houses_2[,!names(bog_houses_2)%in%drop]

bog_houses_2 <- bog_houses_2 %>% mutate(casino = casino_final - vecinos_final) 


# Childcare

bog_houses_2 <- bind_rows(bog_houses_2, osm_childcare_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_2_sp <- bog_houses_2%>%st_buffer(300)%>%as_Spatial()

bog_houses_2_nb <- poly2nb(pl=bog_houses_2_sp, queen= TRUE)

bog_houses_2$num_childcare <- NA
bog_houses_2$val_childcare <- NA

for (i in 1:length(bog_houses_2_nb)) {
  
  bog_houses_2$num_childcare[i] <- size_sum(bog_houses_2_nb[[i]])
  bog_houses_2$val_childcare[i] <- bog_houses_2_nb[[i]]==0
  
}

bog_houses_2 <- bog_houses_2 %>% mutate(childcare_final = ifelse(val_childcare == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_childcare)))) 

drop <- c("num_childcare", "val_childcare")
bog_houses_2 <- bog_houses_2[,!names(bog_houses_2)%in%drop]

bog_houses_2 <- bog_houses_2 %>% mutate(childcare = childcare_final - vecinos_final) 


# Cinema

bog_houses_2 <- bind_rows(bog_houses_2, osm_cinema_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_2_sp <- bog_houses_2%>%st_buffer(300)%>%as_Spatial()

bog_houses_2_nb <- poly2nb(pl=bog_houses_2_sp, queen= TRUE)

bog_houses_2$num_cinema <- NA
bog_houses_2$val_cinema <- NA

for (i in 1:length(bog_houses_2_nb)) {
  
  bog_houses_2$num_cinema[i] <- size_sum(bog_houses_2_nb[[i]])
  bog_houses_2$val_cinema[i] <- bog_houses_2_nb[[i]]==0
  
}

bog_houses_2 <- bog_houses_2 %>% mutate(cinema_final = ifelse(val_cinema == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_cinema)))) 

drop <- c("num_cinema", "val_cinema")
bog_houses_2 <- bog_houses_2[,!names(bog_houses_2)%in%drop]

bog_houses_2 <- bog_houses_2 %>% mutate(cinema = cinema_final - vecinos_final) 


# clinic

bog_houses_2 <- bind_rows(bog_houses_2, osm_clinic_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_2_sp <- bog_houses_2%>%st_buffer(300)%>%as_Spatial()

bog_houses_2_nb <- poly2nb(pl=bog_houses_2_sp, queen= TRUE)

bog_houses_2$num_clinic <- NA
bog_houses_2$val_clinic <- NA

for (i in 1:length(bog_houses_2_nb)) {
  
  bog_houses_2$num_clinic[i] <- size_sum(bog_houses_2_nb[[i]])
  bog_houses_2$val_clinic[i] <- bog_houses_2_nb[[i]]==0
  
}

bog_houses_2 <- bog_houses_2 %>% mutate(clinic_final = ifelse(val_clinic == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_clinic)))) 

drop <- c("num_clinic", "val_clinic")
bog_houses_2 <- bog_houses_2[,!names(bog_houses_2)%in%drop]

bog_houses_2 <- bog_houses_2 %>% mutate(clinic = clinic_final - vecinos_final) 


# college

bog_houses_2 <- bind_rows(bog_houses_2, osm_college_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_2_sp <- bog_houses_2%>%st_buffer(300)%>%as_Spatial()

bog_houses_2_nb <- poly2nb(pl=bog_houses_2_sp, queen= TRUE)

bog_houses_2$num_college <- NA
bog_houses_2$val_college <- NA

for (i in 1:length(bog_houses_2_nb)) {
  
  bog_houses_2$num_college[i] <- size_sum(bog_houses_2_nb[[i]])
  bog_houses_2$val_college[i] <- bog_houses_2_nb[[i]]==0
  
}

bog_houses_2 <- bog_houses_2 %>% mutate(college_final = ifelse(val_college == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_college)))) 

drop <- c("num_college", "val_college")
bog_houses_2 <- bog_houses_2[,!names(bog_houses_2)%in%drop]

bog_houses_2 <- bog_houses_2 %>% mutate(college = college_final - vecinos_final) 


# community_centre

bog_houses_2 <- bind_rows(bog_houses_2, osm_community_centre_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_2_sp <- bog_houses_2%>%st_buffer(300)%>%as_Spatial()

bog_houses_2_nb <- poly2nb(pl=bog_houses_2_sp, queen= TRUE)

bog_houses_2$num_community_centre <- NA
bog_houses_2$val_community_centre <- NA

for (i in 1:length(bog_houses_2_nb)) {
  
  bog_houses_2$num_community_centre[i] <- size_sum(bog_houses_2_nb[[i]])
  bog_houses_2$val_community_centre[i] <- bog_houses_2_nb[[i]]==0
  
}

bog_houses_2 <- bog_houses_2 %>% mutate(community_centre_final = ifelse(val_community_centre == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_community_centre)))) 

drop <- c("num_community_centre", "val_community_centre")
bog_houses_2 <- bog_houses_2[,!names(bog_houses_2)%in%drop]

bog_houses_2 <- bog_houses_2 %>% mutate(community_centre = community_centre_final - vecinos_final) 



# conference_centre

bog_houses_2 <- bind_rows(bog_houses_2, osm_conference_centre_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_2_sp <- bog_houses_2%>%st_buffer(300)%>%as_Spatial()

bog_houses_2_nb <- poly2nb(pl=bog_houses_2_sp, queen= TRUE)

bog_houses_2$num_conference_centre <- NA
bog_houses_2$val_conference_centre <- NA

for (i in 1:length(bog_houses_2_nb)) {
  
  bog_houses_2$num_conference_centre[i] <- size_sum(bog_houses_2_nb[[i]])
  bog_houses_2$val_conference_centre[i] <- bog_houses_2_nb[[i]]==0
  
}

bog_houses_2 <- bog_houses_2 %>% mutate(conference_centre_final = ifelse(val_conference_centre == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_conference_centre)))) 

drop <- c("num_conference_centre", "val_conference_centre")
bog_houses_2 <- bog_houses_2[,!names(bog_houses_2)%in%drop]

bog_houses_2 <- bog_houses_2 %>% mutate(conference_centre = conference_centre_final - vecinos_final) 



# dentist

bog_houses_2 <- bind_rows(bog_houses_2, osm_dentist_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_2_sp <- bog_houses_2%>%st_buffer(300)%>%as_Spatial()

bog_houses_2_nb <- poly2nb(pl=bog_houses_2_sp, queen= TRUE)

bog_houses_2$num_dentist <- NA
bog_houses_2$val_dentist <- NA

for (i in 1:length(bog_houses_2_nb)) {
  
  bog_houses_2$num_dentist[i] <- size_sum(bog_houses_2_nb[[i]])
  bog_houses_2$val_dentist[i] <- bog_houses_2_nb[[i]]==0
  
}

bog_houses_2 <- bog_houses_2 %>% mutate(dentist_final = ifelse(val_dentist == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_dentist)))) 

drop <- c("num_dentist", "val_dentist")
bog_houses_2 <- bog_houses_2[,!names(bog_houses_2)%in%drop]

bog_houses_2 <- bog_houses_2 %>% mutate(dentist = dentist_final - vecinos_final) 


# doctors

bog_houses_2 <- bind_rows(bog_houses_2, osm_doctors_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_2_sp <- bog_houses_2%>%st_buffer(300)%>%as_Spatial()

bog_houses_2_nb <- poly2nb(pl=bog_houses_2_sp, queen= TRUE)

bog_houses_2$num_doctors <- NA
bog_houses_2$val_doctors <- NA

for (i in 1:length(bog_houses_2_nb)) {
  
  bog_houses_2$num_doctors[i] <- size_sum(bog_houses_2_nb[[i]])
  bog_houses_2$val_doctors[i] <- bog_houses_2_nb[[i]]==0
  
}

bog_houses_2 <- bog_houses_2 %>% mutate(doctors_final = ifelse(val_doctors == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_doctors)))) 

drop <- c("num_doctors", "val_doctors")
bog_houses_2 <- bog_houses_2[,!names(bog_houses_2)%in%drop]

bog_houses_2 <- bog_houses_2 %>% mutate(doctors = doctors_final - vecinos_final) 


# events_venue

bog_houses_2 <- bind_rows(bog_houses_2, osm_events_venue_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_2_sp <- bog_houses_2%>%st_buffer(300)%>%as_Spatial()

bog_houses_2_nb <- poly2nb(pl=bog_houses_2_sp, queen= TRUE)

bog_houses_2$num_events_venue <- NA
bog_houses_2$val_events_venue <- NA

for (i in 1:length(bog_houses_2_nb)) {
  
  bog_houses_2$num_events_venue[i] <- size_sum(bog_houses_2_nb[[i]])
  bog_houses_2$val_events_venue[i] <- bog_houses_2_nb[[i]]==0
  
}

bog_houses_2 <- bog_houses_2 %>% mutate(events_venue_final = ifelse(val_events_venue == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_events_venue)))) 

drop <- c("num_events_venue", "val_events_venue")
bog_houses_2 <- bog_houses_2[,!names(bog_houses_2)%in%drop]

bog_houses_2 <- bog_houses_2 %>% mutate(events_venue = events_venue_final - vecinos_final) 


# fast_food

bog_houses_2 <- bind_rows(bog_houses_2, osm_fast_food_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_2_sp <- bog_houses_2%>%st_buffer(300)%>%as_Spatial()

bog_houses_2_nb <- poly2nb(pl=bog_houses_2_sp, queen= TRUE)

bog_houses_2$num_fast_food <- NA
bog_houses_2$val_fast_food <- NA

for (i in 1:length(bog_houses_2_nb)) {
  
  bog_houses_2$num_fast_food[i] <- size_sum(bog_houses_2_nb[[i]])
  bog_houses_2$val_fast_food[i] <- bog_houses_2_nb[[i]]==0
  
}

bog_houses_2 <- bog_houses_2 %>% mutate(fast_food_final = ifelse(val_fast_food == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_fast_food)))) 

drop <- c("num_fast_food", "val_fast_food")
bog_houses_2 <- bog_houses_2[,!names(bog_houses_2)%in%drop]

bog_houses_2 <- bog_houses_2 %>% mutate(fast_food = fast_food_final - vecinos_final) 


# hospital

bog_houses_2 <- bind_rows(bog_houses_2, osm_hospital_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_2_sp <- bog_houses_2%>%st_buffer(300)%>%as_Spatial()

bog_houses_2_nb <- poly2nb(pl=bog_houses_2_sp, queen= TRUE)

bog_houses_2$num_hospital <- NA
bog_houses_2$val_hospital <- NA

for (i in 1:length(bog_houses_2_nb)) {
  
  bog_houses_2$num_hospital[i] <- size_sum(bog_houses_2_nb[[i]])
  bog_houses_2$val_hospital[i] <- bog_houses_2_nb[[i]]==0
  
}

bog_houses_2 <- bog_houses_2 %>% mutate(hospital_final = ifelse(val_hospital == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_hospital)))) 

drop <- c("num_hospital", "val_hospital")
bog_houses_2 <- bog_houses_2[,!names(bog_houses_2)%in%drop]

bog_houses_2 <- bog_houses_2 %>% mutate(hospital = hospital_final - vecinos_final) 


# kindergarten

bog_houses_2 <- bind_rows(bog_houses_2, osm_kindergarten_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_2_sp <- bog_houses_2%>%st_buffer(300)%>%as_Spatial()

bog_houses_2_nb <- poly2nb(pl=bog_houses_2_sp, queen= TRUE)

bog_houses_2$num_kindergarten <- NA
bog_houses_2$val_kindergarten <- NA

for (i in 1:length(bog_houses_2_nb)) {
  
  bog_houses_2$num_kindergarten[i] <- size_sum(bog_houses_2_nb[[i]])
  bog_houses_2$val_kindergarten[i] <- bog_houses_2_nb[[i]]==0
  
}

bog_houses_2 <- bog_houses_2 %>% mutate(kindergarten_final = ifelse(val_kindergarten == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_kindergarten)))) 

drop <- c("num_kindergarten", "val_kindergarten")
bog_houses_2 <- bog_houses_2[,!names(bog_houses_2)%in%drop]

bog_houses_2 <- bog_houses_2 %>% mutate(kindergarten = kindergarten_final - vecinos_final)


# library

bog_houses_2 <- bind_rows(bog_houses_2, osm_library_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_2_sp <- bog_houses_2%>%st_buffer(300)%>%as_Spatial()

bog_houses_2_nb <- poly2nb(pl=bog_houses_2_sp, queen= TRUE)

bog_houses_2$num_library <- NA
bog_houses_2$val_library <- NA

for (i in 1:length(bog_houses_2_nb)) {
  
  bog_houses_2$num_library[i] <- size_sum(bog_houses_2_nb[[i]])
  bog_houses_2$val_library[i] <- bog_houses_2_nb[[i]]==0
  
}

bog_houses_2 <- bog_houses_2 %>% mutate(library_final = ifelse(val_library == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_library)))) 

drop <- c("num_library", "val_library")
bog_houses_2 <- bog_houses_2[,!names(bog_houses_2)%in%drop]

bog_houses_2 <- bog_houses_2 %>% mutate(library = library_final - vecinos_final) 



# love_hotel

bog_houses_2 <- bind_rows(bog_houses_2, osm_love_hotel_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_2_sp <- bog_houses_2%>%st_buffer(300)%>%as_Spatial()

bog_houses_2_nb <- poly2nb(pl=bog_houses_2_sp, queen= TRUE)

bog_houses_2$num_love_hotel <- NA
bog_houses_2$val_love_hotel <- NA

for (i in 1:length(bog_houses_2_nb)) {
  
  bog_houses_2$num_love_hotel[i] <- size_sum(bog_houses_2_nb[[i]])
  bog_houses_2$val_love_hotel[i] <- bog_houses_2_nb[[i]]==0
  
}

bog_houses_2 <- bog_houses_2 %>% mutate(love_hotel_final = ifelse(val_love_hotel == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_love_hotel)))) 

drop <- c("num_love_hotel", "val_love_hotel")
bog_houses_2 <- bog_houses_2[,!names(bog_houses_2)%in%drop]

bog_houses_2 <- bog_houses_2 %>% mutate(love_hotel = love_hotel_final - vecinos_final) 


# marketplace

bog_houses_2 <- bind_rows(bog_houses_2, osm_marketplace_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_2_sp <- bog_houses_2%>%st_buffer(300)%>%as_Spatial()

bog_houses_2_nb <- poly2nb(pl=bog_houses_2_sp, queen= TRUE)

bog_houses_2$num_marketplace <- NA
bog_houses_2$val_marketplace <- NA

for (i in 1:length(bog_houses_2_nb)) {
  
  bog_houses_2$num_marketplace[i] <- size_sum(bog_houses_2_nb[[i]])
  bog_houses_2$val_marketplace[i] <- bog_houses_2_nb[[i]]==0
  
}

bog_houses_2 <- bog_houses_2 %>% mutate(marketplace_final = ifelse(val_marketplace == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_marketplace)))) 

drop <- c("num_marketplace", "val_marketplace")
bog_houses_2 <- bog_houses_2[,!names(bog_houses_2)%in%drop]

bog_houses_2 <- bog_houses_2 %>% mutate(marketplace =  marketplace_final - vecinos_final) 



# monastery

bog_houses_2 <- bind_rows(bog_houses_2, osm_monastery_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_2_sp <- bog_houses_2%>%st_buffer(300)%>%as_Spatial()

bog_houses_2_nb <- poly2nb(pl=bog_houses_2_sp, queen= TRUE)

bog_houses_2$num_monastery <- NA
bog_houses_2$val_monastery <- NA

for (i in 1:length(bog_houses_2_nb)) {
  
  bog_houses_2$num_monastery[i] <- size_sum(bog_houses_2_nb[[i]])
  bog_houses_2$val_monastery[i] <- bog_houses_2_nb[[i]]==0
  
}

bog_houses_2 <- bog_houses_2 %>% mutate(monastery_final = ifelse(val_monastery == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_monastery)))) 

drop <- c("num_monastery", "val_monastery")
bog_houses_2 <- bog_houses_2[,!names(bog_houses_2)%in%drop]

bog_houses_2 <- bog_houses_2 %>% mutate(monastery = monastery_final - vecinos_final) 


# parking

bog_houses_2 <- bind_rows(bog_houses_2, osm_parking_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_2_sp <- bog_houses_2%>%st_buffer(300)%>%as_Spatial()

bog_houses_2_nb <- poly2nb(pl=bog_houses_2_sp, queen= TRUE)

bog_houses_2$num_parking <- NA
bog_houses_2$val_parking <- NA

for (i in 1:length(bog_houses_2_nb)) {
  
  bog_houses_2$num_parking[i] <- size_sum(bog_houses_2_nb[[i]])
  bog_houses_2$val_parking[i] <- bog_houses_2_nb[[i]]==0
  
}

bog_houses_2 <- bog_houses_2 %>% mutate(parking_final = ifelse(val_parking == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_parking)))) 

drop <- c("num_parking", "val_parking")
bog_houses_2 <- bog_houses_2[,!names(bog_houses_2)%in%drop]

bog_houses_2 <- bog_houses_2 %>% mutate(parking = parking_final - vecinos_final) 


# police

bog_houses_2 <- bind_rows(bog_houses_2, osm_police_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_2_sp <- bog_houses_2%>%st_buffer(300)%>%as_Spatial()

bog_houses_2_nb <- poly2nb(pl=bog_houses_2_sp, queen= TRUE)

bog_houses_2$num_police <- NA
bog_houses_2$val_police <- NA

for (i in 1:length(bog_houses_2_nb)) {
  
  bog_houses_2$num_police[i] <- size_sum(bog_houses_2_nb[[i]])
  bog_houses_2$val_police[i] <- bog_houses_2_nb[[i]]==0
  
}

bog_houses_2 <- bog_houses_2 %>% mutate(police_final = ifelse(val_police == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_police)))) 

drop <- c("num_police", "val_police")
bog_houses_2 <- bog_houses_2[,!names(bog_houses_2)%in%drop]

bog_houses_2 <- bog_houses_2 %>% mutate(police = police_final - vecinos_final) 


# restaurant

bog_houses_2 <- bind_rows(bog_houses_2, osm_restaurant_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_2_sp <- bog_houses_2%>%st_buffer(300)%>%as_Spatial()

bog_houses_2_nb <- poly2nb(pl=bog_houses_2_sp, queen= TRUE)

bog_houses_2$num_restaurant <- NA
bog_houses_2$val_restaurant <- NA

for (i in 1:length(bog_houses_2_nb)) {
  
  bog_houses_2$num_restaurant[i] <- size_sum(bog_houses_2_nb[[i]])
  bog_houses_2$val_restaurant[i] <- bog_houses_2_nb[[i]]==0
  
}

bog_houses_2 <- bog_houses_2 %>% mutate(restaurant_final = ifelse(val_restaurant == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_restaurant)))) 

drop <- c("num_restaurant", "val_restaurant")
bog_houses_2 <- bog_houses_2[,!names(bog_houses_2)%in%drop]

bog_houses_2 <- bog_houses_2 %>% mutate(restaurant =  restaurant_final - vecinos_final) 

write_rds(bog_houses_2, "bog_houses_2_1.RDS")

bog_houses_2_2 <- bog_houses_2 %>% select(property_id, bank, bus_station, casino, childcare, cinema,
                                          clinic, college, community_centre, conference_centre, dentist, 
                                          doctors, events_venue, fast_food, hospital,
                                          kindergarten, library, love_hotel, marketplace, monastery,
                                          parking, police, restaurant)

write_rds(bog_houses_2_2, "bog_houses_2_2.RDS")

write_csv(bog_houses_2_2, "bog_houses_2_2.csv")



### Bogota 3

bog_houses_3_sp <- bog_houses_3%>%st_buffer(300)%>%as_Spatial()

bog_houses_3_nb <- poly2nb(pl=bog_houses_3_sp, queen= TRUE)

bog_houses_3$num_vecinos <- NA
bog_houses_3$val_vecinos <- NA

for (i in 1:length(bog_houses_3_nb)) {
  
  bog_houses_3$num_vecinos[i] <- size_sum(bog_houses_3_nb[[i]])
  bog_houses_3$val_vecinos[i] <- bog_houses_3_nb[[i]]==0
  
}

bog_houses_3 <- bog_houses_3 %>% mutate(vecinos_final = ifelse(val_vecinos == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_vecinos)))) 

drop <- c("num_vecinos", "val_vecinos")
bog_houses_3 <- bog_houses_3[,!names(bog_houses_3)%in%drop]

# Bancos

bog_houses_3 <- bind_rows(bog_houses_3, osm_bank_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_3_sp <- bog_houses_3%>%st_buffer(300)%>%as_Spatial()

bog_houses_3_nb <- poly2nb(pl=bog_houses_3_sp, queen= TRUE)

bog_houses_3$num_bank <- NA
bog_houses_3$val_bank <- NA

for (i in 1:length(bog_houses_3_nb)) {
  
  bog_houses_3$num_bank[i] <- size_sum(bog_houses_3_nb[[i]])
  bog_houses_3$val_bank[i] <- bog_houses_3_nb[[i]]==0
  
}

bog_houses_3 <- bog_houses_3 %>% mutate(bank_final = ifelse(val_bank == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_bank)))) 

drop <- c("num_bank", "val_bank")
bog_houses_3 <- bog_houses_3[,!names(bog_houses_3)%in%drop]

bog_houses_3 <- bog_houses_3 %>% mutate(bank = bank_final - vecinos_final) 

# Bus Station

bog_houses_3 <- bind_rows(bog_houses_3, osm_bus_station_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_3_sp <- bog_houses_3%>%st_buffer(300)%>%as_Spatial()

bog_houses_3_nb <- poly2nb(pl=bog_houses_3_sp, queen= TRUE)

bog_houses_3$num_bus_station <- NA
bog_houses_3$val_bus_station <- NA

for (i in 1:length(bog_houses_3_nb)) {
  
  bog_houses_3$num_bus_station[i] <- size_sum(bog_houses_3_nb[[i]])
  bog_houses_3$val_bus_station[i] <- bog_houses_3_nb[[i]]==0
  
}

bog_houses_3 <- bog_houses_3 %>% mutate(bus_station_final = ifelse(val_bus_station == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_bus_station)))) 

drop <- c("num_bus_station", "val_bus_station")
bog_houses_3 <- bog_houses_3[,!names(bog_houses_3)%in%drop]

bog_houses_3 <- bog_houses_3 %>% mutate(bus_station = bus_station_final - vecinos_final) 

# Casino

bog_houses_3 <- bind_rows(bog_houses_3, osm_casino_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_3_sp <- bog_houses_3%>%st_buffer(300)%>%as_Spatial()

bog_houses_3_nb <- poly2nb(pl=bog_houses_3_sp, queen= TRUE)

bog_houses_3$num_casino <- NA
bog_houses_3$val_casino <- NA

for (i in 1:length(bog_houses_3_nb)) {
  
  bog_houses_3$num_casino[i] <- size_sum(bog_houses_3_nb[[i]])
  bog_houses_3$val_casino[i] <- bog_houses_3_nb[[i]]==0
  
}

bog_houses_3 <- bog_houses_3 %>% mutate(casino_final = ifelse(val_casino == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_casino)))) 

drop <- c("num_casino", "val_casino")
bog_houses_3 <- bog_houses_3[,!names(bog_houses_3)%in%drop]

bog_houses_3 <- bog_houses_3 %>% mutate(casino = casino_final - vecinos_final) 


# Childcare

bog_houses_3 <- bind_rows(bog_houses_3, osm_childcare_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_3_sp <- bog_houses_3%>%st_buffer(300)%>%as_Spatial()

bog_houses_3_nb <- poly2nb(pl=bog_houses_3_sp, queen= TRUE)

bog_houses_3$num_childcare <- NA
bog_houses_3$val_childcare <- NA

for (i in 1:length(bog_houses_3_nb)) {
  
  bog_houses_3$num_childcare[i] <- size_sum(bog_houses_3_nb[[i]])
  bog_houses_3$val_childcare[i] <- bog_houses_3_nb[[i]]==0
  
}

bog_houses_3 <- bog_houses_3 %>% mutate(childcare_final = ifelse(val_childcare == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_childcare)))) 

drop <- c("num_childcare", "val_childcare")
bog_houses_3 <- bog_houses_3[,!names(bog_houses_3)%in%drop]

bog_houses_3 <- bog_houses_3 %>% mutate(childcare = childcare_final - vecinos_final) 


# Cinema

bog_houses_3 <- bind_rows(bog_houses_3, osm_cinema_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_3_sp <- bog_houses_3%>%st_buffer(300)%>%as_Spatial()

bog_houses_3_nb <- poly2nb(pl=bog_houses_3_sp, queen= TRUE)

bog_houses_3$num_cinema <- NA
bog_houses_3$val_cinema <- NA

for (i in 1:length(bog_houses_3_nb)) {
  
  bog_houses_3$num_cinema[i] <- size_sum(bog_houses_3_nb[[i]])
  bog_houses_3$val_cinema[i] <- bog_houses_3_nb[[i]]==0
  
}

bog_houses_3 <- bog_houses_3 %>% mutate(cinema_final = ifelse(val_cinema == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_cinema)))) 

drop <- c("num_cinema", "val_cinema")
bog_houses_3 <- bog_houses_3[,!names(bog_houses_3)%in%drop]

bog_houses_3 <- bog_houses_3 %>% mutate(cinema = cinema_final - vecinos_final) 


# clinic

bog_houses_3 <- bind_rows(bog_houses_3, osm_clinic_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_3_sp <- bog_houses_3%>%st_buffer(300)%>%as_Spatial()

bog_houses_3_nb <- poly2nb(pl=bog_houses_3_sp, queen= TRUE)

bog_houses_3$num_clinic <- NA
bog_houses_3$val_clinic <- NA

for (i in 1:length(bog_houses_3_nb)) {
  
  bog_houses_3$num_clinic[i] <- size_sum(bog_houses_3_nb[[i]])
  bog_houses_3$val_clinic[i] <- bog_houses_3_nb[[i]]==0
  
}

bog_houses_3 <- bog_houses_3 %>% mutate(clinic_final = ifelse(val_clinic == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_clinic)))) 

drop <- c("num_clinic", "val_clinic")
bog_houses_3 <- bog_houses_3[,!names(bog_houses_3)%in%drop]

bog_houses_3 <- bog_houses_3 %>% mutate(clinic = clinic_final - vecinos_final) 


# college

bog_houses_3 <- bind_rows(bog_houses_3, osm_college_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_3_sp <- bog_houses_3%>%st_buffer(300)%>%as_Spatial()

bog_houses_3_nb <- poly2nb(pl=bog_houses_3_sp, queen= TRUE)

bog_houses_3$num_college <- NA
bog_houses_3$val_college <- NA

for (i in 1:length(bog_houses_3_nb)) {
  
  bog_houses_3$num_college[i] <- size_sum(bog_houses_3_nb[[i]])
  bog_houses_3$val_college[i] <- bog_houses_3_nb[[i]]==0
  
}

bog_houses_3 <- bog_houses_3 %>% mutate(college_final = ifelse(val_college == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_college)))) 

drop <- c("num_college", "val_college")
bog_houses_3 <- bog_houses_3[,!names(bog_houses_3)%in%drop]

bog_houses_3 <- bog_houses_3 %>% mutate(college = college_final - vecinos_final) 


# community_centre

bog_houses_3 <- bind_rows(bog_houses_3, osm_community_centre_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_3_sp <- bog_houses_3%>%st_buffer(300)%>%as_Spatial()

bog_houses_3_nb <- poly2nb(pl=bog_houses_3_sp, queen= TRUE)

bog_houses_3$num_community_centre <- NA
bog_houses_3$val_community_centre <- NA

for (i in 1:length(bog_houses_3_nb)) {
  
  bog_houses_3$num_community_centre[i] <- size_sum(bog_houses_3_nb[[i]])
  bog_houses_3$val_community_centre[i] <- bog_houses_3_nb[[i]]==0
  
}

bog_houses_3 <- bog_houses_3 %>% mutate(community_centre_final = ifelse(val_community_centre == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_community_centre)))) 

drop <- c("num_community_centre", "val_community_centre")
bog_houses_3 <- bog_houses_3[,!names(bog_houses_3)%in%drop]

bog_houses_3 <- bog_houses_3 %>% mutate(community_centre = community_centre_final - vecinos_final) 



# conference_centre

bog_houses_3 <- bind_rows(bog_houses_3, osm_conference_centre_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_3_sp <- bog_houses_3%>%st_buffer(300)%>%as_Spatial()

bog_houses_3_nb <- poly2nb(pl=bog_houses_3_sp, queen= TRUE)

bog_houses_3$num_conference_centre <- NA
bog_houses_3$val_conference_centre <- NA

for (i in 1:length(bog_houses_3_nb)) {
  
  bog_houses_3$num_conference_centre[i] <- size_sum(bog_houses_3_nb[[i]])
  bog_houses_3$val_conference_centre[i] <- bog_houses_3_nb[[i]]==0
  
}

bog_houses_3 <- bog_houses_3 %>% mutate(conference_centre_final = ifelse(val_conference_centre == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_conference_centre)))) 

drop <- c("num_conference_centre", "val_conference_centre")
bog_houses_3 <- bog_houses_3[,!names(bog_houses_3)%in%drop]

bog_houses_3 <- bog_houses_3 %>% mutate(conference_centre = conference_centre_final - vecinos_final) 



# dentist

bog_houses_3 <- bind_rows(bog_houses_3, osm_dentist_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_3_sp <- bog_houses_3%>%st_buffer(300)%>%as_Spatial()

bog_houses_3_nb <- poly2nb(pl=bog_houses_3_sp, queen= TRUE)

bog_houses_3$num_dentist <- NA
bog_houses_3$val_dentist <- NA

for (i in 1:length(bog_houses_3_nb)) {
  
  bog_houses_3$num_dentist[i] <- size_sum(bog_houses_3_nb[[i]])
  bog_houses_3$val_dentist[i] <- bog_houses_3_nb[[i]]==0
  
}

bog_houses_3 <- bog_houses_3 %>% mutate(dentist_final = ifelse(val_dentist == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_dentist)))) 

drop <- c("num_dentist", "val_dentist")
bog_houses_3 <- bog_houses_3[,!names(bog_houses_3)%in%drop]

bog_houses_3 <- bog_houses_3 %>% mutate(dentist = dentist_final - vecinos_final) 


# doctors

bog_houses_3 <- bind_rows(bog_houses_3, osm_doctors_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_3_sp <- bog_houses_3%>%st_buffer(300)%>%as_Spatial()

bog_houses_3_nb <- poly2nb(pl=bog_houses_3_sp, queen= TRUE)

bog_houses_3$num_doctors <- NA
bog_houses_3$val_doctors <- NA

for (i in 1:length(bog_houses_3_nb)) {
  
  bog_houses_3$num_doctors[i] <- size_sum(bog_houses_3_nb[[i]])
  bog_houses_3$val_doctors[i] <- bog_houses_3_nb[[i]]==0
  
}

bog_houses_3 <- bog_houses_3 %>% mutate(doctors_final = ifelse(val_doctors == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_doctors)))) 

drop <- c("num_doctors", "val_doctors")
bog_houses_3 <- bog_houses_3[,!names(bog_houses_3)%in%drop]

bog_houses_3 <- bog_houses_3 %>% mutate(doctors = doctors_final - vecinos_final) 


# events_venue

bog_houses_3 <- bind_rows(bog_houses_3, osm_events_venue_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_3_sp <- bog_houses_3%>%st_buffer(300)%>%as_Spatial()

bog_houses_3_nb <- poly2nb(pl=bog_houses_3_sp, queen= TRUE)

bog_houses_3$num_events_venue <- NA
bog_houses_3$val_events_venue <- NA

for (i in 1:length(bog_houses_3_nb)) {
  
  bog_houses_3$num_events_venue[i] <- size_sum(bog_houses_3_nb[[i]])
  bog_houses_3$val_events_venue[i] <- bog_houses_3_nb[[i]]==0
  
}

bog_houses_3 <- bog_houses_3 %>% mutate(events_venue_final = ifelse(val_events_venue == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_events_venue)))) 

drop <- c("num_events_venue", "val_events_venue")
bog_houses_3 <- bog_houses_3[,!names(bog_houses_3)%in%drop]

bog_houses_3 <- bog_houses_3 %>% mutate(events_venue = events_venue_final - vecinos_final) 


# fast_food

bog_houses_3 <- bind_rows(bog_houses_3, osm_fast_food_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_3_sp <- bog_houses_3%>%st_buffer(300)%>%as_Spatial()

bog_houses_3_nb <- poly2nb(pl=bog_houses_3_sp, queen= TRUE)

bog_houses_3$num_fast_food <- NA
bog_houses_3$val_fast_food <- NA

for (i in 1:length(bog_houses_3_nb)) {
  
  bog_houses_3$num_fast_food[i] <- size_sum(bog_houses_3_nb[[i]])
  bog_houses_3$val_fast_food[i] <- bog_houses_3_nb[[i]]==0
  
}

bog_houses_3 <- bog_houses_3 %>% mutate(fast_food_final = ifelse(val_fast_food == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_fast_food)))) 

drop <- c("num_fast_food", "val_fast_food")
bog_houses_3 <- bog_houses_3[,!names(bog_houses_3)%in%drop]

bog_houses_3 <- bog_houses_3 %>% mutate(fast_food = fast_food_final - vecinos_final) 


# hospital

bog_houses_3 <- bind_rows(bog_houses_3, osm_hospital_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_3_sp <- bog_houses_3%>%st_buffer(300)%>%as_Spatial()

bog_houses_3_nb <- poly2nb(pl=bog_houses_3_sp, queen= TRUE)

bog_houses_3$num_hospital <- NA
bog_houses_3$val_hospital <- NA

for (i in 1:length(bog_houses_3_nb)) {
  
  bog_houses_3$num_hospital[i] <- size_sum(bog_houses_3_nb[[i]])
  bog_houses_3$val_hospital[i] <- bog_houses_3_nb[[i]]==0
  
}

bog_houses_3 <- bog_houses_3 %>% mutate(hospital_final = ifelse(val_hospital == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_hospital)))) 

drop <- c("num_hospital", "val_hospital")
bog_houses_3 <- bog_houses_3[,!names(bog_houses_3)%in%drop]

bog_houses_3 <- bog_houses_3 %>% mutate(hospital = hospital_final - vecinos_final) 


# kindergarten

bog_houses_3 <- bind_rows(bog_houses_3, osm_kindergarten_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_3_sp <- bog_houses_3%>%st_buffer(300)%>%as_Spatial()

bog_houses_3_nb <- poly2nb(pl=bog_houses_3_sp, queen= TRUE)

bog_houses_3$num_kindergarten <- NA
bog_houses_3$val_kindergarten <- NA

for (i in 1:length(bog_houses_3_nb)) {
  
  bog_houses_3$num_kindergarten[i] <- size_sum(bog_houses_3_nb[[i]])
  bog_houses_3$val_kindergarten[i] <- bog_houses_3_nb[[i]]==0
  
}

bog_houses_3 <- bog_houses_3 %>% mutate(kindergarten_final = ifelse(val_kindergarten == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_kindergarten)))) 

drop <- c("num_kindergarten", "val_kindergarten")
bog_houses_3 <- bog_houses_3[,!names(bog_houses_3)%in%drop]

bog_houses_3 <- bog_houses_3 %>% mutate(kindergarten = kindergarten_final - vecinos_final)


# library

bog_houses_3 <- bind_rows(bog_houses_3, osm_library_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_3_sp <- bog_houses_3%>%st_buffer(300)%>%as_Spatial()

bog_houses_3_nb <- poly2nb(pl=bog_houses_3_sp, queen= TRUE)

bog_houses_3$num_library <- NA
bog_houses_3$val_library <- NA

for (i in 1:length(bog_houses_3_nb)) {
  
  bog_houses_3$num_library[i] <- size_sum(bog_houses_3_nb[[i]])
  bog_houses_3$val_library[i] <- bog_houses_3_nb[[i]]==0
  
}

bog_houses_3 <- bog_houses_3 %>% mutate(library_final = ifelse(val_library == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_library)))) 

drop <- c("num_library", "val_library")
bog_houses_3 <- bog_houses_3[,!names(bog_houses_3)%in%drop]

bog_houses_3 <- bog_houses_3 %>% mutate(library = library_final - vecinos_final) 



# love_hotel

bog_houses_3 <- bind_rows(bog_houses_3, osm_love_hotel_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_3_sp <- bog_houses_3%>%st_buffer(300)%>%as_Spatial()

bog_houses_3_nb <- poly2nb(pl=bog_houses_3_sp, queen= TRUE)

bog_houses_3$num_love_hotel <- NA
bog_houses_3$val_love_hotel <- NA

for (i in 1:length(bog_houses_3_nb)) {
  
  bog_houses_3$num_love_hotel[i] <- size_sum(bog_houses_3_nb[[i]])
  bog_houses_3$val_love_hotel[i] <- bog_houses_3_nb[[i]]==0
  
}

bog_houses_3 <- bog_houses_3 %>% mutate(love_hotel_final = ifelse(val_love_hotel == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_love_hotel)))) 

drop <- c("num_love_hotel", "val_love_hotel")
bog_houses_3 <- bog_houses_3[,!names(bog_houses_3)%in%drop]

bog_houses_3 <- bog_houses_3 %>% mutate(love_hotel = love_hotel_final - vecinos_final) 


# marketplace

bog_houses_3 <- bind_rows(bog_houses_3, osm_marketplace_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_3_sp <- bog_houses_3%>%st_buffer(300)%>%as_Spatial()

bog_houses_3_nb <- poly2nb(pl=bog_houses_3_sp, queen= TRUE)

bog_houses_3$num_marketplace <- NA
bog_houses_3$val_marketplace <- NA

for (i in 1:length(bog_houses_3_nb)) {
  
  bog_houses_3$num_marketplace[i] <- size_sum(bog_houses_3_nb[[i]])
  bog_houses_3$val_marketplace[i] <- bog_houses_3_nb[[i]]==0
  
}

bog_houses_3 <- bog_houses_3 %>% mutate(marketplace_final = ifelse(val_marketplace == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_marketplace)))) 

drop <- c("num_marketplace", "val_marketplace")
bog_houses_3 <- bog_houses_3[,!names(bog_houses_3)%in%drop]

bog_houses_3 <- bog_houses_3 %>% mutate(marketplace =  marketplace_final - vecinos_final) 



# monastery

bog_houses_3 <- bind_rows(bog_houses_3, osm_monastery_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_3_sp <- bog_houses_3%>%st_buffer(300)%>%as_Spatial()

bog_houses_3_nb <- poly2nb(pl=bog_houses_3_sp, queen= TRUE)

bog_houses_3$num_monastery <- NA
bog_houses_3$val_monastery <- NA

for (i in 1:length(bog_houses_3_nb)) {
  
  bog_houses_3$num_monastery[i] <- size_sum(bog_houses_3_nb[[i]])
  bog_houses_3$val_monastery[i] <- bog_houses_3_nb[[i]]==0
  
}

bog_houses_3 <- bog_houses_3 %>% mutate(monastery_final = ifelse(val_monastery == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_monastery)))) 

drop <- c("num_monastery", "val_monastery")
bog_houses_3 <- bog_houses_3[,!names(bog_houses_3)%in%drop]

bog_houses_3 <- bog_houses_3 %>% mutate(monastery = monastery_final - vecinos_final) 


# parking

bog_houses_3 <- bind_rows(bog_houses_3, osm_parking_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_3_sp <- bog_houses_3%>%st_buffer(300)%>%as_Spatial()

bog_houses_3_nb <- poly2nb(pl=bog_houses_3_sp, queen= TRUE)

bog_houses_3$num_parking <- NA
bog_houses_3$val_parking <- NA

for (i in 1:length(bog_houses_3_nb)) {
  
  bog_houses_3$num_parking[i] <- size_sum(bog_houses_3_nb[[i]])
  bog_houses_3$val_parking[i] <- bog_houses_3_nb[[i]]==0
  
}

bog_houses_3 <- bog_houses_3 %>% mutate(parking_final = ifelse(val_parking == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_parking)))) 

drop <- c("num_parking", "val_parking")
bog_houses_3 <- bog_houses_3[,!names(bog_houses_3)%in%drop]

bog_houses_3 <- bog_houses_3 %>% mutate(parking = parking_final - vecinos_final) 


# police

bog_houses_3 <- bind_rows(bog_houses_3, osm_police_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_3_sp <- bog_houses_3%>%st_buffer(300)%>%as_Spatial()

bog_houses_3_nb <- poly2nb(pl=bog_houses_3_sp, queen= TRUE)

bog_houses_3$num_police <- NA
bog_houses_3$val_police <- NA

for (i in 1:length(bog_houses_3_nb)) {
  
  bog_houses_3$num_police[i] <- size_sum(bog_houses_3_nb[[i]])
  bog_houses_3$val_police[i] <- bog_houses_3_nb[[i]]==0
  
}

bog_houses_3 <- bog_houses_3 %>% mutate(police_final = ifelse(val_police == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_police)))) 

drop <- c("num_police", "val_police")
bog_houses_3 <- bog_houses_3[,!names(bog_houses_3)%in%drop]

bog_houses_3 <- bog_houses_3 %>% mutate(police = police_final - vecinos_final) 


# restaurant

bog_houses_3 <- bind_rows(bog_houses_3, osm_restaurant_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_3_sp <- bog_houses_3%>%st_buffer(300)%>%as_Spatial()

bog_houses_3_nb <- poly2nb(pl=bog_houses_3_sp, queen= TRUE)

bog_houses_3$num_restaurant <- NA
bog_houses_3$val_restaurant <- NA

for (i in 1:length(bog_houses_3_nb)) {
  
  bog_houses_3$num_restaurant[i] <- size_sum(bog_houses_3_nb[[i]])
  bog_houses_3$val_restaurant[i] <- bog_houses_3_nb[[i]]==0
  
}

bog_houses_3 <- bog_houses_3 %>% mutate(restaurant_final = ifelse(val_restaurant == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_restaurant)))) 

drop <- c("num_restaurant", "val_restaurant")
bog_houses_3 <- bog_houses_3[,!names(bog_houses_3)%in%drop]

bog_houses_3 <- bog_houses_3 %>% mutate(restaurant =  restaurant_final - vecinos_final) 

write_rds(bog_houses_3, "bog_houses_3_1.RDS")

bog_houses_3_2 <- bog_houses_3 %>% select(property_id, bank, bus_station, casino, childcare, cinema,
                                          clinic, college, community_centre, conference_centre, dentist, 
                                          doctors, events_venue, fast_food, hospital,
                                          kindergarten, library, love_hotel, marketplace, monastery,
                                          parking, police, restaurant)

write_rds(bog_houses_3_2, "bog_houses_3_2.RDS")

write_csv(bog_houses_3_2, "bog_houses_3_2.csv")



### Bogota 4

bog_houses_4_sp <- bog_houses_4%>%st_buffer(300)%>%as_Spatial()

bog_houses_4_nb <- poly2nb(pl=bog_houses_4_sp, queen= TRUE)

bog_houses_4$num_vecinos <- NA
bog_houses_4$val_vecinos <- NA

for (i in 1:length(bog_houses_4_nb)) {
  
  bog_houses_4$num_vecinos[i] <- size_sum(bog_houses_4_nb[[i]])
  bog_houses_4$val_vecinos[i] <- bog_houses_4_nb[[i]]==0
  
}

bog_houses_4 <- bog_houses_4 %>% mutate(vecinos_final = ifelse(val_vecinos == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_vecinos)))) 

drop <- c("num_vecinos", "val_vecinos")
bog_houses_4 <- bog_houses_4[,!names(bog_houses_4)%in%drop]

# Bancos

bog_houses_4 <- bind_rows(bog_houses_4, osm_bank_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_4_sp <- bog_houses_4%>%st_buffer(300)%>%as_Spatial()

bog_houses_4_nb <- poly2nb(pl=bog_houses_4_sp, queen= TRUE)

bog_houses_4$num_bank <- NA
bog_houses_4$val_bank <- NA

for (i in 1:length(bog_houses_4_nb)) {
  
  bog_houses_4$num_bank[i] <- size_sum(bog_houses_4_nb[[i]])
  bog_houses_4$val_bank[i] <- bog_houses_4_nb[[i]]==0
  
}

bog_houses_4 <- bog_houses_4 %>% mutate(bank_final = ifelse(val_bank == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_bank)))) 

drop <- c("num_bank", "val_bank")
bog_houses_4 <- bog_houses_4[,!names(bog_houses_4)%in%drop]

bog_houses_4 <- bog_houses_4 %>% mutate(bank = bank_final - vecinos_final) 

# Bus Station

bog_houses_4 <- bind_rows(bog_houses_4, osm_bus_station_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_4_sp <- bog_houses_4%>%st_buffer(300)%>%as_Spatial()

bog_houses_4_nb <- poly2nb(pl=bog_houses_4_sp, queen= TRUE)

bog_houses_4$num_bus_station <- NA
bog_houses_4$val_bus_station <- NA

for (i in 1:length(bog_houses_4_nb)) {
  
  bog_houses_4$num_bus_station[i] <- size_sum(bog_houses_4_nb[[i]])
  bog_houses_4$val_bus_station[i] <- bog_houses_4_nb[[i]]==0
  
}

bog_houses_4 <- bog_houses_4 %>% mutate(bus_station_final = ifelse(val_bus_station == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_bus_station)))) 

drop <- c("num_bus_station", "val_bus_station")
bog_houses_4 <- bog_houses_4[,!names(bog_houses_4)%in%drop]

bog_houses_4 <- bog_houses_4 %>% mutate(bus_station = bus_station_final - vecinos_final) 

# Casino

bog_houses_4 <- bind_rows(bog_houses_4, osm_casino_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_4_sp <- bog_houses_4%>%st_buffer(300)%>%as_Spatial()

bog_houses_4_nb <- poly2nb(pl=bog_houses_4_sp, queen= TRUE)

bog_houses_4$num_casino <- NA
bog_houses_4$val_casino <- NA

for (i in 1:length(bog_houses_4_nb)) {
  
  bog_houses_4$num_casino[i] <- size_sum(bog_houses_4_nb[[i]])
  bog_houses_4$val_casino[i] <- bog_houses_4_nb[[i]]==0
  
}

bog_houses_4 <- bog_houses_4 %>% mutate(casino_final = ifelse(val_casino == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_casino)))) 

drop <- c("num_casino", "val_casino")
bog_houses_4 <- bog_houses_4[,!names(bog_houses_4)%in%drop]

bog_houses_4 <- bog_houses_4 %>% mutate(casino = casino_final - vecinos_final) 


# Childcare

bog_houses_4 <- bind_rows(bog_houses_4, osm_childcare_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_4_sp <- bog_houses_4%>%st_buffer(300)%>%as_Spatial()

bog_houses_4_nb <- poly2nb(pl=bog_houses_4_sp, queen= TRUE)

bog_houses_4$num_childcare <- NA
bog_houses_4$val_childcare <- NA

for (i in 1:length(bog_houses_4_nb)) {
  
  bog_houses_4$num_childcare[i] <- size_sum(bog_houses_4_nb[[i]])
  bog_houses_4$val_childcare[i] <- bog_houses_4_nb[[i]]==0
  
}

bog_houses_4 <- bog_houses_4 %>% mutate(childcare_final = ifelse(val_childcare == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_childcare)))) 

drop <- c("num_childcare", "val_childcare")
bog_houses_4 <- bog_houses_4[,!names(bog_houses_4)%in%drop]

bog_houses_4 <- bog_houses_4 %>% mutate(childcare = childcare_final - vecinos_final) 


# Cinema

bog_houses_4 <- bind_rows(bog_houses_4, osm_cinema_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_4_sp <- bog_houses_4%>%st_buffer(300)%>%as_Spatial()

bog_houses_4_nb <- poly2nb(pl=bog_houses_4_sp, queen= TRUE)

bog_houses_4$num_cinema <- NA
bog_houses_4$val_cinema <- NA

for (i in 1:length(bog_houses_4_nb)) {
  
  bog_houses_4$num_cinema[i] <- size_sum(bog_houses_4_nb[[i]])
  bog_houses_4$val_cinema[i] <- bog_houses_4_nb[[i]]==0
  
}

bog_houses_4 <- bog_houses_4 %>% mutate(cinema_final = ifelse(val_cinema == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_cinema)))) 

drop <- c("num_cinema", "val_cinema")
bog_houses_4 <- bog_houses_4[,!names(bog_houses_4)%in%drop]

bog_houses_4 <- bog_houses_4 %>% mutate(cinema = cinema_final - vecinos_final) 


# clinic

bog_houses_4 <- bind_rows(bog_houses_4, osm_clinic_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_4_sp <- bog_houses_4%>%st_buffer(300)%>%as_Spatial()

bog_houses_4_nb <- poly2nb(pl=bog_houses_4_sp, queen= TRUE)

bog_houses_4$num_clinic <- NA
bog_houses_4$val_clinic <- NA

for (i in 1:length(bog_houses_4_nb)) {
  
  bog_houses_4$num_clinic[i] <- size_sum(bog_houses_4_nb[[i]])
  bog_houses_4$val_clinic[i] <- bog_houses_4_nb[[i]]==0
  
}

bog_houses_4 <- bog_houses_4 %>% mutate(clinic_final = ifelse(val_clinic == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_clinic)))) 

drop <- c("num_clinic", "val_clinic")
bog_houses_4 <- bog_houses_4[,!names(bog_houses_4)%in%drop]

bog_houses_4 <- bog_houses_4 %>% mutate(clinic = clinic_final - vecinos_final) 


# college

bog_houses_4 <- bind_rows(bog_houses_4, osm_college_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_4_sp <- bog_houses_4%>%st_buffer(300)%>%as_Spatial()

bog_houses_4_nb <- poly2nb(pl=bog_houses_4_sp, queen= TRUE)

bog_houses_4$num_college <- NA
bog_houses_4$val_college <- NA

for (i in 1:length(bog_houses_4_nb)) {
  
  bog_houses_4$num_college[i] <- size_sum(bog_houses_4_nb[[i]])
  bog_houses_4$val_college[i] <- bog_houses_4_nb[[i]]==0
  
}

bog_houses_4 <- bog_houses_4 %>% mutate(college_final = ifelse(val_college == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_college)))) 

drop <- c("num_college", "val_college")
bog_houses_4 <- bog_houses_4[,!names(bog_houses_4)%in%drop]

bog_houses_4 <- bog_houses_4 %>% mutate(college = college_final - vecinos_final) 


# community_centre

bog_houses_4 <- bind_rows(bog_houses_4, osm_community_centre_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_4_sp <- bog_houses_4%>%st_buffer(300)%>%as_Spatial()

bog_houses_4_nb <- poly2nb(pl=bog_houses_4_sp, queen= TRUE)

bog_houses_4$num_community_centre <- NA
bog_houses_4$val_community_centre <- NA

for (i in 1:length(bog_houses_4_nb)) {
  
  bog_houses_4$num_community_centre[i] <- size_sum(bog_houses_4_nb[[i]])
  bog_houses_4$val_community_centre[i] <- bog_houses_4_nb[[i]]==0
  
}

bog_houses_4 <- bog_houses_4 %>% mutate(community_centre_final = ifelse(val_community_centre == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_community_centre)))) 

drop <- c("num_community_centre", "val_community_centre")
bog_houses_4 <- bog_houses_4[,!names(bog_houses_4)%in%drop]

bog_houses_4 <- bog_houses_4 %>% mutate(community_centre = community_centre_final - vecinos_final) 



# conference_centre

bog_houses_4 <- bind_rows(bog_houses_4, osm_conference_centre_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_4_sp <- bog_houses_4%>%st_buffer(300)%>%as_Spatial()

bog_houses_4_nb <- poly2nb(pl=bog_houses_4_sp, queen= TRUE)

bog_houses_4$num_conference_centre <- NA
bog_houses_4$val_conference_centre <- NA

for (i in 1:length(bog_houses_4_nb)) {
  
  bog_houses_4$num_conference_centre[i] <- size_sum(bog_houses_4_nb[[i]])
  bog_houses_4$val_conference_centre[i] <- bog_houses_4_nb[[i]]==0
  
}

bog_houses_4 <- bog_houses_4 %>% mutate(conference_centre_final = ifelse(val_conference_centre == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_conference_centre)))) 

drop <- c("num_conference_centre", "val_conference_centre")
bog_houses_4 <- bog_houses_4[,!names(bog_houses_4)%in%drop]

bog_houses_4 <- bog_houses_4 %>% mutate(conference_centre = conference_centre_final - vecinos_final) 



# dentist

bog_houses_4 <- bind_rows(bog_houses_4, osm_dentist_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_4_sp <- bog_houses_4%>%st_buffer(300)%>%as_Spatial()

bog_houses_4_nb <- poly2nb(pl=bog_houses_4_sp, queen= TRUE)

bog_houses_4$num_dentist <- NA
bog_houses_4$val_dentist <- NA

for (i in 1:length(bog_houses_4_nb)) {
  
  bog_houses_4$num_dentist[i] <- size_sum(bog_houses_4_nb[[i]])
  bog_houses_4$val_dentist[i] <- bog_houses_4_nb[[i]]==0
  
}

bog_houses_4 <- bog_houses_4 %>% mutate(dentist_final = ifelse(val_dentist == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_dentist)))) 

drop <- c("num_dentist", "val_dentist")
bog_houses_4 <- bog_houses_4[,!names(bog_houses_4)%in%drop]

bog_houses_4 <- bog_houses_4 %>% mutate(dentist = dentist_final - vecinos_final) 


# doctors

bog_houses_4 <- bind_rows(bog_houses_4, osm_doctors_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_4_sp <- bog_houses_4%>%st_buffer(300)%>%as_Spatial()

bog_houses_4_nb <- poly2nb(pl=bog_houses_4_sp, queen= TRUE)

bog_houses_4$num_doctors <- NA
bog_houses_4$val_doctors <- NA

for (i in 1:length(bog_houses_4_nb)) {
  
  bog_houses_4$num_doctors[i] <- size_sum(bog_houses_4_nb[[i]])
  bog_houses_4$val_doctors[i] <- bog_houses_4_nb[[i]]==0
  
}

bog_houses_4 <- bog_houses_4 %>% mutate(doctors_final = ifelse(val_doctors == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_doctors)))) 

drop <- c("num_doctors", "val_doctors")
bog_houses_4 <- bog_houses_4[,!names(bog_houses_4)%in%drop]

bog_houses_4 <- bog_houses_4 %>% mutate(doctors = doctors_final - vecinos_final) 


# events_venue

bog_houses_4 <- bind_rows(bog_houses_4, osm_events_venue_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_4_sp <- bog_houses_4%>%st_buffer(300)%>%as_Spatial()

bog_houses_4_nb <- poly2nb(pl=bog_houses_4_sp, queen= TRUE)

bog_houses_4$num_events_venue <- NA
bog_houses_4$val_events_venue <- NA

for (i in 1:length(bog_houses_4_nb)) {
  
  bog_houses_4$num_events_venue[i] <- size_sum(bog_houses_4_nb[[i]])
  bog_houses_4$val_events_venue[i] <- bog_houses_4_nb[[i]]==0
  
}

bog_houses_4 <- bog_houses_4 %>% mutate(events_venue_final = ifelse(val_events_venue == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_events_venue)))) 

drop <- c("num_events_venue", "val_events_venue")
bog_houses_4 <- bog_houses_4[,!names(bog_houses_4)%in%drop]

bog_houses_4 <- bog_houses_4 %>% mutate(events_venue = events_venue_final - vecinos_final) 


# fast_food

bog_houses_4 <- bind_rows(bog_houses_4, osm_fast_food_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_4_sp <- bog_houses_4%>%st_buffer(300)%>%as_Spatial()

bog_houses_4_nb <- poly2nb(pl=bog_houses_4_sp, queen= TRUE)

bog_houses_4$num_fast_food <- NA
bog_houses_4$val_fast_food <- NA

for (i in 1:length(bog_houses_4_nb)) {
  
  bog_houses_4$num_fast_food[i] <- size_sum(bog_houses_4_nb[[i]])
  bog_houses_4$val_fast_food[i] <- bog_houses_4_nb[[i]]==0
  
}

bog_houses_4 <- bog_houses_4 %>% mutate(fast_food_final = ifelse(val_fast_food == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_fast_food)))) 

drop <- c("num_fast_food", "val_fast_food")
bog_houses_4 <- bog_houses_4[,!names(bog_houses_4)%in%drop]

bog_houses_4 <- bog_houses_4 %>% mutate(fast_food = fast_food_final - vecinos_final) 


# hospital

bog_houses_4 <- bind_rows(bog_houses_4, osm_hospital_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_4_sp <- bog_houses_4%>%st_buffer(300)%>%as_Spatial()

bog_houses_4_nb <- poly2nb(pl=bog_houses_4_sp, queen= TRUE)

bog_houses_4$num_hospital <- NA
bog_houses_4$val_hospital <- NA

for (i in 1:length(bog_houses_4_nb)) {
  
  bog_houses_4$num_hospital[i] <- size_sum(bog_houses_4_nb[[i]])
  bog_houses_4$val_hospital[i] <- bog_houses_4_nb[[i]]==0
  
}

bog_houses_4 <- bog_houses_4 %>% mutate(hospital_final = ifelse(val_hospital == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_hospital)))) 

drop <- c("num_hospital", "val_hospital")
bog_houses_4 <- bog_houses_4[,!names(bog_houses_4)%in%drop]

bog_houses_4 <- bog_houses_4 %>% mutate(hospital = hospital_final - vecinos_final) 


# kindergarten

bog_houses_4 <- bind_rows(bog_houses_4, osm_kindergarten_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_4_sp <- bog_houses_4%>%st_buffer(300)%>%as_Spatial()

bog_houses_4_nb <- poly2nb(pl=bog_houses_4_sp, queen= TRUE)

bog_houses_4$num_kindergarten <- NA
bog_houses_4$val_kindergarten <- NA

for (i in 1:length(bog_houses_4_nb)) {
  
  bog_houses_4$num_kindergarten[i] <- size_sum(bog_houses_4_nb[[i]])
  bog_houses_4$val_kindergarten[i] <- bog_houses_4_nb[[i]]==0
  
}

bog_houses_4 <- bog_houses_4 %>% mutate(kindergarten_final = ifelse(val_kindergarten == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_kindergarten)))) 

drop <- c("num_kindergarten", "val_kindergarten")
bog_houses_4 <- bog_houses_4[,!names(bog_houses_4)%in%drop]

bog_houses_4 <- bog_houses_4 %>% mutate(kindergarten = kindergarten_final - vecinos_final)


# library

bog_houses_4 <- bind_rows(bog_houses_4, osm_library_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_4_sp <- bog_houses_4%>%st_buffer(300)%>%as_Spatial()

bog_houses_4_nb <- poly2nb(pl=bog_houses_4_sp, queen= TRUE)

bog_houses_4$num_library <- NA
bog_houses_4$val_library <- NA

for (i in 1:length(bog_houses_4_nb)) {
  
  bog_houses_4$num_library[i] <- size_sum(bog_houses_4_nb[[i]])
  bog_houses_4$val_library[i] <- bog_houses_4_nb[[i]]==0
  
}

bog_houses_4 <- bog_houses_4 %>% mutate(library_final = ifelse(val_library == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_library)))) 

drop <- c("num_library", "val_library")
bog_houses_4 <- bog_houses_4[,!names(bog_houses_4)%in%drop]

bog_houses_4 <- bog_houses_4 %>% mutate(library = library_final - vecinos_final) 



# love_hotel

bog_houses_4 <- bind_rows(bog_houses_4, osm_love_hotel_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_4_sp <- bog_houses_4%>%st_buffer(300)%>%as_Spatial()

bog_houses_4_nb <- poly2nb(pl=bog_houses_4_sp, queen= TRUE)

bog_houses_4$num_love_hotel <- NA
bog_houses_4$val_love_hotel <- NA

for (i in 1:length(bog_houses_4_nb)) {
  
  bog_houses_4$num_love_hotel[i] <- size_sum(bog_houses_4_nb[[i]])
  bog_houses_4$val_love_hotel[i] <- bog_houses_4_nb[[i]]==0
  
}

bog_houses_4 <- bog_houses_4 %>% mutate(love_hotel_final = ifelse(val_love_hotel == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_love_hotel)))) 

drop <- c("num_love_hotel", "val_love_hotel")
bog_houses_4 <- bog_houses_4[,!names(bog_houses_4)%in%drop]

bog_houses_4 <- bog_houses_4 %>% mutate(love_hotel = love_hotel_final - vecinos_final) 


# marketplace

bog_houses_4 <- bind_rows(bog_houses_4, osm_marketplace_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_4_sp <- bog_houses_4%>%st_buffer(300)%>%as_Spatial()

bog_houses_4_nb <- poly2nb(pl=bog_houses_4_sp, queen= TRUE)

bog_houses_4$num_marketplace <- NA
bog_houses_4$val_marketplace <- NA

for (i in 1:length(bog_houses_4_nb)) {
  
  bog_houses_4$num_marketplace[i] <- size_sum(bog_houses_4_nb[[i]])
  bog_houses_4$val_marketplace[i] <- bog_houses_4_nb[[i]]==0
  
}

bog_houses_4 <- bog_houses_4 %>% mutate(marketplace_final = ifelse(val_marketplace == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_marketplace)))) 

drop <- c("num_marketplace", "val_marketplace")
bog_houses_4 <- bog_houses_4[,!names(bog_houses_4)%in%drop]

bog_houses_4 <- bog_houses_4 %>% mutate(marketplace =  marketplace_final - vecinos_final) 



# monastery

bog_houses_4 <- bind_rows(bog_houses_4, osm_monastery_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_4_sp <- bog_houses_4%>%st_buffer(300)%>%as_Spatial()

bog_houses_4_nb <- poly2nb(pl=bog_houses_4_sp, queen= TRUE)

bog_houses_4$num_monastery <- NA
bog_houses_4$val_monastery <- NA

for (i in 1:length(bog_houses_4_nb)) {
  
  bog_houses_4$num_monastery[i] <- size_sum(bog_houses_4_nb[[i]])
  bog_houses_4$val_monastery[i] <- bog_houses_4_nb[[i]]==0
  
}

bog_houses_4 <- bog_houses_4 %>% mutate(monastery_final = ifelse(val_monastery == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_monastery)))) 

drop <- c("num_monastery", "val_monastery")
bog_houses_4 <- bog_houses_4[,!names(bog_houses_4)%in%drop]

bog_houses_4 <- bog_houses_4 %>% mutate(monastery = monastery_final - vecinos_final) 


# parking

bog_houses_4 <- bind_rows(bog_houses_4, osm_parking_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_4_sp <- bog_houses_4%>%st_buffer(300)%>%as_Spatial()

bog_houses_4_nb <- poly2nb(pl=bog_houses_4_sp, queen= TRUE)

bog_houses_4$num_parking <- NA
bog_houses_4$val_parking <- NA

for (i in 1:length(bog_houses_4_nb)) {
  
  bog_houses_4$num_parking[i] <- size_sum(bog_houses_4_nb[[i]])
  bog_houses_4$val_parking[i] <- bog_houses_4_nb[[i]]==0
  
}

bog_houses_4 <- bog_houses_4 %>% mutate(parking_final = ifelse(val_parking == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_parking)))) 

drop <- c("num_parking", "val_parking")
bog_houses_4 <- bog_houses_4[,!names(bog_houses_4)%in%drop]

bog_houses_4 <- bog_houses_4 %>% mutate(parking = parking_final - vecinos_final) 


# police

bog_houses_4 <- bind_rows(bog_houses_4, osm_police_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_4_sp <- bog_houses_4%>%st_buffer(300)%>%as_Spatial()

bog_houses_4_nb <- poly2nb(pl=bog_houses_4_sp, queen= TRUE)

bog_houses_4$num_police <- NA
bog_houses_4$val_police <- NA

for (i in 1:length(bog_houses_4_nb)) {
  
  bog_houses_4$num_police[i] <- size_sum(bog_houses_4_nb[[i]])
  bog_houses_4$val_police[i] <- bog_houses_4_nb[[i]]==0
  
}

bog_houses_4 <- bog_houses_4 %>% mutate(police_final = ifelse(val_police == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_police)))) 

drop <- c("num_police", "val_police")
bog_houses_4 <- bog_houses_4[,!names(bog_houses_4)%in%drop]

bog_houses_4 <- bog_houses_4 %>% mutate(police = police_final - vecinos_final) 


# restaurant

bog_houses_4 <- bind_rows(bog_houses_4, osm_restaurant_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_4_sp <- bog_houses_4%>%st_buffer(300)%>%as_Spatial()

bog_houses_4_nb <- poly2nb(pl=bog_houses_4_sp, queen= TRUE)

bog_houses_4$num_restaurant <- NA
bog_houses_4$val_restaurant <- NA

for (i in 1:length(bog_houses_4_nb)) {
  
  bog_houses_4$num_restaurant[i] <- size_sum(bog_houses_4_nb[[i]])
  bog_houses_4$val_restaurant[i] <- bog_houses_4_nb[[i]]==0
  
}

bog_houses_4 <- bog_houses_4 %>% mutate(restaurant_final = ifelse(val_restaurant == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_restaurant)))) 

drop <- c("num_restaurant", "val_restaurant")
bog_houses_4 <- bog_houses_4[,!names(bog_houses_4)%in%drop]

bog_houses_4 <- bog_houses_4 %>% mutate(restaurant =  restaurant_final - vecinos_final) 

write_rds(bog_houses_4, "bog_houses_4_1.RDS")

bog_houses_4_2 <- bog_houses_4 %>% select(property_id, bank, bus_station, casino, childcare, cinema,
                                          clinic, college, community_centre, conference_centre, dentist, 
                                          doctors, events_venue, fast_food, hospital,
                                          kindergarten, library, love_hotel, marketplace, monastery,
                                          parking, police, restaurant)

write_rds(bog_houses_4_2, "bog_houses_4_2.RDS")

write_csv(bog_houses_4_2, "bog_houses_4_2.csv")


### Bogota 5

bog_houses_5_sp <- bog_houses_5%>%st_buffer(300)%>%as_Spatial()

bog_houses_5_nb <- poly2nb(pl=bog_houses_5_sp, queen= TRUE)

bog_houses_5$num_vecinos <- NA
bog_houses_5$val_vecinos <- NA

for (i in 1:length(bog_houses_5_nb)) {
  
  bog_houses_5$num_vecinos[i] <- size_sum(bog_houses_5_nb[[i]])
  bog_houses_5$val_vecinos[i] <- bog_houses_5_nb[[i]]==0
  
}

bog_houses_5 <- bog_houses_5 %>% mutate(vecinos_final = ifelse(val_vecinos == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_vecinos)))) 

drop <- c("num_vecinos", "val_vecinos")
bog_houses_5 <- bog_houses_5[,!names(bog_houses_5)%in%drop]

# Bancos

bog_houses_5 <- bind_rows(bog_houses_5, osm_bank_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_5_sp <- bog_houses_5%>%st_buffer(300)%>%as_Spatial()

bog_houses_5_nb <- poly2nb(pl=bog_houses_5_sp, queen= TRUE)

bog_houses_5$num_bank <- NA
bog_houses_5$val_bank <- NA

for (i in 1:length(bog_houses_5_nb)) {
  
  bog_houses_5$num_bank[i] <- size_sum(bog_houses_5_nb[[i]])
  bog_houses_5$val_bank[i] <- bog_houses_5_nb[[i]]==0
  
}

bog_houses_5 <- bog_houses_5 %>% mutate(bank_final = ifelse(val_bank == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_bank)))) 

drop <- c("num_bank", "val_bank")
bog_houses_5 <- bog_houses_5[,!names(bog_houses_5)%in%drop]

bog_houses_5 <- bog_houses_5 %>% mutate(bank = bank_final - vecinos_final) 

# Bus Station

bog_houses_5 <- bind_rows(bog_houses_5, osm_bus_station_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_5_sp <- bog_houses_5%>%st_buffer(300)%>%as_Spatial()

bog_houses_5_nb <- poly2nb(pl=bog_houses_5_sp, queen= TRUE)

bog_houses_5$num_bus_station <- NA
bog_houses_5$val_bus_station <- NA

for (i in 1:length(bog_houses_5_nb)) {
  
  bog_houses_5$num_bus_station[i] <- size_sum(bog_houses_5_nb[[i]])
  bog_houses_5$val_bus_station[i] <- bog_houses_5_nb[[i]]==0
  
}

bog_houses_5 <- bog_houses_5 %>% mutate(bus_station_final = ifelse(val_bus_station == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_bus_station)))) 

drop <- c("num_bus_station", "val_bus_station")
bog_houses_5 <- bog_houses_5[,!names(bog_houses_5)%in%drop]

bog_houses_5 <- bog_houses_5 %>% mutate(bus_station = bus_station_final - vecinos_final) 

# Casino

bog_houses_5 <- bind_rows(bog_houses_5, osm_casino_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_5_sp <- bog_houses_5%>%st_buffer(300)%>%as_Spatial()

bog_houses_5_nb <- poly2nb(pl=bog_houses_5_sp, queen= TRUE)

bog_houses_5$num_casino <- NA
bog_houses_5$val_casino <- NA

for (i in 1:length(bog_houses_5_nb)) {
  
  bog_houses_5$num_casino[i] <- size_sum(bog_houses_5_nb[[i]])
  bog_houses_5$val_casino[i] <- bog_houses_5_nb[[i]]==0
  
}

bog_houses_5 <- bog_houses_5 %>% mutate(casino_final = ifelse(val_casino == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_casino)))) 

drop <- c("num_casino", "val_casino")
bog_houses_5 <- bog_houses_5[,!names(bog_houses_5)%in%drop]

bog_houses_5 <- bog_houses_5 %>% mutate(casino = casino_final - vecinos_final) 


# Childcare

bog_houses_5 <- bind_rows(bog_houses_5, osm_childcare_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_5_sp <- bog_houses_5%>%st_buffer(300)%>%as_Spatial()

bog_houses_5_nb <- poly2nb(pl=bog_houses_5_sp, queen= TRUE)

bog_houses_5$num_childcare <- NA
bog_houses_5$val_childcare <- NA

for (i in 1:length(bog_houses_5_nb)) {
  
  bog_houses_5$num_childcare[i] <- size_sum(bog_houses_5_nb[[i]])
  bog_houses_5$val_childcare[i] <- bog_houses_5_nb[[i]]==0
  
}

bog_houses_5 <- bog_houses_5 %>% mutate(childcare_final = ifelse(val_childcare == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_childcare)))) 

drop <- c("num_childcare", "val_childcare")
bog_houses_5 <- bog_houses_5[,!names(bog_houses_5)%in%drop]

bog_houses_5 <- bog_houses_5 %>% mutate(childcare = childcare_final - vecinos_final) 


# Cinema

bog_houses_5 <- bind_rows(bog_houses_5, osm_cinema_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_5_sp <- bog_houses_5%>%st_buffer(300)%>%as_Spatial()

bog_houses_5_nb <- poly2nb(pl=bog_houses_5_sp, queen= TRUE)

bog_houses_5$num_cinema <- NA
bog_houses_5$val_cinema <- NA

for (i in 1:length(bog_houses_5_nb)) {
  
  bog_houses_5$num_cinema[i] <- size_sum(bog_houses_5_nb[[i]])
  bog_houses_5$val_cinema[i] <- bog_houses_5_nb[[i]]==0
  
}

bog_houses_5 <- bog_houses_5 %>% mutate(cinema_final = ifelse(val_cinema == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_cinema)))) 

drop <- c("num_cinema", "val_cinema")
bog_houses_5 <- bog_houses_5[,!names(bog_houses_5)%in%drop]

bog_houses_5 <- bog_houses_5 %>% mutate(cinema = cinema_final - vecinos_final) 


# clinic

bog_houses_5 <- bind_rows(bog_houses_5, osm_clinic_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_5_sp <- bog_houses_5%>%st_buffer(300)%>%as_Spatial()

bog_houses_5_nb <- poly2nb(pl=bog_houses_5_sp, queen= TRUE)

bog_houses_5$num_clinic <- NA
bog_houses_5$val_clinic <- NA

for (i in 1:length(bog_houses_5_nb)) {
  
  bog_houses_5$num_clinic[i] <- size_sum(bog_houses_5_nb[[i]])
  bog_houses_5$val_clinic[i] <- bog_houses_5_nb[[i]]==0
  
}

bog_houses_5 <- bog_houses_5 %>% mutate(clinic_final = ifelse(val_clinic == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_clinic)))) 

drop <- c("num_clinic", "val_clinic")
bog_houses_5 <- bog_houses_5[,!names(bog_houses_5)%in%drop]

bog_houses_5 <- bog_houses_5 %>% mutate(clinic = clinic_final - vecinos_final) 


# college

bog_houses_5 <- bind_rows(bog_houses_5, osm_college_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_5_sp <- bog_houses_5%>%st_buffer(300)%>%as_Spatial()

bog_houses_5_nb <- poly2nb(pl=bog_houses_5_sp, queen= TRUE)

bog_houses_5$num_college <- NA
bog_houses_5$val_college <- NA

for (i in 1:length(bog_houses_5_nb)) {
  
  bog_houses_5$num_college[i] <- size_sum(bog_houses_5_nb[[i]])
  bog_houses_5$val_college[i] <- bog_houses_5_nb[[i]]==0
  
}

bog_houses_5 <- bog_houses_5 %>% mutate(college_final = ifelse(val_college == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_college)))) 

drop <- c("num_college", "val_college")
bog_houses_5 <- bog_houses_5[,!names(bog_houses_5)%in%drop]

bog_houses_5 <- bog_houses_5 %>% mutate(college = college_final - vecinos_final) 


# community_centre

bog_houses_5 <- bind_rows(bog_houses_5, osm_community_centre_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_5_sp <- bog_houses_5%>%st_buffer(300)%>%as_Spatial()

bog_houses_5_nb <- poly2nb(pl=bog_houses_5_sp, queen= TRUE)

bog_houses_5$num_community_centre <- NA
bog_houses_5$val_community_centre <- NA

for (i in 1:length(bog_houses_5_nb)) {
  
  bog_houses_5$num_community_centre[i] <- size_sum(bog_houses_5_nb[[i]])
  bog_houses_5$val_community_centre[i] <- bog_houses_5_nb[[i]]==0
  
}

bog_houses_5 <- bog_houses_5 %>% mutate(community_centre_final = ifelse(val_community_centre == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_community_centre)))) 

drop <- c("num_community_centre", "val_community_centre")
bog_houses_5 <- bog_houses_5[,!names(bog_houses_5)%in%drop]

bog_houses_5 <- bog_houses_5 %>% mutate(community_centre = community_centre_final - vecinos_final) 



# conference_centre

bog_houses_5 <- bind_rows(bog_houses_5, osm_conference_centre_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_5_sp <- bog_houses_5%>%st_buffer(300)%>%as_Spatial()

bog_houses_5_nb <- poly2nb(pl=bog_houses_5_sp, queen= TRUE)

bog_houses_5$num_conference_centre <- NA
bog_houses_5$val_conference_centre <- NA

for (i in 1:length(bog_houses_5_nb)) {
  
  bog_houses_5$num_conference_centre[i] <- size_sum(bog_houses_5_nb[[i]])
  bog_houses_5$val_conference_centre[i] <- bog_houses_5_nb[[i]]==0
  
}

bog_houses_5 <- bog_houses_5 %>% mutate(conference_centre_final = ifelse(val_conference_centre == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_conference_centre)))) 

drop <- c("num_conference_centre", "val_conference_centre")
bog_houses_5 <- bog_houses_5[,!names(bog_houses_5)%in%drop]

bog_houses_5 <- bog_houses_5 %>% mutate(conference_centre = conference_centre_final - vecinos_final) 



# dentist

bog_houses_5 <- bind_rows(bog_houses_5, osm_dentist_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_5_sp <- bog_houses_5%>%st_buffer(300)%>%as_Spatial()

bog_houses_5_nb <- poly2nb(pl=bog_houses_5_sp, queen= TRUE)

bog_houses_5$num_dentist <- NA
bog_houses_5$val_dentist <- NA

for (i in 1:length(bog_houses_5_nb)) {
  
  bog_houses_5$num_dentist[i] <- size_sum(bog_houses_5_nb[[i]])
  bog_houses_5$val_dentist[i] <- bog_houses_5_nb[[i]]==0
  
}

bog_houses_5 <- bog_houses_5 %>% mutate(dentist_final = ifelse(val_dentist == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_dentist)))) 

drop <- c("num_dentist", "val_dentist")
bog_houses_5 <- bog_houses_5[,!names(bog_houses_5)%in%drop]

bog_houses_5 <- bog_houses_5 %>% mutate(dentist = dentist_final - vecinos_final) 


# doctors

bog_houses_5 <- bind_rows(bog_houses_5, osm_doctors_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_5_sp <- bog_houses_5%>%st_buffer(300)%>%as_Spatial()

bog_houses_5_nb <- poly2nb(pl=bog_houses_5_sp, queen= TRUE)

bog_houses_5$num_doctors <- NA
bog_houses_5$val_doctors <- NA

for (i in 1:length(bog_houses_5_nb)) {
  
  bog_houses_5$num_doctors[i] <- size_sum(bog_houses_5_nb[[i]])
  bog_houses_5$val_doctors[i] <- bog_houses_5_nb[[i]]==0
  
}

bog_houses_5 <- bog_houses_5 %>% mutate(doctors_final = ifelse(val_doctors == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_doctors)))) 

drop <- c("num_doctors", "val_doctors")
bog_houses_5 <- bog_houses_5[,!names(bog_houses_5)%in%drop]

bog_houses_5 <- bog_houses_5 %>% mutate(doctors = doctors_final - vecinos_final) 


# events_venue

bog_houses_5 <- bind_rows(bog_houses_5, osm_events_venue_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_5_sp <- bog_houses_5%>%st_buffer(300)%>%as_Spatial()

bog_houses_5_nb <- poly2nb(pl=bog_houses_5_sp, queen= TRUE)

bog_houses_5$num_events_venue <- NA
bog_houses_5$val_events_venue <- NA

for (i in 1:length(bog_houses_5_nb)) {
  
  bog_houses_5$num_events_venue[i] <- size_sum(bog_houses_5_nb[[i]])
  bog_houses_5$val_events_venue[i] <- bog_houses_5_nb[[i]]==0
  
}

bog_houses_5 <- bog_houses_5 %>% mutate(events_venue_final = ifelse(val_events_venue == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_events_venue)))) 

drop <- c("num_events_venue", "val_events_venue")
bog_houses_5 <- bog_houses_5[,!names(bog_houses_5)%in%drop]

bog_houses_5 <- bog_houses_5 %>% mutate(events_venue = events_venue_final - vecinos_final) 


# fast_food

bog_houses_5 <- bind_rows(bog_houses_5, osm_fast_food_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_5_sp <- bog_houses_5%>%st_buffer(300)%>%as_Spatial()

bog_houses_5_nb <- poly2nb(pl=bog_houses_5_sp, queen= TRUE)

bog_houses_5$num_fast_food <- NA
bog_houses_5$val_fast_food <- NA

for (i in 1:length(bog_houses_5_nb)) {
  
  bog_houses_5$num_fast_food[i] <- size_sum(bog_houses_5_nb[[i]])
  bog_houses_5$val_fast_food[i] <- bog_houses_5_nb[[i]]==0
  
}

bog_houses_5 <- bog_houses_5 %>% mutate(fast_food_final = ifelse(val_fast_food == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_fast_food)))) 

drop <- c("num_fast_food", "val_fast_food")
bog_houses_5 <- bog_houses_5[,!names(bog_houses_5)%in%drop]

bog_houses_5 <- bog_houses_5 %>% mutate(fast_food = fast_food_final - vecinos_final) 


# hospital

bog_houses_5 <- bind_rows(bog_houses_5, osm_hospital_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_5_sp <- bog_houses_5%>%st_buffer(300)%>%as_Spatial()

bog_houses_5_nb <- poly2nb(pl=bog_houses_5_sp, queen= TRUE)

bog_houses_5$num_hospital <- NA
bog_houses_5$val_hospital <- NA

for (i in 1:length(bog_houses_5_nb)) {
  
  bog_houses_5$num_hospital[i] <- size_sum(bog_houses_5_nb[[i]])
  bog_houses_5$val_hospital[i] <- bog_houses_5_nb[[i]]==0
  
}

bog_houses_5 <- bog_houses_5 %>% mutate(hospital_final = ifelse(val_hospital == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_hospital)))) 

drop <- c("num_hospital", "val_hospital")
bog_houses_5 <- bog_houses_5[,!names(bog_houses_5)%in%drop]

bog_houses_5 <- bog_houses_5 %>% mutate(hospital = hospital_final - vecinos_final) 


# kindergarten

bog_houses_5 <- bind_rows(bog_houses_5, osm_kindergarten_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_5_sp <- bog_houses_5%>%st_buffer(300)%>%as_Spatial()

bog_houses_5_nb <- poly2nb(pl=bog_houses_5_sp, queen= TRUE)

bog_houses_5$num_kindergarten <- NA
bog_houses_5$val_kindergarten <- NA

for (i in 1:length(bog_houses_5_nb)) {
  
  bog_houses_5$num_kindergarten[i] <- size_sum(bog_houses_5_nb[[i]])
  bog_houses_5$val_kindergarten[i] <- bog_houses_5_nb[[i]]==0
  
}

bog_houses_5 <- bog_houses_5 %>% mutate(kindergarten_final = ifelse(val_kindergarten == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_kindergarten)))) 

drop <- c("num_kindergarten", "val_kindergarten")
bog_houses_5 <- bog_houses_5[,!names(bog_houses_5)%in%drop]

bog_houses_5 <- bog_houses_5 %>% mutate(kindergarten = kindergarten_final - vecinos_final)


# library

bog_houses_5 <- bind_rows(bog_houses_5, osm_library_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_5_sp <- bog_houses_5%>%st_buffer(300)%>%as_Spatial()

bog_houses_5_nb <- poly2nb(pl=bog_houses_5_sp, queen= TRUE)

bog_houses_5$num_library <- NA
bog_houses_5$val_library <- NA

for (i in 1:length(bog_houses_5_nb)) {
  
  bog_houses_5$num_library[i] <- size_sum(bog_houses_5_nb[[i]])
  bog_houses_5$val_library[i] <- bog_houses_5_nb[[i]]==0
  
}

bog_houses_5 <- bog_houses_5 %>% mutate(library_final = ifelse(val_library == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_library)))) 

drop <- c("num_library", "val_library")
bog_houses_5 <- bog_houses_5[,!names(bog_houses_5)%in%drop]

bog_houses_5 <- bog_houses_5 %>% mutate(library = library_final - vecinos_final) 



# love_hotel

bog_houses_5 <- bind_rows(bog_houses_5, osm_love_hotel_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_5_sp <- bog_houses_5%>%st_buffer(300)%>%as_Spatial()

bog_houses_5_nb <- poly2nb(pl=bog_houses_5_sp, queen= TRUE)

bog_houses_5$num_love_hotel <- NA
bog_houses_5$val_love_hotel <- NA

for (i in 1:length(bog_houses_5_nb)) {
  
  bog_houses_5$num_love_hotel[i] <- size_sum(bog_houses_5_nb[[i]])
  bog_houses_5$val_love_hotel[i] <- bog_houses_5_nb[[i]]==0
  
}

bog_houses_5 <- bog_houses_5 %>% mutate(love_hotel_final = ifelse(val_love_hotel == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_love_hotel)))) 

drop <- c("num_love_hotel", "val_love_hotel")
bog_houses_5 <- bog_houses_5[,!names(bog_houses_5)%in%drop]

bog_houses_5 <- bog_houses_5 %>% mutate(love_hotel = love_hotel_final - vecinos_final) 


# marketplace

bog_houses_5 <- bind_rows(bog_houses_5, osm_marketplace_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_5_sp <- bog_houses_5%>%st_buffer(300)%>%as_Spatial()

bog_houses_5_nb <- poly2nb(pl=bog_houses_5_sp, queen= TRUE)

bog_houses_5$num_marketplace <- NA
bog_houses_5$val_marketplace <- NA

for (i in 1:length(bog_houses_5_nb)) {
  
  bog_houses_5$num_marketplace[i] <- size_sum(bog_houses_5_nb[[i]])
  bog_houses_5$val_marketplace[i] <- bog_houses_5_nb[[i]]==0
  
}

bog_houses_5 <- bog_houses_5 %>% mutate(marketplace_final = ifelse(val_marketplace == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_marketplace)))) 

drop <- c("num_marketplace", "val_marketplace")
bog_houses_5 <- bog_houses_5[,!names(bog_houses_5)%in%drop]

bog_houses_5 <- bog_houses_5 %>% mutate(marketplace =  marketplace_final - vecinos_final) 



# monastery

bog_houses_5 <- bind_rows(bog_houses_5, osm_monastery_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_5_sp <- bog_houses_5%>%st_buffer(300)%>%as_Spatial()

bog_houses_5_nb <- poly2nb(pl=bog_houses_5_sp, queen= TRUE)

bog_houses_5$num_monastery <- NA
bog_houses_5$val_monastery <- NA

for (i in 1:length(bog_houses_5_nb)) {
  
  bog_houses_5$num_monastery[i] <- size_sum(bog_houses_5_nb[[i]])
  bog_houses_5$val_monastery[i] <- bog_houses_5_nb[[i]]==0
  
}

bog_houses_5 <- bog_houses_5 %>% mutate(monastery_final = ifelse(val_monastery == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_monastery)))) 

drop <- c("num_monastery", "val_monastery")
bog_houses_5 <- bog_houses_5[,!names(bog_houses_5)%in%drop]

bog_houses_5 <- bog_houses_5 %>% mutate(monastery = monastery_final - vecinos_final) 


# parking

bog_houses_5 <- bind_rows(bog_houses_5, osm_parking_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_5_sp <- bog_houses_5%>%st_buffer(300)%>%as_Spatial()

bog_houses_5_nb <- poly2nb(pl=bog_houses_5_sp, queen= TRUE)

bog_houses_5$num_parking <- NA
bog_houses_5$val_parking <- NA

for (i in 1:length(bog_houses_5_nb)) {
  
  bog_houses_5$num_parking[i] <- size_sum(bog_houses_5_nb[[i]])
  bog_houses_5$val_parking[i] <- bog_houses_5_nb[[i]]==0
  
}

bog_houses_5 <- bog_houses_5 %>% mutate(parking_final = ifelse(val_parking == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_parking)))) 

drop <- c("num_parking", "val_parking")
bog_houses_5 <- bog_houses_5[,!names(bog_houses_5)%in%drop]

bog_houses_5 <- bog_houses_5 %>% mutate(parking = parking_final - vecinos_final) 


# police

bog_houses_5 <- bind_rows(bog_houses_5, osm_police_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_5_sp <- bog_houses_5%>%st_buffer(300)%>%as_Spatial()

bog_houses_5_nb <- poly2nb(pl=bog_houses_5_sp, queen= TRUE)

bog_houses_5$num_police <- NA
bog_houses_5$val_police <- NA

for (i in 1:length(bog_houses_5_nb)) {
  
  bog_houses_5$num_police[i] <- size_sum(bog_houses_5_nb[[i]])
  bog_houses_5$val_police[i] <- bog_houses_5_nb[[i]]==0
  
}

bog_houses_5 <- bog_houses_5 %>% mutate(police_final = ifelse(val_police == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_police)))) 

drop <- c("num_police", "val_police")
bog_houses_5 <- bog_houses_5[,!names(bog_houses_5)%in%drop]

bog_houses_5 <- bog_houses_5 %>% mutate(police = police_final - vecinos_final) 


# restaurant

bog_houses_5 <- bind_rows(bog_houses_5, osm_restaurant_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_5_sp <- bog_houses_5%>%st_buffer(300)%>%as_Spatial()

bog_houses_5_nb <- poly2nb(pl=bog_houses_5_sp, queen= TRUE)

bog_houses_5$num_restaurant <- NA
bog_houses_5$val_restaurant <- NA

for (i in 1:length(bog_houses_5_nb)) {
  
  bog_houses_5$num_restaurant[i] <- size_sum(bog_houses_5_nb[[i]])
  bog_houses_5$val_restaurant[i] <- bog_houses_5_nb[[i]]==0
  
}

bog_houses_5 <- bog_houses_5 %>% mutate(restaurant_final = ifelse(val_restaurant == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_restaurant)))) 

drop <- c("num_restaurant", "val_restaurant")
bog_houses_5 <- bog_houses_5[,!names(bog_houses_5)%in%drop]

bog_houses_5 <- bog_houses_5 %>% mutate(restaurant =  restaurant_final - vecinos_final) 

write_rds(bog_houses_5, "bog_houses_5_1.RDS")

bog_houses_5_2 <- bog_houses_5 %>% select(property_id, bank, bus_station, casino, childcare, cinema,
                                          clinic, college, community_centre, conference_centre, dentist, 
                                          doctors, events_venue, fast_food, hospital,
                                          kindergarten, library, love_hotel, marketplace, monastery,
                                          parking, police, restaurant)

write_rds(bog_houses_5_2, "bog_houses_5_2.RDS")

write_csv(bog_houses_5_2, "bog_houses_5_2.csv")



### Bogota 6

bog_houses_6_sp <- bog_houses_6%>%st_buffer(300)%>%as_Spatial()

bog_houses_6_nb <- poly2nb(pl=bog_houses_6_sp, queen= TRUE)

bog_houses_6$num_vecinos <- NA
bog_houses_6$val_vecinos <- NA

for (i in 1:length(bog_houses_6_nb)) {
  
  bog_houses_6$num_vecinos[i] <- size_sum(bog_houses_6_nb[[i]])
  bog_houses_6$val_vecinos[i] <- bog_houses_6_nb[[i]]==0
  
}

bog_houses_6 <- bog_houses_6 %>% mutate(vecinos_final = ifelse(val_vecinos == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_vecinos)))) 

drop <- c("num_vecinos", "val_vecinos")
bog_houses_6 <- bog_houses_6[,!names(bog_houses_6)%in%drop]

# Bancos

bog_houses_6 <- bind_rows(bog_houses_6, osm_bank_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_6_sp <- bog_houses_6%>%st_buffer(300)%>%as_Spatial()

bog_houses_6_nb <- poly2nb(pl=bog_houses_6_sp, queen= TRUE)

bog_houses_6$num_bank <- NA
bog_houses_6$val_bank <- NA

for (i in 1:length(bog_houses_6_nb)) {
  
  bog_houses_6$num_bank[i] <- size_sum(bog_houses_6_nb[[i]])
  bog_houses_6$val_bank[i] <- bog_houses_6_nb[[i]]==0
  
}

bog_houses_6 <- bog_houses_6 %>% mutate(bank_final = ifelse(val_bank == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_bank)))) 

drop <- c("num_bank", "val_bank")
bog_houses_6 <- bog_houses_6[,!names(bog_houses_6)%in%drop]

bog_houses_6 <- bog_houses_6 %>% mutate(bank = bank_final - vecinos_final) 

# Bus Station

bog_houses_6 <- bind_rows(bog_houses_6, osm_bus_station_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_6_sp <- bog_houses_6%>%st_buffer(300)%>%as_Spatial()

bog_houses_6_nb <- poly2nb(pl=bog_houses_6_sp, queen= TRUE)

bog_houses_6$num_bus_station <- NA
bog_houses_6$val_bus_station <- NA

for (i in 1:length(bog_houses_6_nb)) {
  
  bog_houses_6$num_bus_station[i] <- size_sum(bog_houses_6_nb[[i]])
  bog_houses_6$val_bus_station[i] <- bog_houses_6_nb[[i]]==0
  
}

bog_houses_6 <- bog_houses_6 %>% mutate(bus_station_final = ifelse(val_bus_station == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_bus_station)))) 

drop <- c("num_bus_station", "val_bus_station")
bog_houses_6 <- bog_houses_6[,!names(bog_houses_6)%in%drop]

bog_houses_6 <- bog_houses_6 %>% mutate(bus_station = bus_station_final - vecinos_final) 

# Casino

bog_houses_6 <- bind_rows(bog_houses_6, osm_casino_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_6_sp <- bog_houses_6%>%st_buffer(300)%>%as_Spatial()

bog_houses_6_nb <- poly2nb(pl=bog_houses_6_sp, queen= TRUE)

bog_houses_6$num_casino <- NA
bog_houses_6$val_casino <- NA

for (i in 1:length(bog_houses_6_nb)) {
  
  bog_houses_6$num_casino[i] <- size_sum(bog_houses_6_nb[[i]])
  bog_houses_6$val_casino[i] <- bog_houses_6_nb[[i]]==0
  
}

bog_houses_6 <- bog_houses_6 %>% mutate(casino_final = ifelse(val_casino == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_casino)))) 

drop <- c("num_casino", "val_casino")
bog_houses_6 <- bog_houses_6[,!names(bog_houses_6)%in%drop]

bog_houses_6 <- bog_houses_6 %>% mutate(casino = casino_final - vecinos_final) 


# Childcare

bog_houses_6 <- bind_rows(bog_houses_6, osm_childcare_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_6_sp <- bog_houses_6%>%st_buffer(300)%>%as_Spatial()

bog_houses_6_nb <- poly2nb(pl=bog_houses_6_sp, queen= TRUE)

bog_houses_6$num_childcare <- NA
bog_houses_6$val_childcare <- NA

for (i in 1:length(bog_houses_6_nb)) {
  
  bog_houses_6$num_childcare[i] <- size_sum(bog_houses_6_nb[[i]])
  bog_houses_6$val_childcare[i] <- bog_houses_6_nb[[i]]==0
  
}

bog_houses_6 <- bog_houses_6 %>% mutate(childcare_final = ifelse(val_childcare == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_childcare)))) 

drop <- c("num_childcare", "val_childcare")
bog_houses_6 <- bog_houses_6[,!names(bog_houses_6)%in%drop]

bog_houses_6 <- bog_houses_6 %>% mutate(childcare = childcare_final - vecinos_final) 


# Cinema

bog_houses_6 <- bind_rows(bog_houses_6, osm_cinema_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_6_sp <- bog_houses_6%>%st_buffer(300)%>%as_Spatial()

bog_houses_6_nb <- poly2nb(pl=bog_houses_6_sp, queen= TRUE)

bog_houses_6$num_cinema <- NA
bog_houses_6$val_cinema <- NA

for (i in 1:length(bog_houses_6_nb)) {
  
  bog_houses_6$num_cinema[i] <- size_sum(bog_houses_6_nb[[i]])
  bog_houses_6$val_cinema[i] <- bog_houses_6_nb[[i]]==0
  
}

bog_houses_6 <- bog_houses_6 %>% mutate(cinema_final = ifelse(val_cinema == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_cinema)))) 

drop <- c("num_cinema", "val_cinema")
bog_houses_6 <- bog_houses_6[,!names(bog_houses_6)%in%drop]

bog_houses_6 <- bog_houses_6 %>% mutate(cinema = cinema_final - vecinos_final) 


# clinic

bog_houses_6 <- bind_rows(bog_houses_6, osm_clinic_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_6_sp <- bog_houses_6%>%st_buffer(300)%>%as_Spatial()

bog_houses_6_nb <- poly2nb(pl=bog_houses_6_sp, queen= TRUE)

bog_houses_6$num_clinic <- NA
bog_houses_6$val_clinic <- NA

for (i in 1:length(bog_houses_6_nb)) {
  
  bog_houses_6$num_clinic[i] <- size_sum(bog_houses_6_nb[[i]])
  bog_houses_6$val_clinic[i] <- bog_houses_6_nb[[i]]==0
  
}

bog_houses_6 <- bog_houses_6 %>% mutate(clinic_final = ifelse(val_clinic == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_clinic)))) 

drop <- c("num_clinic", "val_clinic")
bog_houses_6 <- bog_houses_6[,!names(bog_houses_6)%in%drop]

bog_houses_6 <- bog_houses_6 %>% mutate(clinic = clinic_final - vecinos_final) 


# college

bog_houses_6 <- bind_rows(bog_houses_6, osm_college_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_6_sp <- bog_houses_6%>%st_buffer(300)%>%as_Spatial()

bog_houses_6_nb <- poly2nb(pl=bog_houses_6_sp, queen= TRUE)

bog_houses_6$num_college <- NA
bog_houses_6$val_college <- NA

for (i in 1:length(bog_houses_6_nb)) {
  
  bog_houses_6$num_college[i] <- size_sum(bog_houses_6_nb[[i]])
  bog_houses_6$val_college[i] <- bog_houses_6_nb[[i]]==0
  
}

bog_houses_6 <- bog_houses_6 %>% mutate(college_final = ifelse(val_college == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_college)))) 

drop <- c("num_college", "val_college")
bog_houses_6 <- bog_houses_6[,!names(bog_houses_6)%in%drop]

bog_houses_6 <- bog_houses_6 %>% mutate(college = college_final - vecinos_final) 


# community_centre

bog_houses_6 <- bind_rows(bog_houses_6, osm_community_centre_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_6_sp <- bog_houses_6%>%st_buffer(300)%>%as_Spatial()

bog_houses_6_nb <- poly2nb(pl=bog_houses_6_sp, queen= TRUE)

bog_houses_6$num_community_centre <- NA
bog_houses_6$val_community_centre <- NA

for (i in 1:length(bog_houses_6_nb)) {
  
  bog_houses_6$num_community_centre[i] <- size_sum(bog_houses_6_nb[[i]])
  bog_houses_6$val_community_centre[i] <- bog_houses_6_nb[[i]]==0
  
}

bog_houses_6 <- bog_houses_6 %>% mutate(community_centre_final = ifelse(val_community_centre == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_community_centre)))) 

drop <- c("num_community_centre", "val_community_centre")
bog_houses_6 <- bog_houses_6[,!names(bog_houses_6)%in%drop]

bog_houses_6 <- bog_houses_6 %>% mutate(community_centre = community_centre_final - vecinos_final) 



# conference_centre

bog_houses_6 <- bind_rows(bog_houses_6, osm_conference_centre_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_6_sp <- bog_houses_6%>%st_buffer(300)%>%as_Spatial()

bog_houses_6_nb <- poly2nb(pl=bog_houses_6_sp, queen= TRUE)

bog_houses_6$num_conference_centre <- NA
bog_houses_6$val_conference_centre <- NA

for (i in 1:length(bog_houses_6_nb)) {
  
  bog_houses_6$num_conference_centre[i] <- size_sum(bog_houses_6_nb[[i]])
  bog_houses_6$val_conference_centre[i] <- bog_houses_6_nb[[i]]==0
  
}

bog_houses_6 <- bog_houses_6 %>% mutate(conference_centre_final = ifelse(val_conference_centre == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_conference_centre)))) 

drop <- c("num_conference_centre", "val_conference_centre")
bog_houses_6 <- bog_houses_6[,!names(bog_houses_6)%in%drop]

bog_houses_6 <- bog_houses_6 %>% mutate(conference_centre = conference_centre_final - vecinos_final) 



# dentist

bog_houses_6 <- bind_rows(bog_houses_6, osm_dentist_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_6_sp <- bog_houses_6%>%st_buffer(300)%>%as_Spatial()

bog_houses_6_nb <- poly2nb(pl=bog_houses_6_sp, queen= TRUE)

bog_houses_6$num_dentist <- NA
bog_houses_6$val_dentist <- NA

for (i in 1:length(bog_houses_6_nb)) {
  
  bog_houses_6$num_dentist[i] <- size_sum(bog_houses_6_nb[[i]])
  bog_houses_6$val_dentist[i] <- bog_houses_6_nb[[i]]==0
  
}

bog_houses_6 <- bog_houses_6 %>% mutate(dentist_final = ifelse(val_dentist == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_dentist)))) 

drop <- c("num_dentist", "val_dentist")
bog_houses_6 <- bog_houses_6[,!names(bog_houses_6)%in%drop]

bog_houses_6 <- bog_houses_6 %>% mutate(dentist = dentist_final - vecinos_final) 


# doctors

bog_houses_6 <- bind_rows(bog_houses_6, osm_doctors_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_6_sp <- bog_houses_6%>%st_buffer(300)%>%as_Spatial()

bog_houses_6_nb <- poly2nb(pl=bog_houses_6_sp, queen= TRUE)

bog_houses_6$num_doctors <- NA
bog_houses_6$val_doctors <- NA

for (i in 1:length(bog_houses_6_nb)) {
  
  bog_houses_6$num_doctors[i] <- size_sum(bog_houses_6_nb[[i]])
  bog_houses_6$val_doctors[i] <- bog_houses_6_nb[[i]]==0
  
}

bog_houses_6 <- bog_houses_6 %>% mutate(doctors_final = ifelse(val_doctors == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_doctors)))) 

drop <- c("num_doctors", "val_doctors")
bog_houses_6 <- bog_houses_6[,!names(bog_houses_6)%in%drop]

bog_houses_6 <- bog_houses_6 %>% mutate(doctors = doctors_final - vecinos_final) 


# events_venue

bog_houses_6 <- bind_rows(bog_houses_6, osm_events_venue_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_6_sp <- bog_houses_6%>%st_buffer(300)%>%as_Spatial()

bog_houses_6_nb <- poly2nb(pl=bog_houses_6_sp, queen= TRUE)

bog_houses_6$num_events_venue <- NA
bog_houses_6$val_events_venue <- NA

for (i in 1:length(bog_houses_6_nb)) {
  
  bog_houses_6$num_events_venue[i] <- size_sum(bog_houses_6_nb[[i]])
  bog_houses_6$val_events_venue[i] <- bog_houses_6_nb[[i]]==0
  
}

bog_houses_6 <- bog_houses_6 %>% mutate(events_venue_final = ifelse(val_events_venue == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_events_venue)))) 

drop <- c("num_events_venue", "val_events_venue")
bog_houses_6 <- bog_houses_6[,!names(bog_houses_6)%in%drop]

bog_houses_6 <- bog_houses_6 %>% mutate(events_venue = events_venue_final - vecinos_final) 


# fast_food

bog_houses_6 <- bind_rows(bog_houses_6, osm_fast_food_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_6_sp <- bog_houses_6%>%st_buffer(300)%>%as_Spatial()

bog_houses_6_nb <- poly2nb(pl=bog_houses_6_sp, queen= TRUE)

bog_houses_6$num_fast_food <- NA
bog_houses_6$val_fast_food <- NA

for (i in 1:length(bog_houses_6_nb)) {
  
  bog_houses_6$num_fast_food[i] <- size_sum(bog_houses_6_nb[[i]])
  bog_houses_6$val_fast_food[i] <- bog_houses_6_nb[[i]]==0
  
}

bog_houses_6 <- bog_houses_6 %>% mutate(fast_food_final = ifelse(val_fast_food == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_fast_food)))) 

drop <- c("num_fast_food", "val_fast_food")
bog_houses_6 <- bog_houses_6[,!names(bog_houses_6)%in%drop]

bog_houses_6 <- bog_houses_6 %>% mutate(fast_food = fast_food_final - vecinos_final) 


# hospital

bog_houses_6 <- bind_rows(bog_houses_6, osm_hospital_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_6_sp <- bog_houses_6%>%st_buffer(300)%>%as_Spatial()

bog_houses_6_nb <- poly2nb(pl=bog_houses_6_sp, queen= TRUE)

bog_houses_6$num_hospital <- NA
bog_houses_6$val_hospital <- NA

for (i in 1:length(bog_houses_6_nb)) {
  
  bog_houses_6$num_hospital[i] <- size_sum(bog_houses_6_nb[[i]])
  bog_houses_6$val_hospital[i] <- bog_houses_6_nb[[i]]==0
  
}

bog_houses_6 <- bog_houses_6 %>% mutate(hospital_final = ifelse(val_hospital == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_hospital)))) 

drop <- c("num_hospital", "val_hospital")
bog_houses_6 <- bog_houses_6[,!names(bog_houses_6)%in%drop]

bog_houses_6 <- bog_houses_6 %>% mutate(hospital = hospital_final - vecinos_final) 


# kindergarten

bog_houses_6 <- bind_rows(bog_houses_6, osm_kindergarten_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_6_sp <- bog_houses_6%>%st_buffer(300)%>%as_Spatial()

bog_houses_6_nb <- poly2nb(pl=bog_houses_6_sp, queen= TRUE)

bog_houses_6$num_kindergarten <- NA
bog_houses_6$val_kindergarten <- NA

for (i in 1:length(bog_houses_6_nb)) {
  
  bog_houses_6$num_kindergarten[i] <- size_sum(bog_houses_6_nb[[i]])
  bog_houses_6$val_kindergarten[i] <- bog_houses_6_nb[[i]]==0
  
}

bog_houses_6 <- bog_houses_6 %>% mutate(kindergarten_final = ifelse(val_kindergarten == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_kindergarten)))) 

drop <- c("num_kindergarten", "val_kindergarten")
bog_houses_6 <- bog_houses_6[,!names(bog_houses_6)%in%drop]

bog_houses_6 <- bog_houses_6 %>% mutate(kindergarten = kindergarten_final - vecinos_final)


# library

bog_houses_6 <- bind_rows(bog_houses_6, osm_library_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_6_sp <- bog_houses_6%>%st_buffer(300)%>%as_Spatial()

bog_houses_6_nb <- poly2nb(pl=bog_houses_6_sp, queen= TRUE)

bog_houses_6$num_library <- NA
bog_houses_6$val_library <- NA

for (i in 1:length(bog_houses_6_nb)) {
  
  bog_houses_6$num_library[i] <- size_sum(bog_houses_6_nb[[i]])
  bog_houses_6$val_library[i] <- bog_houses_6_nb[[i]]==0
  
}

bog_houses_6 <- bog_houses_6 %>% mutate(library_final = ifelse(val_library == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_library)))) 

drop <- c("num_library", "val_library")
bog_houses_6 <- bog_houses_6[,!names(bog_houses_6)%in%drop]

bog_houses_6 <- bog_houses_6 %>% mutate(library = library_final - vecinos_final) 



# love_hotel

bog_houses_6 <- bind_rows(bog_houses_6, osm_love_hotel_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_6_sp <- bog_houses_6%>%st_buffer(300)%>%as_Spatial()

bog_houses_6_nb <- poly2nb(pl=bog_houses_6_sp, queen= TRUE)

bog_houses_6$num_love_hotel <- NA
bog_houses_6$val_love_hotel <- NA

for (i in 1:length(bog_houses_6_nb)) {
  
  bog_houses_6$num_love_hotel[i] <- size_sum(bog_houses_6_nb[[i]])
  bog_houses_6$val_love_hotel[i] <- bog_houses_6_nb[[i]]==0
  
}

bog_houses_6 <- bog_houses_6 %>% mutate(love_hotel_final = ifelse(val_love_hotel == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_love_hotel)))) 

drop <- c("num_love_hotel", "val_love_hotel")
bog_houses_6 <- bog_houses_6[,!names(bog_houses_6)%in%drop]

bog_houses_6 <- bog_houses_6 %>% mutate(love_hotel = love_hotel_final - vecinos_final) 


# marketplace

bog_houses_6 <- bind_rows(bog_houses_6, osm_marketplace_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_6_sp <- bog_houses_6%>%st_buffer(300)%>%as_Spatial()

bog_houses_6_nb <- poly2nb(pl=bog_houses_6_sp, queen= TRUE)

bog_houses_6$num_marketplace <- NA
bog_houses_6$val_marketplace <- NA

for (i in 1:length(bog_houses_6_nb)) {
  
  bog_houses_6$num_marketplace[i] <- size_sum(bog_houses_6_nb[[i]])
  bog_houses_6$val_marketplace[i] <- bog_houses_6_nb[[i]]==0
  
}

bog_houses_6 <- bog_houses_6 %>% mutate(marketplace_final = ifelse(val_marketplace == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_marketplace)))) 

drop <- c("num_marketplace", "val_marketplace")
bog_houses_6 <- bog_houses_6[,!names(bog_houses_6)%in%drop]

bog_houses_6 <- bog_houses_6 %>% mutate(marketplace =  marketplace_final - vecinos_final) 



# monastery

bog_houses_6 <- bind_rows(bog_houses_6, osm_monastery_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_6_sp <- bog_houses_6%>%st_buffer(300)%>%as_Spatial()

bog_houses_6_nb <- poly2nb(pl=bog_houses_6_sp, queen= TRUE)

bog_houses_6$num_monastery <- NA
bog_houses_6$val_monastery <- NA

for (i in 1:length(bog_houses_6_nb)) {
  
  bog_houses_6$num_monastery[i] <- size_sum(bog_houses_6_nb[[i]])
  bog_houses_6$val_monastery[i] <- bog_houses_6_nb[[i]]==0
  
}

bog_houses_6 <- bog_houses_6 %>% mutate(monastery_final = ifelse(val_monastery == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_monastery)))) 

drop <- c("num_monastery", "val_monastery")
bog_houses_6 <- bog_houses_6[,!names(bog_houses_6)%in%drop]

bog_houses_6 <- bog_houses_6 %>% mutate(monastery = monastery_final - vecinos_final) 


# parking

bog_houses_6 <- bind_rows(bog_houses_6, osm_parking_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_6_sp <- bog_houses_6%>%st_buffer(300)%>%as_Spatial()

bog_houses_6_nb <- poly2nb(pl=bog_houses_6_sp, queen= TRUE)

bog_houses_6$num_parking <- NA
bog_houses_6$val_parking <- NA

for (i in 1:length(bog_houses_6_nb)) {
  
  bog_houses_6$num_parking[i] <- size_sum(bog_houses_6_nb[[i]])
  bog_houses_6$val_parking[i] <- bog_houses_6_nb[[i]]==0
  
}

bog_houses_6 <- bog_houses_6 %>% mutate(parking_final = ifelse(val_parking == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_parking)))) 

drop <- c("num_parking", "val_parking")
bog_houses_6 <- bog_houses_6[,!names(bog_houses_6)%in%drop]

bog_houses_6 <- bog_houses_6 %>% mutate(parking = parking_final - vecinos_final) 


# police

bog_houses_6 <- bind_rows(bog_houses_6, osm_police_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_6_sp <- bog_houses_6%>%st_buffer(300)%>%as_Spatial()

bog_houses_6_nb <- poly2nb(pl=bog_houses_6_sp, queen= TRUE)

bog_houses_6$num_police <- NA
bog_houses_6$val_police <- NA

for (i in 1:length(bog_houses_6_nb)) {
  
  bog_houses_6$num_police[i] <- size_sum(bog_houses_6_nb[[i]])
  bog_houses_6$val_police[i] <- bog_houses_6_nb[[i]]==0
  
}

bog_houses_6 <- bog_houses_6 %>% mutate(police_final = ifelse(val_police == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_police)))) 

drop <- c("num_police", "val_police")
bog_houses_6 <- bog_houses_6[,!names(bog_houses_6)%in%drop]

bog_houses_6 <- bog_houses_6 %>% mutate(police = police_final - vecinos_final) 


# restaurant

bog_houses_6 <- bind_rows(bog_houses_6, osm_restaurant_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_6_sp <- bog_houses_6%>%st_buffer(300)%>%as_Spatial()

bog_houses_6_nb <- poly2nb(pl=bog_houses_6_sp, queen= TRUE)

bog_houses_6$num_restaurant <- NA
bog_houses_6$val_restaurant <- NA

for (i in 1:length(bog_houses_6_nb)) {
  
  bog_houses_6$num_restaurant[i] <- size_sum(bog_houses_6_nb[[i]])
  bog_houses_6$val_restaurant[i] <- bog_houses_6_nb[[i]]==0
  
}

bog_houses_6 <- bog_houses_6 %>% mutate(restaurant_final = ifelse(val_restaurant == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_restaurant)))) 

drop <- c("num_restaurant", "val_restaurant")
bog_houses_6 <- bog_houses_6[,!names(bog_houses_6)%in%drop]

bog_houses_6 <- bog_houses_6 %>% mutate(restaurant =  restaurant_final - vecinos_final) 

write_rds(bog_houses_6, "bog_houses_6_1.RDS")

bog_houses_6_2 <- bog_houses_6 %>% select(property_id, bank, bus_station, casino, childcare, cinema,
                                          clinic, college, community_centre, conference_centre, dentist, 
                                          doctors, events_venue, fast_food, hospital,
                                          kindergarten, library, love_hotel, marketplace, monastery,
                                          parking, police, restaurant)

write_rds(bog_houses_6_2, "bog_houses_6_2.RDS")

write_csv(bog_houses_6_2, "bog_houses_6_2.csv")



### Bogota 7

bog_houses_7_sp <- bog_houses_7%>%st_buffer(300)%>%as_Spatial()

bog_houses_7_nb <- poly2nb(pl=bog_houses_7_sp, queen= TRUE)

bog_houses_7$num_vecinos <- NA
bog_houses_7$val_vecinos <- NA

for (i in 1:length(bog_houses_7_nb)) {
  
  bog_houses_7$num_vecinos[i] <- size_sum(bog_houses_7_nb[[i]])
  bog_houses_7$val_vecinos[i] <- bog_houses_7_nb[[i]]==0
  
}

bog_houses_7 <- bog_houses_7 %>% mutate(vecinos_final = ifelse(val_vecinos == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_vecinos)))) 

drop <- c("num_vecinos", "val_vecinos")
bog_houses_7 <- bog_houses_7[,!names(bog_houses_7)%in%drop]

# Bancos

bog_houses_7 <- bind_rows(bog_houses_7, osm_bank_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_7_sp <- bog_houses_7%>%st_buffer(300)%>%as_Spatial()

bog_houses_7_nb <- poly2nb(pl=bog_houses_7_sp, queen= TRUE)

bog_houses_7$num_bank <- NA
bog_houses_7$val_bank <- NA

for (i in 1:length(bog_houses_7_nb)) {
  
  bog_houses_7$num_bank[i] <- size_sum(bog_houses_7_nb[[i]])
  bog_houses_7$val_bank[i] <- bog_houses_7_nb[[i]]==0
  
}

bog_houses_7 <- bog_houses_7 %>% mutate(bank_final = ifelse(val_bank == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_bank)))) 

drop <- c("num_bank", "val_bank")
bog_houses_7 <- bog_houses_7[,!names(bog_houses_7)%in%drop]

bog_houses_7 <- bog_houses_7 %>% mutate(bank = bank_final - vecinos_final) 

# Bus Station

bog_houses_7 <- bind_rows(bog_houses_7, osm_bus_station_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_7_sp <- bog_houses_7%>%st_buffer(300)%>%as_Spatial()

bog_houses_7_nb <- poly2nb(pl=bog_houses_7_sp, queen= TRUE)

bog_houses_7$num_bus_station <- NA
bog_houses_7$val_bus_station <- NA

for (i in 1:length(bog_houses_7_nb)) {
  
  bog_houses_7$num_bus_station[i] <- size_sum(bog_houses_7_nb[[i]])
  bog_houses_7$val_bus_station[i] <- bog_houses_7_nb[[i]]==0
  
}

bog_houses_7 <- bog_houses_7 %>% mutate(bus_station_final = ifelse(val_bus_station == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_bus_station)))) 

drop <- c("num_bus_station", "val_bus_station")
bog_houses_7 <- bog_houses_7[,!names(bog_houses_7)%in%drop]

bog_houses_7 <- bog_houses_7 %>% mutate(bus_station = bus_station_final - vecinos_final) 

# Casino

bog_houses_7 <- bind_rows(bog_houses_7, osm_casino_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_7_sp <- bog_houses_7%>%st_buffer(300)%>%as_Spatial()

bog_houses_7_nb <- poly2nb(pl=bog_houses_7_sp, queen= TRUE)

bog_houses_7$num_casino <- NA
bog_houses_7$val_casino <- NA

for (i in 1:length(bog_houses_7_nb)) {
  
  bog_houses_7$num_casino[i] <- size_sum(bog_houses_7_nb[[i]])
  bog_houses_7$val_casino[i] <- bog_houses_7_nb[[i]]==0
  
}

bog_houses_7 <- bog_houses_7 %>% mutate(casino_final = ifelse(val_casino == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_casino)))) 

drop <- c("num_casino", "val_casino")
bog_houses_7 <- bog_houses_7[,!names(bog_houses_7)%in%drop]

bog_houses_7 <- bog_houses_7 %>% mutate(casino = casino_final - vecinos_final) 


# Childcare

bog_houses_7 <- bind_rows(bog_houses_7, osm_childcare_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_7_sp <- bog_houses_7%>%st_buffer(300)%>%as_Spatial()

bog_houses_7_nb <- poly2nb(pl=bog_houses_7_sp, queen= TRUE)

bog_houses_7$num_childcare <- NA
bog_houses_7$val_childcare <- NA

for (i in 1:length(bog_houses_7_nb)) {
  
  bog_houses_7$num_childcare[i] <- size_sum(bog_houses_7_nb[[i]])
  bog_houses_7$val_childcare[i] <- bog_houses_7_nb[[i]]==0
  
}

bog_houses_7 <- bog_houses_7 %>% mutate(childcare_final = ifelse(val_childcare == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_childcare)))) 

drop <- c("num_childcare", "val_childcare")
bog_houses_7 <- bog_houses_7[,!names(bog_houses_7)%in%drop]

bog_houses_7 <- bog_houses_7 %>% mutate(childcare = childcare_final - vecinos_final) 


# Cinema

bog_houses_7 <- bind_rows(bog_houses_7, osm_cinema_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_7_sp <- bog_houses_7%>%st_buffer(300)%>%as_Spatial()

bog_houses_7_nb <- poly2nb(pl=bog_houses_7_sp, queen= TRUE)

bog_houses_7$num_cinema <- NA
bog_houses_7$val_cinema <- NA

for (i in 1:length(bog_houses_7_nb)) {
  
  bog_houses_7$num_cinema[i] <- size_sum(bog_houses_7_nb[[i]])
  bog_houses_7$val_cinema[i] <- bog_houses_7_nb[[i]]==0
  
}

bog_houses_7 <- bog_houses_7 %>% mutate(cinema_final = ifelse(val_cinema == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_cinema)))) 

drop <- c("num_cinema", "val_cinema")
bog_houses_7 <- bog_houses_7[,!names(bog_houses_7)%in%drop]

bog_houses_7 <- bog_houses_7 %>% mutate(cinema = cinema_final - vecinos_final) 


# clinic

bog_houses_7 <- bind_rows(bog_houses_7, osm_clinic_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_7_sp <- bog_houses_7%>%st_buffer(300)%>%as_Spatial()

bog_houses_7_nb <- poly2nb(pl=bog_houses_7_sp, queen= TRUE)

bog_houses_7$num_clinic <- NA
bog_houses_7$val_clinic <- NA

for (i in 1:length(bog_houses_7_nb)) {
  
  bog_houses_7$num_clinic[i] <- size_sum(bog_houses_7_nb[[i]])
  bog_houses_7$val_clinic[i] <- bog_houses_7_nb[[i]]==0
  
}

bog_houses_7 <- bog_houses_7 %>% mutate(clinic_final = ifelse(val_clinic == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_clinic)))) 

drop <- c("num_clinic", "val_clinic")
bog_houses_7 <- bog_houses_7[,!names(bog_houses_7)%in%drop]

bog_houses_7 <- bog_houses_7 %>% mutate(clinic = clinic_final - vecinos_final) 


# college

bog_houses_7 <- bind_rows(bog_houses_7, osm_college_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_7_sp <- bog_houses_7%>%st_buffer(300)%>%as_Spatial()

bog_houses_7_nb <- poly2nb(pl=bog_houses_7_sp, queen= TRUE)

bog_houses_7$num_college <- NA
bog_houses_7$val_college <- NA

for (i in 1:length(bog_houses_7_nb)) {
  
  bog_houses_7$num_college[i] <- size_sum(bog_houses_7_nb[[i]])
  bog_houses_7$val_college[i] <- bog_houses_7_nb[[i]]==0
  
}

bog_houses_7 <- bog_houses_7 %>% mutate(college_final = ifelse(val_college == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_college)))) 

drop <- c("num_college", "val_college")
bog_houses_7 <- bog_houses_7[,!names(bog_houses_7)%in%drop]

bog_houses_7 <- bog_houses_7 %>% mutate(college = college_final - vecinos_final) 


# community_centre

bog_houses_7 <- bind_rows(bog_houses_7, osm_community_centre_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_7_sp <- bog_houses_7%>%st_buffer(300)%>%as_Spatial()

bog_houses_7_nb <- poly2nb(pl=bog_houses_7_sp, queen= TRUE)

bog_houses_7$num_community_centre <- NA
bog_houses_7$val_community_centre <- NA

for (i in 1:length(bog_houses_7_nb)) {
  
  bog_houses_7$num_community_centre[i] <- size_sum(bog_houses_7_nb[[i]])
  bog_houses_7$val_community_centre[i] <- bog_houses_7_nb[[i]]==0
  
}

bog_houses_7 <- bog_houses_7 %>% mutate(community_centre_final = ifelse(val_community_centre == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_community_centre)))) 

drop <- c("num_community_centre", "val_community_centre")
bog_houses_7 <- bog_houses_7[,!names(bog_houses_7)%in%drop]

bog_houses_7 <- bog_houses_7 %>% mutate(community_centre = community_centre_final - vecinos_final) 



# conference_centre

bog_houses_7 <- bind_rows(bog_houses_7, osm_conference_centre_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_7_sp <- bog_houses_7%>%st_buffer(300)%>%as_Spatial()

bog_houses_7_nb <- poly2nb(pl=bog_houses_7_sp, queen= TRUE)

bog_houses_7$num_conference_centre <- NA
bog_houses_7$val_conference_centre <- NA

for (i in 1:length(bog_houses_7_nb)) {
  
  bog_houses_7$num_conference_centre[i] <- size_sum(bog_houses_7_nb[[i]])
  bog_houses_7$val_conference_centre[i] <- bog_houses_7_nb[[i]]==0
  
}

bog_houses_7 <- bog_houses_7 %>% mutate(conference_centre_final = ifelse(val_conference_centre == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_conference_centre)))) 

drop <- c("num_conference_centre", "val_conference_centre")
bog_houses_7 <- bog_houses_7[,!names(bog_houses_7)%in%drop]

bog_houses_7 <- bog_houses_7 %>% mutate(conference_centre = conference_centre_final - vecinos_final) 



# dentist

bog_houses_7 <- bind_rows(bog_houses_7, osm_dentist_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_7_sp <- bog_houses_7%>%st_buffer(300)%>%as_Spatial()

bog_houses_7_nb <- poly2nb(pl=bog_houses_7_sp, queen= TRUE)

bog_houses_7$num_dentist <- NA
bog_houses_7$val_dentist <- NA

for (i in 1:length(bog_houses_7_nb)) {
  
  bog_houses_7$num_dentist[i] <- size_sum(bog_houses_7_nb[[i]])
  bog_houses_7$val_dentist[i] <- bog_houses_7_nb[[i]]==0
  
}

bog_houses_7 <- bog_houses_7 %>% mutate(dentist_final = ifelse(val_dentist == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_dentist)))) 

drop <- c("num_dentist", "val_dentist")
bog_houses_7 <- bog_houses_7[,!names(bog_houses_7)%in%drop]

bog_houses_7 <- bog_houses_7 %>% mutate(dentist = dentist_final - vecinos_final) 


# doctors

bog_houses_7 <- bind_rows(bog_houses_7, osm_doctors_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_7_sp <- bog_houses_7%>%st_buffer(300)%>%as_Spatial()

bog_houses_7_nb <- poly2nb(pl=bog_houses_7_sp, queen= TRUE)

bog_houses_7$num_doctors <- NA
bog_houses_7$val_doctors <- NA

for (i in 1:length(bog_houses_7_nb)) {
  
  bog_houses_7$num_doctors[i] <- size_sum(bog_houses_7_nb[[i]])
  bog_houses_7$val_doctors[i] <- bog_houses_7_nb[[i]]==0
  
}

bog_houses_7 <- bog_houses_7 %>% mutate(doctors_final = ifelse(val_doctors == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_doctors)))) 

drop <- c("num_doctors", "val_doctors")
bog_houses_7 <- bog_houses_7[,!names(bog_houses_7)%in%drop]

bog_houses_7 <- bog_houses_7 %>% mutate(doctors = doctors_final - vecinos_final) 


# events_venue

bog_houses_7 <- bind_rows(bog_houses_7, osm_events_venue_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_7_sp <- bog_houses_7%>%st_buffer(300)%>%as_Spatial()

bog_houses_7_nb <- poly2nb(pl=bog_houses_7_sp, queen= TRUE)

bog_houses_7$num_events_venue <- NA
bog_houses_7$val_events_venue <- NA

for (i in 1:length(bog_houses_7_nb)) {
  
  bog_houses_7$num_events_venue[i] <- size_sum(bog_houses_7_nb[[i]])
  bog_houses_7$val_events_venue[i] <- bog_houses_7_nb[[i]]==0
  
}

bog_houses_7 <- bog_houses_7 %>% mutate(events_venue_final = ifelse(val_events_venue == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_events_venue)))) 

drop <- c("num_events_venue", "val_events_venue")
bog_houses_7 <- bog_houses_7[,!names(bog_houses_7)%in%drop]

bog_houses_7 <- bog_houses_7 %>% mutate(events_venue = events_venue_final - vecinos_final) 


# fast_food

bog_houses_7 <- bind_rows(bog_houses_7, osm_fast_food_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_7_sp <- bog_houses_7%>%st_buffer(300)%>%as_Spatial()

bog_houses_7_nb <- poly2nb(pl=bog_houses_7_sp, queen= TRUE)

bog_houses_7$num_fast_food <- NA
bog_houses_7$val_fast_food <- NA

for (i in 1:length(bog_houses_7_nb)) {
  
  bog_houses_7$num_fast_food[i] <- size_sum(bog_houses_7_nb[[i]])
  bog_houses_7$val_fast_food[i] <- bog_houses_7_nb[[i]]==0
  
}

bog_houses_7 <- bog_houses_7 %>% mutate(fast_food_final = ifelse(val_fast_food == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_fast_food)))) 

drop <- c("num_fast_food", "val_fast_food")
bog_houses_7 <- bog_houses_7[,!names(bog_houses_7)%in%drop]

bog_houses_7 <- bog_houses_7 %>% mutate(fast_food = fast_food_final - vecinos_final) 


# hospital

bog_houses_7 <- bind_rows(bog_houses_7, osm_hospital_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_7_sp <- bog_houses_7%>%st_buffer(300)%>%as_Spatial()

bog_houses_7_nb <- poly2nb(pl=bog_houses_7_sp, queen= TRUE)

bog_houses_7$num_hospital <- NA
bog_houses_7$val_hospital <- NA

for (i in 1:length(bog_houses_7_nb)) {
  
  bog_houses_7$num_hospital[i] <- size_sum(bog_houses_7_nb[[i]])
  bog_houses_7$val_hospital[i] <- bog_houses_7_nb[[i]]==0
  
}

bog_houses_7 <- bog_houses_7 %>% mutate(hospital_final = ifelse(val_hospital == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_hospital)))) 

drop <- c("num_hospital", "val_hospital")
bog_houses_7 <- bog_houses_7[,!names(bog_houses_7)%in%drop]

bog_houses_7 <- bog_houses_7 %>% mutate(hospital = hospital_final - vecinos_final) 


# kindergarten

bog_houses_7 <- bind_rows(bog_houses_7, osm_kindergarten_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_7_sp <- bog_houses_7%>%st_buffer(300)%>%as_Spatial()

bog_houses_7_nb <- poly2nb(pl=bog_houses_7_sp, queen= TRUE)

bog_houses_7$num_kindergarten <- NA
bog_houses_7$val_kindergarten <- NA

for (i in 1:length(bog_houses_7_nb)) {
  
  bog_houses_7$num_kindergarten[i] <- size_sum(bog_houses_7_nb[[i]])
  bog_houses_7$val_kindergarten[i] <- bog_houses_7_nb[[i]]==0
  
}

bog_houses_7 <- bog_houses_7 %>% mutate(kindergarten_final = ifelse(val_kindergarten == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_kindergarten)))) 

drop <- c("num_kindergarten", "val_kindergarten")
bog_houses_7 <- bog_houses_7[,!names(bog_houses_7)%in%drop]

bog_houses_7 <- bog_houses_7 %>% mutate(kindergarten = kindergarten_final - vecinos_final)


# library

bog_houses_7 <- bind_rows(bog_houses_7, osm_library_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_7_sp <- bog_houses_7%>%st_buffer(300)%>%as_Spatial()

bog_houses_7_nb <- poly2nb(pl=bog_houses_7_sp, queen= TRUE)

bog_houses_7$num_library <- NA
bog_houses_7$val_library <- NA

for (i in 1:length(bog_houses_7_nb)) {
  
  bog_houses_7$num_library[i] <- size_sum(bog_houses_7_nb[[i]])
  bog_houses_7$val_library[i] <- bog_houses_7_nb[[i]]==0
  
}

bog_houses_7 <- bog_houses_7 %>% mutate(library_final = ifelse(val_library == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_library)))) 

drop <- c("num_library", "val_library")
bog_houses_7 <- bog_houses_7[,!names(bog_houses_7)%in%drop]

bog_houses_7 <- bog_houses_7 %>% mutate(library = library_final - vecinos_final) 



# love_hotel

bog_houses_7 <- bind_rows(bog_houses_7, osm_love_hotel_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_7_sp <- bog_houses_7%>%st_buffer(300)%>%as_Spatial()

bog_houses_7_nb <- poly2nb(pl=bog_houses_7_sp, queen= TRUE)

bog_houses_7$num_love_hotel <- NA
bog_houses_7$val_love_hotel <- NA

for (i in 1:length(bog_houses_7_nb)) {
  
  bog_houses_7$num_love_hotel[i] <- size_sum(bog_houses_7_nb[[i]])
  bog_houses_7$val_love_hotel[i] <- bog_houses_7_nb[[i]]==0
  
}

bog_houses_7 <- bog_houses_7 %>% mutate(love_hotel_final = ifelse(val_love_hotel == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_love_hotel)))) 

drop <- c("num_love_hotel", "val_love_hotel")
bog_houses_7 <- bog_houses_7[,!names(bog_houses_7)%in%drop]

bog_houses_7 <- bog_houses_7 %>% mutate(love_hotel = love_hotel_final - vecinos_final) 


# marketplace

bog_houses_7 <- bind_rows(bog_houses_7, osm_marketplace_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_7_sp <- bog_houses_7%>%st_buffer(300)%>%as_Spatial()

bog_houses_7_nb <- poly2nb(pl=bog_houses_7_sp, queen= TRUE)

bog_houses_7$num_marketplace <- NA
bog_houses_7$val_marketplace <- NA

for (i in 1:length(bog_houses_7_nb)) {
  
  bog_houses_7$num_marketplace[i] <- size_sum(bog_houses_7_nb[[i]])
  bog_houses_7$val_marketplace[i] <- bog_houses_7_nb[[i]]==0
  
}

bog_houses_7 <- bog_houses_7 %>% mutate(marketplace_final = ifelse(val_marketplace == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_marketplace)))) 

drop <- c("num_marketplace", "val_marketplace")
bog_houses_7 <- bog_houses_7[,!names(bog_houses_7)%in%drop]

bog_houses_7 <- bog_houses_7 %>% mutate(marketplace =  marketplace_final - vecinos_final) 



# monastery

bog_houses_7 <- bind_rows(bog_houses_7, osm_monastery_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_7_sp <- bog_houses_7%>%st_buffer(300)%>%as_Spatial()

bog_houses_7_nb <- poly2nb(pl=bog_houses_7_sp, queen= TRUE)

bog_houses_7$num_monastery <- NA
bog_houses_7$val_monastery <- NA

for (i in 1:length(bog_houses_7_nb)) {
  
  bog_houses_7$num_monastery[i] <- size_sum(bog_houses_7_nb[[i]])
  bog_houses_7$val_monastery[i] <- bog_houses_7_nb[[i]]==0
  
}

bog_houses_7 <- bog_houses_7 %>% mutate(monastery_final = ifelse(val_monastery == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_monastery)))) 

drop <- c("num_monastery", "val_monastery")
bog_houses_7 <- bog_houses_7[,!names(bog_houses_7)%in%drop]

bog_houses_7 <- bog_houses_7 %>% mutate(monastery = monastery_final - vecinos_final) 


# parking

bog_houses_7 <- bind_rows(bog_houses_7, osm_parking_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_7_sp <- bog_houses_7%>%st_buffer(300)%>%as_Spatial()

bog_houses_7_nb <- poly2nb(pl=bog_houses_7_sp, queen= TRUE)

bog_houses_7$num_parking <- NA
bog_houses_7$val_parking <- NA

for (i in 1:length(bog_houses_7_nb)) {
  
  bog_houses_7$num_parking[i] <- size_sum(bog_houses_7_nb[[i]])
  bog_houses_7$val_parking[i] <- bog_houses_7_nb[[i]]==0
  
}

bog_houses_7 <- bog_houses_7 %>% mutate(parking_final = ifelse(val_parking == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_parking)))) 

drop <- c("num_parking", "val_parking")
bog_houses_7 <- bog_houses_7[,!names(bog_houses_7)%in%drop]

bog_houses_7 <- bog_houses_7 %>% mutate(parking = parking_final - vecinos_final) 


# police

bog_houses_7 <- bind_rows(bog_houses_7, osm_police_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_7_sp <- bog_houses_7%>%st_buffer(300)%>%as_Spatial()

bog_houses_7_nb <- poly2nb(pl=bog_houses_7_sp, queen= TRUE)

bog_houses_7$num_police <- NA
bog_houses_7$val_police <- NA

for (i in 1:length(bog_houses_7_nb)) {
  
  bog_houses_7$num_police[i] <- size_sum(bog_houses_7_nb[[i]])
  bog_houses_7$val_police[i] <- bog_houses_7_nb[[i]]==0
  
}

bog_houses_7 <- bog_houses_7 %>% mutate(police_final = ifelse(val_police == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_police)))) 

drop <- c("num_police", "val_police")
bog_houses_7 <- bog_houses_7[,!names(bog_houses_7)%in%drop]

bog_houses_7 <- bog_houses_7 %>% mutate(police = police_final - vecinos_final) 


# restaurant

bog_houses_7 <- bind_rows(bog_houses_7, osm_restaurant_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_7_sp <- bog_houses_7%>%st_buffer(300)%>%as_Spatial()

bog_houses_7_nb <- poly2nb(pl=bog_houses_7_sp, queen= TRUE)

bog_houses_7$num_restaurant <- NA
bog_houses_7$val_restaurant <- NA

for (i in 1:length(bog_houses_7_nb)) {
  
  bog_houses_7$num_restaurant[i] <- size_sum(bog_houses_7_nb[[i]])
  bog_houses_7$val_restaurant[i] <- bog_houses_7_nb[[i]]==0
  
}

bog_houses_7 <- bog_houses_7 %>% mutate(restaurant_final = ifelse(val_restaurant == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_restaurant)))) 

drop <- c("num_restaurant", "val_restaurant")
bog_houses_7 <- bog_houses_7[,!names(bog_houses_7)%in%drop]

bog_houses_7 <- bog_houses_7 %>% mutate(restaurant =  restaurant_final - vecinos_final) 

write_rds(bog_houses_7, "bog_houses_7_1.RDS")

bog_houses_7_2 <- bog_houses_7 %>% select(property_id, bank, bus_station, casino, childcare, cinema,
                                          clinic, college, community_centre, conference_centre, dentist, 
                                          doctors, events_venue, fast_food, hospital,
                                          kindergarten, library, love_hotel, marketplace, monastery,
                                          parking, police, restaurant)

write_rds(bog_houses_7_2, "bog_houses_7_2.RDS")

write_csv(bog_houses_7_2, "bog_houses_7_2.csv")



### Bogota 8

bog_houses_8_sp <- bog_houses_8%>%st_buffer(300)%>%as_Spatial()

bog_houses_8_nb <- poly2nb(pl=bog_houses_8_sp, queen= TRUE)

bog_houses_8$num_vecinos <- NA
bog_houses_8$val_vecinos <- NA

for (i in 1:length(bog_houses_8_nb)) {
  
  bog_houses_8$num_vecinos[i] <- size_sum(bog_houses_8_nb[[i]])
  bog_houses_8$val_vecinos[i] <- bog_houses_8_nb[[i]]==0
  
}

bog_houses_8 <- bog_houses_8 %>% mutate(vecinos_final = ifelse(val_vecinos == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_vecinos)))) 

drop <- c("num_vecinos", "val_vecinos")
bog_houses_8 <- bog_houses_8[,!names(bog_houses_8)%in%drop]

# Bancos

bog_houses_8 <- bind_rows(bog_houses_8, osm_bank_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_8_sp <- bog_houses_8%>%st_buffer(300)%>%as_Spatial()

bog_houses_8_nb <- poly2nb(pl=bog_houses_8_sp, queen= TRUE)

bog_houses_8$num_bank <- NA
bog_houses_8$val_bank <- NA

for (i in 1:length(bog_houses_8_nb)) {
  
  bog_houses_8$num_bank[i] <- size_sum(bog_houses_8_nb[[i]])
  bog_houses_8$val_bank[i] <- bog_houses_8_nb[[i]]==0
  
}

bog_houses_8 <- bog_houses_8 %>% mutate(bank_final = ifelse(val_bank == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_bank)))) 

drop <- c("num_bank", "val_bank")
bog_houses_8 <- bog_houses_8[,!names(bog_houses_8)%in%drop]

bog_houses_8 <- bog_houses_8 %>% mutate(bank = bank_final - vecinos_final) 

# Bus Station

bog_houses_8 <- bind_rows(bog_houses_8, osm_bus_station_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_8_sp <- bog_houses_8%>%st_buffer(300)%>%as_Spatial()

bog_houses_8_nb <- poly2nb(pl=bog_houses_8_sp, queen= TRUE)

bog_houses_8$num_bus_station <- NA
bog_houses_8$val_bus_station <- NA

for (i in 1:length(bog_houses_8_nb)) {
  
  bog_houses_8$num_bus_station[i] <- size_sum(bog_houses_8_nb[[i]])
  bog_houses_8$val_bus_station[i] <- bog_houses_8_nb[[i]]==0
  
}

bog_houses_8 <- bog_houses_8 %>% mutate(bus_station_final = ifelse(val_bus_station == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_bus_station)))) 

drop <- c("num_bus_station", "val_bus_station")
bog_houses_8 <- bog_houses_8[,!names(bog_houses_8)%in%drop]

bog_houses_8 <- bog_houses_8 %>% mutate(bus_station = bus_station_final - vecinos_final) 

# Casino

bog_houses_8 <- bind_rows(bog_houses_8, osm_casino_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_8_sp <- bog_houses_8%>%st_buffer(300)%>%as_Spatial()

bog_houses_8_nb <- poly2nb(pl=bog_houses_8_sp, queen= TRUE)

bog_houses_8$num_casino <- NA
bog_houses_8$val_casino <- NA

for (i in 1:length(bog_houses_8_nb)) {
  
  bog_houses_8$num_casino[i] <- size_sum(bog_houses_8_nb[[i]])
  bog_houses_8$val_casino[i] <- bog_houses_8_nb[[i]]==0
  
}

bog_houses_8 <- bog_houses_8 %>% mutate(casino_final = ifelse(val_casino == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_casino)))) 

drop <- c("num_casino", "val_casino")
bog_houses_8 <- bog_houses_8[,!names(bog_houses_8)%in%drop]

bog_houses_8 <- bog_houses_8 %>% mutate(casino = casino_final - vecinos_final) 


# Childcare

bog_houses_8 <- bind_rows(bog_houses_8, osm_childcare_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_8_sp <- bog_houses_8%>%st_buffer(300)%>%as_Spatial()

bog_houses_8_nb <- poly2nb(pl=bog_houses_8_sp, queen= TRUE)

bog_houses_8$num_childcare <- NA
bog_houses_8$val_childcare <- NA

for (i in 1:length(bog_houses_8_nb)) {
  
  bog_houses_8$num_childcare[i] <- size_sum(bog_houses_8_nb[[i]])
  bog_houses_8$val_childcare[i] <- bog_houses_8_nb[[i]]==0
  
}

bog_houses_8 <- bog_houses_8 %>% mutate(childcare_final = ifelse(val_childcare == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_childcare)))) 

drop <- c("num_childcare", "val_childcare")
bog_houses_8 <- bog_houses_8[,!names(bog_houses_8)%in%drop]

bog_houses_8 <- bog_houses_8 %>% mutate(childcare = childcare_final - vecinos_final) 


# Cinema

bog_houses_8 <- bind_rows(bog_houses_8, osm_cinema_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_8_sp <- bog_houses_8%>%st_buffer(300)%>%as_Spatial()

bog_houses_8_nb <- poly2nb(pl=bog_houses_8_sp, queen= TRUE)

bog_houses_8$num_cinema <- NA
bog_houses_8$val_cinema <- NA

for (i in 1:length(bog_houses_8_nb)) {
  
  bog_houses_8$num_cinema[i] <- size_sum(bog_houses_8_nb[[i]])
  bog_houses_8$val_cinema[i] <- bog_houses_8_nb[[i]]==0
  
}

bog_houses_8 <- bog_houses_8 %>% mutate(cinema_final = ifelse(val_cinema == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_cinema)))) 

drop <- c("num_cinema", "val_cinema")
bog_houses_8 <- bog_houses_8[,!names(bog_houses_8)%in%drop]

bog_houses_8 <- bog_houses_8 %>% mutate(cinema = cinema_final - vecinos_final) 


# clinic

bog_houses_8 <- bind_rows(bog_houses_8, osm_clinic_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_8_sp <- bog_houses_8%>%st_buffer(300)%>%as_Spatial()

bog_houses_8_nb <- poly2nb(pl=bog_houses_8_sp, queen= TRUE)

bog_houses_8$num_clinic <- NA
bog_houses_8$val_clinic <- NA

for (i in 1:length(bog_houses_8_nb)) {
  
  bog_houses_8$num_clinic[i] <- size_sum(bog_houses_8_nb[[i]])
  bog_houses_8$val_clinic[i] <- bog_houses_8_nb[[i]]==0
  
}

bog_houses_8 <- bog_houses_8 %>% mutate(clinic_final = ifelse(val_clinic == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_clinic)))) 

drop <- c("num_clinic", "val_clinic")
bog_houses_8 <- bog_houses_8[,!names(bog_houses_8)%in%drop]

bog_houses_8 <- bog_houses_8 %>% mutate(clinic = clinic_final - vecinos_final) 


# college

bog_houses_8 <- bind_rows(bog_houses_8, osm_college_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_8_sp <- bog_houses_8%>%st_buffer(300)%>%as_Spatial()

bog_houses_8_nb <- poly2nb(pl=bog_houses_8_sp, queen= TRUE)

bog_houses_8$num_college <- NA
bog_houses_8$val_college <- NA

for (i in 1:length(bog_houses_8_nb)) {
  
  bog_houses_8$num_college[i] <- size_sum(bog_houses_8_nb[[i]])
  bog_houses_8$val_college[i] <- bog_houses_8_nb[[i]]==0
  
}

bog_houses_8 <- bog_houses_8 %>% mutate(college_final = ifelse(val_college == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_college)))) 

drop <- c("num_college", "val_college")
bog_houses_8 <- bog_houses_8[,!names(bog_houses_8)%in%drop]

bog_houses_8 <- bog_houses_8 %>% mutate(college = college_final - vecinos_final) 


# community_centre

bog_houses_8 <- bind_rows(bog_houses_8, osm_community_centre_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_8_sp <- bog_houses_8%>%st_buffer(300)%>%as_Spatial()

bog_houses_8_nb <- poly2nb(pl=bog_houses_8_sp, queen= TRUE)

bog_houses_8$num_community_centre <- NA
bog_houses_8$val_community_centre <- NA

for (i in 1:length(bog_houses_8_nb)) {
  
  bog_houses_8$num_community_centre[i] <- size_sum(bog_houses_8_nb[[i]])
  bog_houses_8$val_community_centre[i] <- bog_houses_8_nb[[i]]==0
  
}

bog_houses_8 <- bog_houses_8 %>% mutate(community_centre_final = ifelse(val_community_centre == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_community_centre)))) 

drop <- c("num_community_centre", "val_community_centre")
bog_houses_8 <- bog_houses_8[,!names(bog_houses_8)%in%drop]

bog_houses_8 <- bog_houses_8 %>% mutate(community_centre = community_centre_final - vecinos_final) 



# conference_centre

bog_houses_8 <- bind_rows(bog_houses_8, osm_conference_centre_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_8_sp <- bog_houses_8%>%st_buffer(300)%>%as_Spatial()

bog_houses_8_nb <- poly2nb(pl=bog_houses_8_sp, queen= TRUE)

bog_houses_8$num_conference_centre <- NA
bog_houses_8$val_conference_centre <- NA

for (i in 1:length(bog_houses_8_nb)) {
  
  bog_houses_8$num_conference_centre[i] <- size_sum(bog_houses_8_nb[[i]])
  bog_houses_8$val_conference_centre[i] <- bog_houses_8_nb[[i]]==0
  
}

bog_houses_8 <- bog_houses_8 %>% mutate(conference_centre_final = ifelse(val_conference_centre == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_conference_centre)))) 

drop <- c("num_conference_centre", "val_conference_centre")
bog_houses_8 <- bog_houses_8[,!names(bog_houses_8)%in%drop]

bog_houses_8 <- bog_houses_8 %>% mutate(conference_centre = conference_centre_final - vecinos_final) 



# dentist

bog_houses_8 <- bind_rows(bog_houses_8, osm_dentist_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_8_sp <- bog_houses_8%>%st_buffer(300)%>%as_Spatial()

bog_houses_8_nb <- poly2nb(pl=bog_houses_8_sp, queen= TRUE)

bog_houses_8$num_dentist <- NA
bog_houses_8$val_dentist <- NA

for (i in 1:length(bog_houses_8_nb)) {
  
  bog_houses_8$num_dentist[i] <- size_sum(bog_houses_8_nb[[i]])
  bog_houses_8$val_dentist[i] <- bog_houses_8_nb[[i]]==0
  
}

bog_houses_8 <- bog_houses_8 %>% mutate(dentist_final = ifelse(val_dentist == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_dentist)))) 

drop <- c("num_dentist", "val_dentist")
bog_houses_8 <- bog_houses_8[,!names(bog_houses_8)%in%drop]

bog_houses_8 <- bog_houses_8 %>% mutate(dentist = dentist_final - vecinos_final) 


# doctors

bog_houses_8 <- bind_rows(bog_houses_8, osm_doctors_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_8_sp <- bog_houses_8%>%st_buffer(300)%>%as_Spatial()

bog_houses_8_nb <- poly2nb(pl=bog_houses_8_sp, queen= TRUE)

bog_houses_8$num_doctors <- NA
bog_houses_8$val_doctors <- NA

for (i in 1:length(bog_houses_8_nb)) {
  
  bog_houses_8$num_doctors[i] <- size_sum(bog_houses_8_nb[[i]])
  bog_houses_8$val_doctors[i] <- bog_houses_8_nb[[i]]==0
  
}

bog_houses_8 <- bog_houses_8 %>% mutate(doctors_final = ifelse(val_doctors == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_doctors)))) 

drop <- c("num_doctors", "val_doctors")
bog_houses_8 <- bog_houses_8[,!names(bog_houses_8)%in%drop]

bog_houses_8 <- bog_houses_8 %>% mutate(doctors = doctors_final - vecinos_final) 


# events_venue

bog_houses_8 <- bind_rows(bog_houses_8, osm_events_venue_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_8_sp <- bog_houses_8%>%st_buffer(300)%>%as_Spatial()

bog_houses_8_nb <- poly2nb(pl=bog_houses_8_sp, queen= TRUE)

bog_houses_8$num_events_venue <- NA
bog_houses_8$val_events_venue <- NA

for (i in 1:length(bog_houses_8_nb)) {
  
  bog_houses_8$num_events_venue[i] <- size_sum(bog_houses_8_nb[[i]])
  bog_houses_8$val_events_venue[i] <- bog_houses_8_nb[[i]]==0
  
}

bog_houses_8 <- bog_houses_8 %>% mutate(events_venue_final = ifelse(val_events_venue == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_events_venue)))) 

drop <- c("num_events_venue", "val_events_venue")
bog_houses_8 <- bog_houses_8[,!names(bog_houses_8)%in%drop]

bog_houses_8 <- bog_houses_8 %>% mutate(events_venue = events_venue_final - vecinos_final) 


# fast_food

bog_houses_8 <- bind_rows(bog_houses_8, osm_fast_food_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_8_sp <- bog_houses_8%>%st_buffer(300)%>%as_Spatial()

bog_houses_8_nb <- poly2nb(pl=bog_houses_8_sp, queen= TRUE)

bog_houses_8$num_fast_food <- NA
bog_houses_8$val_fast_food <- NA

for (i in 1:length(bog_houses_8_nb)) {
  
  bog_houses_8$num_fast_food[i] <- size_sum(bog_houses_8_nb[[i]])
  bog_houses_8$val_fast_food[i] <- bog_houses_8_nb[[i]]==0
  
}

bog_houses_8 <- bog_houses_8 %>% mutate(fast_food_final = ifelse(val_fast_food == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_fast_food)))) 

drop <- c("num_fast_food", "val_fast_food")
bog_houses_8 <- bog_houses_8[,!names(bog_houses_8)%in%drop]

bog_houses_8 <- bog_houses_8 %>% mutate(fast_food = fast_food_final - vecinos_final) 


# hospital

bog_houses_8 <- bind_rows(bog_houses_8, osm_hospital_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_8_sp <- bog_houses_8%>%st_buffer(300)%>%as_Spatial()

bog_houses_8_nb <- poly2nb(pl=bog_houses_8_sp, queen= TRUE)

bog_houses_8$num_hospital <- NA
bog_houses_8$val_hospital <- NA

for (i in 1:length(bog_houses_8_nb)) {
  
  bog_houses_8$num_hospital[i] <- size_sum(bog_houses_8_nb[[i]])
  bog_houses_8$val_hospital[i] <- bog_houses_8_nb[[i]]==0
  
}

bog_houses_8 <- bog_houses_8 %>% mutate(hospital_final = ifelse(val_hospital == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_hospital)))) 

drop <- c("num_hospital", "val_hospital")
bog_houses_8 <- bog_houses_8[,!names(bog_houses_8)%in%drop]

bog_houses_8 <- bog_houses_8 %>% mutate(hospital = hospital_final - vecinos_final) 


# kindergarten

bog_houses_8 <- bind_rows(bog_houses_8, osm_kindergarten_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_8_sp <- bog_houses_8%>%st_buffer(300)%>%as_Spatial()

bog_houses_8_nb <- poly2nb(pl=bog_houses_8_sp, queen= TRUE)

bog_houses_8$num_kindergarten <- NA
bog_houses_8$val_kindergarten <- NA

for (i in 1:length(bog_houses_8_nb)) {
  
  bog_houses_8$num_kindergarten[i] <- size_sum(bog_houses_8_nb[[i]])
  bog_houses_8$val_kindergarten[i] <- bog_houses_8_nb[[i]]==0
  
}

bog_houses_8 <- bog_houses_8 %>% mutate(kindergarten_final = ifelse(val_kindergarten == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_kindergarten)))) 

drop <- c("num_kindergarten", "val_kindergarten")
bog_houses_8 <- bog_houses_8[,!names(bog_houses_8)%in%drop]

bog_houses_8 <- bog_houses_8 %>% mutate(kindergarten = kindergarten_final - vecinos_final)


# library

bog_houses_8 <- bind_rows(bog_houses_8, osm_library_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_8_sp <- bog_houses_8%>%st_buffer(300)%>%as_Spatial()

bog_houses_8_nb <- poly2nb(pl=bog_houses_8_sp, queen= TRUE)

bog_houses_8$num_library <- NA
bog_houses_8$val_library <- NA

for (i in 1:length(bog_houses_8_nb)) {
  
  bog_houses_8$num_library[i] <- size_sum(bog_houses_8_nb[[i]])
  bog_houses_8$val_library[i] <- bog_houses_8_nb[[i]]==0
  
}

bog_houses_8 <- bog_houses_8 %>% mutate(library_final = ifelse(val_library == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_library)))) 

drop <- c("num_library", "val_library")
bog_houses_8 <- bog_houses_8[,!names(bog_houses_8)%in%drop]

bog_houses_8 <- bog_houses_8 %>% mutate(library = library_final - vecinos_final) 



# love_hotel

bog_houses_8 <- bind_rows(bog_houses_8, osm_love_hotel_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_8_sp <- bog_houses_8%>%st_buffer(300)%>%as_Spatial()

bog_houses_8_nb <- poly2nb(pl=bog_houses_8_sp, queen= TRUE)

bog_houses_8$num_love_hotel <- NA
bog_houses_8$val_love_hotel <- NA

for (i in 1:length(bog_houses_8_nb)) {
  
  bog_houses_8$num_love_hotel[i] <- size_sum(bog_houses_8_nb[[i]])
  bog_houses_8$val_love_hotel[i] <- bog_houses_8_nb[[i]]==0
  
}

bog_houses_8 <- bog_houses_8 %>% mutate(love_hotel_final = ifelse(val_love_hotel == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_love_hotel)))) 

drop <- c("num_love_hotel", "val_love_hotel")
bog_houses_8 <- bog_houses_8[,!names(bog_houses_8)%in%drop]

bog_houses_8 <- bog_houses_8 %>% mutate(love_hotel = love_hotel_final - vecinos_final) 


# marketplace

bog_houses_8 <- bind_rows(bog_houses_8, osm_marketplace_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_8_sp <- bog_houses_8%>%st_buffer(300)%>%as_Spatial()

bog_houses_8_nb <- poly2nb(pl=bog_houses_8_sp, queen= TRUE)

bog_houses_8$num_marketplace <- NA
bog_houses_8$val_marketplace <- NA

for (i in 1:length(bog_houses_8_nb)) {
  
  bog_houses_8$num_marketplace[i] <- size_sum(bog_houses_8_nb[[i]])
  bog_houses_8$val_marketplace[i] <- bog_houses_8_nb[[i]]==0
  
}

bog_houses_8 <- bog_houses_8 %>% mutate(marketplace_final = ifelse(val_marketplace == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_marketplace)))) 

drop <- c("num_marketplace", "val_marketplace")
bog_houses_8 <- bog_houses_8[,!names(bog_houses_8)%in%drop]

bog_houses_8 <- bog_houses_8 %>% mutate(marketplace =  marketplace_final - vecinos_final) 



# monastery

bog_houses_8 <- bind_rows(bog_houses_8, osm_monastery_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_8_sp <- bog_houses_8%>%st_buffer(300)%>%as_Spatial()

bog_houses_8_nb <- poly2nb(pl=bog_houses_8_sp, queen= TRUE)

bog_houses_8$num_monastery <- NA
bog_houses_8$val_monastery <- NA

for (i in 1:length(bog_houses_8_nb)) {
  
  bog_houses_8$num_monastery[i] <- size_sum(bog_houses_8_nb[[i]])
  bog_houses_8$val_monastery[i] <- bog_houses_8_nb[[i]]==0
  
}

bog_houses_8 <- bog_houses_8 %>% mutate(monastery_final = ifelse(val_monastery == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_monastery)))) 

drop <- c("num_monastery", "val_monastery")
bog_houses_8 <- bog_houses_8[,!names(bog_houses_8)%in%drop]

bog_houses_8 <- bog_houses_8 %>% mutate(monastery = monastery_final - vecinos_final) 


# parking

bog_houses_8 <- bind_rows(bog_houses_8, osm_parking_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_8_sp <- bog_houses_8%>%st_buffer(300)%>%as_Spatial()

bog_houses_8_nb <- poly2nb(pl=bog_houses_8_sp, queen= TRUE)

bog_houses_8$num_parking <- NA
bog_houses_8$val_parking <- NA

for (i in 1:length(bog_houses_8_nb)) {
  
  bog_houses_8$num_parking[i] <- size_sum(bog_houses_8_nb[[i]])
  bog_houses_8$val_parking[i] <- bog_houses_8_nb[[i]]==0
  
}

bog_houses_8 <- bog_houses_8 %>% mutate(parking_final = ifelse(val_parking == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_parking)))) 

drop <- c("num_parking", "val_parking")
bog_houses_8 <- bog_houses_8[,!names(bog_houses_8)%in%drop]

bog_houses_8 <- bog_houses_8 %>% mutate(parking = parking_final - vecinos_final) 


# police

bog_houses_8 <- bind_rows(bog_houses_8, osm_police_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_8_sp <- bog_houses_8%>%st_buffer(300)%>%as_Spatial()

bog_houses_8_nb <- poly2nb(pl=bog_houses_8_sp, queen= TRUE)

bog_houses_8$num_police <- NA
bog_houses_8$val_police <- NA

for (i in 1:length(bog_houses_8_nb)) {
  
  bog_houses_8$num_police[i] <- size_sum(bog_houses_8_nb[[i]])
  bog_houses_8$val_police[i] <- bog_houses_8_nb[[i]]==0
  
}

bog_houses_8 <- bog_houses_8 %>% mutate(police_final = ifelse(val_police == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_police)))) 

drop <- c("num_police", "val_police")
bog_houses_8 <- bog_houses_8[,!names(bog_houses_8)%in%drop]

bog_houses_8 <- bog_houses_8 %>% mutate(police = police_final - vecinos_final) 


# restaurant

bog_houses_8 <- bind_rows(bog_houses_8, osm_restaurant_Bogota_sf%>%dplyr::select(osm_id,amenity))

bog_houses_8_sp <- bog_houses_8%>%st_buffer(300)%>%as_Spatial()

bog_houses_8_nb <- poly2nb(pl=bog_houses_8_sp, queen= TRUE)

bog_houses_8$num_restaurant <- NA
bog_houses_8$val_restaurant <- NA

for (i in 1:length(bog_houses_8_nb)) {
  
  bog_houses_8$num_restaurant[i] <- size_sum(bog_houses_8_nb[[i]])
  bog_houses_8$val_restaurant[i] <- bog_houses_8_nb[[i]]==0
  
}

bog_houses_8 <- bog_houses_8 %>% mutate(restaurant_final = ifelse(val_restaurant == TRUE, 0, as.numeric(gsub(".*?([0-9]+).*", "\\1", num_restaurant)))) 

drop <- c("num_restaurant", "val_restaurant")
bog_houses_8 <- bog_houses_8[,!names(bog_houses_8)%in%drop]

bog_houses_8 <- bog_houses_8 %>% mutate(restaurant =  restaurant_final - vecinos_final) 

write_rds(bog_houses_8, "bog_houses_8_1.RDS")

bog_houses_8_2 <- bog_houses_8 %>% select(property_id, bank, bus_station, casino, childcare, cinema,
                                          clinic, college, community_centre, conference_centre, dentist, 
                                          doctors, events_venue, fast_food, hospital,
                                          kindergarten, library, love_hotel, marketplace, monastery,
                                          parking, police, restaurant)

write_rds(bog_houses_8_2, "bog_houses_8_2.RDS")

write_csv(bog_houses_8_2, "bog_houses_8_2.csv")

bog_houses_1_2_f <- bog_houses_1_2 %>% subset(is.na(property_id)==FALSE)
bog_houses_2_2_f <- bog_houses_2_2 %>% subset(is.na(property_id)==FALSE)
bog_houses_3_2_f <- bog_houses_3_2 %>% subset(is.na(property_id)==FALSE)
bog_houses_4_2_f <- bog_houses_4_2 %>% subset(is.na(property_id)==FALSE)
bog_houses_5_2_f <- bog_houses_5_2 %>% subset(is.na(property_id)==FALSE)
bog_houses_6_2_f <- bog_houses_6_2 %>% subset(is.na(property_id)==FALSE)
bog_houses_7_2_f <- bog_houses_7_2 %>% subset(is.na(property_id)==FALSE)
bog_houses_8_2_f <- bog_houses_8_2 %>% subset(is.na(property_id)==FALSE)

bogota_am <- rbind(bog_houses_1_2_f, bog_houses_2_2_f,
                   bog_houses_3_2_f, bog_houses_4_2_f,
                   bog_houses_5_2_f, bog_houses_6_2_f,
                   bog_houses_7_2_f, bog_houses_8_2_f)

write_rds(bogota_am, "bog_am.RDS")

write_csv(bogota_am, "bog_am.csv")

