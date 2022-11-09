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

### EJEMPLO

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


