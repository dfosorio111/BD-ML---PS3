rm(list=ls())

#Samuel
setwd("~/Desktop/Big Data/Repositorios/BD-ML---PS3/scripts/samuel")

#Oficina
setwd("C:/Users/smalkun/OneDrive - Asociación Nacional de Instituciones Financieras/Escritorio/Big Data/BD-ML---PS3/scripts/samuel")

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

train <- read_rds("data/train.Rds")
test <- readRDS("data/test.Rds")

bog_radio <- read_rds("bog_am.RDS")
med_radio <- read_rds("med_houses_2.RDS")
cali_radio <- read_rds("cali_houses_2.RDS")

med_radio <- med_radio %>% subset(is.na(property_id)==FALSE)
cali_radio <- cali_radio %>% subset(is.na(property_id)==FALSE)

bog_dist <- read_rds("C:/Users/smalkun/OneDrive - Asociación Nacional de Instituciones Financieras/Escritorio/Big Data/BD-ML---PS3/data/amenities_bogota.RDS")
med_dist <- read_rds("C:/Users/smalkun/OneDrive - Asociación Nacional de Instituciones Financieras/Escritorio/Big Data/BD-ML---PS3/data/amenities_medellin.RDS")
cali_dist <- read_rds("C:/Users/smalkun/OneDrive - Asociación Nacional de Instituciones Financieras/Escritorio/Big Data/BD-ML---PS3/data/amenities_cali.RDS")


drop <- c("city", "price", "surface_total", "surface_covered",
          "rooms", "bedrooms", "bathrooms", "title", "description",
          "property_type", "operation_type")

bog_dist <- bog_dist[,!names(bog_dist)%in%drop]
med_dist <- med_dist[,!names(med_dist)%in%drop]
cali_dist <- cali_dist[,!names(cali_dist)%in%drop]

bog_dist <- bog_dist %>% rename(arts_centre_d = arts_centre_bog)
bog_dist <- bog_dist %>% rename(bank_d = bank_bog)
bog_dist <- bog_dist %>% rename(bus_station_d = bus_station_bog)
bog_dist <- bog_dist %>% rename(casino_d = casino_bog)
bog_dist <- bog_dist %>% rename(childcare_d = childcare_bog)
bog_dist <- bog_dist %>% rename(cinema_d = cinema_bog)
bog_dist <- bog_dist %>% rename(clinic_d = clinic_bog)
bog_dist <- bog_dist %>% rename(college_d = college_bog)
bog_dist <- bog_dist %>% rename(community_centre_d = community_centre_bog)
bog_dist <- bog_dist %>% rename(conference_centre_d = conference_centre_bog)
bog_dist <- bog_dist %>% rename(dentist_d = dentist_bog)
bog_dist <- bog_dist %>% rename(doctors_d = doctors_bog)
bog_dist <- bog_dist %>% rename(events_d = events_bog)
bog_dist <- bog_dist %>% rename(fast_food_d = fast_food_bog)
bog_dist <- bog_dist %>% rename(hospital_d = hospital_bog)
bog_dist <- bog_dist %>% rename(kindergarten_d = kindergarten_bog)
bog_dist <- bog_dist %>% rename(library_d = library_bog)
bog_dist <- bog_dist %>% rename(love_hotel_d = love_hotel_bog)
bog_dist <- bog_dist %>% rename(marketplace_d = marketplace_bog)
bog_dist <- bog_dist %>% rename(monastery_d = monastery_bog)
bog_dist <- bog_dist %>% rename(parking_d = parking_bog)
bog_dist <- bog_dist %>% rename(police_d = police_bog)
bog_dist <- bog_dist %>% rename(restaurant_d = restaurant_bog)


med_dist <- med_dist %>% rename(bank_d = bank_med)
med_dist <- med_dist %>% rename(bus_station_d = bus_station_med)
med_dist <- med_dist %>% rename(casino_d = casino_med)
med_dist <- med_dist %>% rename(childcare_d = childcare_med)
med_dist <- med_dist %>% rename(cinema_d = cinema_med)
med_dist <- med_dist %>% rename(clinic_d = clinic_med)
med_dist <- med_dist %>% rename(college_d = college_med)
med_dist <- med_dist %>% rename(community_centre_d = community_centre_med)
med_dist <- med_dist %>% rename(conference_centre_d = conference_centre_med)
med_dist <- med_dist %>% rename(dentist_d = dentist_med)
med_dist <- med_dist %>% rename(doctors_d = doctors_med)
med_dist <- med_dist %>% rename(events_d = events_med)
med_dist <- med_dist %>% rename(fast_food_d = fast_food_med)
med_dist <- med_dist %>% rename(hospital_d = hospital_med)
med_dist <- med_dist %>% rename(kindergarten_d = kindergarten_med)
med_dist <- med_dist %>% rename(library_d = library_med)
med_dist <- med_dist %>% rename(love_hotel_d = love_hotel_med)
med_dist <- med_dist %>% rename(marketplace_d = marketplace_med)
med_dist <- med_dist %>% rename(monastery_d = monastery_med)
med_dist <- med_dist %>% rename(parking_d = parking_med)
med_dist <- med_dist %>% rename(police_d = police_med)
med_dist <- med_dist %>% rename(restaurant_d = restaurant_med)


cali_dist <- cali_dist %>% rename(bank_d = bank_cali)
cali_dist <- cali_dist %>% rename(bus_station_d = bus_station_cali)
cali_dist <- cali_dist %>% rename(casino_d = casino_cali)
cali_dist <- cali_dist %>% rename(childcare_d = childcare_cali)
cali_dist <- cali_dist %>% rename(cinema_d = cinema_cali)
cali_dist <- cali_dist %>% rename(clinic_d = clinic_cali)
cali_dist <- cali_dist %>% rename(college_d = college_cali)
cali_dist <- cali_dist %>% rename(community_centre_d = community_centre_cali)
cali_dist <- cali_dist %>% rename(conference_centre_d = conference_centre_cali)
cali_dist <- cali_dist %>% rename(dentist_d = dentist_cali)
cali_dist <- cali_dist %>% rename(doctors_d = doctors_cali)
cali_dist <- cali_dist %>% rename(events_d = events_cali)
cali_dist <- cali_dist %>% rename(fast_food_d = fast_food_cali)
cali_dist <- cali_dist %>% rename(hospital_d = hospital_cali)
cali_dist <- cali_dist %>% rename(kindergarten_d = kindergarten_cali)
cali_dist <- cali_dist %>% rename(library_d = library_cali)
cali_dist <- cali_dist %>% rename(love_hotel_d = love_hotel_cali)
cali_dist <- cali_dist %>% rename(marketplace_d = marketplace_cali)
cali_dist <- cali_dist %>% rename(monastery_d = monastery_cali)
cali_dist <- cali_dist %>% rename(parking_d = parking_cali)
cali_dist <- cali_dist %>% rename(police_d = police_cali)
cali_dist <- cali_dist %>% rename(restaurant_d = restaurant_cali)


drop2 <- c("arts_centre_d")

bog_dist <- bog_dist[,!names(bog_dist)%in%drop2]


bog_am_f <- bog_dist %>% bind_cols(bog_radio)

bog_am_f <- bog_am_f %>% rename(property_id = property_id...1)

bog_am_f <- bog_am_f %>% rename(geometry = geometry...2)

drop3 <- c("geometry...48", "property_id...25")

bog_am_f <- bog_am_f[,!names(bog_am_f)%in%drop3]

med_am_f <- med_dist %>% bind_cols(med_radio)

med_am_f <- med_am_f %>% rename(property_id = property_id...1)

med_am_f <- med_am_f %>% rename(geometry = geometry...2)

med_am_f <- med_am_f[,!names(med_am_f)%in%drop3]

cali_am_f <- cali_dist %>% bind_cols(cali_radio)

cali_am_f <- cali_am_f %>% rename(property_id = property_id...1)

cali_am_f <- cali_am_f %>% rename(geometry = geometry...2)

cali_am_f <- cali_am_f[,!names(cali_am_f)%in%drop3]

train_am <- rbind(bog_am_f, med_am_f)

write_rds(train_am, "train_am.RDS")
write_rds(cali_am_f, "test_am.RDS")
