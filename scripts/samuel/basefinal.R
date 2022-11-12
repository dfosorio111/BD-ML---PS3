rm(list=ls())

setwd("C:/Users/de.franco/Desktop/PS3")

require(pacman)
p_load(tidyverse,rio,skimr,viridis,
       gstat, ## variogram
       sf, ## leer/escribir/manipular datos espaciales
       leaflet, ## Visualizaciones dinámicas
       nngeo, ## st_nn function
       spdep, ## Construct neighbours list from polygon list
       tmaptools, ## geocodificar
       ggsn, ## map scale bar 
       osmdata,
       ggplot2) ## Get OSM's data

train <- read_rds("train_Final_completa.RDS")

test <- read_rds("test.RDS")

## crear la caja de coordenada que contiene el polígono de Cali
cali <- opq(bbox = getbb("Cali Colombia"))

## crear la caja de coordenada que contiene el polígono de Bogotá, DC
bog <- opq(bbox = getbb("Bogotá Colombia"))

## crear la caja de coordenada que contiene el polígono de Medellín
med <- opq(bbox = getbb("Medellín Colombia"))


cbd_b <- geocode_OSM("Plaza de Bolívar, Bogotá", as.sf=T) 

cbd_m <- geocode_OSM("Centro Administrativo La Alpujarra, Medellín", as.sf=T)

cbd_c <- geocode_OSM("Plaza Caycedo, Cali", as.sf=T) 


cbd_b2 <- geocode_OSM("Centro Internacional, Bogotá", as.sf=T)

cbd_m2 <- geocode_OSM("Milla de Oro, Medellín", as.sf=T)

cbd_c2 <- geocode_OSM("Plazoleta Jorge Isaacs, Cali", as.sf=T) 


train_sf <- st_as_sf(x = train, crs=4326)

test_sf <- st_as_sf(x = test, ## datos
                    coords=c("lon","lat"), ## coordenadas
                    crs=4326) ## CRS

bog_sf <- train_sf %>% subset(city == "Bogotá D.C")

med_sf <- train_sf %>% subset(city == "Medellín")


matrix_dist_bog1 <- st_distance(x=bog_sf , y=cbd_b)

matrix_dist_bog2 <- st_distance(x=bog_sf , y=cbd_b2)

matrix_dist_med1 <- st_distance(x=med_sf , y=cbd_m)

matrix_dist_med2 <- st_distance(x=med_sf , y=cbd_m2)

matrix_dist_cal1 <- st_distance(x=test_sf , y=cbd_c)

matrix_dist_cal2 <- st_distance(x=test_sf , y=cbd_c2)


dist_bog1 <- apply(matrix_dist_bog1 , 1 , mean)

dist_bog2 <- apply(matrix_dist_bog2 , 1 , mean)

dist_med1 <- apply(matrix_dist_med1 , 1 , mean)

dist_med2 <- apply(matrix_dist_med2 , 1 , mean)

dist_cal1 <- apply(matrix_dist_cal1 , 1 , mean)

dist_cal2 <- apply(matrix_dist_cal2 , 1 , mean)

bog_sf <- bog_sf %>% 
  mutate(dist_cbd1 = dist_bog1)

bog_sf <- bog_sf %>% 
  mutate(dist_cbd2 = dist_bog2)

med_sf <- med_sf %>%
  mutate(dist_cbd1 = dist_med1)

med_sf <- med_sf %>%
  mutate(dist_cbd2 = dist_med2)

test_sf <- test_sf %>%
  mutate(dist_cbd1 = dist_cal1)

test_sf <- test_sf %>%
  mutate(dist_cbd2 = dist_cal2)

keep <- c("property_id", "dist_cbd1", "dist_cbd2")

bog_sf <- bog_sf[, (names(bog_sf) %in% keep)]

med_sf <- med_sf[, (names(med_sf) %in% keep)]

test_sf <- test_sf[, (names(test_sf) %in% keep)]

train_sf <- rbind(bog_sf, med_sf)

write_rds(train_sf, "train_cbd.RDS")

write_rds(test_sf, "test_cbd.RDS")

train_casi <- read_rds("train_Final_completa.RDS")

test_casi <- read_rds("TEST_BASE_CENSO_ORIGINALES_REGEX_reducida.rds")

am_test <- read_rds("test_am.RDS")

test_casi <- test_casi %>% bind_cols(am_test)

test_casi <- test_casi %>% rename(property_id = property_id...1)

test_casi <- test_casi %>% rename(geometry = geometry...7)

drop3 <- c("geometry...18", "property_id...17")

test_casi <- test_casi[,!names(test_casi)%in%drop3]

test_def <- test_casi %>% bind_cols(test_sf)

test_def <- test_def %>% rename(property_id = property_id...1)

test_def <- test_def %>% rename(geometry = geometry...7)

drop4 <- c("geometry...64", "property_id...61")

test_def <- test_def[,!names(test_def)%in%drop4]

train_def <- train_casi %>% bind_cols(train_sf)

train_def <- train_def %>% rename(property_id = property_id...1)

train_def <- train_def %>% rename(geometry = geometry.x...7)

drop5 <- c("geometry.y", "geometry.x...66", "property_id...63")

train_def <- train_def[,!names(train_def)%in%drop5]

names(train_def)

names(test_def)

baños <- read_rds("TEST_BASE_CENSO_ORIGINALES_REGEX.rds")

keep1 <- c("property_id", "Final_Bathrooms")

baños <- baños[,names(baños)%in%keep1]

test_def <- test_def %>% bind_cols(baños)

test_def <- test_def %>% rename(property_id = property_id...1)

test_def <- test_def %>% rename(geometry = geometry...7)

drop6 <- c("geometry...65", "property_id...63")

test_def <- test_def[,!names(test_def)%in%drop6]

test_def <- test_def %>% rename(Final_Bathrooms_2 = Final_Bathrooms)

write_rds(train_def, "train_final.RDS")

write_rds(test_def, "test_final.RDS")

test_final <- read_rds("test_final.RDS")

train_final <- read_rds("train_final.RDS")
