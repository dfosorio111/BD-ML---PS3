rm(list=ls())

#Samuel
setwd("~/Desktop/Big Data/Repositorios/BD-ML---PS3/scripts/samuel")

require(pacman) 
p_load(tidyverse,rio,skimr,viridis,
       gstat, sf, leaflet, 
       nngeo, spdep, tmaptools,
       ggsn, osmdata, glue,
       patchwork, vip, ggrepel,
       stringi, tidytext, stopwords,
       tidymodels, finetune, ggplot2, stringr)

train <- read_rds("~/Desktop/Big Data/Repositorios/BD-ML---PS3/data/train_final.RDS")
test <- read_rds("~/Desktop/Big Data/Repositorios/BD-ML---PS3/data/test_final.RDS")

precios <- read_rds("~/Desktop/Big Data/Repositorios/BD-ML---PS3/data/Test_con_predicciones.RDS")

## crear la caja de coordenada que contiene el polígono de Cali
cali <- opq(bbox = getbb("Cali Colombia"))

## crear la caja de coordenada que contiene el polígono de Bogotá, DC
bog <- opq(bbox = getbb("Bogotá Colombia"))

## crear la caja de coordenada que contiene el polígono de Medellín
med <- opq(bbox = getbb("Medellín Colombia"))

## Casas por ciudad

bog_h <- train %>% subset(city == "Bogotá D.C")

med_h <- train %>% subset(city == "Medellín")

## Sf

bog_sf <- st_as_sf(x = bog_h, crs=4326)

med_sf <- st_as_sf(x = med_h, crs=4326)

cali_sf <- st_as_sf(x = precios, crs=4326)

# Precio m2

bog_sf <- bog_sf %>% mutate(pm = (price / new_surface_def)/1000000)

med_sf <- med_sf %>% mutate(pm = (price / new_surface_def)/1000000)

cali_sf <- cali_sf %>% mutate(pm = (predictions_XGcustom2 / new_surface_def)/1000000)


## Info barrios

barrios_bog <- read_rds("~/Desktop/Big Data/Repositorios/BD-ML---PS3/data/BogotáUPZ.RDS")

keep <- c("osm_id", "name")

barrios_bog <- barrios_bog[, (names(barrios_bog) %in% keep)]

barrios_med <- read_rds("~/Desktop/Big Data/Repositorios/BD-ML---PS3/data/MedellínComunas.rds")

barrios_med <- barrios_med[, (names(barrios_med) %in% keep)]

barrios_cali <- read_rds("~/Desktop/Big Data/Repositorios/BD-ML---PS3/data/CaliComunas.rds")

barrios_cali <- barrios_cali[, (names(barrios_cali) %in% keep)]


## Unión info barrios

bog_sf <- st_join(bog_sf, barrios_bog)

med_sf <- st_join(med_sf, barrios_med)

cali_sf <- st_join(cali_sf, barrios_cali)


## Promedios de precios por barrio

prom_bog <- bog_sf %>%
  group_by(name) %>%
  summarise_at(vars(pm), list(promedio = mean))

prom_med <- med_sf %>%
  group_by(name) %>%
  summarise_at(vars(pm), list(promedio = mean))

prom_cali <- cali_sf %>%
  group_by(name) %>%
  summarise_at(vars(pm), list(promedio = mean))

## Quitar NAs

prom_med <- prom_med %>% subset(is.na(name) == F)
prom_bog <- prom_bog %>% subset(is.na(name) == F)

## Bogotá

bog_sin <- prom_bog %>% st_set_geometry(NULL)

# Quitar localicades

bog_sin <- prom_bog %>% st_set_geometry(NULL)

bog_join <- full_join(barrios_bog, bog_sin, by="name")

bog_join <- bog_join[-c(1:2),]

# Dividir 

barrios_bog_1 <- bog_join %>% filter(str_detect(name, "Localidad"))

barrios_bog_2 <- bog_join %>% filter(str_detect(name, "UPZ "))

# Mapa 

ggplot()+
  geom_sf(data=barrios_bog_1, aes(fill = promedio)) + theme_bw() +
  theme(axis.title =element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text = element_text(size=6))+
  ggtitle("Precio del metro cuadrado según localidad en Bogotá")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(fill = "Millones de COP")

ggsave("mapas/bogloc", dpi=300, dev='png', height=7, width=7, units="in")

ggplot()+
  geom_sf(data=barrios_bog_2, aes(fill = promedio)) + theme_bw() +
  theme(axis.title =element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text = element_text(size=6))+
  ggtitle("Precio del metro cuadrado según UPZ en Bogotá")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(fill = "Millones de COP")

ggsave("mapas/bogupz", dpi=300, dev='png', height=7, width=7, units="in")

## Medellín

med_sin <- prom_med %>% st_set_geometry(NULL)

# Merge Medellín

med_total <- merge(barrios_med, med_sin, by = "name")

med_total <- barrios_med %>% bind_cols(med_sin)

# Mapa Medellín

ggplot()+
  geom_sf(data=med_total, aes(fill = promedio)) + theme_bw() +
  theme(axis.title =element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text = element_text(size=6))+
  ggtitle("Precio del metro cuadrado según comuna en Medellín")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(fill = "Millones de COP")

ggsave("mapas/medellin", dpi=300, dev='png', height=7, width=7, units="in")

## Cali

cali_sin <- prom_cali %>% st_set_geometry(NULL)

# Merge Cali

cali_total <- merge(barrios_cali, cali_sin, by = "name")

# Mapa Cali

ggplot()+
  geom_sf(data=cali_total, aes(fill = promedio)) + theme_bw() +
  theme(axis.title =element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text = element_text(size=6))+
  ggtitle("Precio del metro cuadrado según comuna en Cali")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(fill = "Millones de COP")

ggsave("mapas/cali", dpi=300, dev='png', height=7, width=7, units="in")
