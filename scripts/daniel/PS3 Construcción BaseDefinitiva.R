#####Código para dejar bases definitivas

###Limpiar 
rm(list = ls())
cat("\014")

## llamar pacman (contiene la función p_load)
require(pacman)

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

#Establacer directorios
#Directorio Daniel
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning/BD-ML---PS3/data")

#Se carga la base creada originalmente
houses_completa <- readRDS("Censo/BaseCompleta.rds")

#Se carga la base que tiene las bases de Medellín y Bogotá modificadas
houses_completa2 <- readRDS("BogMed_sf.rds")

#Guardar solo las variables de "Final_"
keep<- c("property_id", "Final_Bathrooms", 
                       "Final_Metros", "bathrooms_knn", "geometry")

#Guardar la base definitiva
houses_completa2 <-houses_completa2[,(names(houses_completa2) %in% keep)]

#Renombrar las variables de final
#Final_Bathrooms
names(houses_completa2)[names(houses_completa2) == "Final_Bathrooms"] <- "Final_Bathrooms_2"
#Final_Metros
names(houses_completa2)[names(houses_completa2) == "Final_Metros"] <- "Final_Metros_2"

#Pegar las variables
#baños
houses_completa$Final_Bathrooms_2 <- NA
houses_completa$Final_Bathrooms_2[which(houses_completa$property_id == houses_completa2$property_id)] <- houses_completa2$Final_Bathrooms_2[which(houses_completa$property_id == houses_completa2$property_id)]

#Los baños faltantes se llenan con los de KNN
houses_completa$Final_Bathrooms_2[which(houses_completa$property_id == houses_completa2$property_id & is.na(houses_completa$Final_Bathrooms_2) == TRUE)] <- houses_completa2$bathrooms_knn[which(houses_completa$property_id == houses_completa2$property_id & is.na(houses_completa$Final_Bathrooms_2) == TRUE)]


#Metros
houses_completa$Final_Metros_2 <- NA
houses_completa$Final_Metros_2[which(houses_completa$property_id == houses_completa2$property_id)] <- houses_completa2$Final_Metros_2[which(houses_completa$property_id == houses_completa2$property_id)]

#names(houses_completa)
#View(houses_completa%>%dplyr::select("description","Final_Bathrooms", "Final_Bathrooms_2", "Final_Metros", "Final_Metros_2"))



#Exportar esta base
write_rds(houses_completa, "BaseDefinitivaTrain")

#Revisar el número de NA's
sapply(houses_completa, function(y) sum(length(which(is.na(y)))))

#Intentar hacer un st_join con las upz de Bogotá y las comunas de medellín
#Primero intentar unir las divisiones geográficas de Bogotá y Medellín
med_bog <- bind_rows(bog,med)
#st_join
houses_geography <- st_join(houses_completa, med_bog)

#Parece que manda 3 cosas para la misma ubicación, los 2 que no sirven empiezan por "UPZs" que que sirve por "UPZ
houses_geography_def <- houses_geography%>%subset(substr(name,1,4) != "UPZs" | substr(name,1,6) == "Comuna" | is.na(name) == TRUE)

#Hacer el "keep" de las variables definitivas
keep2 <- c(names(houses_completa), "name")
#Guardar la base definitiva
houses_geography_def <-houses_geography_def[,(names(houses_geography_def) %in% keep2)]

sapply(houses_geography_def, function(y) sum(length(which(is.na(y)))))

#Incluir las que faltaron de la original
#Se carga la base que tiene las bases de Medellín y Bogotá modificadas
houses_completa2 <- readRDS("BogMed_sf.rds")
class(houses_completa2)
names(houses_completa2)
names(houses_geography_def)

houses_geography_def$description <- NA
houses_geography_def$bathrooms <- NA
houses_geography_def$description[which(houses_geography_def$property_id == houses_completa2$property_id)] <- houses_completa2$description[which(houses_geography_def$property_id == houses_completa2$property_id)]
houses_geography_def$bathrooms[which(houses_geography_def$property_id == houses_completa2$property_id)] <- houses_completa2$bathrooms[which(houses_geography_def$property_id == houses_completa2$property_id)]


class(houses_geography)


#Exportar
write_rds(houses_geography_def, "BaseGeografía.rds")

