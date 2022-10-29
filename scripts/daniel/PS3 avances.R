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
#Crear la base de test
test <- readRDS("test.Rds")
#Crear la base de train
train <- readRDS("train.RDS")

#Se inicia completando las variables de la base original

#Ver cuáles son las variables que tienen Na's
sapply(train, function(y) sum(length(which(is.na(y)))))
sapply(test, function(y) sum(length(which(is.na(y)))))

#El tipo de propiedad y las habitaciones (bedrooms) no tienen ningún NA
#Primero vamos a rellenar los espacios con NA's de los baños

#Como primer paso se lleva toda la descripción a "minúscula sostenida"
train$description <- tolower(train$description)

#Patrones
x1 <- "[:digit:]+[:punct:]+[:digit:]+[:space:]+"
x2 <- "[:digit:]+[:punct:]+[:digit:]+"
x3 <- "[:digit:]+[:space:]+"
x4 <- "[:digit:]+"
x5 <- "[:word:]+[:space:]+"





#Se crea la variable y se llena con NA's que luego se reemplazan con los valores correctos
train$baños <- NA


## replace values para baños
for (i in c("baño","bano", "bao")){
  train <- train %>% 
    mutate(baños = ifelse(is.na(baños)==T,str_extract(string=description , pattern=paste0(x1,i)),baños),
           baños = ifelse(is.na(baños)==T,str_extract(string=description , pattern=paste0(x2,i)),baños),
           baños = ifelse(is.na(baños)==T,str_extract(string=description , pattern=paste0(x3,i)),baños),
           baños = ifelse(is.na(baños)==T,str_extract(string=description , pattern=paste0(x4,i)),baños),
           baños = ifelse(is.na(baños)==T,str_extract(string=description , pattern=paste0(x5,i)),baños),
           baños = ifelse(is.na(baños)==T,str_extract(string=description , pattern=paste0(x6,i)),baños))
}



## clean var
for (i in c("baño","bano", "bao"," ","\n\n")){
  train$baños <- gsub(i,"",train$baños)
}

#Se crea la variable y se llena con NA's que luego se reemplazan con los valores correctos
train$new_surface <- NA

for (i in c("mts","m2","mt2","metros","cuadrad","mtr", "metrs", "meters","area", "área","espacio de")){
  train <- train %>% 
    mutate(new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x1,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x2,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x3,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x4,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x5,i)),new_surface),
           )
}

## clean var
for (i in c("mts","m2","mt2","metros","cuadrad","mtr", "metrs", "meters","area", "área","espacio de")){
  train$new_surface <- gsub(i,"",train$new_surface)
}

View(train%>%dplyr::select(surface_total, surface_covered, new_surface, description))

train$new_surface_def <- ifelse(is.na(train$surface_covered) == TRUE, train$new_surface, train$surface_covered)

table(is.na(train$surface_covered))
table(is.na(train$new_surface))
table(is.na(train$new_surface_def))

#Ver qué valores tiene 
View(train%>%count(baños))

#Aquellas que se pueden convertir a numéricas directamente se convierten
train$baños2 <- as.numeric(train$baños)

#Se crea una lista de posibles textos de resultado
list_ori <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "uno", "dos",
              "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez",
              "01", "02", "03", "04", "05", "06", "07", "08", "09", "1.5", "2.5",
              "3.5", "4.5", "5.5", "6.5", "7.5", "8.5", "9.5")
#Se crea una lista de valores para reemplazar
list_rep <- c(1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,
              1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5)



#Los valores de texto se reemplazan por números
train$baños3 <- NA
for (i in 1:length(list_ori)) {
  train$baños3[which(train$baños == list_ori[i])] <- list_rep[i]
}

View(train%>%dplyr::select(baños,baños2,baños3))

#Creamos una variable definitiva con las 2 anteriores
train$baños_def <- train$baños2
train$baños_def <- ifelse(is.na(train$baños_def)==TRUE, train$baños3, train$baños_def)

####stand-by####
#Cargar manzanas
mnz <- st_read("mgn/MGN_URB_MANZANA.shp")%>%dplyr::select(MANZ_CCNCT)
#Cargar censo
censo <- import("mnz_censo_2018.rds")

#Convertir datafram a sf
train_sf <- st_as_sf(x = train, ## datos
                     coords=c("lon","lat"), ## coordenadas
                     crs=4326) ## CRS

## unir dos conjuntos de datos basados en la geometría
house <- st_join(x=train_sf, y=mnz)

#Quitar los NA's de las manzanas (Acá se muere)
house <- st_join(x=house , y=mnz , join=st_nn , maxdist=20 , k=1 , progress=F)
 


######Vecinos espaciales#####
#Convertir datafram a sf
train_sf <- st_as_sf(x = train, ## datos
                     coords=c("lon","lat"), ## coordenadas
                     crs=4326) ## CRS

set.seed(1000)
aleatorio <- sample(1:nrow(train_sf),200)



train_sp <- train_sf %>% st_buffer(100) %>% as_Spatial() # poligonos

## obtener vecinos
train_nb <- poly2nb(pl=train_sp , queen=T) # opcion reina

#Crear variables
train_sf$bathrooms_Def <- NA
train_sf$metros_Def <- NA
for (i in 1:nrow(train_sf)) {
  train_sf$bathrooms_Def[i] <- mean(train_sf$bathrooms[train_nb[[i]]],na.rm=T)
  #train_sf$metros_Def[i] <- mean(train_sf$surface_covered[train_nb[[i]]],na.rm=T)
}

names(probando)
View(train_sf%>%dplyr::select(description, bathrooms, baños_def, bathrooms_Def))

#Guardar la variable definitiva
train_sf$Final_Bathrooms <- ifelse(is.na(train_sf$bathrooms) == TRUE, ifelse(is.na(train_sf$baños_def)==TRUE,train_sf$bathrooms_Def,train_sf$baños_def),train_sf$bathrooms)
View(train_sf%>%dplyr::select(description, Final_Bathrooms, bedrooms))

#Los que faltan, vamos a suponer que tienen el mismo número de baños que de cuartos
train_sf$Final_Bathrooms <- ifelse(is.na(train_sf$Final_Bathrooms)== TRUE, train_sf$bedrooms, train_sf$Final_Bathrooms)



### Pruebas con pocos datos


probando <- train_sf[aleatorio,]
probando_sp <- probando %>% st_buffer(1000) %>% as_Spatial() 
## obtener vecinos
train_nb <- poly2nb(pl=probando_sp , queen=T) # opcion reina

probando$metros_Def <- NA
probando$metros_Def2 <- NA
for (i in 1:nrow(probando)) {
  temporal <- as.data.frame(probando[train_nb[[i]],])
  probando$metros_Def [i] <- mean(temporal$surface_covered[which(temporal$bedrooms == probando$bedrooms[i])], na.rm = TRUE)
  probando$metros_Def2[i] <- mean(probando$surface_covered[train_nb[[i]]],na.rm=T)
}

View(probando%>%dplyr::select(surface_covered, metros_Def, metros_Def2, description))

prueba <- probando%>%dplyr::select(surface_covered, metros_Def, metros_Def2, description)%>%subset(is.na(surface_covered) == FALSE & is.na(metros_Def) == FALSE & is.na(metros_Def2) == FALSE)

mean((prueba$surface_covered-prueba$metros_Def)^2)
mean((prueba$surface_covered-prueba$metros_Def2)^2)


probando$bedrooms[32]
probando$bedrooms[train_nb[[32]]]
probando$surface_covered[train_nb[[32]]]
temporal <- as.data.frame(probando[train_nb[[32]],])
mean(temporal$surface_covered[which(temporal$bedrooms == probando$bedrooms[32])], na.rm = TRUE)
