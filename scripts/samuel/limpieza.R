rm(list=ls())

#Samuel
setwd("~/Desktop/Big Data/Repositorios/BD-ML---PS3/scripts/samuel")

require(pacman) 
p_load(tidyverse,rio,skimr,viridis,
       gstat, ## variogram
       sf, ## leer/escribir/manipular datos espaciales
       leaflet, ## Visualizaciones dinámicas
       nngeo, ## st_nn function
       spdep, ## Construct neighbours list from polygon list 
       osmdata) ## Get OSM's data

train <- read_rds("data/train.Rds")

## dataframe to sf
train <- st_as_sf(x = train, ## datos
                  coords=c("lon","lat"), ## coordenadas
                  crs=4326) ## CRS

st_geometry(train)

leaflet() %>% addTiles() %>% addCircleMarkers(data=train)

table(is.na(train$surface_total))

#Completar M2

train <- train %>% mutate(
  new_surface= surface_total)

table(is.na(train$new_surface))

train <- train %>% mutate(
  new_surface= ifelse(is.na(new_surface),surface_covered,new_surface))

x1 <- "[:space:]+[:digit:]+[:space:]+"
x2 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+"
x3 <- "[:space:]+[:digit:]+"
x4 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+"
x5 <- "[:digit:]+[:space:]+"
x6 <- "[:digit:]+[:punct:]+[:digit:]+[:space:]+"
x7 <- "[:digit:]+"
x8 <- "[:digit:]+[:punct:]+[:digit:]+"

x9 <- "+[:space:]+[:digit:]+[:space:]"
x10 <- "+[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]"
x11 <- "+[:space:]+[:digit:]"
x12 <- "+[:space:]+[:digit:]+[:punct:]+[:digit:]"
x13 <- "+[:digit:]+[:space:]"
x14 <- "+[:digit:]+[:punct:]+[:digit:]+[:space:]"
x15 <- "+[:digit:]"
x16 <- "+[:digit:]+[:punct:]+[:digit:]"

for (i in c("mts","m2","mt2","mts2","metros","cuadrad","mtro","mtr2", "M2", "mtrs",
            "MTR", "MTRS", "metrs", "meters","AREA","area", "área", "ÁREA","espacio de",
            "ESPACIO DE", "Mt", "Mts", "Metros", "Mtr", "Mtrs", "Mtr2", "Mtrs2", "Mt2", "Mts2",
            "Metros2", "mtrs2", "metros2", "mts.","m2.","mt2.","mts2.","metros.","cuadrad.","mtro.","mtr2.", "M2", "mtrs",
            "MTR.", "MTRS.", "metrs.", "meters.","AREA.","area.", "área.", "ÁREA.","espacio de.",
            "ESPACIO DE.", "Mt.", "Mts.", "Metros.", "Mtr.", "Mtrs.", "Mtr2.", "Mtrs2.", "Mt2.", "Mts2.",
            "Metros2,", "mtrs2,", "metros2,",  "mts,","m2,","mt2,","mts2,","metros,","cuadrad,","mtro,","mtr2,", "M2", "mtrs",
            "MTR,", "MTRS,", "metrs,", "meters,","AREA,","area,", "área,", "ÁREA,","espacio de,",
            "ESPACIO DE,", "Mt,", "Mts,", "Metros,", "Mtr,", "Mtrs,", "Mtr2,", "Mtrs2,", "Mt2,", "Mts2,",
            "Metros2,", "mtrs2,", "metros2,","Área.","Área","Área,", "M", "M.", "M,", "m", "m.", "m,","mt", "mt", 
            "mt.", "MT2", "MT2,", "MT2.")){
  train <- train %>% 
    mutate(new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x1,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x2,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x3,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x4,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x5,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x6,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x7,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x8,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(i,x9)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(i,x10)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(i,x11)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(i,x12)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(i,x13)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(i,x14)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(i,x15)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(i,x16)),new_surface))
}

## clean var
for (i in c("mts","m2","mt2","mts2","metros","cuadrad","mtro","mtr2", "M2", "mtrs",
            "MTR", "MTRS", "metrs", "meters","AREA","area", "área", "ÁREA","espacio de",
            "ESPACIO DE", "Mt", "Mts", "Metros", "Mtr", "Mtrs", "Mtr2", "Mtrs2", "Mt2", "Mts2",
            "Metros2", "mtrs2", "metros2", "mts.","m2.","mt2.","mts2.","metros.","cuadrad.","mtro.","mtr2.", "M2", "mtrs",
            "MTR.", "MTRS.", "metrs.", "meters.","AREA.","area.", "área.", "ÁREA.","espacio de.",
            "ESPACIO DE.", "Mt.", "Mts.", "Metros.", "Mtr.", "Mtrs.", "Mtr2.", "Mtrs2.", "Mt2.", "Mts2.",
            "Metros2,", "mtrs2,", "metros2,",  "mts,","m2,","mt2,","mts2,","metros,","cuadrad,","mtro,","mtr2,", "M2", "mtrs",
            "MTR,", "MTRS,", "metrs,", "meters,","AREA,","area,", "área,", "ÁREA,","espacio de,",
            "ESPACIO DE,", "Mt,", "Mts,", "Metros,", "Mtr,", "Mtrs,", "Mtr2,", "Mtrs2,", "Mt2,", "Mts2,",
            "Metros2,", "mtrs2,", "metros2,","Área.","Área","Área,", "M", "M.", "M,", "m", "m.", "m,","mt", "mt",
            "mt.", "MT2", "MT2,", "MT2.")){
  train$new_surface <- gsub(i,"",train$new_surface)
}
train$new_surface <- gsub(",",".",train$new_surface)
train$new_surface <- as.numeric(train$new_surface)

table(is.na(train$surface_total))
table(is.na(train$new_surface))
table(is.na(train$bathrooms))

# Arreglo 2

for (i in c("METROS", "m", "mt", "mt,")){
  train <- train %>% 
    mutate(new_surface = ifelse(is.na(new_surface)==T,str_extract_(string=description , pattern=paste0(x1,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x2,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x3,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x4,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x5,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x6,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x7,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x8,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(i,x9)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(i,x10)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(i,x11)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(i,x12)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(i,x13)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(i,x14)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(i,x15)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(i,x16)),new_surface))
}

## clean var
for (i in c("METROS", "m", "mt", "mt,")){
  train$new_surface <- gsub(i,"",train$new_surface)
}
train$new_surface <- gsub(",",".",train$new_surface)
train$new_surface <- as.numeric(train$new_surface)

# Cambiar terrazas


for (i in c("terraza", "balcon", "TERRAZA", "BALCON","Terraza", "balcón", "BALCÓN", 
            "Balcon", "Balcón")){
  train <- train %>% 
    mutate(new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x1,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x2,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x3,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x4,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x5,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x6,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x7,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x8,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(i,x9)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(i,x10)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(i,x11)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(i,x12)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(i,x13)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(i,x14)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(i,x15)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(i,x16)),new_surface))
}

## clean var
for (i in c("METROS", "m", "mt", "mt,")){
  train$new_surface <- gsub(i,"",train$new_surface)
}

table(is.na(train$new_surface))

train <- train %>% mutate(
  new_surface= ifelse(new_surface<30,NA,new_surface))

table(is.na(train$new_surface))

## replace surfare var
table(is.na(train$surface_total))
train$surface_total <- ifelse(is.na(train$surface_total),train$surface_covered,train$surface_total)
table(is.na(train$surface_total))
train$surface_total <- ifelse(is.na(train$surface_total),train$new_surface,train$surface_total)
table(is.na(train$surface_total))





for (i in c("METROS", "metros")){
  train <- train %>% 
    mutate(new_surface = ifelse(is.na(new_surface)==T,str_extract_all(string=description , pattern=paste0(x1,i)),new_surface))
}


str_extract_all(string=train$description , pattern=paste0(x1,"metros"))

## clean var
for (i in c("METROS", "m", "mt", "mt,")){
  train$new_surface <- gsub(i,"",train$new_surface)
}
train$new_surface <- gsub(",",".",train$new_surface)
train$new_surface <- as.numeric(train$new_surface)


