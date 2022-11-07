###
#devtools::install_github("fsingletonthorn/words_to_numbers")
#library(wordstonumbers)



setwd('C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS3/scripts/diego/data')
#### **0.1 Instalar/llamar las librerías de la clase**
require(pacman) 
p_load(tidyverse,rio,skimr,viridis,
       gstat, ## variogram
       sf, ## leer/escribir/manipular datos espaciales
       leaflet, ## Visualizaciones dinámicas
       nngeo, ## st_nn function
       spdep, ## Construct neighbours list from polygon list 
       osmdata) ## Get OSM's data

#### **0.2 Importar conjuntos de datos**

## cargar datos de entrenamiento 
train <- import("train.rds")
## cargar datos de prueba
test <- import("test.rds")


class(train)
skim(train)

class(test)
skim(test)



## convertir dataframe de casas de datos de entrenamiento a  sf 
train_sf <- st_as_sf(x = train, ## datos
                  coords=c("lon","lat"), ## coordenadas
                  crs=4326) ## CRS

st_geometry(train_sf)


## convertir dataframe de casas de datos de prueba a sf 
test_sf <- st_as_sf(x = test, ## datos
                     coords=c("lon","lat"), ## coordenadas
                     crs=4326) ## CRS

st_geometry(test_sf)



### Ver cuáles son las variables que tienen Na's
sapply(train_sf, function(y) sum(length(which(is.na(y)))))
sapply(test_sf, function(y) sum(length(which(is.na(y)))))



### FILTROS
## filtrar datos de entrenamiento de Bogota con geometria

# obtener caja de coordenadas de Bogota
# featuretype = "boundary:administrative"
# format_out = sf polygon
bogota_sf <- getbb(place_name = "Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon

leaflet() %>% addTiles() %>% addPolygons(data=bogota_sf)


## filtrar datos de prueba de Cali con geometria

# obtener caja de coordenadas de Bogota
# featuretype = "boundary:administrative"
# format_out = sf polygon
cali_sf <- getbb(place_name = "Cali", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon

leaflet() %>% addTiles() %>% addPolygons(data=cali_sf)





### opcion 2
## crear interseccion entre x base de casas sf, y sf_polygon   
#houses_inter1 <- st_intersection(x = bogota_sf , y = train_sf)


# graficar sf_polygon de la interseccion entre sf de datos de entrenamiento e interseccion bog_sf_polygon,train_sf
leaflet() %>% addTiles() %>% addPolygons(data=bogota_sf,col="red") %>% addCircles(data=houses_inter1)


### GUARDAR interseccion entre datos entrenamiento y sf_polygon
write_rds(houses_inter1, 'houses_inter1.RDS')


### CARGAR interseccion entre datos entrenamiento y sf_polygon
houses_inter1 <- readRDS('houses_inter1.RDS')




### REGEX: Expresiones regulares

## Regular Expressions
## reemplazar todo


### SOBRE DATOS DE ENTRENAMIENTO: train_sf


# cargar variable de texto de descripcion en minuscula
train_sf$description <- tolower(train_sf$description)


### PATRONES
### VARIABLE BASE: surface_total

x1 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+" # " #.# mts"
x2 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+" # " #.#mts"
x3 <- "[:space:]+[:digit:]+[:punct:]+" # " #.mts"
x4 <- "[:space:]+[:digit:]+[:space:]+"  # " # mts"
x5 <- "[:space:]+[:digit:]+"  # " #mts"
x6 <- "[:digit:]+[:space:]+"  # "# mts"

# crear nueva variable regex 'surface_regex' a partir de REGEX 
#train_sf <- train_sf %>% 
#  mutate(surface_regex = str_extract(string=description , pattern= x1))
#table(house$surface_regex) %>% sort() %>% head()


# crear nueva variable regex 'surface_regex' a partir de REGEX vacia

train_sf$surface_regex <- NA


# iterar por patrones de metros^2
for (i in c("mts","m2","mt2","mts2","metros","cuadrad","mtro","mtr2")){
  
  # crear variable
  train_sf <- train_sf %>% 
    mutate(surface_regex = ifelse(is.na(surface_regex)==TRUE,str_extract(string=description , pattern=paste0(x1,i)),surface_regex),
           surface_regex = ifelse(is.na(surface_regex)==TRUE,str_extract(string=description , pattern=paste0(x2,i)),surface_regex),
           surface_regex = ifelse(is.na(surface_regex)==TRUE,str_extract(string=description , pattern=paste0(x3,i)),surface_regex),
           surface_regex = ifelse(is.na(surface_regex)==TRUE,str_extract(string=description , pattern=paste0(x4,i)),surface_regex),
           surface_regex = ifelse(is.na(surface_regex)==TRUE,str_extract(string=description , pattern=paste0(x5,i)),surface_regex),
           surface_regex = ifelse(is.na(surface_regex)==TRUE,str_extract(string=description , pattern=paste0(x6,i)),surface_regex),
           
           )
}

### limpiar variable surface_regex
for (i in c("mts","m2","mt2","mts2","metros","cuadrad","mtro","mtr2"," ","\n\n")){
  train_sf$surface_regex <- gsub(i,"",train_sf$surface_regex)
}

train_sf$surface_regex <- gsub(",",".",train_sf$surface_regex)
train_sf$surface_regex <- as.numeric(train_sf$surface_regex)


### Ver bases con variables de interés

View(train_sf%>%select(bathrooms,surface_total,surface_covered,surface_regex,description))   


### reemplazar variable 'surface_total' de la base de datos 1. con surface_covered 2. con patrones extraidos de REGEX

# contar NAs en la base
table(is.na(train_sf$surface_total))
train_sf$surface_total <- ifelse(is.na(train_sf$surface_total),train_sf$surface_covered,train_sf$surface_total)

table(is.na(train_sf$surface_total))
train_sf$surface_total <- ifelse(is.na(train_sf$surface_total),train_sf$surface_regex,train_sf$surface_total)

table(is.na(train_sf$surface_total))


### Ver cuáles son las variables que tienen Na's
sapply(train_sf, function(y) sum(length(which(is.na(y)))))
sapply(test_sf, function(y) sum(length(which(is.na(y)))))



### REGEX PALABRAS

### PATRONES
x7 <- "[:word:]+[:space:]+metros"  # "string mts"
x8 <- "[:word:]+metros"  # "stringmts"



texto <- train_sf$description[25]

# extraer patron
word <- str_extract(string = texto , pattern= x8) ## extract pattern

# split de palabras
word_split <- str_split(word, " ")

num_list <- NA
for (palabra in word_split) {
  
  append(num_list,word2num(palabra))
}



### PATRONES
### VARIABLE BASE: bathrooms
x11 <- "[:space:]+[:digit:]+[:space:]+"  # " # baños"
x22 <- "[:space:]+[:digit:]+"  # " #baños"
x33 <- "[:digit:]+[:space:]+"  # "# baños"

# crear nueva variable regex 'surface_regex' a partir de REGEX 
#train_sf <- train_sf %>% 
#  mutate(surface_regex = str_extract(string=description , pattern= x1))
#table(house$surface_regex) %>% sort() %>% head()


# crear nueva variable regex 'surface_regex' a partir de REGEX vacia

train_sf$bathrooms_regex <- NA

# iterar por patrones baños
# extraer patron
for (i in c("baño","bano","bao","baños","banos","bao" )){
  train_sf <- train_sf %>% 
    mutate(bathrooms_regex = ifelse(is.na(bathrooms_regex)==TRUE,str_extract(string=description , pattern=paste0(x11,i)),bathrooms_regex),   
           bathrooms_regex = ifelse(is.na(bathrooms_regex)==TRUE,str_extract(string=description , pattern=paste0(x22,i)),bathrooms_regex),
           )
}


### limpiar variable surface_regex
for (i in c("baño","bano","bao","baños","banos","bao" ,  " ","\n\n")){
  train_sf$bathrooms_regex <- gsub(i,"",train_sf$bathrooms_regex)
}
train_sf$bathrooms_regex <- gsub(",",".",train_sf$bathrooms_regex)
train_sf$bathrooms_regex <- as.numeric(train_sf$bathrooms_regex)


### Ver bases con variables de interés

View(train_sf%>%select(bathrooms,bathrooms_regex,description))   


### reemplazar variable 'surface_total' de la base de datos 1. con surface_covered 2. con patrones extraidos de REGEX

# contar NAs en la base
table(is.na(train_sf$bathrooms))
train_sf$bathrooms <- ifelse(is.na(train_sf$bathrooms),train_sf$bathrooms_regex,train_sf$bathrooms)

table(is.na(train_sf$bathrooms))


#train_sf$surface_total <- ifelse(is.na(train_sf$surface_total),train_sf$surface_regex,train_sf$surface_total)
#table(is.na(train_sf$surface_total))





### REGEX PALABRAS
### VARIABLE BASE: bathrooms

### PATRONES
x_1 <- "uno+[:space:]+"  # " uno baños"
x_2 <- "dos+[:space:]+"  # " dos baños"
x_3 <- "tres+[:space:]+"  # " tres baños"
x_4 <- "cuatro+[:space:]+"  # " cuatro baños"
x_5 <- "cinco+[:space:]+"  # " cinco baños"
x_6 <- "seis+[:space:]+"  # " seis baños"
x_7 <- "siete+[:space:]+"  # " siete baños"
x_8 <- "ocho+[:space:]+"  # " ocho baños"
x_9 <- "nueve+[:space:]+"  # " nueve baños"
x_10 <- "diez+[:space:]+"  # " diez baños"
x_11 <- "once+[:space:]+"  # " once baños"
x_12 <- "doce+[:space:]+"  # " doce baños"
x_13 <- "trece+[:space:]+"  # " trece baños"
x_14 <- "catorce+[:space:]+"  # " catorce baños"
x_15 <- "quince+[:space:]+"  # " quince baños"
x_16 <- "dieciseis+[:space:]+"  # " dieciseis baños"
x_17 <- "diecisiete+[:space:]+"  # " diecisiete baños"
x_18 <- "dieciocho+[:space:]+"  # " dieciocho baños"
x_19 <- "diecinueve+[:space:]+"  # " diecinueve baños"
x_20<- "veinte+[:space:]+"  # " veinte baños"



# iterar por patrones baños
# extraer patron
for (i in c("baño","bano","bao","baños","banos","bao" )){
  train_sf <- train_sf %>% 
    mutate(bathrooms_regex = ifelse(is.na(bathrooms_regex)==TRUE,str_extract(string=description , pattern=paste0(x_1,i)),bathrooms_regex),   
           bathrooms_regex = ifelse(is.na(bathrooms_regex)==TRUE,str_extract(string=description , pattern=paste0(x_2,i)),bathrooms_regex),
           bathrooms_regex = ifelse(is.na(bathrooms_regex)==TRUE,str_extract(string=description , pattern=paste0(x_3,i)),bathrooms_regex),
           bathrooms_regex = ifelse(is.na(bathrooms_regex)==TRUE,str_extract(string=description , pattern=paste0(x_4,i)),bathrooms_regex),
           bathrooms_regex = ifelse(is.na(bathrooms_regex)==TRUE,str_extract(string=description , pattern=paste0(x_5,i)),bathrooms_regex),
           bathrooms_regex = ifelse(is.na(bathrooms_regex)==TRUE,str_extract(string=description , pattern=paste0(x_6,i)),bathrooms_regex),
           bathrooms_regex = ifelse(is.na(bathrooms_regex)==TRUE,str_extract(string=description , pattern=paste0(x_7,i)),bathrooms_regex),
           bathrooms_regex = ifelse(is.na(bathrooms_regex)==TRUE,str_extract(string=description , pattern=paste0(x_8,i)),bathrooms_regex),
           bathrooms_regex = ifelse(is.na(bathrooms_regex)==TRUE,str_extract(string=description , pattern=paste0(x_9,i)),bathrooms_regex),
           bathrooms_regex = ifelse(is.na(bathrooms_regex)==TRUE,str_extract(string=description , pattern=paste0(x_10,i)),bathrooms_regex),
           bathrooms_regex = ifelse(is.na(bathrooms_regex)==TRUE,str_extract(string=description , pattern=paste0(x_11,i)),bathrooms_regex),
           bathrooms_regex = ifelse(is.na(bathrooms_regex)==TRUE,str_extract(string=description , pattern=paste0(x_12,i)),bathrooms_regex),
           bathrooms_regex = ifelse(is.na(bathrooms_regex)==TRUE,str_extract(string=description , pattern=paste0(x_13,i)),bathrooms_regex),
           bathrooms_regex = ifelse(is.na(bathrooms_regex)==TRUE,str_extract(string=description , pattern=paste0(x_14,i)),bathrooms_regex),
           bathrooms_regex = ifelse(is.na(bathrooms_regex)==TRUE,str_extract(string=description , pattern=paste0(x_15,i)),bathrooms_regex),
           bathrooms_regex = ifelse(is.na(bathrooms_regex)==TRUE,str_extract(string=description , pattern=paste0(x_16,i)),bathrooms_regex),
           bathrooms_regex = ifelse(is.na(bathrooms_regex)==TRUE,str_extract(string=description , pattern=paste0(x_17,i)),bathrooms_regex),
           bathrooms_regex = ifelse(is.na(bathrooms_regex)==TRUE,str_extract(string=description , pattern=paste0(x_18,i)),bathrooms_regex),
           bathrooms_regex = ifelse(is.na(bathrooms_regex)==TRUE,str_extract(string=description , pattern=paste0(x_19,i)),bathrooms_regex),
           bathrooms_regex = ifelse(is.na(bathrooms_regex)==TRUE,str_extract(string=description , pattern=paste0(x_20,i)),bathrooms_regex),
    )
}


### limpiar variable surface_regex
for (i in c("baño","bano","bao","baños","banos","bao" ,  " ","\n\n")){
  train_sf$bathrooms_regex <- gsub(i,"",train_sf$bathrooms_regex)
}
train_sf$bathrooms_regex <- gsub(",",".",train_sf$bathrooms_regex)
train_sf$bathrooms_regex <- as.numeric(train_sf$bathrooms_regex)


### Ver bases con variables de interés

View(train_sf%>%select(bathrooms,bathrooms_regex,description))   


### reemplazar variable 'surface_total' de la base de datos 1. con surface_covered 2. con patrones extraidos de REGEX

# contar NAs en la base
table(is.na(train_sf$bathrooms))
train_sf$bathrooms <- ifelse(is.na(train_sf$bathrooms),train_sf$bathrooms_regex,train_sf$bathrooms)

table(is.na(train_sf$bathrooms))









### FUNCION

# funcion para convertir palabras en numeros

word2num <- function(word){
  wsplit <- strsplit(tolower(word)," ")[[1]]
  one_digits <- list(cero=0, uno=1, dos=2, tres=3, cuatro=4, cinco=5,
                     seis=6, siete=7, ocho=8, nueve=9)
  teens <- list(once=11, doce=12, trece=13, catorce=14, quince=15,
                dieciseis=16, diecisiete =17, dieciocho =18, diecinueve=19)
  ten_digits <- list(diez=10, veinte=20, treinta=30, cuarenta=40, cincuenta=50,
                     sesenta=60, setenta=70, ochenta=80, noventa=90)
  doubles <- c(teens,ten_digits)
  out <- 0
  i <- 1
  while(i <= length(wsplit)){
    j <- 1
    if(i==1 && wsplit[i]=="hundred")
      temp <- 100
    else if(i==1 && wsplit[i]=="thousand")
      temp <- 1000
    else if(wsplit[i] %in% names(one_digits))
      temp <- as.numeric(one_digits[wsplit[i]])
    else if(wsplit[i] %in% names(teens))
      temp <- as.numeric(teens[wsplit[i]])
    else if(wsplit[i] %in% names(ten_digits))
      temp <- (as.numeric(ten_digits[wsplit[i]]))
    if(i < length(wsplit) && wsplit[i+1]=="hundred"){
      if(i>1 && wsplit[i-1] %in% c("hundred","thousand"))
        out <- out + 100*temp
      else
        out <- 100*(out + temp)
      j <- 2
    }
    else if(i < length(wsplit) && wsplit[i+1]=="thousand"){
      if(i>1 && wsplit[i-1] %in% c("hundred","thousand"))
        out <- out + 1000*temp
      else
        out <- 1000*(out + temp)
      j <- 2
    }
    else if(i < length(wsplit) && wsplit[i+1] %in% names(doubles)){
      temp <- temp*100
      out <- out + temp
    }
    else{
      out <- out + temp
    }
    i <- i + j
  }
  return(list(word,out))
}





