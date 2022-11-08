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
  
  # split sobre el patron de entrada INICIAL
  wsplit <- strsplit(tolower(word)," ")[[1]]
  
  
  contador <- 0
  for (word in wsplit) {
    
    contador <- contador + 1
    if (word=="y") {
      wsplit <- wsplit[-contador]
    }
    
  }
  
  
  
  # crear diccionarios de orden de magnitud
  one_digits <- list(cero=0, un=1, dos=2, tres=3, cuatro=4, cinco=5,
                     seis=6, siete=7, ocho=8, nueve=9)
  teens <- list(once=11, doce=12, trece=13, catorce=14, quince=15,
                dieciseis=16, diecisiete =17, dieciocho =18, diecinueve=19)
  
  veintis <- list(veintiun=21, veintidos=22, veintitres=23, veinticuatro=24, veinticinco=25,
                  veintiseis=26, veintisiete =27, veintiocho =28, veintinueve=29)
  
  ten_digits <- list(diez=10, veinte=20, treinta=30, cuarenta=40, cincuenta=50,
                     sesenta=60, setenta=70, ochenta=80, noventa=90)
  
  hun_digits <- list(ciento=100, doscientos=200, trescientos=300, cuatrocientos=400, quinientos=500,
                     seiscientos=600, setecientos=700, ochocientos=800, novecientos=900)
  
  
  doubles <- c(teens,ten_digits)
  
  
  # numero de salida
  out <- 0
  
  i <- 1
  
  # itera sobre la lista de split de patron
  while(i <= length(wsplit)){
    
    # cuenta numero de palabras auxiliar  crear variable j=1
    j <- 1
    
    
    ### SI PATRON ES 100 O 1000
    # 1 palabra Y es cien=100
    if(i==1 && wsplit[i]=="uno")
      temp <- 1
    
    if(i==1 && wsplit[i]=="diez")
      temp <- 10
    
    
    if(i==1 && wsplit[i]=="cien")
      temp <- 100
    
    # 1 palabra Y es mil=1000
    else if(i==1 && wsplit[i]=="mil")
      temp <- 1000
    
    
    ### SI PATRON ESTA EN LOS DICCIONARIOS
    
    # wsplit[i] esta en ones
    else if(wsplit[i] %in% names(one_digits))
      temp <- as.numeric(one_digits[wsplit[i]])
    
    # wsplit[i] esta en teens
    else if(wsplit[i] %in% names(teens))
      temp <- as.numeric(teens[wsplit[i]])
    
    
    # wsplit[i] esta en veintis
    else if(wsplit[i] %in% names(veintis))
      temp <- as.numeric(veintis[wsplit[i]])
    
    
    # wsplit[i] esta en ten_digits
    else if(wsplit[i] %in% names(ten_digits))
      temp <- (as.numeric(ten_digits[wsplit[i]]))
    
    
    # wsplit[i] esta en hun_digits
    else if(wsplit[i] %in% names(hun_digits))
      temp <- (as.numeric(hun_digits[wsplit[i]]))
    
    
    
    
    ### PATRONES COMPUESTOS
    
    # i es menor al tamaño de wsplit Y  siguiente palabra wsplit[i+1] es ciento
    if(i < length(wsplit) && wsplit[i+1]=="ciento"){
      
      # i es mayor a 1 Y anterior palabra wsplit[i-1] es ciento o mil
      if(i>1 && wsplit[i-1] %in% c("ciento","mil"))
        
        # numero de salida  out es 100*temp + out
        out <- out + 100*temp
      else
        out <- 100*(out + temp)
      
      # j es 2
      j <- 2
    }
    
    
    
    # i es menor al tamaño de wsplit Y siguiente palabra wsplit[i+1] es mil
    else if(i < length(wsplit) && wsplit[i+1]=="mil"){
      
      # i es mayor a 1 Y anterior palabra wsplit[i-1] es ciento o mil
      if(i>1 && wsplit[i-1] %in% c("ciento","mil"))
        
        # numero de salida  out es 1000*temp + out
        out <- out + 1000*temp
      else
        
        out <- 1000*(out + temp)
      
      # j es 2
      j <- 2
    }
    
    #   # i es menor al tamaño de wsplit Y siguiente palabra wsplit[i+1] es 2 cifras
    #    else if(i < length(wsplit) && wsplit[i+1] %in% names(doubles)){
    #      temp <- temp*100
    #      out <- out + temp
    #    }
    
    
    else{
      out <- out + temp
    }
    i <- i + j
  }
  return(list(word,out))
}




#### METROS

# cargar variable de texto de descripcion en minuscula
houses_inter1$description <- tolower(houses_inter1$description)

# crear nueva variable new_surface
houses_inter1$new_surface <- NA

# PATRONES


###
### limpiar variable surface_regex
for (i in c("baño","bano","bao","baños","banos","bao" ,  " ","\n\n")){
  train_sf$bathrooms_regex <- gsub(i,"",train_sf$bathrooms_regex)
}
train_sf$bathrooms_regex <- gsub(",",".",train_sf$bathrooms_regex)
train_sf$bathrooms_regex <- as.numeric(train_sf$bathrooms_regex)






### REGEX PALABRAS

### PATRONES
x1 <- "[:word:]+[:space:]+[:word:]+[:space:]+[:word:]+[:space:]+[:word:]+[:space:]+"  # "string mts"
x2 <- "[:word:]+[:space:]+[:word:]+[:space:]+[:word:]+[:space:]+"
x3 <- "[:word:]+[:space:]+[:word:]+[:space:]+"
x4 <- "[:word:]+[:space:]+"
x5 <- "[:word:]+"

# BASE SAMPLE

prueba <- houses_inter1[1:100,]


# crear variables de patrones
prueba$new_surface1 <- NA
prueba$new_surface2 <- NA
prueba$new_surface3 <- NA
prueba$new_surface4 <- NA
prueba$new_surface5 <- NA

for (i in c( "metros", "m2","mt2", "m ","cuadrad","mtr", "mts",  "metrs", "meters","area", "área","espacio de")){
  
  prueba <- prueba %>% 
      mutate(new_surface1 = ifelse(is.na(new_surface1)==T,str_extract(string=description , pattern=paste0(x1,i)),new_surface1),
             new_surface2 = ifelse(is.na(new_surface2)==T,str_extract(string=description , pattern=paste0(x2,i)),new_surface2),
             new_surface3 = ifelse(is.na(new_surface3)==T,str_extract(string=description , pattern=paste0(x3,i)),new_surface3),
             new_surface4 = ifelse(is.na(new_surface4)==T,str_extract(string=description , pattern=paste0(x4,i)),new_surface4),
             new_surface5 = ifelse(is.na(new_surface5)==T,str_extract(string=description , pattern=paste0(x5,i)),new_surface5),
      )
}

## limpiar variables de palabras
for (i in c("m2","mt2", "m ", "metros","cuadrad","mtr", "mts", "metrs", "meters","area", "área","espacio de",  "\n\n")){
  prueba$new_surface1 <- str_trim(gsub(i,"",prueba$new_surface1))
  prueba$new_surface2 <- str_trim(gsub(i,"",prueba$new_surface2))
  prueba$new_surface3 <- str_trim(gsub(i,"",prueba$new_surface3))
  prueba$new_surface4 <- str_trim(gsub(i,"",prueba$new_surface4))
  prueba$new_surface5 <- str_trim(gsub(i,"",prueba$new_surface5))
}


# variables del IF

prueba$new_surface2_2 <- NA


### TRY CATCH: UN FOR PARA CADA VARIABLE


for (i in 1:nrow(prueba)) {
  
  fine <- NA
  
  tryCatch(               
    
    # Specifying expression
    expr = {              
      
      word2num(prueba$new_surface2[i])
      fine <- "FINE!!!"
      print("Everything was fine.")
    },
    # Specifying error message
    error = function(e){         
      
      print("There was an error message.")
    },
    
    warning = function(w){      
      print("There was a warning message.")
    },
    
    finally = {            
      print("finally Executed")
    }
  )
  

  if (is.na(fine)==FALSE) {
    
    prueba$new_surface2_2[i] <- word2num(prueba$new_surface2[i])[2]
    
  }
  
}







#Si algo quedó con , se reemplaza por punto para convertirlo así a numérico
train$new_surface2 <- as.numeric(gsub(pattern = ",", replacement = ".", x = train$new_surface))

#Note que los que son chiquiticos la gran mayoría están divididos por mil porque usan el punto de separador de miles
train$new_surface2[which(train$new_surface2 < 5)] <- 1000*train$new_surface2[which(train$new_surface2 < 5)]

#Se utiliza el surface total, pero si no tiene dato se llena con surface covered
train$new_surface_def <- ifelse(is.na(train$surface_total),train$surface_covered,train$surface_total)
is.na(train$new_surface_def)%>%table() #34600 NA


#Ahora, los que queden vacío ahí se llenan con la del regex
train$new_surface_def2 <- ifelse(is.na(train$new_surface_def),train$new_surface2,train$new_surface_def)
is.na(train$new_surface_def2)%>%table() #19360 NA



### TRY CATCH

tryCatch(               
  
  # Specifying expression
  expr = {              
    
    
    temp <- gsub(i,"",str_extract(string=houses_inter1$description[j], pattern=paste0(x1,i)))
    
    word2num("cuarenta y dos")
    fine <- "FINE!!!"
    print("Everything was fine.")
  },
  # Specifying error message
  error = function(e){         
    
    print("There was an error message.")
  },
  
  warning = function(w){      
    print("There was a warning message.")
  },
  
  finally = {            
    print("finally Executed")
  }
)

