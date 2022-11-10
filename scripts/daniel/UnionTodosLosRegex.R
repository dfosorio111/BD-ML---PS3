### REGEX CON WORD2NUM PARA METROS

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

#Se carga la base que tiene todo

train_def <- read_rds("TRAIN_REGEX_CENSO_GEOGRAFIA.rds")


### REGEX PALABRAS


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


### PATRONES DE PALABRAS
x1 <- "[:word:]+[:space:]+[:word:]+[:space:]+[:word:]+[:space:]+[:word:]+[:space:]+"  # "string mts"
x2 <- "[:word:]+[:space:]+[:word:]+[:space:]+[:word:]+[:space:]+"
x3 <- "[:word:]+[:space:]+[:word:]+[:space:]+"
x4 <- "[:word:]+[:space:]+"
x5 <- "[:word:]+"


# crear variables de patrones
train_def$new_surface1 <- NA
train_def$new_surface2 <- NA
train_def$new_surface3 <- NA
train_def$new_surface4 <- NA
train_def$new_surface5 <- NA

# variables del IF
train_def$new_surface1_1 <- NA
train_def$new_surface2_2 <- NA
train_def$new_surface3_3 <- NA
train_def$new_surface4_4 <- NA
train_def$new_surface5_5 <- NA

#se deja todo el textro en mínusclulas para facilitar los sufijos
train_def$description <- tolower(train_def$description)

# iterar por sufijos de 'metros'
for (i in c( "metros", "m2","mt2", "m ","cuadrad","mtr", "mts",  "metrs", "meters","area", "área","espacio de")){
  
  # agregar variable new_surface# que extrae de la variable de descripcion el patron i
  train_def <- train_def %>% 
    mutate(new_surface1 = ifelse(is.na(new_surface1)==T,str_extract(string=description , pattern=paste0(x1,i)),new_surface1),
           new_surface2 = ifelse(is.na(new_surface2)==T,str_extract(string=description , pattern=paste0(x2,i)),new_surface2),
           new_surface3 = ifelse(is.na(new_surface3)==T,str_extract(string=description , pattern=paste0(x3,i)),new_surface3),
           new_surface4 = ifelse(is.na(new_surface4)==T,str_extract(string=description , pattern=paste0(x4,i)),new_surface4),
           new_surface5 = ifelse(is.na(new_surface5)==T,str_extract(string=description , pattern=paste0(x5,i)),new_surface5),
    )
}


## limpiar variables de patrones i extraidos
for (i in c("m2","mt2", "m ", "metros","cuadrad","mtr", "mts", "metrs", "meters","area", "área","espacio de",  "\n\n")){
  train_def$new_surface1 <- str_trim(gsub(i,"",train_def$new_surface1))
  train_def$new_surface2 <- str_trim(gsub(i,"",train_def$new_surface2))
  train_def$new_surface3 <- str_trim(gsub(i,"",train_def$new_surface3))
  train_def$new_surface4 <- str_trim(gsub(i,"",train_def$new_surface4))
  train_def$new_surface5 <- str_trim(gsub(i,"",train_def$new_surface5))
}



### TRY CATCH:

for (i in 1:nrow(train_def)) {
  
  fine <- FALSE
  
  tryCatch(               
    
    # Specifying expression
    expr = {              
      
      word2num(train_def$new_surface1[i])
      fine <- TRUE
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
  
  
  if (fine == TRUE ) {
    
    train_def$new_surface1_1[i] <- as.numeric(word2num(train_def$new_surface1[i])[[2]])
    
  }
  
}



for (i in 1:nrow(train_def)) {
  
  fine <- FALSE
  
  tryCatch(               
    
    # Specifying expression
    expr = {              
      
      word2num(train_def$new_surface2[i])
      fine <- TRUE
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
  
  
  if (fine == TRUE ) {
    
    train_def$new_surface2_2[i] <- as.numeric(word2num(train_def$new_surface2[i])[[2]])
    
  }
  
}



for (i in 1:nrow(train_def)) {
  
  fine <- FALSE
  
  tryCatch(               
    
    # Specifying expression
    expr = {              
      
      word2num(train_def$new_surface3[i])
      fine <- TRUE
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
  
  
  if (fine == TRUE ) {
    
    train_def$new_surface3_3[i] <- as.numeric(word2num(train_def$new_surface3[i])[[2]])
    
  }
  
}


for (i in 1:nrow(train_def)) {
  
  fine <- FALSE
  
  tryCatch(               
    
    # Specifying expression
    expr = {              
      
      word2num(train_def$new_surface4[i])
      fine <- TRUE
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
  
  
  if (fine == TRUE ) {
    
    train_def$new_surface4_4[i] <- as.numeric(word2num(train_def$new_surface4[i])[[2]])
    
  }
  
}


for (i in 1:nrow(train_def)) {
  
  fine <- FALSE
  
  tryCatch(               
    
    # Specifying expression
    expr = {              
      
      word2num(train_def$new_surface5[i])
      fine <- TRUE
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
  
  
  if (fine == TRUE ) {
    
    train_def$new_surface5_5[i] <- as.numeric(word2num(train_def$new_surface5[i])[[2]])
    
  }
  
}



# crear variable new_surfaceFinal
train_def$new_surfaceFinal <- NA


# iterar sobre las filas 
for (i in 1:nrow(train_def)) {
  
  # convertir variables de texto con NA a -1
  if (is.na(train_def$new_surface1_1[i])==TRUE) {
    train_def$new_surface1_1[i] = -1
  }
  if (is.na(train_def$new_surface2_2[i])==TRUE) {
    train_def$new_surface2_2[i] = -1
  }
  if (is.na(train_def$new_surface3_3[i])==TRUE) {
    train_def$new_surface3_3[i] = -1
  }
  if (is.na(train_def$new_surface4_4[i])==TRUE) {
    train_def$new_surface4_4[i] = -1
  }
  if (is.na(train_def$new_surface5_5[i])==TRUE) {
    train_def$new_surface5_5[i] = -1
  }
  
  #train_def$new_surfaceFinal[i] <-max(train_def$new_surface1_1[i],train_def$new_surface2_2[i],train_def$new_surface3_3[i],train_def$new_surface4_4[i],train_def$new_surface5_5[i])
  
  if (train_def$new_surface1_1[i]>train_def$new_surface2_2[i]) {
    train_def$new_surfaceFinal[i] <- train_def$new_surface1_1[i]
  }else if (train_def$new_surface2_2[i]>train_def$new_surface3_3[i]) {
    train_def$new_surfaceFinal[i] <- train_def$new_surface2_2[i]
  }else if (train_def$new_surface3_3[i]>train_def$new_surface4_4[i]) {
    train_def$new_surfaceFinal[i] <- train_def$new_surface3_3[i]
  }else if (train_def$new_surface4_4[i]>train_def$new_surface5_5[i]) {
    train_def$new_surfaceFinal[i] <- train_def$new_surface4_4[i]
  }else{
    train_def$new_surfaceFinal[i] <- train_def$new_surface5_5[i]
  }
  
}

# mutate para actualizar la variable new_surfaceFinal -1 con NAs 
#También las que están debajo de 40 las voy a quitar, porque empiezan a presentar muchos porblemas, aparecen números, 
#la mayoría se refiere a terrazas, etc.
train_def <- train_def%>%mutate(new_surfaceFinal = ifelse(new_surfaceFinal<40,NA,new_surfaceFinal))


# corroborar cuántos NAs quedan
sapply(train_def, function(y) sum(length(which(is.na(y)))))

################################################Resulta mejor el siguiente orden de criterios:
#1. Utilizar los datos que se tienen de surface covered y surface total
#2. regex antiguo que contempla números
#3. regex que identifica palabras.
#4. vecinos espaciales
#5. ver cómo llenar los NA que quedan

#Primero se deben crear los regex antiguos (lo que sería el segundo criterio)

#Patrones (Se corren nuevamente, importante que se corra solo si ya se corrió todo lo de arriba (regex de texto))
x1 <- "[:digit:]+[:punct:]+[:digit:]+[:space:]+"
x2 <- "[:digit:]+[:punct:]+[:digit:]+"
x3 <- "[:digit:]+[:space:]+"
x4 <- "[:digit:]+"
x5 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+"
x6 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+"
x7 <- "[:space:]+[:digit:]+[:space:]+"
x8 <- "[:space:]+[:digit:]+"

train_def$new_surface <- NA

for (i in c("m2","mt2", "m ", "metros","cuadrad","mtr", "mts",  "metrs", "meters","area", "área","espacio de")){
  train_def <- train_def %>% 
    mutate(new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x1,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x2,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x3,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x4,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x5,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x6,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x7,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x8,i)),new_surface)
    )
}

## clean var
for (i in c("m2","mt2", "m ", "metros","cuadrad","mtr", "mts", "metrs", "meters","area", "área","espacio de")){
  train_def$new_surface <- gsub(i,"",train_def$new_surface)
}

#Si algo quedó con , se reemplaza por punto para convertirlo así a numérico
train_def$new_surface_num <- as.numeric(gsub(pattern = ",", replacement = ".", x = train_def$new_surface))

#Note que los que son chiquiticos la gran mayoría están divididos por mil porque usan el punto de separador de miles
train_def$new_surface_num[which(train_def$new_surface_num < 5)] <- 1000*train_def$new_surface_num[which(train_def$new_surface_num < 5)]

#Ahora sí: criterio número 1
#Se utiliza el surface total, pero si no tiene dato se llena con surface covered
train_def$new_surface_def <- ifelse(is.na(train_def$surface_total),train_def$surface_covered,train_def$surface_total)
is.na(train_def$new_surface_def)%>%table() #34.600 NA

#Criterio número 2
#Ahora, los que queden vacío ahí se llenan con la del regex que identifica números
train_def$new_surface_def <- ifelse(is.na(train_def$new_surface_def),train_def$new_surface_num,train_def$new_surface_def)
is.na(train_def$new_surface_def)%>%table() #19.263 NA

#Criterio número 3
#Los que están con NA se llenan con el regex que identifica número en texto (palabras)
train_def$new_surface_def <- ifelse(is.na(train_def$new_surface_def),train_def$new_surfaceFinal,train_def$new_surface_def)
is.na(train_def$new_surface_def)%>%table() #18.935 NA

#Antes de pasar al criterio 4, es buena idea revisar cómo se distribuyen los datos de metros a ver si hay outliers
quantile(train_def$new_surface_def, prob=seq(0.05,1,0.05), na.rm = TRUE) #Parece estar bien en general, pero algunos datos raros en los extremos
#Zoom al 5% de menor tamaño
quantile(train_def$new_surface_def, prob=seq(0,0.05,0.001), na.rm = TRUE)
#Zoom al 5% de mayor tamaño
quantile(train_def$new_surface_def, prob=seq(0.95,1,0.001), na.rm = TRUE)

#Los datos que sean mayores a 5000 en la definitiva y menores a 5000 en alguna de las "intermedias" se cambian
train_def$new_surface_def <- ifelse(train_def$new_surface_def > 5000 & train_def$new_surface_num < 5000,train_def$new_surface_num,train_def$new_surface_def)
#Menos del 0.1% son mayores a 4500 m2 y la mayoría parece estar mal
#Se dejan esos como NA
train_def$new_surface_def <- ifelse(train_def$new_surface_def > 4500,NA,train_def$new_surface_def)

#Ahora por abajo
#Solo el 1.5% de los datos son menores a 17m2 y es poco creible tener apartamentos o casas de ese tamaño
train_def$new_surface_def <- ifelse(train_def$new_surface_def < 17,NA,train_def$new_surface_def)

#Se revisa de nuevo la distribución
quantile(train_def$new_surface_def, prob=seq(0.05,1,0.05), na.rm = TRUE)

#revisión de NA
is.na(train_def$new_surface_def)%>%table() #19.438


#Criterio número 4
#Los datos que quedan faltando por llenar se completan con vecinos espaciales
#Armado de buffers de 300 metros a la redonda
train_sp <- train_def %>% st_buffer(300) %>% as_Spatial() # poligonos

## obtener vecinos
train_nb <- poly2nb(pl=train_sp , queen=T) # opcion reina

#Para que se un poco más rápido solo va a llenar vecinos espaciales de aquellas que están en NA
#Crear variables
train_def$metros_Def <- NA
train_def$metros_Def2 <- NA
for (i in 1:nrow(train_def)) {
  if (is.na(train_def$new_surface_def[i]) == TRUE) {
    temporal <- as.data.frame(train_def[train_nb[[i]],])
    train_def$metros_Def [i] <- mean(temporal$new_surface_def[which(temporal$bedrooms == train_def$bedrooms[i])], na.rm = TRUE)
    train_def$metros_Def2[i] <- mean(train_def$new_surface_def[train_nb[[i]]],na.rm=TRUE)
  }
    
}



#No corrió, pero antes ya se había hecho con buffers de 200m
#Se carga la base que contiene esas variables
BogMed <- read_rds("BogMed_sf.rds")

#Se crean las variables vacías
train_def$metros_Def <- NA
train_def$metros_Def2 <- NA
train_def$metros_Def[which(train_def$property_id == BogMed$property_id)] <- BogMed$metros_Def[which(train_def$property_id == BogMed$property_id)]
train_def$metros_Def2[which(train_def$property_id == BogMed$property_id)] <- BogMed$metros_Def2[which(train_def$property_id == BogMed$property_id)]

#Se une primero con metros def que es la que aproxima el promedio de aquellos que tienen los mismos cuartos
train_def$new_surface_def <- ifelse(is.na(train_def$new_surface_def),train_def$metros_Def,train_def$new_surface_def)

#revisión de NA
is.na(train_def$new_surface_def)%>%table() #840

#Se utiliza ahora metros def 2 que solo es vecinos espaciales
train_def$new_surface_def <- ifelse(is.na(train_def$new_surface_def),train_def$metros_Def2,train_def$new_surface_def)

#revisión de NA
is.na(train_def$new_surface_def)%>%table() #33



# corroborar cuántos NAs quedan
sapply(train_def, function(y) sum(length(which(is.na(y)))))

#Salvar y exportar la base
write_rds(train_def, "BASE_CENSO_ORIGINALES_REGEX.rds")

probando <- read_rds("BASE_CENSO_ORIGINALES_REGEX.rds")

#Solo mantener las variables definitivas
keep <- c("property_id", "city", "price", "bedrooms", "property_type", "operation_type",
          "geometry", "mean_H_NRO_CUARTOS2", "med_H_NRO_CUARTOS2", "sum_HA_TOT_PER2", 
          "med_V_TOT_HOG2", "mod_VA1_ESTRATO2", "mod_V_MAT_PARED2", "mod_V_MAT_PISO2",
          "mod_VE_RECBAS2", "Final_Bathrooms_2", "new_surface_def")

reducida <- train_def[,(names(train_def) %in% keep)]

sapply(reducida, function(y) sum(length(which(is.na(y)))))

#salvar la base reducida
write_rds(reducida, "BASE_CENSO_ORIGINALES_REGEX_reducida.rds")

probando2 <- read_rds("BASE_CENSO_ORIGINALES_REGEX_reducida.rds")
