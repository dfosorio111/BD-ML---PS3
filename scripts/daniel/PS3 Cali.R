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
#Directorio Daniel
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning/BD-ML---PS3/data")
#Crear la base de test que contiene los datos de Cali
test <- readRDS("test.Rds")

#Contar NA's en las variables del test
sapply(test, function(y) sum(length(which(is.na(y)))))

#Como primer paso se lleva toda la descripción a "minúscula sostenida"
test$description <- tolower(test$description)


#################1mer filtro: utilizar regex

#Patrones
x1 <- "[:digit:]+[:punct:]+[:digit:]+[:space:]+"
x2 <- "[:digit:]+[:punct:]+[:digit:]+"
x3 <- "[:digit:]+[:space:]+"
x4 <- "[:digit:]+"
x5 <- "[:word:]+[:space:]+"


#Se crea la variable y se llena con NA's que luego se reemplazan con los valores correctos
test$baños <- NA

## replace values para baños
for (i in c("baño","bano", "bao")){
  test <- test %>% 
    mutate(baños = ifelse(is.na(baños)==T,str_extract(string=description , pattern=paste0(x1,i)),baños),
           baños = ifelse(is.na(baños)==T,str_extract(string=description , pattern=paste0(x2,i)),baños),
           baños = ifelse(is.na(baños)==T,str_extract(string=description , pattern=paste0(x3,i)),baños),
           baños = ifelse(is.na(baños)==T,str_extract(string=description , pattern=paste0(x4,i)),baños),
           baños = ifelse(is.na(baños)==T,str_extract(string=description , pattern=paste0(x5,i)),baños))
}

## clean var
for (i in c("baño","bano", "bao"," ","\n\n")){
  test$baños <- gsub(i,"",test$baños)
}

#Aquellas que se pueden convertir a numéricas directamente se convierten
test$baños2 <- as.numeric(test$baños)

#Se crea una lista de posibles textos de resultado
list_ori <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "uno", "dos",
              "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez",
              "01", "02", "03", "04", "05", "06", "07", "08", "09", "1.5", "2.5",
              "3.5", "4.5", "5.5", "6.5", "7.5", "8.5", "9.5")
#Se crea una lista de valores para reemplazar
list_rep <- c(1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,
              1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5)


#Los valores de texto se reemplazan por números
test$baños3 <- NA
for (i in 1:length(list_ori)) {
  test$baños3[which(test$baños == list_ori[i])] <- list_rep[i]
}

#Creamos una variable definitiva con las 2 anteriores
test$baños_def <- test$baños2
test$baños_def <- ifelse(is.na(test$baños_def)==TRUE, test$baños3, test$baños_def)
#Se arman los baños juntando originales con regex
test$baños_def2 <- ifelse(is.na(test$bathrooms)==TRUE, test$baños_def, test$bathrooms)

sum(is.na(test$baños_def) & is.na(test$bathrooms))


#Se utiliza knn para aquellos sin valores originales (esto se debe hacer antes de la transformación a sf)
knn_houses <- test%>%dplyr::select(bedrooms, lat, lon, bathrooms)%>%subset(is.na(bathrooms) == FALSE)
knn_houses2 <- test%>%subset(is.na(bathrooms) == FALSE)%>%dplyr::select(bedrooms, lat, lon)
x_train <- scale(knn_houses2[,1:3]) 
knn_houses_test <- test%>%subset(is.na(bathrooms) == TRUE)%>%dplyr::select(bedrooms, lat, lon)
x_test <- scale(knn_houses_test[,1:3]) 
nearest5 <- knn(train=x_train, test=x_test, cl=knn_houses$bathrooms, k=5)

#Con esto se va a llenar solo aquellos que al final queden definitivamente en NA
contador <- 0
test$bathrooms_knn <- NA
for (i in 1:nrow(test)) {
  if (is.na(test$bathrooms[i]) == TRUE) {
    contador <- contador + 1
    test$bathrooms_knn[i] <- as.numeric(nearest5[contador])
  }
}

#Se replica el proceso de regex para los metros
#Se crea la variable y se llena con NA's que luego se reemplazan con los valores correctos
test$new_surface <- NA

for (i in c("m2","mt2", "m ", "metros","cuadrad","mtr", "mts",  "metrs", "meters","area", "área","espacio de")){
  test <- test %>% 
    mutate(new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x1,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x2,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x3,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x4,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x5,i)),new_surface),
    )
}

## clean var
for (i in c("m2","mt2", "m ", "metros","cuadrad","mtr", "mts", "metrs", "meters","area", "área","espacio de")){
  test$new_surface <- gsub(i,"",test$new_surface)
}

#Si algo quedó con , se reemplaza por punto para convertirlo así a numérico
test$new_surface2 <- as.numeric(gsub(pattern = ",", replacement = ".", x = test$new_surface))

#Note que los que son chiquiticos la gran mayoría están divididos por mil porque usan el punto de separador de miles
test$new_surface2[which(test$new_surface2 < 5)] <- 1000*test$new_surface2[which(test$new_surface2 < 5)]

#Se utiliza el surface total, pero si no tiene dato se llena con surface covered
test$new_surface_def <- ifelse(is.na(test$surface_total),test$surface_covered,test$surface_total)
is.na(test$new_surface_def)%>%table() #2838 NA


#Ahora, los que queden vacío ahí se llenan con la del regex
test$new_surface_def2 <- ifelse(is.na(test$new_surface_def),test$new_surface2,test$new_surface_def)
is.na(test$new_surface_def2)%>%table() #1636 NA

#########################2do filtro: vecinos espaciales



#Convertir dataframe a sf
test_sf <- st_as_sf(x = test, ## datos
                     coords=c("lon","lat"), ## coordenadas
                     crs=4326) ## CRS

#Armado de buffers de 200 metros a la redonda
test_sp <- test_sf %>% st_buffer(200) %>% as_Spatial() # poligonos

## obtener vecinos
test_nb <- poly2nb(pl=test_sp , queen=T) # opcion reina

#Crear variables
test_sf$bathrooms_Def <- NA
test_sf$metros_Def <- NA
test_sf$metros_Def2 <- NA
for (i in 1:nrow(test_sf)) {
  test_sf$bathrooms_Def[i] <- mean(test_sf$baños_def2[test_nb[[i]]],na.rm=TRUE)
  temporal <- as.data.frame(test_sf[test_nb[[i]],])
  test_sf$metros_Def [i] <- mean(temporal$new_surface_def2[which(temporal$bedrooms == test_sf$bedrooms[i])], na.rm = TRUE)
  test_sf$metros_Def2[i] <- mean(test_sf$new_surface_def2[test_nb[[i]]],na.rm=TRUE)
}

#Revisión de NA
is.na(test_sf$bathrooms_Def)%>%table()
is.na(test_sf$metros_Def)%>%table()
is.na(test_sf$metros_Def2)%>%table()

#Llenar los baños
test_sf$Final_Bathrooms <- ifelse(is.na(test_sf$bathrooms) == TRUE, ifelse(is.na(test_sf$baños_def2) == TRUE, test_sf$bathrooms_Def, test_sf$baños_def2), test_sf$bathrooms)
sum(is.na(test_sf$Final_Bathrooms))

#Completar los metros
test_sf$Final_Metros <- ifelse(is.na(test_sf$new_surface_def2) == TRUE, test_sf$metros_Def, test_sf$new_surface_def2)
sum(is.na(test_sf$Final_Metros))

#Exportar la base 
write_rds(test_sf, "Cali_sf.rds")


#######################################Censo y Manzanas
#Cargar manzanas
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning")
mnz <- st_read("MGN_URB_MANZANA.shp") #%>%dplyr::select(MANZ_CCNCT)
#Manzanas de Cali
mnz_cal <- mnz%>%subset(COD_MPIO == "76001")
#Cargar censo Bogotá
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning/BD-ML---PS3/data/Censo")
## data manzanas
mgn_cal <- import("76ValleDelCauca/CNPV2018_MGN_A2_76.CSV")
#Selección de variables
mgn_cal <- mgn_cal %>% dplyr::select(COD_DANE_ANM,UA_CLASE,COD_ENCUESTAS,U_VIVIENDA)
## data hogar
hog_cal <- import("76ValleDelCauca/CNPV2018_2HOG_A2_76.CSV")
hog_cal <- hog_cal %>% dplyr::select(UA_CLASE,COD_ENCUESTAS,U_VIVIENDA,H_NROHOG,H_NRO_CUARTOS,HA_TOT_PER)
## data vivienda
viv_cal <- import("76ValleDelCauca/CNPV2018_1VIV_A2_76.CSV") 
viv_cal <- viv_cal %>% dplyr::select(COD_ENCUESTAS,UA_CLASE,U_VIVIENDA,V_TOT_HOG,VA1_ESTRATO, V_MAT_PARED, V_MAT_PISO, VE_RECBAS)
## join hogar-vivienda
viv_hog_cal <- left_join(hog_cal,viv_cal,by=c("COD_ENCUESTAS","U_VIVIENDA","UA_CLASE"))
## joing mnz-hogar-vivienda
viv_hog_mgn_cal <- left_join(viv_hog_cal,mgn_cal,by=c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA"))
#Ver las variables que tenemos
names(viv_hog_mgn_cal)

#Exportamos los datos 
export(viv_hog_mgn_cal,"Censo_Cali.rds")

#Crear función moda
moda <- function(codes){
  which.max(tabulate(codes))
}

viv_hog_mgn_cal$H_NRO_CUARTOS[which(viv_hog_mgn_cal$H_NRO_CUARTOS == 99)] <- NA
##=== collapse data ===##
db <- viv_hog_mgn_cal %>%
  group_by(COD_DANE_ANM) %>% 
  summarise(mean_H_NRO_CUARTOS=mean(H_NRO_CUARTOS,na.rm=T),
            med_H_NRO_CUARTOS=median(H_NRO_CUARTOS,na.rm=T),
            sum_HA_TOT_PER=sum(HA_TOT_PER,na.rm=T), 
            med_V_TOT_HOG=median(V_TOT_HOG,na.rm=T),
            mod_VA1_ESTRATO=moda(VA1_ESTRATO),
            mod_V_MAT_PARED=moda(V_MAT_PARED),
            mod_V_MAT_PISO=moda(V_MAT_PISO),
            mod_VE_RECBAS=moda(VE_RECBAS)
  )


#Unir Censo con Manzanas
#Se cambia el crs de las manzanas para que se pueda hacer el pegue
mnz_cal <- st_transform(mnz_cal, 4326)
mnz_cal <- mnz_cal%>%dplyr::select(COD_DANE)
house_cal <- st_join(x=test_sf , y=mnz_cal)

#Base definitiva con las variables del censo, unido por las manzanas
house_def_cal <- left_join(house_cal, db,by=c("COD_DANE"="COD_DANE_ANM"))

#Vecinos cercanos
house_sp_cal <- house_def_cal %>% st_buffer(200) %>% as_Spatial() # poligonos

## obtener vecinos
house_nb_cal <- poly2nb(pl=house_sp_cal , queen=T) # opcion reina

#Crear variables
house_def_cal$mean_H_NRO_CUARTOS2 <- NA
house_def_cal$med_H_NRO_CUARTOS2 <- NA
house_def_cal$sum_HA_TOT_PER2 <- NA
house_def_cal$med_V_TOT_HOG2 <- NA
house_def_cal$mod_VA1_ESTRATO2 <- NA
house_def_cal$mod_V_MAT_PARED2 <- NA
house_def_cal$mod_V_MAT_PISO2 <- NA
house_def_cal$mod_VE_RECBAS2 <- NA
for (i in 1:nrow(house_def_cal)) {
  house_def_cal$mean_H_NRO_CUARTOS2[i] <- mean(house_def_cal$mean_H_NRO_CUARTOS[house_nb_cal[[i]]],na.rm=TRUE)
  house_def_cal$med_H_NRO_CUARTOS2[i] <- mean(house_def_cal$med_H_NRO_CUARTOS[house_nb_cal[[i]]],na.rm=TRUE)
  house_def_cal$sum_HA_TOT_PER2[i] <- mean(house_def_cal$sum_HA_TOT_PER[house_nb_cal[[i]]],na.rm=TRUE)
  house_def_cal$med_V_TOT_HOG2[i] <- mean(house_def_cal$med_V_TOT_HOG[house_nb_cal[[i]]],na.rm=TRUE)
  house_def_cal$mod_VA1_ESTRATO2[i] <- moda(house_def_cal$mod_VA1_ESTRATO[house_nb_cal[[i]]])
  house_def_cal$mod_V_MAT_PARED2[i] <- moda(house_def_cal$mod_V_MAT_PARED[house_nb_cal[[i]]])
  house_def_cal$mod_V_MAT_PISO2[i] <- moda(house_def_cal$mod_V_MAT_PISO[house_nb_cal[[i]]])
  house_def_cal$mod_VE_RECBAS2[i] <- moda(house_def_cal$mod_VE_RECBAS[house_nb_cal[[i]]])
}

#Revisar NA
sapply(house_def_cal, function(y) sum(length(which(is.na(y)))))

#Nombres de la base
names(house_def_cal)

lista_definitivas <- c("property_id","city", "price", "surface_total", "surface_covered", "bedrooms",
                       "title", "description", "property_type", "operation_type", "Final_Bathrooms", 
                       "Final_Metros", "geometry", "COD_DANE", "mean_H_NRO_CUARTOS2", "med_H_NRO_CUARTOS2",
                       "sum_HA_TOT_PER2", "med_V_TOT_HOG2", "mod_VA1_ESTRATO2", "mod_V_MAT_PARED2",
                       "mod_V_MAT_PISO2", "mod_VE_RECBAS2")

#Guardar la base definitiva
houses_Cali <- house_def_cal[,(names(house_def_cal) %in% lista_definitivas)]

#Exportar la base
write_rds(houses_Cali,"BaseCali.rds")



#######################################Completar la base del test con el regex de diego
cali <- read_rds("Censo/BaseCali.rds")
sapply(cali, function(y) sum(length(which(is.na(y)))))

#Cargar la base que tiene los metros_def y metros_def2
cali_sf <- read_rds("Cali_sf.rds")
sapply(cali_sf, function(y) sum(length(which(is.na(y)))))

###############Código del regex
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
cali$new_surface1 <- NA
cali$new_surface2 <- NA
cali$new_surface3 <- NA
cali$new_surface4 <- NA
cali$new_surface5 <- NA

# variables del IF
cali$new_surface1_1 <- NA
cali$new_surface2_2 <- NA
cali$new_surface3_3 <- NA
cali$new_surface4_4 <- NA
cali$new_surface5_5 <- NA

#se deja todo el textro en mínusclulas para facilitar los sufijos
cali$description <- tolower(cali$description)

# iterar por sufijos de 'metros'
for (i in c( "metros", "m2","mt2", "m ","cuadrad","mtr", "mts",  "metrs", "meters","area", "área","espacio de")){
  
  # agregar variable new_surface# que extrae de la variable de descripcion el patron i
  cali <- cali %>% 
    mutate(new_surface1 = ifelse(is.na(new_surface1)==T,str_extract(string=description , pattern=paste0(x1,i)),new_surface1),
           new_surface2 = ifelse(is.na(new_surface2)==T,str_extract(string=description , pattern=paste0(x2,i)),new_surface2),
           new_surface3 = ifelse(is.na(new_surface3)==T,str_extract(string=description , pattern=paste0(x3,i)),new_surface3),
           new_surface4 = ifelse(is.na(new_surface4)==T,str_extract(string=description , pattern=paste0(x4,i)),new_surface4),
           new_surface5 = ifelse(is.na(new_surface5)==T,str_extract(string=description , pattern=paste0(x5,i)),new_surface5),
    )
}


## limpiar variables de patrones i extraidos
for (i in c("m2","mt2", "m ", "metros","cuadrad","mtr", "mts", "metrs", "meters","area", "área","espacio de",  "\n\n")){
  cali$new_surface1 <- str_trim(gsub(i,"",cali$new_surface1))
  cali$new_surface2 <- str_trim(gsub(i,"",cali$new_surface2))
  cali$new_surface3 <- str_trim(gsub(i,"",cali$new_surface3))
  cali$new_surface4 <- str_trim(gsub(i,"",cali$new_surface4))
  cali$new_surface5 <- str_trim(gsub(i,"",cali$new_surface5))
}



### TRY CATCH:

for (i in 1:nrow(cali)) {
  
  fine <- FALSE
  
  tryCatch(               
    
    # Specifying expression
    expr = {              
      
      word2num(cali$new_surface1[i])
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
    
    cali$new_surface1_1[i] <- as.numeric(word2num(cali$new_surface1[i])[[2]])
    
  }
  
}



for (i in 1:nrow(cali)) {
  
  fine <- FALSE
  
  tryCatch(               
    
    # Specifying expression
    expr = {              
      
      word2num(cali$new_surface2[i])
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
    
    cali$new_surface2_2[i] <- as.numeric(word2num(cali$new_surface2[i])[[2]])
    
  }
  
}



for (i in 1:nrow(cali)) {
  
  fine <- FALSE
  
  tryCatch(               
    
    # Specifying expression
    expr = {              
      
      word2num(cali$new_surface3[i])
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
    
    cali$new_surface3_3[i] <- as.numeric(word2num(cali$new_surface3[i])[[2]])
    
  }
  
}


for (i in 1:nrow(cali)) {
  
  fine <- FALSE
  
  tryCatch(               
    
    # Specifying expression
    expr = {              
      
      word2num(cali$new_surface4[i])
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
    
    cali$new_surface4_4[i] <- as.numeric(word2num(cali$new_surface4[i])[[2]])
    
  }
  
}


for (i in 1:nrow(cali)) {
  
  fine <- FALSE
  
  tryCatch(               
    
    # Specifying expression
    expr = {              
      
      word2num(cali$new_surface5[i])
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
    
    cali$new_surface5_5[i] <- as.numeric(word2num(cali$new_surface5[i])[[2]])
    
  }
  
}



# crear variable new_surfaceFinal
cali$new_surfaceFinal <- NA


# iterar sobre las filas 
for (i in 1:nrow(cali)) {
  
  # convertir variables de texto con NA a -1
  if (is.na(cali$new_surface1_1[i])==TRUE) {
    cali$new_surface1_1[i] = -1
  }
  if (is.na(cali$new_surface2_2[i])==TRUE) {
    cali$new_surface2_2[i] = -1
  }
  if (is.na(cali$new_surface3_3[i])==TRUE) {
    cali$new_surface3_3[i] = -1
  }
  if (is.na(cali$new_surface4_4[i])==TRUE) {
    cali$new_surface4_4[i] = -1
  }
  if (is.na(cali$new_surface5_5[i])==TRUE) {
    cali$new_surface5_5[i] = -1
  }
  
  #cali$new_surfaceFinal[i] <-max(cali$new_surface1_1[i],cali$new_surface2_2[i],cali$new_surface3_3[i],cali$new_surface4_4[i],cali$new_surface5_5[i])
  
  if (cali$new_surface1_1[i]>cali$new_surface2_2[i]) {
    cali$new_surfaceFinal[i] <- cali$new_surface1_1[i]
  }else if (cali$new_surface2_2[i]>cali$new_surface3_3[i]) {
    cali$new_surfaceFinal[i] <- cali$new_surface2_2[i]
  }else if (cali$new_surface3_3[i]>cali$new_surface4_4[i]) {
    cali$new_surfaceFinal[i] <- cali$new_surface3_3[i]
  }else if (cali$new_surface4_4[i]>cali$new_surface5_5[i]) {
    cali$new_surfaceFinal[i] <- cali$new_surface4_4[i]
  }else{
    cali$new_surfaceFinal[i] <- cali$new_surface5_5[i]
  }
  
}

# mutate para actualizar la variable new_surfaceFinal -1 con NAs 
#También las que están debajo de 40 las voy a quitar, porque empiezan a presentar muchos porblemas, aparecen números, 
#la mayoría se refiere a terrazas, etc.
cali <- cali%>%mutate(new_surfaceFinal = ifelse(new_surfaceFinal<40,NA,new_surfaceFinal))


# corroborar cuántos NAs quedan
sapply(cali, function(y) sum(length(which(is.na(y)))))

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

cali$new_surface <- NA

for (i in c("m2","mt2", "m ", "metros","cuadrad","mtr", "mts",  "metrs", "meters","area", "área","espacio de")){
  cali <- cali %>% 
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
  cali$new_surface <- gsub(i,"",cali$new_surface)
}

#Si algo quedó con , se reemplaza por punto para convertirlo así a numérico
cali$new_surface_num <- as.numeric(gsub(pattern = ",", replacement = ".", x = cali$new_surface))

#Note que los que son chiquiticos la gran mayoría están divididos por mil porque usan el punto de separador de miles
cali$new_surface_num[which(cali$new_surface_num < 5)] <- 1000*cali$new_surface_num[which(cali$new_surface_num < 5)]

#Ahora sí: criterio número 1
#Se utiliza el surface total, pero si no tiene dato se llena con surface covered
cali$new_surface_def <- ifelse(is.na(cali$surface_total),cali$surface_covered,cali$surface_total)
is.na(cali$new_surface_def)%>%table() #3.838 NA

#Criterio número 2
#Ahora, los que queden vacío ahí se llenan con la del regex que identifica números
cali$new_surface_def <- ifelse(is.na(cali$new_surface_def),cali$new_surface_num,cali$new_surface_def)
is.na(cali$new_surface_def)%>%table() #1.629 NA

#Criterio número 3
#Los que están con NA se llenan con el regex que identifica número en texto (palabras)
cali$new_surface_def <- ifelse(is.na(cali$new_surface_def),cali$new_surfaceFinal,cali$new_surface_def)
is.na(cali$new_surface_def)%>%table() #1.610 NA

#Antes de pasar al criterio 4, es buena idea revisar cómo se distribuyen los datos de metros a ver si hay outliers
quantile(cali$new_surface_def, prob=seq(0.05,1,0.05), na.rm = TRUE) #Parece estar bien en general, pero algunos datos raros en los extremos
#Zoom al 5% de menor tamaño
quantile(cali$new_surface_def, prob=seq(0,0.05,0.001), na.rm = TRUE)
#Zoom al 5% de mayor tamaño
quantile(cali$new_surface_def, prob=seq(0.95,1,0.001), na.rm = TRUE)


#Los datos que sean mayores a 5000 en la definitiva y menores a 5000 en alguna de las "intermedias" se cambian
cali$new_surface_def <- ifelse(cali$new_surface_def > 5000 & cali$new_surface_num < 5000,cali$new_surface_num,cali$new_surface_def)
#Menos del 0.1% son mayores a 4500 m2 y la mayoría parece estar mal
#Se dejan esos como NA
cali$new_surface_def <- ifelse(cali$new_surface_def > 4500,NA,cali$new_surface_def)

#Ahora por abajo
#Solo el 1.5% de los datos son menores a 17m2 y es poco creible tener apartamentos o casas de ese tamaño
cali$new_surface_def <- ifelse(cali$new_surface_def < 17,NA,cali$new_surface_def)

#Se revisa de nuevo la distribución
quantile(cali$new_surface_def, prob=seq(0.05,1,0.05), na.rm = TRUE)
#Zoom al 5% de menor tamaño
quantile(cali$new_surface_def, prob=seq(0,0.05,0.001), na.rm = TRUE)
#Zoom al 5% de mayor tamaño
quantile(cali$new_surface_def, prob=seq(0.95,1,0.001), na.rm = TRUE)

max(cali$new_surface_def, na.rm = TRUE)

#revisión de NA
is.na(cali$new_surface_def)%>%table() #1.667

#Criterio número 4
#Los datos que quedan faltando por llenar se completan con vecinos espaciales

#Se crean las variables vacías
cali$metros_Def <- NA
cali$metros_Def2 <- NA
#se hace el join manual
cali$metros_Def[which(cali$property_id == cali_sf$property_id)] <- cali_sf$metros_Def[which(cali$property_id == cali_sf$property_id)]
cali$metros_Def2[which(cali$property_id == cali_sf$property_id)] <- cali_sf$metros_Def2[which(cali$property_id == cali_sf$property_id)]

#Se une primero con metros def que es la que aproxima el promedio de aquellos que tienen los mismos cuartos
cali$new_surface_def <- ifelse(is.na(cali$new_surface_def),cali$metros_Def,cali$new_surface_def)

#revisión de NA
is.na(cali$new_surface_def)%>%table() #182

#Se utiliza ahora metros def 2 que solo es vecinos espaciales
cali$new_surface_def <- ifelse(is.na(cali$new_surface_def),cali$metros_Def2,cali$new_surface_def)

#revisión de NA
is.na(cali$new_surface_def)%>%table() #18

# corroborar cuántos NAs quedan
sapply(cali, function(y) sum(length(which(is.na(y)))))

#Salvar y exportar la base
write_rds(cali, "TEST_BASE_CENSO_ORIGINALES_REGEX.rds")

#Solo mantener las variables definitivas
keep <- c("property_id", "city", "price", "bedrooms", "property_type", "operation_type",
          "geometry", "mean_H_NRO_CUARTOS2", "med_H_NRO_CUARTOS2", "sum_HA_TOT_PER2", 
          "med_V_TOT_HOG2", "mod_VA1_ESTRATO2", "mod_V_MAT_PARED2", "mod_V_MAT_PISO2",
          "mod_VE_RECBAS2", "Final_Bathrooms_2", "new_surface_def")

reducida <- cali[,(names(cali) %in% keep)]

sapply(reducida, function(y) sum(length(which(is.na(y)))))

#salvar la base reducida
write_rds(reducida, "TEST_BASE_CENSO_ORIGINALES_REGEX_reducida.rds")
