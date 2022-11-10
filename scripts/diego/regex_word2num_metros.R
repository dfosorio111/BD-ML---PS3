### REGEX CON WORD2NUM PARA METROS

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

# BASE SAMPLE

#Este es el directorio de Daniel
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning/BD-ML---PS3/data")
houses_inter1 <- readRDS("train.rds")
BDT <- readrd("BaseGeografía.rds")


BDT <- as.data.frame(BDT)

prueba <- houses_inter1[1:1000,]


# crear variables de patrones
prueba$new_surface1 <- NA
prueba$new_surface2 <- NA
prueba$new_surface3 <- NA
prueba$new_surface4 <- NA
prueba$new_surface5 <- NA

# variables del IF

prueba$new_surface1_1 <- NA
prueba$new_surface2_2 <- NA
prueba$new_surface3_3 <- NA
prueba$new_surface4_4 <- NA
prueba$new_surface5_5 <- NA

prueba$description <- tolower(prueba$description)

# iterar por sufijos de 'metros'
for (i in c( "metros", "m2","mt2", "m ","cuadrad","mtr", "mts",  "metrs", "meters","area", "área","espacio de")){
  
  # agregar variable new_surface# que extrae de la variable de descripcion el patron i
  prueba <- prueba %>% 
    mutate(new_surface1 = ifelse(is.na(new_surface1)==T,str_extract(string=description , pattern=paste0(x1,i)),new_surface1),
           new_surface2 = ifelse(is.na(new_surface2)==T,str_extract(string=description , pattern=paste0(x2,i)),new_surface2),
           new_surface3 = ifelse(is.na(new_surface3)==T,str_extract(string=description , pattern=paste0(x3,i)),new_surface3),
           new_surface4 = ifelse(is.na(new_surface4)==T,str_extract(string=description , pattern=paste0(x4,i)),new_surface4),
           new_surface5 = ifelse(is.na(new_surface5)==T,str_extract(string=description , pattern=paste0(x5,i)),new_surface5),
    )
}


## limpiar variables de patrones i extraidos
for (i in c("m2","mt2", "m ", "metros","cuadrad","mtr", "mts", "metrs", "meters","area", "área","espacio de",  "\n\n")){
  prueba$new_surface1 <- str_trim(gsub(i,"",prueba$new_surface1))
  prueba$new_surface2 <- str_trim(gsub(i,"",prueba$new_surface2))
  prueba$new_surface3 <- str_trim(gsub(i,"",prueba$new_surface3))
  prueba$new_surface4 <- str_trim(gsub(i,"",prueba$new_surface4))
  prueba$new_surface5 <- str_trim(gsub(i,"",prueba$new_surface5))
}



### TRY CATCH:

for (i in 1:nrow(prueba)) {
  
  fine <- FALSE
  
  tryCatch(               
    
    # Specifying expression
    expr = {              
      
      word2num(prueba$new_surface1[i])
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
    
    prueba$new_surface1_1[i] <- as.numeric(word2num(prueba$new_surface1[i])[[2]])
    
  }
  
}



for (i in 1:nrow(prueba)) {
  
  fine <- FALSE
  
  tryCatch(               
    
    # Specifying expression
    expr = {              
      
      word2num(prueba$new_surface2[i])
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
    
    prueba$new_surface2_2[i] <- as.numeric(word2num(prueba$new_surface2[i])[[2]])
    
  }
  
}



for (i in 1:nrow(prueba)) {
  
  fine <- FALSE
  
  tryCatch(               
    
    # Specifying expression
    expr = {              
      
      word2num(prueba$new_surface3[i])
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
    
    prueba$new_surface3_3[i] <- as.numeric(word2num(prueba$new_surface3[i])[[2]])
    
  }
  
}


for (i in 1:nrow(prueba)) {
  
  fine <- FALSE
  
  tryCatch(               
    
    # Specifying expression
    expr = {              
      
      word2num(prueba$new_surface4[i])
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
    
    prueba$new_surface4_4[i] <- as.numeric(word2num(prueba$new_surface4[i])[[2]])
    
  }
  
}


for (i in 1:nrow(prueba)) {
  
  fine <- FALSE
  
  tryCatch(               
    
    # Specifying expression
    expr = {              
      
      word2num(prueba$new_surface5[i])
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
    
    prueba$new_surface5_5[i] <- as.numeric(word2num(prueba$new_surface5[i])[[2]])
    
  }
  
}



# crear variable new_surfaceFinal
prueba$new_surfaceFinal <- NA


# iterar sobre las filas 
for (i in 1:nrow(prueba)) {
  
  # convertir variables de texto con NA a -1
  if (is.na(prueba$new_surface1_1[i])==TRUE) {
    prueba$new_surface1_1[i] = -1
  }
  if (is.na(prueba$new_surface2_2[i])==TRUE) {
    prueba$new_surface2_2[i] = -1
  }
  if (is.na(prueba$new_surface3_3[i])==TRUE) {
    prueba$new_surface3_3[i] = -1
  }
  if (is.na(prueba$new_surface4_4[i])==TRUE) {
    prueba$new_surface4_4[i] = -1
  }
  if (is.na(prueba$new_surface5_5[i])==TRUE) {
    prueba$new_surface5_5[i] = -1
  }
  
  #prueba$new_surfaceFinal[i] <-max(prueba$new_surface1_1[i],prueba$new_surface2_2[i],prueba$new_surface3_3[i],prueba$new_surface4_4[i],prueba$new_surface5_5[i])
  
  if (prueba$new_surface1_1[i]>prueba$new_surface2_2[i]) {
    prueba$new_surfaceFinal[i] <- prueba$new_surface1_1[i]
  }else if (prueba$new_surface2_2[i]>prueba$new_surface3_3[i]) {
    prueba$new_surfaceFinal[i] <- prueba$new_surface2_2[i]
  }else if (prueba$new_surface3_3[i]>prueba$new_surface4_4[i]) {
    prueba$new_surfaceFinal[i] <- prueba$new_surface3_3[i]
  }else if (prueba$new_surface4_4[i]>prueba$new_surface5_5[i]) {
    prueba$new_surfaceFinal[i] <- prueba$new_surface4_4[i]
  }else{
    prueba$new_surfaceFinal[i] <- prueba$new_surface5_5[i]
  }
  
}

# mutate para actualizar la variable new_surfaceFinal -1 con NAs 
prueba <- prueba%>%mutate(new_surfaceFinal = ifelse(new_surfaceFinal==-1,NA,new_surfaceFinal))


# corroborar cuántos NAs quedan
sapply(prueba, function(y) sum(length(which(is.na(y)))))

