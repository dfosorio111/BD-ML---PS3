### FUNCION

# funcion para convertir palabras en numeros

word2num <- function(word){
  
  # split sobre el patron de entrada
  wsplit <- strsplit(tolower(word)," ")[[1]]
  
  
  # crear diccionarios de orden de magnitud
  one_digits <- list(cero=0, uno=1, dos=2, tres=3, cuatro=4, cinco=5,
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
    
    # crear variable j=1
    j <- 1
    
    
    ### SI PATRON ES 100 O 1000
    # 1 palabra Y es cien=100
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
    
    # i es 
    if(i < length(wsplit) && wsplit[i+1]=="ciento"){
      if(i>1 && wsplit[i-1] %in% c("ciento","mil"))
        out <- out + 100*temp
      else
        out <- 100*(out + temp)
      j <- 2
    }
    else if(i < length(wsplit) && wsplit[i+1]=="mil"){
      if(i>1 && wsplit[i-1] %in% c("ciento","mil"))
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



word2num("ciento tres")