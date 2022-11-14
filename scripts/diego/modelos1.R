### MODELOS

# limpiar ambiente
rm(list=ls())


# instalar paquetes
install.packages("packages")

require("pacman")

## llamar y/o instalar librerias
p_load(tidyverse,rio,skimr,viridis,
       dplyr,
       gstat, ## variogram
       sf, ## datos espaciales
       leaflet, ## visualizaciones
       nngeo, ## st_nn function
       spdep,
       osmdata, 
       tmaptools, ## geocodificar
       ggsn, ## map scale bar 
       osmdata,  ## Get OSM's data
       class,
       nngeo, ## st_nn function
       spdep, ## Construct neighbours list from polygon list 
       MASS) ## packages with census data

# cargar librerias
p_load(tidyverse, rvest, data.table, dplyr, skimr, caret, rio, 
       vtable, stargazer, ggplot2, boot, MLmetrics, lfe,
       tidyverse, fabricatr, stargazer, Hmisc, writexl, viridis, here,
       modelsummary, # tidy, msummary
       gamlr,        # cv.gamlr
       ROCR, # ROC curve
       class, glmnet, janitor, doParallel, rattle, fastDummies, tidymodels, themis, AER, randomForest,xgboost, ranger)

# establecer directorio
setwd('C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS3/scripts/diego/')

# cargar las bases
train_amenities <- as.data.frame( readRDS('data/train_am.RDS'))
test_amenities <-as.data.frame(readRDS('data/train_am.RDS') )

# cargar originales y censo
train_original_censo <- as.data.frame( readRDS('data/BASE_CENSO_ORIGINALES_REGEX_reducida.rds') )
#test_original_censo <- as.data.frame( readRDS('data/BASE_CENSO_ORIGINALES_REGEX_reducida.rds') )


# ver NAs de las bases
sapply(train_amenities, function(y) sum(length(which(is.na(y)))))

sapply(train_original_censo, function(y) sum(length(which(is.na(y)))))

# reemplazar NAs en la base de orginal-censo
train_original_censo <- train_original_censo %>% mutate(mean_H_NRO_CUARTOS2 = ifelse(is.na(mean_H_NRO_CUARTOS2)==TRUE,mean(train_original_censo$mean_H_NRO_CUARTOS2, na.rm=TRUE),mean_H_NRO_CUARTOS2))
train_original_censo <- train_original_censo %>% mutate(med_H_NRO_CUARTOS2 = ifelse(is.na(med_H_NRO_CUARTOS2)==TRUE,median(train_original_censo$med_H_NRO_CUARTOS2, na.rm=TRUE),med_H_NRO_CUARTOS2))
train_original_censo <- train_original_censo %>% mutate(sum_HA_TOT_PER2 = ifelse(is.na(sum_HA_TOT_PER2)==TRUE,mean(train_original_censo$sum_HA_TOT_PER2, na.rm=TRUE),sum_HA_TOT_PER2))
train_original_censo <- train_original_censo %>% mutate(med_V_TOT_HOG2 = ifelse(is.na(med_V_TOT_HOG2)==TRUE,median(train_original_censo$med_V_TOT_HOG2, na.rm=TRUE),med_V_TOT_HOG2))


train_original_censo <- train_original_censo%>%mutate(new_surface_def = ifelse(is.na(new_surface_def)==TRUE, mean(train_original_censo$new_surface_def, na.rm=TRUE),new_surface_def))

# escoger las bases
#train_original <- train_original_censo%>%dplyr::select(property_id, price,bedrooms,property_type, operation_type,geometry, Final_Bathrooms, metros_Def2 ,mean_H_NRO_CUARTOS2, med_H_NRO_CUARTOS2, sum_HA_TOT_PER2,med_V_TOT_HOG2, mod_VA1_ESTRATO2, mod_V_MAT_PARED2, mod_V_MAT_PISO2, mod_VE_RECBAS2)

# merge dataframes de orginial-censo y amenities
train <- merge(train_original_censo, train_amenities, by = 'property_id')

# quitar city (?)
train <- train[,-2]
#test <- merge(test_original_censo, test_amenities, by = 'property_id')



# ver correlacion de variables
#cor(train, method = c(""))


# crear variable del logaritmo del precio de las casas + 1
train$log_price <- log(train$price)

# extraer variables categoricas
names(train)

categoricas <- c("property_type")

# convertir variables categoricas en factores
# iterar en lista de variables categoricas
for (var in categoricas) {
  train[,var] <- as.factor(train[,var, drop = TRUE])
}

# drop columns
train <- train[-c(4,6,17)]


# crear matriz de dummies
df <- model.matrix(~. -1, train) %>%data.frame()

# write.csv(df,"df_dummies.csv")


# Dividimos train/test (80/20)

#Se establece semilla
set.seed(100)
n <- nrow(train)
smp_size <- floor(0.8*n)
train_ind <- sample(1:n, size = smp_size)
#Crear train set para ajustar los parámetros
train_set <- train[train_ind, ]
#Crear test set para evaluar el modelo
test_set <- train[-train_ind, ]




variables_numericas <- c("property_id","price","bedrooms",
                         "operation_type","mean_H_NRO_CUARTOS2","med_H_NRO_CUARTOS2",
                         "sum_HA_TOT_PER2","med_V_TOT_HOG2","mod_VA1_ESTRATO2", 
                         "mod_V_MAT_PARED2","mod_V_MAT_PISO2","mod_VE_RECBAS2",
                         "Final_Bathrooms_2", "new_surface_def", "bank_d",
                         "bus_station_d","casino_d","childcare_d",
                         "cinema_d","clinic_d","college_d",
                         "community_centre_d","conference_centre_d","dentist_d",
                         "doctors_d","events_d","fast_food_d",
                         "hospital_d","kindergarten_d","library_d",
                         "love_hotel_d","marketplace_d","monastery_d",
                         "parking_d","police_d","restaurant_d",
                         "bank","bus_station","casino",
                         "childcare","cinema","clinic",
                         "college","community_centre","conference_centre",
                         "dentist","doctors","events_venue",
                         "fast_food","hospital","kindergarten",
                         "library","love_hotel","marketplace",
                         "monastery","parking","police", "restaurant")

# crear escalador
escalador <- preProcess(train_set[, variables_numericas])

train_s <- train_set
test_s <- test_set

train_s[, variables_numericas] <- predict(escalador, train_set[, variables_numericas])
test_s[, variables_numericas] <- predict(escalador, test_set[, variables_numericas])

# convertir bases a dataframe
train_s <- data.frame(train_s)
test_s <- data.frame(test_s)
train_set <- data.frame(train_set)
test_set <- data.frame(test_set)



# variables de regresion
y_train <- train_s[,"log_price"]
y_test <-  test_s[,"log_price"]



# drop columns
train_s <- train_s[-c(4,6,17)]
test_s <- test_s[-c(4,6,17)]


### REGRESION LINEAL

### NO SIRVE
lin_reg <- lm(formula = log_price~. -price-operation_type, data = train_s)




### REGULARIZACIÓN(shrinkage): (LASSO L1 - RIDGE L2)




### RANDOM FOREST


