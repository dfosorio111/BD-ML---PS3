### MODELOS

# limpiar ambiente
rm(list=ls())

# instalar paquetes
install.packages("pacman")

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

p_load(tidyverse,rio,glue,
       hexbin,
       patchwork,vip, ## plot: 
       ggrepel, ## plot: geom_text_repel
       stringi,tidytext,stopwords, ## text-data
       tidymodels,finetune,ranger)

p_load(simstudy, tidyverse, SuperLearner, caret, glmnet, randomForest, RhpcBLASctl, MLmetrics)

# establecer directorio
setwd('C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS3/scripts/diego')


# CARGAR TRAIN COMPLETA FINAL (original, censo, amenities)
train_completa <- readRDS('data/train_final.RDS')

# hacer prueba (sample) de la base de train
train <- sample_n(train_completa,500)

# cargar datos completos
train <- train_completa

# CARGAR TRAIN COMPLETA FINAL (original, censo, amenities)
test_completa <- readRDS('data/test_final.RDS')

# hacer prueba (sample) de la base de train (PARA HACER PREDICCIONES CON EL MEJOR MODELO)
test <- sample_n(test_completa,100)

# cargar datos completos
test <- test_completa

# quitar city (?)
#test <- merge(test_original_censo, test_amenities, by = 'property_id')

# crear variable del logaritmo del precio de las casas
train$log_price <- log(train$price)
test$log_price <- log(test$price)

# quitar variables
#train <- train[-c(2,3,6,7,18)]
drop <- c("city", "operation_type", "geometry.x" ,"geometry.y", "geometry",
          "mod_V_MAT_PARED2", "mod_V_MAT_PISO2","mod_VE_RECBAS2")

train <- train[, (!names(train) %in% drop)]
test <- test[, (!names(test) %in% drop)]


# ver correlacion de variables
#cor(train, method = c(""))

# extraer variables categoricas
names(train)

categoricas <- c("property_type","mod_VA1_ESTRATO2")

# convertir variables categoricas en factores
# iterar en lista de variables categoricas
for (var in categoricas) {
  train[,var] <- as.factor(train[,var, drop = TRUE])
}

# crear matriz de dummies con las variables categoricas
df <- model.matrix(~. -property_id-1, train) %>%data.frame()

write_csv(df,"data/df_dummies_RF.csv")
# Dividimos train/test (80/20)
# split los datos de entrenamiento en train-set y test-set

# crear 3 bases: train, validation, test
# split los datos de entrenamiento en train-set y test-set  80-20%
set.seed(100)
n <- nrow(df)
smp_size <- floor(0.8*n)
train_obs <- sample(floor(nrow(df)*0.8))

# crear X_train para ajustar/entrenar los modelos
X_train <- df[train_obs, ]

# split X_train en train-set y validation-set  80/20%
set.seed(100)
val_obs <- sample(floor(nrow(X_train)*0.2))

# crear X_train y X_val
X_val <- X_train[val_obs,]
X_train <- X_train[-val_obs,]

# crear X_test
X_test <- df[-train_obs, ]

# crear y 
y_train <- X_train[,"log_price"]
y_val <- X_val[,"log_price"]
y_test <- X_test[,"log_price"]

# guardar variable de precio real
price_train <- X_train$price
price_val <- X_val$price
price_test <- X_test$price# quitar y de las bases de X (variables)
drop_logprice <- c("price","log_price")

X_train <- X_train[, (!names(X_train) %in% drop_logprice)]
X_val <- X_val[, (!names(X_val) %in% drop_logprice)]
X_test <- X_test[, (!names(X_test) %in% drop_logprice)]

# lista de variables numericas
variables_numericas <- c("bedrooms","mean_H_NRO_CUARTOS2","med_H_NRO_CUARTOS2",
                         "sum_HA_TOT_PER2","med_V_TOT_HOG2","Final_Bathrooms_2","new_surface_def",  
                         "bank_d","bus_station_d","casino_d","childcare_d","cinema_d", 
                         "clinic_d","college_d",
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
                         "monastery","parking","police", "restaurant",
                         "dist_cbd1", "dist_cbd2" )

# Estandarizar VARIABLES NUMERICAS (CONTINUAS) DESPUES de hacer split de la base de datos 
# crear escalador
escalador <- preProcess(X_train[, variables_numericas])

train_s <- X_train
val_s <- X_val
test_s <- X_test

train_s[, variables_numericas] <- predict(escalador, X_train[, variables_numericas])
val_s[, variables_numericas] <- predict(escalador, X_val[, variables_numericas])
test_s[, variables_numericas] <- predict(escalador, X_test[, variables_numericas])

# convertir bases a dataframe
train_s <- data.frame(train_s)
val_s <- data.frame(val_s)
test_s <- data.frame(test_s)

X_train <- data.frame(X_train)
X_val <- data.frame(X_val)
X_test <- data.frame(X_test)


# variables de regresion
#y_train <- train_s[,"log_price"]
#y_test <-  test_s[,"log_price"]

#  unificar bases completas
train_modelos <- data.frame(y_train, train_s)
val_modelos <- data.frame(y_val, val_s)
test_modelos <- data.frame(y_test, test_s)

# Random Forest
tunegrid_rf <- data.frame(min.node.size= c(20,50),
                          mtry = c(7,10),
                          splitrule = "variance")
#splitrule = "gini")

# crear K-Fold particiones sobre la base para ajustar los modelos
set.seed(100)
cv5 <- trainControl(number = 5,
                    method = "cv", 
                    summaryFunction = custom_summary)
#summaryFunction: CUSTOM FUNCTION del Error

# ajustar modelo de Rando Forest
# hiper parametros: metodo= ranger, trainControl=CV, metrica opt = 'RMSE'
modelo_rf <- train(log_price~. -1,
                   data = train,
                   method = "ranger",
                   trControl= cv5,
                   metric = "anti_fiasco",
                   tuneGrid = tunegrid_rf)

# hiper parametros de modelo optimo
modelo_rf
plot(modelo_rf)
rf_opt <- modelo_rf$bestTune

# crear modelo de RandomForest
random_forest <- ranger(
  formula = y_train~.-1,
  data = train_modelos,
  num.trees = 1000,
  mtry = 7,
  splitrule = 'variance',
  min.node.size = 1,
)

# crear predicciones
y_predict_train <-as.data.frame(predict(random_forest, X_train))
y_predict_val <- as.data.frame(predict(random_forest, X_val))
y_predict_test <- as.data.frame(predict(random_forest, X_test))


#Resultados:
res_rf <-  as.data.frame(cbind(price_val,mean(price_val)) )
# crear variable de prediccion de precio (exp(valor prediccion))
res_rf <- res_rf%>%mutate(y_predict_val = exp(y_predict_val))
colnames(res_rf) <- c("Real", "Promedio", "Prediccion")


res_rf$error <- res_rf$Real- res_rf$Prediccion
res_rf$fun_error <- ifelse(res_rf$error>0, res_rf$error^2, ifelse(res_rf$error<(-40000000),res_rf$error^6,res_rf$error))

rmse <- sqrt( mean(res_rf$error^2))
mae <- mean(abs(res_rf$error^2))
avg <- mean(res_rf$fun_error)

metricas_rf <- data.frame(Modelo = "Random Forest",
                          AVG = avg,
                          RMSE = rmse,
                          MAE = mae,
                          coeficientes = lin_reg$coefficients
)

# GUARDAR MODELO DE LIN REG
write_rds(random_forest,'random_forest.RDS')
# CARGAR MODELO DE LIN REG
random_forest <- readRDS('random_forest.RDS')

# GUARDAR DATAFRAME DE RESULTADOS LIN REG
write_rds(res_rf,'resultados_rf.RDS')
# CARGAR DATAFRAME DE RESULTADOS LIN REG
res_rf <- readRDS('resultados_rf.RDS.RDS')


# GUARDAR METRICAS DE EVALUACION EN VALIDATION-SET
write_rds(metricas_rf,'metricas_rf.RDS')
# CARGAR DATAFRAME DE RESULTADOS LIN REG
metricas_rf <- readRDS('metricas_rf.RDS')


