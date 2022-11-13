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
       tidymodels,finetune)

p_load(tidyverse, SuperLearner, caret, glmnet, randomForest, RhpcBLASctl, MLmetrics)

# establecer directorio
setwd('C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS3/scripts/diego')

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


# CARGAR TRAIN COMPLETA FINAL (original, censo, amenities)
train_completa <- readRDS('data/train_final.RDS')

# hacer prueba (sample) de la base de train
train <- sample_n(train_completa,500)

# hacer los modelos con la base completa
train <- train_completa

# CARGAR TRAIN COMPLETA FINAL (original, censo, amenities)
test_completa <- readRDS('data/test_final.RDS')

# hacer prueba (sample) de la base de train (PARA HACER PREDICCIONES CON EL MEJOR MODELO)
test <- sample_n(test_completa,100)

# hacer los modelos con la base completa
test <- test_completa

# quitar city (?)
#test <- merge(test_original_censo, test_amenities, by = 'property_id')

# crear variable del logaritmo del precio de las casas
train$log_price <- log(train$price)
test$log_price <- log(test$price)


# quitar variables que no aportan a la construccion del modelo
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

write_csv(df,"data/df_dummies_lm.csv")


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

# quitar y de las bases de X (variables)
drop_logprice <- c("log_price")

X_train <- X_train[, (!names(X_train) %in% drop_logprice)]
X_val <- X_val[, (!names(X_val) %in% drop_logprice)]
X_test <- X_test[, (!names(X_test) %in% drop_logprice)]

# guardar variable de precio real del train
price_train <- X_train$price
price_val <- X_val$price
price_test <- X_test$price

# quitar price de las bases de X (variables)
drop_price <- c("price")

X_train <- X_train[, (!names(X_train) %in% drop_price)]
X_val <- X_val[, (!names(X_val) %in% drop_price)]
X_test <- X_test[, (!names(X_test) %in% drop_price)]


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
#train_modelos$y_train <- as.numeric(train_modelos$y_train)

val_modelos <- data.frame(y_val, val_s)
#val_modelos$y_val <- as.numeric(val_modelos$y_val)

test_modelos <- data.frame(y_test, test_s)
#test_modelos$y_test <- as.numeric(test_modelos$y_test)


### REGRESION LINEAL

# ajustar/entrenar modelos con train-set
# quitar la variable price
lin_reg <- lm(formula = y_train~ property_typeCasa +police + parking + cinema_d+Final_Bathrooms_2+ mod_VA1_ESTRATO26+ cinema+ marketplace+ kindergarten + mod_VA1_ESTRATO23, data = train_s)
summary(lin_reg)

y_predict_train <- predict(lin_reg, train_s)
y_predict_val <- predict(lin_reg, val_s)
y_predict_test <- predict(lin_reg, test_s)


# Predictores (coeficientes)
lin_reg_coeff <- lin_reg$coefficients %>%
  enframe(name = "predictor", value = "coeficiente")

lin_reg_coeff %>%
  filter(predictor != "`(Intercept)`") %>%
  ggplot(aes(x = reorder(predictor, abs(coeficiente)), 
             y = coeficiente)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  labs(title = "Coeficientes de Modelo Lin. Reg", 
       x = "Variables",
       y = "Coeficientes") +
  theme_bw()

#Resultados:
res_lin_reg <-  as.data.frame(cbind(price_val,mean(price_val)) )
# crear variable de prediccion de precio (exp(valor prediccion))
res_lin_reg <- res_lin_reg%>%mutate(y_predict_val = exp(y_predict_val))
colnames(res_lin_reg) <- c("Real", "Promedio", "Prediccion")
res_lin_reg$error <- res_lin_reg$Real- res_lin_reg$Prediccion
res_lin_reg$fun_error <- ifelse(res_lin_reg$error>0, res_lin_reg$error^2, ifelse(res_lin_reg$error<-40000000,res_lin_reg$error^6,res_lin_reg$error))
rmse <- sqrt( mean(res_lin_reg$error^2))
mae <- mean(abs(res_lin_reg$error^2))
avg <- mean(res_lin_reg$fun_error)

metricas_lm <- data.frame(Modelo = "Regresión",
                            AVG = avg,
                            RMSE = rmse,
                            MAE = mae,
                            coeficientes = lin_reg$coefficients
)

# GUARDAR MODELO DE LIN REG
write_rds(lin_reg,'lin_reg.RDS')
# CARGAR MODELO DE LIN REG
lin_reg <- readRDS('lin_reg.RDS')

# GUARDAR DATAFRAME DE RESULTADOS LIN REG
write_rds(res_lin_reg,'resultados_lm.RDS')
# CARGAR DATAFRAME DE RESULTADOS LIN REG
res_lin_reg <- readRDS('resultados_lm.RDS')


# GUARDAR METRICAS DE EVALUACION EN VALIDATION-SET
write_rds(metricas_lm,'metricas_lm.RDS')
# CARGAR DATAFRAME DE RESULTADOS LIN REG
metricas_lm <- readRDS('metricas_lm.RDS')
