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

write_csv(df,"data/df_dummies_SL.csv")

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
price_test <- X_test$price


# quitar y de las bases de X (variables)
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


### SUPER-LEARNER:

# mirar modelos disponibles
listWrappers()["All"]

# Random Forest: 
# hiper parametros: num.trees (numero de arboles), mtry(sqrt(features) para ajustar), min.node.size()

# Elastic Ne: 
# hiper parametros: alpha (numero de arboles), nfolds (numero de folds), nlambda

SL.ranger
SL.glmnet
SL.xgboost
SL.randomForest

# Random Forest
tunegrid_rf <- data.frame(min.node.size= c(1,3,5,7,9,15),
                          mtry = ceiling(sqrt(ncol(X_val))- 2),
                          splitrule = "variance")
#splitrule = "gini")

# crear K-Fold particiones sobre la base para ajustar los modelos
set.seed(100)
cv5 <- trainControl(number = 5,
                    method = "cv")
#summaryFunction: CUSTOM FUNCTION del Error

# ajustar modelo de Rando Forest
# hiper parametros: metodo= ranger, trainControl=CV, metrica opt = 'RMSE'
modelo_rf <- train(y_train~. -1,
                   data = train_modelos,
                   method = "ranger",
                   trControl= cv5,
                   metric = 'RMSE',
                   tuneGrid = tunegrid_rf)

modelo_rf
plot(modelo_rf)
rf_opt <- modelo_rf$bestTune


SL.rf1 <- create.Learner("SL.ranger", list(mtry=6,splitrule='variance', min.node.size=3))


### HAY QUE CAMBIARLO CUANDO SE AJUSTE CON LA BASE COMPLETA
#SL.rf.better = function(...){
#  SL.randomForest(..., min.node.size = 3, mtry=6)
#}


#modelo_rf_opt <- finalize(tunegrid_rf, select(modelo_rf))

# # entrenar el modelo optimo
# modelo_rf_opt <- train(y_train~ .-y_train-property_id,
#                    data = train_modelos,
#                    method = "ranger",
#                    min.node.size = 5,
#                    mtry = 4,
# )

# crear vector de predicciones
#y_predict <- predict(modelo_rf, X_val)


# ELASTIC NET

# crear grid_search para elastic net
tunegrid_glmnet <- data.frame(alpha= seq(0,1,length.out=10),
                              lambda = seq(0,1,length.out=50) )

set.seed(100)

# crear modelo de Elastic Net
# hiper parametros: method: glmnet, preProcess centrar y escalar
# trainControl: CV-5Fold, metric optimizar: RMSE, tuneGrid = gridSearch
modelo_glmnet <- train(y_train~ .-1,
                       data = train_modelos,
                       method = "glmnet",
                       trControl= cv5,
                       metric = 'RMSE',
                       tuneGrid = tunegrid_glmnet)
# preProcess = c("center", "scale"),

modelo_glmnet
plot(modelo_glmnet)
glmnet_opt <- modelo_glmnet$bestTune

# crear SL de Elastic Net
SL.en <- create.Learner("SL.glmnet", list(alpha = 0.2222222, lambda =0.04081633))

### HAY QUE CAMBIARLO CUANDO SE AJUSTE CON LA BASE COMPLETA
#SL.glmnet.better = function(...){
#  SL.glmnet(..., alpha = 0.3333333, lambda = 0.06122449)
#}

# XGBOOST
# crear K-Fold=5 de XGBoost
set.seed(100)
cv5_xgboost <- vfold_cv(data=train_modelos, v = 5, strata = y_train)

# set metricas de evaluacion
metricas <- metric_set(rmse,mae,rsq)

# pre procesamiento de los datos y funcion de modelo
xg_recipe <- recipe(formula= y_train~. ,
                    data = train_modelos)%>%
  # cambiar rol de variable property_id
  step_dummy(all_nominal_predictors() ) %>%
  step_nzv(all_predictors())

# update_role(property_id, new_role = "id") %>% 

# crear modelo XGBoost con parámetros a sintonizar
modelo_xgboost <- boost_tree(trees= tune(),
                             tree_depth = tune(),
                             min_n = tune(),
                             mtry = tune(),
                             sample_size = tune(),
                             learn_rate = tune() ) %>%
  set_engine("xgboost") %>%
  set_mode("regression")


# crear work-flow: compactar el pre procesamiento de recipie y el modelo a tunnear
xgb_wf <- workflow(xg_recipe, modelo_xgboost)

# sintonización de hiper parametros
xgb_grid <- grid_max_entropy(tree_depth(c(1L,5L)), # 5-8
                             trees(c(50,100)), # 300-700
                             min_n(c(1L,3L)), #0-5-1% de observaciones
                             sample_prop(c(0.5,0.8) ),
                             mtry(c(6,8)), #sqrt(features)
                             learn_rate (c(-2,-1)),
                             size = 20)
xgb_grid

xgb <- tune_race_anova(object = xgb_wf,
                       resamples = cv5_xgboost,
                       grid = xgb_grid,
                       metrics = metric_set(rmse,mae,rsq),
                       control = control_race(verbose_elim = TRUE) )

xgb
plot_race(xgb)
xgb_opt <- show_best(xgb) 

# FALTA METER PARAMETROS DE XGBOOST OPTIMOS
#SL.forest2 <- create.Learner("SL.randomForest", list(ntree = 100))


#SL.xgb.better = function(...){
#  SL.xgb(..., mtry = 7, tree =  89, min_n=2, tree_depth=4, learn_rate=  0.0755 , sample_size= 0.765   )  
#}

### SUPER LEARNER
# crear matriz de dummies
# set the seed for reproducibility
set.seed(123)

# Un SL que tiene RandomForest, Elastic Net, XGBoost
### FALTA AGREGAR EL  SL.xgb$names de XGBOOST
sl.lib <- c(SL.rf1$names, SL.en$names, "SL.lm")

fitY <- SuperLearner(Y = y_train, X = data.frame(train_s),
                     method = "method.NNLS", SL.library = sl.lib)

fitY

