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
setwd('C:/Users/df.osorio11/Documents/diego')


# CARGAR TRAIN COMPLETA FINAL (original, censo, amenities)
train_completa <- readRDS('data/train_final.RDS')

# hacer prueba (sample) de la base de train
train <- sample_n(train_completa,500)


# CARGAR TRAIN COMPLETA FINAL (original, censo, amenities)
test_completa <- readRDS('data/test_final.RDS')

# hacer prueba (sample) de la base de train (PARA HACER PREDICCIONES CON EL MEJOR MODELO)
test <- sample_n(test_completa,100)


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

write_csv(df,"data/df_dummies.csv")


### SUPER-LEARNER:

### RANDOM FOREST
# crear 3 bases: train, validation, test
# split los datos de entrenamiento en train-set y test-set  80-20%
set.seed(100)
n <- nrow(train)
smp_size <- floor(0.8*n)
train_obs <- sample(floor(nrow(train)*0.8))

# crear X_train para ajustar/entrenar los modelos
X_train <- train[train_obs, ]

#train_ind <- sample(1:n, size = smp_size)
#Crear train set para ajustar los parámetros
#train_set <- train[train_ind, ]
#Crear test set para evaluar el modelo
#test_set <- train[-train_ind, ]

# split X_train en train-set y validation-set  80/20%
set.seed(100)
val_obs <- sample(floor(nrow(X_train)*0.2))


# crear X_train y X_val
X_val <- X_train[val_obs,]
X_train <- X_train[-val_obs,]


# crear X_test
X_test <- train[-train_obs, ]

# crear y 
y_train <- X_train[,"log_price"]
y_val <- X_val[,"log_price"]
y_test <- X_test[,"log_price"]



# quitar y de las bases de X (variables)
drop_logprice <- c("log_price")

X_train <- X_train[, (!names(X_train) %in% drop_logprice)]
X_val <- X_val[, (!names(X_val) %in% drop_logprice)]
X_test <- X_test[, (!names(X_test) %in% drop_logprice)]


#  unificar bases completas
train_modelos <- data.frame(y_train, X_train)
train_modelos$y_train <- as.numeric(train_modelos$y_train)

val_modelos <- data.frame(y_val, X_val)
val_modelos$y_val <- as.numeric(val_modelos$y_val)

test_modelos <- data.frame(y_test, X_test)
test_modelos$y_test <- as.numeric(test_modelos$y_test)

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
modelo_rf <- train(y_train~. -property_id,
                   data = train_modelos,
                   method = "ranger",
                   trControl= cv5,
                   metric = 'RMSE',
                   tuneGrid = tunegrid_rf)

modelo_rf
plot(modelo_rf)
rf_opt <- modelo_rf$bestTune

### HAY QUE CAMBIARLO CUANDO SE AJUSTE CON LA BASE COMPLETA
SL.rf.better = function(...){
  SL.randomForest(..., min.node.size = 3, mtry=6)
}


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
modelo_glmnet <- train(y_train~ .-property_id,
                       data = train_modelos,
                       method = "glmnet",
                       preProcess = c("center", "scale"),
                       trControl= cv5,
                       metric = 'RMSE',
                       tuneGrid = tunegrid_glmnet)

modelo_glmnet
plot(modelo_glmnet)
glmnet_opt <- modelo_glmnet$bestTune

### HAY QUE CAMBIARLO CUANDO SE AJUSTE CON LA BASE COMPLETA
SL.glmnet.better = function(...){
  SL.glmnet(..., alpha = 0.3333333, lambda = 0.06122449)
}


# XGBOOST

# crear K-Fold=5 de XGBoost
set.seed(100)
cv5_xgboost <- vfold_cv(data=train_modelos, v = 5, strata = y_train)

# set metricas de evaluacion
metricas <- metric_set(rmse,mae,rsq)

# pre procesamiento de los datos y funcion de modelo
xg_recipe <- recipe(formula= y_train~. ,
                    data = train_modelos)%>%
  update_role(property_id, new_role = "id") %>% # cambiar rol de variable property_id
  step_dummy(all_nominal_predictors() ) %>%
  step_nzv(all_predictors())


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


SL.xgb.better = function(...){
  SL.xgb(..., mtry = 7, tree =  89, min_n=2, tree_depth=4, learn_rate=  0.0755 , sample_size= 0.765   )  
}


### SUPER LEARNER
# crear matriz de dummies

base_sl <- train_modelos

# quitar y de las bases de X (variables)
drop_prop <- c("property_id", "y_train")


drop_cate <- c("property_id", "property_type", "y_train", "mod_VA1_ESTRATO2")

base_sl <- base_sl[, (!names(base_sl) %in% drop_prop)]
base_sl <- base_sl[, (!names(base_sl) %in% drop_cate)]


df_train <- model.matrix(~. -1, base_sl) %>%data.frame() 


X_train_scaled <- data.frame(scale(base_sl))

set.seed(100)

y_train <- train_modelos$y_train

cv_sl <- CV.SuperLearner(Y= y_train, X = X_train_scaled,
                         family = gaussian(),
                         cvControl = list(V = 5),
                         SL.library = c("SL.mean", "SL.rf.better" , "SL.glmnet",
                                        "SL.lm"),
                         control = list(saveFitLibrary = TRUE))
summary(cv_sl)


