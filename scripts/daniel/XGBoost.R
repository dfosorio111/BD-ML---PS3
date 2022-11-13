# XGBOOST COMPLETO

require(pacman)

p_load(tidyverse,rio,glue,
       hexbin,
       patchwork,vip, ## plot: 
       ggrepel, ## plot: geom_text_repel
       stringi,tidytext,stopwords, ## text-data
       tidymodels,finetune) 

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

p_load(tidyverse, rvest, data.table, dplyr, skimr, caret, rio, 
       vtable, stargazer, ggplot2, boot, MLmetrics, lfe,
       tidyverse, fabricatr, stargazer, Hmisc, writexl, viridis, here,
       modelsummary, # tidy, msummary
       gamlr,        # cv.gamlr
       ROCR, # ROC curve
       class, glmnet, janitor, doParallel, rattle, fastDummies, tidymodels, themis, AER, randomForest,xgboost, ranger)



#Directorio Universidad
setwd("C:/Users/de.franco/Desktop/R")

#Directorio Daniel
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning/BD-ML---PS3/data")


#importar la base del train
base <- read_rds("train_final.rds")

#Convertir en factores las variables categóricas
base$property_type <- as.factor(base$property_type)
base$mod_VA1_ESTRATO2 <- as.factor(base$mod_VA1_ESTRATO2)


n_cores <- detectCores()
cl <- makePSOCKcluster(n_cores-1)
registerDoParallel(cl)

set.seed(1000)

n <- floor(0.8*nrow(base))
t1 <- sample(1:n, size=n)

train <- base[t1,]

test <- base[-t1,]

#Base definitiva


train <- train %>% mutate(log_price = log(price))
test <- test %>% mutate(log_price = log(price))

drop2 <- c("city", "operation_type", "geometry", 
           "mod_V_MAT_PARED2", "mod_V_MAT_PISO2", "mod_VE_RECBAS2")

train2 <- train[,(!names(train) %in% drop2)]
test2 <- test[,(!names(test) %in% drop2)]



drop <- c("city", "operation_type", "geometry", 
          "mod_V_MAT_PARED2", "mod_V_MAT_PISO2", "mod_VE_RECBAS2", "price",
          "property_id")

train <- train[,(!names(train) %in% drop)]
test <- test[,(!names(test) %in% drop)]



## set n-folds
set.seed(234)
db_folds <- vfold_cv(data=train, v=5 , strata=log_price)
db_folds


db_recipe <- recipe(formula=log_price ~ . , data=train) %>% ## En recip se detallan los pasos que se aplicarán a un conjunto de datos para prepararlo para el análisis de datos.
  update_role(property_id, new_role = "id") %>%## cambiar role para variable id
  step_dummy(all_nominal_predictors()) %>%
  step_nzv(all_predictors())
db_recipe




## Boosted Tree Model Specification
xgb_spec <- boost_tree(trees = tune(),
                       tree_depth = tune(),
                       mtry = tune(),
                       sample_size = tune(),
                       learn_rate = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")
xgb_spec

## workflow
xgb_word_wf <- workflow(db_recipe, xgb_spec)

#Griilita
xgb_grid <- grid_max_entropy(trees(c(100L, 200L)),
                             tree_depth(c(5L, 8L)),
                             mtry(c(5L, 9L)), 
                             sample_prop(c(0.5, 1.0)), 
                             learn_rate(c(-2, -1)),
                             size = 20)
xgb_grid

## estimate model


######################################Barra de progreso
n_iter <- 20 # Number of iterations of the loop

# Initializes the progress bar
pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

for(i in 1:n_iter) {
  
  #---------------------
  # Code to be executed
  #---------------------
  
  xgb_word_rs <- tune_race_anova(object = xgb_word_wf,
                                 resamples = db_folds,
                                 grid = xgb_grid,
                                 metrics = metric_set(rmse),
                                 control = control_race(verbose_elim = T))
  
  #---------------------
  
  # Sets the progress bar to the current state
  setTxtProgressBar(pb, i)
}

close(pb) # Close the connection

#empezó a correr a las 5:26




## plot model
plot_race(xgb_word_rs)

## best model
show_best(xgb_word_rs)

#Predecir con el xgboost

#Guardar el modelo
write_rds(xgb_word_rs, "Modelo_XGBoost.rds")

#Cargar el modelo
Modelo_XG <- read_rds("Modelo_XGBoost.rds")

#mostrar los mejores
show_best(Modelo_XG)

## xgboost model
xgb_last <- xgb_word_wf %>%
  finalize_workflow(select_best(xgb_word_rs, "rmse"))
xgb_last

## min log loss
predictions <- collect_predictions(xgb_last)

## predictons vs truht value
predictions <- collect_predictions(xgb_last) %>%
  conf_mat(price, .pred_class)
predictions


#Predicciones del XGBoost
predicciones <- predict(xgb_last, train_def)


#########xgboost magistral

require("xgboost")
grid_default <- expand.grid(nrounds = c(3000),
                            max_depth = c(5,8),
                            eta = c(0.001),
                            gamma = c(0,1),
                            min_child_weight = c(100),
                            colsample_bytree = c(0.7),
                            subsample = c(0.6))
set.seed(1410)
#xgboost <- train(
#  log_price ~ . - property_id,
#  data = train,
#  method = "xgbTree",
#  trControl = ctrl,
#  metric = "RMSE",
#  tuneGrid = grid_default,
#  preProcess = c("center", "scale")
#)


ctrl <- trainControl(method = "cv", number = 5)

######################################Barra de progreso
n_iter <- 10 # Number of iterations of the loop

# Initializes the progress bar
pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

for(i in 1:n_iter) {
  
  #---------------------
  # Code to be executed
  #---------------------
  
  set.seed(1410)
  xgboost <- train(
    log_price ~ . ,
    data = train,
    method = "xgbTree",
    trControl = ctrl,
    metric = "RMSE",
    tuneGrid = grid_default,
    preProcess = c("center", "scale")
  )
  #---------------------
  
  # Sets the progress bar to the current state
  setTxtProgressBar(pb, i)
}

close(pb) # Close the connection



#####################################################Modelo con métrica personalizada


#Se construye la métrica personalizada
custom_summary <- function(data, lev = NULL, model = NULL){
  out = mean(ifelse((exp(data[, "pred"])-exp(data[, "obs"]))/1000000 > 0, (exp(data[, "pred"])-exp(data[, "obs"]))^2, 
                    ifelse((exp(data[, "pred"])-exp(data[, "obs"]))/1000000 < (-40) ,(exp(data[, "pred"])-exp(data[, "obs"]))^6 ,(exp(data[, "pred"])-exp(data[, "obs"])))) ) 
  names(out) = c("anti_fiasco")
  out
}


####Se usa un control que incorpore la métrica
ctrl <- trainControl(method = "cv",  
                     number = 2,     
                     summaryFunction = custom_summary)

#Se construye la grilla
grid_default <- expand.grid(nrounds = c(2000),
                            max_depth = c(5, 10),
                            eta = c(0.01, 0.05),
                            gamma = c(0,1),
                            min_child_weight = c(50,100),
                            colsample_bytree = c(0.7),
                            subsample = c(0.6))

#Se corre el modelo
#modelo
set.seed(1410)
xgboost <- train(
  log_price ~ . ,
  data = train,
  method = "xgbTree",
  trControl = ctrl,
  metric = "anti_fiasco",
  tuneGrid = grid_default,
  preProcess = c("center", "scale"),
  maximize = FALSE
)

write_rds(xgboost, "modelo_xgboost_custom.rds")


#######################################################Predicciones


write_rds(xgboost, "modelo_xgboost_con_1000.rds")
mod_xgboost <- read_rds("modelo_xgboost_con_1000.rds")



##########################################predict (con 1000 árboles)

#Test
pred_xgb_test2 <- predict(mod_xgboost, test2)
#Train
pred_xgb_train2 <- predict(mod_xgboost, train2)

test2$predicciones_XG_1000 <- exp(pred_xgb_test2)
View(test2%>%dplyr::select(price, predicciones_XG_1000))

#Se crea el error en pesos
test2 <- test2%>%mutate(error_XG_1000 = (predicciones_XG_1000 - price)/1000000)%>%mutate(fun_error_XG_1000 = ifelse(error_XG_1000 > 0, error_XG_1000^2,ifelse(error_XG_1000<(-40),error_XG_1000^6,error_XG_1000)))
View(test2%>%dplyr::select(price, predicciones_XG_1000, error_XG_1000, fun_error_XG_1000))
AVG <- mean(test2$fun_error_XG_1000)
AVG 
RMSE <- sqrt(mean(test2$error_XG_1000^2))
RMSE
MAE <- mean(abs(test2$error_XG_1000))
MAE
hist(test2$error_XG_1000)

quantile(test2$error_XG_1000, probs = seq(0,1,0.01))

#Cantidad de casas que no se compran
test2%>%mutate(no_compra = ifelse(error_XG_1000<(-40),1,0))%>%count(no_compra)%>%mutate(perc = 100*n/sum(n) )

#Presupuesto de las compras
test2%>%subset(error_XG_1000>=(-40))%>%summarise(Presup = -sum(error_XG_1000))

#Cargar el de 1500 árboles
mod_xgboost_1500 <- read_rds("XGB_1500.rds")

##########################################predict (con 1500 árboles)

#Test
pred_xgb_test2_1500 <- predict(mod_xgboost_1500, test2)
#Train
pred_xgb_train2_1500 <- predict(mod_xgboost_1500, train2)

test2$predicciones_XG_1500 <- exp(pred_xgb_test2_1500)
View(test2%>%dplyr::select(price, predicciones_XG_1000 ,predicciones_XG_1500))

#Se crea el error en pesos
test2 <- test2%>%mutate(error_XG_1500 = (predicciones_XG_1500 - price)/1000000)%>%mutate(fun_error_XG_1500 = ifelse(error_XG_1500 > 0, error_XG_1500^2,ifelse(error_XG_1500<(-40),error_XG_1500^6,error_XG_1500)))
View(test2%>%dplyr::select(price, predicciones_XG_1500, error_XG_1500, fun_error_XG_1500))
AVG_1500 <- mean(test2$fun_error_XG_1500)
AVG_1500 
RMSE_1500 <- sqrt(mean(test2$error_XG_1500^2))
RMSE_1500
MAE_1500 <- mean(abs(test2$error_XG_1500))
MAE_1500
hist(test2$error_XG_1500)

quantile(test2$error_XG_1500, probs = seq(0,1,0.01))

#Cantidad de casas que no se compran
test2%>%mutate(no_compra = ifelse(error_XG_1500<(-40),1,0))%>%count(no_compra)%>%mutate(perc = 100*n/sum(n) )

#Presupuesto de las compras
test2%>%subset(error_XG_1500>=(-40))%>%summarise(Presup = -sum(error_XG_1500))



#Cargar el de 2000 árboles
mod_xgboost_2000 <- read_rds("XGB_2000.rds")

##########################################predict (con 2000 árboles)

#Test
pred_xgb_test2_2000 <- predict(mod_xgboost_2000, test2)
#Train
pred_xgb_train2_2000 <- predict(mod_xgboost_2000, train2)

test2$predicciones_XG_2000 <- exp(pred_xgb_test2_2000)
View(test2%>%dplyr::select(price, predicciones_XG_1000 ,predicciones_XG_2000))

#Se crea el error en pesos
test2 <- test2%>%mutate(error_XG_2000 = (predicciones_XG_2000 - price)/1000000)%>%mutate(fun_error_XG_2000 = ifelse(error_XG_2000 > 0, error_XG_2000^2,ifelse(error_XG_2000<(-40),error_XG_2000^6,error_XG_2000)))
View(test2%>%dplyr::select(price, predicciones_XG_2000, error_XG_2000, fun_error_XG_2000))
AVG_2000 <- mean(test2$fun_error_XG_2000)
AVG_2000 
RMSE_2000 <- sqrt(mean(test2$error_XG_2000^2))
RMSE_2000
MAE_2000 <- mean(abs(test2$error_XG_2000))
MAE_2000
hist(test2$error_XG_2000)

quantile(test2$error_XG_2000, probs = seq(0,1,0.01))

#Cantidad de casas que no se compran
test2%>%mutate(no_compra = ifelse(error_XG_2000<(-40),1,0))%>%count(no_compra)%>%mutate(perc = 100*n/sum(n) )

#Presupuesto de las compras
test2%>%subset(error_XG_2000>=(-40))%>%summarise(Presup = -sum(error_XG_2000))

#Cargar el modelo con error personalizado
mod_xgb_custom <- read_rds("modelo_xgboost_custom.rds")

##########################################predict (Customized)



#Test
pred_xgb_test2_custom <- predict(mod_xgb_custom, test2)
#Train
pred_xgb_train2_custom <- predict(mod_xgb_custom, train2)

test2$predicciones_XG_custom <- exp(pred_xgb_test2_custom)
View(test2%>%dplyr::select(price, predicciones_XG_2000 ,predicciones_XG_custom))

#Se crea el error en pesos
test2 <- test2%>%mutate(error_XG_custom = (predicciones_XG_custom - price)/1000000)%>%mutate(fun_error_XG_custom = ifelse(error_XG_custom > 0, error_XG_custom^2,ifelse(error_XG_custom<(-40),error_XG_custom^6,error_XG_custom)))
View(test2%>%dplyr::select(price, predicciones_XG_custom, error_XG_custom, fun_error_XG_custom))
AVG_custom <- mean(test2$fun_error_XG_custom)
AVG_custom 
RMSE_custom <- sqrt(mean(test2$error_XG_custom^2))
RMSE_custom
MAE_custom <- mean(abs(test2$error_XG_custom))
MAE_custom
hist(test2$error_XG_custom)

quantile(test2$error_XG_custom, probs = seq(0,1,0.01))

#Cantidad de casas que no se compran
test2%>%mutate(no_compra = ifelse(error_XG_custom<(-40),1,0))%>%count(no_compra)%>%mutate(perc = 100*n/sum(n) )

#Presupuesto de las compras
test2%>%subset(error_XG_custom>=(-40))%>%summarise(Presup = -sum(error_XG_custom))


###########Modelos RF con Ranger
model <- ranger(
  formula         = log_price ~ ., 
  data            = train, 
  num.trees       = 200,
  mtry            = 9,
  max.depth       = 10,
  sample.fraction = 1,
  min.node.size   = 50,
  seed            = 123, # Notese el seteo de la semilla
  #importance      = "impurity" 
)


write_rds(xgboost, "modelo_xgboost_con_3000.rds")
mod_xgboost <- read_rds("modelo_xgboost_con_3000.rds")





##########################################predict (RF 200)

#Test
pred_RF_test2_200 <- predict(model, test2)
#Train
pred_RF_train2_200 <- predict(model, train2)

test2$predicciones_RF_200 <- exp(pred_RF_test2_200$predictions)
View(test2%>%dplyr::select(price,predicciones_XG_2000, predicciones_RF_200))

#Se crea el error en pesos
test2 <- test2%>%mutate(error_RF_200 = (predicciones_RF_200 - price)/1000000)%>%mutate(fun_error_RF_200 = ifelse(error_RF_200 > 0, error_XG_2000^2,ifelse(error_RF_200<(-40),error_RF_200^6,error_RF_200)))
View(test2%>%dplyr::select(price, predicciones_RF_200, error_RF_200, fun_error_RF_200))
AVG_200 <- mean(test2$fun_error_RF_200)
AVG_200 
RMSE_200 <- sqrt(mean(test2$error_RF_200^2))
RMSE_200
MAE_200 <- mean(abs(test2$error_RF_200))
MAE_200
hist(test2$error_RF_200)

quantile(test2$error_RF_200, probs = seq(0,1,0.01))

#Cantidad de casas que no se compran
test2%>%mutate(no_compra = ifelse(error_RF_200<(-40),1,0))%>%count(no_compra)%>%mutate(perc = 100*n/sum(n) )

#Presupuesto de las compras
test2%>%subset(error_RF_200>=(-40))%>%summarise(Presup = -sum(error_RF_200))





#Toca quitar lo de los nucleos al final
stopCluster(cl)




#Cargar el test definitivo
test_def <- read_rds("test_final.rds")
names(test_def)

#Variables que son factores
test_def$property_type <- as.factor(test_def$property_type)
test_def$mod_VA1_ESTRATO2 <- as.factor(test_def$mod_VA1_ESTRATO2)

test_def%>%count(mod_VA1_ESTRATO2)

#Se reemplaza el estrato "9" por estarto 5 que es el valor más probable
test_def$mod_VA1_ESTRATO2[which(test_def$mod_VA1_ESTRATO2 == 9)] <- 5
test_def%>%count(mod_VA1_ESTRATO2)

# corroborar cuántos NAs quedan
sapply(test_def, function(y) sum(length(which(is.na(y)))))

#Llenar los NA
test_def$med_H_NRO_CUARTOS2[which(is.na(test_def$med_H_NRO_CUARTOS2) == TRUE)] <- median(test_def$med_H_NRO_CUARTOS2, na.rm = TRUE)
test_def$mean_H_NRO_CUARTOS2[which(is.na(test_def$mean_H_NRO_CUARTOS2) == TRUE)] <- mean(test_def$mean_H_NRO_CUARTOS2, na.rm = TRUE)
test_def$med_V_TOT_HOG2 [which(is.na(test_def$med_V_TOT_HOG2 ) == TRUE)] <- median(test_def$med_V_TOT_HOG2 , na.rm = TRUE)
test_def$sum_HA_TOT_PER2[which(is.na(test_def$sum_HA_TOT_PER2) == TRUE)] <- mean(test_def$sum_HA_TOT_PER2, na.rm = TRUE)
test_def$new_surface_def[which(is.na(test_def$new_surface_def) == TRUE)] <- median(test_def$new_surface_def, na.rm = TRUE)
test_def$Final_Bathrooms_2 [which(is.na(test_def$Final_Bathrooms_2 ) == TRUE)] <- median(test_def$Final_Bathrooms_2 , na.rm = TRUE)


#Predicción con el XGboost de 1000 árboles
predict_test_def_1000 <- predict(mod_xgboost, test_def)
test_def$predictions_XGB1000 <- exp(predict_test_def_1000)

#Predicción con el XGboost de 1500 árboles
predict_test_def_1500 <- predict(mod_xgboost_1500, test_def)
test_def$predictions_XGB1500 <- exp(predict_test_def_1500)

#Predicción con el XGboost de 2000 árboles
predict_test_def_2000 <- predict(mod_xgboost_2000, test_def)
test_def$predictions_XGB2000 <- exp(predict_test_def_2000)

#Predicción con el RF de 200 árboles
predict_test_def_200RF <- predict(model, test_def)
test_def$predictions_RF200 <- exp(predict_test_def_200RF$predictions)

#Predicción XGboost personalizado
predict_test_def_XGcustom <- predict(mod_xgb_custom, test_def)
test_def$predictions_XGcustom <- exp(predict_test_def_XGcustom)

#GUARDAR LA BASE
write_rds(test_def, "Test_con_predicciones.rds")
