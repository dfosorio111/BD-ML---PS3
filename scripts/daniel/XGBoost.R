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
       class, glmnet, janitor, doParallel, rattle, fastDummies, tidymodels, themis, AER, randomForest,xgboost, ranger,
       spatialsample)



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
                     number = 5,     
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


#####################################################Modelo con métrica personalizada y espacial


#Se construye la métrica personalizada
custom_summary <- function(data, lev = NULL, model = NULL){
  out = mean(ifelse((exp(data[, "pred"])-exp(data[, "obs"]))/1000000 > 0, (exp(data[, "pred"])-exp(data[, "obs"]))^2, 
                    ifelse((exp(data[, "pred"])-exp(data[, "obs"]))/1000000 < (-40) ,(exp(data[, "pred"])-exp(data[, "obs"]))^6 ,(exp(data[, "pred"])-exp(data[, "obs"])))) ) 
  names(out) = c("anti_fiasco")
  out
}

base_original <- readRDS("train_final.rds")


base_original_sf <- st_as_sf(base_original)

base_original_sf_med <- base_original_sf%>%subset(city == "Medellín")

mini_med <- base_original_sf_med[1:500,]


class(mini_train_sf)

block_folds <- spatial_block_cv(mini_med, v = 5)
autoplot(block_folds)


drop2 <- c("city", "operation_type", "geometry", 
           "mod_V_MAT_PARED2", "mod_V_MAT_PISO2", "mod_VE_RECBAS2")

mini_med <- mini_med[,(!names(train) %in% drop2)]



drop <- c("city", "operation_type", "geometry", 
          "mod_V_MAT_PARED2", "mod_V_MAT_PISO2", "mod_VE_RECBAS2", "price",
          "property_id")

mini_med <- mini_med[,(!names(train) %in% drop)]



####Se usa un control que incorpore la métrica
ctrl <- trainControl(method = "cv",  
                     number = 5,     
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
  trControl = block_folds,
  metric = "RMSE",
  tuneGrid = grid_default,
  preProcess = c("center", "scale"),
  maximize = FALSE
)

write_rds(xgboost, "modelo_xgboost_custom.rds")



#####################################################Modelo con métrica personalizada (2da versión)


#Se construye la métrica personalizada
custom_summary <- function(data, lev = NULL, model = NULL){
  out = mean(ifelse(abs((exp(data[, "pred"])-exp(data[, "obs"]))/1000000) > 40, (exp(data[, "pred"])-exp(data[, "obs"]))^6,(exp(data[, "pred"])-exp(data[, "obs"])))) 
  names(out) = c("anti_fiasco2")
  out
}


####Se usa un control que incorpore la métrica
ctrl <- trainControl(method = "cv",  
                     number = 5,     
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
  metric = "anti_fiasco2",
  tuneGrid = grid_default,
  preProcess = c("center", "scale"),
  maximize = FALSE
)

write_rds(xgboost, "modelo_xgboost_custom2.rds")




#######################################################Predicciones


write_rds(xgboost, "modelo_xgboost_con_1000.rds")
mod_xgboost <- read_rds("modelo_xgboost_con_1000.rds")
#Se cargan los modelos de regresión lineal y de EN
model_lm <- read_rds("lin_reg.rds")
model_EN <- read_rds("modelo_en.rds")



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

write_rds(xgboost, "modelo_xgboost_con_3000.rds")
mod_xgboost_3000 <- read_rds("modelo_xgboost_con_3000.rds")

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


##########################################predict (con 3000 árboles)

#Test
pred_xgb_test2_3000 <- predict(mod_xgboost_3000, test2)
#Train
pred_xgb_train2_3000 <- predict(mod_xgboost_3000, train2)

test2$predicciones_XG_3000 <- exp(pred_xgb_test2_3000)
View(test2%>%dplyr::select(price, predicciones_XG_1000 ,predicciones_XG_3000))

#Se crea el error en pesos
test2 <- test2%>%mutate(error_XG_3000 = (predicciones_XG_3000 - price)/1000000)%>%mutate(fun_error_XG_3000 = ifelse(error_XG_3000 > 0, error_XG_3000^2,ifelse(error_XG_3000<(-40),error_XG_3000^6,error_XG_3000)))
View(test2%>%dplyr::select(price, predicciones_XG_3000, error_XG_3000, fun_error_XG_3000))
AVG_3000 <- mean(test2$fun_error_XG_3000)
AVG_3000 
RMSE_3000 <- sqrt(mean(test2$error_XG_3000^2))
RMSE_3000
MAE_3000 <- mean(abs(test2$error_XG_3000))
MAE_3000
hist(test2$error_XG_3000)

quantile(test2$error_XG_3000, probs = seq(0,1,0.01))

#Cantidad de casas que no se compran
test2%>%mutate(no_compra = ifelse(error_XG_3000<(-40),1,0))%>%count(no_compra)%>%mutate(perc = 100*n/sum(n) )

#Presupuesto de las compras
test2%>%subset(error_XG_3000>=(-40))%>%summarise(Presup = -sum(error_XG_3000))



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

test2%>%summarise(Presup = -sum(error_XG_custom))

#Cargar el modelo con error personalizado
mod_xgb_custom2 <- read_rds("modelo_xgboost_custom2.rds")

##########################################predict (Customized 2)



#Test
pred_xgb_test2_custom2 <- predict(mod_xgb_custom2, test2)
#Train
pred_xgb_train2_custom2 <- predict(mod_xgb_custom2, train2)

test2$predicciones_XG_custom2 <- exp(pred_xgb_test2_custom2)
View(test2%>%dplyr::select(price, predicciones_XG_2000 ,predicciones_XG_custom2))

#Se crea el error en pesos
test2 <- test2%>%mutate(error_XG_custom2 = (predicciones_XG_custom2 - price)/1000000)%>%mutate(fun_error_XG_custom2 = ifelse(error_XG_custom2 > 0, error_XG_custom2^2,ifelse(error_XG_custom2<(-40),error_XG_custom2^6,error_XG_custom2)))
View(test2%>%dplyr::select(price, predicciones_XG_custom2, error_XG_custom2, fun_error_XG_custom2))
AVG_custom2 <- mean(test2$fun_error_XG_custom2)
AVG_custom2 
RMSE_custom2 <- sqrt(mean(test2$error_XG_custom2^2))
RMSE_custom2
MAE_custom2 <- mean(abs(test2$error_XG_custom2))
MAE_custom2
hist(test2$error_XG_custom2)

quantile(test2$error_XG_custom2, probs = seq(0,1,0.01))

#Cantidad de casas que no se compran
test2%>%mutate(no_compra = ifelse(error_XG_custom2<(-40),1,0))%>%count(no_compra)%>%mutate(perc = 100*n/sum(n) )

#Presupuesto de las compras
test2%>%subset(error_XG_custom2>=(-40))%>%summarise(Presup = -sum(error_XG_custom2))



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


model2 <- ranger(
  formula         = log_price ~ ., 
  data            = train, 
  num.trees       = 500,
  mtry            = 9,
  max.depth       = 10,
  sample.fraction = 1,
  min.node.size   = 50,
  seed            = 123, # Notese el seteo de la semilla
  #importance      = "impurity" 
)


model3 <- ranger(
  formula         = log_price ~ ., 
  data            = train, 
  num.trees       = 2000,
  mtry            = 9,
  max.depth       = 10,
  sample.fraction = 1,
  min.node.size   = 50,
  seed            = 123, # Notese el seteo de la semilla
  #importance      = "impurity" 
)






##########################################predict (RF 200)

#Test
pred_RF_test2_200 <- predict(model, test2)
#Train
pred_RF_train2_200 <- predict(model, train2)

test2$predicciones_RF_200 <- exp(pred_RF_test2_200$predictions)
View(test2%>%dplyr::select(price,predicciones_XG_2000, predicciones_RF_200))

#Se crea el error en pesos
test2 <- test2%>%mutate(error_RF_200 = (predicciones_RF_200 - price)/1000000)%>%mutate(fun_error_RF_200 = ifelse(error_RF_200 > 0, error_RF_200^2,ifelse(error_RF_200<(-40),error_RF_200^6,error_RF_200)))
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

##########################################predict (RF 500)

#Test
pred_RF_test2_500 <- predict(model2, test2)
#Train
pred_RF_train2_500 <- predict(model2, train2)

test2$predicciones_RF_500 <- exp(pred_RF_test2_500$predictions)
View(test2%>%dplyr::select(price,predicciones_XG_2000, predicciones_RF_500))

#Se crea el error en pesos
test2 <- test2%>%mutate(error_RF_500 = (predicciones_RF_500 - price)/1000000)%>%mutate(fun_error_RF_500 = ifelse(error_RF_500 > 0, error_RF_500^2,ifelse(error_RF_500<(-40),error_RF_500^6,error_RF_500)))
View(test2%>%dplyr::select(price, predicciones_RF_500, error_RF_500, fun_error_RF_500))
AVG_500 <- mean(test2$fun_error_RF_500)
AVG_500 
RMSE_500 <- sqrt(mean(test2$error_RF_500^2))
RMSE_500
MAE_500 <- mean(abs(test2$error_RF_500))
MAE_500
hist(test2$error_RF_500)

quantile(test2$error_RF_500, probs = seq(0,1,0.01))

#Cantidad de casas que no se compran
test2%>%mutate(no_compra = ifelse(error_RF_500<(-40),1,0))%>%count(no_compra)%>%mutate(perc = 100*n/sum(n) )

#Presupuesto de las compras
test2%>%subset(error_RF_500>=(-40))%>%summarise(Presup = -sum(error_RF_500))


##########################################predict (RF 2000)

#Test
pred_RF_test2_2000 <- predict(model3, test2)
#Train
pred_RF_train2_2000 <- predict(model3, train2)

test2$predicciones_RF_2000 <- exp(pred_RF_test2_2000$predictions)
View(test2%>%dplyr::select(price,predicciones_XG_2000, predicciones_RF_2000))

#Se crea el error en pesos
test2 <- test2%>%mutate(error_RF_2000 = (predicciones_RF_2000 - price)/1000000)%>%mutate(fun_error_RF_2000 = ifelse(error_RF_2000 > 0, error_RF_2000^2,ifelse(error_RF_2000<(-40),error_RF_2000^6,error_RF_2000)))
View(test2%>%dplyr::select(price, predicciones_RF_2000, error_RF_2000, fun_error_RF_2000))
AVG_2000_rang <- mean(test2$fun_error_RF_2000)
AVG_2000_rang 
RMSE_2000_rang <- sqrt(mean(test2$error_RF_2000^2))
RMSE_2000_rang
MAE_2000_rang <- mean(abs(test2$error_RF_2000))
MAE_2000_rang
hist(test2$error_RF_2000)

quantile(test2$error_RF_2000, probs = seq(0,1,0.01))

#Cantidad de casas que no se compran
test2%>%mutate(no_compra = ifelse(error_RF_2000<(-40),1,0))%>%count(no_compra)%>%mutate(perc = 100*n/sum(n) )

#Presupuesto de las compras
test2%>%subset(error_RF_2000>=(-40))%>%summarise(Presup = -sum(error_RF_2000))



###################################### predicción con regresión lineal y EN



df <- model.matrix(~. -1, test) %>%data.frame()
df2 <- model.matrix(~. -1, train) %>%data.frame()

#Como el modelo se corre con las variables escaladas, deben escalarse también

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

escalador <- preProcess(df[, variables_numericas])
escalador2 <- preProcess(df2[, variables_numericas])

test_s <- df
test_s[, variables_numericas] <- predict(escalador, df[, variables_numericas])
train_s <- df2
train_s[, variables_numericas] <- predict(escalador, df2[, variables_numericas])

y_train <- train$log_price

train_en <- data.frame(y_train, train_s)


#############################No me esta dejando predecir el EN, volveré a entrenarlo
# crear K-Fold particiones sobre la base para ajustar los modelos
set.seed(100)
cv5 <- trainControl(number = 5,
                    method = "cv")

# crear grid_search para elastic net
tunegrid_glmnet <- data.frame(alpha= seq(0,1,length.out=10),
                              lambda = seq(0,1,length.out=50) )

# crear modelo de Elastic Net
# hiper parametros: method: glmnet, preProcess centrar y escalar
# trainControl: CV-5Fold, metric optimizar: RMSE, tuneGri

modelo_glmnet <- train(y_train~ property_typeCasa +police + parking + cinema_d+Final_Bathrooms_2+ mod_VA1_ESTRATO26+ cinema+ marketplace+ kindergarten + mod_VA1_ESTRATO23 ,
                       data = train_en,
                       method = "glmnet",
                       trControl= cv5,
                       metric = 'anti_fiasco',
                       tuneGrid = tunegrid_glmnet)

# preProcess = c("center", "scale"),
modelo_glmnet

# plot del modelo 
plot(modelo_glmnet)
glmnet_opt <- modelo_glmnet$bestTune


####################Se entrena el modelo con los hiperparámetros óptimos
set.seed(100)
cv5 <- trainControl(number = 5,
                    method = "cv",
                    summaryFunction = custom_summary)

a <- glmnet_opt$alpha
l <- glmnet_opt$lambda
# crear grid_search para elastic net
tunegrid_glmnet <- data.frame(alpha= a,
                              lambda = l )

# crear modelo de Elastic Net
# hiper parametros: method: glmnet, preProcess centrar y escalar
# trainControl: CV-5Fold, metric optimizar: RMSE, tuneGri

modelo_glmnet <- train(y_train~ property_typeCasa +police + parking + cinema_d+Final_Bathrooms_2+ mod_VA1_ESTRATO26+ cinema+ marketplace+ kindergarten + mod_VA1_ESTRATO23 ,
                       data = train_en,
                       method = "glmnet",
                       trControl= cv5,
                       metric = 'anti_fiasco',
                       tuneGrid = tunegrid_glmnet)


#####################################Modelo de Ridge
# crear K-Fold particiones sobre la base para ajustar los modelos
set.seed(100)
cv5 <- trainControl(number = 5,
                    method = "cv")

# crear grid_search para lasso
tunegrid_ridge <- data.frame(alpha= 0,
                              lambda = seq(0.01,1,length.out=50) )

modelo_ridge <- train(y_train~ property_typeCasa +police + parking + cinema_d+Final_Bathrooms_2+ mod_VA1_ESTRATO26+ cinema+ marketplace+ kindergarten + mod_VA1_ESTRATO23 ,
                       data = train_en,
                       method = "glmnet",
                       trControl= cv5,
                       metric = 'RMSE',
                       tuneGrid = tunegrid_ridge)

# preProcess = c("center", "scale"),
modelo_ridge

# plot del modelo 
plot(modelo_ridge)
ridge_opt <- modelo_ridge$bestTune
ridge_opt


####################Se entrena el modelo con los hiperparámetros óptimos
set.seed(100)
cv5 <- trainControl(number = 5,
                    method = "cv")

a <- ridge_opt$alpha
l <- ridge_opt$lambda
# crear grid_search para elastic net
tunegrid_ridge <- data.frame(alpha= a,
                              lambda = l )

# crear modelo de Elastic Net
# hiper parametros: method: glmnet, preProcess centrar y escalar
# trainControl: CV-5Fold, metric optimizar: RMSE, tuneGri

modelo_ridge <- train(y_train~ property_typeCasa +police + parking + cinema_d+Final_Bathrooms_2+ mod_VA1_ESTRATO26+ cinema+ marketplace+ kindergarten + mod_VA1_ESTRATO23 ,
                       data = train_en,
                       method = "glmnet",
                       trControl= cv5,
                       metric = 'RMSE',
                       tuneGrid = tunegrid_ridge)


#####################################Modelo de Lasso
# crear K-Fold particiones sobre la base para ajustar los modelos
set.seed(100)
cv5 <- trainControl(number = 5,
                    method = "cv")

# crear grid_search para lasso
tunegrid_lasso <- data.frame(alpha= 1,
                             lambda = seq(0,1,length.out=50) )

modelo_lasso <- train(y_train~ property_typeCasa +police + parking + cinema_d+Final_Bathrooms_2+ mod_VA1_ESTRATO26+ cinema+ marketplace+ kindergarten + mod_VA1_ESTRATO23 ,
                      data = train_en,
                      method = "glmnet",
                      trControl= cv5,
                      metric = 'RMSE',
                      tuneGrid = tunegrid_lasso)

# preProcess = c("center", "scale"),
modelo_lasso

# plot del modelo 
plot(modelo_lasso)
lasso_opt <- modelo_lasso$bestTune
lasso_opt


####################Se entrena el modelo con los hiperparámetros óptimos
set.seed(100)
cv5 <- trainControl(number = 5,
                    method = "cv")

a <- lasso_opt$alpha
l <- lasso_opt$lambda
# crear grid_search para elastic net
tunegrid_lasso <- data.frame(alpha= a,
                             lambda = l )

# crear modelo de Elastic Net
# hiper parametros: method: glmnet, preProcess centrar y escalar
# trainControl: CV-5Fold, metric optimizar: RMSE, tuneGri

modelo_lasso <- train(y_train~ property_typeCasa +police + parking + cinema_d+Final_Bathrooms_2+ mod_VA1_ESTRATO26+ cinema+ marketplace+ kindergarten + mod_VA1_ESTRATO23 ,
                      data = train_en,
                      method = "glmnet",
                      trControl= cv5,
                      metric = 'RMSE',
                      tuneGrid = tunegrid_lasso)



#########################################Predicción reg lineal
#Test
pred_lm_test2 <- predict(model_lm, test_s)


test2$predicciones_lm <- exp(pred_lm_test2)
View(test2%>%dplyr::select(price, predicciones_lm))

#Se crea el error en pesos
test2 <- test2%>%mutate(error_lm = (predicciones_lm - price)/1000000)%>%mutate(fun_error_lm = ifelse(error_lm > 0, error_lm^2,ifelse(error_lm<(-40),error_lm^6,error_lm)))
View(test2%>%dplyr::select(price, predicciones_lm, error_lm, fun_error_lm))
AVG_lm <- mean(test2$fun_error_lm)
AVG_lm 
RMSE_lm <- sqrt(mean(test2$error_lm^2))
RMSE_lm
MAE_lm <- mean(abs(test2$error_lm))
MAE_lm
hist(test2$error_lm)

quantile(test2$error_lm, probs = seq(0,1,0.01))

#Cantidad de casas que no se compran
test2%>%mutate(no_compra = ifelse(error_lm<(-40),1,0))%>%count(no_compra)%>%mutate(perc = 100*n/sum(n) )

#Presupuesto de las compras
test2%>%subset(error_lm>=(-40))%>%summarise(Presup = -sum(error_lm))


#########################################Predicción EN
#Test
pred_EN_test2 <-  predict(modelo_glmnet, test_s)


test2$predicciones_EN <- exp(pred_EN_test2)
View(test2%>%dplyr::select(price, predicciones_EN))

#Se crea el error en pesos
test2 <- test2%>%mutate(error_EN = (predicciones_EN - price)/1000000)%>%mutate(fun_error_EN = ifelse(error_EN > 0, error_EN^2,ifelse(error_EN<(-40),error_EN^6,error_EN)))
View(test2%>%dplyr::select(price, predicciones_EN, error_EN, fun_error_EN))
AVG_EN <- mean(test2$fun_error_EN)
AVG_EN 
RMSE_EN <- sqrt(mean(test2$error_EN^2))
RMSE_EN
MAE_EN <- mean(abs(test2$error_EN))
MAE_EN
hist(test2$error_EN)

quantile(test2$error_EN, probs = seq(0,1,0.01))

#Cantidad de casas que no se compran
test2%>%mutate(no_compra = ifelse(error_EN<(-40),1,0))%>%count(no_compra)%>%mutate(perc = 100*n/sum(n) )

#Presupuesto de las compras
test2%>%subset(error_EN>=(-40))%>%summarise(Presup = -sum(error_EN))


#########################################Predicción lasso
#Test
pred_l_test2 <-  predict(modelo_lasso, test_s)




test2$predicciones_l <- exp(pred_l_test2)
View(test2%>%dplyr::select(price, predicciones_l))

#Se crea el error en pesos
test2 <- test2%>%mutate(error_l = (predicciones_l - price)/1000000)%>%mutate(fun_error_l = ifelse(error_l > 0, error_l^2,ifelse(error_l<(-40),error_l^6,error_l)))
View(test2%>%dplyr::select(price, predicciones_l, error_l, fun_error_l))
AVG_l <- mean(test2$fun_error_l)
AVG_l 
RMSE_l <- sqrt(mean(test2$error_l^2))
RMSE_l
MAE_l <- mean(abs(test2$error_l))
MAE_l
hist(test2$error_l)

quantile(test2$error_l, probs = seq(0,1,0.01))

#Cantidad de casas que no se compran
test2%>%mutate(no_compra = ifelse(error_l<(-40),1,0))%>%count(no_compra)%>%mutate(perc = 100*n/sum(n) )

#Presupuesto de las compras
test2%>%subset(error_l>=(-40))%>%summarise(Presup = -sum(error_l))



#########################################Predicción ridge
#Test
pred_r_test2 <-  predict(modelo_ridge, test_s)


test2$predicciones_r <- exp(pred_r_test2)
View(test2%>%dplyr::select(price, predicciones_r))

#Se crea el error en pesos
test2 <- test2%>%mutate(error_r = (predicciones_r - price)/1000000)%>%mutate(fun_error_r = ifelse(error_r > 0, error_r^2,ifelse(error_r<(-40),error_r^6,error_r)))
View(test2%>%dplyr::select(price, predicciones_r, error_r, fun_error_r))
AVG_r <- mean(test2$fun_error_r)
AVG_r 
RMSE_r <- sqrt(mean(test2$error_r^2))
RMSE_r
MAE_r <- mean(abs(test2$error_r))
MAE_r
hist(test2$error_r)

quantile(test2$error_r, probs = seq(0,1,0.01))

#Cantidad de casas que no se compran
test2%>%mutate(no_compra = ifelse(error_r<(-40),1,0))%>%count(no_compra)%>%mutate(perc = 100*n/sum(n) )

#Presupuesto de las compras
test2%>%subset(error_r>=(-40))%>%summarise(Presup = -sum(error_r))



#Toca quitar lo de los nucleos al final
stopCluster(cl)



View(test2%>%dplyr::select(price, predicciones_XG_custom, error_XG_custom, fun_error_XG_custom))

######################################Una última cosa  (No me gustó como quedó)
#Para evitar que cuando nos pasemos sea por mucho, 
#vamos a normalizar el logaritmo del precio predicho
#Eso va a comprimir un poco los datos con precios más altos

#Este es el dato original
hist(test2$log_price)
#Este es el dato predicho con el mejor modelo
hist(pred_xgb_test2_custom)
hist(exp(pred_xgb_test2_custom))

#Promedio y desviación original
media <- mean(log(base$price))
mean(pred_xgb_test2_custom)

#Desviación
sd <- sd(log(base$price))
sd(pred_xgb_test2_custom)

predicciones_normalizadas <- (pred_xgb_test2_custom-media)/sd + media
predicciones_normalizadas%>%head()

pred_xgb_test2_custom%>%head()

hist(predicciones_normalizadas)
hist(exp(predicciones_normalizadas))

#Cómo es la distribución de los precios?
quantile(base$price, prob = seq(0,1,0.01))
quantile(test2$predicciones_XG_custom, prob = seq(0,1,0.01))

#En el último percentil
quantile(base$price, prob = seq(0.99,1,0.001))
quantile(test2$predicciones_XG_custom, prob = seq(0.99,1,0.001))

#Menos del 4% de los aptos vale más de 2.500 millones, la mayoría de esos no los compramos porque nos vamos por debajo
#Pero más grave aún, en algunos predecimos precios muy altos y eso nos está dañando el presupuesto
#Podemos ver qué pasa si truncamos esos datos y no ofertamos nunca más de 2.500 millones


##########################################predict (Customized)

test2 <- test2%>%mutate(pred_trunc = ifelse(predicciones_XG_custom>2500000000,2500000000, predicciones_XG_custom) )

#Se crea el error en pesos
test2 <- test2%>%mutate(error_pred_trunc = (pred_trunc - price)/1000000)%>%mutate(fun_error_pred_trunc = ifelse(error_pred_trunc > 0, error_pred_trunc^2,ifelse(error_pred_trunc<(-40),error_pred_trunc^6,error_pred_trunc)))
View(test2%>%dplyr::select(price, pred_trunc, error_pred_trunc, fun_error_pred_trunc))
AVG_trunc <- mean(test2$fun_error_pred_trunc)
AVG_trunc 
RMSE_trunc <- sqrt(mean(test2$error_pred_trunc^2))
RMSE_trunc
MAE_trunc <- mean(abs(test2$error_pred_trunc))
MAE_trunc
hist(test2$error_pred_trunc)

quantile(test2$error_pred_trunc, probs = seq(0,1,0.01))

#Cantidad de casas que no se compran
test2%>%mutate(no_compra = ifelse(error_pred_trunc<(-40),1,0))%>%count(no_compra)%>%mutate(perc = 100*n/sum(n) )

#Presupuesto de las compras
test2%>%subset(error_pred_trunc>=(-40))%>%summarise(Presup = -sum(error_pred_trunc))



#######################################Exportar los resultados en el test

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

#Predicción XGboost personalizado (2da versión)
predict_test_def_XGcustom2 <- predict(mod_xgb_custom2, test_def)
test_def$predictions_XGcustom2 <- exp(predict_test_def_XGcustom2)


hist(train$log_price)
hist(train2$price)
hist(log(base$price))

#GUARDAR LA BASE
write_rds(test_def, "Test_con_predicciones.rds")


############################Histograma de los errores del mejor modelo

ggplot(df, aes(x=test2$error_XG_custom))+
  geom_histogram(color="darkblue", fill="lightblue", bins = 200, alpha = 1)+
  theme_classic()+
  xlab("Error (millones de pesos)")+
  ylab("Frecuencia")

#Agrupa aproximadamente el 97% de los datos

ggplot(test2, aes(x=error_XG_custom))+
  geom_histogram(color="darkblue", fill="lightblue", bins = 200, alpha = 1)+
  theme_classic()+
  geom_vline(xintercept = -40, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = 40, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = 0, linetype="dotted", color = "black", size=1)+
  xlab("Error (millones de pesos)")+
  ylab("Frecuencia")+
  ggtitle("Distribución del error de predicción en millones de pesos")+
  xlim(-1000,800)+
  theme(text = element_text(size = 16), plot.title = element_text(size = 20, hjust = 0.5))

quantile(test2$error_XG_custom, probs = seq(0,1,0.005))
#poquito menos del 34% y desde el 63% (O sea el 29% está en nuestro rango ideal) 
#29% en lo que queremos tener
#34% no se cierran (se subestiman por más de 40 millones)
#37% se sobre estiman por más de 40 millones.


#Cargar el submission template
sub_tembp <- read.csv("submission_template.csv")

#Ver las predicciones de Cali cómo están dando
summary(test_def$predictions_XGcustom)
mean(test_def$predictions_XGcustom)
desv <- sd(test_def$predictions_XGcustom)

#Vamos a respetar la desviación de precios que tenemos, mejor no agrandarla
#Pero si vamos a centrar en la media de Cali
ggplot(test_def, aes(x=predictions_XGcustom/1000000))+
  geom_histogram(color="darkblue", fill="lightblue", bins = 200, alpha = 1)+
  theme_classic()+
  geom_vline(xintercept = 555.3, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = 837.1, linetype="dotted", color = "black", size=1)+
  xlab("Error (millones de pesos)")+
  ylab("Frecuencia")+
  ggtitle("Distribución del error de predicción en millones de pesos")+
  theme(text = element_text(size = 16), plot.title = element_text(size = 20, hjust = 0.5))

ponderador = 555314430/mean(test_def$predictions_XGcustom)

test_def <- test_def%>%mutate(predicciones_def = ponderador*predictions_XGcustom)
mean(test_def$predicciones_def )
summary(test_def$predicciones_def )

ggplot(test_def, aes(x=predicciones_def/1000000))+
  geom_histogram(color="darkblue", fill="lightblue", bins = 200, alpha = 1)+
  theme_classic()+
  xlab("Error (millones de pesos)")+
  ylab("Frecuencia")+
  ggtitle("Distribución del error de predicción en millones de pesos")+
  theme(text = element_text(size = 16), plot.title = element_text(size = 20, hjust = 0.5))


#Gráfico comparativo
ggplot(test_def, aes(x=predictions_XGcustom/1000000))+
  geom_histogram(color="darkblue", fill="blue", bins = 200, alpha = 1)+
  geom_histogram(aes(x=predicciones_def/1000000), color="red", fill="red", bins = 200, alpha = 0.5)+
  theme_classic()+
  geom_vline(xintercept = 555.3, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = 837.1, linetype="dotted", color = "blue", size=1)+
  xlab("Precios predichos en Cali (millones de pesos)")+
  ylab("Frecuencia")+
  ggtitle("Distribución del precio predicho en millones de pesos")+
  theme(text = element_text(size = 16), plot.title = element_text(size = 20, hjust = 0.5))

quantile(test_def$predictions_XGcustom/1000000, probs = seq(0,1,0.05))
quantile(test_def$predicciones_def/1000000, probs = seq(0,1,0.05))

#Se reemplazan las predicciones definitivas en el submission template
sub_tembp$price[which(sub_tembp$property_id == test_def$property_id)] <- test_def$predicciones_def[which(sub_tembp$property_id == test_def$property_id)]

#Se guarda y exporta el submission template
write.csv(sub_tembp, "predictions_franco_malkun_osorio.csv")

resultados <- read.csv("predictions_franco_malkun_osorio.csv")

resultados <- resultados[,2:3]

write.csv(resultados, "predictions_franco_malkun_osorio.csv", row.names = FALSE)


ggplot(resultados, aes(x=price/1000000))+
  geom_histogram(color="darkblue", fill="lightblue", bins = 200, alpha = 1)+
  theme_classic()+
  geom_vline(xintercept = 555.3, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = 837.1, linetype="dotted", color = "black", size=1)+
  xlab("Error (millones de pesos)")+
  ylab("Frecuencia")+
  xlim(0,5000)+
  ggtitle("Distribución del error de predicción en millones de pesos")+
  theme(text = element_text(size = 16), plot.title = element_text(size = 20, hjust = 0.5))

mean(resultados$price)
sd(resultados$price)

mean(base$price)
sd(base$price)

sum(is.na(resultados$price))

#Importancia de las variables
variable_importance <- varImp(mod_xgb_custom)

V = caret::varImp(mod_xgb_custom)

plot(V, main = "Importancia de las variables", xlab = "Importancia", ylab = "variable")

V = V$importance[1:10,]

ggplot(
  V,
  mapping = NULL,
  top = dim(V$importance)[1],
  environment = NULL, fill = "blue"
)


ggplot(base, aes(x=price/1000000))+
  geom_histogram(color="darkblue", fill="lightblue", bins = 200, alpha = 1)+
  theme_classic()+
  geom_vline(xintercept = 555.3, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = 837.1, linetype="dotted", color = "black", size=1)+
  xlab("Error (millones de pesos)")+
  ylab("Frecuencia")+
  xlim(0,5000)+
  ggtitle("Distribución del error de predicción en millones de pesos")+
  theme(text = element_text(size = 16), plot.title = element_text(size = 20, hjust = 0.5))

