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
grid_default <- expand.grid(nrounds = c(1000),
                            max_depth = c(5,8),
                            eta = c(0.01),
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


#predict
pred_xgb_test <- predict(xgboost, test)
pred_xgb_train <- predict(xgboost, train)

pred_xgb_test2 <- predict(xgboost, test2)
pred_xgb_train2 <- predict(xgboost, train2)

test2$predicciones <- exp(pred_xgb_test2)

View(test2%>%dplyr::select(price, predicciones))

write_rds(xgboost, "modelo_xgboost_con_1000.rds")

mod_xgboost <- read_rds("modelo_xgboost.rds")

prueba_predict <- predict(mod_xgboost, test)

#Se crea el error en pesos
test2 <- test2%>%mutate(error = predicciones - price)%>%mutate(fun_error = ifelse(error > 0, error^2,ifelse(error<-40000000,error^6,error)))
View(test2%>%dplyr::select(price, predicciones, error))
AVG <- mean(test2$fun_error)
RMSE <- sqrt(mean(test2$error^2))
MAE <- mean(abs(test2$error))



######Predicciones
grid_default <- expand.grid(nrounds = c(20),
                            max_depth = c(1,5),
                            eta = c(0.01,0.3),
                            gamma = c(0,1),
                            min_child_weight = c(50),
                            colsample_bytree = c(0.7),
                            subsample = c(0.6))

#predict
pred_xgb <- predict(xgboost, test)





#Toca quitar lo de los nucleos al final
stopCluster(cl)

#Cargar el test definitivo

