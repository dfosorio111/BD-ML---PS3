#####Intento 1 del xgboost
rm(list=ls())

#### **Instalar/llamar las librerías de la clase**
require(pacman)
p_load(tidyverse,rio,glue,
       hexbin,
       patchwork,vip, ## plot: 
       ggrepel, ## plot: geom_text_repel
       stringi,tidytext,stopwords, ## text-data
       tidymodels,finetune) 

#Directorio Daniel
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning/BD-ML---PS3/data")


#importar la base del train
probando <- read_rds("BASE_CENSO_ORIGINALES_REGEX_reducida.rds")

#Quitar NA
probando <- probando%>%drop_na()

#Escoger 200 datos aleatoriamente
#Se establece semilla
set.seed(1000)
smp_size <- floor(200)
train_ind <- sample(1:200, size = smp_size)
#Crear train set para ajustar los parámetros
train <- probando[train_ind, ]
#Crear test set para evaluar el modelo
test2 <- probando[-train_ind, ]%>%head(100)


#Base definitiva

keep <- c("property_id", "price", "bedrooms", "property_type",
          "mean_H_NRO_CUARTOS2", "med_H_NRO_CUARTOS2", "sum_HA_TOT_PER2", 
          "med_V_TOT_HOG2", "mod_VA1_ESTRATO2", "mod_V_MAT_PISO2",
          "Final_Bathrooms_2", "new_surface_def")

train <- train[,(names(train) %in% keep)]
test2 <- test2[,(names(test2) %in% keep)]

#Convertir en factores las variables categóricas
train$property_type <- as.factor(train$property_type)
train$mod_VA1_ESTRATO2 <- as.factor(train$mod_VA1_ESTRATO2)
#train$mod_V_MAT_PARED2 <- as.factor(train$mod_V_MAT_PARED2)
#train$mod_V_MAT_PISO2 <- as.factor(train$mod_V_MAT_PISO2)
#train$mod_VE_RECBAS2 <- as.factor(train$mod_VE_RECBAS2)

#train%>%count(mod_V_MAT_PARED2)


## set n-folds
set.seed(234)
db_folds <- vfold_cv(data=train, v=5 , strata=price)
db_folds


db_recipe <- recipe(formula=price ~ . , data=train) %>% ## En recip se detallan los pasos que se aplicarán a un conjunto de datos para prepararlo para el análisis de datos.
  update_role(property_id, new_role = "id") %>%## cambiar role para variable id
  step_dummy(all_nominal_predictors()) %>%
  step_nzv(all_predictors())
db_recipe




## Boosted Tree Model Specification
xgb_spec <- boost_tree(trees = 50,
                       tree_depth = tune(),
                       min_n = tune(),
                       mtry = tune(),
                       sample_size = tune(),
                       learn_rate = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")
xgb_spec

## workflow
xgb_word_wf <- workflow(db_recipe, xgb_spec)

#Griilita
xgb_grid <- grid_max_entropy(tree_depth(c(5L, 10L)),
                             min_n(c(10L, 40L)),
                             mtry(c(5L, 10L)), 
                             sample_prop(c(0.5, 1.0)), 
                             learn_rate(c(-2, -1)),
                             size = 20)
xgb_grid

## estimate model
xgb_word_rs <- tune_race_anova(object = xgb_word_wf,
                               resamples = db_folds,
                               grid = xgb_grid,
                               metrics = metric_set(rmse),
                               control = control_race(verbose_elim = T))
xgb_word_rs

## plot model
plot_race(xgb_word_rs)

## best model
show_best(xgb_word_rs)


test <- read_rds("TEST_BASE_CENSO_ORIGINALES_REGEX.rds")
