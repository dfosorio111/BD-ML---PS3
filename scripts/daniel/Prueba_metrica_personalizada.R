#Este es un código de prueba
#Por facilidad no pongo paquetes o bases, pero se debe correr con paquetes y bases del código "XGBoost.R"
#Solo cargo unos que probablemente no estaban

#El modelo que sirvió viene de este enlance:
browseURL("https://stackoverflow.com/questions/46827054/create-rmsle-metric-in-caret-in-r")

p_load(ISLR, xgboost, tidyverse,
       Metrics)

#Train mini
mini_train <- train[1:1000,]


# Custom Metric
evalerror <- function(preds, mini_train) {
  labels <- mini_train$log_price
  u = (preds-labels)^2
  err <- sqrt((sum(u) / length(u)))
  return(list(metric = "MyError", value = err))
}

#Intento 2
madSummary <- function (data) {
  out <- mean(data$obs - data$pred, na.rm = TRUE)
  names(out) <- "PRUEB"
  out
}


#la tercera es la vencida
custom_summary <- function(data, lev = NULL, model = NULL){
  out = mean(data[, "obs"]-data[, "pred"])
  names(out) = c("custom_metric")
  out
}

#Funcionó!!!!!!!! ahora intento con la métrica que queremos
custom_summary <- function(data, lev = NULL, model = NULL){
  out = mean(ifelse((exp(data[, "pred"])-exp(data[, "obs"]))/1000000 > 0, (exp(data[, "pred"])-exp(data[, "obs"]))^2, 
                    ifelse((exp(data[, "pred"])-exp(data[, "obs"]))/1000000 < (-40) ,(exp(data[, "pred"])-exp(data[, "obs"]))^6 ,(exp(data[, "pred"])-exp(data[, "obs"])))) ) 
  names(out) = c("anti_fiasco")
  out
}

ctrl <- trainControl(method = "cv",  
                       number = 2,     
                       summaryFunction = custom_summary)

#Grilla
grid_default <- expand.grid(nrounds = c(50),
                            max_depth = c(5),
                            eta = c(0.3),
                            gamma = c(0),
                            min_child_weight = c(100),
                            colsample_bytree = c(0.7),
                            subsample = c(0.6))


#modelo
set.seed(1410)
xgboost <- train(
  log_price ~ . ,
  data = mini_train,
  method = "xgbTree",
  trControl = ctrl,
  metric = "anti_fiasco",
  tuneGrid = grid_default,
  preProcess = c("center", "scale"),
  maximize = FALSE
)


ctrl <- trainControl(method = "cv",  
                     number = 2)

set.seed(1410)
xgboost2 <- train(
  log_price ~ . ,
  data = mini_train,
  method = "xgbTree",
  trControl = ctrl,
  metric = "RMSE",
  tuneGrid = grid_default,
  preProcess = c("center", "scale")
)

pred <- predict(xgboost, mini_train)

mini_train$pred <- pred

mini_train <- mini_train%>%mutate(error = log_price - pred, error_pesos =(exp(log_price)-exp(pred))/1000000)
hist(mini_train$error)
hist(mini_train$error_pesos)


pred2 <- predict(xgboost2, mini_train)

mini_train$pred2 <- pred2

mini_train <- mini_train%>%mutate(error2 = log_price - pred2, error_pesos2 =(exp(log_price)-exp(pred2))/1000000)
hist(mini_train$error2)
hist(mini_train$error_pesos2)

?train()  
  