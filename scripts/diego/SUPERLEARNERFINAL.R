require("tidyverse")
require("simstudy")
require("here")
require("SuperLearner")

dta<-readRDS("train_final.RDS")

#dta <- dta[1:500,]

dta$property_type <- as.factor(dta$property_type)
dta$mod_VA1_ESTRATO2 <- as.factor(dta$mod_VA1_ESTRATO2)

y<- log(dta$price)

dta <- dta %>% dplyr::select(-price,
                      -geometry, -property_id, -city,
                      -mod_V_MAT_PARED2,-mod_V_MAT_PISO2,
                      -mod_VE_RECBAS2, -operation_type)


X <- dta %>%data.frame()


X_TEST <- test2




colnames(X) <-sub("library", "Xlibrary", colnames(X))
colnames(X_TEST) <-sub("library", "Xlibrary", colnames(X_TEST))


SL.forest1 <- create.Learner("SL.randomForest", list(ntree = 500))
SL.forest2 <- create.Learner("SL.randomForest", list(ntree = 100))
#SL.xg <- create.Learner("SL.xgboost", list(nrounds = 100))
# Un SL que tiene 2 bosques, uno con 1000 arboles, otro con 100 arboles, y una regresion lineal
sl.lib <- c(SL.forest1$names,SL.forest2$names,"SL.mean", "SL.lm")


fitY <- SuperLearner(Y = y, X = data.frame(X),
                     method = "method.NNLS", SL.library = sl.lib)

y_hat <- predict(fitY, newdata = data.frame(X_TEST))


names(X)
names(X_TEST)
