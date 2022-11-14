require("tidyverse")
require("simstudy")
require("here")
require("SuperLearner")

# set the seed for reproducibility
set.seed(123)


#datos simulados, distancia al DBD
ddef <- defData(varname = "dcbd", formula = "0;1", dist = "uniform")

theta1 <- c(0.9, 0.01, 0.5, 0.8, 0.6, 0.3, 0.8)
knots <- c(0.25, 0.5, 0.75)

dt <- genData(1000, ddef)

dt <- genSpline(
  dt = dt, newvar = "price",
  predictor = "dcbd", theta = theta1,
  knots = knots, degree = 3,
  noise.var = .025
)

dt<- dt %>% mutate(price=100000+price*10000)


ggplot(data=dt) +
  geom_point(aes(y=price,x=dcbd),shape=21,size=1.5) +
  xlab("Distancia al Centro (en millas)") +
  ylab("Precio en US$") +
  theme_bw() +
  theme(text=element_text(size=20))



SL.forest1 <- create.Learner("SL.randomForest", list(ntree = 1000))
SL.forest2 <- create.Learner("SL.randomForest", list(ntree = 100))

# Un SL que tiene 2 bosques, uno con 10000 arboles, otro con 100 arboles, y una regresion lineal
sl.lib <- c(SL.forest1$names,SL.forest2$names,"SL.lm")

fitY <- SuperLearner(Y = y, X = data.frame(x),
                     method = "method.NNLS", SL.library = sl.lib)

fitY