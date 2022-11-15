# Making Money with Machine Learning

En el presente ejercicio se realizan modelos de Machine Learning con el propósito de predecir de la manera más acertada posible el precio de las viviendas de la ciudad colombiana de Santiago de Cali. El objetivo final de los modelos es cerrar el mayor número de negocios posibles maximizando el porcentaje de inmuebles comprados en la ciudad de Cali con el gasto mínimo posible. Se hace uso de los datos de propiedad individual de las ciudades de Bogotá y Medellín provenientes de la plataforma de Properati para entrenar los modelos.

Se plantea una métrica de evaluación distinta a minimizar el error cuadrático medio (MSE) para entrenar el modelo. A partir del error, es decir la diferencia entre el precio real de la vivienda y el precio estimado, se define una función personalizada para esta aplicación propuesta por los autores, descrita en la sección de modelos y resultados.

Los modelos de predicción muestran que las variables más importantes para predecir el precio de los inmuebles son los metros cuadrados, el número de baños y el estrato. A partir de los distintos modelos desarrollados encontramos que los árboles de regresión hacen predicciones más acertadas que los de regresión lineal, lo que sugiere que las interacciones entre las variables y no linealidades son relevantes. En cuanto al modelo con XGBoost, vemos que los resultados obtenidos mejoran considerablemente al entrenar con la métrica personalizada. De esta manera, la métrica es nuestra principal contribución frente al fiasco de Zillow.

<img width="500" alt="image" src="https://user-images.githubusercontent.com/112331993/201814095-2a1f436d-0a98-42f1-b80e-edf742e898fb.png">
