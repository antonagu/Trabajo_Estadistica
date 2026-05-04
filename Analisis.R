install.packages("readxl") 
library(readxl)
install.packages("moments")
library(moments)
install.packages("fitdistrplus")
library(fitdistrplus)

# Cargar el archivo
datos <- read_excel("colesterol.xlsx")

# Ver la estructura básica
str(datos) # Dice el tipo de variables que tenemos.
head(datos) # Enseña las primeras 6 filas de la tabla.
colest <- datos$'Niveles de colesterol'

# Estadísticos
media <- mean(colest)
mediana  <- median(colest)
varianza <- var(colest)
desv_tip <- sd(colest)
rango    <- max(colest) - min(colest)
RIC      <- IQR(colest) # Recorrido Intercuartílico

coef_asimetria <- skewness(colest)
print(coef_asimetria)

# Vemos que el coeficiente de asimetria es de 0.3079347, luego la cola larga esta a la derecha. Luego lo veremos en la
# grafica. Por tanto los datos sigue una distribucion asimetrica positiva.

# Como vemos que el coeficiente de asimetria no es tan grande y esta cerca de 0, podriamos pensar que se parece a 
# una normal, como por ejemplo la log normal, que es asimetrica por la derecha. Compaermos la distribucion, con los datos, 
# con el histograma.

# Ajustar una Normal y una Log-normal para comparar
fit_ln <- fitdist(colest, "lnorm")

# Graficar ambas para ver cuál "pisa" mejor las barras del histograma
plot.legend <- c("Log-normal")
denscomp(list( fit_ln), legendtext = plot.legend)


par(mfrow = c(1, 2))
hist(colest, 
     main="Distribución de Niveles de Colesterol", 
     xlab="Nivel de Colesterol", 
     ylab="Frecuencia", 
     col="lightblue", 
     border="black")

boxplot(colest, 
        main="Diagrama de Caja de Colesterol", 
        ylab="Nivel", 
        col="orange")

