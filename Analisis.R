install.packages("readxl") 
library(readxl)

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

# Calculamos el coeficiente de asimetría >>> AF = m_3/s_3

coeficiente_fisher <- function(x) {
  n <- length(x)
  m3 <- sum((x - mean(x))^3) / n
  s3 <- (sd(x))^3 
  return(m3 / s3)
}

AF <- coeficiente_fisher(colest)

# Vemos que el coeficiente de asimetria es de 0.30451 > 0, luego, la cola larga esta a la derecha. De hecho, podemos
#observar que la media es mayor que la mediana. Por tanto los datos observados sigue una distribucion asimétrica positiva.

par(mfrow = c(1, 2))
hist(colest, breaks = 30,
     main="Distribución de Niveles de Colesterol", 
     xlab="Nivel de Colesterol", 
     ylab="Frecuencia", 
     col="lightblue", 
     border="black")

# Como el coeficiente de asimetría no es nulo, y viendo el histograma, podemos concluir que los datos no siguen
# una distribución normal. Gracias al histograma, podemos ver que es una distribución unimodal y que puede parecerse
# a una distribución normal pero ya hemos comprobado que no.  

# Como hemos visto que la distribución que sigue los datos es asimétrica a la derecha que también es parecida a una
# distribución normal, y los datos observados que son los niveles de colesterol no pueden ser negativos, podemos 
# pensar que los datos pueden seguir una distribución de una log-normal. Primero representemos el histograma más el KDE.

hist(colest, breaks = 30, probability = TRUE, 
     col = "lightblue", border = "white",
     main = "Histograma y KDE",
     xlab = "Valores")
lines(density(colest), col = "blue", lwd = 2)

# Aún no podemos estar seguros de que pueda seguir una log-normal, entonces realicemos un QQ-plot aplicando el logaritmo a los datos
# ya que si X sigue una log-normal, entonces log(X) sigue una normal.

qqnorm(log(colest))
qqline(log(colest), col = "red")

# Podemos observar que los puntos se alinean sobre la línea roja, entonces log(X) sigue una distribución de una normal.
# Entonces podemos concluir que los datos siguen una distribción asimétrica por la derecha que es compatible con una 
# distribución log-normal.

# Ahora representemos el box-plot de los datos: 

boxplot(colest, 
        main="Diagrama de Caja de Colesterol", 
        ylab="Nivel de colesterol", 
        col="orange")

# Podemos observar en el boxplot que hay un valor atipico que alcanza los 345mg/dL, entonces podemos pensar que el dato
# es incorrecto ya que ese nivel de colesterol es muy elevado.
# También observamos que la media está descentrada, por tanto la distribución es asimétrica como hemos visto antes.
# Vemos que la caja del boxplot no es tan estrecha, es decir que la mayoría de personas se concentran en un rango cercano
 # a la mediana.


j = 0 # Contador para altos
k = 0 # Contador para moderados
for (i in colest) {if (i >= 240) {j = j + 1
k = k + 1} else if (i >= 200) {k = k + 1}}
# Calculamos las proporciones (135 es el total de tu archivo)
altos = j / 135
medios = k / 135
print(altos)
print(medios)
