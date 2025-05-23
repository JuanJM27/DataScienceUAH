########################################################################################################## #
#' Iniciaci�n a la gesti�n de datos en R
#'   Ejercicios de MANIPULACI�N DE DATOS EN R                          
#'                             
#' by Ignacio Morales-Castilla
#' date: feb 2024
#'                              
########################################################################################################## #



# 1.  PRUEBA DE TIPO Y ESTRUCTURA DE DATOS ----
######################################### #

### 1.1. Adivina el tipo Y estructura de los siguientes datos y verifica si tienes raz�n
c(3,8,5.2)

"Alcal�"

1!=2

# 1.2. Convierte los siguientes datos como se sugiere
#c("friday","monday","tuesday")  # a factor
#c("friday","monday","tuesday")  # a factor ordenado, donde el orden es como en una semana
#c("friday","monday","tuesday")  # a lista con 3 elementos
#matrix(1,3,4)                   # a data frame
#matrix(1,3,4)                   # a array
#data.frame(a=1:3,b=4:6,c=7:9)   # a matrix



########################### #
# 2.  EXPLORACI�N DE DATOS ----
########################### #

# 2.1. Abre cualquier base de datos disponible en R y expl�rala:
data()

# �chale un vistazo en conjunto, a sus primeras filas y a sus �ltimas filas
# Busca atributos
# Informa sobre su estructura y tipo de datos
# Si hay alg�n factor, informa sobre sus niveles
# Informa sobre sus dimensiones (n�mero de filas y columnas)
# Informa sobre el nombre de sus variables y casos
# Informa sobre estad�sticas descriptivas de cada variable
# Expl�rala visualmente

# 2.2. Considera el siguiente vector y expl�ralo:
v <-  c(4,3,2,4,2,1,4,2,2,3,4,3,3,2,2)
# Cuenta el n�mero de elementos
# Informa sobre los valores �nicos
# Cuenta las veces que aparece cada valor en el vector
# Informa sobre las posiciones que tienen valores duplicados (que han aparecido anteriormente en el vector) 
# Informa sobre las posiciones de la primera vez que aparecen los valores en "a" en el vector
# a <- c(4,2)

# 2.3. Cuenta el n�mero de caracteres en la siguiente cadena
# "liudhksh@b!cvksj h?.douvyhu"



########################## #
# 3.  SUBCONJUNTOS DE DATOS ----
########################## #
View(CO2)

# 3.1. Selecciona las primeras 7 filas de la base de datos CO2
# 3.2. Selecciona las columnas 2 y 4 de la base de datos CO2
# 3.3. Selecciona las columnas "Plant" y "uptake" de la base de datos CO2
# 3.4. Selecciona las columnas "Plant" y "uptake" y las filas 1 a 42
# de la base de datos CO2
# 3.5. Selecciona las columnas "Plant" y "uptake" y las filas de tipo "Quebec"
# de la base de datos CO2
# 3.6. Selecciona todas las filas excepto aquellas de la planta Qc1
# 3.7. Selecciona todas las filas con uptake de 30 a 40, incluyendo ambos valores
# 3.8. Selecciona todas las filas con uptake de 30 a 40, incluyendo ambos valores,
# usando la funci�n subset()


####################### #
# 4. EDICI�N DE DATOS ----
####################### #
22 <- CO2
# 4.1. Cambia el nombre de la variable "conc" en la base de datos CO22
# 4.2. Cambia los nombres de las filas en la base de datos CO22, usando n�meros del 101 al 184
# 4.3. Cambia los niveles del factor "Threatment" a "chill" y "nonchill" en la base de datos CO22
# 4.4. Crea una nueva variable en la que "uptake" se recode a dos niveles: 
# "LowUptake" y "HighUptake" (< y > que 25)
# 4.5. Crea una nueva variable que sea la ra�z cuadrada (sqrt()) de uptake
# 4.6. Has perdido la concentraci�n y los datos de uptake de la planta Qn2 en la base de datos CO22,
# as� que introduce NA en esas celdas.
# 4.7. Obten la base de datos CO22 excluyendo los casos con NA. 
# Has aprendido dos formas de hacerlo.
# 4.8. Calcula la media de uptake en CO22
# 4.9. Crea un nuevo atributo en CO22 para explicar que se perdieron datos de Qn2


############################## #
# 5. REORGANIZACI�N DE DATOS ----
############################## #

# 5.1. Escribe los valores de CO2$uptake en orden normal y en orden inverso
# 5.2. Reorganiza la base de datos CO2 para que las variables se ajusten en filas y 
# los casos en columnas
# 5.3. Ordena el vector CO2$conc en orden descendente
# 5.4. Ordena la base de datos CO2 por conc en orden descendente
# Opci�n 1 (order{base})

# Opci�n 2 (arrange{dplyr})

# 5.5. Ordena la base de datos CO2 por Plant y conc dentro de la planta, 
# planta en orden alfab�tico (predeterminado) y conc en orden descendente
# 5.6. Aleatoriza el orden de las filas en la base de datos CO2 
# 5.7. Cambia el orden de las columnas en la base de datos CO2, de la siguiente manera:
# "Type","Treatment","Plant","conc","uptake"
# 5.8. Combina las columnas "Plant" y "uptake" de la base de datos CO2
# 5.9. Combina las filas 1,8,15 de la base de datos CO2
# 5.10. Agrega una nueva columna llamada NAs y llena de NA,
# y luego elim�nala 
# 5.11. Elimina las columnas Plant y Type de la base de datos CO2, 
# gu�rdalas por separado y luego p�galas de nuevo en CO2 en sus posiciones
# 5.12. Incorpora la variable NewVar en la base de datos New a la base de datos CO2,
# emparejando las observaciones por las columnas "Plant" y "P",
# y manteniendo todas las l�neas en CO2
# New <- data.frame(P=unique(CO2[1:30,"Plant"]), NewVar=runif(5))
# 5.13. Incorpora la variable NewVar en la base de datos New a la base de datos CO2,
# pero solo mant�n aquellas l�neas que tienen informaci�n completa
# New <- data.frame(P=unique(CO2[1:30,"Plant"]), NewVar=runif(5))



########################################### #
# 6. IMPORTACI�N Y EXPORTACI�N DE DATOS ----
########################################### #

# 6.1. Exporta el dataframe New a un archivo .txt y luego imp�rtalo con un nombre diferente
# exportar

# importar

