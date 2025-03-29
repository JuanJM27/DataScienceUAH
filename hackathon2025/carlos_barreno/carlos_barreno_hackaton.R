################################################################# #
##'
##'  "Seminario - Hackathon datos espacialmente explícitos en R"
##'       
##'       Feb 2025
##'       by Ignacio Morales-Castilla
##'
################################################################# #


## limpiando el ambiente de trabajo
rm(list=ls())
options(stringsAsFactors = FALSE)


## establecer directorio de trabajo (wd)
getwd()  
setwd("C:/Users/samue/Documents/GitHub/DataScienceUAH/") 

## cargar paquetes
library(terra)
library(sf)
library(dplyr)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(googleway)
library(ncdf4)
library(ggplot2)
library(geodata)
library(mapSpain)
library(tidyverse)
library(patchwork)
library(ggrepel)
library(raster)
theme_set(theme_bw())


## 1. Carga de datos espaciales ####
############################### ##

##' busca, descarga y lee datos de temperaturas máximas diarias para 
##' toda Europa, correspondientes al periodo 1980-1984
##' https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php#datafiles 
##' 

tmax <- rast("hackathon2025/carlos_barreno/tx_ens_mean_0.25deg_reg_1980-1994_v30.0e.nc")
tmax


##' lee datos de Parques Nacionales de España (en github)
##' 

enp <- st_read("hackathon2025/data/ENP.shp")
parques <- subset(enp, figura_lp %in% "Parque Nacional") #Me quedo solo con los parques nacionales
                                        

##' lee datos de temperaturas mensuales de España, para enero y julio (en github)


tempmensualenero <- rast("hackathon2025/data/temp.Spain.jan07.tif")
tempmensualenero

tempmensualjulio <- rast("hackathon2025/data/temp.Spain.jul07.tif")
tempmensualjulio

##' Descarga datos para el lince ibérico ("Lynx pardinus"), con el paquete rgbif
##' 

library(rgbif)
?`rgbif-defunct` 
#Visito la web para ver como leer los datos que necesito


datos_lince <- occ_search(scientificName = "Lynx pardinus") #Descargo las observaciones de lince 
datos_lince



## 2. Manipulación de datos ####
############################### ##


##' Crea un objeto con los datos de temperaturas máximas de España del 
##' mes de enero 
##' de 1981. Repite para el mes de julio de 1981. Pista: tendras 
##' que recortar y enmascarar
##' los datos de E-OBS.

#Para recortar los mapas tengo que tener una masara, la cual será el mapa de españa. Para hacerlo usó el paquete Mapspain 

mascara <- esp_get_country()
mascara

#para conocer las capas que tengo que seleccionar para los meses de julio y enero de 1981, paso a formato fecha: 

fechas <- time(tmax)
fechas <- as.Date(fechas)

#Selecciono los dias del mes de enero (01), de 1981:

diasenero1981 <- which(format(fechas, "%Y-%m") == "1981-01")
str(diasenero1981) #nos aseguramos que tenga 31 días

#Selecciono los días del mes de julio (07), de 1981: 
diasjulio1981 <- which(format(fechas, "%Y-%m") == "1981-07")
str(diasjulio1981)

#Genero los dos objetos para cada mes: 

temps_max_enero_1981 <- subset(tmax, diasenero1981)
temps_max_julio_1981 <- subset(tmax, diasjulio1981)

#Paso mi mascara a crs del raster
crs_raster <- crs(temps_max_enero_1981, proj = TRUE)

mascara_repro <- st_transform(mascara, crs_raster)

#Recorto: 

temps_max_enero_1981_crop <- crop(temps_max_enero_1981, mascara_repro)
temps_max_julio_1981_crop <- crop(temps_max_julio_1981, mascara_repro)

#Enmascaro 


temp_max_enero_1981_Spain <- mask(temps_max_enero_1981_crop, mascara_repro)
temp_max_julio_1981_Spain <- mask(temps_max_julio_1981_crop, mascara_repro)


##' Crea un subset con los datos de ocurrencia del lince, solo para España
##' 

#Los datos que he obtenido vienen en formato lista, de las cuales me interesa la de data. 

colnames(datos_lince$data) #Busco dentro de data como se llama la columna que incluye el pais de la observación y veo que aparece como "Country"

unique(datos_lince$data$country) #Busco como aparece españa en la columna, y veo que aparece como "Spain" 

ocurr_lin_esp <- subset(datos_lince$data, country == "Spain" ) #obtengo un dataframe

unique(ocurr_lin_esp$country) #Compruebo que solo tengp observaciones de España.
colnames(ocurr_lin_esp)
ocurr_lin_esp$decimalLatitude
ocurr_lin_esp$decimalLongitude #Hay ocurrencias de Lince que tienen NA, esta información es indispensable para trabajar espacialmente, como no tengo datos de eso, los puedo quitar (creo) 

ocurr_lin_esp_sinNA <- ocurr_lin_esp %>%
  drop_na(decimalLongitude, decimalLatitude)
  
##' Extrae los datos de temperaturas mensuales de junio y enero para 
##' cada ocurrencia del lince en España. Repite para las temperaturas máximas de los
##'  días del mes de enero y del mes de julio de 1981

#Quiero conocer la temperatura de junio y enero para cada punto de coordenada de ocurrencia de lince, para ello, puedo extraer los datos de las temperaturas mensuales a partir de las coordenadas: 

#Me aseguro cual es el sistema de referencia de los datos 
crs(tempmensualenero)
crs(tempmensualjulio)# WGS84, que en EPGS es 4326 (el mismo en el que viene las latitudes y longitudos de rgbif)

#Paso mis datos de ocurrencias a puntos, especificando en que columnas tengo la latitud y longitud.

puntos_ocurr_lin_esp <- data.frame(x = ocurr_lin_esp_sinNA$decimalLongitude,
                               y = ocurr_lin_esp_sinNA$decimalLatitude) 



# Para enero:
temp_mens_ene_lin <- terra::extract(tempmensualenero, puntos_ocurr_lin_esp) #Extraigo los datos, añado el identificador de terra (terra::), porque si no, lo hace con el extract de tidyverse y da error. 
temp_mens_ene_lin  <- cbind(temp_mens_ene_lin, 
                            ocurr_lin_esp_sinNA$key,
                            ocurr_lin_esp_sinNA$decimalLongitude, 
                            ocurr_lin_esp_sinNA$decimalLatitude) # y añado la columna key, (identificador de cada observación de lince) y su latitud y longitud
colnames(temp_mens_ene_lin)[(ncol(temp_mens_ene_lin)-2):ncol(temp_mens_ene_lin)] <- c("key", "longitud", "latitud") #Cambio el nombre de las columnas añadidas

#Para julio: Repito el proceso anterior 
temp_mens_jul_lin <- terra::extract(tempmensualjulio, puntos_ocurr_lin_esp)
temp_mens_jul_lin  <- cbind(temp_mens_jul_lin, 
                            ocurr_lin_esp_sinNA$key,
                            ocurr_lin_esp_sinNA$decimalLongitude, 
                            ocurr_lin_esp_sinNA$decimalLatitude)
colnames(temp_mens_jul_lin)[(ncol(temp_mens_jul_lin)-2):ncol(temp_mens_jul_lin)] <- c("key", "longitud", "latitud")


# Para 1981: (Lo mismo, pero cambiando los raster)
# Para enero de 1981 (ya enmascarado para España)
temp_max_ene_81_lin <- terra::extract(temp_max_enero_1981_Spain, puntos_ocurr_lin_esp)
temp_max_ene_81_lin <- cbind(temp_max_ene_81_lin, 
                             ocurr_lin_esp_sinNA$key, 
                             ocurr_lin_esp_sinNA$decimalLongitude, 
                             ocurr_lin_esp_sinNA$decimalLatitude)
colnames(temp_max_ene_81_lin)[(ncol(temp_max_ene_81_lin)-2):ncol(temp_max_ene_81_lin)] <- c("key", "longitud", "latitud")

# Para julio de 1981 (también enmascarado para España)
temp_max_jul_81_lin <- terra::extract(temp_max_julio_1981_Spain, puntos_ocurr_lin_esp)
temp_max_jul_81_lin <- cbind(temp_max_jul_81_lin, 
                             ocurr_lin_esp_sinNA$key, 
                             ocurr_lin_esp_sinNA$decimalLongitude, 
                             ocurr_lin_esp_sinNA$decimalLatitude)
colnames(temp_max_jul_81_lin)[(ncol(temp_max_jul_81_lin)-2):ncol(temp_max_jul_81_lin)] <- c("key", "longitud", "latitud")



##' Extrae las ocurrencias del lince que tienen lugar dentro de los 
##' Parques Nacionales
##' de España 


# Convierto el data frame a un objeto espacial sf

lince_sf <- st_as_sf(ocurr_lin_esp_sinNA, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

st_crs(lince_sf)
st_crs(parques)#Tiene diferente sistema de referencia, la corrigo,


parques_corregido <- st_transform(parques, st_crs(lince_sf))


lince_en_parques <- st_intersection(lince_sf, parques_corregido) #Me quedo con los valores de lince_sf que caen dentro de los poligonos de los parques

#Compruebo si ha funcionado 
ggplot() +
  geom_sf(data = parques_corregido, fill = "lightgreen") +
  geom_sf(data = lince_en_parques, color = 'red', size = 0.5) #Veo que no hay puntos fuera de los parques


##' Calcula un mapa raster con la media de las temperaturas máximas 
##' de enero de 1981
##' y de julio de 1981
 

temp_max_mean_ene_1981 <- app(temp_max_enero_1981_Spain, mean, na.rm = TRUE) #calculo la media de cada pixel para las 31 capas
temp_max_mean_ene_1981 #Compruebo que solo tengo 1 capa resultante
#Repito el proceso para julio
temp_max_mean_jul_1981 <- app(temp_max_julio_1981_Spain, mean, na.rm = TRUE)
temp_max_mean_jul_1981

##' transforma los mapas con esas medias correspondientes a 1981 a la resolución, extent
##'   y proyección utilizada por los mapas mensuales que descargaste de github

temp_max_mean_ene_1981_reproyect <- project(temp_max_mean_ene_1981, tempmensualenero, method = "bilinear") #Project ajusta todo en SpatRasters, y el method billinear es para variables continuas
temp_max_mean_jul_1981_reproyect <- project(temp_max_mean_jul_1981, tempmensualjulio, method = "bilinear")


## 3. Análisis: Realiza los análisis necesarios para resolver los siguientes problemas ####
###################################################################################################### ##

##' 3.a. Compara (p.ej. correlaciones, diagrama de dispersión) las temperaturas
##' experimentadas por el lince dentro del Parque Nacional de Doñana en enero y julio 
##' de 1981. 


unique(lince_en_parques$sitename) #Todas las observaciones de lince en parques estan en doñana, auna si me aseguro
lince_en_doñana <- subset(lince_en_parques, sitename == "Doñana")
colnames(lince_en_doñana) #No tengo columna de latitud y longitud porque la perdi al pasarla a sf, pero tengo la columna de geometria, resultado de pasar la tabla a sf a partir de la longitud y latitud, por lo tanto puedo sacar de alli la longitud y latitud. 



#Puedo comparar todas las tempertauras maximas (31 temperaturas para cada observación de lince) o las medias de las temperaturas maximas (una temperatura por cada lince) Voy a hacer ambas



# Primero para las medias de las temperaturas maxima de cada lince: 
#Extraigo las coordenadas a partir de la columna de geometria
lince_en_doñana <- lince_en_doñana %>%
  mutate(latitud = st_coordinates(.)[, 2],  # Extrae la latitud (Y)
         longitud = st_coordinates(.)[, 1])  # Extrae la longitud (X)

puntos_ocurr_lin_doñana <- data.frame(x = lince_en_doñana$longitud,
                                   y = lince_en_doñana$latitud)
#Extraigo la temperatura media maxima de los puntos de lince en doñana
temp_max_mean_ene_lin_doñana <- terra::extract(temp_max_mean_ene_1981_reproyect, puntos_ocurr_lin_doñana)
temp_max_mean_ene_lin_doñana <- cbind(temp_max_mean_ene_lin_doñana, lince_en_doñana$key)
colnames(temp_max_mean_ene_lin_doñana)[ncol(temp_max_mean_ene_lin_doñana)] <- "key"

temp_max_mean_jul_lin_doñana <- terra::extract(temp_max_mean_jul_1981_reproyect, puntos_ocurr_lin_doñana)
temp_max_mean_jul_lin_doñana <- cbind(temp_max_mean_jul_lin_doñana, lince_en_doñana$key)
colnames(temp_max_mean_jul_lin_doñana)[ncol(temp_max_mean_jul_lin_doñana)] <- "key"

temp_max_mean_global_lin_doñana_1981 <- merge(temp_max_mean_ene_lin_doñana, temp_max_mean_jul_lin_doñana, by = "key")  #Uno los dataframe para plotearlos
#Dispersion de ambas tempertauras medias maximas
ggplot(temp_max_mean_global_lin_doñana_1981, aes(x = mean.x, y = mean.y)) +
  geom_point(alpha = 0.6, shape = 0.1) +
  labs(x = "Temperatura de enero", y = "Temperatura de julio") +
  theme_minimal() 
#Salen pocos puntos, puede que esten muy solapados. 
#Visualizo la media de cada lince en enero y julio 
ggplot(temp_max_mean_global_lin_doñana_1981,aes(x = key , y = mean.x)) +
  geom_point() +
  labs(x = "Observaciones de Lince", y = "Temperatura en enero", title = "Temperaturas Lince Enero en Doñana") +
  theme_minimal() +
  scale_x_discrete(labels = NULL) #Muchos pontos con la misma temp, por eso los puntos solapados. 

ggplot(temp_max_mean_global_lin_doñana_1981,aes(x = key , y = mean.y)) +
  geom_point() +
  labs(x = "Observaciones de Lince", y = "Temperatura en julio", title = "Temperaturas Lince Julio en Doñana") +
  theme_minimal() +
  scale_x_discrete(labels = NULL) #Lo mismo. 
#Paso a formato tiddy y asi representarlos juntos: 
datos_largos <- pivot_longer(temp_max_mean_global_lin_doñana_1981, cols = c( mean.x,mean.y), 
                             names_to = "Mes", values_to = "Temperatura")
# Crear el diagrama de dispersión
ggplot(datos_largos, aes(x = reorder(key, Temperatura), y = Temperatura, color = Mes)) +
  geom_point(size = 2, alpha = 0.7) +  
  labs(title = "Temperaturas de dos meses", 
       x = "Individuo", 
       y = "Temperatura", 
       color = "Mes") + 
  scale_color_manual(values = c("mean.x" = "red", "mean.y" = "cyan"), 
                     labels = c("Enero", "Julio")) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(labels = NULL)



#Ahora para todas las temperaturas experimentadas por el  lince (31 temperaturas por cada observación): ya tengo extraido todos los puntos de lince en España, puedo separarlas por las que estan en doñana:

# Filtrar linces dentro del Parque Nacional de Doñana
lince_doñana_temp_ene <- subset(temp_max_ene_81_lin, key %in% lince_en_doñana$key)
lince_doñana_temp_jul <- subset(temp_max_jul_81_lin, key %in% lince_en_doñana$key)
lince_ene_tidy <- lince_doñana_temp_ene %>%   #Paso a formato tidy la tabla de enero 
  pivot_longer(cols = starts_with("tx_"),
               names_to = "dia",
               values_to = "temperatura") %>%
  mutate(dia = as.numeric(gsub("tx_", "", dia)),
         mes = "enero")
# Paso la tabla de julio a formato tidy
lince_jul_tidy <- lince_doñana_temp_jul %>%
  pivot_longer(cols = starts_with("tx_"),
               names_to = "dia",
               values_to = "temperatura") %>%
  mutate(dia = as.numeric(gsub("tx_", "", dia)),
         mes = "julio")
#Quiero crear un dataframe con las 2 temperatura para cada dia, sin embaargo no las puedo unir ya que al pasar a formato tidy los dataframe, los dias pasaron a ser el numero que acompaña a tx(capa del raster incial), siendo de 548 a 578 los dias de julio, y de  367 a 397, habiendo una diferencia de 181 dias, entre ambos, resto esta diferneciaa julio para que los días coincidan y poder agrupar por día. 

lince_jul_tidy$dia <- lince_jul_tidy$dia - 181  
str(lince_jul_tidy)
str(lince_ene_tidy)
# Uno ambos dataframe
lince_comparacion <- merge(lince_ene_tidy, lince_jul_tidy, 
                           by.x = c("key", "dia"), by.y = c("key", "dia"), 
                           suffixes = c("_ene", "_jul")) #añade un sufijo para distingirlos al hacer el merge  
str(lince_comparacion)

ggplot (lince_comparacion, aes(x = temperatura_ene, y = temperatura_jul)) +
  geom_point(shape = 1, alpha = 0.5) +
  labs(x = "Temperatura de enero", y = "Temperatura de julio", title = "Temperaturas maximas del lince en los mese de enero y julio") +
  theme_minimal() 

##' 3.b. Haz un mapa que compare la media de las temperaturas máximas del mes de 
##' enero de 1981 con las temperaturas medias del mes de enero. ¿en qué parte de 
##' España las diferencias entre máximas del 81 y medias son más pequeñas? ¿Dónde
##' son más grandes?   
##' Pista: usa operaciones aritméticas

#Tengo ambos rasters, puedo restarlos haciendo que cada pixel devuelva la diferencia. 

Mapa_dif_temps <- (temp_max_mean_ene_1981_reproyect - tempmensualenero)
plot(Mapa_dif_temps, main = "Diferencia entre temperaturas máximas (1981) y medias (Enero)") 
#Como he restado de las medias maximas las medias mensuales, los valores más altos del raster indican que la maximas estuvieron muy por encima de la media, mientras que los valores mas bajos son los puntos donde las medias maximas estuvieron por debajo de las medias mensuales. 

##' 3.c. ¿En qué parque nacional fueron mayores las diferencias entre la media de las
##' temperaturas máximas de julio de 1981 y las temperaturas medias de enero?   

#Hago lo mismo que antes, pero cortando el mapa con los parques
Mapa_dif_temps_jul <- (temp_max_mean_jul_1981_reproyect - tempmensualjulio)
plot(Mapa_dif_temps_jul, main = "Diferencia entre temperaturas máximas (1981) y medias (julio)") 

mapa_dif_temps_parques <- mask(Mapa_dif_temps_jul, parques_corregido)

plot(mapa_dif_temps_parques) #Me falta saber que parques son, me interesa poner las etiquetas de los parques

df_raster <- as.data.frame(mapa_dif_temps_parques, xy = TRUE)


raster_extent <- ext(mapa_dif_temps_parques) #Para parque corregi la proyección pero no la extensión, para las etiquetas de los nombres de los parques me interesa la misma extension
parques_corregido_reext <- st_crop(parques_corregido, raster_extent )
    
ggplot() +
  geom_tile(data = df_raster, aes(x = x, y = y, fill = mean)) +
  scale_fill_viridis_c(name = "Diferencia Temp") +
  geom_label_repel(data = parques_corregido_reext, aes(label = sitename, geometry = geometry), stat = "sf_coordinates", size = 3) +
  theme_minimal() 

# Las diferencias de temperaturas entre la media de las temperaturas maximas de julio fue mayor en el parque nacional de sierra nevada. 



## 4. Bonus ##


##' En unos días habrá un nuevo mapa cargado en la carpeta de datos
##' de github. Cárgalo en R, multiplícalo por el mapa que has generado
##' en el apartado 3b, y visualízalo. ¿Qué ha pasado? ¿Serías capaz de
##' generar un resultado parecido, pero con una imagen propia? Recuerda
##' que una fotografía, no deja de ser un mapa raster...

bonus <- rast("hackathon2025/bonus.tif")
plot(bonus, main = "Bonus") 

multiplicacion <- (bonus * Mapa_dif_temps)
plot(multiplicacion, main = "Multiplicación")

#El raster de bonus incluia pixeles con valores de 0 o muy cercanos a 0, por tanto al multplicar los raster, todos estos pixeles devuelven un valor de 0, salvo los que en el mapa de bonus si tenían valores. 
#Para lograr un imagen similar podemos subir una fotografia, pasarla a raster y reclasificar los pixeles en función de lo que queramos. 
library(jpeg)

img <- readJPEG("hackathon2025/carlos_barreno/imagenpropia.jpg")

# Convertir la imagen a raster
raster_img <- raster(img) #No se puede
dim(img)  #el numero 3 es por que hay 3 capas, R, G, B, me interesa tener solo 1

# hago el promedio de las 3 bandas
gray_img <- (img[,,1] + img[,,2] + img[,,3]) / 3

#Reclasificolos pixeles para por ejemplo tener valores de 1 y de 0 

# Definir un umbral para reclasificar mas facil
umbral <- 0.5


img_0_1 <- ifelse(gray_img > umbral, 1, 0) #Si el pixel es mayor a 0.5 lo convertira en 1 y si no, lo pasara a 0
raster_imag <- raster(img_0_1)

plot(raster_imag) #visualizo la imagen, ahora tengo pixeles con 0 y con 1, al multiplicar, deberia desaparecer los pixeles con valores de 0 (El centro de la imagen)


# Ajusto la extensión de la imagen 
extent(raster_imag) <- extent(Mapa_dif_temps) #No deja porque son de diferente tipo, 
raster_imag <- rast(raster_imag) #cambio el tipo del raster de la imapgen a spatraster

# Vuelvo a intentar 
ext(raster_imag) <- ext(Mapa_dif_temps) #ahora si deja


# Ajusto la resolución para tener el mismo numero de filas y columnas de pixeles
raster_imag <- resample(raster_imag, Mapa_dif_temps, method="bilinear")

#Multiplico ambos raster
resultado <- Mapa_dif_temps * raster_imag
plot(resultado) #Los pixeles del centro de la imagen han pasado a ser 0. y el resto se han conservado.Esto podria ser util si tenemos un mapa con poligonos o lineas que querramos elimanar de un mapa..... 




