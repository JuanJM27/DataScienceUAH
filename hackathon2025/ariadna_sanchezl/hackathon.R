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
setwd("F:/hackathon2025")


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
theme_set(theme_bw())



## 1. Carga de datos espaciales ##
############################### ##

##' busca, descarga y lee datos de temperaturas máximas diarias para 
##' toda Europa, correspondientes al periodo 1980-1984
##' https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php#datafiles 
##' 
#aquí dentro del directorio tuve que hace Ctrl+Shift+H para poner mi carpeta "datos"
temp_max_dia <- rast("tx_ens_mean_0.1deg_reg_1980-1994_v30.0e.nc")


##' lee datos de Parques Nacionales de España (en github)
##' 
enps <- st_read("F:/hackathon2025/datos/ENP.shp")
enps


##' lee datos de temperaturas mensuales de España, para enero y julio (en github)
##' 
temp_men_enero <- rast("temp.Spain.jan07.tif")
temp_men_julio <- rast("temp.Spain.jul07.tif")


##' Descarga datos para el lince ibérico ("Lynx pardinus"), con el paquete rgbif
##' 
install.packages("rgbif")
library(rgbif)
lince <- occ_search(scientificName = "Lynx pardinus",
                    limit = 5000)


## 2. Manipulación de datos ##
############################### ##


##' Crea un objeto con los datos de temperaturas máximas de España del 
##' mes de enero 
##' de 1981. Repite para el mes de julio de 1981. Pista: tendras 
##' que recortar y enmascarar
##' los datos de E-OBS.

#Identificamos que dias pertenecen al año 1981

temp_max_dia <- rast("tx_ens_mean_0.1deg_reg_1980-1994_v30.0e.nc")

años <- 1980:1994

# Definir la duración de cada año (bisiesto o no)
dias_por_año <- ifelse(años %% 4 == 0 & (años %% 100 != 0 | años %% 400 == 0), 366, 365)

# Crear una lista donde almacenaremos los datos por año
tmedia_por_año <- list()

# Extraer datos año por año
inicio <- 1
for (i in seq_along(años)) {
  fin <- inicio + dias_por_año[i] - 1  # Determinar el rango de días
  tmedia_por_año[[as.character(años[i])]] <- subset(temp_max_dia, inicio:fin)
  inicio <- fin + 1  # Actualizar el inicio para el siguiente año
}

temp_max_81 <- tmedia_por_año[["1981"]]
temp_max_81
fechas <- as.Date(time(temp_max_81))

#identificamos cuales pertenecen a enero y cuales a julio

temp_max_enero <- subset(temp_max_81,which(format(fechas, "%m")=="01"))
temp_max_julio <- subset(temp_max_81,which(format(fechas, "%m")=="07"))

#Ahora para que estos datos sean solo de España los tenemos que recortar 
CCAA_sf <- esp_get_ccaa()

#cortamos el raster con los datos de las temp max de enero y de julio
tmaxenerospain <- crop(temp_max_enero, CCAA_sf)
plot(tmaxenerospain[[1]])

#en el plot aparece otros paises lo que no nos interesa por lo que la enmascaramos
mapamundo <- world(resolution = 2, 
                   path = "F:/hackathon2025/datos")
dev.off()
plot(mapamundo)
plot
mapsspain <- mapamundo[69,]
mapsspain <- mapamundo[mapamundo$NAME_0== "Spain",]
which(mapamundo$NAME_0 %in% c("Portugal", "Spain", "France"))
plot(mapsspain)

##recortar el mapa para la españa peninsular
mappeninsula <- crop(mapsspain, ext(-10,5,35,44))
plot(mappeninsula)
#obteniendo así los datos de enero de 1981 en España
tmaxenerospain <- mask(tmaxenerospain, mappeninsula)
plot(tmaxenerospain[[1]])


#y para julio 
tmaxjuliospain <- crop(temp_max_julio, CCAA_sf)
tmaxjuliospain <- mask(tmaxjuliospain, mappeninsula)
plot(tmaxjuliospain[[1]])


##' Crea un subset con los datos de ocurrencia del lince, solo para España
##' 

class(lince)
lincespain <- subset(lince$data, country == "Spain")
lincespain 
table(lince$data$country)
lince$data

points(x = lincespain$decimalLongitude,
       y = lincespain$decimalLatitude,
       col="salmon",pch=19,cex=0.7)

##' Extrae los datos de temperaturas mensuales de junio y enero para 
##' cada ocurrencia del lince en España. Repite para las temperaturas máximas de los
##'  días del mes de enero y del mes de julio de 1981
##' 
temp_enero_lince <- extract(temp_men_enero, data.frame(lincespain$decimalLongitude, lincespain$decimalLatitude))

temp_julio_lince <- extract(temp_men_julio, data.frame(lincespain$decimalLongitude, lincespain$decimalLatitude))

tmax_enero_lince <- extract(tmaxenerospain, data.frame(lincespain$decimalLongitude, lincespain$decimalLatitude))

tmax_julio_lince <- extract(tmaxjuliospain, data.frame(lincespain$decimalLongitude, lincespain$decimalLatitude))




##' Extrae las ocurrencias del lince que tienen lugar dentro de los 
##' Parques Nacionales
##' de España

ppnn <- subset(enps, figura_lp %in% c("Parque Nacional"))
lines(ppnn, col="black",lwd=1.5)

# Convertir datos del lince a un objeto sf (eliminando NA primero)
lincespain <- lincespain %>% filter(!is.na(decimalLongitude) & !is.na(decimalLatitude))
linces_sf <- st_as_sf(lincespain, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# Parques Nacionales tengan el mismo CRS
ppnn_sf <- st_transform(ppnn, st_crs(linces_sf))

# Extraer solo los linces dentro de los Parques Nacionales
linces_en_pnn <- st_filter(linces_sf, ppnn_sf)

nrow(linces_en_pnn) 
dev.off()
plot(st_geometry(ppnn_sf), col = "lightgreen", border = "darkgreen")
plot(st_geometry(linces_en_pnn), col = "red", pch = 19, add = TRUE)

##' Calcula un mapa raster con la media de las temperaturas máximas 
##' de enero de 1981
##' y de julio de 1981
 
media_enero_81 <- mean(tmaxenerospain, na.rm = TRUE)

media_julio_81 <- mean(tmaxjuliospain, na.rm = TRUE)

mapa_tmediamax_enero <- plot(media_enero_81)

mapa_tmediamax_julio <- plot(media_julio_81)



##' transforma los mapas con esas medias correspondientes a 1981 a la resolución, extent
##'   y proyección utilizada por los mapas mensuales que descargaste de github

mapa_tmediamax_enero <- plot(media_enero_81)
mapa_tmediamax_julio <- plot(media_julio_81)

temp_men_enero <- rast("temp.Spain.jan07.tif")
temp_men_julio <- rast("temp.Spain.jul07.tif")

crs_mensual <- crs(temp_men_enero)   # Proyección CRS
res_mensual <- res(temp_men_enero)   # Resolución espacial
ext_mensual <- ext(temp_men_enero)   # Extensión geográfica

# Reproyectar los mapas de temperatura media máxima de 1981
media_enero_81_proj <- project(media_enero_81, crs_mensual, method = "bilinear")
media_julio_81_proj <- project(media_julio_81, crs_mensual, method = "bilinear")

# Ajustar la resolución y extensión a la de los mapas mensuales
media_enero_81_final <- resample(media_enero_81_proj, temp_men_enero, method = "bilinear")
media_julio_81_final <- resample(media_julio_81_proj, temp_men_julio, method = "bilinear")

#Para transformar los mapas de temperatura media máxima de 1981, lo mejor es "bilinear" porque suaviza los valores sin distorsionarlos mucho

plot(media_enero_81_final)
plot(media_julio_81_final)




## 3. Análisis: Realiza los análisis necesarios para resolver los siguientes problemas ##
###################################################################################################### ##

##' 3.a. Compara (p.ej. correlaciones, diagrama de dispersión) las temperaturas
##' experimentadas por el lince dentro del Parque Nacional de Doñana en enero y julio 
##' de 1981. 
# Filtrar el Parque Nacional de Doñana
library(terra)
donana_sf <- subset(ppnn_sf, sitename == "Doñana")
view(donana_sf)
plot(donana_sf)
linces_donana <- st_intersection(linces_sf, donana_sf)
donana_enero <- terra::extract(tmaxenerospain, linces_donana)
donana_julio <- terra::extract(tmaxjuliospain, linces_donana)

enero81day <- colMeans(donana_enero, na.rm = TRUE)
julio81day <- colMeans(donana_julio, na.rm = TRUE)

df <- data.frame(Enero = c(enero81day), Julio = (julio81day))
df <- filter(df[-1,])
ggplot(df, aes(x = Enero, y = Julio)) + geom_smooth() + geom_point()




##' 3.b. Haz un mapa que compare la media de las temperaturas máximas del mes de 
##' enero de 1981 con las temperaturas medias del mes de enero. ¿en qué parte de 
##' España las diferencias entre máximas del 81 y medias son más pequeñas? ¿Dónde
##' son más grandes?   
##' Pista: usa operaciones aritméticas

# diferencia entre las temperaturas
diferencia_enero <- media_enero_81_final - temp_men_enero

plot(diferencia_enero, main = "Diferencia Temp Máx Ene 1981 - Temp Media Ene")

min_diff <- min(values(diferencia_enero), na.rm = TRUE)
max_diff <- max(values(diferencia_enero), na.rm = TRUE)
#entonces la diferencia más grande de temperatura es de 7.935, aprox unos 8ºC, pero sale en negativo porque era más grande el valor de Temperatura media del tif

idx_cero <- which.min(abs(values(diferencia_enero)))  # Índice del valor más cercano a 0
valor_cero <- values(diferencia_enero)[idx_cero]
cat("Diferencia más cercana a 0:", valor_cero, "\n")


# Obtener coordenadas del punto con mayor diferencia (más extrema en valor absoluto)
xy_max <- as.data.frame(xyFromCell(diferencia_enero, which.min(values(diferencia_enero))))

# Obtener coordenadas del punto con menor diferencia (más cercana a 0)
xy_cero <- as.data.frame(xyFromCell(diferencia_enero, idx_cero))

print(xy_max)  # Ver ubicación de la mayor diferencia
print(xy_cero) # Ver ubicación de la menor diferencia

# Graficar el raster de diferencias
plot(diferencia_enero, main = "Diferencias Temp Máx Ene 1981 - Temp Media Ene")

# Poner el punto de mayor diferencia en rojo
points(xy_max[,1], xy_max[,2], col = "red", pch = 19, cex = 1.5)

# Poner el punto de menor diferencia en azul
points(xy_cero[,1], xy_cero[,2], col = "blue", pch = 19, cex = 1.5)


library(mapSpain)
library(tidyverse)
library(sf)

cantabria_municipios <- esp_get_munic_siane(region = "Cantabria") %>%
  mutate(Provincia = esp_dict_translate(ine.prov.name, "es"))

# Obtener los municipios de Cantabria
cantabria_municipios <- esp_get_munic_siane(region = "Cantabria") %>%
  mutate(Name = name)

xy_max <- as.numeric(xy_max)

# Convertir xy_max en un objeto sf con el mismo CRS que los municipios
punto_max_sf <- st_as_sf(data.frame(lon = xy_max[1], lat = xy_max[2]), 
                         coords = c("lon", "lat"), crs = 4326)

# Transformar al CRS de los municipios (si es necesario)
punto_max_sf <- st_transform(punto_max_sf, st_crs(cantabria_municipios))

# Crear el mapa
ggplot() +
  geom_sf(data = cantabria_municipios, fill = "lightgray", color = "black") + # Municipios
  geom_sf(data = punto_max_sf, color = "red", size = 3) +  # Punto de mayor diferencia
  geom_sf_text(data = cantabria_municipios, aes(label = Name), size = 3, check_overlap = TRUE) + # Nombres de municipios
  labs(title = "Punto de Mayor Diferencia de Temperatura en Cantabria",
       subtitle = "Comparación Temp Máx Ene 1981 - Temp Media Ene") +
  theme_minimal()


andalucia_municipios <- esp_get_munic_siane(region = "Andalucía") %>%
  mutate(Name = name)

# Asegurar que xy_cero es un vector numérico
xy_cero <- as.numeric(xy_cero)

# Convertir xy_cero en un objeto sf con el mismo CRS que los municipios
punto_cero_sf <- st_as_sf(data.frame(lon = xy_cero[1], lat = xy_cero[2]), 
                          coords = c("lon", "lat"), crs = 4326)

# Transformar al CRS de los municipios (si es necesario)
punto_cero_sf <- st_transform(punto_cero_sf, st_crs(andalucia_municipios))

# Crear el mapa con ggplot2
ggplot() +
  geom_sf(data = andalucia_municipios, fill = "lightgray", color = "black") +  # Municipios
  geom_sf(data = punto_cero_sf, color = "blue", size = 3) +  # Punto de menor diferencia
  geom_sf_text(data = andalucia_municipios, aes(label = Name), size = 3, check_overlap = TRUE) + # Nombres de municipios
  labs(title = "Punto de Menor Diferencia de Temperatura en Andalucía",
       subtitle = "Comparación Temp Máx Ene 1981 - Temp Media Ene") +
  theme_minimal()

#Por lo tanto la mayor diferencia de temperaturas se da en Cantabría, para ser más concretos en Cabezón de Liébana 
#y la menor diferencia de temperaturas se da en Andalucía, para ser más exactos en Morón de la Frontera

##' 3.c. ¿En qué parque nacional fueron mayores las diferencias entre la media de las
##' temperaturas máximas de julio de 1981 y las temperaturas medias de enero?   

library(terra)
library(sf)
library(mapSpain)
library(dplyr)
library(ggplot2)

diferencia_jul_enero <- media_julio_81_final - temp_men_enero

difppnn <- terra::extract(diferencia_jul_enero, ppnn)
difppnn

#media <- sapply(difppnn, function(x) mean(x, na.rm = TRUE))

parque_final <- ppnn$sitename[which.max(difppnn$mean)]
cat(parque_final, "es el parque nacional con mayores diferencias")
print(parque_final)














## 4. Bonus ##
#################

##' En unos días habrá un nuevo mapa cargado en la carpeta de datos
##' de github. Cárgalo en R, multiplícalo por el mapa que has generado
##' en el apartado 3b, y visualízalo. ¿Qué ha pasado? ¿Serías capaz de
##' generar un resultado parecido, pero con una imagen propia? Recuerda
##' que una fotografía, no deja de ser un mapa raster...

bonus <- rast("F:/hackathon2025/datos/bonus.tif")
plot(bonus, main = "Bonus") 

multiplicado <- (bonus * diferencia_enero)
plot(multiplicado, main = "Multiplicación")

#El raster de bonus incluia pixeles con valores de 0 o muy cercanos a 0, por tanto al multplicar los raster, todos estos pixeles devuelven un valor de 0, salvo los que en el mapa de bonus si tenían valores. 
#Para lograr un imagen similar podemos subir una fotografia, pasarla a raster y reclasificar los pixeles en función de lo que queramos. 
library(jpeg)

img <- readJPEG("F:/hackathon2025/datos/imagencirculo.jpg")
img

# Convertir la imagen a raster
#raster_img <- raster(img) #No se puede
dim(img)  #el numero 3 es por que hay 3 capas, R, G, B, me interesa tener solo 1

# hago el promedio de las 3 bandas
gray_img <- (img[,,1] + img[,,2] + img[,,3]) / 3

dim(gray_img)

#Reclasificolos pixeles para por ejemplo tener valores de 1 y de 0 

# Definir un umbral para reclasificar mas facil
umbral <- 0.5

library(raster)
img_0_1 <- ifelse(gray_img > umbral, 1, 0) #Si el pixel es mayor a 0.5 lo convertira en 1 y si no, lo pasara a 0
img_0_1
imagen <- raster(img_0_1)

plot(imagen) #visualizo la imagen, ahora tengo pixeles con 0 y con 1, al multiplicar, deberia desaparecer los pixeles con valores de 0 (El centro de la imagen)


# Ajusto la extensión de la imagen 
raster_imag <- rast(imagen) #cambio el tipo del raster de la imagen a spatraster

# Vuelvo a intentar 
ext(raster_imag) <- ext(diferencia_enero) #ahora si deja


# Ajusto la resolución para tener el mismo numero de filas y columnas de pixeles
imag_fin <- resample(raster_imag, diferencia_enero, method="bilinear")
imag_fin

#Multiplico ambos raster
resultado <- imag_fin * diferencia_enero
plot(resultado) 
#Los pixeles del centro de la imagen han pasado a ser 0. y el resto se han conservado.Esto podria ser util si tenemos 
#un mapa con poligonos o lineas que querramos elimanar de un mapa..... 




