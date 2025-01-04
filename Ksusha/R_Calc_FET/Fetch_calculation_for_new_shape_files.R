# В этом скрипте делается попытка оценить приложимось модели, построенной для Белого моря к баренцевоморским данным


# Создание карты

library(sp)
library(dplyr)
library(mgcv)
library(reshape2)
library(ggplot2)
library(ggmap)
library(mapproj)
library(maps)

library(rgeos) #этот пакет содержит какую-то хрень, которая позволяет обойти проблему пр чтении фалов средсвами maptools. 
#Att! Этот пакет должен быть загружен до maptools

library(mapdata)
library(maptools) # Rgshhs
library(PBSmapping)
library(gridExtra)
library(grid)

library(akima)
library(car)
library(waver)
library(MuMIn)
library(readxl)

# Вычисляем fetch для точек сбора 2023 #################

# Задаем пределы координат для карт

# Tuva_x <- c(33.4,  33.65)
# 
# Tuva_y <- c(69.17, 69.23)
# 
# 
# Tuva_x_small <- c(33.56,  33.65)
# 
# Tuva_y_small <- c(69.17, 69.21)





# read shape file into R
# murm_shape <- readShapeSpatial("Maps/Murmanskaya_obl/boundary-polygon-land-lvl4.shp", proj4string = CRS("+proj=longlat +datum=WGS84"))
# 
# karel_shape <- readShapeSpatial("Maps/Karelia/boundary-polygon-land-lvl4.shp", proj4string = CRS("+proj=longlat +datum=WGS84"))
# 
# 
# # Объединение шейп-файлов
# library(raster)


# Kand_shape <- bind(murm_shape, karel_shape)
# 
# plot(Kand_shape)
# 


Kand_shape <- read.table("Data/ggKand_upper_2021.csv", sep = ",", header = TRUE)

# Карта для ggplot
gg_murm_karel <- Kand_shape

head(gg_murm_karel)

# Tuva_map <- 
# ggplot(gg_murm, aes(x = long, y = lat, group = group)) + 
#   geom_polygon(fill = "gray20") +
#   # coord_map(xlim = c(150., 151.52), ylim = c(59.45, 59.8) )+
#   coord_map(xlim = Tuva_x_small, ylim = Tuva_y_small) +
#   theme_bw()

library(ggplot2)

Kand_x <- c(32.4, 32.8)
Kand_y <- c(66.93, 67.05)


Kand_map <- 
  ggplot(gg_murm_karel, aes(x = Lon, y = Lat, group = group)) + 
  geom_polygon(fill = "gray20") +
  # coord_map(xlim = c(150., 151.52), ylim = c(59.45, 59.8) )+
  coord_map(xlim = Kand_x, ylim = Kand_y) +
  theme_bw()

Kand_map + 
  geom_point(data = myt, aes(group = 1), color = "blue")
# цветом размер ,запулить текст

## Данные по поселениям мидий
