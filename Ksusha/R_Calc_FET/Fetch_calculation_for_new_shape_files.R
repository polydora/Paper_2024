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
library(waver)
library(readxl)

# Вычисляем fetch для точек сбора 2024 #################





# read shape file into R
murm_shape <- readShapeSpatial("Maps/Murmanskaya_obl/boundary-polygon-land-lvl4.shp", proj4string = CRS("+proj=longlat +datum=WGS84"))

karel_shape <- readShapeSpatial("Maps/Karelia/boundary-polygon-land-lvl4.shp", proj4string = CRS("+proj=longlat +datum=WGS84"))


# Объединение шейп-файлов
library(raster)

Kand_shape <- bind(murm_shape, karel_shape)

plot(Kand_shape)




# Карта для ggplot
gg_murm_karel <- fortify(Kand_shape)



Kand_x <- c(32.434, 32.7)
Kand_y <- c(66.94, 67.059)

Kand_map <- 
  ggplot(gg_murm_karel, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "gray50") +
  # coord_map(xlim = c(150., 151.52), ylim = c(59.45, 59.8) )+
  coord_map(xlim = Kand_x, ylim = Kand_y) +
  theme_bw()

Kand_map


## Данные по поселениям мидий

# points <- read_excel("data/TuMyt_2009_2010_for_SDM.xlsx")

myt <- read_excel("Data/Fet.xlsx")



myt_site <- myt %>% 
  group_by(Site) %>% 
  select(Lat, Lon) %>% 
  summarise(Lat = mean(Lat), Lon = mean(Lon))

# Рисуем карту расположения точек сбора материала

Kand_map +
  geom_point(data = myt_site, aes(x = Lon, y = Lat, group = 1), fill = "yellow", shape = 21)





points <- myt_site %>% select(Site, Lat, Lon) 


points$long <- as.numeric(points$Lon)
points$lat <- as.numeric(points$Lat)

 points$long_corrected <- points$long
 points$lat_corrected <- points$lat

# Создем датафрейм с координатами точек 
fetch.df = data.frame(
  lon = points$long_corrected, 
  lat = points$lat_corrected,
  Site = points$Site)


fetch_locs = SpatialPoints(fetch.df[, 1:2], CRS(proj4string(murm_shape)))

# plot(fetch_locs)

library(waver)

fetch <- fetch_len_multi(pts = fetch_locs, bearings = c(0, 45, 90, 135, 180, 225, 270, 215), shoreline = Kand_shape,  dmax = 100000, spread = 0,  method = "btree", projected = FALSE)


fetch <- 
  cbind(points, fetch ) 

write.table(x = fetch, file = "Data/fetch.csv", dec = ",", sep = ";")


################## Поиск аномальных точек для Fetch-анализа по новому шейп-файлу ##############

# anomal_df <- 
#   fetch %>%
#   filter(!complete.cases(.))
# 
# Kand_map +
#   geom_point(data = anomal_df, aes(group = 1), shape = 21, color = "blue", fill = "yellow", size = 3)
# 
# anomal_x <- c(32.49, 32.51)
# anomal_y <- c(66.89, 66.9)
# 
# 
# anomal_sampl <- data.frame(long = 32.50190, lat = 66.896)
# 
# ggplot(gg_murm_karel, aes(x = long, y = lat, group = group)) + 
#   geom_polygon(fill = "gray20") +
#   coord_map(xlim = anomal_x, ylim = anomal_y) +
#   theme_bw() +
#   geom_point(data = anomal_sampl, aes(x = long, y = lat, group = 1), shape = 21, color = "blue", fill = "yellow")
# 
#######




