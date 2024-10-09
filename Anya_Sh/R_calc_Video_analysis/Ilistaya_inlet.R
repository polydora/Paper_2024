library(ggplot2)
library(readxl)
library(dplyr)
library(cowplot)
library(patchwork)
library(reshape2)
library(grid)

shore <- read_excel("Data/Координаты Илистой губы.xlsx", sheet = "Shoreline")
piles <- read_excel("Data/Координаты Илистой губы.xlsx", sheet = "piles")
standard_stations <- read_excel("Data/Координаты Илистой губы.xlsx", sheet = "Standard stations")



piles_long <-   
   piles %>% melt %>% select(-Pile)

piles1 <- piles [1:6, ]
piles2 <- piles [7:12, ]

piles3 <- merge(piles1, piles2, by = "Line")

video <- read_excel("Data/Обработка видео.xlsx")

# video <- 
#   video %>% 
#   filter(!Video_station %in% 1:14)

video_full <- video[complete.cases(video), ]


complete.cases(video)

ggplot(data = shore, aes(x = E, y = N)) +
  geom_path(aes( color = depth)) +
  geom_point(data = video_full, aes(size  = (Filamentous_Algae)))


ggplot(data = video_full, aes(x= Depth , y=Laminaria))+
geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))
 

ggplot(data = video_full, aes(x= Depth , y=Filamentous_Algae))+
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

ggplot(data = video_full, aes(x= Depth , y=Asterias_in_frame))+
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))


#####################################

depth <- read_excel("Data/Координаты Илистой губы.xlsx", sheet = "depth")
 
ggplot(depth, aes(x = E, y = N)) + 
  geom_point(aes(color = Depth)) +
  scale_color_gradient(low = "cyan", high = "darkblue") +
  geom_path(data = shore, aes(group = depth))


tide <- read_excel("Data/tide_16_08_2023.xlsx")

library(lubridate)

tide$Hour <- hour(tide$Time)

plot(tide$Hour, tide$Depth)

library(mgcv)
library(gratia)


tide_model <- gam(Depth ~ s(Hour), data = tide)

draw(tide_model)


depth$Hour <- hour(depth$Time)


depth$Tide_height <- predict(tide_model, newdata = depth, type = "response")

depth$True_depth <- depth$Depth - depth$Tide_height


ggplot(depth %>% filter(True_depth > 0), aes(x = E, y = N)) + 
  geom_point(aes(color = True_depth)) +
  scale_color_gradient(low = "cyan", high = "darkblue") +
  geom_path(data = shore, aes(group = depth))

