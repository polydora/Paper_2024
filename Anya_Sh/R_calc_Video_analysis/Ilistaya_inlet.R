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

names(video_full)




video_full %>% 
  select(Stattion, N, E,Laminaria) %>% 
  group_by(Stattion) %>% 
  summarise(N = mean(N), E = mean(E), Laminaria = mean(Laminaria)) %>% 
  filter(Laminaria !=0)-> df 

ggplot(data = shore, aes(x = E, y = N)) +
  geom_path(aes( color = depth)) +
  geom_point(data = df, aes(size  = Laminaria)) +
  ggtitle("Laminaria") +
  guides(color = "none", size = "none")

video_full %>% 
  select(Stattion, N, E,Fucus) %>% 
  group_by(Stattion) %>% 
  summarise(N = mean(N), E = mean(E), Fucus = mean(Fucus)) %>% 
  filter(Fucus !=0)-> df
ggplot(data = shore, aes(x = E, y = N)) +
  geom_path(aes( color = depth)) +
  geom_point(data = df, aes(size  = Fucus)) +
  ggtitle("Фукусы") +
  guides(color = "none", size = "none")

video_full %>% 
  select(Stattion, N, E,Filamentous_Algae) %>% 
  group_by(Stattion) %>% 
  summarise(N = mean(N), E = mean(E), Filamentous_Algae = mean(Filamentous_Algae)) %>% 
  filter(Filamentous_Algae !=0)-> df
ggplot(data = shore, aes(x = E, y = N)) +
  geom_path(aes( color = depth)) +
  geom_point(data = df, aes(size  = Filamentous_Algae)) +
  ggtitle("Нитчатые водоросли") +
  guides(color = "none", size = "none")










ggplot(data = shore, aes(x = E, y = N)) +
  geom_path(aes( color = depth)) +
  geom_point(data = video_full, aes(size  = (Asterias_number))) +
  ggtitle("Морские звезды") +
  guides(color = "none", size = "none")

ggplot(data = shore, aes(x = E, y = N)) +
  geom_path(aes( color = depth)) +
  geom_point(data = video_full, aes(size  = (Asterias_in_frame))) +
  ggtitle("Морские звезды в кадре") +
  guides(color = "none", size = "none")

ggplot(data = shore, aes(x = E, y = N)) +
  geom_path(aes( color = depth)) +
  geom_point(data = video_full, aes(size  = (Fucus))) +
  ggtitle("Фукусы") +
  guides(color = "none", size = "none")

ggplot(data = shore, aes(x = E, y = N)) +
  geom_path(aes( color = depth)) +
  geom_point(data = video_full, aes(size  = (Dead_macoma))) +
  ggtitle("Мертвые макомы")
  guides(color = "none", size = "none")

  ggplot(data = shore, aes(x = E, y = N)) +
    geom_path(aes( color = depth)) +
    geom_point(data = video_full, aes(size  = (Mytilus))) +
    ggtitle("Мидии")
  guides(color = "none", size = "none")
  
  ggplot(data = shore, aes(x = E, y = N)) +
    geom_path(aes( color = depth)) +
    geom_point(data = video_full, aes(size  = (Polychaeta))) +
    ggtitle("Полихеты")
  guides(color = "none", size = "none")
  
  ggplot(data = shore, aes(x = E, y = N)) +
    geom_path(aes( color = depth)) +
    geom_point(data = video_full, aes(size  = (Laminaria))) +
    ggtitle("Ламинария")
  guides(color = "none", size = "none")
  
  
ggplot(data = shore, aes(x = E, y = N)) +
  geom_path(aes(color = depth)) +
  geom_point(data = video_full, aes(size  = (Asterias_number)))




cover<- read_excel("Data/Площади водорослей в рамке.xlsx")



names(cover)


library(dplyr)

cover_mean <-
cover %>% 
  group_by(Station) %>% 
  summarise(E = mean(E), N = mean(N),Filamentous_Algae = mean(Filamentous_Algae),  Laminaria = mean(Laminaria),  Fucus = mean(Fucus))
  

ggplot(data = shore, aes(x = E, y = N)) +
  geom_path(aes( color = depth)) +
  geom_point(data = cover_mean, aes(size  = (Filamentous_Algae))) +
  ggtitle(" Площади нитчатых водорослей") +
  guides(color = "none", size = "none")


ggplot(data = shore, aes(x = E, y = N)) +
  geom_path(aes( color = depth)) +
  geom_point(data = cover_mean, aes(size  = (Fucus))) +
  ggtitle(" Площади фукусов") +
  guides(color = "none", size = "none")



ggplot(data = shore, aes(x = E, y = N)) +
  geom_path(aes( color = depth)) +
  geom_point(data = cover_mean, aes(size  = (Laminaria))) +
  ggtitle(" Площади ламинарии") +
  guides(color = "none", size = "none")



ggplot(data = video_full, aes(x= Depth , y=Laminaria))+
geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))
 

ggplot(data = video_full, aes(x= Depth , y=Filamentous_Algae))+
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

ggplot(data = video_full, aes(x= Depth , y=Asterias_in_frame))+
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))


row.names(video_full) <- video_full$Replication


df <-
video_full %>%
  select(Asterias_in_frame, Filamentous_Algae, Laminaria, Dead_macoma, Mytilus, Polychaeta, Fucus) %>% 
  filter(rowSums(.) !=0)
  

library(vegan)


ord <- metaMDS(df, distance = "jaccard")

plot(ord, display = "species", type = "t")
plot(ord, display = "sites", type = "t")



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

