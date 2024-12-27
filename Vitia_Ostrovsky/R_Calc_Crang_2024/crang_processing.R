library(readxl)
library(vegan)
library(dplyr)

Crang_2024 <- read_excel("Data/Crang_2024.xlsx", na = "NA")

str(Crang_2024)




Crang_2024 %>%
  select(-c(Sample, ID, W, L_Car, Lon, Lat)) %>% 
  group_by(Area, Site) %>% 
  summarise_all(.funs = "mean") ->
  crang


ord_crang <- metaMDS(crang[ ,-c(1:2) ], distance = "bray")

plot(ord_crang, type = "t")


library(ggplot2)

mds_points <- data.frame(scores(ord_crang)$sites)

mds_points$Area <- crang$Area

ggplot(mds_points, aes(NMDS1, NMDS2, color=Area)) +
  geom_point(size = 4)



community <- read_excel("Data/Crang_2024.xlsx", sheet = "Чистовые",  na = "NA")

community %>% 
  group_by(Area, Site) %>% 
  select(-ID) %>% 
  summarise_all(.funs = "mean") ->
  comm





ord_comm <- metaMDS(comm[,-c(1:2)], distance = "bray")

plot(ord_comm, type = "t")

mds_points_comm <- data.frame(scores(ord_comm)$sites)

mds_points_comm$Area <-comm$Area
ggplot(mds_points_comm, aes(  NMDS1, NMDS2,t, color = Area)) +
  geom_point(size = 4)

