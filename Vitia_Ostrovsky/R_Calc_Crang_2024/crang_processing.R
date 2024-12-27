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



community <- read_excel("Data/Crang_2024.xlsx", sheet =  na = "NA")






crang <- Crang_2024 %>%
  select(-c(ID, W, L_Car, Lat, Lon)) %>%
  group_by(Area, Sample) %>%
  summarise(across(everything(), mean), .groups = 'drop')

ord_crang <- metaMDS(crang[,-c(1:2)], distance = "bray")

plot(ord_crang, type = "t")

mds_points <- data.frame(scores(ord_crang)$sites)

mds_points$Area <-crang$Area
ggplot(mds_points, aes(  NMDS1, NMDS2,t, color = Area)) +
  geom_point(size = 4)

