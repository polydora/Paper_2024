library(readxl)
library(ggplot2)
library(dplyr)
library(reshape2)
library(gratia)
library(mgcv)


myt <- read_excel("Data/Fet.xlsx")


fetch <- read.table("Data/fetch.csv", sep = ";", dec = ",")
  
fetch <-
fetch %>% 
  select(-c(long, lat, long_corrected, lat_corrected)) %>% 
  mutate(fetch = rowMeans(select(., 4:ncol(.)), na.rm = TRUE))

myt$Site <- factor(myt$Site)

myt <-
  myt %>%
  mutate(Prop_T = NT_Full/(NT_Full + NE_Full))

myt <- myt %>%
  mutate(Site = as.integer(Site))

myt <- 
  myt %>% 
  mutate(Dens_E = NE_Full/Weight_Fucoid, Dens_T = NT_Full/Weight_Fucoid, Total_Dens = Dens_E + Dens_T)

fetch %>% 
  select(Site, fetch) %>% 
  merge(., myt) -> fetch_myt_combined

# fetch_myt_combined %>% 
#   group_by(Site, Fucoid) %>% 
#   summarise(fetch = mean(mean_fetch),
#             Prop_T = mean(Prop_T, na.rm = T),
#             Dens_E = mean(Dens_E),
#             Dens_T = mean(Dens_T),
#             Total_Dens = mean(Total_Dens)) -> fetch_myt_combined

ggplot(fetch_myt_combined, aes(x = log(fetch), y = Prop_T, color = Fucoid)) +
  geom_point() +
  geom_smooth()

ggplot(fetch_myt_combined, aes(x = log(fetch), y = Dens_E, color = Fucoid)) +
  geom_point() +
  geom_smooth()

ggplot(fetch_myt_combined, aes(x = log(fetch), y = Dens_T, color = Fucoid)) +
  geom_point() +
  geom_smooth()


ggplot(fetch_myt_combined, aes(x = log(fetch), y = Total_Dens, color = Fucoid)) +
  geom_point() +
  geom_smooth()

myt$Fucoid <- factor(myt$Fucoid)

fetch_myt_combined$Fucoid <- factor(fetch_myt_combined$Fucoid)
#модель соотношения T и E

Mod <- gam(Prop_T ~ s(log(fetch), by = Fucoid, k = 6) + Fucoid + s(Total_Dens), data = fetch_myt_combined)

summary(Mod)

draw(Mod)

модели плотности

Mod_Dens_T <- gam(Dens_T ~ s(log(fetch), by = Fucoid, k = 4) + Fucoid, data = fetch_myt_combined)

summary(Mod_Dens_T)

draw(Mod_Dens_T)


Mod_Dens_E <- gam(Dens_E ~ s(log(fetch), by = Fucoid, k = 4) + Fucoid, data = fetch_myt_combined)

summary(Mod_Dens_E)

Pl_Dens_E_A <- draw(Mod_Dens_E,select = 1)

Pl_Dens_E_A <-
Pl_Dens_E_A + 
  theme_bw() +
  ggtitle("На A.nodosum", subtitle = NULL) +
  labs(x = "Логарифм величины fetc", y = "Частный эффеект")

Pl_Dens_E_F <- draw(Mod_Dens_E,select = 2)

Pl_Dens_E_F <-
Pl_Dens_E_F + 
  theme_bw() +
  ggtitle("На F.vesiculosus", subtitle = NULL) +
  labs(x = "Логарифм величины fetc", y = "Частный эффеект")

library(cowplot)

plot_grid(Pl_Dens_E_A, Pl_Dens_E_F)

#####################


myt %>% 
  filter(Site %in% c(8,9)) %>% 
  ggplot(aes(x = Fucoid, y = Dens_E, fill = Site)) +
  geom_boxplot()

myt %>% 
  filter(Site %in% c(8,9)) %>% 
  ggplot(aes(x = Fucoid, y = Dens_T, fill = Site)) +
  geom_boxplot()

myt %>% 
  select(Site, Fucoid, Dens_E, Dens_T) %>% 
  melt() %>% 
  ggplot(aes(x = Fucoid, y = value, fill = variable )) +
  geom_boxplot() +
  facet_wrap(~ Site)
  

myt %>% 
  filter(Site %in% c(8,9)) %>% 
  ggplot(aes(x = Fucoid, y = Prop_T, fill = Site)) +
  geom_boxplot()



ggplot(myt, aes(x = Fucoid, y = Dens_E)) +
  geom_boxplot()

ggplot(myt, aes(x = Fucoid, y = Dens_T)) +
  geom_boxplot()



