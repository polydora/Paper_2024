library(readxl)
library(ggplot2)
library(dplyr)
library(reshape2)


myt <- read_excel("Data/Fet.xlsx")

myt$Site <- factor(myt$Site)

myt <-
myt %>%
  mutate(Prop_T = NT_Full/(NT_Full + NE_Full))


myt$Fucoid <- factor(myt$Fucoid)

myt <- 
  myt %>% 
  mutate(Dens_E = NE_Full/Weight_Fucoid, Dens_T = NT_Full/Weight_Fucoid, Total_Dens = Dens_E + Dens_T)


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


