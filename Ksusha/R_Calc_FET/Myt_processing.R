library(readxl)
library(ggplot2)
library(dplyr)


myt <- read_excel("Data/Fet.xlsx")

myt <-
myt %>%
  mutate(Prop_T = NT_Full/(NT_Full + NE_Full))


myt$Fucoid <- factor(myt$Fucoid)

myt$Fi_T <- 2*asin(sqrt(myt$Prop_T)) *180/pi

myt <- 
  myt %>% 
  mutate(Dens_E = NE_Full/Weight_Fucoid, Dens_T = NT_Full/Weight_Fucoid, Total_Dens = Dens_E + Dens_T)



ggplot(myt, aes(x = Fucoid, y = Prop_T)) +
  geom_boxplot()


ggplot(myt, aes(x = Weight_Fucoid, y = log(NE_Full), color = Fucoid)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm")
  
ggplot(myt, aes(x = Weight_Fucoid, y = log(NT_Full), color = Fucoid)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm")



myt %>% 
  ggplot(aes(x = Prop_T, y = Dens_T, color = Fucoid)) +
  geom_point() +
  geom_smooth()

myt %>% 
  ggplot(aes(x = Prop_T, y = Dens_E, color = Fucoid)) +
  geom_point() + 
  geom_smooth()


myt %>% 
  ggplot(aes(x = Lon, y = Lat)) +
  geom_point(aes(size = Prop_T), position = position_jitter(width = 0.01)) +
  facet_wrap(~ Fucoid)


myt %>% 
  ggplot(aes(x = log(Dens_E), y = log(Dens_T), color = Fucoid)) + 
  geom_point() +
  geom_smooth(method = "lm")

myt %>% 
  ggplot(aes(x = Fucoid, y = (log(Dens_E) - log(Dens_T)))) +
  geom_boxplot()
  

myt %>% 
  ggplot(aes(x = log(Dens_E + Dens_T), y = Prop_T, color = Fucoid)) +
  geom_point() +
  geom_smooth()

library(mgcv)
library(gratia)

mod <- gam(Fi_T ~ s((Total_Dens), by = Fucoid, k = 5) + Fucoid + s(Site, bs = "re"), data = myt)

appraise(mod)

summary(mod)

draw(mod)
