library(readxl)
library(dplyr)
library(ggplot2)


dinfet <- read_excel("Data/Din Fet 2024.xlsx")

ggplot(data = dinfet, aes(x = Morphotype, y = log(Force) )) + 
  geom_boxplot(aes(fill = F_Sp))


ggplot(data = dinfet, aes(x = Site, y = log(Force), fill = F_Sp)) + 
  geom_boxplot() + 
  facet_wrap(~Morphotype)



ggplot(data = dinfet, aes(x = (Branch), y = log(Force), color = F_Sp)) + 
  geom_point() + 
  facet_wrap(~Morphotype) + 
  geom_smooth(method = "lm")

Mod <- lm(log(Force) ~ , data = )
