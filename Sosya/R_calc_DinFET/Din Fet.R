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



ggplot(data = dinfet, aes(x = (Branch), y = log(Mussel_Weight), color = F_Sp)) + 
  geom_point() + 
  facet_wrap(~Morphotype) + 
  geom_smooth(method = "lm")



Mod_L <- lm(L ~ Branch * Morphotype * F_Sp, data = dinfet)
summary(Mod_L)



Mod <- lm(log(Force) ~ Branch * Morphotype * F_Sp + Mussel_Weight, data = dinfet)

anova(Mod)

summary(Mod)



`drop1(Mod, test = "F")


Mod2 <- update(Mod, .~.- Branch:Morphotype:F_Sp)

drop1(Mod2, test = "F")

Mod3 <- update(Mod2, .~.- Branch:Morphotype)

drop1(Mod3, test = "F")

summary(Mod3)
