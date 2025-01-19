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

library(dplyr)

dinfet %>% 
  group_by(F_Sp, Morphotype) %>% 
  do(data.frame(Branch = seq(min(.$Branch), max(.$Branch), 1))) %>% 
  mutate(Mussel_Weight = mean(dinfet$Mussel_Weight)) -> My_data
  

predicted <- predict(Mod, newdata = My_data, se.fit = TRUE)

My_data$Fit <- predicted$fit
My_data$SE <- predicted$se.fit
My_data$Upr <- My_data$Fit + 1.96*My_data$SE 
My_data$Low <- My_data$Fit - 1.96*My_data$SE



ggplot(My_data, aes(x = Branch, y = Fit)) + 
  geom_ribbon(aes(ymin = Low, ymax = Upr, fill = F_Sp), alpha = 0.3) +
  geom_line(aes(color = F_Sp), size = 1) +
  facet_wrap(~Morphotype) +
  scale_color_manual(values = c("red", "blue") )+
  theme_bw() +
  geom_point(data = dinfet, aes(y = log(Force), color = F_Sp)) + 
  labs(x = "Номер развилки", y = "Логарифм силы прикрепления")




############################

`drop1(Mod, test = "F")


Mod2 <- update(Mod, .~.- Branch:Morphotype:F_Sp)

drop1(Mod2, test = "F")

Mod3 <- update(Mod2, .~.- Branch:Morphotype)

drop1(Mod3, test = "F")

summary(Mod3)
