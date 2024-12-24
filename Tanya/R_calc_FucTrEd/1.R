library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(vegan)

#Загрузка данных
myt <- read_excel('Data/FucTrEd.xlsx', sheet = 'Лист2')


myt <-
myt %>% 
  mutate(NT =T_a_Asc + T_a_Fuc + T_a_bot + T_dead_Asc + T_dead_Fuc + T_dead_bot, 
         NE = E_a_Asc + E_a_Fuc + E_a_bot + E_dead_Asc + E_dead_Fuc + E_dead_bot,
         Prop_T = NT/(NT + NE),
         N_enitial = NT + NE,
         N_final = T_a_Asc + T_a_Fuc + T_a_bot + E_a_Asc + E_a_Fuc + E_a_bot,
         Prop_dead_T = (T_dead_Asc + T_dead_Fuc + T_dead_bot)/NT,
         Prop_dead_E = (E_dead_Asc + E_dead_Fuc + E_dead_bot)/NE,
         N_T_algae = T_a_Asc + T_a_Fuc + T_dead_Asc + T_dead_Fuc,
         N_E_algae = E_a_Asc + E_a_Fuc + E_dead_Asc + E_dead_Fuc,
         N_T_bot = T_a_bot + T_dead_bot,
         N_E_bot = E_a_bot + E_dead_bot,
         Prop_T_algae = N_T_algae /(N_T_algae + N_T_bot),
         Prop_E_algae = N_E_algae /(N_E_algae + N_E_bot),
         N_Fuc = T_a_Fuc + T_dead_Fuc + E_a_Fuc + E_dead_Fuc,
         N_Asc = T_a_Asc + T_dead_Asc + E_a_Asc + E_dead_Asc,
         Prop_Fuc = N_Fuc/(N_Asc + N_Fuc),
         Prop_Asc = N_Asc/(N_Asc + N_Fuc),
         Prop_algae = (N_T_algae + N_E_algae)/(NT + NE),
         Prop_dead = (T_dead_Asc + T_dead_Fuc + T_dead_bot + E_dead_Asc + E_dead_Fuc + E_dead_bot)/(NT + NE),
         Prop_dead = (T_dead_Asc + T_dead_Fuc + T_dead_bot)/NT,
           Prop_T_bottom = N_T_bot /(N_E_bot + N_T_bot),
         Prop_T_algae_2 = N_T_algae /(N_T_algae + N_E_algae),
           Diff = Prop_T_algae_2 - Prop_T_bottom,
         d = N_Fuc - N_Asc) %>% 
  filter(Type == 'закрытый') %>% 
  filter(N_final != 0)

# ggplot(myt, aes(x = NE+NT, y = 1:nrow(myt))) + 
#   geom_point()

myt <-
  myt %>% 
  filter(ID != 34)

range(myt$Prop_T)

#ggplot(myt, aes(x = Prop_T, y = Diff)) +
  #geom_point()+
  #geom_smooth()


#ggplot(myt, aes(x = (NT + NE), y = Diff)) +
  #geom_point()+
  #geom_smooth(method = "lm")
 



ggplot(myt, aes(x=N_enitial, y=Prop_dead_T)) +
  geom_point()+
  geom_smooth(method = "lm")
ggplot(myt, aes(x=N_enitial, y=Prop_dead_E)) +
  geom_point()+
  geom_smooth(method = "lm")

ggplot(myt, aes(x=N_final, y=Prop_dead_T)) +
  geom_point()+
  geom_smooth(method = "lm")
ggplot(myt, aes(x=N_final, y=Prop_dead_E)) +
  geom_point() +
  geom_smooth(method = "lm") 


# Доля Т на фукоидах. Значимо зависит от итоговой плотности N_final, Estimate > 0.
mod_1 <- lm(Prop_T_algae ~ Prop_T + N_final + weight_Asc + weight_Fuc, data = myt)
summary(mod_1)

drop1(mod_1)
mod_2 <- lm(Prop_T_algae ~ Prop_T + N_final + weight_Asc, data = myt)

drop1(mod_2)

mod_3 <- lm(Prop_T_algae ~ Prop_T + N_final, data = myt)
drop1(mod_3)

summary(mod_3)

My_data <- expand.grid(Prop_T = seq(min(myt$Prop_T), max(myt$Prop_T),length.out = 20), N_final = quantile(myt$N_final, probs = c(0.25, 0.5, 0.75)))

predicted <- predict(mod_3, newdata = My_data, se.fit = T)
My_data$Predicted <- predicted$fit
My_data$SE <- predicted$se.fit

ggplot(My_data, aes(x = Prop_T, y = Predicted))+
  geom_line()+
  geom_ribbon(aes(ymin = Predicted - 2*SE, ymax = Predicted + 2*SE), alpha = 0.2)+
  facet_wrap(~N_final)




###################


# Плотность Е на фукоидах. От этих факторов значимо не зависит.
mod2 <- lm(Prop_E_algae ~ Prop_T + N_final + weight_Asc + weight_Fuc, data = myt)
summary(mod2)

drop1(mod2)
mod2_1 <- lm(Prop_E_algae ~ Prop_T + N_final + weight_Asc, data = myt)
drop1(mod2_1)
mod2_2 <- lm(Prop_E_algae ~ N_final + weight_Asc, data = myt)
drop1(mod2_2)
mod2_3 <- lm(Prop_E_algae ~ weight_Asc, data = myt) 
drop1(mod2_3)

summary(mod2_3)

# Визуализация относительно Prop_T
ggplot(myt, aes(x=N_final, y = Prop_T_algae)) +
  geom_point()+
  geom_smooth() 

ggplot(myt, aes(x=N_final, y = Prop_E_algae)) +
  geom_point()+
  geom_smooth()


###################################
##################################




#Распределение по фукоидам. Выбор по весу. Чем больше вес Fucus, тем больше на нем мидий. Обратно с Ascophyllum

mod_3 <- lm(Prop_Fuc ~ Prop_T + N_final + weight_Fuc + weight_Asc, data = myt)
summary(mod_3)

ggplot(myt, aes(x = weight_Fuc, y = Prop_Fuc))+
  geom_point() +
  geom_smooth(method = "lm")
ggplot(myt, aes(x = weight_Asc, y = Prop_Fuc))+
  geom_point() +
  geom_smooth(method = "lm")

# То же, но для Prop_Asc

mod_5 <- lm(Prop_Asc ~ Prop_T + N_final + weight_Fuc + weight_Asc, data = myt)
summary(mod_5)


#Плотность на фукоидах обоих морфотипов. Значимо зависит от Prop_T, Estimate > 0

# mod_6 <- lm(Prop_algae ~ Prop_T + N_final + weight_Fuc + weight_Asc, data = myt)
# summary(mod_6)

#Визуализация
#ggplot(myt, aes(x = Prop_T, y = Prop_algae)) +
  #geom_point() +
  #geom_smooth(method = "lm")

#plot(mod_6)

# Prop_dead. Значимо ни от чего не зависит
mod_7 <- lm(Prop_dead ~  Prop_algae + N_final + weight_Asc + weight_Fuc, data = myt)
summary(mod_7)

drop1(mod_7)
mod7_1 <- lm(Prop_dead ~  Prop_algae + N_final + weight_Fuc, data = myt)
drop1(mod7_1)
mod7_2 <- lm(Prop_dead ~  Prop_algae + N_final, data = myt)
drop1(mod7_2)
mod7_3 <- lm(Prop_dead ~  N_final, data = myt)
drop1(mod7_3)
summary(mod7_3)

vif(mod_7)


# Чем меньше d, тем больше мидий на Ascophyllum. Уменьшается с повышением плотности мидий и веса пучка Ascophyllum. Увеличивается с увеичением веса пучка Fucus.
mod_8 <- lm(d ~  Prop_algae + N_final + weight_Asc + weight_Fuc, data = myt)
summary(mod_8)

ggplot(myt, aes(x = N_final, y = d)) +
  geom_point()+
  geom_smooth(method = "lm")
ggplot(myt, aes(x = weight_Asc, y = d)) +
  geom_point()+
  geom_smooth(method = "lm")
ggplot(myt, aes(x = weight_Fuc, y = d)) +
  geom_point()+
  geom_smooth(method = "lm")
  
 
ggplot(myt, aes(x=N_final, y = Prop_T, size = Prop_T_algae)) +
  geom_point()
 