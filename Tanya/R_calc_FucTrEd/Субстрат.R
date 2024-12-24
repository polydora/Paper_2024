library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)


myt_status <- read_excel('Data/FucTrEd.xlsx', sheet = 'Лист3')



myt_status %>% 
  pivot_longer(cols = -c(ID, Location, Status, Morphotype)  ) %>%
  uncount(value)  %>%
  as_tibble ->
  myt_outcome_1

myt_outcome_1 %>%
  mutate(
         Substrate = ifelse(Location == "Грунт", 0, 1)) ->
  myt_outcome_1





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
         d = N_Fuc - N_Asc,
         weight_algae = weight_Asc + weight_Fuc) %>% 
  filter(Open == 'закрытый') %>% 
  filter(N_final != 0)

myt <-
  myt %>% 
  filter(Type)

myt <- 
  myt %>% 
  select(ID, Prop_T, N_final, weight_Asc, weight_Fuc, weight_algae)

myt <-
  myt %>% 
  filter(ID != 34)



myt_outcome_1 <- merge(myt_outcome_1, myt, all.y = T)



mod_1 <- glm(Substrate ~  Prop_T*Morphotype + N_final*Morphotype +  weight_Asc*Morphotype  + weight_Fuc*Morphotype , data = myt_outcome_1 )

summary(mod_1)

drop1(mod_1)
mod_1_2 <- update(mod_1, .~. - Morphotype:weight_Fuc)
drop1(mod_1_2)
mod_1_3 <- update(mod_1_2, .~. - Prop_T:Morphotype)
drop1(mod_1_3)
mod_1_4 <- update(mod_1_3, .~. - Morphotype:N_final)
drop1(mod_1_4)
mod_1_5 <- update(mod_1_4, .~. - Morphotype:weight_Asc)
drop1(mod_1_5)
mod_1_6 <- update(mod_1_5, .~. - weight_Fuc )
drop1(mod_1_6)
mod_1_7 <- update(mod_1_6, .~. - Morphotype)

drop1(mod_1_7, test = "F")
mod_1_8 <- update(mod_1_7, .~. - N_final)

drop1(mod_1_8, test = "F")


summary(mod_1_8)



mod_2 <- glm(Substrate ~  Prop_T*Morphotype*N_final*weight_algae, data = myt_outcome_1 )
drop1(mod_2)
mod_2_2 <- update(mod_2, .~. - Prop_T:Morphotype:N_final:weight_algae)
drop1(mod_2_2)
mod_2_3 <- update(mod_2_2, .~. - Prop_T:N_final:weight_algae)
drop1(mod_2_3)
mod_2_4 <- update(mod_2_3, .~. - Prop_T:Morphotype:weight_algae)
drop1(mod_2_4)
mod_2_5 <- update(mod_2_4, .~. - Prop_T:weight_algae)
drop1(mod_2_5)
mod_2_6 <- update(mod_2_5, .~. - Morphotype:N_final:weight_algae)
drop1(mod_2_6)
mod_2_7 <- update(mod_2_6, .~. - Morphotype:weight_algae)
drop1(mod_2_7)
mod_2_8 <- update(mod_2_7, .~. - Prop_T:Morphotype:N_final)
drop1(mod_2_8)
mod_2_9 <- update(mod_2_8, .~. - Morphotype:N_final)
drop1(mod_2_9)
mod_2_10 <- update(mod_2_9, .~. - N_final:weight_algae)
drop1(mod_2_10)
mod_2_11 <- update(mod_2_10, .~. - N_final:weight_algae)
drop1(mod_2_11)

summary(mod_2_10)
#Визуализация mod_2_10

My_data <- expand.grid(N_final = myt_outcome_1$N_final, weight_algae = myt_outcome_1$weight_algae, Morphotype = c("T", "E"), Prop_T = seq(min(myt_outcome_1$Prop_T), max(myt_outcome_1$Prop_T), length.out = 20))


predicted <- predict(mod_2_10, newdata = My_data, se.fit = T)

My_data$Predicted <- predicted$fit
My_data$SE <- predicted$se.fit


ggplot(My_data, aes(x = Prop_T, y = Predicted)) + geom_line() + geom_ribbon(aes(ymin = Predicted - 2*SE, ymax = Predicted + 2*SE), alpha = 0.2)+ facet_wrap(~ Substrate)


mod_2_2 <- update(mod_2, .~. - Prop_T:Morphotype:N_final)
drop1(mod_2_2)
mod_2_3 <- update(mod_2_2, .~. - Morphotype:N_final)
drop1(mod_2_3)
mod_2_4 <- update(mod_2_3, .~. - Prop_T:N_final)
drop1(mod_2_4)
#
mod_2_5 <- update(mod_2_4, .~. - Prop_T:Morphotype)
drop1(mod_2_5)
mod_2_6 <- update(mod_2_5, .~. - Morphotype)
drop1(mod_2_6)
mod_2_7 <- update(mod_2_6, .~. - Prop_T)
drop1(mod_2_7)

summary(mod_2_7)


mod_2_2 <- update(mod_2, .~. - Prop_T:Morphotype:N_final:weight_Asc:weight_Fuc)
drop1(mod_2_2)
mod_2_3 <- update(mod_2_2, .~. - Prop_T:Morphotype:weight_Asc:weight_Fuc)
drop1(mod_2_3)
mod_2_4 <- update(mod_2_3, .~. - Morphotype:N_final:weight_Asc:weight_Fuc)
drop1(mod_2_4)
mod_2_5 <- update(mod_2_4, .~. - Prop_T:N_final:weight_Asc:weight_Fuc)
drop1(mod_2_5)
mod_2_6 <- update(mod_2_5, .~. - Prop_T:weight_Asc:weight_Fuc)
drop1(mod_2_6)
mod_2_7 <- update(mod_2_6, .~. - N_final:weight_Asc:weight_Fuc)
drop1(mod_2_7)



summary(mod_2_7)

 