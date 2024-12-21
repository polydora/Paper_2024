
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)


myt <- read_excel('Data/FucTrEd.xlsx', sheet = 'Лист2')



myt <- 
  myt %>% 
  filter(Open == "закрытый")

names(myt)

myt <-
  myt %>% 
  mutate(N_total = T_a_Asc + T_a_Fuc + T_a_bot + T_dead_Asc + T_dead_Fuc + T_dead_bot + E_a_Asc + E_a_Fuc + E_a_bot + E_dead_Asc + E_dead_Fuc + E_dead_bot,
         NT = T_a_Asc + T_a_Fuc + T_a_bot + T_dead_Asc + T_dead_Fuc + T_dead_bot, 
         NE = E_a_Asc + E_a_Fuc + E_a_bot + E_dead_Asc + E_dead_Fuc + E_dead_bot,
         Prop_T = NT/(NT + NE))

hist(myt$N_total)

hist(myt$NE)


myt <- 
myt %>% 
  mutate(Density = factor(case_when(N_total <= 20 ~ "Low",
                             N_total > 20 & N_total < 60 ~ "Medium",
                             N_total >= 60 ~ "Nigh" )))
myt <-
  myt %>% 
  mutate(N_Algae = T_a_Asc + T_a_Fuc + T_dead_Asc + T_dead_Fuc + E_a_Asc + E_a_Fuc + E_dead_Asc + E_dead_Fuc,
         N_Bottom = T_a_bot + T_dead_bot + E_a_bot + E_dead_bot,
         N_Algae_alive = T_a_Asc + T_a_Fuc + E_a_Asc + E_a_Fuc,
         N_Bottom_alive = T_a_bot + E_a_bot,
         B_Algae = weight_Fuc + weight_Asc)


library(mgcv)
library(gratia)

mod <- gam(cbind(N_Algae, N_Bottom) ~ s(Prop_T, by = Density, k = 5) + Density  + s(B_Algae), data = myt, family = "binomial", method = "REML")

summary(mod)

draw(mod)


mod_alive <- gam(cbind(N_Algae_alive, N_Bottom_alive) ~ s(Prop_T, by = Density, k = 5) + Density  + s(B_Algae, by = Density, k = 5) , data = myt, family = "binomial", method = "REML")

summary(mod_alive)
draw(mod_alive)

appraise(mod)





T_a_Asc + T_a_Fuc + T_dead_Asc + T_dead_Fuc + E_a_Asc + E_a_Fuc + E_dead_Asc + E_dead_Fuc

ggplot(myt, aes(x = Density, y = (T_a_Asc + T_a_Fuc)/(T_a_Asc + T_a_Fuc + E_a_Asc + E_a_Fuc) )) +
  geom_boxplot()


ggplot(myt, aes(x = Density, y = (T_a_bot + T_a_bot)/(T_a_bot + T_a_bot + E_a_bot + E_a_bot) )) +
  geom_boxplot()

###############################################333

myt_status <- read_excel('Data/FucTrEd.xlsx', sheet = 'Лист3')

library(tidyr)
myt_status %>% 
  filter(Type == "ET") %>% 
  pivot_longer(cols = -c(ID, Location, Status, Morphotype, Type)  ) %>%
  uncount(value)  %>%
  as_tibble ->
  myt_outcome

myt_outcome %>%
  mutate(Out = ifelse(Status == "мертвые", 1, 0),
         Substrate = ifelse(Location == "Грунт", "Bottom", "Algae")) ->
  myt_outcome


df <- 
  myt %>% 
  select(ID, Density, Prop_T, B_Algae)

myt_outcome <- 
merge(myt_outcome, df)


Mod_deadness <- gam(Out ~ s(Prop_T, by = Density ) + Density + Morphotype, family = "binomial", data = myt_outcome, method = "REML")

summary(Mod_deadness)

appraise(Mod_deadness)

