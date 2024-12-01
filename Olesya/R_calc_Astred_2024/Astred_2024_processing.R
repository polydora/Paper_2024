library(readxl)
library(dplyr)
library(ggplot2)


astred <- read_excel("Data/Astred24.xlsx", na = "NA", sheet = "Mussel")

astred <-
astred %>% 
  filter(!is.na(Type))


cage_param <- 
astred %>%
  group_by(Type, Cage_ID) %>% 
  summarise(Prop_T = mean(Morphotype == "t"), 
            Prop_Dead = mean(Status == "dead")) 
  
quantile(cage_param$Prop_T)


nrow(cage_param)

arubens <- read_excel("Data/Astred24.xlsx", na = "NA", sheet = "Asterias rubens")

df <-
arubens %>%
  group_by(Type, Cage_ID) %>% 
  summarise(B = sum(W), Abund = n()) 

nrow(df)

cage_param <-
merge(cage_param, df, all.x = T)

nrow(cage_param)
 
ggplot(data = cage_param, aes(x = Prop_T, y = Prop_Dead)) +
  geom_point(aes(color = Type), size = 4) +
  geom_smooth(method = "loess", se = T)

ggplot(data = cage_param, aes(x = Type, y = Prop_Dead)) +
  geom_boxplot() 


ggplot(data = cage_param, aes(x = Prop_T, y = B)) +
  geom_point(aes(color = Type), size = 4) +
  geom_smooth(method = "loess", se = T)

ggplot(data = cage_param, aes(x = Prop_T, y = Abund)) +
  geom_point(aes(color = Type), size = 4) +
  geom_smooth(method = "loess", se = T)


ggplot(data = cage_param, aes(x = B, y = Prop_Dead)) +
  geom_point()


individual_fate <-
merge(astred, cage_param, all.x = T)

individual_fate$Out <- ifelse(individual_fate$Status == "dead", 1,0)

individual_fate$Morphotype <- factor(individual_fate$Morphotype)

individual_fate$Cage_ID_2 <- paste(individual_fate$Type, individual_fate$Cage_ID, sep = "_")

individual_fate$Cage_ID_2 <- factor(individual_fate$Cage_ID_2)


library(mgcv)

Mod_B <- gam(Out ~ s(B, k=5), data = individual_fate, family = "binomial")

library(gamm4)

Mod_B <- gam(Out ~ s(Prop_T, bs = "cs", k = 5) , data = individual_fate, family = binomial(link = "logit"))


summary(Mod_B)

plot(Mod_B)

library(gratia)

draw(Mod_B)

plot(Mod_B, pages = 1)

appraise(Mod_B)

library(DHARMa)

simulateResiduals(Mod_B, plot = T)

summary(Mod_B)


