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
  


arubens <- read_excel("Data/Astred24.xlsx", na = "NA", sheet = "Asterias rubens")

df <-
arubens %>%
  group_by(Type, Cage_ID) %>% 
  summarise(B = sum(W)) 

cage_param <-
merge(cage_param, df, all.x = T)


 
ggplot(data = cage_param, aes(x = Prop_T, y = Prop_Dead)) +
  geom_point(aes(color = Type), size = 4) +
  geom_smooth(method = "loess", se = T)
  
ggplot(data = cage_param, aes(x = B, y = Prop_Dead)) +
  geom_point()






