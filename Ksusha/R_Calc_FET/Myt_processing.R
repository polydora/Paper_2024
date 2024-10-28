library(readxl)
library(ggplot2)
library(dplyr)


myt <- read_excel("Data/Fet.xlsx")

myt <-
myt %>%
  mutate(Prop_T = NT_Full/(NT_Full + NE_Full))



ggplot(myt, aes(x = Fucoid, y = Prop_T)) +
  geom_boxplot()



ggplot(myt, aes(x = (NE_Full+NT_Full)/Weight_Fucoid, y = Prop_T, color = Fucoid)) +
  geom_point(size = 4) + 
  geom_smooth(method = "lm")




