library(readxl)

library(ggplot2)
library(dplyr)
library(reshape2)
library(patchwork)
l



crang <- read_excel("Data/Crangon_2023.xlsx")


Pl_PointClass_2023 <-
ggplot(crang, aes(x = L_Car)) +
geom_histogram(binwidth = 0.5) +
theme_bw() +
labs(x = "Длина карапакса (мм)", y = "Частота")


Pl_Class_2023 <-
ggplot(crang, aes(y= W, x = L_Car)) +
geom_point() +
theme_bw()+
labs(x = "Длина карапакса (мм)", y = "Вес")



includ <- c("Empty",	"Oligochaeta",	"Harpacticoidea",	"Nematoda",	"Ostracoda", "Amphipoda") 

Pl_empty_2023 <-
  crang %>% 
  group_by(Class) %>% 
  summarise_at(vars(5:20), mean, na.rm = TRUE) %>% 
  filter(Class > 0) %>%  
  melt(id.vars = "Class", variable.name = "Species", value.name = "Freq") %>% filter(Species == "Empty") %>% 
  ggplot(., aes(x = Class, y = Freq, color="blue")) +
  geom_col() +
  theme_bw() +
  ggtitle("2023")+
  labs(x = "Класс",y = "Пусто")



crangon <- read_excel("Data/Crang 2022 full.xlsx")

Pl_empty_2022 <-
  crangon %>%
  group_by(Class) %>% 
  summarise_at(vars(3:18), mean, na.rm = TRUE) %>% 
  filter(Class > 0) %>%  
  melt(id.vars = "Class", variable.name = "Species", value.name = "Freq") %>%
  filter(Species == "Empty") %>% 
  ggplot(., aes(x = Class, y = Freq, color= "red")) +
  geom_col() + 
  theme_bw() +
  ggtitle("2022")+
  labs(x = "Класс",y = "Пусто")
crang <- read_excel("Data/Crangon_2023.xlsx")
ggarrange(Pl_empty_2022,Pl_empty_2023 + rremove("x.text"), 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)


includ <- c("Oligochaeta",	"Harpacticoidea",	"Nematoda",	"Ostracoda", "Amphipoda")


crang %>%
  group_by(Class) %>% 
  summarise_at(vars(5:20), mean, na.rm = TRUE) %>% 
  filter(Class > 0) %>%  
  melt(id.vars = "Class", variable.name = "Species", value.name = "Freq") %>%
  filter(Species %in% c("Oligochaeta",	"Harpacticoidea",	"Nematoda",	"Ostracoda", "Amphipoda")) %>% 
  ggplot(., aes(x = Class, y = Freq)) + 
  geom_col() +
  facet_wrap(~Species)+
  theme_bw()

  

  # install.packages("patchwork")


Pl_empty_2022 / Pl_empty_2023

Pl_PointClass_2023/ Pl_Class_2023








































                  