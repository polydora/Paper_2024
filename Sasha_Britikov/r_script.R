library(readxl)
library(ggplot2)
library(dplyr)
library(broom)
library(clipr)
library(installr)

updateR()

version

pelets <- read_excel('Tables/Pelets_2024.xlsx', sheet = "Пеллеты улиток")

pelets_for_grafs <- read_excel('Tables/Pelets_2024.xlsx', sheet = "Пеллеты улиток")

ggplot(pelets, aes( y = Pell, fill = Species)) + geom_boxplot()  +
  facet_grid(Type~ Substrate)

pelets %>%
  filter(Type == "Caged") %>%
  ggplot(., aes(x = Size, y = Pell, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Substrate)

pelets %>%
  filter(Type == "Caged") %>%
  ggplot(., aes(x = Species, y = Pell, fill = Species)) +
  geom_boxplot()  +
  facet_wrap(~Substrate)

pelets %>%
  filter(Type == "Caged") -> df

df2 <-
  df %>% filter(Pell !=0)

pelets %>%
  filter(Type == "Wild") %>%
  group_by(Species) %>%
  summarise(Mean_pel = mean(Pell))

df3 <-
df %>%
  mutate(Abundant_Pel = case_when(
    Species == "Obt" & Pell >=31 ~ 1,
    Species == "Obt" & Pell < 31  ~ 0,
    Species == "Sax" & Pell >= 16 ~ 1,
    Species == "Sax" & Pell < 16 ~ 0
  ))


df3 %>%
  group_by(Type_2, Species) %>%
  summarise(Prop = mean(Abundant_Pel))

Model_caged_prop <- glm(Abundant_Pel ~ Type_2*Species, data = df3, family = "binomial")

summary(Model_caged_prop)

plot(Model_caged_prop)

hist(residuals(Model_caged_prop))


Model_caged <- aov(log(Pell +1) ~ Substrate*Species, data = df)
summary(Model_caged)

plot(Model_caged)


hist(residuals(Model_caged))

TukeyHSD(Model_caged)


summary(post_hock)

version

# ggplot(pelets_for_grafs, aes(y = Pell, x = Size, fill = Substrate))
# + geom_line()
# + theme_bw()


snailsseparationsondiflevel <- read_excel('Tables/Pelets_2024.xlsx', sheet = "ОБилие литторин на уровнях")



snailsseparationsondiflevel<-
snailsseparationsondiflevel %>%
  mutate(ratio =  L.saxatilis/( L.saxatilis + L.obtusata))



ggplot(snailsseparationsondiflevel, aes(x = Real_H, y = ratio )) +
  geom_point() +
  facet_wrap(~Site, nrow = 2)



library(reshape2)

df <-
snailsseparationsondiflevel %>%
  select(-ratio) %>%
  melt(., id.vars = c("Date", "Description", "Site", "Sampling_Level", "Real_H", "Sample")) %>%
  group_by(Site, Real_H, variable) %>%
  summarise(mean_N = mean(value))




ggplot(df, aes(x = Real_H, y = log(mean_N + 1) )) +
  geom_point() +
  geom_line() +
  facet_grid(variable~Site)




`ggplot(snailsseparationsondiflevel, aes(x = Sampling_Level)) +
  geom_bar() +
  scale_fill_hue(c = 40) +
  facet_wrap(~ Site)

