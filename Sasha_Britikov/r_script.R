library(readxl)
library(ggplot2)
library(dplyr)
library(broom)
library(clipr)

pelets <- read_excel('Tables/Pelets_2024.xlsx', sheet = "Пеллеты улиток")

ggplot(pelets, aes( y = Pell, fill = Species)) + geom_boxplot()  +
  facet_grid(Type~ Type_2)


pelets %>%
  filter(Type == "Caged") %>%
  ggplot(., aes(x = Size, y = Pell, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Type_2)

pelets %>%
  filter(Type == "Caged") %>%
  ggplot(., aes(x = Species, y = Pell, fill = Species)) +
  geom_boxplot()  +
  facet_wrap(~Type_2)

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


Model_caged <- aov(log(Pell +1) ~ Type_2*Species, data = df)
summary(Model_caged)

plot(Model_caged)

hist(residuals(Model_caged))

TukeyHSD(Model_caged)


summary(post_hock)

version
