library(readxl)
library(vegan)
library(dplyr)
library(ggplot2)

Crang_2024 <- read_excel("Data/Crang_2024.xlsx", na = "NA")



str(Crang_2024)




Crang_2024 %>%
  select(-c(Sample, ID, W, L_Car, Lon, Lat)) %>% 
  group_by(Area, Site) %>% 
  summarise_all(.funs = "mean") ->
  crang


ord_crang <- metaMDS(crang[ ,-c(1:2) ], distance = "bray")

plot(ord_crang, type = "t")




mds_points <- data.frame(scores(ord_crang)$sites)

mds_points$Area <- crang$Area

ggplot(mds_points, aes(NMDS1, NMDS2, color=Area)) +
  geom_point(size = 4)+
  theme_bw()




community <- read_excel("Data/Crang_2024.xlsx", sheet = "Чистовые",  na = "NA")

community %>% 
  group_by(Area, Site) %>% 
  select(-ID) %>% 
  summarise_all(.funs = "mean") ->
  comm





ord_comm <- metaMDS(comm[,-c(1:2)], distance = "bray")

plot(ord_comm, type = "t")

mds_points_comm <- data.frame(scores(ord_comm)$sites)

mds_points_comm$Area <-comm$Area

ggplot(mds_points_comm, aes(  NMDS1, NMDS2,t, color = Area)) +
  geom_point(size = 4)+
  theme_bw()
################

crang %>% 
  select(Area,Site,Empty) ->
  diverse
  


crang %>% 
  ungroup() %>% 
  select(-c(Area,Site,Empty)) %>% 
  specnumber() ->
  diverse$Spec_Num


crang %>% 
  ungroup() %>% 
  select(-c(Area,Site,Empty)) %>% 
  diversity() ->
  diverse$H_crang


comm %>% 
  ungroup() %>% 
  select(-c(Area, Site)) %>% 
  diversity()->
  diverse$H_com

ggplot(diverse, aes(x = H_com, y = Spec_Num))+
  geom_point(aes(color = Area), size = 4)+ 
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("red", "blue")) +
  labs(x = "Разнообразие в сообществе", y = "Количество видов в питании")


ggplot(diverse, aes(x = H_com, y = Empty))+
  geom_point(aes(color = Area))+ 
  geom_smooth(method = "lm")+ 
theme_bw()  

ggplot(diverse, aes(x = H_com, y = H_crang))+
  geom_point(aes(color = Area))+
  geom_smooth(method = "lm")



comm%>%
  ungroup() %>% 
  select(-c(Area, Site)) %>% 
  rowSums() ->
  diverse$comm_total_N

ggplot(diverse, aes(x = comm_total_N, y = H_crang))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(diverse, aes(x = comm_total_N, y = Spec_Num))+
  geom_point()+
  geom_smooth(method = "lm")




#########################
comm_dist <- (vegdist(comm[,-c(1:2)], method = "bray"))

dendr_comm <- as.dendrogram(hclust(d = comm_dist, method = "ward.D" ))

plot(dendr_comm)
p

crang_dist <- (vegdist(crang[,-c(1:2)], method = "bray"))
dendr_crang <- as.dendrogram(hclust(d = crang_dist, method = "ward.D" ))
plot(dendr_crang)

library(dendextend)


tanglegram(dendr_comm, dendr_crang, sort = TRUE)


untang_w <- untangle_step_rotate_2side(dendr_comm, dendr_crang, print_times = T)

tanglegram(untang_w[[1]], untang_w[[2]],
           highlight_distinct_edges = FALSE,
           common_subtrees_color_lines = F,
           main = "",
           main_left = "Сообщество",
           main_right = "Питание",
           columns_width = c(8, 1, 8),
           margin_top = 3.2, margin_bottom = 2.5,
           margin_inner = 4, margin_outer = 0.5,
           lwd = 1.2, edge.lwd = 1.2, 
           lab.cex = 1.5, cex_main = 2)
