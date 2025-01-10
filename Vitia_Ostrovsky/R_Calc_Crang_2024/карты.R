library(readxl)
outline <- read_excel("Data/Crang_2024.xlsx", sheet = "Outline_s",  na = "NA")
sampl <- read_excel("Data/Crang_2024.xlsx" , sheet = "Coordinates_s",  na = "NA")
  
Pl_map <-
  ggplot(outline, aes(x = Lon, y = Lat)) + 
  geom_path(aes(linetype = Type)) + 
  guides(linetype = "none") + 
  scale_linetype_manual(values = c(2,1, 3)) +
  theme_minimal()
    
Pl_map + geom_point(data = sampl) 
    
 
    