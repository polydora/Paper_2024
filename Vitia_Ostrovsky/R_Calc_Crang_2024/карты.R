library(readxl)
outline <- read_excel("Data/Crang 2022 full.xlsx", sheet = "Outline",  na = "NA")
sampl <- read_excel("Data/Crang 2022 full.xlsx" , sheet = "Coordinats_South")
  
Pl_map <-
  ggplot(outline, aes(x = Lon, y = Lat)) + 
  geom_path(aes(linetype = Type)) + 
  guides(linetype = "none") + 
  scale_linetype_manual(values = c(2,1, 3)) +
  theme_minimal()+
  geom_point(data = sampl) 

    Pl_map + geom_point(data = sampl) 
    
    
    Outline_N <- read_excel("Data/Crang_2024.xlsx", sheet="Outline", na ="NA")
    
    samples_N <- read_excel("Data/Crang_2024.xlsx" , sheet = "Nor")
    Samples_Nord <- read_excel("Data/Crang_2024.xlsx", sheet = "Nord")
    Pl_map_N <-
    ggplot(Outline_N, aes(x = Lon, y = Lat)) + 
      geom_path(aes(linetype = Type)) + 
      guides(linetype = "none") + 
      scale_linetype_manual(values = c(2,1, 3)) +
      theme_bw() 
    
    Pl_map_N + geom_point(data = samples_N)
   
    
    
    Pl_map_N + geom_point(data = Samples_Nord) 
    