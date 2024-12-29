library(readxl)
outline <- read_excel("Data/Crang 2022 full.xlsx", sheet = "Outline",  na = "NA")

  
Pl_Map_NN<-
    ggplot(outline, aes(x = Lon, y =Lat)) + 
    geom_path(aes( Type)) + 
    guides( "none") + 
    scale_linetype_manual(values = c(2,3,1)) +
    theme_minimal()

    Pl_Map_NN + geom_point(data = samples) 
 
    
    
    
      
    