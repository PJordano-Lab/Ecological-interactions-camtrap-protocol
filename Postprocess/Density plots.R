
    ####################################
    ####################################
  
    #hay que eliminar los Timestamp issues "FINOS"
    
    ####################################
    ####################################
    final_data <- read.csv(file="/Users/Pablo/Documents/GitHub/Animal-detection-cameratrap/Results/final_data.csv")
        final_data <- read.csv(file="/Users/PV/Documents/GitHub/Animal-detection-cameratrap/Results/final_data.csv")
    str(final_data)
    
    ####################################
    ####################################
    
    #           PHITOCENTRIC          #
    
    ####################################
    ####################################
    
    ##########   REPETIR CON BD agrupada por eventos cada 5 min ################
    str(final_data)
    
    df <- filter(final_data, TimestampIssue == "FALSE") #Eliminate "BASIC" timestamp issues
    str(df)
    
    #Necesito:
    #1.crear una tabla con columnas week; plant sp; plant_id ; number of interactions (1 por entrada)
    #2.de esta tabla crear otra con la suma del numero de interacciones por semana y plant_sp (primer uncount)
    #3. de ahí sacar la tabla grande donde convertir las n interacciones (en la columna de interacciones) en filas.
    
    #1. crear el dataset
    date <- as.Date(df$DateTime)
    week <- format(date, "%V")
    plant_sp <- df$plant
    interaction <- as.integer(df$Behaviour == "eating" | df$Behaviour == "probably eating")
    
    d <- data.frame(cbind(week, plant_sp, interaction))
    str(d)
    
    #crear la tabla para plotear (contar el nuemro de interacciones por semana)
    uncount.f.data <- as.data.frame(d %>% 
                                      group_by(week, plant_sp) %>%
                                      summarise(sum = sum(as.numeric(interaction), na.rm=TRUE))) # contar numero de interacciones (sólo eating) agrupadas (sum) por semana 
    uncount.f.data <- tidyr::uncount(na.omit(uncount.f.data), weights=as.integer(sum))
    
    #Crear un vector para ordenar el eje de la x por semanas desde que se comenzó el muestreo
    week_order <- c("28", "29", "30", "31", "32", "33", "34", "35" ,"36", "37" ,"38" ,"39", "40" ,"41", "42" ,"43" ,"44" ,"45", "46", "47", "48", "49", "50", "51", "52", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20","21","22","23","24","25","26","27")  
    
    
    #Plotear los density plots por especie de planta
    ggplot(uncount.f.data, aes(x = factor(week, level = week_order), y = plant_sp, group = plant_sp, fill = plant_sp)) + 
      geom_violin(bw=0.8, alpha=0.7) + theme_classic() 
    theme(legend.position = "none", axis.text.y = element_text(face="bold", size=13),
          axis.text.x = element_text(face="bold", size=13),
          axis.title.x = element_text(face="bold", size=15, 
                                      margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.title.y = element_blank()) + scale_x_continuous(labels = 1:53, breaks = 1:53)
    
      
      ####################################
      ####################################
      #                                  #
      #           ZOOCENTRIC             #
      #                                  #
      ####################################
      ####################################
      
      #1. crear el dataset
        sp1 <- subset(dat, Sp1 == "vulpes vulpes")
      
      date <- as.Date(dat$DateTime)
      week <- format(date, "%V")
      animal <- dat$Sp1 
      interaction <- as.integer(dat$Behaviour == "eating")
      
      d <- data.frame(cbind(week, animal, interaction))
      str(d)
      
      #crear la tabla para plotear (contar el nuemro de interacciones por semana)
      uncount.f.data <- as.data.frame(d %>% 
                                        group_by(week, animal) %>%
                                        summarise(sum = sum(as.numeric(interaction), na.rm=TRUE))) # contar numero de interacciones (sólo eating) agrupadas (sum) por semana 
      uncount.f.data <- tidyr::uncount(na.omit(uncount.f.data), weights=as.integer(sum))
      
      #Plotear los density plots por especie de planta
      ggplot(uncount.f.data, aes(x = week, y = animal, group = animal, fill = animal)) + 
        geom_violin(bw=0.8, alpha=0.7) + theme_classic() 
      theme(legend.position = "none", axis.text.y = element_text(face="bold", size=13),
            axis.text.x = element_text(face="bold", size=13),
            axis.title.x = element_text(face="bold", size=15, 
                                        margin = margin(t = 20, r = 0, b = 0, l = 0)),
            axis.title.y = element_blank())   
    
      
      
    
   #Fechas de colocación del primer deployment    
      Aune_date <- format(as.Date("26/10/21"), "%V")
      Aspa_date <- format(as.Date("18/11/21"), "%V")
      Mcom_date <- format(as.Date("14/10/21"), "%V")
      Oeur_date <- format(as.Date("26/10/21"), "%V")
      Ppin_date <- format(as.Date("8/10/21"), "%V")
      Pbou_date <- format(as.Date("27/9/21"), "%V")
      Rper_date <- format(as.Date("14/10/21"), "%V")
      Rulm_date <- format(as.Date("19/8/21"), "%V")
      Sasp_date <- format(as.Date("21/10/21"), "%V")
      Joxy_date <- format(as.Date("16/12/21"), "%V")
      Corema2_date <- format(as.Date("15/7/21"), "%V")
      
     
       
      ####################################
      ####################################
      #                                  #
      #           TODOS LOS DATOS        #
      #                                  #
      ####################################
      ####################################
      
      ##########   REPETIR CON BD agrupada por eventos cada 5 min ################
      str(dat)

      
      #Necesito:
      #1.crear una tabla con columnas week; plant sp; plant_id ; number of interactions (1 por entrada)
      #2.de esta tabla crear otra con la suma del numero de interacciones por semana y plant_sp (primer uncount)
      #3. de ahí sacar la tabla grande donde convertir las n interacciones (en la columna de interacciones) en filas.
      
      #1. crear el dataset
      date <- as.Date(dat$DateTime)
      week <- format(date, "%V")
      plant_sp <- dat$plant
      interaction <- as.integer(dat$Behaviour == "eating" | dat$Behaviour == "probably eating")
      
      d <- data.frame(cbind(week, plant_sp, interaction))
      str(d)
      
      #crear la tabla para plotear (contar el nuemro de interacciones por semana)
      uncount.f.data <- as.data.frame(d %>% 
                                        group_by(week, plant_sp) %>%
                                        summarise(sum = sum(as.numeric(interaction), na.rm=TRUE))) # contar numero de interacciones (sólo eating) agrupadas (sum) por semana 
      uncount.f.data <- tidyr::uncount(na.omit(uncount.f.data), weights=as.integer(sum))
      
      #Crear un vector para ordenar el eje de la x por semanas desde que se comenzó el muestreo
      week_order <- c("28", "29", "30", "31", "32", "33", "34", "35" ,"36", "37" ,"38" ,"39", "40" ,"41", "42" ,"43" ,"44" ,"45", "46", "47", "48", "49", "50", "51", "52", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20","21","22","23","24","25","26","27")  
      
      
      #Plotear los density plots por especie de planta
      ggplot(uncount.f.data, aes(x = factor(week, level = week_order), y = plant_sp, group = plant_sp, fill = plant_sp)) + 
        geom_violin(bw=0.8, alpha=0.7) + theme_classic() 
      theme(legend.position = "none", axis.text.y = element_text(face="bold", size=13),
            axis.text.x = element_text(face="bold", size=13),
            axis.title.x = element_text(face="bold", size=15, 
                                        margin = margin(t = 20, r = 0, b = 0, l = 0)),
            axis.title.y = element_blank()) + scale_x_continuous(labels = 1:53, breaks = 1:53)
      
      
      
      
      
      
      