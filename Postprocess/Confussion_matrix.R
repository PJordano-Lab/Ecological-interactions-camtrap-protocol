######CONFUSSION MATRIX#########
library(jsonlite)
#data2 from phototrapping results contain only 

#             IA
#         +       -
#  
#      +   P      FN
#Real  
#      -   FP      N


describe(data2$plant) #Number of records for each species (with false positives)

#same results as DF -TOTAL RECORDS (positive results from IA) IA (P + FP) 
IA_total_records <- data2 %>%
  group_by(plant) %>%
  summarise(n())

# (TRUE) POSITIVES # 
d <- data2 %>% filter(data2$Sp1 != "")  #IA P data --> Real animals detected by IA

IA_true_positives <- d %>%
  group_by(plant) %>%
  summarise(n())

# FALSE POSITIVES # 
dd <- data2 %>% filter(data2$Sp1 == "")  #IA FP data --> No animals ("") but detect as with animal by IA

IA_false_positives <- dd %>%
  group_by(plant) %>%
  summarise(n())

# (REAL) NEGATIVES # 
#Working with json
setwd("/Users/PV/Documents/GitHub/Animal-detection-cameratrap/AI Results") #MAc Sobremesa EBD
setwd("/Users/Pablo/Documents/GitHub/Animal-detection-cameratrap/AI Results") #Macbook casa

json1 <- read_json("donana-aguilar-2022-02-17_detections_videos.filtered_rde_0.60_0.85_20_0.15_aggregated.json", simplifyVector = TRUE)
json2 <- read_json("donana-aguilar-2022-03-18_detections.filtered_rde_0.60_0.85_20_0.15_aggregated.json", simplifyVector = TRUE)
json3 <- read_json("donana-aguilar-2022-01-14_detections.filtered_rde_0.60_0.85_20_0.10_aggregated.json", simplifyVector = TRUE)
json4 <- read_json("donana-aguilar-2022-04-21_detections.filtered_rde_0.60_0.85_15_0.20_aggregated.json", simplifyVector = TRUE)

df1 <- data.frame(json1) %>% select(images.file, images.max_detection_conf)
df2 <- data.frame(json2) %>% select(images.file, images.max_detection_conf)
df3 <- data.frame(json3) %>% select(images.file, images.max_detection_conf)
df4 <- data.frame(json4) %>% select(images.file, images.max_detection_conf)

json_data <- rbind(df1,df2,df3,df4) #Dataframe of all AI results
str(json_data)

colnames(json_data) <- c("file","conf")
tail(json_data)

#Numero de videos de cada especie que hemos procesado con IA
file_path <- json_data$file #Only file path  from jsons
file_path <- str_remove_all(file_path, "_frames")  #remove frames and _frames to clean the path
file_path <- str_remove_all(file_path, "frames/") 
file_path <- str_replace(file_path,"Rev_", "Pyrus/") #Add name for pyrus rows (was only rev_)
file_path <- str_replace(file_path,"Pyrus_r6", "Pyrus/Rev_6_20220112/")
tail(file_path)

plant <- word(file_path, 1, sep = fixed("/"))  #Extract only the first word (plant species)
tail(plant)
unique(plant)

json_dat <- cbind(json_data,plant, file_path) #merge new column to dataset and name it
head(json_dat)

#Total videos
videos_por_sp <- json_dat %>%
  group_by(plant) %>%
  summarise(n())

#Revised videos
revised_videos <- filter(json_dat, conf >= 0.8)

IA_positives <- revised_videos%>%
  group_by(plant) %>%
  summarise(n())

#Non revised videos - Include negatives and false negatives (==> need revision). 
non_rev_videos <- filter(json_dat, conf < 0.8) 

IA_negatives <- non_rev_videos%>%
    group_by(plant) %>%
    summarise(n())


###########################################################################
## VIDEO SAMPLE FOR PLANT SPECIES TO CREATE A NEW VISUALIZATION VIDEOSET ##
###########################################################################

#Need to make a 10% subsample (for each species) to watch them and build a new confusion matrix.
  #En total el 10% de videos por revisar será 17.189 videos
  #Tomar una muestra aleatroia del 10% de videos de cada especies
#En las especies donde el 10% de los videos es muy poco tomamos toda la muestra =  (Pinus) o buena parte n= 700 (Rubia)

videos_por_sp <- json_dat %>%
  group_by(plant) %>%
  summarise(n()) %>%
  mutate(subsample_n = videos_por_sp$`n()` *10/100, .after = NULL) #10% of videos summarised by species (sample size)

####### Subsample each species  (Here ARBUTUS, later the rest )
dir_Aune <- list.dirs ("/Volumes/G-RAID/SUMHAL/Olea") 
Aune_videos <- list.files(dir_Aune, full.names = T) 

set.seed(123)
Aune_sample <- sample(Aune_videos, size = 731 , replace = F )

#1 Seleccionar los videos a cortarn(Cambiar selección para cada especie de planta)
origen <- Aune_sample

#2. Crear una carpeta de destino  
path_inter <- origen %>%
  str_remove_all("/Volumes/G-RAID/SUMHAL/")

destino <- file.path("/Volumes/LaCie_PV/sample", path_inter ) 

dest_folder <- dirname(destino)

lapply(dest_folder, dir.create, recursive = T) 

#3. función Copy-paste
file.copy(from = origen, to = destino, overwrite = T, recursive = T)


################################################
######          CHANGE SPECIES    ##############
################################################
videos_por_sp   #Change sample size for each species

dir <- list.dirs ("/Volumes/G-RAID/SUMHAL/Rubia")  #Change species name!!
sp_videos <- list.files(dir, full.names = T) 

set.seed(123)
sp_sample <- sample(sp_videos, size = 700 , replace = F )


#1 Seleccionar los videos a cortarn(Cambiar selección para cada especie de planta)
origen <- sp_sample

#2. Crear una carpeta de destino  
path_inter <- origen %>%
  str_remove_all("/Volumes/G-RAID/SUMHAL/")

destino <- file.path("/Volumes/LaCie_PV/sample", path_inter ) 

dest_folder <- dirname(destino)

lapply(dest_folder, dir.create, recursive = T) 

#3. función Copy-paste
file.copy(from = origen, to = destino, overwrite = T, recursive = T)



##############################################################################
############   OBTAINING CONFUSSION MATRIX FROM HUMAN REVISED     ############
##############################################################################
setwd("/Users/Pablo/Documents/GitHub/Animal-detection-cameratrap/Results/sample_datasets") #Macbook casa

# REMINDER : ALL PROCESSED DATA BY AI json_dat (#json_dat == all the IA results in a single df))
tail(json_dat)

#JOXY human processed
sample <- read.csv("Myrtus_sample.csv") #Joxy 
str(sample)  

#Summary Human + and -
   # Human Negatives       
sample %>%        
  group_by(Sp1) %>%
  summarise(n())
    #Human positives      
sample %>%
  filter(Sp1 != "empty") %>%
  summarise(n())

#MERGE AI + Human processed 
# 1.Construct the relative path and reduce columns  
s <- sample %>% select(File, RelativePath, Sp1, Obs) %>% 
  unite("filepath_wrong", RelativePath, File, sep ="/") 

    filepath_2 <- str_replace_all(s$filepath_wrong, "\\\\", "/" ) 
    filepath <- file.path("Myrtus", filepath_2, fsep = "/")  #Change plant species
    filepath <- str_remove_all(filepath, "_")
      s2 <- cbind(s, filepath) 
      colnames(s2) <- c( "filepath_wrong", "Sp1", "Obs", "file_path")

head(s2)
str(s2)

#Results from AI (confussion matrix script) #Change name species!!!
  
AI_results_sp <- json_dat %>%
  filter(plant == "Myrtus") %>%
      mutate (file_path = str_remove_all(file_path, "_"))

head(AI_results_sp) 

# 2.Merge dataset
merge <- left_join(s2, AI_results_sp, by = "file_path") #There is a problem with some videos from Ppin 004 and 005, that werent IA processed (and also some jpgs)  

merge_2 <- merge %>%
  filter(conf != "NA")

write.csv(merge_2, file="/Users/Pablo/Desktop/merge.csv")

str(joxy_merge_2)
head(joxy_merge)

#Select AI conficence > 0.8 (positive for AI)
#Filter only videos with high confidence level 
joxy08 <- joxy_merge_2 %>%
        filter(conf < 0.8) %>%
        summarise(n())
str(joxy08)

#Human positives vs negatives
unique(joxy_merge_2$Sp1)

joxy08 <- joxy_merge_2 %>%
  filter(Sp1 == "empty") %>%
  summarise(n())

human_negatives <- joxy_merge_2 %>%
  group_by(Sp1) %>%
  summarise(n())


summary(str_detect(joxy_total$file, ".MP4"))

joxy_merge$file_path
joxy_total$file_path













##############################################################################
###################     SUMMARY BY SPECIES (ARBUTUS UNEDO)     ################
##############################################################################
aune <- subset(dat,dat$plant == "Arbutus")
str(aune)

# Visits group by animal species  
visits <- data.frame(aune %>%
                       group_by(Sp1) %>%
                       summarise(n()))

visits <- visits[order(-visits$n..),] #Order
sum(visits$n..)                     #Total animal visits 
visit_prop <- (visits$n../sum(visits$n..))*100 #Proportion
visit_prop <- cbind(visits,visit_prop)
visit_prop
#write.csv(visit_prop, file="/Users/PV/Desktop/Aune_visits.csv")

# Eating
eating <- subset(aune, Behaviour == "eating")

e_visits <- data.frame(eating %>%
                         group_by(Sp1) %>%
                         summarise(n()))

e_visits <- e_visits[order(-e_visits$n..),] #Order
sum(e_visits$n..)  #Total interactions with arbutus
e_visit_prop <- e_visits$n../sum(e_visits$n..)*100 #Proportion
e_visit_prop <- cbind(e_visits,e_visit_prop)
e_visit_prop
#write.csv(e_visit_prop, file="/Users/PV/Desktop/Aune_eating_visits.csv")
Behaviour <- data.frame(aune %>% group_by(Behaviour) %>% summarise(n()))  #Just to know the numbers if we need to include probably eating or searching 
#write.csv(Behaviour, file="/Users/PV/Desktop/Aune_behaviours.csv")







