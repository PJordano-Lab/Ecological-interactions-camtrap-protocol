######CONFUSSION MATRIX#########
library(jsonlite)
#data2 from phototrapping results contain only 

#             CV
#         +       -
#  
#      +   P      FN
#Real  
#      -   FP      N


describe(data2$plant) #Number of records for each species (with false positives)

#same results as DF -TOTAL RECORDS (positive results from CV) CV (P + FP) 
CV_total_records <- data2 %>%
  group_by(plant) %>%
  summarise(n())

# (TRUE) POSITIVES # 
d <- data2 %>% filter(data2$Sp1 != "")  #CV P data --> Real animals detected by CV

CV_true_positives <- d %>%
  group_by(plant) %>%
  summarise(n())

# FALSE POSITIVES # 
dd <- data2 %>% filter(data2$Sp1 == "")  #CV FP data --> No animals ("") but detect as with animal by CV

CV_false_positives <- dd %>%
  group_by(plant) %>%
  summarise(n())

# (REAL) NEGATIVES # 
#Working with json
setwd("Your directory")


json1 <- read_json("Your file1.json", simplifyVector = TRUE)
json2 <- read_json("Your file2.json", simplifyVector = TRUE)

df1 <- data.frame(json1) %>% select(images.file, images.max_detection_conf)
df2 <- data.frame(json2) %>% select(images.file, images.max_detection_conf)

json_data <- rbind(df1,df2) #Dataframe of all CV results

colnames(json_data) <- c("file","conf")

#Processed videos for each species (processed by CV)
file_path <- json_data$file #Only file path  from jsons
file_path <- str_remove_all(file_path, "_frames")  #remove frames and _frames to clean the path
file_path <- str_remove_all(file_path, "frames/") 

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

CV_positives <- revised_videos%>%
  group_by(plant) %>%
  summarise(n())

#Non revised videos - Include negatives and false negatives (==> need revision). 
non_rev_videos <- filter(json_dat, conf < 0.8) 

CV_negatives <- non_rev_videos%>%
    group_by(plant) %>%
    summarise(n())


###########################################################################
## VIDEO SAMPLE FOR PLANT SPECIES TO CREATE A NEW VISUALIZATION VIDEOSET ##
###########################################################################

#Need to make a 10% subsample (for each species) to watch them and build a new confusion matrix.
  #We chose a random sample of 10% de videos for each species (stratified)

videos_por_sp <- json_dat %>%
  group_by(plant) %>%
  summarise(n()) %>%
  mutate(subsample_n = videos_por_sp$`n()` *10/100, .after = NULL) #10% of videos summarised by species (sample size)

####### Subsample each species (Here ARBUTUS, later the rest)
dir_Aune <- list.dirs ("/Volumes/G-RAID/SUMHAL/Olea") 
Aune_videos <- list.files(dir_Aune, full.names = T) 

set.seed(123)
Aune_sample <- sample(Aune_videos, size = 731 , replace = F )

#1 Seleccionar los videos a cortarn(CambCVr selección para cada especie de planta)
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

videos_por_sp   #Change sample size for each species (10% each)

dir <- list.dirs ("/Volumes/G-RAID/SUMHAL/RubCV")  #Change species name!!
sp_videos <- list.files(dir, full.names = T) 

set.seed(123)
sp_sample <- sample(sp_videos, size = 700 , replace = F ) #For Aune 10% is n = 700 

#Select videos to copy-paste (see copy-paset script) 

##############################################################################
############   OBTAINING CONFUSSION MATRIX FROM HUMAN REVISED     ############
##############################################################################
setwd("Your directory")

# REMINDER : ALL PROCESSED DATA BY AI json_dat (#json_dat == all the CV results in a single df))
tail(json_dat)

#human processed (for a single plant species)
sample <- read.csv("plant species.csv") #Example: Joxy 
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
    filepath <- file.path("Joxy", filepath_2, fsep = "/")  #Change plant species
    filepath <- str_remove_all(filepath, "_")
      s2 <- cbind(s, filepath) 
      colnames(s2) <- c( "filepath_wrong", "Sp1", "Obs", "file_path")

#Results from AI (confussion matrix script) #Change species name!
  
AI_results_sp <- json_dat %>%
  filter(plant == "Joxy") %>%
      mutate (file_path = str_remove_all(file_path, "_"))

# 2.Merge dataset
merge <- left_join(s2, AI_results_sp, by = "file_path") 

merge_2 <- merge %>%
  filter(conf != "NA")

write.csv(merge_2, file="your directory/merge.csv")

#Select AI conficence > 0.8 (positive for AI)
#Filter only videos with high confidence level 
joxy08 <- joxy_merge_2 %>%
        filter(conf < 0.8) %>%
        summarise(n())

#Human positives vs negatives
unique(joxy_merge_2$Sp1)

joxy08 <- joxy_merge_2 %>%
  filter(Sp1 == "empty") %>%
  summarise(n())

human_negatives <- joxy_merge_2 %>%
  group_by(Sp1) %>%
  summarise(n())




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







