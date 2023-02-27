##########################################################

#      WORKING WITH Java Script Open Notation (.json - results from IA)

##########################################################
library(jsonlite)
library(dplyr)
library(Hmisc)
library(stringr)

setwd("/Users/PV/Documents/GitHub/Animal-detection-cameratrap/AI Results")  #Mac sobremesa EBD
  setwd("/Users/Pablo/Documents/GitHub/Animal-detection-cameratrap/AI Results")  #MacBookPro Pablo

#IMPORT AND CLEAN THE AI RESULTS FOR VIDEO LEVEL
data <- read_json("donana-aguilar-2022-03-18_detections.filtered_rde_0.60_0.85_20_0.15_aggregated.json", simplifyVector = TRUE)
df <- data.frame(data) 
head(df)
str(data)

#Clean the json: only need the max detection confidence from the frame level and the video path 
d <- df %>% select("images.file", "images.max_detection_conf")
colnames(d) <- c("file","conf")
head(d)
tail(d)

#Filter only videos with high confidence level 
p08 <- filter(d, conf >= 0.8) 
describe(p08)

# Create a list of the video paths with conf >= 0.8
kk <- p08 %>% select("file") 

#Convert from df to atomic vector
kk2 <- kk$file
head(kk2)
#Create a directory to put the results in (videos with animals)
dir.create("/Volumes/LaCie_free2/vid_w_anim", recursive=T)

#remove unusable string (-frames) to obtain a correct path to videos with animals
l0 <- str_remove_all(kk2, "_frames") 
l <- str_remove_all(l0, "frames/") 
head(l)

#introduce the rest of the path 
l2 <- file.path("/Volumes/G-RAID/SUMHAL", l)
head(l2)

#CREATE THE NEW DIRECTORIES IN G-RAID-2
l22 <- dirname(l2) # Old Path 
head(l22)

l23 <- str_remove_all(l2, "/Volumes/G-RAID/")   #remove part of the path
head(l23)

l24 <- file.path("/Volumes/G-RAID-2/vid_w_anim", l23)   # New path 
head(l24)

l25 <- dirname(l24) #Path to the new folder ("vid_w_anim")
head(l25)

#May be ommited
 #l26 <- data.frame(l25) #need to be df for next step
 #l26 <- distinct(l26) #Only diffeent paths
#str(l26)
#head(l26)
  #l26 <- l26$l26 #as vecotr
  #l26 <- data.frame(l26)

lapply(l25, dir.create, recursive =T) # Create directories from the new path

#COPY AND PASTE THE FILES FROM OLD TO NEW PATHS
file.copy(from = l2, to = l24, 
          overwrite = TRUE, recursive=TRUE,
          copy.mode = TRUE, copy.date = T)

head(l2)
head(l24)

###################################################
###################################################
###################   REPEAT   #################### RUBUS_YR_2
################################################### 
###################################################
###################################################
setwd("/Users/PV/Documents/GitHub/Animal-detection-cameratrap/AI results/Rubus_Yr2")  #Mac sobremesa EBD

#IMPORT AND CLEAN THE AI RESULTS FOR VIDEO LEVEL
data <- read_json("donana-aguilar-2023-01-09-v5a.0.0_detections.filtered_rde_0.09_0.85_20_0.20_aggregated.json", simplifyVector = TRUE)
df <- data.frame(data) 
head(df)

#Clean the json: only need the max detection confidence from the frame level and the video path 
d <- df %>% select("images.file", "images.max_detection_conf") %>%
  rename(file = images.file,  , conf = images.max_detection_conf)
head(d)

#Filter only videos with high confidence level 
p08 <- filter(d, conf > 0.1) 
describe(p08) 
describe(d)

# Create a list of the video paths with conf >= 0.1
selection <- p08 %>% select("file") 

#Convert from df to atomic vector
sel <- selection$file
head(sel)
  
origen <- file.path("/Volumes/G-RAID-2/SUMHAL_YR2", sel)
 
carpetas <- origen %>%
    str_replace_all("Rubus", "Rubus_selection") %>%
    str_replace_all("G-RAID-2/SUMHAL_YR2", "LaCie_PV")%>%
    dirname()
  
destino <- origen %>%
  str_replace_all("Rubus", "Rubus_selection") %>%
  str_replace_all("G-RAID-2/SUMHAL_YR2", "LaCie_PV")
  

lapply(carpetas, dir.create, recursive =T) # Create directories from the new path

#COPY AND PASTE THE FILES FROM OLD TO NEW PATHS
file.copy(from = origen, to = destino, 
          overwrite = TRUE, recursive=TRUE,
          copy.mode = TRUE, copy.date = T)

