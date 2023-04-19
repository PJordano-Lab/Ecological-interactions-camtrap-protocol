##########################################################

#WORKING WITH Java Script Open Notation (.json - results from IA)

##########################################################

library(jsonlite)
library(dplyr)
library(Hmisc)
library(stringr)

setwd("Your work directory")

#IMPORT AND CLEAN THE AI RESULTS FOR VIDEO LEVEL
data <- read_json("your aggregated json.josn", simplifyVector = TRUE) #Read the AI results aggregated at video level in json format
  df <- data.frame(data) #Convert it to a DF. Note that confidence levels are listed in a column with coordinates of boiunding boxes. It is meaningless.  
    d <- df %>% select("images.file", "images.max_detection_conf") #Clean the json: only need the max detection confidence from the frame level and the video path 
      colnames(d) <- c("file","conf")   #Change column names

#Filter only videos with high confidence level (note that for the example we set it to 0.8)
p08 <- filter(d, conf >= 0.8) 
describe(p08)

# Create a list of the video paths with the confidence threshold
selection <- p08 %>% select("file") 
  sel <- selection$file #Convert from df to atomic vector

origin <- file.path("Your filepath", sel) #Set the original file path to videos

container <- origin %>%
  str_replace_all("original name", "original_name_selection") %>%  #Change path from "original" to "selection" (i.e. from "rubus" to "rubus_selection")
  str_replace_all("original device path", "new device path")%>% #Change destination in case you want to use an other device. 
  dirname()

destiny <- origin %>%
  str_replace_all("original name", "original_name_selection") %>% #Change path from "original" to "selection" (i.e. from "rubus" to "rubus_selection")
  str_replace_all("original device path", "new device path")

lapply(container, dir.create, recursive =T) # Create directories for the new path

#COPY AND PASTE THE FILES FROM OLD TO NEW PATHS
file.copy(from = origin, to = destiny, 
          overwrite = TRUE, recursive=TRUE,
          copy.mode = TRUE, copy.date = T)

#-------------------------------------------------------------------------------------------

#Combine json results from MD into a single file

#Read the json files (x4) 
json_file_1 <- fromJSON("/Users/Pablo/Documents/GitHub/Animal-detection-cameratrap/AI results/donana-aguilar-2022-01-14_detections.filtered_rde_0.60_0.85_20_0.10_aggregated.json")
df_1 <- as.data.frame(json_file_1)

json_file_2 <- fromJSON("/Users/Pablo/Documents/GitHub/Animal-detection-cameratrap/AI results/donana-aguilar-2022-02-17_detections_videos.filtered_rde_0.60_0.85_20_0.15_aggregated.json")
df_2 <- as.data.frame(json_file_1)

json_file_3 <- fromJSON("/Users/Pablo/Documents/GitHub/Animal-detection-cameratrap/AI results/donana-aguilar-2022-03-18_detections.filtered_rde_0.60_0.85_20_0.15_aggregated.json")
df_3 <- as.data.frame(json_file_1)

json_file_4 <- fromJSON("/Users/Pablo/Documents/GitHub/Animal-detection-cameratrap/AI results/donana-aguilar-2022-04-21_detections.filtered_rde_0.60_0.85_15_0.20_aggregated.json")
df_4 <- as.data.frame(json_file_1)

#Combine the data frames
combined_df <- rbind(df_1, df_2, df_3, df_4)

#Write the combined df to JSON
writeLines(toJSON(combined_df), "/Users/Pablo/Documents/GitHub/Animal-detection-cameratrap/AI results/combined_file.json")
