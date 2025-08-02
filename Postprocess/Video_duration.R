library(stringr)
library(dplyr)
library(lubridate)
library(av)

# This code extracts the video duration from files that are listed in an external metadata file (videos.csv) 
#to include a "duration" column that will represent the interaction intensity.  

# Get a list of all files in the folder (likely would be an external HD device) 
dir <- list.dirs ("/RelativePath/folder_name") #List directories from the folder
fil <- list.files(dir, full.names = T) #File list (with directories)
fil_clean <- fil[!file.info(fil)$isdir] #File list (w/o container folders)
write.csv(fil_clean, "Your directory/video_file_list.csv", sep = ",")  #store it as a csv for later validation

# Get a list from the selected files (videos with eating events) from the Timelapse csv output (eating.csv)
fil_eating <- str_c(eating$RelativePath, eating$File, sep ="/")  #Selected list of videos 
fil_eat_path <- file.path("Your directory", fil_eating) #Vidoe lists and path

# Correct some path syntax mistakes (detected by setdiff function, see below)   
fil_eat_clean <- fil_eat_path |>
  str_replace_all("\\\\", "/") |>
  str_replace_all("Pyrus_r6", "Pyrus") |>  #The next 2 rows are examples of mistakes in Relative path tha should be corrected 
  str_replace_all("/Volumes/G-RAID/SUMHAL/Rev", "/Volumes/G-RAID/SUMHAL/Pyrus/Rev") 

# Compare eating list to the entire file list (all files in GRAID) to detect missmatches  
fil_lost <- setdiff(fil_eat_clean, fil_clean) #What is in my_list_2 that is not in my_list_1?
length(fil_lost)

# Extract video duration from eating list
info <- lapply(fil_eat_clean, av_media_info)  #Apply function av_media info to video list 
duration <- sapply(info, function(x){as.numeric(x[1])}) #extract only video duration (needed for chacking that videos are 10 sec duration)

# Join duration to database  
eating <- cbind(eating, duration)

write.csv(eating,"Your directory/eating_duration.csv")
