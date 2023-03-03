library(stringr)
library(av)
library(dplyr)

# This code splits video files into frames and store them in new directories. 
# It first gets a list of all video files within a directory, removes the 
# container path, and creates a new path where the frames will be stored. 
# It then creates the new directories based on the new path and applies "chispas"
# a function to recursively split each video into frames and store them in the
#new directories. Finally, the function is applied to the list of videos.

setwd("/Volumes/your_directory/") #Set work directory to the higher level file container.

#Get the file list
dir <- list.dirs ("/Volumes/your_directory/your_upper_level_folder") # List directories
ls_video <- list.files(dir, full.names = T) # List files from each directory

# Relative Path editing
ls_2 <- str_remove_all(ls_videos, "/Volumes/your_directory/") #Remove the container path (higher level)  
ls_3 <- file.path("/Volumes/your_new_framelevel_directory/frames", lista_2)  # insert "frames" folder in the path in the same or a new directory

# Create the new directories where the frames will be stored (not that it has the same structure than the video files) 
lapply(ls_3, dir.create, recursive = T) 

# function ("chispas") to recursively split each video into frames and store them in the new directories 
chispas <- function(x){
  dest.dir = file.path("/Volumes/your_new_framelevel_directory/frames", x)
  frames = list.files(dest.dir)
  if(length(frames) < 1 && file.size (x) >= 2000000 && file_ext(x) == c("MP4", "AVI")) {                              
    av_video_images(video = x, destdir = file.path("/Volumes/your_new_framelevel_directory/frames", x), format="jpg", fps=1)
  }}

# Apply the function to the list of videos
lapply (ls_2, chispas)
