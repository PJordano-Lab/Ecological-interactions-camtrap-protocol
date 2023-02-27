library(stringr)
library(av)
library(dplyr)


setwd("/Volumes/G-RAID-2/") #Set work directory to the higher level file container.

#Get the file list
dir <- list.dirs ("/Volumes/G-RAID-2/SUMHAL_YR2/Osyris") # List directories
lista_videos <- list.files(dir, full.names = T) # List files from each directory

# Relative Path editing
lista_2 <- str_remove_all(lista_videos, "/Volumes/G-RAID-2/") #Remove the container path (higher level)  
lista_3 <- file.path("/Volumes/G-RAID-2/frames", lista_2)  # insert "frames" folder in the path in the same or a new directory

# crear directorios para guardar los frames (en /Volumes/LaCie/frames == Lista_3 )
lapply(lista_3, dir.create, recursive = T) 

# Create the new directories where the frames will be stored (not that it has the same structure than the video files) 
lapply(lista_3, dir.create, recursive = T) 

# function ("chispas") to recursively split each video into frames and store them in the new directories 
chispas <- function(x){
  dest.dir = file.path("/Volumes/G-RAID-4/frames3", x)
  frames = list.files(dest.dir)
  if(length(frames) < 1 && file.size (x) >= 2000000 && file_ext(x) == c("MP4", "AVI")) {                              
    av_video_images(video = x, destdir = file.path("/Volumes/G-RAID-4/frames3", x), format="jpg", fps=1)
  }}

# Apply the function to the list of videos
lapply (lista_2, chispas)