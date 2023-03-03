library(tidyr)
library(stringr)
library(dplyr)

# This R code is designed for automatizing the creation of a standardized
# format for organizing camera-trap data for Ecological interactions. The data 
# generated here will be the core of the "video.csv" database for controlling the fieldwork 
# and controlling sampling effort. 
# The code extracts the first and last video date information from a set of 
# subfolders containing video files. 
# It calculates the number of days between the first and last video files and 
# counts the number of files in each subfolder. 
# The results are stored in a data frame and written to a CSV file. 

  # Set the parent directory where the video subfolders are located (suggestion: chack each plant species for better controlling posible errors)
  parent_dir <- "/Volumes/G-RAID-2/SUMHAL_YR2/Smilax"
  
  # Get a list of all subdirectories within the parent directory
  subdirs <- list.dirs(parent_dir, recursive = TRUE)
  
  # Create an empty list to store the results
  results_list <- list()
  
  # Loop through each subdirectory
  for (subdir in subdirs) {
    
    # Get a list of all video files in the subdirectory
    video_files <- list.files(subdir, pattern = "\\.(mp4|avi|mov)$", ignore.case = TRUE)
    
    # Skip the subdirectory if there are no video files in it
    if (length(video_files) == 0) next
    
    # Get the file information for each video file
    file_info <- file.info(paste0(subdir, "/", video_files))
    
    # Get the date and time of the first and last video files
    first_file_date <- min(file_info$mtime)
    last_file_date <- max(file_info$mtime)
    
    # Calculate the number of days between the first and last video files
    num_days <- difftime(last_file_date, first_file_date, units = "days")
    
    # Count the number of files in the subdirectory
    num_files <- length(video_files)
    
    # Append the results for this subdirectory to the results list
    results_list[[subdir]] <- list(first_file_date = first_file_date, 
                                   last_file_date = last_file_date,
                                   num_files = num_files,
                                   num_days = as.numeric(num_days))
  }
  
  
  # Convert the results list to a data frame
  results_df <- data.frame(subdir = character(),
                           first_file_date = character(),
                           last_file_date = character(),
                           num_files = integer(),
                           num_days = numeric(),
                           stringsAsFactors = FALSE)
  for (i in seq_along(results_list)) {
    results_df[i, "subdir"] <- names(results_list)[i]
    results_df[i, "first_file_date"] <- format(results_list[[i]]$first_file_date, "%d/%m/%Y %H:%M:%S")
    results_df[i, "last_file_date"] <- format(results_list[[i]]$last_file_date, "%d/%m/%Y %H:%M:%S")
    results_df[i, "num_files"] <- results_list[[i]]$num_files
    results_df[i, "num_days"] <- results_list[[i]]$num_days
  }

  #split Revision and Individual from path
  results_df_2 <- results_df %>%
    mutate(new_dir = str_remove_all(subdir, "/Volumes/G-RAID-2/SUMHAL_YR2/Smilax/Sasp_")) %>%           
    separate(new_dir, into = c("rev", "date", "individual_pl", "camera", sep = "/")) %>%
    mutate(deployment = str_c(individual_pl, camera, sep = "_")) %>%
    select(deployment, rev, num_files, first_file_date, last_file_date, num_days)%>%
    data.frame()
           
  head(results_df_2)
  
    # Write the data frame to a CSV file
  write.csv(results_df_2, file = "/Users/PV/Desktop/Sasp_dates.csv", row.names = FALSE)
  
  
  ###########################
  
  #This code will generate the Deployment.csv table to control the upper level of sampling effort
  
  deployment_dates <- results_df_2 %>%
    group_by(deployment) %>%
    summarize(first_date = min(first_file_date),
              last_date = max(last_file_date))
  
  
min(file_info$mtime)

#split Revision and Individual from path
results_deployments <- results_df %>%
  mutate(new_dir = str_remove_all(subdir, "/Volumes/G-RAID-2/SUMHAL_YR2/Arbutus/Aune_")) %>%           
  separate(new_dir, into = c("rev", "date", "individual_pl", "camera", sep = "/")) %>%
  mutate(deployment = str_c(individual_pl, camera, sep = "_")) %>%
  group_by(deployment) %>%
  summarise(min(first_file_date))%>%
  data.frame()

library(readxl)
library(dplyr)
 Mcom <- read_excel("/Users/Pablo/Documents/GitHub/SUMHAL_WP5_fieldwork/SUMHAL_fieldwork_Myrtus_communis/Myrtus_phototrapping_yr2.xlsx", sheet = 2) 
 
kk <- Mcom %>%
  data.frame() %>%
  group_by(Deployment_ID) %>%
  summarise(as.POSIXct(min(First.video)),
            as.POSIXct(max(Last.video)))
 
write.csv(kk, "/Users/Pablo/Desktop/lista.csv")
  