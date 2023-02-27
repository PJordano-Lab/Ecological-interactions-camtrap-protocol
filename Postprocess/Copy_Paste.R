
# Script para copiar seleccion de videos y pegarlos en otra carpeta

########################################################################
########################################################################
########################################################################
#SELECT AND COPY PASTE VIDEOS OF NOT DETERMINED SPECIES (TO VISUALIZE THEM)
#1 Seleccionar los videos a cortar
#2. Crear una carpeta de destino 
#3. fiunción Copy-paste
########################################################################
########################################################################
########################################################################

#dat from phototrapping_results script


          #1 Seleccionar los videos a cortar
#not determined videos
no_det <- dat %>%
  filter(Sp1 == "?")

file_path_no_det <- str_c(no_det$new_path, no_det$File, sep="/") %>%
  str_replace_all ("\\\\", "/")

origen_no_det <- file.path ("/Volumes/G-RAID/SUMHAL", file_path_no_det)

#2. Crear una carpeta de destino  
destino_no_det <- file.path("/Volumes/LaCie_free2/indet", file_path_no_det) 

dest_folder <- dirname(destino_no_det)

lapply(dest_folder, dir.create, recursive = T) 

#3. función Copy-paste
file.copy(from = origen_no_det, to = destino_no_det, overwrite = T, recursive = T)



#Duración de los videos (seg)
arb <- av_media_info("/Volumes/G-RAID/SUMHAL/Arbutus/Rev1_20211109/Aune001_19/IMG_0001.AVI")
arb$duration

#tamaño de los videos (bytes)
file.size("/Volumes/G-RAID/SUMHAL/Arbutus/Rev1_20211109/Aune001_19/IMG_0001.AVI")

