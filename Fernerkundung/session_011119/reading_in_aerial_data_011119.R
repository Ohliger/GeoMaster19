# Plotting an .tif-image with raster package ------------------------------------------------
## Load packages
libs = c("link2GI",
         "raster")
lapply(libs, require, character.only = TRUE)

###### Setting up the working environment.######
# Set pathes -------------------------------------------------------------------
if(Sys.info()[4] == "PC"){
  username <- "David"
} else if(Sys.info()[4] == "DESKTOP-LKKGU08"){
  username <- "hhans"
} else{print("You need to configure the filepath_base yourself!")}
## the if-else-loop sets the filepath depending on the system used ("PC" = PC, "DESKTOP-LKKGU08" = Laptop)

filepath_base <- paste0("C:/Users/", username, "/Google Drive/Studium/WiSe19/Fernerkundung")

# Set project specific subfolders -------------------------------------------------------------------
project_folders <- c("log", "output/", "src/", "run",
                     "data/", "data/aerial/", "data/data_mof/", "data/grass/", "data/lidar/","data/tmp/")

envrmt <- initProj(projRootDir = filepath_base,
                   projFolders = project_folders, 
                   path_prefix = "path_", 
                   global = FALSE)
################################################

# read in a single image
aerial_image <- raster("~/Fernerkundung_test/data/aerial/474000_5630000.tif")

plot(aerial_image)

# alternative for raster-function
aerial_image <- brick("~/Fernerkundung_test/data/aerial/474000_5630000.tif")

plot(aerial_image) #plots all 3 RGB-layers as a different plot; RasterBrick = multilayer raster object whereas raster uses only 1 band
plotRGB(aerial_image) #plots and RGB-image

# alternative for path-name
arial_image <- raster(paste0(envrmt$path_data_aerial, "/474000_5630000.tif")) #with paste0
arial_image <- raster(file.path(envrmt$path_data_aerial, "474000_5630000.tif")) #with file.path
## file.path connects character strings with an /, so that "C:", "user", "hhans" gets "C:/user/hhans"

# what is written in the raster?
aerial_image

# find out, if the single images are quadratisch or not
compareRaster(aerial_image)

# Reading in all the files and plot them --------------------------------------------------

aerial_files <- list.files(envrmt$path_data_aerial)
images <- list()

for(i in 1:length(aerial_files)){
  image <- brick(paste0(envrmt$path_data_aerial, aerial_files[i]))
  images <- c(images, image)
}

images$fun <- min
  
aerial_map_complete <- do.call(mosaic, images) # sorgt dafür, dass die weißen Bereiche der einzelnen Bilder abgeschnitten werden

plotRGB(aerial_map_complete)

saveRDS(aerial_map_complete, paste0(envrmt$path_output, "aerial_map_complete.RDS"))

