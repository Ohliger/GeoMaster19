# Define packages ------------------------------------------------------
libs <- c("link2GI",
         "raster",
         "glcm",
         "rgdal")

# set environment ----------------------------------------------------
source("../setEnvironment.R")
rasterOptions(progress = "text", tmpdir = envrmt$path_data_tmp, overwrite = T) 

## read and crop aerial file
uniwald_aerial <- brick(paste0(envrmt$path_output, "uwc.tif"))
names(uniwald_aerial) <- c("red", "green", "blue")

uwaExtent <- extent(c(477500.0, 478218.0, 5631730.0, 5632500.0)) # test area extend 
uniwald_aerial <- crop(uniwald_aerial, uwaExtent)

## calculate rgb_indices
source("funct_rgb_indices.R")

indices_rgb <- rgb_indices(red = uniwald_aerial[[1]],
                      green = uniwald_aerial[[2]],
                      blue = uniwald_aerial[[3]],
                      rgbi = c("VVI", "VARI", "NDTI", "RI",
                               "SCI", "BI", "SI", "HI", "TGI", "GLI", "NGRDI", "GRVI", "GLAI", "HUE",
                               "CI", "SAT", "SHP"))

# grey level co-occurence matrix 
textures_redch_3 <- glcm(uniwald_aerial[[1]], n_grey = 16, window = c(3, 3)) # red channel, 3x3 window 
textures_grech_3 <- glcm(uniwald_aerial[[2]], n_grey = 16, window = c(3, 3)) # green channel, 3x3 window 
textures_bluch_3 <- glcm(uniwald_aerial[[3]], n_grey = 16, window = c(3, 3)) # bluechannel, 3x3 window 

textures_redch_5 <- glcm(uniwald_aerial[[1]], n_grey = 16, window = c(5, 5)) # red channel, 5x5 window 
textures_grech_5 <- glcm(uniwald_aerial[[2]], n_grey = 16, window = c(5, 5)) # green channel, 5x5 window 
textures_bluch_5 <- glcm(uniwald_aerial[[3]], n_grey = 16, window = c(5, 5)) # bluechannel, 5x5 window 

# implement filters
filter_mean_red_3 <- focal(uniwald_aerial[[1]], w=matrix(1,3,3), fun=mean)
#......


# get test area CHM
chm <- brick(paste0(envrmt$path_data_lidar, "/level2/mof_chm_all.tif"))
chm <- crop(chm, uwaExtent)
chm <- disaggregate(chm, fact = 5) # fit resolution of CHM to resolution of uniwald_aerial
chm <- crop(chm, uniwald_aerial@extent) # fit extend of CHM to extend of uniwald_aerial
crs(chm) <- uniwald_aerial@crs # fit crs of CHM to crs of uniwald_aeria

# stack layers
uniwald_aerial <- stack(uniwald_aerial, indices_rgb)
uniwald_aerial <- stack(uniwald_aerial, textures_redch_3)
uniwald_aerial <- stack(uniwald_aerial, textures_grech_3)
uniwald_aerial <- stack(uniwald_aerial, textures_bluch_3)
uniwald_aerial <- stack(uniwald_aerial, textures_redch_5)
uniwald_aerial <- stack(uniwald_aerial, textures_grech_5)
uniwald_aerial <- stack(uniwald_aerial, textures_bluch_5)
uniwald_aerial <- stack(uniwald_aerial, chm)

## if RasterStack has more than 20 layers, it is saved in multiple files by 10 layers each
if(nlayers(uniwald_aerial) > 20){
  for (i in seq(from = 1, to = nlayers(uniwald_aerial), by = 10)) {
    if(i == 1){
      n <- 0
    } else {
      n <- round(c(i/10), 0)
    }
    if(c(i+10) < nlayers(uniwald_aerial)){
      uniwald_temp <- uniwald_aerial[[i:c(i+9)]]
    } else {
      uniwald_temp <- uniwald_aerial[[i:nlayers(uniwald_aerial)]]
    }
    raster::writeRaster(uniwald_temp, paste0(envrmt$path_output, "/uwc_test_area_multilayer_", n,".tif"), format = "GTiff")
    saveRDS(labels(uniwald_aerial), paste0(envrmt$path_output, "uwc_layernames.RDS"))}
} else {
  raster::writeRaster(uniwald_aerial, paste0(envrmt$path_output, "/uwc_test_area_multilayer.tif"), format = "GTiff")
  saveRDS(labels(uniwald_aerial), paste0(envrmt$path_output, "uwc_layernames.RDS"))
}

## read in all parts of the multilayer RasterStack and stack them
uniwald_multi_list <- list.files(envrmt$path_output, pattern = glob2rx("*.tif"), full.names = TRUE)
uniwald_multi_list <- uniwald_multi_list[grep("uwc_test_area_multilayer_", uniwald_multi_list)]

for(i in 1:length(uniwald_multi_list)) {
  part <- brick(uniwald_multi_list[i])
  if(exists("uniwald_aerial")) {
    uniwald_aerial <- stack(uniwald_aerial, part)
  } else {
    uniwald_aerial <- part
  }}
layernames <- readRDS(paste0(envrmt$path_output, "uwc_layernames.RDS"))
names(uniwald_aerial) <- layernames
