require(link2GI)
require(utils)

# Set paths -------------------------------------------------------------------

## the if-else-loop sets the filepath depending on the system used ("PC" = PC, "DESKTOP-LKKGU08" = Laptop)
if(Sys.info()[4] == "PC"){
  filepath_base <- "F:/Studium/WiSe19/Datenanalyse"
} else if(Sys.info()[4] == "DESKTOP-LKKGU08"){
  filepath_base <- "F:/Studium/WiSe19/Datenanalyse"
} else if(Sys.info()[4] == "DESKTOP-J45TOVE"){
  filepath_base = "R:/Studium/Marburg/WS19/mpg-envinsys-plygrnd"
} else {
  warning("No base path configured. The working environment will be set up in ", getwd())
  filepath_base = getwd()
}

# Set project specific subfolders -------------------------------------------------------------------
project_folders = c("data/",
                    "tmp/",
                    "src/",
                    "output/")

envrmt <- initProj(projRootDir = filepath_base,
                   projFolders = project_folders, 
                   path_prefix = "path_", 
                   global = FALSE)

# Set temp
#if(require(raster) == T){
#  rasterOptions(progress = "text", tmpdir = envrmt$path_data_tmp, overwrite = T)
#} else {}

# load packages, defined by libs
lapply(libs, function(packages) {
  k <- packages[!(packages %in% installed.packages()[,"Package"])];
  if(length(k)){install.packages(k);}
  for(package_name in packages){library(package_name,character.only=TRUE, quietly = TRUE);}
})


