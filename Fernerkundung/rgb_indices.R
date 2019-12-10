# ===============================================================================
# rgb_indices.R
# Author: Jonas Viehweger
# Copyright: 
#
# Description:  script calculates given rgb indices
#
# Input:        r,g,b     = red, green and blue bands
#               parallel  = If multiple cores should be used for the calculation 
#                           (experimental)
#               indices   = vector containing indices to be calculated
#               output    = filename to save the output
#               overwrite = if already existing files should be overwritten
#
# Output:       Raster Stack containing calculated indices
#
# Remarks: 
# ===============================================================================


rgb_indices <- function(r, g, b, 
                        parallel = F,
                        indices = c("BI", "SCI", "GLI", "HI", "NDTI", "NGRDI", 
                                    "RI", "SI", "TGI", "VARI", "VVI", "GLAI", 
                                    "GRVI", "CI", "HUE", "SAT", "SHP"),
                        output = NULL,
                        overwrite = T){

  # 0 - load packages
  #---------------------
  
  require(parallel)
  require(raster)
  
  
  
  # 1 - define variables
  #---------------------
  
  rasterOptions(overwrite = T)
  
  indices <- sapply(indices, toupper)
  if(parallel){rgb <- stack(r,g,b)}
  
  ## Build a dataframe with all available indices
  indexShort <- c("BI", "SCI", "GLI", "HI", "NDTI", "NGRDI", 
                "RI", "SI", "TGI", "VARI", "VVI", "GLAI", 
                "GRVI", "CI", "HUE", "SAT", "SHP")


  indexNames <- c("Brightness Index", 
  "Soil Colour Index", 
  "Green leaf index", 
  "Primary colours hue index", 
  "Normalized difference turbidity Index (Water)", 
  "Normalized green red difference index",
  "Redness Index",
  "Spectral Slope Saturation Index",
  "Triangular greenness index (TGI)",
  "Visible Atmosherically Resistant Index",
  "Visual Vegetation Index",
  "Green Leaf Area Index",
  "Green-Red Vegetation Index",
  "Red Coloration Index",
  "Hue Index",
  "Saturation Index",
  "Shape Index")


  formula <- c("sqrt((r^2 + g^2 + b*2) / 3) ",
                "(r - g) / (r + g) ",
                "(2*g-r-b)/(2*g+r+b) ",
                "(2*r-g-b)/(g-b) ",
                "(r - g) / (r + g) ",
                "(g-r)/(g+r) ",
                "r^2 / (b*g^3) ",
                "(r - b) / (r + b) ",
                "-0.5*(190*(r - g)- 120*(r - b)) ",
                "(g - r) / (g + r - b) ",
                "(1 - abs((r - 30) / (r + 30))) * (1 - abs((g - 50) / (g + 50))) * (1 - abs((b - 1) / (b + 1))) ",
                "(25 * ((g - r) / (g +  r -  b )) + 1.25 ) ",
                "(g-r)/(g+r) ",
                "(r - b) / r ",
                "atan(2 * (r - g - b) / 30.5 * (g - b)) ",
                "(max(r,g,b)-min(r,g,b))/max(r,g,b)",
                "2 * (r - g - b) / (g - b) ")


  indexDF <- data.frame(indexShort, indexNames, formula, stringsAsFactors = F)
  
  
  
  # 2 - start code 
  #--------------------
  
  calculateIndex <- function(shortIndex){
    require(raster)
    
    if(shortIndex == "SAT"){
      max <- max(r,g,b)
      min <- min(r,g,b)
      indexValues <- (max-min)/max
      names(indexValues) <- shortIndex
      return(indexValues)
    } else {
      formula <- function(r,g,b){ 
        getFormula <- indexDF[indexDF[,1] %in% shortIndex,3]
        eval(str2expression(getFormula))
      }
      
      indexValues <- overlay(r, g, b, fun = formula)
      names(indexValues) <- shortIndex
      return(indexValues)
    }
  }
  
  if(parallel){
    cl <- makeCluster(detectCores(logical = F))
    clusterExport(cl, list("indexDF","r","g","b"), envir = environment())
    indexRaster <- clusterApply(cl, indices, calculateIndex)
    stopCluster(cl)
  } else {
    indexRaster <- lapply(indices, calculateIndex)
  }
  
  indexBrick <- brick(indexRaster)
  if(!is.null(output)) writeRaster(indexBrick, output)
  return(indexBrick)
}
