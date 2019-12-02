rgb_indices <- function(red,green,blue,rgbi=c("VVI","VARI","NDTI","RI","SCI","BI",
                                              "SI","HI",
                                              "TGI","GLI",
                                              "NGRDI","GRVI",
                                              "GLAI","HUE",
                                              "CI","SAT","SHP")) {
  
  ## compatibility check
  #  if (raster::nlayers(rgb) < 3)
  #    stop("Argument 'rgb' needs to be a Raster* object with at least 3 layers (usually red, green and blue).")
  
  ### processing
  
  
  ## separate visible bands
  # red <- raster::raster(rgb[[1]])
  # green <- raster::raster(rgb[[2]])
  # blue <- raster::raster(rgb[[3]])
  
  
  indices <- lapply(rgbi, function(item){
    ## calculate Visible Vegetation Index vvi
    if (item == "VVI"){
      cat(getCrayon()[[3]](":::: Visible Vegetation Index  ",item,"\n"))
      #cat("\n      calculate Visible Vegetation Index (VVI)")
      VVI <- (1 - abs((red - 30) / (red + 30))) *
        (1 - abs((green - 50) / (green + 50))) *
        (1 - abs((blue - 1) / (blue + 1)))
      names(VVI) <- "VVI"
      return(VVI)
      
    } else if (item == "VARI") {
      # calculate Visible Atmospherically Resistant Index (VARI)
      cat(getCrayon()[[3]](":::: Visible Atmospherically Resistant Index ",item,"\n"))
      #cat("\n      calculate Visible Atmospherically Resistant Index (VARI)")
      VARI <- (green - red) / (green + red - blue)
      names(VARI) <- "VARI"
      return(VARI)
      
    } else if (item == "NDTI") {
      ## Normalized difference turbidity index
      cat(getCrayon()[[3]](":::: Normalized Difference Turbidity Index ",item,"\n"))
      #cat("\n      calculate Normalized difference turbidity index (NDTI)")
      NDTI <- (red - green) / (red + green)
      names(NDTI) <- "NDTI"
      return(NDTI)
      GLAI
    } else if (item == "RI") {
      # redness index
      cat(getCrayon()[[3]](":::: Redness Index ",item,"\n"))
      #cat("\n      calculate redness index (RI)")
      RI <- red**2 / (blue*green**3)
      names(RI) <- "RI"
      return(RI)
      
    } else if (item == "SCI") {
      # SCI Soil Colour Index
      cat(getCrayon()[[3]](":::: Soil Colour Index ",item,"\n"))
      #cat("\n      calculate Soil Colour Index (SCI)")
      SCI <- (red - green) / (red + green)
      names(SCI) <- "SCI"
      return(SCI)
      
    } else if (item == "BI") {
      #  Brightness Index
      cat(getCrayon()[[3]](":::: Brightness Index ",item,"\n"))
      #cat("\n      calculate Brightness Index (BI)")
      BI <- sqrt((red**2 + green**2 + blue*2) / 3)
      names(BI) <- "BI"
      return(BI)
      
    } else if (item == "SI") {
      # SI Spectra Slope Saturation Index
      cat(getCrayon()[[3]](":::: Spectra Slope Saturation Index ",item,"\n"))
      #cat("\n      calculate Spectra Slope Saturation Index (SI)")
      SI <- (red - blue) / (red + blue)
      names(SI) <- "SI"
      return(SI)
      
    } else if (item=="HI"){
      # HI Primary colours Hue Index
      cat(getCrayon()[[3]](":::: Primary Colours Hue Index ",item,"\n"))
      #cat("\n      calculate Primary colours Hue Index (HI)")
      HI<-(2*red-green-blue)/(green-blue)
      names(HI) <- "HI"
      return(HI)
      
    } else if (item=="TGI"){
      # Triangular greenness index
      cat(getCrayon()[[3]](":::: Triangular Greenness Index ",item,"\n"))
      #cat("\n      calculate Triangular greenness index (TGI)")
      TGI <- -0.5*(190*(red - green)- 120*(red - blue))
      names(TGI) <- "TGI"
      return(TGI)
      
    } else if (item=="GLI"){
      #cat("\n      calculate Green leaf index (GLI)")
      cat(getCrayon()[[3]](":::: Green Leaf Index ",item,"\n"))
      # Green leaf index
      GLI<-(2*green-red-blue)/(2*green+red+blue)
      names(GLI) <- "GLI"
      return(GLI)
      
    } else if (item=="NGRDI"){
      # NGRDI Normalized green red difference index
      cat(getCrayon()[[3]](":::: Normalized Green-Red Difference Index ",item,"\n"))
      #cat("\n      calculate Normalized green red difference index  (NGRDI)")
      NGRDI<-(green-red)/(green+red)
      names(NGRDI) <- "NGRDI"
      return(NGRDI)
      
    }  else if (item=="GLAI"){
      # NGRDI Normalized green red difference index
      cat(getCrayon()[[3]](":::: Greenish Leaf Area Index ",item,"\n"))
      #cat("\n      calculate greenish Leaf Area Index  (GLAI) (highly experimental)")
      # vevi<-(green - red) / (green +  red -  blue )
      GLAI = (25 * ((green - red) / (green +  red -  blue )) + 1.25 )
      names(GLAI) <- "GLAI"
      return(GLAI)
      
    }  else if (item=="GRVI"){
      # GRVI  Green-Red Vegetation Index  Remote Sensing 2010, 2, 2369-2387; doi:10.3390/rs2102369
      cat(getCrayon()[[3]](":::: Green-Red Vegetation Index ",item,"\n"))
      #cat("\n      calculate  Green-Red Vegetation Index   (GRVI)")
      GRVI<-(green-red)/(green+red)
      names(GRVI) <- "GRVI"
      return(GRVI)
      
    } else if (item == "CI") {
      # CI  https://www.indexdatabase.de/search/?s=color
      cat(getCrayon()[[3]](":::: Coloration Index ",item,"\n"))
      # cat("\n      calculate Coloration Index (CI)")
      CI <- (red - blue) / red
      names(CI) <- "CI"
      return(CI)
      
    } else if (item == "HUE") {
      # HUE Index https://www.indexdatabase.de/search/?s=HUE
      cat(getCrayon()[[3]](":::: Hue Index ",item,"\n"))
      #cat("\n      calculate Hue Index (HUE)")
      HUE <- 	 atan(2 * (red - green - blue) / 30.5 * (green - blue))
      names(HUE) <- "HUE"
      return(HUE)
      
    }  else if (item == "SAT") {
      # Saturation Index https://www.indexdatabase.de/db/i-single.php?id=77
      cat(getCrayon()[[3]](":::: Saturation Index ",item,"\n"))
      #cat("\n      calculate Saturation Index (SAT)")
      SAT <- 	 (max(red,green,blue) - min(red,green,blue)) / max(red,green,blue)
      names(SAT) <- "SAT"
      return(SAT)
      
    } else if (item == "SHP") {
      # Shape Index https://www.indexdatabase.de/search/?s=shape
      cat(getCrayon()[[3]](":::: Shape Index ",item,"\n"))
      #cat("\n      calculate Shape Index (SHP)")
      SHP <- 	 2 * (red - green - blue) / (green - blue)
      names(SHP) <- "SHP"
      return(SHP)
      
    }
  })
  return(raster::stack(indices))
}

getCrayon <- function(){
  head <- crayon::black $ bgGreen
  err  <- crayon::red $ bold
  note <- crayon::blue $ bold
  ok   <- crayon::green $ bold
  return(list(note,err,ok,head))
}