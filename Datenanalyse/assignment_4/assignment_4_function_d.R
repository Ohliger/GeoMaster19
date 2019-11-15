
## create function -------------------------------------------

crop.data.sorting <- function(path_of_dataset) {
  crop_yield <- read.csv(path_of_dataset, skip = 6, sep = ";", dec = ",")
  crop_yield <- crop_yield[-c(8926:nrow(crop_yield)),]

  names(crop_yield) <- c("Jahr", "ID", "Ort", names(crop_yield[,4:ncol(crop_yield)]))

  for(c in colnames(crop_yield)[4:ncol(crop_yield)]){
    crop_yield[, c][crop_yield[, c] == "."] <- NA
    crop_yield[, c] <- as.numeric(sub(",", ".", as.character(crop_yield[, c])))
  }

  ort <- strsplit(as.character(crop_yield$Ort), ",")


  ort_df <- lapply(ort, function(i){
    p1 <- sub("^\\s+", "", i[1])
    if(length(i) > 2){
      p2 <- sub("^\\s+", "", i[2])
      p3 <- sub("^\\s+", "", i[3])
    } else if (length(i) > 1){
      p2 <- sub("^\\s+", "", i[2])
      p3 <- NA
    } else {
      p2 <- NA
      p3 <- NA
    }
    data.frame(A = p1,
               B = p2,
               C = p3)
  })


  ort_df <- do.call("rbind", ort_df)

  ort_df[!is.na(ort_df$C),] <- ort_df[!is.na(ort_df$C), c(1,3,2)]


  for(r in seq(nrow(ort_df))){
    if(is.na(ort_df$B[r]) & grepl("kreis", tolower(ort_df$A[r]))){
      ort_df$B[r] <- "Landkreis"
    }
  }

  ort_df$ID <- crop_yield$ID

  ort_df$B[is.na(ort_df$B) & nchar(as.character(ort_df$ID) == 2)] <- "Bundesland"
  ort_df$B[ort_df$ID == "DG"] <- "Land"

  ort_df$Jahr <- crop_yield$Jahr

  crop_yield <- merge.data.frame(ort_df, crop_yield[,-3], by = c("Jahr", "ID"))

  crop_yield <<- crop_yield # needed for having a data frame as output; otherwise, the crop_yield data frame would only be stored temporarily and be deleted, when the function finishes
}

crop.data.sorting("C:/Users/David/Google Drive/Studium/WiSe19/mpg-envinsys-plygrnd/data_analysis/data/crop_yield_not_clean.csv")

