splitPlace = function(df){
  place = strsplit(as.character(df$Place), ",")
  
  place_df = lapply(place, function(i){
    p1 = sub("^\\s+", "", i[1])  # Trim leading white spaces
    if(length(i) > 2){
      p2 = sub("^\\s+", "", i[2])
      p3 = sub("^\\s+", "", i[3])
    } else if (length(i) > 1){
      p2 = sub("^\\s+", "", i[2])
      p3 = NA
    } else {
      p2 = NA
      p3 = NA
    }
    data.frame(A = p1,
               B = p2,
               C = p3)
  })
  place_df = do.call("rbind", place_df)
  place_df$ID = df$ID 
  place_df$Year = df$Year
  head(place_df)
  
  place_df[!is.na(place_df$C),] = place_df[!is.na(place_df$C), c(1,3,2, 4, 5)]
  
  for(r in seq(nrow(place_df))){
    if(is.na(place_df$B[r]) &
       grepl("kreis", tolower(place_df$A[r]))){
      place_df$B[r] = "Landkreis"
    }
  }
  unique(df_long$Place[is.na(place_df$B)])
  
  place_df$B[is.na(place_df$B) & nchar(as.character(place_df$ID) == 2)] = "Bundesland"
  place_df$B[place_df$ID == "DG"] = "Land"
  head(place_df)
  
  df_long_final = merge(df_long, place_df, by = c("ID", "Year"))
  
  df_long_final$Place = NULL 
  
  return(df_long_final)
}