#HA4_Carl_Step3

cy_fun <- function(x){
  cy <- read.table(file.path(x),
                   skip = 6, header = TRUE, sep = ";", dec = ",",fill = TRUE, nrows = c(8925:8930))
  
  str(cy)
  
  #Anpassen der ersten drei Reihennamen
  names(cy) <- c("Jahr", "ID", "Ort","Winterweizen", "Roggen.und.Wintermenggetreide", "Wintergerste ", "Sommergerste", "Hafer", "Triticale", "Kartoffeln", "Zuckerrueben", "Winterraps", "Silomais")
  
  ##Ersetzen der leeren Felder durch NA und Anpassen des Dezimalzeichen
  for(c in colnames(cy)[4:13]){
    cy[, c][cy[, c] == "."] <- NA
    cy[, c][cy[, c] == "-"] <- NA
    cy[, c] <- as.numeric(sub(",", ".", as.character(cy[, c])))
  }
  
  cy_long <- melt(cy, id.vars = c("Jahr", "ID", "Ort"))
  head(cy_long)
  
  Ort <- strsplit(as.character(cy$Ort), ",")
  head(Ort)
  
  Ort_df <- lapply(Ort, function(i){
    p1 <- sub("^\\s+", "", i[1])  # Trim leading white spaces
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
  Ort_df <- do.call("rbind", Ort_df)
  Ort_df$ID <- cy$ID 
  Ort_df$Jahr <- cy$Jahr
  head(Ort_df)
  
  #Zur Untersuchung welche Attributtypen es in Spalte 2 bzw 3 gibt
  unique(Ort_df[, 2])
  unique(Ort_df[, 3])
  #Damit wird geschaut welche Attribute die Spalte B annimmt, wenn die Spalte C ein Attribut beinhaltet
  unique(Ort_df$B[!is.na(Ort_df$C)])
  
  #Damit die Reihenfolge passt, muss Reihe 2 und 3 den Paltz innerhalb des Dataframe tauschen
  Ort_df[!is.na(Ort_df$C),] <- Ort_df[!is.na(Ort_df$C), c(1,3,2, 4, 5)]
  
  
  head(Ort_df)
  
  unique(cy$Ort[is.na(Ort_df$B)])
  
  #Damit werden alle kleinen Orts-Kreise unter der neuen Kategorie "Landkreis" zusammen gefasst 
  for(r in seq(nrow(Ort_df))){
    if(is.na(Ort_df$B[r]) &
       grepl("kreis", tolower(Ort_df$A[r]))){
      Ort_df$B[r] <- "Landkreis"
    }
  }
  unique(cy_long$Ort[is.na(Ort_df$B)])
  
  sum(is.na(Ort_df$B))
  
  Ort_df$B[is.na(Ort_df$B) & nchar(as.character(Ort_df$ID) == 3)] <- "Landkreis"
  Ort_df$B[is.na(Ort_df$B) & nchar(as.character(Ort_df$ID) == 2)] <- "Bundesland"
  Ort_df$B[Ort_df$ID == "DG"] <- "Land"
  
  
  
  head(Ort_df)
  
  #Zeigt das wir nun keine leeren Listeneintraege mehr in Spalte B im Place haben
  sum(is.na(Ort_df$B))
  
  #Nun wird  der bereinigte Place Dataframe wieder mit dem Daten Dataframe lu mit der Funktion Merge ueber die Spalten ID und Year zusammengebaut. 
  cy_long_final <- merge(Ort_df,cy, by = c("ID", "Jahr"))
  
  #Loeschen der Spalte Ort
  cy_long_final$Ort <- NULL
  return(cy_long_final)
  
}