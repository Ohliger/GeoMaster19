forward_feature_selection <- function(data, dep, vars, selected_vars = NULL, stat_indc = "AIC"){
  require(caret)
  fwd_fs <- lapply(seq(length(vars)), function(v){
    if(is.null(selected_vars)){
      formula <- paste(dep, " ~ ", paste(vars[v], collapse=" + "))
    } else {
      formula <- paste(dep, " ~ ", paste(c(selected_vars, vars[v]), collapse=" + "))
    }
    #Leave many out cross validation
    
    set.seed(123)
    train.control <- trainControl(method = "cv", number = 3)
    model <- train(as.formula(formula), data = data, method = "lm",
                          trControl = train.control)

    lmod <- lm(formula, data = data)
    results <- data.frame(Variable = vars[v],
                          RMSE = model$results$RMSE,
                          AIC = round(AIC(lmod), 4))
    return(results)
  })
  fwd_fs <- do.call("rbind", fwd_fs)
  
  if(!is.null(selected_vars)){
    formula <- paste(dep, " ~ ", paste(selected_vars, collapse=" + "))
    lmod <- lm(formula, data = data)
    set.seed(123)
    train.control <- trainControl(method = "cv", number = 3)
    model <- train(as.formula(formula), data = data, method = "lm",
                   trControl = train.control)
    results_selected <- data.frame(Variable = paste0("all: ", paste(selected_vars, collapse=", ")),
                                   RMSE = model$results$RMSE,
                                   AIC = round(AIC(lmod), 4))
    fwd_fs <- rbind(results_selected, fwd_fs)
  }
  
  
  best_var <- as.character(fwd_fs$Variable[which(fwd_fs[[stat_indc]] == min(fwd_fs[[stat_indc]]))])
  
  
  first <- fwd_fs[[stat_indc]][1]
  test <- (fwd_fs[[stat_indc]][fwd_fs[[stat_indc]]<first])
  
  if(length(test)==0){
    return(FALSE)
  } else return(c(best_var,min(fwd_fs[[stat_indc]])))
}
