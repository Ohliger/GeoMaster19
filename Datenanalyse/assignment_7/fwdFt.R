forward_feature_selection <- function(data, dep, vars, selected_vars = NULL){
  fwd_fs <- lapply(seq(length(vars)), function(v){
    if(is.null(selected_vars)){
      formula <- paste(dep, " ~ ", paste(vars[v], collapse=" + "))
    } else {
      formula <- paste(dep, " ~ ", paste(c(selected_vars, vars[v]), collapse=" + "))
    }
    
    # Define training control
    set.seed(123)
    train_control <- trainControl(method = "cv", number = 3)
    # Train the model
    model <- train(as.formula(formula), data = data, method = "lm",
                   trControl = train_control)
    
    lmod <- lm(formula, data = data)
    results <- data.frame(Variable = vars[v],
                          Adj_R_sqrd = round(summary(lmod)$adj.r.squared, 4),
                          RMSE = model$results$RMSE,
                          AIC = round(AIC(lmod), 4))
    return(results)
  })
  fwd_fs <- do.call("rbind", fwd_fs)
  
  if(!is.null(selected_vars)){
    formula <- paste(dep, " ~ ", paste(selected_vars, collapse=" + "))
    
    # Define training control
    set.seed(123)
    train_control <- trainControl(method = "cv", number = 3)
    # Train the model
    model <- train(as.formula(formula), data = data, method = "lm",
                   trControl = train_control)
    
    lmod <- lm(formula, data = data)
    results_selected <- data.frame(Variable = paste0("all: ", paste(selected_vars, collapse=", ")),
                                   Adj_R_sqrd = round(summary(lmod)$adj.r.squared, 4),
                                   RMSE = model$results$RMSE,
                                   AIC = round(AIC(lmod), 4))
    fwd_fs <- rbind(results_selected, fwd_fs)
  }
  print(fwd_fs)
  
  current <- fwd_fs$AIC[1]
  if(any(fwd_fs$AIC < current)){
    best_var <- as.character(fwd_fs$Variable[which(fwd_fs$AIC == min(fwd_fs$AIC))])
    return(best_var)
  } else {
    return(F)
  }
}