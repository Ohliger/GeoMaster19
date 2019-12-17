## K-FOLD CROSS VALIDATION ##
# Implementation of a K-Fold Cross Validation Algorithm
# Splits the data in k mutually exclusive, even folds and
# returns the mean RMSE of all k validation runs

k_fold_cv <- function(data, formula, folds){
  
  range <- nrow(data)
  rangeValues <- 1:range
  trainSize <- range %/% folds
  
  cv_sample <- lapply(1:folds, function(fold){
    set.seed(range+folds)
    formula <- as.formula(formula)
    dep <- all.vars(formula)[1]
    
    smpl <- sample(rangeValues, trainSize)
    rangeValues <- rangeValues[-smpl]
    
    train <- data[-smpl,]
    test <- data[smpl,]
    lmod <- lm(formula, data = train)
    pred <- predict(lmod, newdata = test)
    obsv <- test[[dep]]
    resid <- obsv - pred
    rmse <- sqrt(mean((resid)^2))
    return(rmse)
  })
  return(mean(cv_sample[[1]]))
}


forward_feature_selection <- function(data, dep, vars, selection_parameter = "AIC"){
  selected_vars <- NULL
  performance <- NULL
  
  lowerBetter <- selection_parameter %in% c("AIC", "RMSE")
  higherBetter <- selection_parameter %in% c("Adj_R_sqrd")
  if(!(lowerBetter | higherBetter)){
    stop("Selection parameter is not available.")
  }
  
  ## CALCULATE PERFORMANCE ##
  # Takes independent variables, calculates linear model and returns 3 different
  # performance measures for the model. 
  
  calc_performance <- function(v, additional = NULL){
    formula <- paste(dep, " ~ ", paste(c(v, additional), collapse=" + "))
    
    rmse <- k_fold_cv(data, formula, 3)
    
    lmod <- lm(formula, data = data)
    results <- data.frame(Variable = paste(v, collapse=", "),
                          Adj_R_sqrd = round(summary(lmod)$adj.r.squared, 4),
                          RMSE = rmse,
                          AIC = round(AIC(lmod), 4))
    return(results)
  }
  
  repeat{
    
    # Calculate the model performance with all remaining independet 
    # variables added one by one to the current model
    
    fwd_fs <- lapply(vars, calc_performance, additional = selected_vars)
    fwd_fs <- do.call("rbind", fwd_fs)
    
    
    ## VARIABLE SELECTION ##
    # Check if any of the additional variables improve the prediction.
    # Pick the predictor with the biggest improvement and append to selected_vars
    # If there isn't any improvement break() to end the loop.
    #
    # 'is.null(current)' check is necessary for the first iteration of the loop.
    
    selection_row <- fwd_fs[[selection_parameter]]
    current <- performance[length(performance)]
    
    if(lowerBetter & (any(selection_row < current) | is.null(current))){
      best_row <- which.min(selection_row)
    } else if(higherBetter & (any(selection_row > current) | is.null(current))){
      best_row <- which.max(selection_row)
    } else {
      break()
    } 
    
    best_var <- as.character(fwd_fs$Variable[best_row])
    selected_vars <- append(selected_vars, best_var)
    vars <- vars[!best_row]
    
    best_perf <- selection_row[best_row]
    performance <- append(performance, best_perf)
    
  }

  var_perf <- data.frame(Added.Variable = selected_vars,
                         Performance = performance)
  return(var_perf)
}



