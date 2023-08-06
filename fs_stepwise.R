library(caret)
library(leaps)

#' Stepwise regression with cross-validation
#'
#' Fits a stepwise regression model with backward, forward, or both stepwise
#' selection, and performs cross-validation to evaluate model performance.
#'
#' @param data A data.frame containing the variables to be used in the regression.
#' @param dependent_var The name of the dependent variable as a string.
#' @param step_type The type of stepwise selection to perform: "backward", "forward", or "both".
#' @param seed An integer seed for reproducibility. Default is NULL (no seed).
#' @param verbose Logical indicating whether to print intermediate summaries.
#' @param control_settings List of control settings for trainControl. Default is 10-fold cross-validation.
#' @param tune_grid A data frame specifying the grid of tuning parameters to explore. Default is NULL.
#' @param return_models Logical indicating whether to return the train model objects. Default is FALSE.
#'
#' @return A list containing the following items:
#' \itemize{
#' \item \code{result}: A data.frame containing the results of the cross-validation.
#' \item \code{best_tune}: The value of \code{nvmax} that gives the best performance.
#' \item \code{final_model_summary}: A summary of the final model.
#' \item \code{importance}: The variable importance from the final model.
#' \item \code{step_model_train} (optional): The final trained model, if return_models = TRUE.
#' }
#'
#' @examples
#' \dontrun{
#' library(datasets)
#' data(swiss)
#' results <- fs_stepwise(swiss, "Fertility", "backward", verbose = TRUE)
#' }
#'
#' @importFrom stats lm regsubsets
#' @importFrom caret trainControl train varImp
#' @export
fs_stepwise <- function(data, 
                        dependent_var, 
                        step_type = "both", 
                        seed = NULL, 
                        verbose = FALSE, 
                        control_settings = trainControl(method = "cv", number = 10),
                        tune_grid = NULL,
                        return_models = FALSE) {
  
  if(!is.data.frame(data)){
    stop("Input 'data' should be a data frame")
  }
  
  if(is.character(dependent_var)){
    dep_var <- dependent_var
  } else {
    dep_var <- deparse(substitute(dependent_var))
  }
  
  
  if(!(dep_var %in% colnames(data))){
    stop("Dependent variable is not a valid column in the data frame")
  }
  
  if(!step_type %in% c("backward", "forward", "both")){
    stop("Invalid step_type")
  }
  
  if(!is.null(seed)) {
    set.seed(seed)
  }
  
  formula <- reformulate(".", response = dep_var)
  
  # Set 'nvmax' to the number of predictors
  nvmax <- ncol(data) - 1
  if(is.null(tune_grid)) {
    tune_grid <- data.frame(nvmax = 1:nvmax)
  }
  
  models <- regsubsets(formula, data = data, nvmax = nvmax, method = "seqrep")
  
  if(verbose) {
    print(summary(models))
  }
  
  # Map step_type to method
  method <- switch(step_type,
                   "both" = "leapSeq",
                   "backward" = "leapBackward",
                   "forward" = "leapForward"
  )
  
  step_model_train <- train(formula, data = data,
                            method = method, 
                            tuneGrid = tune_grid,
                            trControl = control_settings
  )
  
  result <- step_model_train$results
  best_tune <- step_model_train$bestTune
  final_model_summary <- summary(step_model_train$finalModel)
  
  # Get the variable importance from the final model
  importance <- varImp(step_model_train)
  
  output_list <- list(result = result, 
                      best_tune = best_tune, 
                      final_model_summary = final_model_summary, 
                      importance = importance)
  if(return_models) {
    output_list$step_model_train <- step_model_train
  }
  
  return(output_list)
}
