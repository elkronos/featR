# Load required packages
library(brms)
library(tidyverse)
library(parallel)
library(loo)

#' Bayesian Feature Selection for Model Optimization
#'
#' Fits multiple Bayesian models to the data, using different combinations of predictor columns,
#' and selects the best model based on WAIC. The function then appends the fitted values and residuals 
#' from the best model to the input dataset.
#' 
#' @param data A data frame containing the data to be modeled.
#' @param response_col A character string indicating the name of the response column.
#' @param predictor_cols A character vector indicating the names of predictor columns.
#' @param date_col A character string indicating the name of the date column. Default is NULL.
#' @param prior An object of class `prior`. Default is NULL.
#' @param brm_args A list of additional arguments to be passed to the `brm` function. Default is an empty list.
#' @importFrom parallel detectCores
#' @importFrom brms brm waic fitted resid
#' @return A list containing the best model, modified data, MAE, and RMSE of the best model.
#' @examples
#' \dontrun{
#' library(brms)
#' library(tidyverse)
#' data <- data.frame(response = rnorm(100), predictor1 = rnorm(100), predictor2 = rnorm(100))
#' result <- fs_bayes(data, "response", c("predictor1", "predictor2"))
#' }
fs_bayes <- function(data, response_col, predictor_cols, date_col = NULL,
                     prior = NULL, brm_args = list()) {
  
  # Add week feature if date column is provided
  if (!is.null(date_col)) {
    data <- data %>%
      mutate(week = as.integer(format(!!sym(date_col), "%U")))
    predictor_cols <- c(predictor_cols, "week")
  }
  
  # Generate all possible combinations of predictor columns
  predictor_combinations <- unlist(
    lapply(seq_along(predictor_cols),
           function(i) combn(predictor_cols, i, simplify = FALSE)),
    recursive = FALSE
  )
  
  best_model <- NULL
  best_waic <- Inf
  
  # Setup for parallel processing
  n_cores <- detectCores(logical = FALSE)
  
  # Fit models for each predictor combination
  for (predictor_comb in predictor_combinations) {
    
    formula_str <- paste0(response_col, " ~ ", paste(predictor_comb, collapse = " + "))
    model <- fit_model(data, formula_str, n_cores, prior, brm_args)
    
    if (!is.null(model)) {
      waic_val <- waic(model)$estimates["waic"]
      update_best_model(waic_val, model)
    }
  }
  
  data <- add_metrics_to_data(data, best_model)
  
  cat("Best Model:", as.character(formula(best_model)), "\n")
  cat("Best WAIC:", best_waic, "\n")
  
  mae <- mean(data$abs_residuals)
  rmse <- sqrt(mean(data$squared_residuals))
  
  cat("MAE:", mae, "\n")
  cat("RMSE:", rmse, "\n")
  
  return(list("Model" = best_model, "Data" = data, "MAE" = mae, "RMSE" = rmse))
}

#' Fit Bayesian Model
#'
#' Fits a Bayesian model to the data based on the specified formula and returns the model.
#' 
#' @param data A data frame containing the data.
#' @param formula_str A character string representing the formula for the model.
#' @param n_cores Integer indicating the number of cores to use.
#' @param prior An object of class `prior`.
#' @param brm_args A list of additional arguments to be passed to the `brm` function.
#' @return A `brmsfit` object if successful, or NULL if there's an error.
#' @importFrom brms brm
#' @examples
#' \dontrun{
#' library(brms)
#' data <- data.frame(response = rnorm(100), predictor1 = rnorm(100))
#' fit_model(data, "response ~ predictor1", 1, NULL, list())
#' }
fit_model <- function(data, formula_str, n_cores, prior, brm_args) {
  tryCatch({
    model <- do.call(
      brm,
      modifyList(
        list(
          formula = as.formula(formula_str),
          data = data,
          family = gaussian(),
          prior = prior,
          cores = n_cores,
          iter = 4000
        ),
        brm_args
      )
    )
    return(model)
  }, warning = function(w) {
    message("Warning for Model:", formula_str, ": ", w$message)
    return(NULL)
  }, error = function(e) {
    message("Error for Model:", formula_str, ": ", e$message)
    return(NULL)
  })
}

#' Update Best Model Based on WAIC
#'
#' Updates the global variables best_model and best_waic if the provided model has a lower WAIC.
#' 
#' @param waic_val Numeric value indicating the WAIC of the current model.
#' @param model The `brmsfit` object representing the current model.
update_best_model <- function(waic_val, model) {
  if (!is.na(waic_val) && waic_val < best_waic) {
    best_model <<- model
    best_waic <<- waic_val
  }
}

#' Add Fitted Values and Residuals to Data
#'
#' Appends fitted values, residuals, absolute residuals, and squared residuals 
#' from the specified model to the input dataset.
#' 
#' @param data A data frame.
#' @param model A `brmsfit` object.
#' @return A modified data frame with added columns for fitted values and residuals.
#' @examples
#' \dontrun{
#' library(brms)
#' data <- data.frame(response = rnorm(100), predictor1 = rnorm(100))
#' model <- brm(response ~ predictor1, data = data)
#' add_metrics_to_data(data, model)
#' }
add_metrics_to_data <- function(data, model) {
  fitted_values <- fitted(model)
  residuals <- resid(model)
  
  data$fitted_values <- fitted_values
  data$residuals <- residuals
  data$abs_residuals <- abs(residuals)
  data$squared_residuals <- residuals^2
  
  return(data)
}