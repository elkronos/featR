# Feature Selection Repository

This repository contains a collection of R functions to assist with feature selection and dimensionality reduction. Each script is thoroughly documented with its purpose, key assumptions, usage examples, and expected outputs to help you select the most appropriate method for your data analysis and predictive modeling tasks.

The functions can be organized into four main categories:

- **1. Statistical & Filter Methods**
- **2. Regularization Methods**
- **3. Model-Based & Wrapper Methods**
- **4. Dimensionality Reduction Techniques**

---

## 1. Statistical & Filter Methods

These functions evaluate features using statistical tests or simple filtering criteria to determine their relevance to the target variable.

| Function Name       | Description                                                                                         | Key Assumptions                                                                                      | Outputs                                                         | Primary Applications                                         | When to Use                                                                     |
|---------------------|-----------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------|--------------------------------------------------------------|---------------------------------------------------------------------------------|
| **`fs_chi`**        | Uses chi-square tests to assess the association between categorical features and the target.        | Target and categorical features exist; chi-square test assumptions are met.                         | List of significant features and corresponding p-values.         | Selecting significant categorical features.                  | When you need to statistically evaluate categorical feature relevance.          |
| **`fs_correlation`**| Removes highly correlated numeric features based on a specified threshold.                         | Data is numeric; valid correlation method (Pearson/Spearman); threshold between 0 and 1.              | Correlation matrix and names of selected variables.             | Handling multicollinearity and reducing redundancy.           | When you want to eliminate redundant features to improve model performance.     |
| **`fs_infogain`**   | Computes the information gain for each feature relative to the target variable.                     | Target exists; appropriate handling of numeric and categorical data.                               | Data frame of features with corresponding information gains.      | Ranking features by relevance for prediction tasks.           | When you need to assess which features most reduce uncertainty.                |
| **`fs_variance`**   | Applies variance thresholding to retain/remove features with high/low variability.                             | Data is numeric; threshold is a non-negative numeric value.                                        | Filtered dataset containing only features with sufficient variance. | Reducing dimensionality by eliminating low-information features. | When you want to remove features that contribute little variation.             |

---

## 2. Regularization Methods

These functions integrate feature selection directly into the modeling process by applying regularization techniques.

| Function Name       | Description                                                                                         | Key Assumptions                                                                                      | Outputs                                                         | Primary Applications                                         | When to Use                                                                     |
|---------------------|-----------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------|--------------------------------------------------------------|---------------------------------------------------------------------------------|
| **`fs_elastic`**    | Performs elastic net regression with cross-validation (combining Lasso and Ridge penalties).         | Data contains numeric predictors/response; proper hyperparameter grid and missing value handling.    | Best model coefficients, optimal alpha & lambda values, RMSE.      | Handling multicollinearity and selecting relevant predictors.  | When you need a robust model that balances variable selection with regularization. |
| **`fs_lasso`**      | Fits a Lasso regression model using cross-validation to identify key predictors via sparsity.        | Data contains numeric predictors/response; valid tuning parameters and preprocessing steps applied.  | Variable importance scores and optionally the fitted Lasso model.   | Variable selection and regularization in regression tasks.      | When you require a sparse model to enhance prediction accuracy and interpretability. |

---

## 3. Model-Based & Wrapper Methods

These functions integrate feature selection with model training, often using iterative or ensemble approaches.

| Function Name            | Description                                                                                         | Key Assumptions                                                                                      | Outputs                                                         | Primary Applications                                         | When to Use                                                                     |
|--------------------------|-----------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------|--------------------------------------------------------------|---------------------------------------------------------------------------------|
| **`fs_bayes`**           | Uses Bayesian modeling (via the `brms` package) to explore predictor combinations and select the best model using LOO cross-validation. | Data is properly preprocessed; Bayesian priors and model structure are appropriate.                 | Best model, fitted values, residuals, MAE, and RMSE.                | Selecting predictors with uncertainty estimates.              | When you need robust evaluation using Bayesian inference despite higher computation cost. |
| **`fs_boruta`**          | Implements the Boruta algorithm to determine all relevant features, with options to filter highly correlated ones. | Target exists; supports parallel processing and proper correlation handling.                       | Selected features and the full Boruta result object.               | Automated feature selection in high-dimensional datasets.      | When you require a robust, out-of-the-box method for feature relevance.         |
| **`fs_randomforest`**    | Applies random forest for classification or regression with integrated feature importance evaluation. | Target variable exists; preprocessing steps and task type (classification/regression) are defined.    | Trained model, predictions, and performance metrics (accuracy or RMSE). | Building ensemble models with built-in feature evaluation.     | When you need a versatile method that handles non-linear relationships and feature ranking. |
| **`fs_recursivefeature`**| Executes Recursive Feature Elimination (RFE) to iteratively select an optimal subset of features.     | Data is properly preprocessed; RFE control parameters are correctly specified.                      | Optimal feature subset, variable importance scores, resampling results, and optionally the final model. | Systematic feature selection to improve model interpretability. | When you require an iterative elimination process to fine-tune your feature set. |
| **`fs_stepwise`**        | Performs stepwise regression with nested cross-validation to select features and tune the model.      | Dependent variable exists; a valid stepwise selection strategy (forward/backward/both) is provided.    | Cross-validation results, best tuning parameters, final model summary, and variable importance. | Feature selection in regression via systematic elimination.    | When you need a methodical, cross-validated approach to feature selection.      |
| **`fs_svm`**             | Trains an SVM model (for classification or regression) with optional feature selection and class imbalance handling. | Data is valid; target variable exists; task type and cross-validation settings are correctly defined.  | Trained SVM model, test set predictions, and performance metrics (accuracy, R², MSE, or MAE). | Building SVM models with integrated feature selection.         | When you need a flexible SVM approach that includes feature selection and imbalance management. |
| **`fs_mars`**            | Trains and evaluates a Multivariate Adaptive Regression Splines (MARS) model to capture non-linear relationships. | Required libraries are loaded; proper handling of missing values and hyperparameter tuning is in place.  | Trained MARS model and performance metrics (RMSE for regression or accuracy for classification). | Modeling complex, non-linear relationships with automatic feature selection. | When you require an interpretable model that can capture non-linear interactions. |

---

## 4. Dimensionality Reduction Techniques

These functions reduce the number of features by transforming or decomposing the data into lower-dimensional representations.

| Function Name   | Description                                                                                         | Key Assumptions                                                                                      | Outputs                                                         | Primary Applications                                         | When to Use                                                                     |
|-----------------|-----------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------|--------------------------------------------------------------|---------------------------------------------------------------------------------|
| **`fs_pca`**    | Executes Principal Component Analysis (PCA) to reduce dimensionality and visualize underlying data patterns. | Data is numeric; PCA assumptions are met; number of components to retain is specified.              | PCA loadings, scores, variance explained, and optional visual plots. | Dimensionality reduction and data visualization.              | When you need to simplify data complexity or explore underlying structures.     |
| **`fs_svd`**    | Performs Singular Value Decomposition (SVD) on a numeric matrix, optionally scaling and centering the data. | Input is a valid numeric matrix with no missing values; scaling option is specified correctly.       | Singular values, left singular vectors, and right singular vectors.  | Data compression, noise reduction, and latent structure analysis. | When you need matrix decomposition for dimensionality reduction or feature extraction. |

---

## Detailed Function Descriptions

### 1. `fs_bayes`
Utilizes Bayesian methods (via the `brms` package) to evaluate all possible combinations of predictors. It selects the optimal model based on the lowest Leave-One-Out Cross-Validation (LOO) value and returns key metrics (MAE and RMSE) along with fitted values and residuals.

### 2. `fs_boruta`
Applies the Boruta algorithm to assess feature importance automatically, removing the target variable prior to analysis. It can also limit the number of features and filter out highly correlated ones, making it ideal for high-dimensional datasets.

### 3. `fs_chi`
Conducts chi-square tests to determine the significance of associations between categorical features and the target variable. It returns the names of significant features and their p-values, aiding in dimensionality reduction for categorical data.

### 4. `fs_correlation`
Calculates a correlation matrix for numeric data to identify pairs of highly correlated features. It then selects features based on a user-specified threshold, helping to mitigate issues with multicollinearity.

### 5. `fs_elastic`
Performs elastic net regression—an approach that blends Lasso and Ridge penalties—with cross-validation to identify optimal regularization parameters. It returns the best model coefficients along with the chosen alpha and lambda values.

### 6. `fs_infogain`
Computes the information gain for each feature relative to the target variable. This metric helps in ranking features according to their ability to reduce uncertainty, assisting in informed feature selection.

### 7. `fs_lasso`
Fits a Lasso regression model using cross-validation to yield variable importance scores. It effectively performs feature selection by shrinking some coefficients to zero, thereby enhancing model interpretability.

### 8. `fs_mars`
Trains a Multivariate Adaptive Regression Splines (MARS) model to capture complex, non-linear relationships among features. It evaluates model performance using RMSE for numeric responses or accuracy for categorical outcomes.

### 9. `fs_pca`
Executes Principal Component Analysis to transform and reduce the dimensionality of a dataset. It produces principal component loadings, scores, and a measure of variance explained, along with optional visualization to help interpret the results.

### 10. `fs_randomforest`
Applies the random forest algorithm for either classification or regression tasks. In addition to training a robust model, it returns predictions and performance metrics (accuracy or RMSE), along with insights into feature importance.

### 11. `fs_recursivefeature`
Uses Recursive Feature Elimination (RFE) to iteratively remove the least important features, ultimately determining an optimal subset for model training. It returns the final set of features along with variable importance scores and resampling results.

### 12. `fs_stepwise`
Conducts stepwise regression with nested cross-validation, progressively adding or removing features based on their contribution to model performance. It provides the best tuning parameters, a summary of the final model, and variable importance rankings.

### 13. `fs_svd`
Performs Singular Value Decomposition (SVD) on a numeric matrix, breaking it down into its singular values and corresponding vectors. This technique is useful for data compression, noise reduction, and identifying latent data structures.

### 14. `fs_svm`
Trains a Support Vector Machine (SVM) model for classification or regression. With options for feature selection and class imbalance handling, it returns the trained model, predictions, and performance metrics (accuracy, R², MSE, or MAE).

### 15. `fs_variance`
Applies variance thresholding to a numeric dataset by removing features with variability below a user-defined threshold. This preprocessing step helps in eliminating features that contribute little to model performance.

---

Feel free to explore these functions to enhance your feature selection and dimensionality reduction workflows in R!
