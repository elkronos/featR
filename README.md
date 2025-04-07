# Feature Selection Repository

This repository contains a comprehensive collection of R functions designed to support feature selection, model training, and dimensionality reduction. Each script is thoroughly documented with its purpose, key assumptions, usage examples, and expected outputs to help you choose the most appropriate method for your data analysis and predictive modeling tasks.

The repository is organized into four main categories:

1. **Statistical & Filter Methods**
2. **Regularization Methods**
3. **Model-Based & Wrapper Methods**
4. **Dimensionality Reduction Techniques**

---

## 1. Statistical & Filter Methods

These functions evaluate features using statistical tests or filtering criteria to assess their relevance.

| Function Name        | Description                                                                                                     | Key Assumptions                                                  | Outputs                                               | Primary Applications                                | When to Use                                                                     |
|----------------------|-----------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------|-------------------------------------------------------|----------------------------------------------------|---------------------------------------------------------------------------------|
| **`fs_chi`**         | Performs chi-square tests to assess associations between categorical features and the target.                   | Categorical features and target exist; chi-square assumptions met.| Significant features with corresponding p-values.     | Selecting relevant categorical features.           | When you need to test the association of categorical features with the target.  |
| **`fs_correlation`** | Identifies and removes highly correlated numeric features based on a specified threshold.                       | All features are numeric; valid correlation method selected.      | Correlation matrix and names of selected features.    | Handling multicollinearity and reducing redundancy. | When you want to eliminate redundant features to improve model performance.     |
| **`fs_infogain`**    | Computes information gain for each feature relative to the target variable.                                     | Target exists; supports numeric, categorical, and date data.       | Data frame of features with information gain scores.  | Ranking features by relevance.                     | When you need to quantify how much each feature reduces uncertainty.            |
| **`fs_variance`**    | Applies variance thresholding to filter features based on their variability.                                    | Data is numeric; threshold is a non-negative numeric value.         | A numeric matrix with features filtered by variance.  | Removing low-information features.                 | When you want to eliminate features that show little variation.                 |

---

## 2. Regularization Methods

These functions integrate feature selection directly into the model training process using regularization techniques.

| Function Name     | Description                                                                                                     | Key Assumptions                                                  | Outputs                                               | Primary Applications                                | When to Use                                                                     |
|-------------------|-----------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------|-------------------------------------------------------|----------------------------------------------------|---------------------------------------------------------------------------------|
| **`fs_elastic`**  | Performs elastic net regression with cross-validation to combine Lasso and Ridge penalties.                     | Numeric predictors/response; proper hyperparameter tuning.        | Best model coefficients, optimal alpha & lambda, RMSE.| Handling multicollinearity and variable selection.  | When you need a robust model that balances variable selection and regularization.|
| **`fs_lasso`**    | Fits a Lasso regression model using cross-validation to promote sparsity in the predictor set.                   | Numeric predictors/response; proper preprocessing and tuning.       | Variable importance scores and a fitted Lasso model.  | Sparse model construction and interpretation.      | When you require a sparse, interpretable model for regression tasks.            |

---

## 3. Model-Based & Wrapper Methods

These functions combine feature selection with model training, often incorporating iterative or ensemble techniques.

| Function Name             | Description                                                                                                     | Key Assumptions                                                  | Outputs                                               | Primary Applications                                 | When to Use                                                                     |
|---------------------------|-----------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------|-------------------------------------------------------|-----------------------------------------------------|---------------------------------------------------------------------------------|
| **`fs_bayes`**            | Fits a Bayesian regression model using the `brms` package with LOO cross-validation for model selection.         | Preprocessed data; appropriate Bayesian priors and structure.      | Fitted model, predictions, residuals, MAE, RMSE.      | Incorporating uncertainty in predictor selection.   | When robust evaluation using Bayesian inference is needed despite higher computation cost. |
| **`fs_boruta`**           | Implements the Boruta algorithm to determine all relevant features with optional removal of correlated ones.      | Target exists; supports parallel processing and correlation filtering. | Selected features and full Boruta object.           | Automated feature selection in high-dimensional datasets. | When an out-of-the-box method for identifying relevant features is required.  |
| **`fs_randomforest`**     | Trains a Random Forest model with support for parallel processing and integrated feature selection.              | Data is preprocessed; applicable for both classification and regression. | Trained model, predictions, and performance metrics (accuracy or RMSE). | Building ensemble models with built-in feature ranking. | When you need a versatile model capable of handling non-linear relationships.   |
| **`fs_recursivefeature`** | Uses Recursive Feature Elimination (RFE) to iteratively select an optimal subset of features and optionally trains a final model. | Proper data splitting and control parameters; supports parallel processing. | Optimal feature subset, variable importance, and resampling results; final model if requested. | Systematic feature selection to improve interpretability and performance. | When an iterative elimination process is needed to fine-tune your feature set.  |
| **`fs_stepwise`**         | Performs stepwise regression using `stepAIC` to select features based on a specified direction (forward, backward, or both). | Data is a valid data frame; dependent variable exists; linear model assumptions.        | Final linear model and variable importance from coefficients. | Feature selection in linear regression.              | When a methodical, cross-validated approach to feature selection is required.  |
| **`fs_svm`**              | Trains an SVM model with options for feature selection, class imbalance handling, and hyperparameter tuning via cross-validation. | Data is valid; target exists; proper tuning and preprocessing applied. | Trained SVM model, predictions, and performance metrics (accuracy, R-squared, RMSE, MAE). | Building SVM models with integrated feature selection and imbalance handling. | When you need a flexible SVM approach for either classification or regression tasks. |
| **`fs_mars`**             | Trains and evaluates a Multivariate Adaptive Regression Splines (MARS) model to capture non-linear relationships.   | Required libraries are loaded; proper handling of missing data and tuning is in place. | Trained MARS model and performance metrics (RMSE or accuracy). | Modeling complex, non-linear interactions with automatic feature selection. | When an interpretable model that captures non-linearities is required.             |

---

## 4. Dimensionality Reduction Techniques

These functions reduce the dimensionality of data by transforming it into a lower-dimensional space.

| Function Name    | Description                                                                                                     | Key Assumptions                                                  | Outputs                                               | Primary Applications                                 | When to Use                                                                     |
|------------------|-----------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------|-------------------------------------------------------|-----------------------------------------------------|---------------------------------------------------------------------------------|
| **`fs_pca`**     | Executes Principal Component Analysis (PCA) to reduce dimensionality and visualize underlying data patterns.     | Data is numeric; PCA assumptions are met; number of components specified. | PCA loadings, scores, variance explained, and optional visual plots. | Simplifying data structure and visualization.       | When you need to reduce dimensionality while retaining variance information.   |
| **`fs_svd`**     | Performs Singular Value Decomposition (SVD) with options for scaling, truncation, and approximate computations for large matrices. | Input matrix is numeric and complete; scaling options specified.   | Singular values, left singular vectors, and right singular vectors.  | Dimensionality reduction and data decomposition.     | When efficient matrix factorization is needed, especially for large datasets.   |

---

## Repository Summary

The repository now includes the following functions:

- **Statistical & Filter Methods:**  
  `fs_chi`, `fs_correlation`, `fs_infogain`, `fs_variance`

- **Regularization Methods:**  
  `fs_elastic`, `fs_lasso`

- **Model-Based & Wrapper Methods:**  
  `fs_bayes`, `fs_boruta`, `fs_randomforest`, `fs_recursivefeature`, `fs_stepwise`, `fs_svm`, `fs_mars`

- **Dimensionality Reduction Techniques:**  
  `fs_pca`, `fs_svd`

For detailed usage instructions and examples, please refer to the individual R scripts in this repository.
