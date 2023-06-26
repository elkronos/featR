# feature_selection
 This repo contains a series of functions aimed at helping to select features and reduce dimensionality. Each script contains a function with detailed roxygen outlining the functions purpose and providing examples you can reproduce in R. 

## Summary and considerations
| Function             | Data Processing Considerations                          | Assumptions                                        | Outcome Type                       | Uses                                                                                                                 |
|----------------------|---------------------------------------------------------|----------------------------------------------------|------------------------------------|----------------------------------------------------------------------------------------------------------------------|
| `fs_bayes`           | Feature selection for Bayesian models                    | Bayesian modeling                                  | Best model, WAIC, MAE, RMSE         | Identifying relevant predictors, evaluating their performance                                                       |
| `fs_boruta`          | Feature selection using the Boruta algorithm             | High-dimensional datasets                          | Selected features                  | Automating feature selection process                                                                                  |
| `fs_chi`             | Feature selection using chi-square test                  | Categorical features, target variable significance | Significant categorical features    | Reducing dataset dimensionality, improving predictive model performance                                               |
| `fs_correlation`     | Feature selection based on feature correlation           | Correlation threshold                              | Selected variables, correlation matrix | Identifying and selecting highly correlated features                                                                 |
| `fs_elastic`         | Elastic net regression with cross-validation             | Multicollinearity                                  | Coefficients, alpha, lambda         | Selecting best model, assessing predictor importance with multicollinearity                                            |
| `fs_infogain`        | Calculation of information gain for feature selection    | Predictive power                                   | Informative/relevant variables      | Prioritizing variables contributing the most to the predictive power of the model                                    |
| `fs_lasso`           | Lasso regression model and variable importance scores    | Variable importance                                | Trained model, variable importance  | Understanding predictor importance, feature selection                                                                |
| `fs_mars`            | MARS model training and evaluation                       | Regression/classification tasks, hyperparameters   | RMSE or accuracy, trained model     | Building and assessing MARS models                                                                                    |
| `fs_pca`             | Principal component analysis (PCA)                       | Dimensionality reduction, pattern analysis         | Principal component loadings, scores | Reducing dimensionality, analyzing relationships among variables                                                     |
| `fs_randomforest`    | Random forest algorithm for classification/regression    | Data preprocessing, feature selection              | Trained model, predictions, accuracy | Applying random forest algorithm for classification/regression tasks                                                 |
| `fs_recursivefeature`| Recursive Feature Elimination (RFE) using Random Forest  | Variable importance, target variable prediction     | Variable importance scores          | Identifying important variables for predicting a specific outcome or target variable                                 |
| `fs_stepwise`        | Stepwise regression with cross-validation                | Feature selection, model evaluation                | Cross-validation results, coefficients | Feature selection, model evaluation using cross-validation                                                           |
| `fs_svd`             | Singular Value Decomposition (SVD)                       | Matrix analysis                                   | Reduced matrix                      | Analyzing matrix structure, reducing dimensionality, solving linear systems of equations                            |
| `svm_model`          | Support Vector Machine (SVM) model training              | Classification/regression tasks, hyperparameters   | Trained model, predictions          | Training SVM models, classification/regression tasks                                                                 |
| `fs_variance`        | Variance thresholding for feature selection              | Variance threshold                                 | Thresholded data                    | Preprocessing data, focusing on informative features in high-dimensional datasets                                   |

## Descriptions
Below is a more detailed description of each function.

### 1. `fs_bayes` 
Performs feature selection for Bayesian models using the brms package in R. It generates all possible combinations of predictor columns, fits a Bayesian model for each combination, and selects the best model based on the lowest WAIC value. The function also calculates model quality metrics such as MAE and RMSE. This function is useful when you want to identify the most relevant predictors for your Bayesian model and evaluate their performance.

### 2. `fs_boruta`
Used to select features using the Boruta algorithm. It removes the target variable from the data and then runs the Boruta algorithm to identify relevant features. It also allows for limiting the number of selected features and removing highly correlated variables. It is especially useful when dealing with high-dimensional datasets where you want to automate the feature selection process.

### 3. `fs_chi`
Performs feature selection on a data frame by applying the chi-square test to identify categorical features that are statistically significant with respect to the target variable. It returns a list containing the names of the significant categorical features and their corresponding p-values. This function can be used when you want to determine which categorical features have a significant association with the target variable in order to reduce the dimensionality of the dataset or improve the performance of a predictive model.

### 4. `fs_correlation`
Selects features based on their correlation with other features. It calculates the correlation matrix using either Pearson or Spearman correlation method, finds the variables that have a correlation above a specified threshold, and returns a list containing the correlation matrix and the selected variables. It can be used when you want to identify and select features that are highly correlated with each other in a dataset.

### 5. `fs_elastic` 
Performs elastic net regression with cross-validation using the glmnet package. It takes a data frame with response and predictor variables, a formula specifying the model, a numeric vector for the mixing parameter (alpha), a trainControl object for cross-validation settings, and a logical value indicating whether to perform principal component analysis (PCA) on the predictors. It returns a list with coefficients, alpha value, and lambda value for the best model. This function is useful when you want to perform elastic net regression with cross-validation to select the best model and assess the importance of predictors in the presence of multicollinearity.

### 6. `fs_infogain`
Calculates the information gain for each variable in a given data frame, which measures the reduction in entropy or uncertainty when the data is split based on that variable. It is used in feature selection tasks to identify the variables that are most informative or relevant for predicting the target variable. The function also handles date columns by extracting year, month, and day features. By calculating the information gain for each variable, you can prioritize and select the variables that contribute the most to the predictive power of your model.

### 7. `fs_lasso` 
Trains and tests a Lasso regression model on a dataset. It fits the model using cross-validation and calculates variable importance scores. It is useful when you want to understand the importance of different predictor variables in predicting a response variable and want to perform feature selection.

### 8. `fs_mars` 
Used to train and evaluate a MARS (Multivariate Adaptive Regression Splines) model on a given dataset. It splits the dataset into training and test sets, performs grid search over a predefined set of hyperparameters, trains the MARS model on the training set using cross-validation, and then evaluates the model by making predictions on the test set. It calculates the root mean squared error (RMSE) for numeric response variables or accuracy for categorical response variables and returns the evaluation metric along with the trained model. This function can be used when you want to build and assess a MARS model for regression or classification tasks.

### 9. `fs_pca` 
Performs principal component analysis (PCA) on a given dataset. It calculates the principal component loadings, scores, and the proportion of variance explained by each principal component. It also generates a data table with the principal component scores and labels. This function can be used when you want to reduce the dimensionality of your data and analyze the underlying patterns or relationships among variables. The function also provides a PCA plot to visualize the data in the reduced-dimensional space.

### 10. `fs_randomforest`
Used to apply the random forest algorithm for classification or regression tasks. It takes in a data frame with features and a target variable, and performs various operations such as data preprocessing, feature selection, and parallel computing. It returns a list containing the trained random forest model, predictions for the target variable, and the accuracy (classification accuracy or RMSE) of the model.

### 11. `fs_recrusivefeature` 
Loads a specified dataset, splits it into training and testing sets, and performs Recursive Feature Elimination (RFE) using the Random Forest algorithm on the training set. It returns a data frame containing the variable importance scores computed from the RFE analysis. This function can be used when you want to identify the most important variables in a dataset for predicting a specific outcome or target variable.

### 12. `fs_stepwise` 
Performs stepwise regression with cross-validation. It takes a data frame, the name of the dependent variable, and the type of stepwise selection (backward, forward, or both) as inputs. It returns a list containing the results of the cross-validation, the best tuning parameter value, a summary of the final model, and the coefficients of the final model. This function is useful when you want to perform feature selection and evaluate the performance of the regression model using cross-validation.

### 13. `fs_svd` 
Performs Singular Value Decomposition (SVD) on a matrix. It takes the input matrix, an optional argument to scale the matrix, and an optional argument to specify the number of singular values to keep. One might use this function to analyze the structure and properties of a matrix, such as reducing dimensionality, identifying dominant features, or solving linear systems of equations.

### 14. `svm_model` 
Trains an SVM (Support Vector Machine) model using cross-validation and grid search. It can be used for classification or regression tasks. It takes a data frame, the target variable, the type of task, the number of cross-validation folds, and an optional tuning grid as inputs. It returns a list containing the trained model, predictions, and either the accuracy (for classification) or the R-squared score (for regression).

### 15. `fs_variance`
Takes a numeric matrix or data frame as input and applies variance thresholding to remove features (columns) whose variances are below a specified threshold. It returns a numeric matrix with the thresholded data. One might use this function to preprocess data before further analysis or modeling, especially when dealing with high-dimensional datasets and wanting to focus on the most informative features.


# Contact
- email: napoleonic_bores@proton.me
- discord: elkronos
