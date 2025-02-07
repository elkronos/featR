###############################################################################
# Testing Infrastructure - Chi-Squared
###############################################################################
test_fs_chi <- function() {
  cat("Running UAT for fs_chi...\n")
  
  # Test 1: Data frame input and target column validation
  test_that("fs_chi errors if data is not a data frame", {
    expect_error(fs_chi("not a data frame", "target"))
  })
  
  test_that("fs_chi errors if target column is missing", {
    df <- data.frame(A = factor(c("a", "b")), B = factor(c("x", "y")))
    expect_error(fs_chi(df, "target"))
  })
  
  # Test 2: No categorical features available
  test_that("fs_chi returns empty result if no categorical features", {
    df <- data.frame(target = factor(c("Yes", "No")), X = 1:2)
    res <- fs_chi(df, "target")
    expect_equal(length(res$significant_features), 0)
    expect_equal(length(res$p_values), 0)
  })
  
  # Test 3: Feature with low frequency counts (not significant)
  test_that("fs_chi handles low frequency counts without significant association", {
    set.seed(123)
    target <- factor(rep(c("Yes", "No"), each = 250))
    feature_low_freq <- factor(c(rep("A", 495), rep("B", 5)))
    df_low <- data.frame(feature_low_freq = feature_low_freq, target = target)
    
    expect_message(
      res_low <- fs_chi(df_low, "target"),
      regexp = "Low expected counts.*feature 'feature_low_freq'"
    )
    expect_length(res_low$significant_features, 0)
    expect_true("feature_low_freq" %in% names(res_low$p_values))
  })
  
  # Test 4: Feature with low frequency counts (significant association)
  test_that("fs_chi detects significant association with low frequency counts", {
    set.seed(123)
    target <- factor(c(rep("Yes", 495), rep("No", 5)))
    feature_low_freq <- factor(c(rep("A", 495), rep("B", 5)))
    df_low_sig <- data.frame(feature_low_freq = feature_low_freq, target = target)
    
    expect_message(
      res_low_sig <- fs_chi(df_low_sig, "target"),
      regexp = "Low expected counts.*feature 'feature_low_freq'"
    )
    expect_equal(length(res_low_sig$significant_features), 1)
    expect_true("feature_low_freq" %in% res_low_sig$significant_features)
  })
  
  cat("All UAT for fs_chi completed.\n")
}

# Run the unit tests
# test_fs_chi()
