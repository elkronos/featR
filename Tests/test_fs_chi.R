###############################################################################
# Testing Infrastructure - Chi-Squared (for modular fs_chi)
###############################################################################

library(testthat)

test_fs_chi <- function() {
  cat("Running UAT for fs_chi...\n")
  
  # -------------------------
  # Test 0: Sanity: fs_chi exists
  # -------------------------
  test_that("fs_chi is available", {
    expect_true(exists("fs_chi"))
    expect_true(is.function(fs_chi))
  })
  
  # -------------------------
  # Test 1: Input validation
  # -------------------------
  test_that("errors if data is not a data frame", {
    expect_error(fs_chi("not a data frame", "target"))
  })
  
  test_that("errors if target column missing", {
    df <- data.frame(A = factor(c("a", "b")), B = factor(c("x", "y")))
    expect_error(fs_chi(df, "target"))
  })
  
  test_that("errors if target has < 2 non-NA levels", {
    df <- data.frame(target = factor(c("Yes", NA, NA)))
    expect_error(fs_chi(df, "target"))
  })
  
  # -------------------------
  # Test 2: No categorical features
  # -------------------------
  test_that("returns empty results when no factor features", {
    df <- data.frame(target = factor(c("Yes", "No")), X = 1:2, Y = c(3.1, 4.2))
    res <- fs_chi(df, "target")
    expect_named(res, c("results", "significant_features"))
    expect_equal(nrow(res$results), 0)
    expect_length(res$significant_features, 0)
  })
  
  # -------------------------
  # Test 3: Basic 2x2 with adequate counts (continuity correction path)
  # -------------------------
  test_that("applies continuity correction by default for 2x2 when expected >= 5", {
    set.seed(1)
    target  <- factor(rep(c("Yes", "No"), each = 100))
    feat    <- factor(rep(c("A", "B"), times = 100))
    df <- data.frame(target = target, feat = feat)
    
    out <- fs_chi(df, "target", continuity_correction = NULL, p_adjust_method = "none")
    expect_s3_class(out$results, "data.frame")
    expect_true("feat" %in% out$results$feature)
    row <- out$results[out$results$feature == "feat", ]
    expect_equal(nrow(row), 1)
    # In a balanced 2x2, correction should be applied when continuity_correction = NULL
    expect_true(isTRUE(row$correction_applied))
    expect_equal(row$method, "asymptotic")
    expect_true(is.finite(row$p_value))
  })
  
  test_that("can disable continuity correction for 2x2", {
    set.seed(1)
    target  <- factor(rep(c("Yes", "No"), each = 100))
    feat    <- factor(rep(c("A", "B"), times = 100))
    df <- data.frame(target = target, feat = feat)
    
    out <- fs_chi(df, "target", continuity_correction = FALSE, p_adjust_method = "none")
    row <- out$results[out$results$feature == "feat", ]
    expect_false(isTRUE(row$correction_applied))
  })
  
  # -------------------------
  # Test 4: Low expected counts -> simulation path (not necessarily significant)
  # -------------------------
  test_that("falls back to simulation when expected counts are low", {
    set.seed(123)
    target <- factor(rep(c("Yes", "No"), each = 250))
    feature_low <- factor(c(rep("A", 495), rep("B", 5)))
    df_low <- data.frame(feature_low = feature_low, target = target)
    
    out <- fs_chi(df_low, "target", p_adjust_method = "none")
    row <- out$results[out$results$feature == "feature_low", ]
    expect_equal(nrow(row), 1)
    expect_equal(row$method, "simulation")
    expect_true(is.finite(row$p_value) || is.na(row$p_value))
  })
  
  # -------------------------
  # Test 5: Detect significant association (clear signal)
  # -------------------------
  test_that("detects significant association when present", {
    set.seed(42)
    # Make feature highly associated with target
    target <- factor(c(rep("Yes", 300), rep("No", 200)))
    feat <- factor(c(rep("A", 290), rep("B", 10),   # mostly A in Yes
                     rep("A", 50),  rep("B", 150))) # mostly B in No
    df <- data.frame(target = target, feat = feat)
    
    out <- fs_chi(df, "target", p_adjust_method = "bonferroni")
    row <- out$results[out$results$feature == "feat", ]
    expect_true(row$significant)
    expect_true("feat" %in% out$significant_features)
  })
  
  # -------------------------
  # Test 6: p.adjust methods (bonferroni vs none)
  # -------------------------
  test_that("respects p.adjust method", {
    set.seed(999)
    target <- factor(rep(c("Yes", "No"), each = 200))
    f1 <- factor(sample(c("A","B"), 400, TRUE))
    f2 <- factor(sample(c("X","Y"), 400, TRUE))
    df <- data.frame(target, f1, f2)
    
    out_bonf <- fs_chi(df, "target", p_adjust_method = "bonferroni")
    out_none <- fs_chi(df, "target", p_adjust_method = "none")
    
    # Join rows by feature
    rb <- out_bonf$results[, c("feature", "adj_p_value")]
    rn <- out_none$results[, c("feature", "adj_p_value")]
    m <- merge(rb, rn, by = "feature", suffixes = c("_bonf", "_none"))
    expect_true(all(m$adj_p_value_bonf >= m$adj_p_value_none, na.rm = TRUE))
  })
  
  # -------------------------
  # Test 7: Character coercion to factor (including target)
  # -------------------------
  test_that("coerces character columns to factor, including target", {
    df <- data.frame(
      target = c("Y","Y","N","N"),
      f1 = c("A","A","B","B"),
      f2 = factor(c("X","Y","X","Y")),
      stringsAsFactors = FALSE
    )
    out <- fs_chi(df, "target", p_adjust_method = "none")
    expect_true(all(out$results$feature %in% c("f1","f2")))
  })
  
  # -------------------------
  # Test 8: Feature skipped when <2 levels after NA removal
  # -------------------------
  test_that("skips features with <2 levels after NA removal", {
    df <- data.frame(
      target = factor(c("Y","N","Y","N")),
      f1 = factor(c(NA, NA, NA, "only_one_non_na")),  # effectively <2 levels
      f2 = factor(c("A","A","B","B"))
    )
    out <- fs_chi(df, "target", p_adjust_method = "none")
    row1 <- out$results[out$results$feature == "f1", ]
    row2 <- out$results[out$results$feature == "f2", ]
    expect_true(is.na(row1$p_value))
    expect_true(is.finite(row2$p_value))
  })
  
  # -------------------------
  # Test 9 (optional): Parallel path smoke test
  # -------------------------
  test_that("parallel path (multisession) runs without error (smoke test)", {
    skip_on_cran()
    skip_on_ci()
    set.seed(321)
    target <- factor(rep(c("Yes","No"), each = 150))
    f1 <- factor(sample(c("A","B","C"), 300, TRUE))
    f2 <- factor(sample(c("X","Y"), 300, TRUE))
    df <- data.frame(target, f1, f2)
    
    expect_silent({
      out <- fs_chi(
        df, "target",
        parallel = TRUE,
        temp_multisession = TRUE,
        p_adjust_method = "none"
      )
      expect_equal(nrow(out$results), 2)
    })
  })
  
  cat("All UAT for fs_chi completed.\n")
}

# To run locally:
# test_fs_chi()
