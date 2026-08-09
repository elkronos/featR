#' featR: A Unified Toolkit for Feature Selection
#'
#' Filter, wrapper, and embedded feature-selection methods behind a
#' consistent set of `fs_*()` functions, plus PCA/SVD dimensionality
#' reduction helpers. Heavy modeling engines (brms, caret, glmnet, ...) are
#' optional Suggests; each function checks for what it needs and tells you
#' what to install.
#'
#' @keywords internal
#' @importFrom data.table as.data.table is.data.table data.table copy set setDT setDF setnames rbindlist := .N .SD .I
"_PACKAGE"

# Symbols used in data.table non-standard evaluation.
utils::globalVariables(".")
