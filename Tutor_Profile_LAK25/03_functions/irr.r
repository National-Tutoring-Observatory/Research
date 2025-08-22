irr_metrics <- function(var1, var2, primary = c("var1", "var2")) {
  # Dependencies
  if (!requireNamespace("irr", quietly = TRUE)) {
    stop("Package 'irr' is required. Install with install.packages('irr').")
  }

  primary <- match.arg(primary)

  # Coerce to factors with shared levels
  all_levels <- union(levels(factor(var1)), levels(factor(var2)))
  v1 <- factor(var1, levels = all_levels, exclude = NULL)
  v2 <- factor(var2, levels = all_levels, exclude = NULL)

  # Cohen's kappa (order doesn't matter)
  kappa <- irr::kappa2(data.frame(rater1 = v1, rater2 = v2))$value

  # Choose "truth" based on primary
  truth <- if (primary == "var1") v1 else v2
  pred  <- if (primary == "var1") v2 else v1

  # Confusion matrix (may include NA as a level)
  tbl <- table(truth, pred, useNA = "ifany")

  # Ensure square with all levels
  # (table already squares across shared levels; keep them explicit)
  lvl <- rownames(tbl)

  # Sums
  tp <- diag(tbl)
  names(tp) <- lvl
  row_tot <- rowSums(tbl)          # actual per class
  col_tot <- colSums(tbl)          # predicted per class
  total   <- sum(tbl)

  # Per-class metrics relative to primary ("truth")
  precision <- tp / col_tot[names(tp)]
  recall    <- tp / row_tot
  f1        <- (2 * precision * recall) / (precision + recall)

  # Replace NaN/Inf with NA
  clean <- function(x) { x[!is.finite(x)] <- NA_real_; x }
  precision <- clean(precision)
  recall    <- clean(recall)
  f1        <- clean(f1)

  # Support (actual instances per class)
  support <- as.integer(row_tot)

  # Overall metrics
  accuracy <- sum(tp, na.rm = TRUE) / total
  macro_precision <- mean(precision, na.rm = TRUE)
  macro_recall    <- mean(recall,    na.rm = TRUE)
  macro_f1        <- mean(f1,        na.rm = TRUE)

  # Per-class counts (useful for auditing)
  fp <- col_tot[names(tp)] - tp
  fn <- row_tot - tp
  tn <- total - (tp + fp + fn)

  per_class <- data.frame(
    Code = factor(names(tp), levels = lvl),
    Support = support,
    TP = as.integer(tp),
    FP = as.integer(fp),
    FN = as.integer(fn),
    TN = as.integer(tn),
    Precision = as.numeric(precision),
    Recall = as.numeric(recall),
    F1 = as.numeric(f1),
    row.names = NULL
  )

  # Sort by factor order
  per_class <- per_class[match(lvl, per_class$Code), , drop = FALSE]

  list(
    overall = list(
      kappa = unname(kappa),
      accuracy = unname(accuracy),
      macro_precision = unname(macro_precision),
      macro_recall = unname(macro_recall),
      macro_f1 = unname(macro_f1),
      primary_reference = primary
    ),
    per_class = per_class,
    confusion_matrix = as.matrix(tbl)
  )
}
