library(dplyr)
library(purrr)
library(tidyr)
library(rlang)

.manual_kappa_from_table <- function(tab) {
  n <- sum(tab)
  if (n == 0) return(NA_real_)
  po <- sum(diag(tab)) / n
  rs <- rowSums(tab); cs <- colSums(tab)
  pe <- sum((rs / n) * (cs / n))
  den <- 1 - pe
  if (den == 0) return(if (po == 1) 1 else NA_real_)
  (po - pe) / den
}

.confusion_table <- function(a, b, levels) {
  a <- factor(a, levels = levels)
  b <- factor(b, levels = levels)
  xtabs(~ a + b, drop.unused.levels = FALSE)
}

# truth = ground truth labels
# pred  = rater/system labels to evaluate
compute_irr <- function(df, truth, pred, na_label = "NONE") {
  truth <- enquo(truth)
  pred  <- enquo(pred)

  dat <- df %>% transmute(truth = !!truth, pred = !!pred)

  prf_dat <- dat %>% filter(!(is.na(truth) & is.na(pred)))
  labels <- sort(unique(c(prf_dat$truth, prf_dat$pred)))
  labels <- labels[!is.na(labels)]

  per_label_prf <- map_dfr(labels, function(L) {
    tp <- sum(prf_dat$pred == L & prf_dat$truth == L, na.rm = TRUE)
    fp <- sum(prf_dat$pred == L & prf_dat$truth != L, na.rm = TRUE)
    fn <- sum(prf_dat$pred != L & prf_dat$truth == L, na.rm = TRUE)
    support <- sum(prf_dat$truth == L, na.rm = TRUE)
    precision <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
    recall    <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
    f1        <- if (is.na(precision) || is.na(recall) || (precision + recall) == 0) NA_real_ else 2 * precision * recall / (precision + recall)
    tibble(label = L, support, tp, fp, fn, precision, recall, f1)
  })

  kappad <- dat %>%
    mutate(
      truth2 = ifelse(is.na(truth), na_label, as.character(truth)),
      pred2  = ifelse(is.na(pred),  na_label, as.character(pred))
    )

  lvls <- sort(unique(c(kappad$truth2, kappad$pred2)))
  tab_all <- .confusion_table(kappad$truth2, kappad$pred2, lvls)  # rows=truth, cols=pred
  n_all <- sum(tab_all)
  acc_all <- if (n_all > 0) sum(diag(tab_all)) / n_all else NA_real_
  kappa_all <- .manual_kappa_from_table(tab_all)

  per_label_kappa <- map_dfr(labels, function(L) {
    a_bin <- factor(ifelse(dat$truth == L, "POS", "NEG"), levels = c("NEG","POS"))
    b_bin <- factor(ifelse(dat$pred  == L, "POS", "NEG"), levels = c("NEG","POS"))
    tab   <- .confusion_table(a_bin, b_bin, levels = c("NEG","POS"))
    n     <- sum(tab)
    accuracy <- if (n > 0) sum(diag(tab)) / n else NA_real_
    kappa <- .manual_kappa_from_table(tab)
    tp <- unname(tab["POS","POS"])
    tn <- unname(tab["NEG","NEG"])
    fp <- unname(tab["NEG","POS"]) # pred POS, truth NEG => column POS, row NEG
    fn <- unname(tab["POS","NEG"]) # pred NEG, truth POS => column NEG, row POS
    tibble(label = L, tp = tp, tn = tn, fp = fp, fn = fn, accuracy = accuracy, kappa = kappa)
  })

  per_label <- per_label_kappa %>%
    left_join(per_label_prf %>% select(label, support, precision, recall, f1), by = "label") %>%
    select(label, support, tp, tn, fp, fn, accuracy, precision, recall, f1, kappa)

  tp_micro <- sum(per_label_prf$tp, na.rm = TRUE)
  fp_micro <- sum(per_label_prf$fp, na.rm = TRUE)
  fn_micro <- sum(per_label_prf$fn, na.rm = TRUE)

  precision_micro <- if ((tp_micro + fp_micro) == 0) NA_real_ else tp_micro / (tp_micro + fp_micro)
  recall_micro    <- if ((tp_micro + fn_micro) == 0) NA_real_ else tp_micro / (tp_micro + fn_micro)
  f1_micro        <- if (is.na(precision_micro) || is.na(recall_micro) || (precision_micro + recall_micro) == 0) NA_real_ else 2 * precision_micro * recall_micro / (precision_micro + recall_micro)

  macro_precision <- mean(per_label_prf$precision, na.rm = TRUE)
  macro_recall    <- mean(per_label_prf$recall,    na.rm = TRUE)
  macro_f1        <- mean(per_label_prf$f1,        na.rm = TRUE)

  total_support <- sum(per_label_prf$support, na.rm = TRUE)
  w <- if (total_support > 0) per_label_prf$support / total_support else rep(NA_real_, nrow(per_label_prf))
  wmacro_precision <- if (all(is.na(w))) NA_real_ else weighted.mean(per_label_prf$precision, w, na.rm = TRUE)
  wmacro_recall    <- if (all(is.na(w))) NA_real_ else weighted.mean(per_label_prf$recall,    w, na.rm = TRUE)
  wmacro_f1        <- if (all(is.na(w))) NA_real_ else weighted.mean(per_label_prf$f1,        w, na.rm = TRUE)

  overall <- tibble(
    metric = c("precision_micro","recall_micro","f1_micro",
               "precision_macro","recall_macro","f1_macro",
               "precision_wmacro","recall_wmacro","f1_wmacro",
               "kappa_overall_manual","accuracy_overall"),
    value  = c(precision_micro, recall_micro, f1_micro,
               macro_precision, macro_recall, macro_f1,
               wmacro_precision, wmacro_recall, wmacro_f1,
               kappa_all, acc_all)
  )

  list(
    per_label_prf   = per_label_prf,
    per_label_kappa = per_label_kappa,
    per_label       = per_label,
    overall         = overall,
    confusion       = as_tibble(as.data.frame(tab_all), .name_repair = "minimal")
  )
}

# Example usage:
# out <- compute_irr(merged, ground_truth, teacherMove)
# out$per_label
# out$overall
