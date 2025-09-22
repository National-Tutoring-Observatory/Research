library(dplyr)
library(purrr)
library(tidyr)
library(rlang)
library(tibble)

# Helper: for each truth index, find the nearest prediction within tol (if any)
.nearest_pred_within_tol <- function(truth_idx, pred_idx, tol) {
  if (!length(truth_idx) || !length(pred_idx)) {
    return(tibble(truth = integer(), pred = integer()))
  }
  mapply(function(ti) {
    in_win <- pred_idx[abs(pred_idx - ti) <= tol]
    if (!length(in_win)) return(c(truth = ti, pred = NA_integer_))
    # choose nearest (break ties by smallest index)
    diffs <- abs(in_win - ti)
    pick <- in_win[order(diffs, in_win)][1]
    c(truth = ti, pred = pick)
  }, truth_idx) |>
    t() |>
    as_tibble() |>
    mutate(truth = as.integer(truth), pred = as.integer(pred))
}

# Tolerance-based IRR where tolerance only affects matching (TP/FN via windows), not negatives
# TP: predicted utterances that fall within tol of ANY ground-truth utterance (for that label)
# FP: predicted utterances that do NOT fall within tol of ANY ground-truth utterance
# FN: ground-truth utterances that have NO prediction within tol
compute_irr_wTol <- function(df, truth, pred, session, utt, tol = 1) {
  truth   <- enquo(truth)
  pred    <- enquo(pred)
  session <- enquo(session)
  utt     <- enquo(utt)

  dat <- df %>%
    transmute(
      session = as.character(!!session),
      utt     = as.integer(!!utt),
      truth   = as.character(!!truth),
      pred    = as.character(!!pred)
    ) %>%
    filter(!(is.na(truth) & is.na(pred)))

  labels <- sort(unique(c(dat$truth, dat$pred)))
  labels <- labels[!is.na(labels)]

  per_label_res <- map(labels, function(L) {
    by_sess <- dat %>%
      mutate(tpos = truth == L, ppos = pred == L) %>%
      group_by(session) %>%
      summarise(
        truth_idx = list(sort(unique(utt[tpos]))),
        pred_idx  = list(sort(unique(utt[ppos]))),
        .groups = "drop"
      )

    # TP: count predictions that are within tol of ANY truth (window membership)
    tp_sess <- by_sess %>%
      mutate(
        # union of windows around each truth index
        win_set = map(truth_idx, function(ti) {
          if (!length(ti)) return(integer())
          # build compact set of all indices in windows; do not assume contiguous utt ids
          # we only need membership test against pred_idx so compute per-pred check later
          ti
        }),
        tp = pmap_int(list(win_set, truth_idx, pred_idx), function(win_set, ti, pi) {
          if (!length(pi) || !length(ti)) return(0L)
          # TP: any pred within tol of any truth
          sum(sapply(pi, function(p) any(abs(ti - p) <= tol)))
        }),
        fp = pmap_int(list(truth_idx, pred_idx), function(ti, pi) {
          if (!length(pi)) return(0L)
          sum(sapply(pi, function(p) !any(abs(ti - p) <= tol)))
        }),
        fn = pmap_int(list(truth_idx, pred_idx), function(ti, pi) {
          if (!length(ti)) return(0L)
          sum(sapply(ti, function(ti1) !any(abs(pi - ti1) <= tol)))
        })
      )

    tp <- sum(tp_sess$tp)
    fp <- sum(tp_sess$fp)
    fn <- sum(tp_sess$fn)

    # Support = number of truth instances
    support <- sum(lengths(by_sess$truth_idx))

    precision <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
    recall    <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
    f1        <- if (is.na(precision) || is.na(recall) || (precision + recall) == 0) NA_real_ else 2 * precision * recall / (precision + recall)

    # Pairs (nearest pred for each truth, within tol if exists) for auditing
    pairs <- by_sess %>%
      mutate(pairs = map2(truth_idx, pred_idx, ~.nearest_pred_within_tol(.x, .y, tol))) %>%
      select(session, pairs) %>%
      unnest(pairs) %>%
      mutate(label = L, matched = !is.na(pred))

    list(
      per_label = tibble(label = L, support = support, tp = tp, fp = fp, fn = fn,
                         precision = precision, recall = recall, f1 = f1),
      pairs = pairs
    )
  })

  per_label_prf <- bind_rows(map(per_label_res, "per_label"))
  pairs_all     <- bind_rows(map(per_label_res, "pairs"))

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
               "precision_wmacro","recall_wmacro","f1_wmacro"),
    value  = c(precision_micro, recall_micro, f1_micro,
               macro_precision, macro_recall, macro_f1,
               wmacro_precision, wmacro_recall, wmacro_f1)
  )

  list(
    per_label_prf = per_label_prf,
    overall       = overall,
    pairs         = pairs_all
  )
}

# Example:
# out_tol <- compute_irr_wTol(merged, ground_truth, teacherMove, session = Session_ID, utt = Utterance_ID, tol = 1)
# out_tol$per_label_prf
# out_tol$overall
# out_tol$pairs
