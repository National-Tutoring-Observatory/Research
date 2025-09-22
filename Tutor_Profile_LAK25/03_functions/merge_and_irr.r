suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(irr)
})

# ---- helpers ----
.normalize_text <- function(x) {
  x %>%
    tolower() %>%
    str_replace_all("[[:space:]]+", " ") %>%
    str_replace_all("[[:punct:]]+", "") %>%
    str_trim()
}

.norm_name <- function(x) tolower(gsub("[^a-z0-9]", "", x))

.resolve_col <- function(df, desired) {
  nms <- names(df)
  norm_map <- setNames(.norm_name(nms), nms)
  desired_norm <- .norm_name(desired)
  hit <- names(norm_map)[norm_map == desired_norm]
  if (length(hit) >= 1) return(hit[1])

  # fallback: try to detect teacherMove variants
  tm_idx <- gsub(".*?(\\d+)$", "\\1", desired)
  has_idx <- grepl("\\d+$", desired)
  if (has_idx) {
    patt <- paste0("^teachermove[._-]?", tm_idx, "$")
    hit2 <- nms[grepl(patt, nms, ignore.case = TRUE)]
    if (length(hit2) >= 1) return(hit2[1])
  }
  # any teacherMove* column
  hit3 <- nms[grepl("^teachermove[._-]?\\d+$", nms, ignore.case = TRUE)]
  if (length(hit3) >= 1) return(hit3[1])

  stop(sprintf("Could not find column like '%s'. Available columns: %s",
               desired, paste(nms, collapse = ", ")))
}

# ---- align within a session (content + order aware) ----
.align_session <- function(csv_sess, gt_sess) {
  csv_sess <- csv_sess %>% mutate(row_csv = row_number(),
                                  raw = Content,
                                  norm = .normalize_text(Content))
  gt_sess  <- gt_sess  %>% mutate(row_gt  = row_number(),
                                  raw = Content,
                                  norm = .normalize_text(Content))

  m1 <- inner_join(csv_sess %>% select(row_csv, raw),
                   gt_sess  %>% select(row_gt,  raw),
                   by = "raw", multiple = "all") %>%
    select(row_csv, row_gt)

  used_csv <- unique(m1$row_csv); used_gt <- unique(m1$row_gt)

  rem_csv <- csv_sess %>% filter(!(row_csv %in% used_csv))
  rem_gt  <- gt_sess  %>% filter(!(row_gt  %in% used_gt))
  m2 <- inner_join(rem_csv %>% select(row_csv, norm),
                   rem_gt  %>% select(row_gt,  norm),
                   by = "norm", multiple = "all") %>%
    select(row_csv, row_gt)

  used_csv <- c(used_csv, m2$row_csv); used_gt <- c(used_gt, m2$row_gt)

  rem_csv <- csv_sess %>% filter(!(row_csv %in% used_csv))
  rem_gt  <- gt_sess  %>% filter(!(row_gt  %in% used_gt))
  m3 <- tibble(row_csv = integer(), row_gt = integer())
  if (nrow(rem_csv) > 0 && nrow(rem_gt) > 0 && nrow(rem_csv) == nrow(rem_gt)) {
    m3 <- tibble(row_csv = rem_csv$row_csv, row_gt = rem_gt$row_gt)
  }

  bind_rows(m1, m2, m3) %>% distinct()
}

# ---- main ----
merge_and_irr <- function(csv_path,
                          gt_df,
                          move_col = "teacherMove.0",
                          restrict_role = "TEACHER",
                          out_merged_csv = NULL) {

  run <- readr::read_csv(
    csv_path,
    col_types = cols(.default = col_character()),
    na = c("", "NA", "NaN", "null")
  ) %>%
    rename(
      Utterance_ID = any_of(c("_id","X_id")),
      Session_ID_run = sessionId,
      Role = role,
      Content = content
    ) %>%
    mutate(
      Session_ID_run = str_trim(Session_ID_run),
      Role           = str_trim(Role),
      Content        = str_squish(Content)
    )

  move_col_resolved <- .resolve_col(run, move_col)
  run <- run %>% mutate(pred_move = .data[[move_col_resolved]])

  if (!is.null(restrict_role)) run <- run %>% filter(Role == restrict_role)

  gt <- gt_df %>%
    select(Session_ID, Role, Content, ground_truth) %>%
    mutate(
      Session_ID   = str_trim(as.character(Session_ID)),
      Role         = str_trim(as.character(Role)),
      Content      = str_squish(as.character(Content)),
      ground_truth = as.character(ground_truth)
    )
  if (!is.null(restrict_role)) gt <- gt %>% filter(Role == restrict_role)

  overlap <- length(intersect(unique(run$Session_ID_run), unique(gt$Session_ID)))

  if (overlap > 0) {
    merged <- run %>%
      group_by(Session_ID_run) %>%
      group_modify(~{
        sess_gt <- gt %>% filter(Session_ID == .y$Session_ID_run)
        if (nrow(sess_gt) == 0) return(.x %>% mutate(ground_truth = NA_character_))
        map_idx <- .align_session(.x %>% select(Content), sess_gt %>% select(Content))
        .x %>%
          mutate(row_csv = row_number()) %>%
          left_join(map_idx, by = "row_csv") %>%
          left_join(
            sess_gt %>% mutate(row_gt = row_number()) %>% select(row_gt, ground_truth),
            by = "row_gt"
          ) %>%
          select(-row_csv, -row_gt)
      }) %>%
      ungroup()
  } else {
    merged <- run %>%
      left_join(gt %>% select(Role, Content, ground_truth),
                by = c("Role","Content"))

    na_idx <- which(is.na(merged$ground_truth))
    if (length(na_idx)) {
      run_norm <- run %>%
        transmute(.rid = row_number(), Role, nContent = .normalize_text(Content))
      gt_norm <- gt %>%
        transmute(Role, nContent = .normalize_text(Content), ground_truth_norm = ground_truth)
      fill_tbl <- run_norm %>% left_join(gt_norm, by = c("Role","nContent"))
      merged$ground_truth[na_idx] <- fill_tbl$ground_truth_norm[na_idx]
    }
  }

  eval_df <- merged %>%
    filter(!is.na(pred_move), pred_move != "", !is.na(ground_truth), ground_truth != "") %>%
    transmute(truth = str_trim(ground_truth), pred = str_trim(pred_move))

  if (nrow(eval_df) >= 2) {
    classes  <- sort(unique(c(eval_df$truth, eval_df$pred)))
    conf_mat <- table(factor(eval_df$truth, levels = classes),
                      factor(eval_df$pred,  levels = classes))

    overall_kappa <- tryCatch(
      irr::kappa2(eval_df[, c("truth","pred")], weight = "unweighted")$value,
      error = function(e) NA_real_
    )

    per_class <- map_dfr(classes, function(cls) {
      tp <- sum(eval_df$truth == cls & eval_df$pred == cls)
      fp <- sum(eval_df$truth != cls & eval_df$pred == cls)
      fn <- sum(eval_df$truth == cls & eval_df$pred != cls)
      precision <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
      recall    <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
      f1        <- if (is.na(precision) || is.na(recall) || (precision + recall) == 0) NA_real_ else
        2 * precision * recall / (precision + recall)

      bin_truth <- factor(ifelse(eval_df$truth == cls, "pos", "neg"), levels = c("neg","pos"))
      bin_pred  <- factor(ifelse(eval_df$pred  == cls, "pos", "neg"), levels = c("neg","pos"))
      kappa_bin <- tryCatch(irr::kappa2(data.frame(bin_truth, bin_pred), weight = "unweighted")$value,
                            error = function(e) NA_real_)

      tibble(code = cls, kappa = kappa_bin, precision = precision, recall = recall, F1 = f1)
    })

    tp_vec <- diag(conf_mat)
    fp_vec <- colSums(conf_mat) - tp_vec
    fn_vec <- rowSums(conf_mat) - tp_vec

    micro_precision <- if ((sum(tp_vec) + sum(fp_vec)) == 0) NA_real_ else sum(tp_vec) / (sum(tp_vec) + sum(fp_vec))
    micro_recall    <- if ((sum(tp_vec) + sum(fn_vec)) == 0) NA_real_ else sum(tp_vec) / (sum(tp_vec) + sum(fn_vec))
    micro_f1        <- if (is.na(micro_precision) || is.na(micro_recall) || (micro_precision + micro_recall) == 0) NA_real_ else
      2 * micro_precision * micro_recall / (micro_precision + micro_recall)

    macro_precision <- mean(per_class$precision, na.rm = TRUE)
    macro_recall    <- mean(per_class$recall,    na.rm = TRUE)
    macro_f1        <- mean(per_class$F1,        na.rm = TRUE)

    overall_tbl <- tibble(
      scope = "overall",
      kappa = overall_kappa,
      precision = micro_precision,
      recall = micro_recall,
      F1 = micro_f1,
      macro_precision = macro_precision,
      macro_recall = macro_recall,
      macro_F1 = macro_f1
    )

    irr_table <- bind_rows(
      overall_tbl,
      per_class %>% mutate(scope = paste0("code:", code)) %>% select(scope, kappa, precision, recall, F1)
    )
  } else {
    conf_mat  <- matrix(0, nrow = 0, ncol = 0)
    irr_table <- tibble(scope = "overall", kappa = NA_real_, precision = NA_real_, recall = NA_real_,
                        F1 = NA_real_, macro_precision = NA_real_, macro_recall = NA_real_, macro_F1 = NA_real_)
  }

  if (!is.null(out_merged_csv)) readr::write_csv(merged, out_merged_csv, na = "")

  list(
    merged = merged,
    eval_rows = eval_df,
    confusion_matrix = if (exists("conf_mat")) conf_mat else matrix(0,0,0),
    irr_table = irr_table,
    diagnostics = list(
      move_col_resolved = move_col_resolved,
      overlap_sessions = overlap,
      n_run_rows = nrow(run),
      n_gt_rows = nrow(gt),
      matched_rows = sum(!is.na(merged$ground_truth)),
      match_rate = mean(!is.na(merged$ground_truth))
    )
  )
}
