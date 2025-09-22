library(dplyr)
library(openxlsx)

export_fp_fn_context_xlsx <- function(df, label, path = "fp_fn_context.xlsx", context = 4) {
  df2 <- df %>%
    mutate(
      utter_num = suppressWarnings(as.numeric(Utterance_ID))
    ) %>%
    group_by(Session_ID) %>%
    arrange(utter_num, .by_group = TRUE) %>%
    mutate(seq_idx = dplyr::row_number()) %>%
    ungroup()

  build_context <- function(sid, idx) {
    df2 %>%
      filter(Session_ID == sid, dplyr::between(seq_idx, idx - context, idx + context)) %>%
      transmute(
        Session_ID,
        Utterance_ID,
        seq_idx,
        offset   = seq_idx - idx,
        is_focal = seq_idx == idx,
        Role,
        Content,
        ground_truth,
        teacherMove
      ) %>%
      arrange(seq_idx)
  }

  fp_focal <- df2 %>%
    filter(teacherMove == label & (is.na(ground_truth) | ground_truth != label)) %>%
    select(Session_ID, Utterance_ID, seq_idx, Role, Content, ground_truth, teacherMove)

  fn_focal <- df2 %>%
    filter(ground_truth == label & (is.na(teacherMove) | teacherMove != label)) %>%
    select(Session_ID, Utterance_ID, seq_idx, Role, Content, ground_truth, teacherMove)

  wb <- createWorkbook()
  addWorksheet(wb, "INDEX")
  index_rows <- list()

  write_examples <- function(focal_tbl, type_prefix) {
    if (nrow(focal_tbl) == 0) return(invisible(NULL))
    for (i in seq_len(nrow(focal_tbl))) {
      sid <- focal_tbl$Session_ID[i]
      idx <- focal_tbl$seq_idx[i]
      uid <- focal_tbl$Utterance_ID[i]

      ctx <- build_context(sid, idx)

      sheet_name <- sprintf("%s_%03d", type_prefix, i)
      addWorksheet(wb, sheet_name)
      writeData(wb, sheet = sheet_name, x = ctx)

      foc_rows <- which(ctx$is_focal) + 1
      if (length(foc_rows)) {
        addStyle(
          wb, sheet = sheet_name,
          style = createStyle(fgFill = "#FFF2CC"),
          rows = foc_rows, cols = 1:ncol(ctx),
          gridExpand = TRUE, stack = TRUE
        )
      }
      freezePane(wb, sheet = sheet_name, firstRow = TRUE)
      setColWidths(wb, sheet = sheet_name, cols = 1:ncol(ctx), widths = "auto")

      index_rows[[length(index_rows) + 1]] <- data.frame(
        type = type_prefix,
        label = label,
        sheet = sheet_name,
        Session_ID = sid,
        Utterance_ID = uid,
        Role = focal_tbl$Role[i],
        Content = focal_tbl$Content[i],
        ground_truth = focal_tbl$ground_truth[i],
        teacherMove = focal_tbl$teacherMove[i],
        stringsAsFactors = FALSE
      )
    }
    invisible(NULL)
  }

  write_examples(fp_focal, "FP")
  write_examples(fn_focal, "FN")

  index_df <- if (length(index_rows)) dplyr::bind_rows(index_rows) else
    data.frame(type = character(), label = character(), sheet = character(),
               Session_ID = character(), Utterance_ID = character(),
               Role = character(), Content = character(),
               ground_truth = character(), teacherMove = character(),
               stringsAsFactors = FALSE)

  writeData(wb, sheet = "INDEX", x = index_df)
  freezePane(wb, sheet = "INDEX", firstRow = TRUE)
  setColWidths(wb, sheet = "INDEX", cols = 1:ncol(index_df), widths = "auto")

  saveWorkbook(wb, file = path, overwrite = TRUE)

  invisible(list(
    path = normalizePath(path, mustWork = FALSE),
    label = label,
    fp_n = nrow(fp_focal),
    fn_n = nrow(fn_focal)
  ))
}

# Example:
# res <- export_fp_fn_context_xlsx(merged, label = "PROVIDING_EXPLANATION",
#                                  path = "fp_fn_examples_PROVIDING_EXPLANATION.xlsx",
#                                  context = 4)
