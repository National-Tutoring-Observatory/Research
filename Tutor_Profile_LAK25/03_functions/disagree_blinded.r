suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(openxlsx)
})

disagree_blinded <- function(data,
                             session_col,
                             role_col,
                             content_col,
                             human_col,
                             ai_col,
                             seed = 123,
                             keep_cols = NULL,
                             export_xlsx = NULL,
                             export_key_xlsx = NULL,
                             highlight_method = c("border","fill"),
                             highlight_color = "#FFD966") {

  highlight_method <- match.arg(highlight_method)

  dat_clean <- data %>%
    mutate(
      .human_label = na_if(str_trim({{ human_col }}), ""),
      .ai_label    = na_if(str_trim({{ ai_col }}), "")
    )

  all_rows <- dat_clean %>%
    transmute(
      .Session_ID = {{ session_col }},
      .Role       = {{ role_col }},
      .Content    = {{ content_col }},
      .human_label,
      .ai_label
    )

  if (!is.null(keep_cols)) {
    all_rows <- bind_cols(all_rows, dat_clean %>% select({{ keep_cols }}))
  }

  all_rows <- all_rows %>%
    mutate(
      Disagreement = !(is.na(.human_label) & is.na(.ai_label)) &
        (is.na(.human_label) | is.na(.ai_label) | .human_label != .ai_label)
    ) %>%
    mutate(Disagreement_ID = if_else(Disagreement, row_number(), NA_integer_), .before = 1)

  set.seed(seed)
  assign_first <- if (sum(all_rows$Disagreement, na.rm = TRUE) > 0) {
    sample(c("human_first", "ai_first"), sum(all_rows$Disagreement, na.rm = TRUE), replace = TRUE)
  } else character(0)

  all_rows <- all_rows %>% mutate(Rater_1 = NA_character_, Rater_2 = NA_character_)

  if (length(assign_first) > 0) {
    idx <- all_rows$Disagreement
    all_rows$Rater_1[idx] <- ifelse(assign_first == "human_first",
                                    all_rows$.human_label[idx],
                                    all_rows$.ai_label[idx])
    all_rows$Rater_2[idx] <- ifelse(assign_first == "human_first",
                                    all_rows$.ai_label[idx],
                                    all_rows$.human_label[idx])
  }

  blinded <- all_rows %>%
    select(Disagreement_ID, .Session_ID, .Role, .Content, Rater_1, Rater_2) %>%
    rename(Session_ID = .Session_ID, Role = .Role, Content = .Content) %>%
    mutate(Decision = NA_character_, .after = Rater_2)

  key <- all_rows %>%
    filter(Disagreement) %>%
    transmute(
      Disagreement_ID,
      Rater_1_source = if_else(assign_first == "human_first", "HUMAN", "AI"),
      Rater_2_source = if_else(assign_first == "human_first", "AI",    "HUMAN"),
      human_label = .human_label,
      ai_label    = .ai_label
    )

  if (!is.null(export_xlsx)) {
    wb <- createWorkbook()
    addWorksheet(wb, "Blinded")
    writeData(wb, "Blinded", blinded)

    disagree_rows <- which(!is.na(blinded$Disagreement_ID)) + 1

    if (length(disagree_rows) > 0) {
      if (highlight_method == "fill") {
        st <- createStyle(fgFill = highlight_color)  # may render black on some Excel setups
        addStyle(wb, "Blinded", style = st,
                 rows = disagree_rows, cols = 1:ncol(blinded),
                 gridExpand = TRUE, stack = TRUE)
      } else { # border-based highlight (robust on Mac/Win, avoids black-fill bug)
        st <- createStyle(border = "thick",
                          borderColour = highlight_color,
                          textDecoration = "bold")
        addStyle(wb, "Blinded", style = st,
                 rows = disagree_rows, cols = 1:ncol(blinded),
                 gridExpand = TRUE, stack = TRUE)
      }
    }

    saveWorkbook(wb, export_xlsx, overwrite = TRUE)
  }

  if (!is.null(export_key_xlsx)) {
    wb2 <- createWorkbook()
    addWorksheet(wb2, "Key")
    writeData(wb2, "Key", key)
    saveWorkbook(wb2, export_key_xlsx, overwrite = TRUE)
  }

  list(blinded = blinded, key = key)
}

# Example:
# res <- disagree_blinded(
#   data = dat_dif,
#   session_col = Session_ID,
#   role_col = Role,
#   content_col = Content,
#   human_col = Human_TeacherMove,
#   ai_col = AI_TeacherMove,
#   export_xlsx = "blinded_output.xlsx",
#   export_key_xlsx = "blinded_key.xlsx",
#   highlight_method = "border",      # <- avoids black fill
#   highlight_color = "#FFD966"
# )
