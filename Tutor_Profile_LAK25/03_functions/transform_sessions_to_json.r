# transform_sessions_to_json.R
# Usage:
#   Rscript transform_sessions_to_json.R input.csv output_dir
# Or source and call: transform_sessions_to_json("input.csv", "out")

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(jsonlite)
})

transform_sessions_to_json <- function(input_csv, output_dir) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  df <- read_csv(input_csv, show_col_types = FALSE, progress = FALSE) |>
    rename_with(~str_replace_all(.x, "\\s+", "_")) |>
    rename(
      Session_ID   = any_of(c("Session_ID", "Session.ID", "Session")),
      Utterance_ID = any_of(c("Utterance_ID", "Utterance.ID", "Utterance")),
      Role         = any_of("Role"),
      Content      = any_of(c("Content", "Text")),
      Start_Time   = any_of(c("Start_Time", "Start.Time", "Start")),
      End_Time     = any_of(c("End_Time", "End.Time", "End"))
    ) |>
    mutate(
      Utterance_ID = suppressWarnings(as.integer(Utterance_ID)),
      Content = as.character(Content)
    ) |>
    arrange(Session_ID, Utterance_ID)

  sessions <- df |>
    group_by(Session_ID) |>
    summarise(
      transcript = str_squish(paste0(Content, collapse = " ")),
      .groups = "drop"
    ) |>
    mutate(
      transcript = if_else(
        str_detect(transcript, "^TUTORING-CONVERSATION:"),
        transcript,
        paste0("TUTORING-CONVERSATION: ", transcript)
      )
    )

  pwalk(
    list(sessions$Session_ID, sessions$transcript),
    function(id, tx) {
      obj <- list(id = id, transcript = tx)
      out_path <- file.path(output_dir, paste0(id, ".json"))
      writeLines(toJSON(obj, auto_unbox = TRUE), con = out_path, useBytes = TRUE)
    }
  )

  invisible(output_dir)
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 2) {
  transform_sessions_to_json(args[[1]], args[[2]])
}
