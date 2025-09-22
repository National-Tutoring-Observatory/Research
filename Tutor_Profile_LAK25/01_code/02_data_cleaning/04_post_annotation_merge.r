# Merge UPChieve transcripts (JSON) with outcomes and raw_sessions (JSONL) into one CSV

library(jsonlite)
library(tidyverse)

# Paths
anno_folder <- "Tutor_Profile_LAK25/02_data/Annoated_UPC_files_78"
outcomes_file <- "Tutor_Profile_LAK25/02_data/UPChieve_LAK_data/outcomes_analysis.jsonl"
raw_file <- "Tutor_Profile_LAK25/02_data/UPChieve_LAK_data/raw_sessions.jsonl"

# ---------- Load Annotated Transcripts ----------
read_transcript_file <- function(file_path) {
  dat <- fromJSON(file_path, simplifyVector = FALSE)
  tr <- dat$transcript
  if (is.null(tr) || length(tr) == 0) return(NULL)

  tibble(
    utterance_id = vapply(tr, `[[`, character(1), "_id"),
    role         = vapply(tr, `[[`, character(1), "role"),
    content      = vapply(tr, `[[`, character(1), "content"),
    start_time   = vapply(tr, `[[`, character(1), "start_time"),
    end_time     = vapply(tr, `[[`, character(1), "end_time"),
    teacherMove  = map_chr(tr, function(x) {
      if (length(x$annotations) > 0) x$annotations[[1]]$teacherMove else NA_character_
    }),
    reasoning    = map_chr(tr, function(x) {
      if (length(x$annotations) > 0) x$annotations[[1]]$reasoning else NA_character_
    }),
    session_id   = tools::file_path_sans_ext(basename(file_path))
  )
}

anno_files <- list.files(anno_folder, pattern = "\\.json$", full.names = TRUE, recursive = TRUE)
anno_df <- map_dfr(anno_files, read_transcript_file)

# ---------- Load Outcomes + Raw Sessions ----------
outcomes_df <- stream_in(file(outcomes_file)) %>% as_tibble()
raw_df <- stream_in(file(raw_file)) %>% as_tibble()

# ---------- Merge ----------
merged <- anno_df %>%
  left_join(outcomes_df, by = "session_id") %>%
  left_join(raw_df, by = c("session_id", "student_id", "topic_name", "subject_name"))
str(merged)
length(unique(merged$session_id))
merged$teacherMove <- ifelse(merged$teacherMove == "GIVING_HINT" | merged$teacherMove== "EXEMPLIFYING" | merged$teacherMove== "REVOICING", "SCAFFOLDING", merged$teacherMove)

# ---------- Save ----------
write_csv(merged, "Tutor_Profile_LAK25/02_data/UPChieve_LAK_data/annotated_utterances_for_analysis.csv")
