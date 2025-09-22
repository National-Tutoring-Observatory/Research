merge_tutoring_files <- function(
  utter_csv_path,
  jsonl1_path,
  jsonl2_path,
  out_csv_path = NULL
) {
  suppressPackageStartupMessages({
    library(readr)
    library(dplyr)
    library(purrr)
    library(jsonlite)
    library(janitor)
    library(tidyr)
  })

  read_jsonl <- function(path) {
    if (!file.exists(path) || file.size(path) == 0) return(tibble())
    lines <- read_lines(path, locale = locale(encoding = "UTF-8"))
    if (!length(lines)) return(tibble())
    map(lines, ~fromJSON(.x, flatten = TRUE)) %>% bind_rows() %>% as_tibble() %>% clean_names()
  }

  # Utterances
  utter <- read_csv(
    utter_csv_path,
    show_col_types = FALSE,
    locale = locale(encoding = "UTF-8")
  ) %>%
    clean_names() %>%
    rename(
      session_id   = dplyr::coalesce(match("session_id", names(.)), match("sessionid", names(.))) %>% { names(.)[.[1]] },
      utterance_id = dplyr::coalesce(match("utterance_id", names(.)), match("utteranceid", names(.))) %>% { names(.)[.[1]] }
    ) %>%
    distinct(session_id, utterance_id, .keep_all = TRUE)

  stopifnot("session_id" %in% names(utter))

  # JSONL sources
  j1 <- read_jsonl(jsonl1_path)
  j2 <- read_jsonl(jsonl2_path)

  # Ensure unique rows per session in each JSONL (first non-NA wins)
  squash_by_session <- function(df) {
    if (nrow(df) == 0) return(df)
    stopifnot("session_id" %in% names(df))
    df %>%
      group_by(session_id) %>%
      summarise(across(everything(), ~ suppressWarnings(first(.[!is.na(.)])), .names = "{.col}"), .groups = "drop")
  }

  j1u <- squash_by_session(j1)
  j2u <- squash_by_session(j2)

  # Merge JSONLs: coalesce overlapping columns, keep one row per session_id
  meta <- list(j1u, j2u) %>%
    discard(~ nrow(.x) == 0) %>%
    reduce(function(a, b) {
      out <- full_join(a, b, by = "session_id", suffix = c("_x", "_y"))
      both <- intersect(names(a), names(b))
      both <- setdiff(both, "session_id")
      if (length(both)) {
        for (nm in both) {
          x <- paste0(nm, "_x")
          y <- paste0(nm, "_y")
          out[[nm]] <- dplyr::coalesce(out[[x]], out[[y]])
          out[[x]] <- NULL
          out[[y]] <- NULL
        }
      }
      # Keep any remaining *_x/*_y (non-overlapping names from each side)
      out
    })

  # Final safety: one row per session_id
  meta <- squash_by_session(meta)

  # Join
  merged <- utter %>% left_join(meta, by = "session_id")

  if (!is.null(out_csv_path)) write_csv(merged, out_csv_path)

  merged
}
