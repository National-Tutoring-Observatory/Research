suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(jsonlite)
  library(rlang)
})

write_jsonl_per_session <- function(df,
                                    out_dir,
                                    session_col = Session_ID,
                                    role_col    = Role,
                                    content_col = Content) {

  session_col <- enquo(session_col)
  role_col    <- enquo(role_col)
  content_col <- enquo(content_col)

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  dat <- df %>%
    mutate(.row_id = row_number()) %>%
    arrange(.row_id) %>%
    group_by(!!session_col) %>%
    mutate(Utterance_ID = row_number() - 1L) %>%
    ungroup() %>%
    select(
      Session_ID = !!session_col,
      Utterance_ID,
      Role = !!role_col,
      Content = !!content_col
    )

  dat %>%
    group_split(Session_ID, .keep = TRUE) %>%
    walk(function(g) {
      sid <- g$Session_ID[1]
      safe_name <- gsub("[^A-Za-z0-9._-]", "_", sid)
      path <- file.path(out_dir, paste0(safe_name, ".jsonl"))

      lines <- apply(g, 1, function(r) {
        toJSON(
          list(
            Session_ID   = unname(r[["Session_ID"]]),
            Utterance_ID = as.integer(r[["Utterance_ID"]]),
            Role         = unname(r[["Role"]]),
            Content      = unname(r[["Content"]])
          ),
          auto_unbox = TRUE, null = "null"
        )
      })

      writeLines(lines, con = path, useBytes = TRUE)
    })

  invisible(TRUE)
}

# Example:
# write_jsonl_per_session(out$reviewer2$updated_ground_truth,
#                         out_dir = "Tutor_Profile_LAK25/02_data/jsonl_sessions")
