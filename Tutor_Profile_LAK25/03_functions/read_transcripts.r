read_transcripts <- function(parent_folder) {
  files <- list.files(parent_folder, pattern = "\\.json$", recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) return(tibble::tibble())

  parse_file <- function(file_path) {
    dat <- jsonlite::fromJSON(file_path, simplifyVector = FALSE)
    tr  <- dat$transcript
    if (is.null(tr) || length(tr) == 0) return(NULL)

    tibble::tibble(
      `_id`        = vapply(tr, `[[`, character(1), "_id"),
      role         = vapply(tr, `[[`, character(1), "role"),
      content      = vapply(tr, `[[`, character(1), "content"),
      start_time   = vapply(tr, `[[`, character(1), "start_time"),
      end_time     = vapply(tr, `[[`, character(1), "end_time"),
      annotations  = lapply(tr, `[[`, "annotations")
    ) |>
      dplyr::mutate(
        Session_ID   = tools::file_path_sans_ext(basename(file_path)),
        Utterance_ID = as.character(`_id`)
      ) |>
      dplyr::select(Session_ID, Utterance_ID, role, content, start_time, end_time, annotations) |>
      tidyr::unnest_longer(annotations, keep_empty = TRUE) |>
      tidyr::unnest_wider(annotations, names_sep = "_", simplify = TRUE) |>
      dplyr::rename(
        Annotation_ID = `annotations__id`,
        identifiedBy  = annotations_identifiedBy,
        teacherMove   = annotations_teacherMove,
        reasoning     = annotations_reasoning
      )
  }

  res <- purrr::map(files, parse_file) |> purrr::compact()
  if (length(res) == 0) return(tibble::tibble())
  dplyr::bind_rows(res)
}
