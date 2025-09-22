suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readxl)
  library(openxlsx)
  library(tidyr)
  library(purrr)
})

# --- Robust parser for reviewer decisions ---
# Supports: "rater_1", "rater1", "1", "left", exact label text, "AI", "HUMAN"
.parse_decision <- function(decision, r1_label, r2_label) {
  d <- tolower(trimws(as.character(decision)))
  r1 <- tolower(trimws(as.character(r1_label)))
  r2 <- tolower(trimws(as.character(r2_label)))

  case_when(
    # direct rater references
    d %in% c("rater_1","rater1","1","left","l","r1") ~ "Rater_1",
    d %in% c("rater_2","rater2","2","right","r","r2") ~ "Rater_2",

    # exact label text entered
    d == r1 ~ "Rater_1",
    d == r2 ~ "Rater_2",

    # explicit source entered
    d %in% c("ai","human") ~ toupper(d),  # will be handled downstream

    # common checkmarks / booleans (assume "yes" means Rater_1 if only R1 was presented first)
    d %in% c("y","yes","✓","check","true","t") ~ "Rater_1",
    d %in% c("n","no","✗","x","false","f") ~ "Rater_2",

    TRUE ~ NA_character_
  )
}

# --- Agreement computation for one reviewer file ---
compute_reviewer_agreement <- function(reviewer_xlsx,
                                       key_xlsx,
                                       reviewer_sheet = "Blinded",
                                       key_sheet = "Key",
                                       reviewer_id = "Reviewer",
                                       decision_col = "Decision") {

  # Read with openxlsx to preserve column names consistently
  blinded <- openxlsx::read.xlsx(reviewer_xlsx, sheet = reviewer_sheet) %>% as_tibble()
  key     <- openxlsx::read.xlsx(key_xlsx,      sheet = key_sheet)      %>% as_tibble()

  # Sanity: ensure required columns exist
  req_cols <- c("Disagreement_ID","Rater_1","Rater_2", decision_col)
  missing  <- setdiff(req_cols, names(blinded))
  if (length(missing)) stop(sprintf("Missing columns in %s: %s", reviewer_xlsx, paste(missing, collapse=", ")))

  df <- blinded %>%
    filter(!is.na(Disagreement_ID)) %>%
    mutate(
      chosen_or_source = .parse_decision(.data[[decision_col]], Rater_1, Rater_2)
    ) %>%
    left_join(key %>% select(Disagreement_ID, Rater_1_source, Rater_2_source), by = "Disagreement_ID") %>%
    mutate(
      chosen_source = case_when(
        chosen_or_source == "Rater_1" ~ Rater_1_source,
        chosen_or_source == "Rater_2" ~ Rater_2_source,
        chosen_or_source %in% c("AI","HUMAN") ~ chosen_or_source,  # explicit entry
        TRUE ~ NA_character_
      )
    )

  # Diagnostics: what values were seen in the decision column
  decisions_seen <- df %>%
    count(!!sym(decision_col), name = "n") %>%
    arrange(desc(n))

  # Summary: counts and proportions of HUMAN vs AI
  summary <- df %>%
    filter(!is.na(chosen_source)) %>%
    count(chosen_source, name = "n") %>%
    mutate(
      reviewer = reviewer_id,
      prop = n / sum(n),
      chosen_source = factor(chosen_source, levels = c("HUMAN","AI"))
    ) %>%
    arrange(chosen_source)

  list(summary = summary, details = df, decisions_seen = decisions_seen)
}

# --- Compare two reviewers and bind summaries; include diagnostics for unmapped decisions ---
compare_two_reviewers <- function(reviewer1_xlsx,
                                  reviewer2_xlsx,
                                  key_xlsx,
                                  reviewer1_id = "Reviewer_1",
                                  reviewer2_id = "Reviewer_2",
                                  reviewer_sheet = "Blinded",
                                  key_sheet = "Key",
                                  decision_col = "Decision") {

  r1 <- compute_reviewer_agreement(
    reviewer_xlsx = reviewer1_xlsx,
    key_xlsx = key_xlsx,
    reviewer_sheet = reviewer_sheet,
    key_sheet = key_sheet,
    reviewer_id = reviewer1_id,
    decision_col = decision_col
  )

  r2 <- compute_reviewer_agreement(
    reviewer_xlsx = reviewer2_xlsx,
    key_xlsx = key_xlsx,
    reviewer_sheet = reviewer_sheet,
    key_sheet = key_sheet,
    reviewer_id = reviewer2_id,
    decision_col = decision_col
  )

  combined_summary <- bind_rows(r1$summary, r2$summary) %>%
    tidyr::pivot_wider(names_from = chosen_source, values_from = c(n, prop), values_fill = 0)

  # Add quick diagnostics on unmapped decisions
  diag_unmapped <- bind_rows(
    r1$details %>% mutate(reviewer = reviewer1_id),
    r2$details %>% mutate(reviewer = reviewer2_id)
  ) %>%
    mutate(unmapped = is.na(chosen_source)) %>%
    group_by(reviewer) %>%
    summarise(
      total = n(),
      disagreements = sum(!is.na(Disagreement_ID)),
      unmapped = sum(unmapped),
      unmapped_rate = unmapped / disagreements,
      .groups = "drop"
    )

  list(
    reviewer1 = r1,
    reviewer2 = r2,
    combined_summary = combined_summary,
    diagnostics = diag_unmapped
  )
}

# --- Example ---
# out <- compare_two_reviewers(
#   reviewer1_xlsx = "02_data/disagreement_blinded_jinsook.xlsx",
#   reviewer2_xlsx = "02_data/disagreement_blinded_sarah.xlsx",
#   key_xlsx = "02_data/disagreement_key.xlsx",
#   reviewer1_id = "J",
#   reviewer2_id = "S",
#   decision_col = "Decision"
# )
# out$combined_summary
# out$diagnostics                  # check unmapped counts/rates per reviewer
# out$reviewer2$decisions_seen     # see raw distinct decision entries causing NAs
