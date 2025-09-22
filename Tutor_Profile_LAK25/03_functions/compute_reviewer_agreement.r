suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readxl)
  library(openxlsx)
  library(tidyr)
  library(purrr)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a
.normalize_text <- function(x) x %>% trimws() %>% stringr::str_squish()

.parse_decision <- function(decision, r1_label, r2_label) {
  d  <- tolower(trimws(as.character(decision)))
  r1 <- tolower(trimws(as.character(r1_label)))
  r2 <- tolower(trimws(as.character(r2_label)))
  dplyr::case_when(
    d %in% c("rater_1","rater1","1","left","l","r1") ~ "Rater_1",
    d %in% c("rater_2","rater2","2","right","r","r2") ~ "Rater_2",
    d == r1 ~ "Rater_1",
    d == r2 ~ "Rater_2",
    d %in% c("ai","human") ~ toupper(d),
    d %in% c("y","yes","✓","check","true","t") ~ "Rater_1",
    d %in% c("n","no","✗","x","false","f") ~ "Rater_2",
    TRUE ~ NA_character_
  )
}

compute_reviewer_agreement <- function(reviewer_xlsx,
                                       key_xlsx,
                                       reviewer_sheet = "Blinded",
                                       key_sheet = "Key",
                                       reviewer_id = "Reviewer",
                                       decision_col = "Decision",
                                       original_df = NULL) {

  blinded_full <- openxlsx::read.xlsx(reviewer_xlsx, sheet = reviewer_sheet) %>% as_tibble()
  key          <- openxlsx::read.xlsx(key_xlsx,      sheet = key_sheet)      %>% as_tibble()

  req_cols <- c("Disagreement_ID","Rater_1","Rater_2","Session_ID","Role","Content", decision_col)
  missing  <- setdiff(req_cols, names(blinded_full))
  if (length(missing)) stop(sprintf("Missing columns in %s: %s", reviewer_xlsx, paste(missing, collapse=", ")))

  df_dis <- blinded_full %>%
    mutate(
      !!decision_col := as.character(.data[[decision_col]]),
      across(c(Rater_1, Rater_2, Session_ID, Role, Content), as.character)
    ) %>%
    filter(!is.na(Disagreement_ID)) %>%
    mutate(
      decision_is_na   = is.na(.data[[decision_col]]),
      chosen_or_source = .parse_decision(.data[[decision_col]], Rater_1, Rater_2),
      chosen_or_source = dplyr::case_when(
        decision_is_na & is.na(Rater_1) & !is.na(Rater_2) ~ "Rater_1",
        decision_is_na & is.na(Rater_2) & !is.na(Rater_1) ~ "Rater_2",
        TRUE ~ chosen_or_source
      )
    ) %>%
    left_join(
      key %>% select(Disagreement_ID, Rater_1_source, Rater_2_source, human_label, ai_label),
      by = "Disagreement_ID"
    ) %>%
    mutate(
      chosen_source = dplyr::case_when(
        chosen_or_source == "Rater_1" ~ Rater_1_source,
        chosen_or_source == "Rater_2" ~ Rater_2_source,
        chosen_or_source %in% c("AI","HUMAN") ~ chosen_or_source,
        TRUE ~ NA_character_
      ),
      reviewer_choice_label = dplyr::case_when(
        chosen_source == "HUMAN" ~ human_label,
        chosen_source == "AI"    ~ ai_label,
        TRUE ~ NA_character_
      ),
      disagreed_both = is.na(chosen_source)
    )

  agree_sum <- df_dis %>%
    filter(!is.na(chosen_source)) %>%
    count(chosen_source, name = "n") %>%
    mutate(prop = n / sum(n))

  summary <- tibble(
    reviewer = reviewer_id,
    n_HUMAN = agree_sum$n[match("HUMAN", agree_sum$chosen_source)] %||% 0L,
    n_AI    = agree_sum$n[match("AI",    agree_sum$chosen_source)] %||% 0L,
    n_disagree_both = sum(df_dis$disagreed_both, na.rm = TRUE),
    prop_HUMAN = agree_sum$prop[match("HUMAN", agree_sum$chosen_source)] %||% 0,
    prop_AI    = agree_sum$prop[match("AI",    agree_sum$chosen_source)] %||% 0,
    prop_disagree_both = (sum(df_dis$disagreed_both, na.rm = TRUE)) / nrow(df_dis)
  )

  disagreed_cases <- df_dis %>%
    filter(disagreed_both) %>%
    select(Disagreement_ID, Session_ID, Role, Content, Rater_1, Rater_2,
           !!sym(decision_col), chosen_or_source, chosen_source, human_label, ai_label)

  decisions_map <- df_dis %>%
    select(Disagreement_ID, chosen_source, reviewer_choice_label, disagreed_both)

  updated <- blinded_full %>%
    mutate(across(c(Session_ID, Role, Content), as.character)) %>%
    left_join(decisions_map, by = "Disagreement_ID") %>%
    left_join(key %>% select(Disagreement_ID, human_label, ai_label), by = "Disagreement_ID") %>%
    mutate(
      ground_truth_reviewer  = reviewer_choice_label,
      orginal_rater_reviewer = chosen_source,
      disagreed_both = if_else(!is.na(Disagreement_ID), coalesce(disagreed_both, FALSE), FALSE)
    )

  if (!is.null(original_df)) {
    orig_min <- original_df %>%
      transmute(
        Session_ID = .normalize_text(as.character(Session_ID)),
        Role       = .normalize_text(as.character(Role)),
        Content    = .normalize_text(as.character(Content)),
        Utterance_ID = as.integer(Utterance_ID),
        original_human = na_if(trimws(Human_TeacherMove), ""),
        original_ai    = na_if(trimws(AI_TeacherMove), "")
      ) %>%
      arrange(Session_ID, Utterance_ID) %>%
      group_by(Session_ID, Role, Content) %>%
      mutate(.occ = row_number()) %>%
      ungroup()

    updated <- updated %>%
      mutate(
        Session_ID_norm = .normalize_text(as.character(Session_ID)),
        Role_norm       = .normalize_text(as.character(Role)),
        Content_norm    = .normalize_text(as.character(Content))
      ) %>%
      group_by(Session_ID_norm, Role_norm, Content_norm) %>%
      mutate(.occ = row_number()) %>%
      ungroup() %>%
      left_join(
        orig_min,
        by = c("Session_ID_norm" = "Session_ID",
               "Role_norm"       = "Role",
               "Content_norm"    = "Content",
               ".occ"            = ".occ"),
        relationship = "one-to-one"
      ) %>%
      mutate(
        original_agreed = dplyr::case_when(
          !is.na(original_human) & original_human == original_ai ~ original_human,
          TRUE ~ NA_character_
        )
      ) %>%
      select(-Session_ID_norm, -Role_norm, -Content_norm, -.occ)
  } else {
    updated <- updated %>%
      mutate(
        original_human = NA_character_,
        original_ai    = NA_character_,
        original_agreed = NA_character_
      )
  }

  updated_ground_truth <- updated %>%
    mutate(
      ground_truth  = coalesce(ground_truth_reviewer, original_agreed),
      original_rater = dplyr::case_when(
        !is.na(ground_truth_reviewer) ~ orginal_rater_reviewer,
        !is.na(original_agreed)       ~ "ORIGINAL_AGREEMENT",
        TRUE ~ NA_character_
      )
    ) %>%
    select(
      Disagreement_ID, Session_ID, Role, Content,
      ground_truth, original_rater, disagreed_both,
      original_human, original_ai, original_agreed,
      ground_truth_reviewer, orginal_rater_reviewer
    )

  list(
    summary = summary,
    details = df_dis,
    disagreed_cases = disagreed_cases,
    updated_ground_truth = updated_ground_truth
  )
}
