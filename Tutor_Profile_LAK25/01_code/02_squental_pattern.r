# load packages
library(dplyr)
library(tidyr)
library(purrr)
library(arulesSequences)


# load data
out<-read.csv("Tutor_Profile_LAK25/02_data/HMM_posterior_probs.csv")
str(out)
table(out$state_label)

support <- 0.10
drop_items <- c("UNLABLED_TEACHER_CHAT", "STUDENT_CHAT")

safe_name <- function(x) gsub("[^A-Za-z0-9_-]+", "_", x)

out_dir <- "Tutor_Profile_LAK25/02_data"
res_dir <- "Tutor_Profile_LAK25/05_table"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(res_dir, showWarnings = FALSE, recursive = TRUE)

states <- unique(out$state_label)
df_list <- list()

for (st in states) {
  seq_data <- out %>%
    filter(state_label == st) %>%
    transmute(sequenceID = Session_ID,
              eventID    = as.integer(Utterance.ID),
              items      = as.character(AI_TeacherMove)) %>%
    arrange(sequenceID, eventID) %>%
    filter(!is.na(eventID), eventID > 0,
           !items %in% drop_items)

  if (nrow(seq_data) == 0) {
    message("Skipping state ", st, " (no valid sequences).")
    next
  }

  txt_path <- file.path(out_dir, paste0("seq_", safe_name(st), ".txt"))
  write.table(seq_data, txt_path, sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)

  seq_trans <- read_baskets(txt_path, info = c("sequenceID", "eventID"))

  sps <- cspade(seq_trans,
                parameter = list(support = support),
                control   = list(verbose = TRUE))

  if (length(sps) == 0) {
    message("No patterns found for state ", st)
    next
  }

  df <- as(sps, "data.frame")
  df$num_moves <- lengths(regmatches(df$sequence, gregexpr("\\{[^}]+\\}", df$sequence)))
  df$State <- st
  df_list[[st]] <- df

  write.csv(df, file.path(res_dir, paste0("SPM_", safe_name(st), ".csv")), row.names = FALSE)
}

if (length(df_list) > 0) {
  results_all <- dplyr::bind_rows(df_list) %>%
    arrange(State, desc(support))
  write.csv(results_all, file.path(res_dir, "SPM_all_states.csv"), row.names = FALSE)
} else {
  warning("No sequential patterns mined for any state.")
}

write.csv(results_all, file.path(res_dir, "SPM_all_states.csv"), row.names = FALSE)
write.csv(results_all, file.path(res_dir, "SPM_all_states.csv"), row.names = FALSE)
results_all <- read.csv(file.path(res_dir, "SPM_all_states.csv"))

# example views
head(results_all[results_all$State == states[1] & results_all$num_moves > 3, ], 10)
head(results_all[results_all$State == states[2] & results_all$num_moves > 3, ], 10)
head(results_all[results_all$State == states[3] & results_all$num_moves > 3, ], 10)

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)

# --- colors ---
move_colors <- c(
  "ERROR_CORRECTION"         = "#d53e4f",
  "PROVIDING_EXPLANATION"    = "#f46d43",
  "SCAFFOLDING"              = "#fdae61",
  "GIVING_HINT"              = "#fee08b",
  "PROBING_STUDENT_THINKING" = "#ffffbf",
  "EXEMPLIFYING"             = "#e6f598",
  "PROMPTING"                = "#abdda4",
  "REVOICING"                = "#66c2a5",
  "USING_VISUAL_CUES"        = "#3288bd",
  "EMOTIONAL_SUPPORT"        = "#6a4ea3",
  "GIVING_PRAISE"            = "#984EA3",
  "UNLABLED_TEACHER_CHAT"    = "#FFFFFF",
  "STUDENT_CHAT"             = "#DDDDDD"
)

# parse sequence string into moves
parse_moves <- function(s) str_extract_all(s, "(?<=\\{)[^}]+(?=\\})")[[1]]

# You’re only seeing ~5 because the y variable is the *sequence string*,
# which can clash across facets and drop levels. Use a per-state RANK for y.

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)

# parse sequence string into moves
parse_moves <- function(s) str_extract_all(s, "(?<=\\{)[^}]+(?=\\})")[[1]]

top_n <- 10

# pick top-N per state
res_top <- results_all %>%
  filter(num_moves > 3) %>%
  group_by(State) %>%
  slice_max(order_by = support, n = top_n, with_ties = FALSE) %>%
  arrange(State, desc(support)) %>%
  mutate(rank = row_number()) %>%        # 1 = highest support within the State
  ungroup()

# explode into long form
long <- res_top %>%
  mutate(moves = map(sequence, parse_moves)) %>%
  unnest_longer(moves, indices_include = TRUE,
                values_to = "Move", indices_to = "pos")

# ensure highest support at top by y = factor(rank, rev)
long <- long %>%
  mutate(rank_f = factor(rank, levels = rev(sort(unique(rank)))))

# support labels (once per rank row)
support_df <- res_top %>%
  transmute(State, rank,
            rank_f = factor(rank, levels = rev(sort(unique(res_top$rank)))),
            support = round(support, 2))

# max pattern length for x scale
max_len <- max(long$pos, na.rm = TRUE)



ggplot(long, aes(x = pos, y = rank_f)) +
  geom_tile(aes(fill = Move), width = 0.95, height = 0.95, color = "white") +
  geom_text(data = support_df,
            aes(x = 0, y = factor(rank, levels = rev(sort(unique(rank)))), label = support),
            hjust = 1, size = 3.8) +
  scale_fill_manual(values = move_colors, na.value = "grey70") +
  scale_x_continuous(breaks = seq_len(max_len),
                     expand = expansion(add = c(2, 0.5))) +
  labs(x = "Position in Pattern", y = NULL, fill = "Move") +
  facet_wrap(~ State, scales = "free_y") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    strip.text = element_text(face = "bold")
  )


 ggsave("Tutor_Profile_LAK25/04_figures/spm_top_sequences_by_state.jpeg",
        width = 16, height = 8, dpi = 300)







library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)

# --- clean text: tokenize into trigrams ---
trigrams <- out %>%
  filter(!is.na(Content), Content != "", Content != "NA") %>%
  unnest_tokens(trigram, Content, token = "ngrams", n = 3)

# --- count trigrams by state ---
trigram_counts <- trigrams %>%
  count(state_label, trigram, sort = TRUE) %>%
  filter(!is.na(trigram), trigram != "")

# --- take top 10 per state ---
top_trigrams <- trigram_counts %>%
  group_by(state_label) %>%
  slice_max(order_by = n, n = 10, with_ties = FALSE) %>%
  ungroup()

# --- reorder factors so bars are ordered ---
top_trigrams <- top_trigrams %>%
  group_by(state_label) %>%
  mutate(trigram = reorder_within(trigram, n, state_label)) %>%
  ungroup()

# --- plot ---
ggplot(top_trigrams, aes(x = trigram, y = n, fill = state_label)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~ state_label, scales = "free_y") +
  labs(x = NULL, y = "Count", title = "Top Trigrams of Content by State") +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(face = "bold"))

 ggsave("Tutor_Profile_LAK25/04_figures/top_trigams_sequences_by_state.jpeg",
        width = 16, height = 8, dpi = 300)
