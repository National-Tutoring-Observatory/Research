## load data
dat <- read.csv("Clustering_Exploration/02_data/UPChieve_HumanAI_Annotations.csv", stringsAsFactors = FALSE)
str(dat)
length(unique(dat$Session_ID))
table(dat$AI_TeacherMove, dat$Role)
levels(as.factor(dat$AI_TeacherMove))


# Recode empty AI_TeacherMove based on Role
dat$AI_TeacherMove[dat$AI_TeacherMove == "" & dat$Role == "TEACHER"] <- "UNLABLED_TEACHER_CHAT"
dat$AI_TeacherMove[dat$AI_TeacherMove == "" & dat$Role == "STUDENT_1"] <- "STUDENT_CHAT"

move_colors <- c(
  "ERROR_CORRECTION" = "#d53e4f",           # 1
  "PROVIDING_EXPLANATION" = "#f46d43",              # 2
  "SCAFFOLDING" = "#fdae61",              # 3
  "GIVING_HIN" = "#fee08b",      # 4
  "PROBING_STUDENT_THINKING" = "#ffffbf", # 5
  "EXEMPLIFYING" = "#e6f598",       # 6
  "PROMPTING" = "#abdda4",  # 7
  "REVOICING" = "#66c2a5",            # 8
  "USING_VISUAL_CUEST" = "#3288bd",            # 9
  "EMOTIONAL_SUPPORT" = "#6a4ea3",      # unchanged
  "GIVING_PRAISE" = "#984EA3",          # unchanged
  "UNLABLED_TEACHER_CHAT" = "#FFFFFF",  # unchanged
  "STUDENT_CHAT" = "#DDDDDD" 
)

# Install if needed
# install.packages("arulesSequences")
library(arulesSequences)
library(dplyr)

# Prepare data: each utterance is an event in a session
seq_data <- dat %>%
  select(Session_ID, Utterance.ID, AI_TeacherMove) %>%
  arrange(Session_ID, Utterance.ID)

# arulesSequences expects a data.frame with columns: sequenceID, eventID, and items
names(seq_data) <- c("sequenceID", "eventID", "items")
seq_data$eventID <- as.integer(seq_data$eventID)
seq_data <- seq_data %>%
  filter(!is.na(eventID), eventID > 0,
  !items %in% c('UNLABLED_TEACHER_CHAT', 'STUDENT_CHAT'))
# Convert to transactions
write.table(seq_data, "Clustering_Exploration/02_data/seq_data.txt", sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)
seq_trans <- read_baskets("Clustering_Exploration/02_data/seq_data.txt", info = c("sequenceID", "eventID"))

# Mine sequential patterns (e.g., min support 0.1)
sps <- cspade(seq_trans, parameter = list(support = 0.3), control = list(verbose = TRUE))

# View results
summary(sps)
inspect(sps)


# Get results as a data.frame
results_df <- as(sps, "data.frame")

# Sort by support (descending)
results_df <- results_df[order(-results_df$support), ]

# View top sequences
head(results_df[results_df$num_moves > 3, ], 30)  
# Only patterns that occur in more than 5 sequences

results_df$num_moves <- lengths(regmatches(results_df$sequence, gregexpr("\\{[^}]+\\}", results_df$sequence)))

results_df_filtered <- results_df[results_df$num_moves == 7, ]

# View top filtered patterns
head(results_df_filtered, 50)

# Now extract motifs from the frequent patterns

# Load necessary libraries
library(arulesSequences)
library(tidyverse)

# For simplicity, let's say we want the top N motifs
top_motifs <- head(sort(seq_trans, by = "support"), n = 10)

# Print top motifs
inspect(top_motifs)


table(dat$AI_TeacherMove)

library(ggplot2)
top_patterns <- head(results_df[order(-results_df$support) & results_df$num_moves >= 3, ], 20)
ggplot(top_patterns, aes(x = reorder(sequence, support), y = support)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top Sequential Patterns", x = "Pattern", y = "Support") +
  theme_minimal()


# Install if needed
# install.packages("ggalluvial")
library(ggalluvial)
library(dplyr)

# Extract top 2-move patterns
results_df$num_moves <- lengths(regmatches(results_df$sequence, gregexpr("\\{[^}]+\\}", results_df$sequence)))
sankey_df <- results_df %>%
  filter(num_moves == 2) %>%
  arrange(desc(support)) %>%
  head(20)
#install.packages("ggalluvial")
library(ggalluvial)
# Parse moves from sequence string
library(stringr)
sankey_df$move1 <- str_extract(sankey_df$sequence, "\\{[^}]+\\}") %>% str_replace_all("[\\{\\}]", "")
sankey_df$move2 <- str_extract(sankey_df$sequence, "(?<=\\},)\\{[^}]+\\}") %>% str_replace_all("[\\{\\}]", "")

# Prepare data for ggalluvial
ggplot(sankey_df,
       aes(axis1 = move1, axis2 = move2, y = support)) +
  geom_alluvium(aes(fill = move1), width = 1/12) +
  geom_stratum(width = 1/12, fill = "grey", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Move 1", "Move 2"), expand = c(.05, .05)) +
  labs(title = "Sankey Diagram of Top 2-Move Patterns",
       y = "Support") +
  theme_minimal()

  library(ggalluvial)
library(stringr)
library(dplyr)

# Extract top 3-move patterns
results_df$num_moves <- lengths(regmatches(results_df$sequence, gregexpr("\\{[^}]+\\}", results_df$sequence)))
sankey_df <- results_df %>%
  filter(num_moves > 1, num_moves <= 3) %>%
  arrange(desc(support)) %>%
  head(20)

# Parse moves from sequence string
sankey_df$moves <- str_extract_all(sankey_df$sequence, "\\{([^}]+)\\}")
sankey_df$move1 <- sapply(sankey_df$moves, function(x) str_replace_all(x[1], "[\\{\\}]", ""))
sankey_df$move2 <- sapply(sankey_df$moves, function(x) str_replace_all(x[2], "[\\{\\}]", ""))
sankey_df$move3 <- sapply(sankey_df$moves, function(x) str_replace_all(x[3], "[\\{\\}]", ""))

# Prepare data for ggalluvial
ggplot(sankey_df,
       aes(axis1 = move1, axis2 = move2, axis3 = move3, y = support)) +
  geom_alluvium(aes(fill = move1), width = 1/12) +
  geom_stratum(width = 1/12, fill = "grey", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Move 1", "Move 2", "Move 3"), expand = c(.05, .05)) +
  labs(title = "Sankey Diagram of Top 3-Move Patterns",
       y = "Support") +
  theme_minimal()

# Extract top patterns with 2-5 moves
results_df$num_moves <- lengths(regmatches(results_df$sequence, gregexpr("\\{[^}]+\\}", results_df$sequence)))
sankey_df <- results_df %>%
  filter(num_moves >= 2, num_moves <= 5) %>%
  arrange(desc(support)) %>%
  head(30)

# Parse up to 5 moves, pad with NA if missing
max_moves <- 5
moves_list <- str_extract_all(sankey_df$sequence, "\\{([^}]+)\\}")
pad_moves <- function(x, n) { y <- str_replace_all(x, "[\\{\\}]", ""); length(y) <- n; y }
padded_moves <- lapply(moves_list, pad_moves, max_moves)
padded_moves_df <- as.data.frame(do.call(rbind, padded_moves), stringsAsFactors = FALSE)
names(padded_moves_df) <- paste0("move", 1:max_moves)

# Remove any previous move columns before binding
sankey_df <- sankey_df[, !(names(sankey_df) %in% names(padded_moves_df))]
sankey_df <- cbind(sankey_df, padded_moves_df)

# Remove rows where move1 or move2 is NA (must have at least 2 moves)
sankey_df <- sankey_df[!is.na(sankey_df$move1) & !is.na(sankey_df$move2), ]

# Remove rows where all moves are NA (shouldn't happen, but just in case)
sankey_df <- sankey_df[rowSums(is.na(sankey_df[, paste0("move", 1:max_moves)])) < max_moves, ]

# Set factor levels for each move axis by support
for (i in 1:max_moves) {
  move_col <- paste0("move", i)
  move_counts <- sankey_df %>% count(.data[[move_col]], wt = support, sort = TRUE)
  move_counts <- move_counts[!is.na(move_counts[[move_col]]), ]
  sankey_df[[move_col]] <- factor(sankey_df[[move_col]], levels = move_counts[[move_col]])
}

# Only plot if there is data
if (nrow(sankey_df) > 0) {
  ggplot(sankey_df,
         aes(axis1 = move1, axis2 = move2, axis3 = move3, axis4 = move4, axis5 = move5, y = support)) +
    geom_alluvium(aes(fill = move1), width = 1/12, na.rm = TRUE) +
    geom_stratum(width = 1/12, fill = "grey", color = "black", na.rm = TRUE) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), na.rm = TRUE) +
    scale_x_discrete(limits = paste("Move", 1:max_moves), expand = c(.05, .05)) +
    labs(title = "Sankey Diagram of Top 2-5 Move Patterns",
         y = "Support") +
    theme_minimal()
} else {
  print("No valid patterns to plot.")
}


ggplot(sankey_df,
         aes(axis1 = move1, axis2 = move2, axis3 = move3, axis4 = move4, axis5 = move5, y = support)) +
    geom_alluvium(aes(fill = move1), width = 1/12, na.rm = TRUE) +
    geom_stratum(width = 1/12, fill = "grey", color = "black", na.rm = TRUE) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), na.rm = TRUE) +
    scale_x_discrete(limits = paste("Move", 1:max_moves), expand = c(.05, .05)) +
    labs(title = "Sankey Diagram of Top 2-5 Move Patterns",
         y = "Support") +
    theme_minimal()




library(dplyr)
library(forcats)
library(ggplot2)
library(ggalluvial)


# detect move columns and ensure support exists
move_cols <- grep("^move\\d+$", names(sankey_df), value = TRUE)
stopifnot(length(move_cols) >= 2, "support" %in% names(sankey_df))

# clean and prepare
sankey_clean <- sankey_df %>%
  select(all_of(move_cols), support) %>%
  filter(if_all(all_of(move_cols), ~ !is.na(.))) %>%
  mutate(across(all_of(move_cols), ~ fct_explicit_na(.x, na_level = "(missing)"))) %>%
  mutate(id = row_number())

# lodes form for ggalluvial
sankey_lodes <- ggalluvial::to_lodes_form(
  data  = sankey_clean,
  axes  = move_cols,
  id    = "id",
  key   = "axis",
  value = "move",
  discern = FALSE
) %>%
  left_join(sankey_clean %>% select(id, first_move = !!sym(move_cols[1])), by = "id")

# x-axis labels
x_labels <- paste("Move", seq_along(move_cols))

# plot
ggplot(
  sankey_lodes,
  aes(x = axis, stratum = move, alluvium = id, y = support)
) +
  geom_flow(aes(fill = first_move), width = 1/12, na.rm = TRUE) +
  geom_stratum(width = 1/12, fill = "grey", color = "black", na.rm = TRUE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), na.rm = TRUE) +
  scale_x_discrete(limits = move_cols, labels = x_labels, expand = c(.05, .05)) +
  labs(title = "Sankey Diagram of Top 2–5 Move Patterns", y = "Support", fill = paste0(move_cols[1], " (first)")) +
  theme_minimal()

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggalluvial)


# columns and axis labels
move_cols <- grep("^move[1-5]$", names(sankey_df), value = TRUE)
x_labels  <- paste("Move", seq_along(move_cols))

# long/lodes data
sankey_lodes <- sankey_df %>%
  mutate(
    id         = row_number(),
    support    = as.numeric(support),
    first_move = as.character(move1)
  ) %>%
  pivot_longer(cols = all_of(move_cols), names_to = "axis", values_to = "move") %>%
  filter(!is.na(move)) %>%                      # drop missing steps
  mutate(
    axis = factor(axis, levels = move_cols),
    move = as.character(move)
  )

# plot
ggplot(
  sankey_lodes,
  aes(x = axis, stratum = move, alluvium = id, y = support)
) +
  geom_alluvium(aes(fill = first_move), width = 1/12, na.rm = TRUE) +
  geom_stratum(width = 1/12, fill = "grey", color = "black", na.rm = TRUE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), na.rm = TRUE) +
  scale_x_discrete(limits = move_cols, labels = x_labels, expand = c(.05, .05)) +
  labs(title = "Sankey Diagram of Top 2–5 Move Patterns", y = "Support", fill = paste0(move_cols[1], " (first)")) +
  theme_minimal()
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggalluvial)
library(stringr)

# columns and axis labels
move_cols <- grep("^move[1-5]$", names(sankey_df), value = TRUE)
axis_labels <- gsub("^move", "Move ", move_cols)

# assign colors to moves
move_levels <- unique(unlist(lapply(sankey_df[move_cols], levels)))
move_colors <- RColorBrewer::brewer.pal(min(length(move_levels), 8), "Set2")
move_color_map <- setNames(move_colors, move_levels)

# long/lodes data
sankey_lodes <- sankey_df %>%
  mutate(
    id         = row_number(),
    support    = as.numeric(support),
    first_move = as.character(move1)
  ) %>%
  pivot_longer(cols = all_of(move_cols), names_to = "axis", values_to = "move") %>%
  filter(!is.na(move)) %>%
  mutate(
    axis = factor(axis, levels = move_cols),
    move = as.character(move)
  )

# plot
p <- ggplot(
  sankey_lodes,
  aes(x = axis, stratum = move, alluvium = id, y = support)
) +
  geom_alluvium(aes(fill = first_move), width = 1/8, alpha = 0.7, na.rm = TRUE) +
  geom_stratum(aes(fill = move), width = 1/4, color = "black", na.rm = TRUE) +
  scale_fill_manual(values = move_color_map) +
  geom_text(
    stat = "stratum",
    aes(label = after_stat(str_wrap(str_replace_all(stratum, "_", " "), width = 12))),
    size = 3,
    lineheight = 0.9,
    na.rm = TRUE
  ) +
  scale_x_discrete(limits = move_cols, labels = axis_labels, expand = c(.05, .05)) +
  guides(fill = guide_legend(title = "Move")) +
  labs(title = "Sankey Diagram of Top 2–5 Move Patterns", y = "Support", x = "axis") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

# save wide graph
ggsave("sankey_moves_wide.png", plot = p, width = 18, height = 10, dpi = 300, units = "in")

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggalluvial)
library(stringr)
library(scales)

# columns and axis labels
move_cols <- grep("^move[1-5]$", names(sankey_df), value = TRUE)
axis_labels <- gsub("^move", "Move ", move_cols)

# custom colors for moves
move_color_map <- c(
  "ERROR_CORRECTION" = "#d53e4f",
  "PROVIDING_EXPLANATION" = "#f46d43",
  "SCAFFOLDING" = "#fdae61",
  "GIVING_HIN" = "#fee08b",
  "PROBING_STUDENT_THINKING" = "#ffffbf",
  "EXEMPLIFYING" = "#e6f598",
  "PROMPTING" = "#abdda4",
  "REVOICING" = "#66c2a5",
  "USING_VISUAL_CUEST" = "#3288bd",
  "EMOTIONAL_SUPPORT" = "#6a4ea3",
  "GIVING_PRAISE" = "#984EA3",
  "UNLABLED_TEACHER_CHAT" = "#FFFFFF",
  "STUDENT_CHAT" = "#DDDDDD"
)

# collapse identical paths so flows only split when moves actually change
path_df <- sankey_df %>%
  select(all_of(move_cols), support) %>%
  mutate(across(all_of(move_cols), ~ as.character(.))) %>%
  group_by(across(all_of(move_cols))) %>%
  summarise(support = sum(as.numeric(support), na.rm = TRUE), .groups = "drop") %>%
  mutate(
    id         = row_number(),
    first_move = .data[[move_cols[1]]]
  )

# lodes (long) data
sankey_lodes <- path_df %>%
  tidyr::pivot_longer(cols = all_of(move_cols), names_to = "axis", values_to = "move") %>%
  filter(!is.na(move)) %>%
  mutate(
    axis = factor(axis, levels = move_cols),
    move = as.character(move)
  )

# plot with streams kept intact until change
p <- ggplot(
  sankey_lodes,
  aes(x = axis, stratum = move, alluvium = id, y = support)
) +
  geom_alluvium(aes(fill = first_move), width = 0.18, alpha = 0.7, na.rm = TRUE) +
  geom_stratum(aes(fill = move), width = 0.32, color = "black", na.rm = TRUE) +
  scale_fill_manual(values = move_color_map, drop = FALSE) +
  geom_text(
    stat = "stratum",
    aes(label = after_stat(str_wrap(str_replace_all(stratum, "_", " "), width = 12))),
    size = 3,
    lineheight = 0.9,
    na.rm = TRUE
  ) +
  scale_x_discrete(limits = move_cols, labels = axis_labels, expand = c(.05, .05)) +
  guides(fill = guide_legend(title = "Move")) +
  labs(title = "Sankey Diagram of Top 2–5 Move Patterns", y = "Support", x = "axis") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")


# save wide graph
ggsave("sankey_moves_wide.png", plot = p, width = 18, height = 10, dpi = 300, units = "in")


