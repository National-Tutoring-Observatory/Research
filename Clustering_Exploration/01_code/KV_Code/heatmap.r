## packages
library(dplyr)
#install.packages("tidytext", dependencies = TRUE)
library(tidytext)
library(ggplot2)
#install.packages("patchwork")
library(patchwork)
library(lubridate)


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

# Assign session number
dat <- dat %>%
  mutate(session_num = as.integer(factor(Session_ID)))

# Plot
ggplot(dat, aes(x = Utterance.ID, y = session_num, fill = AI_TeacherMove)) +
  geom_tile() +
  labs(
    title = "Teacher Moves Over Time by Session",
    x = "Utterance (Time)",
    y = "Session Number",
    fill = "Teacher Move"
  ) +
  scale_fill_manual(values = move_colors) +
  theme_minimal()

# Check column name
names(dat)

# Remove rows with missing or malformed Start.Time
dat <- dat %>%
  filter(!is.na(StartTime)) %>%
  filter(grepl("^\\d{2}:\\d{2}:\\d{2}$", StartTime))

# Now convert
dat$EndTime <- period_to_seconds(hms(dat$EndTime))
# Convert Start.Time to seconds
#dat$start_sec <- period_to_seconds(hms(dat$Start.Time))

# Calculate total session time
session_total <- dat %>%
  group_by(Session_ID) %>%
  summarise(session_total_time = max(EndTime, na.rm = TRUE))

# Merge total session time into dat
dat <- dat %>%
  left_join(session_total, by = "Session_ID") %>%
  mutate(percent_time = 100 * EndTime / session_total_time)

# Assign session number
dat <- dat %>%
  mutate(session_num = as.integer(factor(Session_ID)))

# Plot with percent of total time on x-axis
ggplot(dat, aes(x = percent_time, y = session_num, fill = AI_TeacherMove)) +
  geom_tile() +
  labs(
    title = "Teacher Moves Over Session Time (%)",
    x = "Percent of Session Time",
    y = "Session Number",
    fill = "Teacher Move"
  ) +
  scale_fill_manual(values = move_colors) +
  theme_minimal()
library(lubridate)
library(dplyr)
library(ggplot2)

# Remove rows with missing or malformed EndTime
dat <- dat %>%
  filter(!is.na(EndTime)) %>%
  filter(grepl("^\\d{2}:\\d{2}:\\d{2}$", EndTime))

# Convert EndTime to seconds
dat$EndTime_sec <- period_to_seconds(hms(dat$EndTime))

# Calculate total session time (max EndTime_sec per session)
session_total <- dat %>%
  group_by(Session_ID) %>%
  summarise(session_total_time = max(EndTime_sec, na.rm = TRUE))

# Merge and calculate percent time
dat <- dat %>%
  left_join(session_total, by = "Session_ID") %>%
  mutate(percent_time = 100 * EndTime_sec / session_total_time)

# Remove rows with NA or zero percent_time
dat <- dat %>%
  filter(!is.na(percent_time), percent_time > 0)

# Assign session number
dat <- dat %>%
  mutate(session_num = as.integer(factor(Session_ID)))

# Plot
ggplot(dat, aes(x = percent_time, y = session_num, fill = AI_TeacherMove)) +
  geom_tile() +
  labs(
    title = "Teacher Moves Over Session Time (%)",
    x = "Percent of Session Time",
    y = "Session Number",
    fill = "Teacher Move"
  ) +
  scale_fill_manual(values = move_colors) +
  theme_minimal()

ggplot(dat, aes(x = percent_time, y = session_num, fill = AI_TeacherMove)) +
geom_tile(aes(width = percent_time), height = 0.8) +  
labs(
    title = "Teacher Moves Over Session Time (%)",
    x = "Percent of Session Time",
    y = "Session Number",
    color = "Teacher Move"
  ) +
  scale_fill_manual(values = move_colors) +
  theme_minimal()

dat %>% select(AI_TeacherMove, session_num, session_total_time, percent_time) %>% head()


table(is.na(dat$EndTime_sec))
summary(dat$EndTime_sec)
summary(dat$percent_time)
library(stringr)

# Calculate total utterances per session
session_counts <- dat %>%
  group_by(Session_ID) %>%
  summarise(session_utterance_count = n())

# Merge with main data
dat <- dat %>%
  left_join(session_counts, by = "Session_ID")

# Plot: utterance grid by session and teacher move, faceted by session utterance count
ggplot(dat, aes(x = Utterance.ID, y = session_num, fill = AI_TeacherMove)) +
  geom_tile(width = 1, height = 1) +
  facet_wrap(~ session_utterance_count, labeller = label_both) +
  labs(
    title = "Teacher Moves by Utterance and Session (Faceted by Session Utterance Count)",
    x = "Utterance Number",
    y = "Session Number",
    fill = "Teacher Move"
  ) +
  scale_fill_manual(values = move_colors) +
  theme_minimal()

# Bin sessions into quartiles by utterance count
session_counts <- dat %>%
  group_by(Session_ID) %>%
  summarise(session_utterance_count = n()) %>%
  mutate(session_utterance_count_quartile = ntile(session_utterance_count, 4))

# Merge quartile info into main data
dat <- dat %>%
  left_join(session_counts %>% select(Session_ID, session_utterance_count_quartile), by = "Session_ID")

# Plot: utterance grid by session and teacher move, faceted by session utterance count quartile
ggplot(dat, aes(x = Utterance.ID, y = as.factor(session_num), fill = AI_TeacherMove)) +
  geom_tile(width = 1, height = 1) +
  facet_wrap(~ session_utterance_count_quartile, scales = "free", labeller = label_both) +
  labs(
    title = "Teacher Moves by Utterance and Session (Faceted by Session Utterance Count Quartile)",
    x = "Utterance Number",
    y = "Session Number",
    fill = "Teacher Move"
  ) +
  scale_fill_manual(values = move_colors) +
  theme_minimal()


  for (q in 1:4) {
  dat_q <- dat %>% filter(session_utterance_count_quartile == q)
  p <- ggplot(dat_q, aes(x = Utterance.ID, y = as.factor(session_num), fill = AI_TeacherMove)) +
    geom_tile(width = 1, height = 1) +
    labs(
      title = paste("Teacher Moves by Utterance and Session (Quartile", q, ")"),
      x = "Utterance Number",
      y = "Session Number",
      fill = "Teacher Move"
    ) +
    scale_fill_manual(values = move_colors) +
    theme_minimal()
  print(p)
}
str(p, 1)
for (q in 1:4) {
  dat_q <- dat %>% filter(session_utterance_count_quartile == q)
  p <- ggplot(dat_q, aes(x = Utterance.ID, y = as.factor(session_num), fill = AI_TeacherMove)) +
    geom_tile(width = 1, height = 1) +
    labs(
      title = paste("Teacher Moves by Utterance and Session (Quartile", q, ")"),
      x = "Utterance Number",
      y = "Session Number",
      fill = "Teacher Move"
    ) +
    scale_fill_manual(values = move_colors) +
    theme_minimal()
  print(p)  # This will display each plot separately in the R plotting window
  ggsave(paste0("/Users/kpv27/Documents/code/Research/Clustering_Exploration/05_tables/teacher_moves_quartile_", q, ".png"), plot = p, width = 10, height = 6)
}


