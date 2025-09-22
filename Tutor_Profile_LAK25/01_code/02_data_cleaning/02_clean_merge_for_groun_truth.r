
# This script merges three datasets (two JSON, one CSV) based on session ID
# and exports the result to a new CSV file.

# Load necessary libraries. You may need to install them with install.packages()
# if you haven't already.
library(jsonlite)
library(readr)
library(dplyr)
library(stringr)

# --- Define File Paths ---
# Please replace these with the actual paths to your files.
sessions_json_path <- "/Users/kpv27/Documents/code/Research/Tutor_Profile_LAK25/02_data/outcomes_analysis.jsonl"
student_sessions_json_path <- "/Users/kpv27/Documents/code/Research/Tutor_Profile_LAK25/02_data/student_meta.jsonl"
session_dialog_csv_path <- "02_data/UPChieve_HumanAI_Annotations_2.csv"
output_csv_path <- "02_data/merged_Upcheive_first_30sessions.csv"

# --- Read and Process Sessions Data (JSON 1) ---
# This file is newline-delimited JSON. We read it line by line and then bind them.
sessions_data <- bind_rows(lapply(read_lines(sessions_json_path), fromJSON, flatten = TRUE))
sessions_df <- as_tibble(sessions_data)

# --- Read and Process Student Sessions Data (JSON 2) ---
# This file is also newline-delimited JSON. We read it line by line.
student_sessions_data <- bind_rows(lapply(read_lines(student_sessions_json_path), fromJSON, flatten = TRUE))
student_df <- as_tibble(student_sessions_data)

# --- Read and Process Session Dialog Data (CSV) ---
# The CSV is a standard format that can be read directly.
dialog_df <- read_csv(session_dialog_csv_path)

# Standardize the session ID column name to match the other data frames
dialog_df <- dialog_df %>%
  rename(session_id = Session_ID)

# --- Merge the Data Frames ---
# Start by merging the sessions data with the student sessions data.
# We'll use a left join to keep all records from the main sessions data.
merged_data <- left_join(sessions_df, student_df, by = "session_id", suffix = c(".sessions", ".students"))

# Now, join the dialog data. This is a one-to-many relationship, so the number of
# rows will increase to include every utterance from the dialog.
merged_data <- left_join(merged_data, dialog_df, by = "session_id", suffix = c(".sessions_students", ".dialog"))

# --- Clean and Finalize the Merged Data ---
# Remove any columns you don't need after merging to clean up the dataset.
# For example, we'll remove the redundant subject_name column from the second JSON file
# since it's already in the first one.
merged_data <- merged_data %>%
  select(-subject_name.students, -topic_name.students)

# You can add more data cleaning steps here as needed, such as
# converting data types or handling missing values.

# --- Export to CSV ---
# Write the final merged data frame to a new CSV file.
write_csv(merged_data, output_csv_path)

# Print a confirmation message to the console.
cat("Successfully merged all data and saved to", output_csv_path, "\n")


colnames(merged_data)

## review data
# check NAs ffor each column
sapply(merged_data, function(x) sum(is.na(x)))

unique(merged_data$subject_name.sessions)
class(merged_data$created_at)
class(merged_data$ended_at)
library(dplyr)
library(lubridate)

merged_data <- merged_data %>%
  mutate(
    created_at = ymd_hms(created_at, tz = "UTC"),
    ended_at   = ymd_hms(ended_at, tz = "UTC"),
    duration_minutes = as.numeric(difftime(ended_at, created_at, units = "mins"))
  )

mean(merged_data$duration_minutes)
sd(merged_data$duration_minutes)
quantile(merged_data$duration_minutes)

# number of unique sessions
length(unique(merged_data$session_id))
# number of unique students
length(unique(merged_data$student_id))


next100 <- read.csv("02_data/UPCHieve_Next100_noAnno.csv")
write.csv(SessionIDs,"02_data/Next100_SessionIDs.csv",row.names = FALSE)


length(next100$Role)

session_counts <- next100 %>%
  group_by(Session.ID) %>%
  summarise(row_count = n()) %>%
  arrange(desc(row_count))

mean(session_counts$row_count)
sd(session_counts$row_count)
range(session_counts$row_count)


library(dplyr)
library(dplyr)
library(stringr)

role_stats <- next100 %>%
  mutate(
    Role = case_when(
      str_detect(tolower(Role), "student") ~ "student",
      str_detect(tolower(Role), "teacher|tutor") ~ "teacher",
      TRUE ~ Role
    )
  ) %>%
  group_by(Role, Session.ID) %>%
  summarise(row_count = n(), .groups = "drop") %>%
  group_by(Role) %>%
  summarise(
    mean_rows = mean(row_count),
    sd_rows = sd(row_count),
    q25 = quantile(row_count, 0.25),
    median = quantile(row_count, 0.5),
    q75 = quantile(row_count, 0.75),
    min_rows = min(row_count),
    max_rows = max(row_count),
    .groups = "drop"
  )

role_stats


library(dplyr)

utterance_counts <- next100 %>%
  mutate(
    Role = case_when(
      grepl("student", Role, ignore.case = TRUE) ~ "student",
      grepl("teacher|tutor", Role, ignore.case = TRUE) ~ "teacher",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Role)) %>%
  group_by(Role, Session.ID) %>%
  summarise(row_count = n(), .groups = "drop")

t_test_result <- t.test(row_count ~ Role, data = utterance_counts)
t_test_result
