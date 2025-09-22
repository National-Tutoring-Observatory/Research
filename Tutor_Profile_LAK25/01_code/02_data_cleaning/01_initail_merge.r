source("Tutor_Profile_LAK25/03_functions/merge_tutoring_files.r")


merge_tutoring_files(
  utter_csv_path = "Tutor_Profile_LAK25/02_data/UPChieve_LAK_data/UPCHieve_Next100_noAnno.csv",
  jsonl1_path = "Tutor_Profile_LAK25/02_data/UPChieve_LAK_data/outcomes_analysis.jsonl",
  jsonl2_path ="Tutor_Profile_LAK25/02_data/UPChieve_LAK_data/raw_sessions.jsonl",
  out_csv_path = "Tutor_Profile_LAK25/02_data/UPChieve_LAK_data/merged_Upcheive_next100.csv"
) 

dat_merged <- read.csv("Tutor_Profile_LAK25/02_data/UPChieve_LAK_data/merged_Upcheive_next100.csv", stringsAsFactors = FALSE)

str(dat_merged)
colnames(dat_merged)

session_level <- dat_merged %>%
  group_by(session_id) %>%
  mutate(
    n_utterances = n(),
    n_student_utterances = sum(role == "STUDENT"),
    n_tutor_utterances   = sum(role == "TEACHER"),
    avg_student_utterance_length = mean(nchar(content[role == "STUDENT"])),
    avg_tutor_utterance_length   = mean(nchar(content[role == "TEACHER"]))
  ) %>%
  select(-role, -utterance_id, -content, -start_time, -end_time) %>% 
  distinct()

write.csv(session_level, "Tutor_Profile_LAK25/02_data/UPChieve_LAK_data/student_session_level_data.csv", row.names = FALSE)
dim(student_level)

length(unique(student_level$session_id))
