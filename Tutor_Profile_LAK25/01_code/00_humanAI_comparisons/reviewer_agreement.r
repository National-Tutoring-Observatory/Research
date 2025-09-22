# this script creates files for blind review of human vs AI teacher moves

## Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
source("Tutor_Profile_LAK25/03_functions/compute_reviewer_agreement.r")
source("Tutor_Profile_LAK25/03_functions/write_jsonl_per_session.r")
source("Tutor_Profile_LAK25/03_functions/compare_two_reviewers.r")



# Example:
dat <- read.csv("Tutor_Profile_LAK25/02_data/UPChieve_HumanAI_Annotations_2.csv", stringsAsFactors = FALSE)
out <- compute_reviewer_agreement(
  #reviewer1_xlsx = "Tutor_Profile_LAK25/02_data/disagreement_blinded_jinsook.xlsx",
  reviewer_xlsx = "Tutor_Profile_LAK25/02_data/disagreement_blinded_sarah.xlsx",
  key_xlsx = "Tutor_Profile_LAK25/02_data/disagreement_key.xlsx",
reviewer_sheet = "Blinded",
                                       key_sheet = "Key",
                                       reviewer_id = "Reviewer",
                                       decision_col = "Decision",
  original_df = dat
)
as.data.frame(out$summary)
table(is.na(out$reviewer2$updated_ground_truth$ground_truth))

write.csv(out$reviewer2$updated_ground_truth, "Tutor_Profile_LAK25/02_data/updated_ground_truth.csv", row.names = FALSE)

out$combined_summary

# not in thr right formate
write_jsonl_per_session(out$reviewer2$updated_ground_truth, out_dir = "Tutor_Profile_LAK25/02_data/jsonl_sessions")
