# this script creates files for blind review of human vs AI teacher moves

## Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
source("Tutor_Profile_LAK25/03_functions/irr.r")
source("Tutor_Profile_LAK25/03_functions/disagree_blinded.r")


## load data 
dat <- read.csv("Tutor_Profile_LAK25/02_data/UPChieve_HumanAI_Annotations_2.csv", stringsAsFactors = FALSE)
str(dat)
length(unique(dat$Session_ID))
table(dat$AI_TeacherMove, dat$Role)
levels(as.factor(dat$AI_TeacherMove))
levels(as.factor(dat$Human_TeacherMove))

# create a pipl
dat_dif <- dat %>%
  select(Session_ID, Role, Content, Human_TeacherMove, AI_TeacherMove)
head(dat_dif)

dis<-disagree_blinded(data = dat_dif,
                                session_col = Session_ID,
                                role_col = Role,
                                content_col = Content,
                                human_col= Human_TeacherMove,
                                ai_col = AI_TeacherMove,
                                seed = 123,
                                keep_cols = NULL,
                                export_xlsx = "Tutor_Profile_LAK25/02_data/disagreement_blinded.xlsx",
                                export_key_xlsx = "Tutor_Profile_LAK25/02_data/disagreement_key.xlsx",
   highlight_method = "border",      # <- avoids black fill
   highlight_color = "#FFD966"
 )

                                
write.csv(dis$blinded, "Tutor_Profile_LAK25/02_data/disagreement_blinded.csv", row.names = FALSE)
write.csv(dis$key, "Tutor_Profile_LAK25/02_data/disagreement_blinded_key.csv", row.names = FALSE)
write.csv(dis$key, "Tutor_Profile_LAK25/02_data/disagreement_blinded_key.csv", row.names = FALSE)
str(dis$key)



# --- Usage ---
# out <- compare_two_reviewers(
#   reviewer1_xlsx = "blinded_reviewer_A.xlsx",
#   reviewer2_xlsx = "blinded_reviewer_B.xlsx",
#   key_xlsx = "blinded_key.xlsx",
#   reviewer1_id = "A",
#   reviewer2_id = "B"
# )
# out$combined_summary       # table of counts/props agreeing with HUMAN vs AI for each reviewer
# out$reviewer1$details      # per-row mapping (includes chosen_source)
# out$reviewer2$details
