library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(tibble)


### JSON to CSV conversion and unnesting
parent_folder <- "Tutor_Profile_LAK25/02_data/Annotator_Runs/68c9a8c8ddf9c3336ce76f2c"

source("Tutor_Profile_LAK25/03_functions/read_transcripts.r")

all_data_unnested <- read_transcripts(parent_folder)

write.csv(all_data_unnested,
          "Tutor_Profile_LAK25/02_data/Annotator_Runs/Run1.csv",
          row.names = FALSE)

str(all_data_unnested)
table((all_data_unnested$teacherMove))

updated_ground_truth <- read.csv("Tutor_Profile_LAK25/02_data/updated_ground_truth.csv", stringsAsFactors = FALSE)
str(updated_ground_truth)


updated_ground_truth <- updated_ground_truth %>%
  group_by(Session_ID) %>%
  mutate(Utterance_ID = row_number() - 1) %>%
  ungroup()    

updated_ground_truth <- updated_ground_truth %>%
  mutate(
    Session_ID   = as.character(Session_ID),
    Utterance_ID = as.character(Utterance_ID)
  )

all_data_unnested <- all_data_unnested %>%
  mutate(
    Session_ID   = as.character(Session_ID),
    Utterance_ID = as.character(Utterance_ID)
  )

merged <- updated_ground_truth %>%
  left_join(all_data_unnested %>% select(Session_ID, Utterance_ID, teacherMove, reasoning),
            by = c("Session_ID", "Utterance_ID"))
str(merged)

table(merged$ground_truth)
table(merged$teacherMove)
table(merged$teacherMove== merged$ground_truth)
table(merged$teacherMove,  merged$ground_truth== "GIVING_HINT")
table(merged$teacherMove,  merged$ground_truth== "SCAFFOLDING")
table(merged$teacherMove,  merged$ground_truth== "EXEMPLIFYING")
table(merged$teacherMove,  merged$ground_truth== "REVOICING")


merged$teacherMove <- ifelse(merged$teacherMove == "GIVING_HINT" | merged$teacherMove== "EXEMPLIFYING" | merged$teacherMove== "REVOICING", "SCAFFOLDING", merged$teacherMove)
merged$ground_truth <- ifelse(merged$ground_truth == "GIVING_HINT" | merged$ground_truth== "EXEMPLIFYING" | merged$ground_truth== "REVOICING", "SCAFFOLDING", merged$ground_truth)


source("Tutor_Profile_LAK25/03_functions/compute_irr.r")
#Example usage:
out <- compute_irr(merged %>%filter(Role == 'TEACHER'), truth = ground_truth, pred = teacherMove)
out$overall
out$per_label
data.frame(out$per_label)
data.frame(out$per_label_kappa)
# out$confusion

out$per_label <- data.frame(out$per_label)

out$per_label <- out$per_label |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))%>% 
  select(label, f1, kappa)

out$per_label


#out$confusion

source("Tutor_Profile_LAK25/03_functions/extract_fp_fn_context.r")
ctx <- extract_fp_fn_context(merged, label = "PROVIDING_EXPLANATION", context = 4)
length(ctx$fp); length(ctx$fn)
as.data.frame(ctx$fp[[1]])  # a tibble with 4 before + focal + 4 after (when available)

 res <- export_fp_fn_context_xlsx(merged, label = "SCAFFOLDING",
                                  path = "Tutor_Profile_LAK25/02_data/Annotator_Runs/Run1_eval/fp_fn_examples_SCAFFOLDING.xlsx",
                                  context = 4)

res <- export_fp_fn_context_xlsx(merged, label = "EXEMPLIFYING",
                                  path = "Tutor_Profile_LAK25/02_data/Annotator_Runs/Run1_eval/fp_fn_examples_EXEMPLIFYING.xlsx",
                                  context = 4)

res <- export_fp_fn_context_xlsx(merged, label = "REVOICING",
                                  path = "Tutor_Profile_LAK25/02_data/Annotator_Runs/Run1_eval/fp_fn_examples_REVOICING.xlsx",
                                  context = 4)

res <- export_fp_fn_context_xlsx(merged, label = "SCAFFOLDING",
                                  path = "Tutor_Profile_LAK25/02_data/Annotator_Runs/Run1_eval/fp_fn_examples_SCAFFOLDING.xlsx",
                                  context = 4)

table(merged$ground_truth == merged$teacherMove)
table(is.na(updated_ground_truth$ground_truth))
table(is.na(all_data_unnested$teacherMove))
dim(merged)


###### RUN 2 #######
### JSON to CSV conversion and unnesting
parent_folder <- "Tutor_Profile_LAK25/02_data/Annotator_Runs/68cc31f9ceb0974104ca4405"


all_data_unnested <- read_transcripts(parent_folder)

write.csv(all_data_unnested,
          "Tutor_Profile_LAK25/02_data/Annotator_Runs/Run2.csv",
          row.names = FALSE)




updated_ground_truth <- updated_ground_truth %>%
  group_by(Session_ID) %>%
  mutate(Utterance_ID = row_number() - 1) %>%
  ungroup()    

updated_ground_truth <- updated_ground_truth %>%
  mutate(
    Session_ID   = as.character(Session_ID),
    Utterance_ID = as.character(Utterance_ID)
  )

all_data_unnested <- all_data_unnested %>%
  mutate(
    Session_ID   = as.character(Session_ID),
    Utterance_ID = as.character(Utterance_ID)
  )

merged <- updated_ground_truth %>%
  left_join(all_data_unnested %>% select(Session_ID, Utterance_ID, teacherMove, reasoning),
            by = c("Session_ID", "Utterance_ID"))
str(merged)


merged$teacherMove <- ifelse(merged$teacherMove == "GIVING_HINT" | merged$teacherMove== "EXEMPLIFYING" | merged$teacherMove== "REVOICING", "SCAFFOLDING", merged$teacherMove)
merged$ground_truth <- ifelse(merged$ground_truth == "GIVING_HINT" | merged$ground_truth== "EXEMPLIFYING" | merged$ground_truth== "REVOICING", "SCAFFOLDING", merged$ground_truth)

table(merged$ground_truth)
table(merged$teacherMove)
table(merged$teacherMove== merged$ground_truth)

source("Tutor_Profile_LAK25/03_functions/compute_irr.r")
#Example usage:
out <- compute_irr(merged %>%filter(Role == 'TEACHER'), truth = ground_truth, pred = teacherMove)
out$overall
out$per_label
data.frame(out$per_label)
data.frame(out$per_label_kappa)
# out$confusion

###### RUN 3 #######




### JSON to CSV conversion and unnesting
parent_folder <- "Tutor_Profile_LAK25/02_data/Annotator_Runs/68cc5c5287aae37ae869718c"
all_data_unnested <- read_transcripts(parent_folder)

write.csv(all_data_unnested,
          "Tutor_Profile_LAK25/02_data/Annotator_Runs/Run3.csv",
          row.names = FALSE)


#merged$teacherMove <- ifelse(merged$teacherMove == "GIVING_HINT" | merged$teacherMove== "EXEMPLIFYING" | merged$teacherMove== "REVOICING", "SCAFFOLDING", merged$teacherMove)



updated_ground_truth <- updated_ground_truth %>%
  group_by(Session_ID) %>%
  mutate(Utterance_ID = row_number() - 1) %>%
  ungroup()    

updated_ground_truth <- updated_ground_truth %>%
  mutate(
    Session_ID   = as.character(Session_ID),
    Utterance_ID = as.character(Utterance_ID)
  )

all_data_unnested <- all_data_unnested %>%
  mutate(
    Session_ID   = as.character(Session_ID),
    Utterance_ID = as.character(Utterance_ID)
  )

merged <- updated_ground_truth %>%
  left_join(all_data_unnested %>% select(Session_ID, Utterance_ID, teacherMove, reasoning),
            by = c("Session_ID", "Utterance_ID"))
str(merged)
merged$ground_truth <- ifelse(merged$ground_truth == "GIVING_HINT" | merged$ground_truth== "EXEMPLIFYING" | merged$ground_truth== "REVOICING", "SCAFFOLDING", merged$ground_truth)

table(merged$ground_truth)
table(merged$teacherMove)
table(merged$teacherMove== merged$ground_truth)

source("Tutor_Profile_LAK25/03_functions/compute_irr.r")
#Example usage:
out <- compute_irr(merged %>%filter(Role == 'TEACHER'), truth = ground_truth, pred = teacherMove)
out$overall
out$per_label
data.frame(out$per_label)
data.frame(out$per_label_kappa)
# out$confusion

###### RUN 4 #######

### JSON to CSV conversion and unnesting
parent_folder <- "Tutor_Profile_LAK25/02_data/Annotator_Runs/68cc65c0c44da10642b29fbf"
all_data_unnested <- read_transcripts(parent_folder)

write.csv(all_data_unnested,
          "Tutor_Profile_LAK25/02_data/Annotator_Runs/Run3.csv",
          row.names = FALSE)





updated_ground_truth <- updated_ground_truth %>%
  group_by(Session_ID) %>%
  mutate(Utterance_ID = row_number() - 1) %>%
  ungroup()    

updated_ground_truth <- updated_ground_truth %>%
  mutate(
    Session_ID   = as.character(Session_ID),
    Utterance_ID = as.character(Utterance_ID)
  )

all_data_unnested <- all_data_unnested %>%
  mutate(
    Session_ID   = as.character(Session_ID),
    Utterance_ID = as.character(Utterance_ID)
  )

merged <- updated_ground_truth %>%
  left_join(all_data_unnested %>% select(Session_ID, Utterance_ID, teacherMove, reasoning),
            by = c("Session_ID", "Utterance_ID"))
str(merged)
merged$ground_truth <- ifelse(merged$ground_truth == "GIVING_HINT" | merged$ground_truth== "EXEMPLIFYING" | merged$ground_truth== "REVOICING", "SCAFFOLDING", merged$ground_truth)
merged$teacherMove <- ifelse(merged$teacherMove == "GIVING_HINT" | merged$teacherMove== "EXEMPLIFYING" | merged$teacherMove== "REVOICING", "SCAFFOLDING", merged$teacherMove)

table(merged$ground_truth)
table(merged$teacherMove)
table(merged$teacherMove== merged$ground_truth)

source("Tutor_Profile_LAK25/03_functions/compute_irr.r")
#Example usage:
out <- compute_irr(merged %>%filter(Role == 'TEACHER'), truth = ground_truth, pred = teacherMove)
out$overall
out$per_label
data.frame(out$per_label)
data.frame(out$per_label_kappa)
# out$confusion


###### RUN 5 #######

### JSON to CSV conversion and unnesting
parent_folder <- "Tutor_Profile_LAK25/02_data/Annotator_Runs/68cc67a8c44da10642b2a6e6"
all_data_unnested <- read_transcripts(parent_folder)

write.csv(all_data_unnested,
          "Tutor_Profile_LAK25/02_data/Annotator_Runs/Run5.csv",
          row.names = FALSE)





updated_ground_truth <- updated_ground_truth %>%
  group_by(Session_ID) %>%
  mutate(Utterance_ID = row_number() - 1) %>%
  ungroup()    

updated_ground_truth <- updated_ground_truth %>%
  mutate(
    Session_ID   = as.character(Session_ID),
    Utterance_ID = as.character(Utterance_ID)
  )

all_data_unnested <- all_data_unnested %>%
  mutate(
    Session_ID   = as.character(Session_ID),
    Utterance_ID = as.character(Utterance_ID)
  )

merged <- updated_ground_truth %>%
  left_join(all_data_unnested %>% select(Session_ID, Utterance_ID, teacherMove, reasoning),
            by = c("Session_ID", "Utterance_ID"))
str(merged)
merged$ground_truth <- ifelse(merged$ground_truth == "GIVING_HINT" | merged$ground_truth== "EXEMPLIFYING" | merged$ground_truth== "REVOICING", "SCAFFOLDING", merged$ground_truth)
merged$teacherMove <- ifelse(merged$teacherMove == "GIVING_HINT" | merged$teacherMove== "EXEMPLIFYING" | merged$teacherMove== "REVOICING", "SCAFFOLDING", merged$teacherMove)

table(merged$ground_truth)
table(merged$teacherMove)
table(merged$teacherMove== merged$ground_truth)

source("Tutor_Profile_LAK25/03_functions/compute_irr.r")
#Example usage:
out <- compute_irr(merged %>%filter(Role == 'TEACHER'), truth = ground_truth, pred = teacherMove)
out$overall
out$per_label
data.frame(out$per_label)
data.frame(out$per_label_kappa)
# out$confusion

###### RUN 6 #######

### JSON to CSV conversion and unnesting
parent_folder <- "Tutor_Profile_LAK25/02_data/Annotator_Runs/68cc70ddc44da10642b2ae0e"
all_data_unnested <- read_transcripts(parent_folder)

write.csv(all_data_unnested,
          "Tutor_Profile_LAK25/02_data/Annotator_Runs/Run6.csv",
          row.names = FALSE)

updated_ground_truth <- updated_ground_truth %>%
  group_by(Session_ID) %>%
  mutate(Utterance_ID = row_number() - 1) %>%
  ungroup()    

updated_ground_truth <- updated_ground_truth %>%
  mutate(
    Session_ID   = as.character(Session_ID),
    Utterance_ID = as.character(Utterance_ID)
  )

all_data_unnested <- all_data_unnested %>%
  mutate(
    Session_ID   = as.character(Session_ID),
    Utterance_ID = as.character(Utterance_ID)
  ) 

merged <- updated_ground_truth %>%
  left_join(all_data_unnested %>% select(Session_ID, Utterance_ID, teacherMove, reasoning),
            by = c("Session_ID", "Utterance_ID")) %>% filter(Session_ID != `01801e36-4f2a-d484-204d-0928deb31994`)
str(merged)
merged$ground_truth <- ifelse(merged$ground_truth == "GIVING_HINT" | merged$ground_truth== "EXEMPLIFYING" | merged$ground_truth== "REVOICING", "SCAFFOLDING", merged$ground_truth)
merged$teacherMove <- ifelse(merged$teacherMove == "GIVING_HINT" | merged$teacherMove== "EXEMPLIFYING" | merged$teacherMove== "REVOICING", "SCAFFOLDING", merged$teacherMove)

table(merged$ground_truth)
table(merged$teacherMove)
table(merged$teacherMove== merged$ground_truth)

source("Tutor_Profile_LAK25/03_functions/compute_irr.r")
#Example usage:
out <- compute_irr(merged %>%filter(Role == 'TEACHER'), truth = ground_truth, pred = teacherMove)
out$overall
out$per_label
data.frame(out$per_label)
data.frame(out$per_label_kappa)
# out$confusion

###### RUN 67 #######

### JSON to CSV conversion and unnesting
parent_folder <- "Tutor_Profile_LAK25/02_data/Annotator_Runs/68cc9763c44da10642b2b537"
all_data_unnested <- read_transcripts(parent_folder)

write.csv(all_data_unnested,
          "Tutor_Profile_LAK25/02_data/Annotator_Runs/Run7.csv",
          row.names = FALSE)

updated_ground_truth <- updated_ground_truth %>%
  group_by(Session_ID) %>%
  mutate(Utterance_ID = row_number() - 1) %>%
  ungroup()    

updated_ground_truth <- updated_ground_truth %>%
  mutate(
    Session_ID   = as.character(Session_ID),
    Utterance_ID = as.character(Utterance_ID)
  )

all_data_unnested <- all_data_unnested %>%
  mutate(
    Session_ID   = as.character(Session_ID),
    Utterance_ID = as.character(Utterance_ID)
  ) 

merged <- updated_ground_truth %>%
  left_join(all_data_unnested %>% select(Session_ID, Utterance_ID, teacherMove, reasoning),
            by = c("Session_ID", "Utterance_ID")) %>% filter(Session_ID != `01801e36-4f2a-d484-204d-0928deb31994`)
str(merged)
merged$ground_truth <- ifelse(merged$ground_truth == "GIVING_HINT" | merged$ground_truth== "EXEMPLIFYING" | merged$ground_truth== "REVOICING", "SCAFFOLDING", merged$ground_truth)
merged$teacherMove <- ifelse(merged$teacherMove == "GIVING_HINT" | merged$teacherMove== "EXEMPLIFYING" | merged$teacherMove== "REVOICING", "SCAFFOLDING", merged$teacherMove)

table(merged$ground_truth)
table(merged$teacherMove)
table(merged$teacherMove== merged$ground_truth)

source("Tutor_Profile_LAK25/03_functions/compute_irr.r")
#Example usage:
out <- compute_irr(merged %>%filter(Role == 'TEACHER'), truth = ground_truth, pred = teacherMove)
out$overall
out$per_label
data.frame(out$per_label)
data.frame(out$per_label_kappa)
# out$confusion