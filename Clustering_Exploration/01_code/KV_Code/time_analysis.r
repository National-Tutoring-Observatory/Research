## packages
library(dplyr)
#install.packages("tidytext", dependencies = TRUE)
library(tidytext)
library(ggplot2)
#install.packages("patchwork")
library(patchwork)

## load data
dat <- read.csv("Clustering_Exploration/02_data/UPCHieve_Next100_noAnno.csv", stringsAsFactors = FALSE)
str(dat)
unique(dat$Session.ID)

library(dplyr)
library(lubridate)

    # Convert time to seconds
# Remove rows with missing or malformed Start.Time or End.Time
dat <- dat %>%
  filter(!is.na(Start.Time), !is.na(End.Time)) %>%
  filter(grepl("^\\d{2}:\\d{2}:\\d{2}$", Start.Time), grepl("^\\d{2}:\\d{2}:\\d{2}$", End.Time)) %>% 
      mutate(
        start_sec = period_to_seconds(hms(Start.Time)),
        end_sec = period_to_seconds(hms(End.Time)),
        duration = end_sec - start_sec
    )

# Total utterance time per session and role
role_time <- dat %>%
  group_by(Session.ID, Role) %>%
  summarise(role_total_time = sum(duration, na.rm = TRUE), .groups = "drop")

# Total session time
session_time <- dat %>%
  group_by(Session.ID) %>%
  summarise(session_total_time = sum(duration, na.rm = TRUE), .groups = "drop")

# Merge and calculate percentage
result <- role_time %>%
  left_join(session_time, by = "Session.ID") %>%
  mutate(percent_of_total = 100 * role_total_time / session_total_time)

library(tidyr)

result_wide <- result %>%
  pivot_wider(
    id_cols = Session.ID,
    names_from = Role,
    values_from = c(role_total_time, percent_of_total)
  )

str(result_wide)

print(result)

plot(result_wide$role_total_time_STUDENT_1, result_wide$role_total_time_TEACHER,
     xlab = "Percent of Total Time - TEACHER",
     ylab = "Percent of Total Time - STUDENT_1",
     main = "Role Time Distribution per Session"
)

hist(result_wide$role_total_time_STUDENT_1 + result_wide$role_total_time_TEACHER,
     xlab = "Total Time per Session",
     main = "Total Time Distribution per Session",
     xlim = c(0, 500),
     breaks = 10000
)
ggplot(result_wide, aes(x = role_total_time_TEACHER, y = role_total_time_STUDENT_1)) +
  geom_point(aes(size = percent_of_total_STUDENT_1) )+
  labs(title = "Role Time Distribution per Session",
       x = "Total Time - TEACHER",
       y = "Total Time - STUDENT") +
  theme_classic() +
coord_cartesian(xlim = c(0, 200), ylim = c(0, 200))


ggplot(result_wide, aes(x = role_total_time_TEACHER, y = role_total_time_STUDENT_1)) +
  # Shade below the diagonal (teacher speaks more)
  annotate("rect", xmin = 0, xmax = 200, ymin = 0, ymax = 200, 
           alpha = 0.15, fill = "lightblue") +
  # Shade above the diagonal (student speaks more)
  annotate("polygon", 
           x = c(0, 500, 500), 
           y = c(0, 0, 500), 
           fill = "lightpink", 
           alpha = 0.15) +
  annotate("polygon", 
           x = c(0, 0, 500), 
           y = c(0, 500, 500), 
           fill = "lightblue", 
           alpha = 0.15) +
  geom_point(aes(size = percent_of_total_STUDENT_1)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  annotate("text", x = 150, y = 50, label = "Teacher speaks more", color = "blue", size = 6) +
  annotate("text", x = 50, y = 150, label = "Student speaks more", color = "red", size = 6) +
  labs(title = "Role Time Distribution per Session",
       x = "Total Time - TEACHER",
       y = "Total Time - STUDENT") +
  theme_classic() +
    theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(0, 500), ylim = c(0, 500))
