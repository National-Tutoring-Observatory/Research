## packages
library(dplyr)
#install.packages("tidytext", dependencies = TRUE)
library(tidytext)
library(ggplot2)
#install.packages("patchwork")
library(patchwork)

## load data
dat <- read.csv("Clustering_Exploration/02_data/UPChieve_HumanAI_Annotations.csv", stringsAsFactors = FALSE)
str(dat)
unique(dat$Session.ID)

# For TEACHER
teacher_ngrams <- dat %>%
  filter(Role == "TEACHER") %>%
  unnest_tokens(trigram, Content, token = "ngrams", n = 3) %>%
  count(trigram, sort = TRUE) %>%
  slice_max(n, n = 10)

print(teacher_ngrams)

# Remove NA if present
teacher_ngrams <- teacher_ngrams[!is.na(teacher_ngrams$trigram), ]
ggplot(teacher_ngrams, aes(x = reorder(trigram, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top Teacher Trigrams", x = "Trigram", y = "Count") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    plot.title = element_text(size = 20)
  )

ggsave("Clustering_Exploration/04_figures/teacher_trigrams.png", width = 10, height = 6)


student_ngrams <- dat %>%
  filter(Role == "STUDENT_1") %>%
  unnest_tokens(trigram, Content, token = "ngrams", n = 3) %>%
  count(trigram, sort = TRUE) %>%
  slice_max(n, n = 10)

print(student_ngrams)

# Remove NA if present
student_ngrams <- student_ngrams[!is.na(student_ngrams$trigram), ]
ggplot(student_ngrams, aes(x = reorder(trigram, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top Student Trigrams", x = "Trigram", y = "Count") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    plot.title = element_text(size = 20)
  )

ggsave("Clustering_Exploration/04_figures/student_trigrams.png", width = 10, height = 6)

library(patchwork)

# Teacher plot
p_teacher <- ggplot(teacher_ngrams, aes(x = reorder(trigram, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top Teacher Trigrams", x = "Trigram", y = "Count") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    plot.title = element_text(size = 20)
  )

# Student plot
p_student <- ggplot(student_ngrams, aes(x = reorder(trigram, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top Student Trigrams", x = "Trigram", y = "Count") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    plot.title = element_text(size = 20)
  )

# Combine and save
combined_plot <- p_teacher / p_student
ggsave("Clustering_Exploration/04_figures/teacher_student_trigrams.png", combined_plot, width = 10, height = 12)



# For TEACHER
teacher_ngrams <- dat %>%
  filter(Role == "TEACHER") %>%
  unnest_tokens(trigram, Content, token = "ngrams", n = 3) %>%
  count(trigram, sort = TRUE) %>%
  slice_max(n, n = 5)


student_ngrams <- dat %>%
  filter(Role == "STUDENT_1") %>%
  unnest_tokens(trigram, Content, token = "ngrams", n = 3) %>%
  count(trigram, sort = TRUE) %>%
  slice_max(n, n = 5)



# Add Role column
teacher_ngrams$Role <- "Teacher"
student_ngrams$Role <- "Student"

teacher_ngrams <- teacher_ngrams[!is.na(teacher_ngrams$trigram), ]
student_ngrams <- student_ngrams[!is.na(student_ngrams$trigram), ]

# Combine
combined_ngrams <- rbind(teacher_ngrams, student_ngrams)


# Plot
ggplot(combined_ngrams, aes(x = reorder(trigram, n), y = n, fill = Role)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top Trigrams by Role", x = "Trigram", y = "Count") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    plot.title = element_text(size = 20)
  )

ggsave("Clustering_Exploration/04_figures/top_trigrams_by_role.png", width = 12, height = 6)
