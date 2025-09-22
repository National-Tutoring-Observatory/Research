



## load data 
dat <- read.csv("Tutor_Profile_LAK25/02_data/UPChieve_LAK_data/annotated_utterances_for_analysis.csv", stringsAsFactors = FALSE)
str(dat)
length(unique(dat$session_id))
IDs<-as.data.frame((unique(dat$session_id)))
# write.csv(IDs, "Tutor_Profile_LAK25/05_tables/session_ids.csv", row.names = FALSE)
# table(dat$teacherMove, dat$Role)
# levels(as.factor(dat$teacherMove))

###################################################
## Prepare data for Hidden Markov model ###########
###################################################

# Recode empty teacherMove based on Role
# dat$teacherMove[dat$teacherMove == "" & dat$Role == "TEACHER"] <- "UNLABLED_TEACHER_CHAT"
# dat$teacherMove[dat$teacherMove == "" & dat$Role == "STUDENT_1"] <- "STUDENT_CHAT"
table(dat$teacherMove)


dat_HM <- dat %>%  
    filter(teacherMove != "") %>% # this drops both unspecified teacher moves and student chat
    mutate(teacherMove = factor(teacherMove, levels = c(
         "EMOTIONAL_SUPPORT",
        "ERROR_CORRECTION", 
       # "EXEMPLIFYING",
       # "GIVING_HINT", 
        "GIVING_PRAISE",
        "PROBING_STUDENT_THINKING", 
        "PROMPTING",
        "PROVIDING_EXPLANATION", 
       # "REVOICING",
        "SCAFFOLDING", 
        "STUDENT_CHAT"
      # , "UNLABLED_TEACHER_CHAT", "USING_VISUAL_CUES"
    ))) %>%
    dplyr::select(session_id, utterance_id, teacherMove, content)


best_fit <- readRDS("Tutor_Profile_LAK25/06_models/HHM.rds")

post <- posterior(best_fit, type = "viterbi")
head(post)

state_labels <- c(
  "1" = "Thought Elicitation",
  "2" = "Guided Explanation",
  "3" = "Encorgaging Feedback"
)

# Ensure ordering within and across sessions
dat_HM <- dat_HM[order(dat_HM$session_id, dat_HM$utterance_id), ]

# Factorize observed categories
dat_HM$teacherMove <- factor(dat_HM$teacherMove)

# ntimes: length of each session in the (now) ordered data
session_lengths <- as.numeric(table(dat_HM$session_id))

# Most likely state per row (Viterbi-like decode via argmax posterior)
dat_HM$state_hat <- apply(post[ , paste0("S", 1:3)], 1, which.max)
head(dat_HM)



dat_HM$state_label <- state_labels[as.character(dat_HM$state_hat)]

# Check
head(dat_HM[, c("session_id","utterance_id","teacherMove","state_hat","state_label")])

best_k <- 3


# ---- Visualize ----
ggplot(dat_HM, aes(x = utterance_id, y = state_label, group = session_id)) +
  geom_step() +
  facet_wrap(~ session_id, scales = "free_x") +
  labs(title = "Hidden State Sequence per Session",
       x = "Utterance index", y = "Hidden State") +
  theme_minimal()



# posterior() output has probs in columns S1...Sk
post_probs <- post[, paste0("S", 1:best_k)]

# Rename columns with descriptive labels
colnames(post_probs) <- state_labels <- c(
  "Thought Elicitation",
   "Guided Explanation",
   "Encouraging Feedback"
)
#colnames(post_probs)
# Add them to your main dataframe
dat_HM <- cbind(dat_HM, post_probs)

# Also keep the most likely state with label
dat_HM$state_label <- colnames(post_probs)[max.col(post_probs, ties.method = "first")]



# Check
head(dat_HM[, c("session_id","utterance_id","teacherMove",
                "Thought Elicitation",
   "Guided Explanation",
   "Encouraging Feedback","state_label")])


# Pivot probabilities into long format
plotdat <- dat_HM %>%
  dplyr::select(session_id, utterance_id,
         `Thought Elicitation`, `Guided Explanation`, `Encouraging Feedback`) %>%
  pivot_longer(cols = c(`Thought Elicitation`, `Guided Explanation`, `Encouraging Feedback`),
               names_to = "state", values_to = "probability")

# Line plot: probability trajectories by state within each session
ggplot(plotdat, aes(x = utterance_id, y = probability, color = state)) +
  geom_line(size = 1) +
  facet_wrap(~ session_id, scales = "free_x") +
  labs(title = "Hidden state probabilities across sessions",
       x = "Utterance index", y = "Posterior probability") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")


# ---- (Optional) save outputs ----
# write.csv(metrics, "hmm_model_selection_metrics.csv", row.names = FALSE)
# write.csv(dat_HM[, c("session_id","utterance_id","teacherMove","state_hat")],
#           "hmm_posterior_states.csv", row.names = FALSE)

# ---- packages ----
#remove.packages("rgl")
#install.packages("rgl")  # or install.packages("rgl", type = "source")
library(rgl)
library(kml3d)

library(dplyr)
library(tidyr)
library(purrr)
#install.packages("kml3d", dependencies = TRUE)
library(kml3d)      
#install.packages("longitudinalData")      # uses longitudinalData under the hood
library(longitudinalData)

# Input assumptions:
library(dplyr)
library(purrr)
library(tidyr)

# rank-based resampling to L steps per session
resample_by_rank <- function(df, L = 50) {
  stopifnot(all(c("session_id", "utterance_id") %in% names(df)))
  state_cols <- setdiff(names(df), c("session_id", "utterance_id"))

  # ensure ordering by utterance_id
  df <- df %>% arrange(utterance_id)

  # relative positions for existing utterances and target grid
  rel_pos <- if (nrow(df) == 1) 0 else seq(0, 1, length.out = nrow(df))
  target  <- if (L == 1) 0 else seq(0, 1, length.out = L)

  resampled <- map_dfc(state_cols, function(col) {
    y <- df[[col]]
    ok <- !is.na(y)
    if (!any(ok)) {
      rep(NA_real_, L)
    } else if (length(unique(rel_pos[ok])) == 1) {
      rep(y[ok][1], L)
    } else {
      approx(x = rel_pos[ok], y = y[ok], xout = target, rule = 2, ties = "ordered")$y
    }
  })

  tibble(
    session_id   = df$session_id[[1]],
    step         = seq_len(L)
  ) %>% bind_cols(setNames(resampled, state_cols))
}

# optional gate: keep sessions with any signal across state cols
has_signal <- function(df) {
  state_cols <- setdiff(names(df), c("session_id", "utterance_id"))
  any(rowSums(!is.na(df[state_cols])) > 0)
}

# columns to include
state_cols <- c("Thought Elicitation", "Guided Explanation", "Encouraging Feedback")

# pipeline
L <- 50  # set desired standardized length
dat_HM <- as_tibble(dat_HM)
traj_df <- dat_HM %>%
  dplyr::select(session_id, utterance_id, all_of(state_cols)) %>%
  group_by(session_id) %>%
  group_split() %>%
  keep(has_signal) %>%
  map_df(resample_by_rank, L = L)

# NAs per column
colSums(is.na(dat_HM))

# ---- 2) Build 3D longitudinal object (subjects x time x variables) ----
# array shape: [nSubjects, L, 3]
ids <- unique(traj_df$session_id)
n   <- length(ids)



# define ids, n, and L
ids <- traj_df %>% distinct(session_id) %>% pull()
n   <- length(ids)
L   <- max(traj_df$step)

# build 3D array [n sessions, L steps, 3 states]
arr <- traj_df %>%
  mutate(session_id = factor(session_id, levels = ids)) %>%
  arrange(session_id, step) %>%
  dplyr::select(`Thought Elicitation`, `Guided Explanation`, `Encouraging Feedback`) %>%
  as.matrix() %>%
  array(
    dim = c(n, L, 3),
    dimnames = list(
      session = ids,
      step    = as.character(seq_len(L)),
      state   = c("Thought Elicitation", "Guided Explanation", "Encouraging Feedback")
    )
  )

library(kml3d)

# Build the right object class for kml3d (ClusterLongData3d, not LongData)
ld3d <- clusterLongData3d(
  traj     = arr,                                        # [n x L x 3]
  idAll    = as.character(ids),
  time     = seq_len(dim(arr)[2]),
  varNames = c("Thought Elicitation", "Guided Explanation", "Encouraging Feedback")
)

set.seed(123)
kml3d(ld3d, nbClusters = 2:6, nbRedrawing = 20)




# Example: extract 3-cluster assignment and quality
cl_k3   <- getClusters(ld3d, 3)

# Partition for a given K
cl_k3 <- getClusters(ld3d, 3)

# Summary of clustering quality
 print(ld3d)   # shows criteria values (Calinski, Davies-Bouldin, etc.)

# Plot trajectories by cluster
plot(ld3d, )  





library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(methods)

# --- parse helpers -----------------------------------------------------------
cap_show <- function(x) capture.output(show(x))

parse_metric <- function(lines, patterns) {
  ln <- lines[Reduce(`|`, lapply(patterns, function(p) grepl(p, lines, ignore.case = TRUE)))]
  if (!length(ln)) return(NA_real_)
  as.numeric(str_trim(sub(".*=\\s*", "", ln[1])))
}

parse_partition_metrics <- function(part_obj) {
  z  <- cap_show(part_obj)
  tibble(
    Calinski = parse_metric(z, c("Calinski\\s*&\\s*Harab(a|)tz", "Calinski\\s*$")),
    DB       = parse_metric(z, c("Davies\\s*&\\s*Bouldin")),
    BIC      = parse_metric(z, c("^\\s*-?\\s*BIC:\\s")),
    AIC      = parse_metric(z, c("^\\s*-?\\s*AIC:\\s"))
  )
}

# --- extract all redrawings across K ----------------------------------------
extract_kml3d_criteria <- function(ld3d) {
  c_slots <- grep("^c\\d+$", slotNames(ld3d), value = TRUE)
  bind_rows(lapply(c_slots, function(cs) {
    parts <- methods::slot(ld3d, cs)
    if (!is.list(parts) || !length(parts)) return(NULL)
    K <- as.integer(sub("^c", "", cs))
    bind_rows(lapply(seq_along(parts), function(i) {
      parse_partition_metrics(parts[[i]]) %>%
        mutate(K = K, redraw = i)
    }))
  })) %>% relocate(K, redraw)
}

# --- run ---------------------------------------------------------------------
crit <- extract_kml3d_criteria(ld3d)

crit_summary <- crit %>%
  group_by(K) %>%
  summarise(
    nRedraw  = n(),
    meanCH   = mean(Calinski, na.rm = TRUE),
    medianCH = median(Calinski, na.rm = TRUE),
    maxCH    = max(Calinski, na.rm = TRUE),
    meanDB   = mean(DB, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  arrange(desc(meanCH))

crit_summary

# --- visuals -----------------------------------------------------------------
# Boxplot of CH by K
ggplot(crit, aes(factor(K), Calinski)) +
  geom_boxplot() +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.3, size = 1) +
  labs(x = "K", y = "Calinski–Harabasz (higher = better)") +
  theme_bw(base_size = 14)

# Line plot of mean CH by K (+ points)
ggplot(crit_summary, aes(K, meanCH)) +
  geom_line() +
  geom_point(size = 2) +
  labs(x = "K", y = "Mean Calinski–Harabasz") +
  scale_x_continuous(breaks = crit_summary$K) +
  theme_bw(base_size = 14)

# Choose best K by mean CH (tie -> smaller K)
best_K <- crit_summary %>%
  filter(meanCH == max(meanCH, na.rm = TRUE)) %>%
  slice_min(K, n = 1) %>%
  pull(K)

best_K
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

# inputs: arr [n x L x p], ld3d (ClusterLongData3d), K <- 4
clusters <- getClusters(ld3d, 4)
vn <- ld3d@varNames
L  <- dim(arr)[2]
p  <- dim(arr)[3]
cls_ids   <- sort(unique(clusters))
cls_names <- paste0("Cluster_", cls_ids)

# centers: L x K per variable, with named columns to avoid tidyselect collisions
centers <- map(seq_len(p), function(j) {
  mat <- sapply(cls_ids, function(k) {
    colMeans(arr[clusters == k, , j, drop = FALSE], na.rm = TRUE)
  })
  colnames(mat) <- cls_names
  tibble(step = seq_len(L), variable = vn[j]) |>
    bind_cols(as_tibble(mat))
})

df_centers <- bind_rows(centers, .id = "var_id") |>
  pivot_longer(
    cols = all_of(cls_names),
    names_to = "cluster",
    values_to = "value"
  ) |>
  mutate(cluster = factor(cluster, levels = cls_names, labels = paste("Cluster", cls_ids)))

# optional smoothing
smooth_span <- 0.25
df_smooth <- df_centers |>
  group_by(variable, cluster) |>
  arrange(step, .by_group = TRUE) |>
  mutate(value = predict(loess(value ~ step, span = smooth_span))) |>
  ungroup()

ggplot(df_smooth, aes(step, value, color = cluster)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ variable, ncol = p, scales = "fixed") +
  labs(x = "Standardized session step", y = "Average probability",
       title = "Cluster mean trajectories (K = 4)") +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text = element_text(face = "bold")
  )


# Assumes:
# - arr: [n_sessions x L_steps x p_states] with probabilities
# - ids: session IDs in same order as arr dim 1
# - clusters: vector of length n_sessions with cluster id (1..K)
# - vn: state names (length p), e.g. c("Thought Elicitation","Guided Explanation","Encouraging Feedback")
# - centers: list length p; each is [L x K] (from previous step)
# - K, L, p, n are defined

library(dplyr)
library(tidyr)
library(purrr)

# 1) Hard state per session & step (argmax across states)
hard_id <- apply(arr, c(1,2), function(v) {
  if (all(is.na(v))) return(NA_integer_)
  which.max(replace(v, is.na(v), -Inf))
})
hard_state <- matrix(vn[hard_id], nrow = dim(arr)[1], ncol = dim(arr)[2])

# long form with cluster labels
hard_long <- tibble(
  session = rep(ids, each = L),
  step    = rep(seq_len(L), times = length(ids)),
  state_i = as.integer(hard_id)
) %>%
  mutate(
    cluster = clusters[match(session, ids)],
    state   = ifelse(is.na(state_i), NA_character_, vn[state_i])
  )

# 2) Dominant (modal) state per cluster-time step
mode_int <- function(x, nbins) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_integer_)
  tab <- tabulate(x, nbins)
  which.max(tab)
}

cluster_dominant <- hard_long %>%
  group_by(cluster, step) %>%
  summarise(state_i = mode_int(state_i, p), .groups = "drop") %>%
  mutate(state = ifelse(is.na(state_i), NA_character_, vn[state_i]))

# 3) From cluster centroids only: dominant state per step (argmax over states)
dom_from_centers <- map_dfr(seq_len(K), function(k) {
  mat <- cbind(centers[[1]][, k], centers[[2]][, k], centers[[3]][, k])  # [L x p]
  idx <- max.col(mat, ties.method = "first")
  tibble(cluster = k, step = seq_len(L), state_i = idx, state = vn[idx])
})

# 4) Optional: wide labels per session (n x L matrix of strings)
hard_labels_wide <- matrix(hard_long$state, nrow = length(ids), ncol = L, byrow = TRUE,
                           dimnames = list(ids, paste0("t", seq_len(L))))

# 5) Optional: quick plot of dominant state by cluster
# install.packages("ggplot2") if needed
library(ggplot2)
ggplot(dom_from_centers, aes(step, factor(state, levels = vn), color = factor(cluster))) +
  geom_point(alpha = 0.2) +
  geom_line(aes(group = cluster), linewidth = 2, alpha = 0.4 )+
  scale_y_discrete(NULL) +
  labs(x = "Standardized step", color = "Cluster",
       title = "Dominant (highest-probability) state by cluster over time")
