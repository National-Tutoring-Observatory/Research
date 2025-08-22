


# Most likely state per row (Viterbi-like decode via argmax posterior)
dat_HM$state_hat <- apply(post[ , paste0("S", 1:best_k)], 1, which.max)
head(dat_HM)



dat_HM$state_label <- state_labels[as.character(dat_HM$state_hat)]

# Check
head(dat_HM[, c("Session_ID","Utterance.ID","AI_TeacherMove","state_hat","state_label")])




# ---- Visualize ----
ggplot(dat_HM, aes(x = Utterance.ID, y = state_label, group = Session_ID)) +
  geom_step() +
  facet_wrap(~ Session_ID, scales = "free_x") +
  labs(title = "Hidden State Sequence per Session",
       x = "Utterance index", y = "Hidden State") +
  theme_minimal()



# posterior() output has probs in columns S1...Sk
post_probs <- post[, paste0("S", 1:best_k)]

# Rename columns with descriptive labels
colnames(post_probs) <- c("Scaffolding mode", "Correction mode", "Support mode")

# Add them to your main dataframe
dat_HM <- cbind(dat_HM, post_probs)

# Also keep the most likely state with label
dat_HM$state_label <- colnames(post_probs)[max.col(post_probs, ties.method = "first")]



# Check
head(dat_HM[, c("Session_ID","Utterance.ID","AI_TeacherMove",
                "Scaffolding mode","Correction mode","Support mode","state_label")])


# Pivot probabilities into long format
plotdat <- dat_HM %>%
  dplyr::select(Session_ID, Utterance.ID,
         `Scaffolding mode`, `Correction mode`, `Support mode`) %>%
  pivot_longer(cols = c(`Scaffolding mode`, `Correction mode`, `Support mode`),
               names_to = "state", values_to = "probability")

# Line plot: probability trajectories by state within each session
ggplot(plotdat, aes(x = Utterance.ID, y = probability, color = state)) +
  geom_line(size = 1) +
  facet_wrap(~ Session_ID, scales = "free_x") +
  labs(title = "Hidden state probabilities across sessions",
       x = "Utterance index", y = "Posterior probability") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")


# ---- (Optional) save outputs ----
# write.csv(metrics, "hmm_model_selection_metrics.csv", row.names = FALSE)
# write.csv(dat_HM[, c("Session_ID","Utterance.ID","AI_TeacherMove","state_hat")],
#           "hmm_posterior_states.csv", row.names = FALSE)

# ---- packages ----
remove.packages("rgl")
install.packages("rgl")  # or install.packages("rgl", type = "source")
library(rgl)
library(kml3d)

library(dplyr)
library(tidyr)
library(purrr)
install.packages("kml3d", dependencies = TRUE)
library(kml3d)      
install.packages("longitudinalData")      # uses longitudinalData under the hood
library(longitudinalData)

# Input assumptions:
# dat_HM has columns: Session_ID, Utterance.ID, 
# `Scaffolding mode`, `Correction mode`, `Support mode`

# ---- 1) Resample each session to a common length L ----
L <- 50  # target length per session (adjust as needed)

resample_session <- function(df, L = 50) {
  df <- df %>% arrange(Utterance.ID)
  t_in  <- seq_along(df$Utterance.ID)
  t_out <- seq(1, length(t_in), length.out = L)

  tibble(
    Session_ID = df$Session_ID[1],
    t = seq_len(L),
    `Scaffolding mode` = approx(t_in, df$`Scaffolding mode`, xout = t_out, rule = 2)$y,
    `Correction mode`  = approx(t_in, df$`Correction mode`,  xout = t_out, rule = 2)$y,
    `Support mode`     = approx(t_in, df$`Support mode`,     xout = t_out, rule = 2)$y
  )
}

traj_df <- dat_HM %>%
  dplyr::select(Session_ID, Utterance.ID, `Scaffolding mode`, `Correction mode`, `Support mode`) %>%
  group_by(Session_ID) %>%
  group_split() %>%
  map_df(resample_session, L = L)

# ---- 2) Build 3D longitudinal object (subjects x time x variables) ----
# array shape: [nSubjects, L, 3]
ids <- unique(traj_df$Session_ID)
n   <- length(ids)

arr <- traj_df %>%
  mutate(Session_ID = factor(Session_ID, levels = ids)) %>%
  arrange(Session_ID, t) %>%
  dplyr::select(`Scaffolding mode`, `Correction mode`, `Support mode`) %>%
  as.matrix() %>%
  array(dim = c(n, L, 3))

ld3d <- longData3d(
  traj      = arr,
  idAll     = ids,
  time      = 1:L,
  varNames  = c("Scaffolding", "Correction", "Support")
)

# ---- 3) Run longitudinal k-means (kml3d) across K = 2..6 clusters ----
set.seed(123)
kml3d(ld3d, nbClusters = 2:6, nbRedrawing = 20, toPlot = "none")

# Inspect quality criteria and choose K (Calinski-Harabasz, etc.)
# plotAllCriterion(ld3d)   # optional: opens a GUI plot window

# Pick the best K (example: choose the partition with max Calinski-Harabasz)
crit <- getClustersCriterion(ld3d)
bestK <- as.integer(names(which.max(crit["calinski", ])))

# ---- 4) Extract cluster labels for bestK ----
cl_lab <- getClusters(ld3d, bestK)

cluster_map <- tibble(Session_ID = ids, cluster_kml = as.integer(cl_lab))
cluster_map

# ---- 5) (Optional) Join clusters back to the resampled trajectories and plot ----
plot_df <- traj_df %>%
  left_join(cluster_map, by = "Session_ID") %>%
  pivot_longer(c(`Scaffolding mode`, `Correction mode`, `Support mode`),
               names_to = "state", values_to = "prob")

library(ggplot2)
ggplot(plot_df, aes(x = t, y = prob, color = state)) +
  geom_line(alpha = 0.8) +
  facet_wrap(~ cluster_kml, ncol = bestK, scales = "free_y") +
  labs(title = sprintf("kml3d clustering of session state-prob trajectories (K = %d)", bestK),
       x = "Standardized utterance index (1..L)", y = "Probability") +
  theme_minimal()

# --- Clean reinstall (align versions) ---
# If needed: install.packages("remotes")
library(remotes)

remove.packages(c("kml3d","kml","longitudinalData"))

# Install known-compatible versions
install_version("longitudinalData", version = "2.6.1")
install_version("kml",              version = "2.5.0")
install_version("kml3d",            version = "2.5.1")   # ok with 2.5.0 too

# --- Load (headless rgl so no XQuartz needed for compute) ---
options(rgl.useNULL = TRUE); Sys.setenv(RGL_USE_NULL = "true")
library(longitudinalData)
library(kml3d)

# --- Rebuild ld3d (if not in memory) ---
# ld3d <- longData3d(traj = arr, idAll = ids, time = 1:L,
#                    varNames = c("Scaffolding","Correction","Support"))

# --- Run kml3d ---
set.seed(123)
kml3d(ld3d, nbClusters = 2:6, nbRedrawing = 20, toPlot = "none")

# Pick best K and extract labels
crit  <- getClustersCriterion(ld3d)
bestK <- as.integer(names(which.max(crit["calinski", ])))
labs  <- getClusters(ld3d, bestK)

cluster_map <- data.frame(Session_ID = slot(ld3d, "idAll"), cluster_kml = as.integer(labs))
cluster_map
