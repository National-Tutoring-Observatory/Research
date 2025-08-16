## load data
dat <- read.csv("Clustering_Exploration/02_data/UPChieve_HumanAI_Annotations.csv", stringsAsFactors = FALSE)
str(dat)
length(unique(dat$Session_ID))
table(dat$AI_TeacherMove, dat$Role)
levels(as.factor(dat$AI_TeacherMove))


# # Recode empty AI_TeacherMove based on Role
# dat$AI_TeacherMove[dat$AI_TeacherMove == "" & dat$Role == "TEACHER"] <- "UNLABLED_TEACHER_CHAT"
# dat$AI_TeacherMove[dat$AI_TeacherMove == "" & dat$Role == "STUDENT_1"] <- "STUDENT_CHAT"

# Prepare data for hidden Markov model
dat_HM <- dat %>%  
    filter(AI_TeacherMove != "") %>%
    mutate(AI_TeacherMove = factor(AI_TeacherMove, levels = c(
         "EMOTIONAL_SUPPORT",
        "ERROR_CORRECTION", 
        "EXEMPLIFYING",
        "GIVING_HINT", "GIVING_PRAISE",
        "PROBING_STUDENT_THINKING", "PROMPTING",
        "PROVIDING_EXPLANATION", "REVOICING",
        "SCAFFOLDING", "STUDENT_CHAT"
      # , "UNLABLED_TEACHER_CHAT", "USING_VISUAL_CUES"
    ))) %>%
    dplyr::select(Session_ID, Utterance.ID, AI_TeacherMove) 

head(dat_HM)
table(dat_HM$AI_TeacherMove)


# HMM model selection (2–6 states) across all sessions in depmixS4
install.packages("depmixS4")
library(depmixS4)
set.seed(123)

# ---- Prep ----
# Ensure ordering within and across sessions
dat_HM <- dat_HM[order(dat_HM$Session_ID, dat_HM$Utterance.ID), ]

# Factorize observed categories
dat_HM$AI_TeacherMove <- factor(dat_HM$AI_TeacherMove)

# ntimes: length of each session in the (now) ordered data
session_lengths <- as.numeric(table(dat_HM$Session_ID))

# ---- Fit a grid of models ----
fit_one <- function(k) {
  mod <- depmix(
    response = AI_TeacherMove ~ 1,
    data     = dat_HM,
    nstates  = k,
    family   = multinomial("identity"),
    ntimes   = session_lengths
  )
  fit(mod, verbose = FALSE)
}

ks <- 2:10
fits <- lapply(ks, fit_one)

# ---- Collect fit metrics ----
metrics <- data.frame(
  nstates = ks,
  logLik  = sapply(fits, logLik),
  AIC     = sapply(fits, AIC),
  BIC     = sapply(fits, BIC),
  row.names = NULL
)
metrics[order(metrics$BIC), ]
plot(metrics$nstates, metrics$AIC, type = "b", pch = 19,
     xlab = "Number of States", ylab = "AIC",
     main = "HMM Model Selection by AIC")

# ---- Pick best by BIC and refit (already fit above) ----
best_idx <- which.min(metrics$AIC)
best_k   <- metrics$nstates[best_idx]
best_fit <- fits[[best_idx]]

# ---- Inspect best model ----
summary(best_fit)            # transitions + emission probs
post <- posterior(best_fit)  # state posteriors per utterance


# Most likely state per row (Viterbi-like decode via argmax posterior)
dat_HM$state_hat <- apply(post[ , paste0("S", 1:best_k)], 1, which.max)
head(dat_HM)

# Map numeric states to descriptive labels
state_labels <- c(
  "1" = "Scaffolding mode",
  "2" = "Correction mode",
  "3" = "Support mode"
)

dat_HM$state_label <- state_labels[as.character(dat_HM$state_hat)]

# Check
head(dat_HM[, c("Session_ID","Utterance.ID","AI_TeacherMove","state_hat","state_label")])




# ---- Visualize ----
# State transition  
library(ggplot2)



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
