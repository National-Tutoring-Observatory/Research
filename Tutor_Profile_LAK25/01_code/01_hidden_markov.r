## Load necessary libraries
library(dplyr)
library(tidyr)
library(depmixS4) 
library(ggplot2)
library(stringr)



## load data 
dat <- read.csv("Tutor_Profile_LAK25/02_data/UPChieve_HumanAI_Annotations_2.csv", stringsAsFactors = FALSE)
str(dat)
length(unique(dat$Session_ID))
table(dat$AI_TeacherMove, dat$Role)
levels(as.factor(dat$AI_TeacherMove))

###################################################
## Prepare data for Hidden Markov model ###########
###################################################

# Recode empty AI_TeacherMove based on Role
# dat$AI_TeacherMove[dat$AI_TeacherMove == "" & dat$Role == "TEACHER"] <- "UNLABLED_TEACHER_CHAT"
# dat$AI_TeacherMove[dat$AI_TeacherMove == "" & dat$Role == "STUDENT_1"] <- "STUDENT_CHAT"

dat_HM <- dat %>%  
    filter(AI_TeacherMove != "") %>% # this drops both unspecified teacher moves and student chat
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
    dplyr::select(Session_ID, Utterance_ID, AI_TeacherMove, Content)


# Ensure ordering within and across sessions
dat_HM <- dat_HM[order(dat_HM$Session_ID, dat_HM$Utterance_ID), ]

# Factorize observed categories
dat_HM$AI_TeacherMove <- factor(dat_HM$AI_TeacherMove)

# ntimes: length of each session in the (now) ordered data
session_lengths <- as.numeric(table(dat_HM$Session_ID))


###################################################
## Fit Hidden Markov model ########################
###################################################

set.seed(123)
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
metrics[order(metrics$logLik), ]
plot(metrics$nstates, metrics$logLik, type = "b", pch = 19,
     xlab = "Number of States", ylab = "AIC",
     main = "HMM Model Selection by AIC")

# ---- Pick best by BIC and refit (already fit above) ----
best_idx <- which.min(metrics$logLik)
best_k   <- metrics$nstates[3]
best_fit <- fits[[best_idx]]

# ---- Inspect best model ----
summary(best_fit)            # transitions + emission probs
post <- posterior(best_fit)  # state posteriors per utterance

# save best fit model
saveRDS(best_fit, file = "Tutor_Profile_LAK25/06_models/HHM_Prelim2.rds")

str(best_fit, 2)


###################################################
## save Posterior probabilities ###################
###################################################
# best_fit <- readRDS("Tutor_Profile_LAK25/06_models/HHM_Prelim.rds")
# post <- posterior(best_fit)
# Combine with original (ordered) data
out <- dplyr::bind_cols(dat_HM, post)

# Ensure MAP state exists
state_cols <- grep("^S\\d+$", names(out), value = TRUE)
if (!"state" %in% names(out)) {
  out$state <- max.col(out[, state_cols], ties.method = "first")
}

state_labels <- c(
  "1" = "Guidance",
  "2" = "Correction",
  "3" = "Support"
)

# Optional: label states (uses your state_labels if defined)
if (exists("state_labels")) {
  out$state_label <- state_labels[as.character(out$state)]
  # Rename probability columns using labels
  new_prob_names <- paste0("P_", state_labels[gsub("^S", "", state_cols)])
  names(out)[match(state_cols, names(out))] <- new_prob_names
} else {
  out$state_label <- paste0("S", out$state)
  names(out)[match(state_cols, names(out))] <- paste0("P_", state_cols)
}

# Reorder columns: ids, observed, probs, MAP state
out <- out %>%
  relocate(Session_ID, Utterance.ID, AI_TeacherMove) %>%
  relocate(any_of(sort(grep("^P_", names(out), value = TRUE))), .after = AI_TeacherMove) %>%
  relocate(state, state_label, .after = last_col())

# Save
write.csv(out, "Tutor_Profile_LAK25/02_data/HMM_posterior_probs2.csv", row.names = FALSE)
str(out)

###################################################
## Vizualize Emission Prob ########################
###################################################

# vizualize the emission probs
em_probs <- as.data.frame(as.table(summary(best_fit)))
names(em_probs) <- c("State","Move","Prob")

em_probs$Move  <- factor(em_probs$Move,  levels = colnames(M))
em_probs$State <- factor(em_probs$State, levels = rownames(M))

max(em_probs$Prob)  # should be 1

state_labels <- c(
  "1" = "Guidance",
  "2" = "Correction",
  "3" = "Guidance"
)
# replace df with your data frame name (e.g., em_probs or dat_HM)
em_probs$State <- factor(
  state_labels[sub("^St", "", as.character(em_probs$State))],
  levels = state_labels
)
em_probs



em_probs <- em_probs %>%
  mutate(Move = gsub("^Re1\\.", "", Move)) %>%
  mutate(Move = str_to_title(str_replace_all(Move, "_", " "))) %>%
  mutate(Move = factor(Move, levels = unique(Move)))

# --- Order moves by probs in first state, then second, then third ---
state_order <- levels(em_probs$State)
move_order <- em_probs %>%
  tidyr::pivot_wider(names_from = State, values_from = Prob, values_fill = 0) %>%
  arrange(desc(.data[[state_order[3]]]),
          desc(.data[[state_order[2]]]),
          desc(.data[[state_order[1]]])) %>%
  pull(Move)
em_probs <- em_probs %>% mutate(Move = factor(Move, levels = move_order))


ggplot(em_probs, aes(y = State, x = Move, fill = Prob)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", Prob)), size = 6) +
  scale_fill_gradient(
    limits = c(0, 0.5),
    oob = scales::squish,
    breaks = seq(0, 0.5, by = 0.10),
    labels = function(x) sprintf("%.2f", x),
    low = "white", high = "steelblue",
    name = "Probability"
  ) +
  labs(x = NULL, y = NULL, title = "Hidden Markov Model Emission Probabilities") +
  scale_x_discrete(position = "top") +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 16, angle = 80, vjust = 2, hjust = 0),
    axis.text.y = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 14),
    plot.title   = element_text(size = 20, face = "bold")
  )

ggsave(
  "Tutor_Profile_LAK25/04_figures/Prelim_HMM_emission_probs.jpeg",
  plot = last_plot(),
  width = 12,
  height = 8,
  dpi = 300
)

###################################################
## Posterior State Sequence Plot ##################
###################################################

# Posterior over states
post <- posterior(best_fit)

# Add within-session time index and join with posterior
seq_df <- dat_HM %>%
  group_by(Session_ID) %>%
  mutate(t = row_number()) %>%
  ungroup()

viz <- dplyr::bind_cols(seq_df, post)

# Ensure a MAP state column exists
if (!"state" %in% names(viz)) {
  state_cols <- grep("^S\\d+$", names(viz), value = TRUE)
  viz$state <- max.col(viz[, state_cols], ties.method = "first")
}

# Optional: label states (uses existing state_labels if present)
if (exists("state_labels")) {
  viz$state_lab <- factor(state_labels[as.character(viz$state)], levels = state_labels)
} else {
  viz$state_lab <- factor(paste0("S", viz$state),
                          levels = paste0("S", sort(unique(viz$state))))
}

# Plot MAP state sequence per session
ggplot(viz, aes(x = t, y = state_lab, group = Session_ID)) +
  geom_step(linewidth = 0.8) +
  facet_wrap(~ Session_ID, scales = "free_x") +
  labs(
    x = "Utterance Index",
    y = "Most Likely State",
    title = "HMM Posterior (MAP) State Sequence by Session"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )


  # pick 3 session IDs (replace with your own choices if needed)
example_sessions <- sample(unique(viz$Session_ID), 6)

# filter to those sessions only
viz_examples <- viz %>% filter(Session_ID %in% example_sessions)

# plot
ggplot(viz_examples, aes(x = t, y = state_lab, group = Session_ID)) +
  geom_step(linewidth = 0.8) +
  facet_wrap(~ Session_ID, scales = "free_x") +
  labs(
    x = "Utterance Index",
    y = "Most Likely State",
    title = "HMM Posterior State Sequence – Example Tutoring Sessions"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1) # box around each facet
  )
ggsave(
  "Tutor_Profile_LAK25/04_figures/Prelim_HMM_Posterior_State_Sequence.jpeg",
  plot = last_plot(),
  width = 12,
  height = 8,
  dpi = 300
)


###################################################
## State Transition Diagram #######################
###################################################

# THIS DOES NOT WORK YET


# Required packages
library(dplyr)
#install.packages('igraph', dependencies = TRUE)
library(igraph)
#install.packages("ggraph", dependencies = TRUE)
library(ggraph)
library(ggplot2)

# Ensure MAP state column exists (from earlier)
if (!exists("viz")) {
  post <- posterior(best_fit)
  seq_df <- dat_HM %>%
    group_by(Session_ID) %>%
    mutate(t = row_number()) %>%
    ungroup()
  viz <- dplyr::bind_cols(seq_df, post)
  if (!"state" %in% names(viz)) {
    state_cols <- grep("^S\\d+$", names(viz), value = TRUE)
    viz$state <- max.col(viz[, state_cols], ties.method = "first")
  }
}
# Optional labeled states
if (exists("state_labels")) {
  viz$state_lab <- factor(state_labels[as.character(viz$state)], levels = state_labels)
} else {
  viz$state_lab <- factor(paste0("S", viz$state),
                          levels = paste0("S", sort(unique(viz$state))))
}

# Build transition probability table (within sessions)
transitions <- viz %>%
  arrange(Session_ID, t) %>%
  group_by(Session_ID) %>%
  mutate(prev = dplyr::lag(state_lab)) %>%
  ungroup() %>%
  filter(!is.na(prev)) %>%
  count(prev, state_lab, name = "n") %>%
  group_by(prev) %>%
  mutate(prob = n / sum(n)) %>%
  ungroup() %>%
  rename(from = prev, to = state_lab)

# Create graph
nodes <- data.frame(name = levels(viz$state_lab))
edges <- transitions %>%
  mutate(from = as.character(from), to = as.character(to))

g <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

# GGRAPH version (nice)
ggraph(g, layout = "circle") +
  geom_edge_link(aes(width = prob), alpha = 0.7,
                 arrow = arrow(length = unit(4, "mm")),
                 end_cap = circle(3, "mm")) +
  geom_edge_label(aes(label = sprintf("%.2f", prob)), label.size = NA, vjust = -0.2) +
  geom_node_point(size = 6) +
  geom_node_text(aes(label = name), vjust = -1, size = 5) +
  scale_edge_width(range = c(0.5, 3)) +
  labs(title = "HMM State Transition Diagram",
       edge_width = "Transition\nProbability") +
  theme_void()

# --- Base igraph fallback (no ggraph) ---
plot(g,
     edge.width = 3 * exp(E(g)$prob),
     edge.arrow.size = 0.5,
     edge.curved = 0.1,
     edge.label = sprintf("%.2f", E(g)$prob),
     vertex.size = 30,
     vertex.label.cex = 1.2)

plot(
  g,
  layout = layout_in_circle(g),
  edge.width = 6 * E(g)$prob,
  edge.arrow.size = 0.5,
  edge.curved = 0.2,
  edge.label = sprintf("%.2f", E(g)$prob),
  vertex.size = 30,
  vertex.label.cex = 1.2
)


jpeg("Tutor_Profile_LAK25/04_figures/hmm_transition_diagram.jpeg",
     width = 1500, height = 1500, res = 100)

par(mar = c(6, 6, 6, 6), xpd = NA, family = "Helvetica")

# layout
L <- layout_in_circle(g)


# ---- PLOT without vertex labels; we’ll draw them manually further out ----
plot(
  g,                                 # igraph object
  layout            = L,             # node positions (here: layout_in_circle)
  rescale           = FALSE,         # keep coordinates as provided (don’t rescale)
  xlim              = range(L[,1]) * 1.6,  # x-axis limits (pad to avoid clipping)
  ylim              = range(L[,2]) * 1.6,  # y-axis limits (pad to avoid clipping)
  margin            = 0.05,          # outer plot margin (device-relative)
  curve_multiple    = TRUE,          # curve parallel edges so they don’t overlap

  vertex.label      = NA,            # suppress default node labels (we add manually or later)
  vertex.size       = 48,            # node size (diameter in points)
  vertex.color      = "grey80",      # node fill color
  vertex.frame.color= "black",       # node border color

  edge.width        = 2,             # edge line width
  edge.arrow.size   = 0.7,           # arrowhead size
  edge.curved       = 0.35,          # curvature of edges (0 = straight)
  edge.color        = "black",       # edge line color
  edge.label        = sprintf("%.2f", E(g)$prob),  # edge labels (transition probs)
  edge.label.x      = ex,            # x-positions for edge labels (precomputed)
  edge.label.y      = ey,            # y-positions for edge labels (precomputed)
  edge.label.cex    = 1.4,           # edge label size
  edge.label.family = "Helvetica",   # edge label font family
  edge.label.color  = "black",       # edge label text color
  edge.label.bg     = "white"        # edge label background (improves legibility)
)



text(L[,1], L[,2], labels = V(g)$name, cex = 2.0, family = "Helvetica")

dev.off()
