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
length(unique(dat$Session_ID))
