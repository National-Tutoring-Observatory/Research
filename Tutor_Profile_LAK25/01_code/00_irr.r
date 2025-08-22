## Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
source("Tutor_Profile_LAK25/03_functions/irr.r")


## load data 
dat <- read.csv("Tutor_Profile_LAK25/02_data/UPChieve_HumanAI_Annotations_2.csv", stringsAsFactors = FALSE)
str(dat)
length(unique(dat$Session_ID))
table(dat$AI_TeacherMove, dat$Role)
levels(as.factor(dat$AI_TeacherMove))
levels(as.factor(dat$Human_TeacherMove))


IRR<-irr_metrics(dat$Human_TeacherMove, dat$AI_TeacherMove, primary = c("var2")) 
str(IRR)

IRR$overall
IRR$per_class
