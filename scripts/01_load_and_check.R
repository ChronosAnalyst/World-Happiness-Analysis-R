# ======================================================
# Capstone Project
# World Happiness Report (2005–2022)
# Step 1: Load & Initial Checks
# ======================================================

# 0. Packages ------------------------------------------------
packages <- c(
  "tidyverse",
  "janitor",
  "skimr",
  "GGally"
)

installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

library(tidyverse)
library(janitor)
library(skimr)
library(GGally)

# 1. Load data -----------------------------------------------
df <- read_csv("Data/World_Happiness_Report_2005_2022.csv") %>%
  clean_names()

# 2. Basic inspection ---------------------------------------
glimpse(df)
skim(df)

# 3. Dimensions ---------------------------------------------
dim(df)

# 4. Missing values -----------------------------------------
sum(is.na(df))
sort(colSums(is.na(df)), decreasing = TRUE)

# 5. Duplicates ----------------------------------------------
sum(duplicated(df))