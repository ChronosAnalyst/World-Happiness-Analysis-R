# ======================================================
# Step 3: Regression Analysis
# ======================================================

library(tidyverse)
library(janitor)

# Load data if needed
if (!exists("df")) {
  df <- readr::read_csv("Data/World_Happiness_Report_2005_2022.csv",
                        show_col_types = FALSE) %>%
    clean_names()
}

# Select variables for regression
reg_df <- df %>%
  select(
    life_ladder,
    log_gdp_per_capita,
    social_support,
    healthy_life_expectancy_at_birth,
    freedom_to_make_life_choices,
    perceptions_of_corruption,
    positive_affect,
    negative_affect
  ) %>%
  drop_na()

# Fit linear model
model <- lm(
  life_ladder ~ log_gdp_per_capita +
    social_support +
    healthy_life_expectancy_at_birth +
    freedom_to_make_life_choices +
    perceptions_of_corruption +
    positive_affect +
    negative_affect,
  data = reg_df
)

# Model summary
summary(model)

# Save coefficients for report
coef_tbl <- broom::tidy(model)
write_csv(coef_tbl, "outputs/regression_coefficients.csv")