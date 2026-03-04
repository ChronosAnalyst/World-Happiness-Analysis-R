# ======================================================
# Step 2: Exploratory Data Analysis (EDA)
# ======================================================

library(tidyverse)
library(janitor)

# If df is not in memory (fresh session), load it again:
if (!exists("df")) {
  df <- readr::read_csv("Data/World_Happiness_Report_2005_2022.csv", show_col_types = FALSE) %>%
    clean_names()
}

# 1) Quick sanity checks
df %>% summarise(
  min_year = min(year, na.rm = TRUE),
  max_year = max(year, na.rm = TRUE),
  n_countries = n_distinct(country_name),
  n_rows = n()
)

# 2) Missingness overview (top 11 columns)
sort(colSums(is.na(df)), decreasing = TRUE)

# 3) Global trend of happiness over time (mean life ladder per year)
global_year <- df %>%
  group_by(year) %>%
  summarise(
    mean_life_ladder = mean(life_ladder, na.rm = TRUE),
    n = sum(!is.na(life_ladder)),
    .groups = "drop"
  )

print(global_year)

ggplot(global_year, aes(x = year, y = mean_life_ladder)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Happiness (Life Ladder) Over Time",
    x = "Year",
    y = "Mean Life Ladder"
  )

# 4) Correlations with Life Ladder (simple, useful for the report)
corr_tbl <- df %>%
  select(life_ladder,
         log_gdp_per_capita,
         social_support,
         healthy_life_expectancy_at_birth,
         freedom_to_make_life_choices,
         generosity,
         perceptions_of_corruption,
         positive_affect,
         negative_affect) %>%
  summarise(across(everything(), ~ cor(life_ladder, .x, use = "complete.obs"))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "cor_with_life_ladder") %>%
  arrange(desc(abs(cor_with_life_ladder)))

print(corr_tbl)

# 5) Save outputs for later use (optional but nice)
write_csv(global_year, "outputs/global_year_trend.csv")
write_csv(corr_tbl, "outputs/correlations_with_life_ladder.csv")
