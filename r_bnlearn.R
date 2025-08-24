setwd("C:/Users/sandy/Google_workspace/_google_sheets_survey/dep_annual_survey_2024/synthesized_data_using_r/github")

library(bnlearn)
library(dplyr)

# Load your dataset
df <- read.csv("dataset_sub.csv")

df_model <- as.data.frame(df)
df_model <- df_model %>%
  mutate(age_grp = cut(
    age,
    breaks = c(19, 25, 30, 35, 40, 45, 50, 55, Inf),
    labels = c("20-25", "26-30", "31-35", "36-40", "41-45", "46-50", "51-55", "Above 55"),
    right = FALSE,
    include.lowest = TRUE
  ))
df_model <- select(df_model, -Timestamp, -id, -age)

# We are left with city, country, gender, age, educstat, digitools

df_model$city <- as.factor(df_model$city)
df_model$country <- as.factor(df_model$country)
df_model$gender <- as.factor(df_model$gender)
df_model$educstat <- as.factor(df_model$educstat)
df_model$age_grp <- as.factor(df_model$age_grp)
df_complete <- df_model[complete.cases(df_model), ]

# Learn structure (Hill-Climbing)
bn_structure <- hc(df_complete)

# Fit conditional probability tables
bn_fit <- bn.fit(bn_structure, data = df_model, method = "bayes")

# Generate synthetic data
synthetic_df <- rbn(bn_fit, n = nrow(df_complete))

# ~~~~~~~~~~~~~~~~
# Check duplicates
# ~~~~~~~~~~~~~~~~

# Tag source
df_model <- df_model %>% mutate(source = "real")
synthetic_df <- synthetic_df %>% mutate(source = "synthetic")

# Combine datasets
combined_df <- bind_rows(df_model, synthetic_df)

# Create a composite key
combined_df <- combined_df %>%
  mutate(combined = paste(city, country, gender, educstat, age_grp, sep = "|"))

# Count occurrences of each composite key
dup_check <- combined_df %>%
  group_by(combined) %>%
  summarise(count = n(), sources = paste(unique(source), collapse = ","), .groups = "drop") %>%
  filter(count > 1 & sources == "real,synthetic")

print(dup_check)

# Conclusion: This method is a privacy risk.
# 118 rows duplicated from both real and synthetic

