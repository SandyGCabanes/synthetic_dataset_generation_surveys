setwd("C:/Users/sandy/Google_workspace/_google_sheets_survey/dep_annual_survey_2024/synthesized_data_using_r/github")

# -------------------------------
# Library Section
# -------------------------------
library(synthpop)      # For synthetic data generation and privacy risk evaluation
library(dplyr)         # For data manipulation
library(tidyr)         # For exploding multi-select columns
library(stringr)       # For string operations in multi-select splitting
library(plotly)        # For interactive charts
library(htmlwidgets)   # For saving Plotly charts as HTML
library(DataExplorer)  # For profiling reports


# -------------------------------
# Load Raw Dataset
# -------------------------------
# Replace "dataset.csv" with your actual data file path or use your loaded df
raw_data <- read.csv("dataset_sub.csv", stringsAsFactors = FALSE)
raw_data <- select(raw_data, -bestproject, -Timestamp)

# -------------------------------
# Flags Creation for Disclosure Risk
# -------------------------------
# Adjust column names according to your data schema
raw_data <- raw_data %>%
  mutate(combined_flag = paste(country, city, gender, age, sep = "_"))


# -------------------------------
# Sensitive Variables & Priority Sequence
# -------------------------------
priority_vars <- c("combined_flag")


# -------------------------------
# Custom Predictor Matrix Setup
# -------------------------------
# Run syn() once without synthesis to get initial setup objects
sds <- syn(raw_data, m = 0, maxfaclevels = 700)  # m=0 means no synthetic data generated, just setup.

# Extract and reset predictor matrix
pred_matrix <- sds$predictor.matrix
pred_matrix[,] <- 0

# Keep only 'age', 'gender', 'datarole' as predictors for all variables
allowed_predictors <- c("age", "gender", "datarole")

# Set allowed predictors to 1 for all target variables
for (var in rownames(pred_matrix)) {
  pred_matrix[var, allowed_predictors] <- 1
}

# Exclude the unique respondent identifier and flags from predictors and predicted
pred_matrix["id", ] <- 0
pred_matrix[, "id"] <- 0
for (flag in priority_vars) {
  pred_matrix[flag, ] <- 0
  pred_matrix[, flag] <- 0
}

# Define the priority sequence for synthesis
priority_sequence <- c("city", "country", "gender", "age")

# Ensure these variables exist in raw_data
priority_sequence <- priority_sequence[priority_sequence %in% colnames(raw_data)]

# -------------------------------
# Synthesis with Timing and Logging
# -------------------------------
cat("Starting synthesis...\n")
synthesis_time <- system.time({
  syn_obj <- syn(raw_data, 
                 predictor.matrix = pred_matrix, 
                 visit.sequence = priority_sequence,
                 maxfaclevels = 700,
                 print.flag = TRUE)
})
cat("Synthesis complete.\n")
print(synthesis_time)

# Extract final predictor matrix
pred_matrix <- syn_obj$

# -------------------------------
# Functions for Disclosure Risk and Utility Assessment
# -------------------------------
calculate_disclosure <- function(syn_obj, original_data, keys, targets) {
  risk_eval <- multi.disclosure(syn_obj, 
                               original = original_data, 
                               keys = keys, 
                               targets = targets, 
                               print.flag = FALSE, 
                               plot = FALSE)
  ident_metrics <- risk_eval$ident  
  repU <- ident_metrics$repU[1]  # Overall replicated uniques metric
  return(repU)
}

calculate_utility_diff <- function(synthetic_data, original_data, numeric_vars) {
  ks_stats <- sapply(numeric_vars, function(var) {
    ks_result <- ks.test(synthetic_data[[var]], original_data[[var]])
    return(ks_result$statistic)
  })
  # Average KS statistic (smaller is better)
  return(mean(ks_stats))
}

# -------------------------------
# Multi-select Helpers
# -------------------------------
normalize_text <- function(x) {
  if (is.na(x) || x == "" || tolower(x) %in% c("na", "null", "none")) return(NA_character_)
  return(trimws(as.character(x)))
}

split_multi <- function(x) {
  if (is.na(x) || nchar(x) == 0) return(character(0))
  parts <- unlist(str_split(x, pattern = ",|;"))
  parts <- sapply(parts, normalize_text)
  parts <- parts[!is.na(parts) & parts != ""]
  return(parts)
}

explode_multiselect <- function(df, col_in, id_col, col_out_name) {
  df %>%
    mutate(!!sym(col_out_name) := lapply(.[[col_in]], split_multi)) %>%
    unnest(cols = all_of(col_out_name)) %>%
    select(!!sym(id_col), !!sym(col_out_name))
}

# Define your multi-select columns
multi_select_cols <- c("digitools", "successmethod", "restofrole", "ingestion", 
                      "transform", "warehs", "orchest", "busint", "reversetl",
                      "dataqual", "datacatalog", "cloudplat", "noncloudplat", 
                      "generaltools", "whatused", "hostedntbk", "hardware", 
                      "otherfb")

id_col <- "id"  # Your unique ID column name

# -------------------------------
# Parameter Tuning with synthpop
# -------------------------------
keys <- c(priority_vars, "country", "city", "province", "age_grp")
targets <- c("salary")       # Sensitive numeric variables to consider for disclosure
numeric_vars <- c("age")     # Numeric vars for utility checks

tuning_results <- data.frame(
  minbucket = numeric(),
  cp = numeric(),
  maxdepth = numeric(),
  repU = numeric(),
  util_diff = numeric(),
  combined_score = numeric(),
  stringsAsFactors = FALSE
)

best_score <- Inf
best_params <- list()
best_syn <- NULL

for (minbucket_val in c(3,5,7)) {
  for (cp_val in c(0.001, 0.01, 0.05)) {
    for (maxdepth_val in c(6,8,10)) {
      
      syn_obj <- syn(raw_data,
                     method = "cart",
                     visit.sequence = priority_vars,
                     predictor.matrix = pred_matrix,
                     minbucket = minbucket_val,
                     cp = cp_val,
                     maxdepth = maxdepth_val,
                     seed = 12345,
                     print.flag = FALSE)
      
      repU <- calculate_disclosure(syn_obj, raw_data, keys, targets)
      util_diff <- calculate_utility_diff(syn_obj$syn, raw_data, numeric_vars)
      
      score <- 0.7 * repU + 0.3 * util_diff
      
      tuning_results <- rbind(tuning_results, data.frame(
        minbucket = minbucket_val,
        cp = cp_val,
        maxdepth = maxdepth_val,
        repU = repU,
        util_diff = util_diff,
        combined_score = score
      ))
      
      if (score < best_score) {
        best_score <- score
        best_params <- list(minbucket = minbucket_val, cp = cp_val, maxdepth = maxdepth_val)
        best_syn <- syn_obj
      }
      
      cat(sprintf("Evaluated minbucket=%d, cp=%.4f, maxdepth=%d -> score=%.5f\n",
                  minbucket_val, cp_val, maxdepth_val, score))
    }
  }
}

# Export tuning summary text report
best_run <- tuning_results[which.min(tuning_results$combined_score), ]

summary_text <- paste0(
  "Parameter Tuning Summary\n",
  "------------------------\n",
  "Best combined score: ", round(best_run$combined_score, 4), "\n",
  "Parameters:\n",
  "  minbucket = ", best_run$minbucket, "\n",
  "  cp = ", best_run$cp, "\n",
  "  maxdepth = ", best_run$maxdepth, "\n\n",
  "Privacy risk (Replicated Uniques): ", round(best_run$repU, 4), "\n",
  "Utility score (avg KS stat): ", round(best_run$util_diff, 4), "\n\n",
  "Total tuning runs: ", nrow(tuning_results), "\n"
)

writeLines(summary_text, "parameter_tuning_summary.txt")
cat("Saved parameter tuning summary to parameter_tuning_summary.txt\n")

cat("Best parameters: ", paste(names(best_params), best_params, sep="=", collapse=", "), "\n")

# -------------------------------
# Use best parameters to synthesize final data if not done already
# -------------------------------
# best_syn contains the best synthpop object

synthetic_data <- best_syn$syn

# -------------------------------
# DataExplorer Reports for raw and synthetic data
# -------------------------------
cat("Generating DataExplorer reports...\n")
create_report(raw_data, output_file = "Original_Data_Report.html")
create_report(synthetic_data, output_file = "Synthetic_Data_Report.html")
cat("Generated DataExplorer reports for original and synthetic datasets.\n")

# -------------------------------
# Multi-select Frequency Comparison and Dashboard
# -------------------------------
output_folder <- "multi_response_reports"
dir.create(output_folder, showWarnings = FALSE)

raw_long_list <- lapply(multi_select_cols, function(col) {
  explode_multiselect(raw_data, col_in = col, id_col = id_col, col_out_name = col)
})
names(raw_long_list) <- multi_select_cols

synthetic_long_list <- lapply(multi_select_cols, function(col) {
  explode_multiselect(synthetic_data, col_in = col, id_col = id_col, col_out_name = col)
})
names(synthetic_long_list) <- multi_select_cols

get_freq_table <- function(long_df, col_name) {
  long_df %>%
    group_by(!!sym(col_name)) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(desc(count))
}

compare_freqs <- function(col) {
  raw_freq <- get_freq_table(raw_long_list[[col]], col) %>%
    rename(raw_count = count) %>%
    mutate(raw_percent = raw_count / sum(raw_count) * 100)
  
  synth_freq <- get_freq_table(synthetic_long_list[[col]], col) %>%
    rename(synth_count = count) %>%
    mutate(synth_percent = synth_count / sum(synth_count) * 100)
  
  full_join(raw_freq, synth_freq, by = col) %>%
    replace_na(list(raw_count = 0, synth_count = 0, raw_percent = 0, synth_percent = 0)) %>%
    arrange(desc(raw_percent))
}

generate_plotly_bar_chart <- function(freq_df, col_name) {
  freq_long <- freq_df %>%
    pivot_longer(cols = c(raw_percent, synth_percent),
                 names_to = "dataset",
                 values_to = "percent")
  
  p <- plot_ly(freq_long,
               x = ~reorder(!!sym(col_name), percent),
               y = ~percent,
               color = ~dataset,
               type = 'bar',
               barmode = 'group',
               hoverinfo = 'text',
               text = ~paste(dataset, ":", round(percent, 2), "%")) %>%
    layout(title = paste0(str_to_title(col_name), ": Raw vs Synthetic"),
           xaxis = list(title = str_to_title(col_name), tickangle = -45),
           yaxis = list(title = "Percentage (%)"),
           margin = list(b = 140))
  return(p)
}

cat("Generating multi-select comparison dashboards...\n")
for (col in multi_select_cols) {
  cat("Processing ", col, "...\n")
  comparison_df <- compare_freqs(col)
  plot <- generate_plotly_bar_chart(comparison_df, col)
  filename <- paste0(output_folder, "/", col, "_comparison.html")
  saveWidget(plot, file = filename, selfcontained = TRUE)
  cat("Saved plot: ", filename, "\n")
}
cat("All multi-response comparison plots generated.\n")

# -------------------------------
# End of Workflow
# -------------------------------


# Notes for this full R script:
#•	Raw data loading and flag creation for disclosure risk
#•	Custom predictor matrix and priority vars for synthpop
#•  Conditional identifiers specified as priority vars
#•  Decision point is score <- 0.7 * repU + 0.3 * util_diff
#•	Parameter tuning loop with disclosure risk and utility scoring
#•	Text summary export of tuning results
#•	Synthesis of final synthetic data with best parameters
#•	DataExplorer reports for raw and synthetic
#•	Multi-select variable explosion, frequency comparison, and interactive Plotly dashboard generation with export
