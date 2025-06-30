# ============================
# Single Window Reliability Analysis
# ============================
# Analyzes reliability for a single window size (from existing results)
# This is a simpler version for single-window analysis

# Clear environment
rm(list = ls())
gc()

library(simstudy)
library(data.table)
library(dplyr)
devtools::load_all()

cat("=== SINGLE WINDOW RELIABILITY ANALYSIS ===\n")

# ============================
# Configuration
# ============================

TARGET_WINDOW_SIZE <- 30  # Change this to analyze different window sizes
RELIABILITY_THRESHOLDS <- c(0.95, 0.90, 0.85, 0.80, 0.75, 0.70)
OUTPUT_PREFIX <- "single_window_reliability"
MAX_DATASETS <- 200  # Limit for memory efficiency

RAW_DATA_FILE <- "simulated_raw_data_tdist.rds"

# ============================
# Load and Process Data
# ============================

cat("Loading raw data...\n")
if (!file.exists(RAW_DATA_FILE)) {
  stop("Raw data file not found: ", RAW_DATA_FILE)
}

raw_datasets <- readRDS(RAW_DATA_FILE)
cat("✓ Loaded", length(raw_datasets), "raw datasets\n")

# Use subset for memory efficiency
use_datasets <- min(MAX_DATASETS, length(raw_datasets))
cat("Using", use_datasets, "datasets for analysis\n")

# ============================
# Process Through Pipeline
# ============================

cat("Processing datasets through volatility pipeline...\n")
cat("Window size:", TARGET_WINDOW_SIZE, "\n")

all_x_z <- numeric(0)
all_score_sd <- numeric(0)

start_time <- Sys.time()

for (i in seq_len(use_datasets)) {
  if (i %% 50 == 0) {
    cat("Processing dataset", i, "/", use_datasets, "\n")
  }
  
  tryCatch({
    result <- run_volatility_pipeline(
      window_size = TARGET_WINDOW_SIZE,
      x = raw_datasets[[i]]$x,
      y = raw_datasets[[i]]$y_norm,
      x_type = "continuous",
      y_type = "continuous"
    )
    
    # Extract data from all windows
    if (!is.null(result$windows)) {
      for (w in result$windows) {
        if (!is.null(w$x_stats) && !is.null(w$x_stats$mean_z) && 
            !is.null(w$score) && !is.null(w$score$sd) &&
            is.finite(w$x_stats$mean_z) && is.finite(w$score$sd)) {
          all_x_z <- c(all_x_z, w$x_stats$mean_z)
          all_score_sd <- c(all_score_sd, w$score$sd)
        }
      }
    }
    
  }, error = function(e) {
    if (i %% 50 == 0) {
      cat("  Warning: Dataset", i, "failed\n")
    }
  })
}

processing_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
cat("✓ Processing complete:", round(processing_time, 2), "minutes\n")
cat("✓ Extracted", length(all_x_z), "data points\n")

# ============================
# Reliability Analysis
# ============================

if (length(all_x_z) > 100) {
  cat("\nCalculating reliability metrics...\n")
  
  metrics <- calculate_reliability_metrics_optimized(
    all_x_z, all_score_sd, RELIABILITY_THRESHOLDS
  )
  
  # Create result structure
  result <- c(
    list(window_size = TARGET_WINDOW_SIZE, n_datapoints = length(all_x_z)),
    metrics
  )
  
  # Print summary
  print_reliability_summary(TARGET_WINDOW_SIZE, metrics)
  
  # Save results
  cat("\nSaving results...\n")
  results_list <- list()
  results_list[[paste0("window_", TARGET_WINDOW_SIZE)]] <- result
  
  saved_files <- save_reliability_results(results_list, OUTPUT_PREFIX)
  
  cat("Files saved:\n")
  cat("- ", saved_files$rds_file, "\n")
  cat("- ", saved_files$csv_file, "\n")
  
} else {
  cat("❌ Insufficient data for analysis (need >100 points)\n")
}

cat("\n✅ Single window reliability analysis complete\n")
cat("For multi-window analysis (3-100), use: source('script/05_multi_window_analysis.R')\n")

rm(list = ls())
gc()
