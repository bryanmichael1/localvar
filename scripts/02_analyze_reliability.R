# ========================================
# STEP 2: ANALYZE RELIABILITY (ALL WINDOW SIZES)
# ========================================
# Tests different window sizes to find the most reliable one
# This will take 10-15 minutes - go get coffee! ☕

# Clear workspace and load packages
rm(list = ls())
gc()

# Install required packages if needed
required_packages <- c("simstudy", "data.table", "dplyr", "future", "furrr", "purrr")
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages)
}

library(simstudy)
library(data.table)
library(dplyr)
library(future)      # Modern parallel processing
library(furrr)       # purrr-style parallel functions
library(purrr)       # Data manipulation functions
devtools::load_all()

cat("=== ANALYZING RELIABILITY ACROSS WINDOW SIZES ===\n")

# ========================================
# SETTINGS (You can change these)
# ========================================

# Input data file (from Step 1)
INPUT_FILE <- "data/simulated_raw_data_tdist.rds"

# Number of datasets to use (NULL to use all)
MAX_DATASETS <- 100  # e.g., 500 to use first 500 datasets, or NULL for all

# Window sizes to test (you can add/remove sizes)
WINDOW_SIZES <- c(3, 5, 10, 15, 20, 25, 30, 40, 50, 75, 100)

# Reliability thresholds to calculate zones for
RELIABILITY_THRESHOLDS <- c(0.95, 0.90, 0.85, 0.80, 0.75, 0.70)

# Output files
OUTPUT_DETAILED <- "data/multi_window_reliability_detailed.rds"
OUTPUT_SUMMARY <- "results/multi_window_reliability_summary.csv"

# Memory settings (for 16GB Mac)
BATCH_SIZE <- 20

cat("Settings:\n")
cat("- Input file:", INPUT_FILE, "\n")
cat("- Max datasets:", ifelse(is.null(MAX_DATASETS), "All", MAX_DATASETS), "\n")
cat("- Window sizes:", paste(WINDOW_SIZES, collapse = ", "), "\n")
cat("- Reliability thresholds:", paste(RELIABILITY_THRESHOLDS * 100, collapse = "%, "), "%\n")
cat("- Batch size:", BATCH_SIZE, "\n\n")

# ========================================
# LOAD DATA
# ========================================

if (!file.exists(INPUT_FILE)) {
  stop("❌ Data file not found: ", INPUT_FILE, 
       "\n🔧 Run '01_generate_data.R' first!")
}

cat("Loading data...\n")
raw_datasets_full <- readRDS(INPUT_FILE)
cat("✓ Loaded", length(raw_datasets_full), "datasets from file\n")

# Subset datasets if MAX_DATASETS is specified
if (!is.null(MAX_DATASETS) && is.numeric(MAX_DATASETS)) {
  n_use <- min(MAX_DATASETS, length(raw_datasets_full))
  cat("Limiting to first", n_use, "datasets for analysis (of", length(raw_datasets_full), ")\n")
  raw_datasets <- raw_datasets_full[seq_len(n_use)]
} else {
  raw_datasets <- raw_datasets_full
}

cat("Using", length(raw_datasets), "datasets for all window sizes\n")

# Validate data structure
first_dataset <- raw_datasets[[1]]
if (!all(c("x", "y_norm") %in% names(first_dataset))) {
  stop("❌ Data must have 'x' and 'y_norm' columns")
}

cat("Data looks good!\n\n")

# ========================================
# HELPER FUNCTIONS
# ========================================

# Progress tracking
track_progress <- function(current, total, window_size, start_time) {
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  pct <- round(100 * current / total, 1)
  eta <- if (current > 0) elapsed * (total - current) / current else NA
  
  cat(sprintf("📊 Progress: %d/%d (%.1f%%) | Window: %d | Elapsed: %.1f min | ETA: %.1f min\n", 
              current, total, pct, window_size, elapsed, eta))
}

# Process one window size with future/furrr parallel processing
process_window_size <- function(raw_datasets, window_size, batch_size) {
  n_datasets <- length(raw_datasets)
  
  cat("  Processing", n_datasets, "datasets with future/furrr parallel processing\n")
  
  # Set up parallel processing plan
  n_cores <- max(1, min(future::availableCores() - 1, 4))  # Cap at 4 cores for stability
  cat("  Using", n_cores, "cores for parallel processing\n")
  
  # Configure future plan for parallel processing
  if (n_cores > 1) {
    future::plan(future::multisession, workers = n_cores)
  } else {
    future::plan(future::sequential)
  }
  
  # Process datasets in parallel using furrr
  results <- furrr::future_map(seq_len(n_datasets), function(i) {
    # Source all R functions in each parallel worker
    source(file.path("R", "core_pipeline.R"))
    source(file.path("R", "helper_functions.R"))
    source(file.path("R", "reliability_functions.R"))
    
    tryCatch({
      # Get dataset
      dataset <- raw_datasets[[i]]
      
      # Validate dataset structure
      if (!all(c("x", "y_norm") %in% names(dataset))) {
        return(list(x_z = numeric(0), score_sd = numeric(0), error = "Missing columns"))
      }
      
      # Run volatility analysis on this dataset
      result <- run_volatility_pipeline(
        x = dataset$x,
        y = dataset$y_norm,
        window_size = window_size,  # This variable is passed via globals
        x_type = "continuous",
        y_type = "continuous"
      )
      
      # Debug: check result structure
      if (is.null(result)) {
        return(list(x_z = numeric(0), score_sd = numeric(0), error = "NULL result"))
      }
      
      if (is.null(result$windows)) {
        error_msg <- if (!is.null(result$meta$error)) result$meta$error else "No windows created"
        return(list(x_z = numeric(0), score_sd = numeric(0), error = error_msg))
      }
      
      # Extract results from all windows
      x_z_vals <- numeric(0)
      score_sd_vals <- numeric(0)
      
      for (w in result$windows) {
        if (!is.null(w$x_stats) && !is.null(w$x_stats$mean_z) && 
            !is.null(w$score) && !is.null(w$score$sd) &&
            is.finite(w$x_stats$mean_z) && is.finite(w$score$sd)) {
          x_z_vals <- c(x_z_vals, w$x_stats$mean_z)
          score_sd_vals <- c(score_sd_vals, w$score$sd)
        }
      }
      
      return(list(x_z = x_z_vals, score_sd = score_sd_vals, n_windows = length(result$windows)))
      
    }, error = function(e) {
      # Return error info for debugging
      return(list(x_z = numeric(0), score_sd = numeric(0), error = e$message))
    })
  }, .options = furrr_options(seed = TRUE, globals = list(raw_datasets = raw_datasets, window_size = window_size)))
  
  # Combine results from all datasets
  all_x_z <- unlist(purrr::map(results, "x_z"))
  all_score_sd <- unlist(purrr::map(results, "score_sd"))
  
  cat("  ✓ Extracted", length(all_x_z), "data points for window size", window_size, "\n")
  return(list(x_z = all_x_z, score_sd = all_score_sd))
}

# ========================================
# MAIN ANALYSIS
# ========================================

cat("🚀 STARTING ANALYSIS...\n")
cat("This will take 10-15 minutes for", length(WINDOW_SIZES), "window sizes\n\n")

start_time <- Sys.time()
all_results <- list()
failed_analyses <- c()

for (i in seq_along(WINDOW_SIZES)) {
  window_size <- WINDOW_SIZES[i]
  
  # Progress update
  track_progress(i - 1, length(WINDOW_SIZES), window_size, start_time)
  
  cat("\n--- Analyzing window size:", window_size, "---\n")
  
  # Memory check every 3 analyses
  if (i %% 3 == 0) {
    gc_result <- gc(verbose = FALSE)
    cat("Memory used:", round(gc_result[2,2], 1), "MB\n")
  }
  
  # Process this window size
  tryCatch({
    # Extract data for this window size
    window_data <- process_window_size(raw_datasets, window_size, BATCH_SIZE)
    
    if (length(window_data$x_z) < 100) {
      cat("⚠️ Too little data for window size", window_size, "\n")
      failed_analyses <- c(failed_analyses, window_size)
      next
    }
    
    # Store raw data only (zone calculations moved to step 3)
    basic_metrics <- list(
      window_size = window_size,
      n_datapoints = length(window_data$x_z),
      x_z_data = window_data$x_z,
      score_sd_data = window_data$score_sd,
      x_range = range(window_data$x_z, na.rm = TRUE),
      baseline_volatility = mean(abs(window_data$score_sd), na.rm = TRUE)
    )
    
    # Store results
    all_results[[paste0("window_", window_size)]] <- basic_metrics
    
    cat("✅ Window size", window_size, "complete! Collected", length(window_data$x_z), "data points\n")
    cat("    x_z range: [", round(basic_metrics$x_range[1], 3), ",", round(basic_metrics$x_range[2], 3), "]\n")
    
  }, error = function(e) {
    cat("❌ Window size", window_size, "failed:", e$message, "\n")
    failed_analyses <- c(failed_analyses, window_size)
  })
  
  # Clean up memory aggressively
  for (j in 1:3) gc(verbose = FALSE)
}

# Final progress
track_progress(length(WINDOW_SIZES), length(WINDOW_SIZES), 
               WINDOW_SIZES[length(WINDOW_SIZES)], start_time)

# ========================================
# SAVE RESULTS
# ========================================

total_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
cat("\n✅ ANALYSIS COMPLETE!\n")
cat("Total time:", round(total_time, 2), "minutes\n")
cat("Successfully analyzed:", length(all_results), "window sizes\n")

if (length(failed_analyses) > 0) {
  cat("⚠️ Failed:", paste(failed_analyses, collapse = ", "), "\n")
}

if (length(all_results) > 0) {
  cat("\n💾 SAVING RAW DATA...\n")
  
  tryCatch({
    # Save raw collected data (no zone analysis yet)
    saveRDS(all_results, OUTPUT_DETAILED)
    
    # Create basic summary table
    summary_data <- data.frame(
      window_size = sapply(all_results, function(x) x$window_size),
      n_datapoints = sapply(all_results, function(x) x$n_datapoints),
      baseline_volatility = sapply(all_results, function(x) x$baseline_volatility),
      x_range_min = sapply(all_results, function(x) x$x_range[1]),
      x_range_max = sapply(all_results, function(x) x$x_range[2])
    )
    
    write.csv(summary_data, OUTPUT_SUMMARY, row.names = FALSE)
    
    cat("✓ Saved raw data:", OUTPUT_DETAILED, "\n")
    cat("✓ Saved basic summary:", OUTPUT_SUMMARY, "\n")
    
    # Show data collection summary
    cat("\n📊 DATA COLLECTION SUMMARY:\n")
    for (i in seq_len(min(5, nrow(summary_data)))) {
      cat(sprintf("  Window %3d: %d data points, x-range [%.3f, %.3f]\n", 
                 summary_data$window_size[i], 
                 summary_data$n_datapoints[i],
                 summary_data$x_range_min[i],
                 summary_data$x_range_max[i]))
    }
    
  }, error = function(e) {
    cat("❌ Error saving:", e$message, "\n")
  })
} else {
  cat("⚠️ No results to save\n")
}

cat("\n🎯 NEXT STEP: Run '03_view_results.R' to calculate zones and see detailed results\n")
cat("📊 The zone analysis (95% reliability, etc.) will be done in step 3 for efficiency\n")

# Clean up parallel processing and memory
future::plan(future::sequential)  # Reset to sequential processing
rm(list = ls())
gc()

cat("\n=== ANALYSIS COMPLETE ===\n")
