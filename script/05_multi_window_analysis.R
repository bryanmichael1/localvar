# ============================
# Multi-Window Size Analysis - RAW DATA
# ============================
# Processes 1000 raw datasets through volatility pipeline with different window sizes
# Your data: 1000 raw datasets with x, y_norm, sim_id
# Optimized for memory efficiency on 16GB Mac M3

# Clear environment and load packages
rm(list = ls())
gc()

library(simstudy)
library(data.table)
library(dplyr)
devtools::load_all()

cat("=== MULTI-WINDOW SIZE ANALYSIS ===\n")
cat("Processing ALL 1000 raw datasets with different window sizes\n")
cat("Optimized for Mac M3 with 16GB RAM\n\n")

# ============================
# Configuration
# ============================

# Window sizes to analyze
WINDOW_SIZES <- c(3, 5, 10, 15, 20, 25, 30, 40, 50, 75, 100)
RELIABILITY_THRESHOLDS <- c(0.95, 0.90, 0.85, 0.80, 0.75, 0.70)
OUTPUT_PREFIX <- "multi_window_reliability"

# Memory efficiency settings
BATCH_SIZE <- 20  # Process datasets in batches to manage memory

# Input file - contains RAW datasets
RAW_DATA_FILE <- "simulated_raw_data_tdist.rds"

# ============================
# Helper Functions
# ============================

#' Memory-efficient progress tracking
track_progress <- function(current, total, window_size, start_time) {
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  pct <- round(100 * current / total, 1)
  eta <- if (current > 0) elapsed * (total - current) / current else NA
  
  cat(sprintf("Progress: %d/%d (%.1f%%) | Window: %d | Elapsed: %.1f min | ETA: %.1f min\n", 
              current, total, pct, window_size, elapsed, eta))
}

#' Process raw datasets through volatility pipeline for specific window size
process_window_size <- function(raw_datasets, window_size, batch_size = 20) {
  n_datasets <- length(raw_datasets)  # Use all available datasets
  n_batches <- ceiling(n_datasets / batch_size)
  
  all_x_z <- numeric(0)
  all_score_sd <- numeric(0)
  
  cat("  Processing", n_datasets, "datasets in", n_batches, "batches\n")
  
  for (batch in seq_len(n_batches)) {
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, n_datasets)
    
    if (batch %% 5 == 0 || batch == n_batches) {
      cat("    Batch", batch, "/", n_batches, "\n")
    }
    
    # Process batch through pipeline
    batch_x_z <- numeric(0)
    batch_score_sd <- numeric(0)
    
    for (i in start_idx:end_idx) {
      tryCatch({
        # Run volatility pipeline on raw data with specified window size
        result <- run_volatility_pipeline(
          x = raw_datasets[[i]]$x,
          y = raw_datasets[[i]]$y_norm,
          window_size = window_size,
          x_type = "continuous",
          y_type = "continuous"
        )
        
        # Extract data from all windows
        if (!is.null(result$windows)) {
          for (w in result$windows) {
            if (!is.null(w$x_stats) && !is.null(w$x_stats$mean_z) && 
                !is.null(w$score) && !is.null(w$score$sd) &&
                is.finite(w$x_stats$mean_z) && is.finite(w$score$sd)) {
              batch_x_z <- c(batch_x_z, w$x_stats$mean_z)
              batch_score_sd <- c(batch_score_sd, w$score$sd)
            }
          }
        }
        
      }, error = function(e) {
        if (i %% 50 == 0) {  # Only print occasional errors to avoid spam
          cat("      Warning: Dataset", i, "failed:", e$message, "\n")
        }
      })
    }
    
    # Add batch results
    all_x_z <- c(all_x_z, batch_x_z)
    all_score_sd <- c(all_score_sd, batch_score_sd)
    
    # Memory cleanup
    rm(batch_x_z, batch_score_sd)
    gc(verbose = FALSE)
  }
  
  cat("  ✓ Extracted", length(all_x_z), "data points for window size", window_size, "\n")
  return(list(x_z = all_x_z, score_sd = all_score_sd))
}

# ============================
# Load and Validate Data
# ============================

cat("Loading raw simulation data...\n")
if (!file.exists(RAW_DATA_FILE)) {
  stop("Raw data file not found: ", RAW_DATA_FILE)
}

raw_datasets <- readRDS(RAW_DATA_FILE)
cat("✓ Loaded", length(raw_datasets), "raw datasets\n")

# Validate data structure
if (length(raw_datasets) == 0) {
  stop("No datasets found in file")
}

# Check first dataset structure
first_dataset <- raw_datasets[[1]]
cat("Dataset structure: ", nrow(first_dataset), "rows,", 
    "columns:", paste(names(first_dataset), collapse = ", "), "\n")

if (!all(c("x", "y_norm") %in% names(first_dataset))) {
  stop("Datasets must have 'x' and 'y_norm' columns")
}

cat("Window sizes to analyze:", paste(WINDOW_SIZES, collapse = ", "), "\n")
cat("Datasets per window: ALL", length(raw_datasets), "datasets\n\n")

# ============================
# Memory Management Setup
# ============================

cat("Initial memory usage:\n")
print(gc())

# ============================
# Multi-Window Analysis
# ============================

cat("\n=== STARTING MULTI-WINDOW ANALYSIS ===\n")
start_time <- Sys.time()

all_results <- list()
failed_analyses <- c()

for (i in seq_along(WINDOW_SIZES)) {
  window_size <- WINDOW_SIZES[i]
  
  # Progress tracking
  track_progress(i - 1, length(WINDOW_SIZES), window_size, start_time)
  
  cat("\n--- Processing window size:", window_size, "---\n")
  
  # Memory check before each analysis
  if (i %% 3 == 0) {  # Every 3 analyses
    gc_result <- gc()
    cat("Memory check: Used", round(gc_result[2,2], 1), "MB\n")
  }
  
  # Process this window size
  tryCatch({
    # Extract data for this window size
    window_data <- process_window_size(raw_datasets, window_size, BATCH_SIZE)
    
    if (length(window_data$x_z) < 100) {
      cat("⚠️ Insufficient data for window size", window_size, 
          "(only", length(window_data$x_z), "points)\n")
      failed_analyses <- c(failed_analyses, window_size)
      next
    }
    
    # Calculate reliability metrics
    metrics <- calculate_reliability_metrics_optimized(
      window_data$x_z, window_data$score_sd, RELIABILITY_THRESHOLDS
    )
    
    # Store results
    all_results[[paste0("window_", window_size)]] <- c(
      list(window_size = window_size, n_datapoints = length(window_data$x_z)),
      metrics
    )
    
    # Print summary
    print_reliability_summary(window_size, metrics)
    cat("✅ Window size", window_size, "analysis complete\n")
    
  }, error = function(e) {
    cat("❌ Analysis failed for window size", window_size, ":", e$message, "\n")
    failed_analyses <- c(failed_analyses, window_size)
  })
  
  # Aggressive garbage collection between analyses
  for (j in 1:3) gc(verbose = FALSE)
}

# Final progress
track_progress(length(WINDOW_SIZES), length(WINDOW_SIZES), 
               WINDOW_SIZES[length(WINDOW_SIZES)], start_time)

cat("\n=== ANALYSIS COMPLETE ===\n")
total_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
cat("Total duration:", round(total_time, 2), "minutes\n")
cat("Successfully analyzed:", length(all_results), "window sizes\n")

if (length(failed_analyses) > 0) {
  cat("Failed analyses:", paste(failed_analyses, collapse = ", "), "\n")
}

# ============================
# Save Results
# ============================

if (length(all_results) > 0) {
  cat("\n=== SAVING RESULTS ===\n")
  
  tryCatch({
    saved_files <- save_reliability_results(all_results, OUTPUT_PREFIX)
    
    cat("Files saved:\n")
    cat("- ", saved_files$rds_file, " (detailed results)\n")
    cat("- ", saved_files$csv_file, " (summary table)\n")
    
    # Display summary statistics
    cat("\n=== SUMMARY STATISTICS ===\n")
    print(saved_files$summary_table)
    
    # Zone width analysis across window sizes
    if (nrow(saved_files$summary_table) > 1) {
      cat("\n=== ZONE WIDTH ANALYSIS ===\n")
      cat("90% Reliability Zone Widths by Window Size:\n")
      zone_data <- saved_files$summary_table[, c("window_size", "zone_90_width")]
      zone_data <- zone_data[order(zone_data$window_size), ]
      print(zone_data)
      
      cat("\nZone Width Statistics (90% reliability):\n")
      print(summary(zone_data$zone_90_width))
      
      # Find optimal window size (smallest zone width)
      optimal_idx <- which.min(zone_data$zone_90_width)
      cat("\nOptimal window size (smallest 90% zone):", 
          zone_data$window_size[optimal_idx], 
          "with width", round(zone_data$zone_90_width[optimal_idx], 3), "\n")
    }
    
  }, error = function(e) {
    cat("❌ Error saving results:", e$message, "\n")
  })
  
} else {
  cat("⚠️ No results to save\n")
}

# ============================
# Final Summary
# ============================

cat("\n=== FINAL MEMORY USAGE ===\n")
print(gc())

cat("\n=== MULTI-WINDOW ANALYSIS COMPLETE ===\n")
cat("Processed", length(raw_datasets), "raw datasets\n")
cat("Window sizes analyzed:", length(all_results), "\n")
cat("Analysis duration:", round(total_time, 2), "minutes\n")

# Clean up
rm(list = ls())
gc()
