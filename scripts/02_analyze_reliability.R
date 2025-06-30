# ========================================
# STEP 2: ANALYZE RELIABILITY (ALL WINDOW SIZES)
# ========================================
# Tests different window sizes to find the most reliable one
# This will take 10-15 minutes - go get coffee! ☕

# Clear workspace and load packages
rm(list = ls())
gc()

library(simstudy)
library(data.table)
library(dplyr)
devtools::load_all()

cat("=== ANALYZING RELIABILITY ACROSS WINDOW SIZES ===\n")

# ========================================
# SETTINGS (You can change these)
# ========================================

# Input data file (from Step 1)
INPUT_FILE <- "data/simulated_raw_data_tdist.rds"

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
raw_datasets <- readRDS(INPUT_FILE)
cat("✓ Loaded", length(raw_datasets), "datasets\n")

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

# Process one window size
process_window_size <- function(raw_datasets, window_size, batch_size) {
  n_datasets <- length(raw_datasets)
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
    
    # Process batch
    batch_x_z <- numeric(0)
    batch_score_sd <- numeric(0)
    
    for (i in start_idx:end_idx) {
      tryCatch({
        # Run volatility analysis on this dataset
        result <- run_volatility_pipeline(
          x = raw_datasets[[i]]$x,
          y = raw_datasets[[i]]$y_norm,
          window_size = window_size,
          x_type = "continuous",
          y_type = "continuous"
        )
        
        # Extract results from all windows
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
        # Only print occasional errors to avoid spam
        if (i %% 50 == 0) {
          cat("      Warning: Dataset", i, "failed\n")
        }
      })
    }
    
    # Add batch results
    all_x_z <- c(all_x_z, batch_x_z)
    all_score_sd <- c(all_score_sd, batch_score_sd)
    
    # Clean up memory
    rm(batch_x_z, batch_score_sd)
    gc(verbose = FALSE)
  }
  
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
    
    # Calculate reliability metrics (this is the analytical part!)
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
    cat("✅ Window size", window_size, "complete!\n")
    
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
  cat("\n💾 SAVING RESULTS...\n")
  
  tryCatch({
    saved_files <- save_reliability_results(all_results, "multi_window_reliability")
    
    cat("✓ Saved detailed results:", saved_files$rds_file, "\n")
    cat("✓ Saved summary table:", saved_files$csv_file, "\n")
    
    # Show quick summary
    cat("\n📊 QUICK SUMMARY:\n")
    summary_table <- saved_files$summary_table
    if ("zone_90_width" %in% names(summary_table)) {
      zone_data <- summary_table[, c("window_size", "zone_90_width")]
      zone_data <- zone_data[order(zone_data$window_size), ]
      zone_data <- zone_data[!is.na(zone_data$zone_90_width), ]
      
      if (nrow(zone_data) > 0) {
        cat("90% Reliability Zone Widths:\n")
        for (i in seq_len(min(5, nrow(zone_data)))) {
          cat(sprintf("  Window %3d: %.3f\n", zone_data$window_size[i], zone_data$zone_90_width[i]))
        }
        
        # Find best
        best_idx <- which.min(zone_data$zone_90_width)
        cat(sprintf("\n🏆 BEST: Window size %d (zone width = %.3f)\n", 
                   zone_data$window_size[best_idx], zone_data$zone_90_width[best_idx]))
      }
    }
    
  }, error = function(e) {
    cat("❌ Error saving:", e$message, "\n")
  })
} else {
  cat("⚠️ No results to save\n")
}

cat("\n🎯 NEXT STEP: Run '03_view_results.R' to see detailed results\n")

# Clean up
rm(list = ls())
gc()
validate_data_format(datasets)

# ========================================
# RUN ANALYSIS
# ========================================

cat("\n--- Starting Reliability Analysis ---\n")
start_time <- Sys.time()

# Run the main analysis
results <- analyze_reliability(
  datasets = datasets,
  window_size = WINDOW_SIZE,
  reliability_thresholds = RELIABILITY_THRESHOLDS,
  batch_size = BATCH_SIZE
)

end_time <- Sys.time()
analysis_duration <- as.numeric(difftime(end_time, start_time, units = "mins"))

cat("\n✓ Analysis completed in", round(analysis_duration, 2), "minutes\n")

# ========================================
# DISPLAY RESULTS
# ========================================

# Print summary to console
print_analysis_summary(results)

# ========================================
# SAVE RESULTS
# ========================================

cat("Saving results...\n")
saved_files <- save_reliability_results(results, OUTPUT_NAME)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Results saved as:\n")
cat("- Detailed results:", saved_files$detailed, "\n")
cat("- Summary table:", saved_files$summary, "\n")

# Show summary table
cat("\nSummary Table:\n")
print(saved_files$summary_data)

cat("\nNext step: Run '03_view_results.R' to create visualizations\n\n")

# ========================================
# INTERPRETATION GUIDE
# ========================================

cat("📊 How to interpret your results:\n\n")

cat("1. MODEL QUALITY (R²):\n")
if (results$model_rsq > 0.1) {
  cat("   ✓ Good: Your model shows clear patterns (R² =", round(results$model_rsq, 3), ")\n")
} else if (results$model_rsq > 0.01) {
  cat("   ⚠ Moderate: Some patterns detected (R² =", round(results$model_rsq, 3), ")\n")
} else {
  cat("   ❌ Poor: Very flat model, little pattern (R² =", round(results$model_rsq, 3), ")\n")
}

cat("\n2. RELIABILITY ZONES:\n")
for (zone_name in names(results$zones)) {
  zone_info <- results$zones[[zone_name]]
  threshold_pct <- gsub("zone_", "", zone_name)
  
  if (!is.na(zone_info$width)) {
    if (zone_info$width < 1.0) {
      cat("   ✓", threshold_pct, "%: Very precise zone (width =", round(zone_info$width, 3), ")\n")
    } else if (zone_info$width < 3.0) {
      cat("   ✓", threshold_pct, "%: Reasonable precision (width =", round(zone_info$width, 3), ")\n")
    } else {
      cat("   ⚠", threshold_pct, "%: Large zone, less precise (width =", round(zone_info$width, 3), ")\n")
    }
  } else {
    cat("   ❌", threshold_pct, "%: Threshold not achievable with this data\n")
  }
}

cat("\n3. NEXT STEPS:\n")
cat("   - Run '03_view_results.R' to see plots\n")
cat("   - Run '04_compare_window_sizes.R' to find optimal window size\n")
cat("   - Adjust WINDOW_SIZE or RELIABILITY_THRESHOLDS and re-run if needed\n\n")
