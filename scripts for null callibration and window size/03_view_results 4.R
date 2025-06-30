# ========================================
# STEP 3: VIEW RESULTS FOR SINGLE WINDOW SIZE
# ========================================
# Creates detailed visualizations and reports for a specific window size
# Use this script to dive deep into one window size configuration

# Clear workspace and load packages
rm(list = ls())
gc()

library(ggplot2)
library(dplyr)
library(data.table)
library(reshape2)
library(rlang)

# Load project package
if (file.exists("DESCRIPTION")) {
  # Development mode - load from source
  devtools::load_all()
} else {
  # Production mode - use installed package
  library(localvar)
}

# Load configuration
source("config.R")

cat("=== SINGLE WINDOW SIZE ANALYSIS ===\n")

# ========================================
# SETTINGS (from config.R - modify config.R to change these)
# ========================================

# Use configuration values
TARGET_WINDOW_SIZE <- CONFIG$default_window_size
RELIABILITY_THRESHOLDS <- CONFIG$reliability_thresholds

cat("🎯 Analyzing window size:", TARGET_WINDOW_SIZE, "\n")
cat("📊 Reliability thresholds:", paste(RELIABILITY_THRESHOLDS, collapse = ", "), "\n\n")

# ========================================
# RUN SINGLE WINDOW ANALYSIS
# ========================================

# Use the shared analysis function
tryCatch({
  results <- generate_single_window_analysis(
    window_size = TARGET_WINDOW_SIZE,
    reliability_thresholds = RELIABILITY_THRESHOLDS,
    output_prefix = paste0("single_window_", TARGET_WINDOW_SIZE)
  )
  
  cat("✅ Analysis complete!\n\n")
  
  # Print summary
  cat("📊 ANALYSIS SUMMARY:\n")
  cat("==========================================\n")
  cat("Window size:", TARGET_WINDOW_SIZE, "\n")
  cat("Model R²:", round(results$window_data$model_rsq, 4), "\n")
  cat("Baseline volatility:", round(results$window_data$baseline_volatility, 4), "\n")
  cat("Data points:", length(results$window_data$x_z_data), "\n\n")
  
  cat("🎯 RELIABILITY ZONES:\n")
  cat("==========================================\n")
  if ("zones" %in% names(results$window_data)) {
    for (threshold in RELIABILITY_THRESHOLDS) {
      zone_name <- paste0("zone_", round(threshold * 100), "pct")
      if (zone_name %in% names(results$window_data$zones)) {
        cat(sprintf("%s reliability: width = %.3f\n", 
                   paste0(round(threshold * 100), "%"), 
                   results$window_data$zones[[zone_name]]$width))
      }
    }
  }
  
  cat("\n📈 PLOTS CREATED:\n")
  cat("==========================================\n")
  for (plot_name in names(results$plots)) {
    filename <- paste0("results/single_window_", TARGET_WINDOW_SIZE, "_", plot_name, "_", TARGET_WINDOW_SIZE, ".png")
    cat("  📊", basename(filename), "\n")
  }
  
  cat("\n✅ Single window analysis complete!\n")
  cat("🎯 TIP: Change TARGET_WINDOW_SIZE at the top of this script to analyze other window sizes\n")
  cat("🔄 NEXT: Run '04_compare_window_sizes.R' to compare all window sizes\n\n")
  
}, error = function(e) {
  cat("❌ Error in analysis:\n")
  cat(e$message, "\n")
  cat("\n💡 Troubleshooting:\n")
  cat("   1. Make sure you've run '02_analyze_reliability.R' first\n")
  cat("   2. Check that window size", TARGET_WINDOW_SIZE, "exists in the data\n")
  cat("   3. Verify the data files are in the correct location\n")
})
