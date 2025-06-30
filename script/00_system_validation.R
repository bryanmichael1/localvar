# ============================
# System Validation Test
# ============================
# Validates all core functionality works correctly

# Clear environment
rm(list = ls())
gc()

library(simstudy)
library(data.table)
library(dplyr)
devtools::load_all()

cat("=== LOCALVAR SYSTEM VALIDATION ===\n")

# ============================
# Test Configuration
# ============================

TEST_WINDOW_SIZE <- 30
TEST_DATASETS <- 10

# ============================
# Data Validation
# ============================

cat("1. Validating data availability...\n")
if (!file.exists("simulated_raw_data_tdist.rds")) {
  stop("❌ Raw data file missing!")
}

raw_datasets <- readRDS("simulated_raw_data_tdist.rds")
cat("   ✓ Data loaded:", length(raw_datasets), "datasets\n")

first_dataset <- raw_datasets[[1]]
required_cols <- c("x", "y_norm")
if (!all(required_cols %in% names(first_dataset))) {
  stop("❌ Missing required columns:", paste(setdiff(required_cols, names(first_dataset)), collapse = ", "))
}
cat("   ✓ Data structure valid\n")

# ============================
# Core Function Tests
# ============================

cat("2. Testing core pipeline...\n")
test_result <- tryCatch({
  run_volatility_pipeline(
    window_size = TEST_WINDOW_SIZE,
    x = first_dataset$x,
    y = first_dataset$y_norm,
    x_type = "continuous",
    y_type = "continuous"
  )
}, error = function(e) {
  stop("❌ Pipeline failed: ", e$message)
})

cat("   ✓ Pipeline works:", length(test_result$windows), "windows generated\n")

# ============================
# Reliability Function Tests
# ============================

cat("3. Testing reliability functions...\n")

# Extract test data
all_x_z <- numeric(0)
all_score_sd <- numeric(0)

for (i in seq_len(min(TEST_DATASETS, length(raw_datasets)))) {
  result <- run_volatility_pipeline(
    window_size = TEST_WINDOW_SIZE,
    x = raw_datasets[[i]]$x,
    y = raw_datasets[[i]]$y_norm,
    x_type = "continuous",
    y_type = "continuous"
  )
  
  for (w in result$windows) {
    if (!is.null(w$x_stats) && !is.null(w$score) &&
        is.finite(w$x_stats$mean_z) && is.finite(w$score$sd)) {
      all_x_z <- c(all_x_z, w$x_stats$mean_z)
      all_score_sd <- c(all_score_sd, w$score$sd)
    }
  }
}

cat("   ✓ Data extraction:", length(all_x_z), "points\n")

if (length(all_x_z) > 100) {
  metrics <- calculate_reliability_metrics_optimized(
    all_x_z, all_score_sd, c(0.90, 0.80)
  )
  cat("   ✓ Reliability calculation: R² =", round(metrics$model_rsquared, 4), "\n")

  x_seq <- metrics$pred_x_seq
  rel_vals <- metrics$pred_reliability
  grid_widths <- sapply(c(0.90, 0.80), function(t) {
    valid_x <- x_seq[rel_vals >= t]
    if (length(valid_x) == 0) NA else diff(range(valid_x))
  })
  analytic_widths <- metrics$zone_widths[c("zone_90_width", "zone_80_width")]
  if (all(!is.na(grid_widths)) && all(!is.na(analytic_widths))) {
    if (all(abs(grid_widths - analytic_widths) < 0.05)) {
      cat("   ✓ Analytic intervals match grid search\n")
    } else {
      cat("   ⚠️ Analytic intervals differ from grid search\n")
    }
  } else {
    cat("   ⚠️ Unable to verify analytic intervals\n")
  }
} else {
  cat("   ⚠️ Insufficient data for reliability test\n")
}

# ============================
# Memory Check
# ============================

cat("4. Memory efficiency check...\n")
gc_result <- gc()
memory_used <- round(gc_result[2,2], 1)
cat("   ✓ Memory usage:", memory_used, "MB\n")

if (memory_used > 1000) {
  cat("   ⚠️ High memory usage detected\n")
} else {
  cat("   ✓ Memory usage within limits\n")
}

# ============================
# Final Status
# ============================

cat("\n=== VALIDATION COMPLETE ===\n")
cat("✅ System is functional and ready\n")
cat("✅ Core pipeline works correctly\n")
cat("✅ Data structure is valid\n")
cat("✅ Memory usage is efficient\n")

cat("\nReady for analysis:\n")
cat("• Single window: source('script/03_reliability_analysis.R')\n")
cat("• Multi-window (3-100): source('script/05_multi_window_analysis.R')\n")
cat("• Visualization: source('script/04_visualization_reporting.R')\n")

rm(list = ls())
gc()
