# ========================================
# STEP 3: VIEW RESULTS FOR SINGLE WINDOW
# ========================================
# Deep-dive analysis of one specific window size
# Creates detailed visualizations and reports for understanding

# Clear workspace and load packages
rm(list = ls())
gc()

library(ggplot2)
library(dplyr)
devtools::load_all()

cat("=== SINGLE WINDOW DEEP-DIVE ANALYSIS ===\n")

# ========================================
# SETTINGS
# ========================================

# Which window size to analyze in detail
WINDOW_SIZE <- 20  # Change this to analyze different window sizes

# Input files
DETAILED_FILE <- "data/multi_window_reliability_detailed.rds"
SUMMARY_FILE <- "results/multi_window_reliability_summary.csv"

cat("Analyzing window size:", WINDOW_SIZE, "\n")
cat("Reading from:", DETAILED_FILE, "\n\n")

# ========================================
# RUN SINGLE WINDOW ANALYSIS
# ========================================

cat("Running detailed analysis for window size", WINDOW_SIZE, "...\n")

# Use the shared function to generate comprehensive analysis
analysis_results <- generate_single_window_analysis(
  window_size = WINDOW_SIZE,
  output_prefix = "single_window"
)

# ========================================
# DISPLAY RESULTS
# ========================================

cat("\n📊 ANALYSIS SUMMARY:\n")
cat("==========================================\n")

# Display summary statistics
if ("summary_stats" %in% names(analysis_results)) {
  for (i in seq_len(nrow(analysis_results$summary_stats))) {
    stat <- analysis_results$summary_stats[i, ]
    cat(sprintf("%-25s: %s\n", stat$Metric, stat$Value))
  }
}

cat("\n📈 PLOTS GENERATED:\n")
cat("==========================================\n")
plot_files <- list.files("results/", pattern = paste0("single_window_.*_", WINDOW_SIZE, ".png"), full.names = FALSE)
for (file in plot_files) {
  cat("  📈", file, "\n")
}

cat("\n📄 REPORT GENERATED:\n")
cat("==========================================\n")
report_files <- list.files("results/", pattern = paste0("single_window_report_", WINDOW_SIZE, ".txt"), full.names = FALSE)
for (file in report_files) {
  cat("  📝", file, "\n")
}

# ========================================
# RECOMMENDATIONS FOR THIS WINDOW SIZE
# ========================================

cat("\n💡 WINDOW SIZE", WINDOW_SIZE, "ASSESSMENT:\n")
cat("==========================================\n")

if ("window_data" %in% names(analysis_results)) {
  window_data <- analysis_results$window_data
  
  # Model quality assessment
  rsq <- window_data$model_rsq
  if (rsq > 0.7) {
    cat("✅ Excellent model fit (R² =", round(rsq, 3), ")\n")
  } else if (rsq > 0.3) {
    cat("✓ Good model fit (R² =", round(rsq, 3), ")\n")
  } else {
    cat("⚠️ Poor model fit (R² =", round(rsq, 3), ") - consider different window size\n")
  }
  
  # Zone width assessment
  if ("zones" %in% names(window_data)) {
    zone_95 <- window_data$zones$zone_95pct
    zone_90 <- window_data$zones$zone_90pct
    
    if (!is.na(zone_90$width)) {
      if (zone_90$width < 1.0) {
        cat("✅ Very precise 90% reliability zone (width =", round(zone_90$width, 3), ")\n")
      } else if (zone_90$width < 2.0) {
        cat("✓ Good 90% reliability zone (width =", round(zone_90$width, 3), ")\n")
      } else {
        cat("⚠️ Wide 90% reliability zone (width =", round(zone_90$width, 3), ") - less precise\n")
      }
    } else {
      cat("❌ No 90% reliability zone found - unreliable window size\n")
    }
    
    if (!is.na(zone_95$width)) {
      cat("  95% reliability zone width:", round(zone_95$width, 3), "\n")
    } else {
      cat("❌ No 95% reliability zone found\n")
    }
  }
  
cat("\n💡 HOW TO USE THESE RESULTS:\n")
cat("==========================================\n")
cat("• Scatter plot shows the raw volatility pattern\n")
cat("• Reliability curve shows where analysis is most trustworthy\n") 
cat("• Zone plot shows the specific regions for reliable analysis\n")
cat("• Use the zones to focus your analysis on reliable regions\n")

cat("\n🔧 TO ANALYZE A DIFFERENT WINDOW SIZE:\n")
cat("==========================================\n")
cat("• Change WINDOW_SIZE at the top of this script\n")
cat("• Re-run to get detailed analysis for that size\n")

cat("\n🎯 NEXT STEP: Run '04_compare_window_sizes.R' to find optimal window!\n")

cat("\n", rep("=", 80), "\n", sep = "")
cat("✅ SINGLE WINDOW ANALYSIS COMPLETE\n")
cat(rep("=", 80), "\n", sep = "")
