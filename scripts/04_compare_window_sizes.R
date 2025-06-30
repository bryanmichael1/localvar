# ========================================
# STEP 4: COMPARE ALL WINDOW SIZES
# ========================================
# Runs single-window analysis for ALL window sizes, then compares them
# This extends script 03 by applying it to multiple windows and finding the optimal one

# Clear workspace and load packages
rm(list = ls())
gc()

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
devtools::load_all()

cat("=== COMPARING ALL WINDOW SIZES ===\n")
cat("This script extends script 03 by running single-window analysis for ALL windows\n\n")

# ========================================
# SETTINGS
# ========================================

# Reliability thresholds for analysis
RELIABILITY_THRESHOLDS <- c(0.95, 0.90, 0.85, 0.80)  # You can modify these

# Input files (from Step 2)
DETAILED_FILE <- "data/multi_window_reliability_detailed.rds"

cat("Reliability thresholds:", paste(RELIABILITY_THRESHOLDS * 100, collapse = "%, "), "%\n")
cat("Reading from:", DETAILED_FILE, "\n\n")

# ========================================
# LOAD RAW DATA AND GET AVAILABLE WINDOW SIZES
# ========================================

if (!file.exists(DETAILED_FILE)) {
  stop("❌ Detailed results not found: ", DETAILED_FILE, 
       "\n🔧 Run '02_analyze_reliability.R' first!")
}

cat("Loading raw data...\n")
raw_results <- readRDS(DETAILED_FILE)
window_sizes <- sapply(raw_results, function(x) x$window_size)
cat("✓ Found data for", length(window_sizes), "window sizes:", paste(sort(window_sizes), collapse = ", "), "\n\n")

# ========================================
# RUN SINGLE-WINDOW ANALYSIS FOR ALL WINDOW SIZES
# ========================================

cat("🔄 RUNNING ANALYSIS FOR ALL WINDOW SIZES...\n")
cat("This extends script 03 by analyzing all windows systematically\n\n")

all_window_results <- list()
summary_data <- data.frame()

for (i in seq_along(window_sizes)) {
  window_size <- window_sizes[i]
  
  cat("📊 Analyzing window size", window_size, paste0("(", i, "/", length(window_sizes), ")"), "...\n")
  
  tryCatch({
    # Run the same single-window analysis as script 03
    window_analysis <- generate_single_window_analysis(
      window_size = window_size,
      detailed_results = raw_results,
      output_prefix = "multi_window",
      reliability_thresholds = RELIABILITY_THRESHOLDS
    )
    
    # Store the results
    all_window_results[[paste0("window_", window_size)]] <- window_analysis
    
    # Extract summary data for comparison
    window_data <- window_analysis$window_data
    summary_row <- data.frame(
      window_size = window_size,
      n_datapoints = length(window_data$x_z_data),
      model_rsq = window_data$model_rsq,
      baseline_volatility = window_data$baseline_volatility
    )
    
    # Add zone widths if available
    if ("zones" %in% names(window_data)) {
      for (zone_name in names(window_data$zones)) {
        zone <- window_data$zones[[zone_name]]
        width_col <- paste0(zone_name, "_width")
        summary_row[[width_col]] <- if (is.na(zone$width)) NA else zone$width
      }
    }
    
    summary_data <- rbind(summary_data, summary_row)
    
    cat("  ✓ Complete! Model R² =", round(window_data$model_rsq, 4), "\n")
    
  }, error = function(e) {
    cat("  ❌ Failed:", e$message, "\n")
  })
}

cat("\n📊 ANALYSIS SUMMARY:\n")
cat("==========================================\n")
cat("Successfully analyzed", length(window_sizes), "window sizes:\n")
for (i in seq_len(min(5, nrow(summary_data)))) {
  row <- summary_data[i, ]
  cat(sprintf("  Window %3d: R² = %6.4f, %d data points\n", 
             row$window_size, row$model_rsq, row$n_datapoints))
}

# Save the comprehensive summary
write.csv(summary_data, "results/multi_window_reliability_summary.csv", row.names = FALSE)
cat("\n💾 Saved comprehensive summary: multi_window_reliability_summary.csv\n")

# ========================================
# DISPLAY COMPARISON SUMMARY
# ========================================

cat(rep("=", 80), "\n", sep = "")
cat("📊 WINDOW SIZE COMPARISON SUMMARY\n")
cat(rep("=", 80), "\n", sep = "")

# Display key columns
display_cols <- c("window_size", "n_datapoints", "model_rsq", "zone_90pct_width", "zone_95pct_width")
available_cols <- display_cols[display_cols %in% names(summary_data)]

if (length(available_cols) > 0) {
  display_data <- summary_data[, available_cols, drop = FALSE]
  display_data <- display_data[order(display_data$window_size), ]
  
  cat("\nWindow | Data Points | R²     | 90% Zone | 95% Zone\n")
  cat("-------|-------------|--------|----------|----------\n")
  
  for (i in seq_len(nrow(display_data))) {
    cat(sprintf("  %3d  |   %8d  | %6.4f |", 
               display_data$window_size[i],
               display_data$n_datapoints[i],
               display_data$model_rsq[i]))
    
    if ("zone_90pct_width" %in% names(display_data)) {
      if (is.finite(display_data$zone_90pct_width[i])) {
        cat(sprintf("  %6.3f  |", display_data$zone_90pct_width[i]))
      } else {
        cat("    ---   |")
      }
    }
    
    if ("zone_95pct_width" %in% names(display_data)) {
      if (is.finite(display_data$zone_95pct_width[i])) {
        cat(sprintf("  %6.3f", display_data$zone_95pct_width[i]))
      } else {
        cat("    ---")
      }
    }
    
    cat("\n")
  }
}

# ========================================
# FIND OPTIMAL WINDOW SIZES
# ========================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("🏆 OPTIMAL WINDOW SIZES\n")
cat(rep("=", 80), "\n", sep = "")

# For different reliability levels
reliability_cols <- c("zone_90pct_width", "zone_95pct_width")
reliability_names <- c("90%", "95%")

for (i in seq_along(reliability_cols)) {
  col_name <- reliability_cols[i]
  rel_name <- reliability_names[i]
  
  if (col_name %in% names(summary_data)) {
    valid_data <- summary_data[is.finite(summary_data[[col_name]]), ]
    
    if (nrow(valid_data) > 0) {
      best_idx <- which.min(valid_data[[col_name]])
      best_window <- valid_data$window_size[best_idx]
      best_width <- valid_data[[col_name]][best_idx]
      
      cat(sprintf("%s reliability: Window size %d (zone width = %.3f)\n", 
                 rel_name, best_window, best_width))
      
      # Show top 3
      sorted_data <- valid_data[order(valid_data[[col_name]]), ]
      cat("  Top 3 choices:\n")
      for (j in seq_len(min(3, nrow(sorted_data)))) {
        cat(sprintf("    %d. Window %d (width = %.3f)\n", 
                   j, sorted_data$window_size[j], sorted_data[[col_name]][j]))
      }
      cat("\n")
    }
  }
}

# ========================================
# CREATE COMPARISON PLOTS
# ========================================

cat("📈 CREATING COMPARISON PLOTS...\n")

# Plot 1: Zone widths comparison
if (any(c("zone_90pct_width", "zone_95pct_width") %in% names(summary_data))) {
  
  # Prepare data for plotting
  plot_data <- summary_data %>%
    select(window_size, contains("zone_")) %>%
    pivot_longer(cols = -window_size, names_to = "threshold", values_to = "width") %>%
    filter(!is.na(width) & is.finite(width)) %>%
    mutate(
      threshold_pct = as.numeric(str_extract(threshold, "\\d+")),
      threshold_label = paste0(threshold_pct, "%")
    )
  
  if (nrow(plot_data) > 0) {
    p1 <- ggplot(plot_data, aes(x = window_size, y = width, color = threshold_label)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      labs(
        title = "Reliability Zone Widths by Window Size",
        subtitle = "Smaller zone width = more precise method",
        x = "Window Size",
        y = "Zone Width",
        color = "Reliability\nThreshold"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9)
      ) +
      scale_x_continuous(breaks = unique(plot_data$window_size))
    
    ggsave("results/zone_width_comparison.png", p1, width = 12, height = 6, dpi = 300)
    cat("✓ Saved zone width comparison: zone_width_comparison.png\n")
  }
}

# Plot 2: Model quality (R²) comparison
if ("model_rsq" %in% names(summary_data)) {
  p2 <- ggplot(summary_data, aes(x = window_size, y = model_rsq)) +
    geom_line(color = "blue", linewidth = 1.2) +
    geom_point(color = "blue", size = 3) +
    labs(
      title = "Model Quality (R²) by Window Size",
      subtitle = "Higher R² = better model fit",
      x = "Window Size",
      y = "R² (Model Quality)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    ) +
    scale_x_continuous(breaks = unique(summary_data$window_size)) +
    ylim(0, max(summary_data$model_rsq, na.rm = TRUE) * 1.1)
  
  ggsave("results/model_quality_comparison.png", p2, width = 10, height = 6, dpi = 300)
  cat("✓ Saved model quality comparison: model_quality_comparison.png\n")
}

# ========================================
# RECOMMENDATIONS
# ========================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("💡 RECOMMENDATIONS\n")
cat(rep("=", 80), "\n", sep = "")

if ("zone_90pct_width" %in% names(summary_data)) {
  valid_90 <- summary_data[is.finite(summary_data$zone_90pct_width), ]
  
  if (nrow(valid_90) > 0) {
    best_90 <- valid_90[which.min(valid_90$zone_90pct_width), ]
    
    cat("For most statistical applications:\n")
    cat(sprintf("→ Use window size %d (90%% zone width = %.3f)\n", 
               best_90$window_size, best_90$zone_90pct_width))
    
    cat("\nWhy this window size is optimal:\n")
    cat("- Smallest 90% reliability zone (most precise)\n")
    cat("- R² =", sprintf("%.4f", best_90$model_rsq), 
        if (best_90$model_rsq > 0.1) "(good model fit)" else "(flat model)\n")
    cat("- Analyzed", best_90$n_datapoints, "data points\n")
    
    cat("\n📈 DETAILED ANALYSIS FILES GENERATED:\n")
    cat("All window sizes have been analyzed with detailed plots and reports!\n")
    cat("Check the results/ folder for:\n")
    cat("- multi_window_scatter_*.png - Scatter plots for each window\n")
    cat("- multi_window_reliability_*.png - Reliability curves\n") 
    cat("- multi_window_zones_*.png - Zone visualizations\n")
    cat("- multi_window_report_*.txt - Detailed text reports\n")
  }
}

cat("\nGeneral guidelines:\n")
cat("- Smaller zone widths = more precise reliability estimates\n")
cat("- Higher R² = better model quality\n")
cat("- Consider your specific application's precision needs\n")

cat("\n🎯 RECOMMENDED WORKFLOW:\n")
cat("1. Use this script (04) to find optimal window size across all options\n")
cat("2. Use script 03 to do deep-dive analysis on your chosen window size\n")
cat("3. Focus your main analysis on the reliable zones identified\n")

cat("\n", rep("=", 80), "\n", sep = "")
cat("✅ MULTI-WINDOW COMPARISON COMPLETE!\n")
cat("📊 Analyzed", length(window_sizes), "window sizes with detailed reports\n")
cat("📈 Generated comparison plots and identified optimal settings\n")
cat(rep("=", 80), "\n", sep = "")
