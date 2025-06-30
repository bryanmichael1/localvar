# ========================================
# STEP 4: COMPARE ALL WINDOW SIZES
# ========================================
# Creates plots and summary tables comparing all window sizes
# Shows which window size gives the most precise results
# Uses the single-window analysis function to generate detailed comparisons

# Clear workspace and load packages
rm(list = ls())
gc()

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
devtools::load_all()

cat("=== COMPARING ALL WINDOW SIZES ===\n")

# ========================================
# SETTINGS
# ========================================

# Reliability thresholds for analysis
RELIABILITY_THRESHOLDS <- c(0.95, 0.90, 0.85, 0.80)  # You can modify these

# Input files (from Step 2)
DETAILED_FILE <- "data/multi_window_reliability_detailed.rds"
SUMMARY_FILE <- "results/multi_window_reliability_summary.csv"

cat("Reliability thresholds:", paste(RELIABILITY_THRESHOLDS * 100, collapse = "%, "), "%\n")
cat("Reading from:", SUMMARY_FILE, "\n\n")

# ========================================
# LOAD RESULTS
# ========================================

if (!file.exists(SUMMARY_FILE)) {
  stop("❌ Summary file not found: ", SUMMARY_FILE, 
       "\n🔧 Run '02_analyze_reliability.R' first!")
}

cat("Loading comparison data...\n")
summary_table <- read.csv(SUMMARY_FILE)
cat("✓ Loaded data for", nrow(summary_table), "window sizes\n")

# ========================================
# SUMMARY TABLE
# ========================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("📊 WINDOW SIZE COMPARISON SUMMARY\n")
cat(rep("=", 80), "\n", sep = "")

# Display key columns
display_cols <- c("window_size", "n_datapoints", "model_rsq", "zone_90pct_width", "zone_95pct_width")
available_cols <- display_cols[display_cols %in% names(summary_table)]

if (length(available_cols) > 0) {
  display_data <- summary_table[, available_cols, drop = FALSE]
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
  
  if (col_name %in% names(summary_table)) {
    valid_data <- summary_table[is.finite(summary_table[[col_name]]), ]
    
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
if (any(c("zone_90pct_width", "zone_95pct_width") %in% names(summary_table))) {
  
  # Prepare data for plotting
  plot_data <- summary_table %>%
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
if ("model_rsq" %in% names(summary_table)) {
  p2 <- ggplot(summary_table, aes(x = window_size, y = model_rsq)) +
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
    scale_x_continuous(breaks = unique(summary_table$window_size)) +
    ylim(0, max(summary_table$model_rsq, na.rm = TRUE) * 1.1)
  
  ggsave("results/model_quality_comparison.png", p2, width = 10, height = 6, dpi = 300)
  cat("✓ Saved model quality comparison: model_quality_comparison.png\n")
}

# ========================================
# GENERATE DETAILED ANALYSIS FOR OPTIMAL WINDOW
# ========================================

cat("\n📊 DETAILED ANALYSIS FOR OPTIMAL WINDOW SIZE...\n")

if ("zone_90pct_width" %in% names(summary_table)) {
  valid_90 <- summary_table[is.finite(summary_table$zone_90pct_width), ]
  
  if (nrow(valid_90) > 0) {
    optimal_window <- valid_90$window_size[which.min(valid_90$zone_90pct_width)]
    
    cat("Running detailed analysis for optimal window size:", optimal_window, "\n")
    
    # Generate comprehensive single-window analysis for the optimal size
    tryCatch({
      optimal_analysis <- generate_single_window_analysis(
        window_size = optimal_window,
        output_prefix = "optimal_window",
        reliability_thresholds = RELIABILITY_THRESHOLDS
      )
      
      cat("✓ Generated detailed plots and report for optimal window size\n")
      
      # Show quick summary
      if ("summary_stats" %in% names(optimal_analysis)) {
        cat("\n📋 OPTIMAL WINDOW SUMMARY:\n")
        for (i in seq_len(min(6, nrow(optimal_analysis$summary_stats)))) {
          stat <- optimal_analysis$summary_stats[i, ]
          cat(sprintf("  %-20s: %s\n", stat$Metric, stat$Value))
        }
      }
      
    }, error = function(e) {
      cat("⚠️ Could not generate detailed analysis:", e$message, "\n")
    })
  }
}

# ========================================
# RECOMMENDATIONS
# ========================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("💡 RECOMMENDATIONS\n")
cat(rep("=", 80), "\n", sep = "")

if ("zone_90pct_width" %in% names(summary_table)) {
  valid_90 <- summary_table[is.finite(summary_table$zone_90pct_width), ]
  
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
  }
}

cat("\nGeneral guidelines:\n")
cat("- Smaller zone widths = more precise reliability estimates\n")
cat("- Higher R² = better model quality\n")
cat("- Consider your specific application's precision needs\n")

cat("\n🎯 NEXT STEP: Use the optimal window size in your main analysis!\n")

cat("\n", rep("=", 80), "\n", sep = "")
cat("✅ COMPARISON COMPLETE - Check the generated plots!\n")
cat(rep("=", 80), "\n", sep = "")
