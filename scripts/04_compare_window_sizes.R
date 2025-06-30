# ========================================
# STEP 4: COMPARE ALL WINDOW SIZES
# ========================================
# Creates plots and summary tables comparing all window sizes
# Shows which window size gives the most precise results

# Clear workspace and load packages
rm(list = ls())
gc()

library(ggplot2)
library(dplyr)
library(tidyr)
devtools::load_all()

cat("=== COMPARING ALL WINDOW SIZES ===\n")

# ========================================
# SETTINGS
# ========================================

# Input files (from Step 2)
DETAILED_FILE <- "data/multi_window_reliability_detailed.rds"
SUMMARY_FILE <- "results/multi_window_reliability_summary.csv"

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
display_cols <- c("window_size", "n_final", "r_squared", "zone_90_width", "zone_95_width")
available_cols <- display_cols[display_cols %in% names(summary_table)]

if (length(available_cols) > 0) {
  display_data <- summary_table[, available_cols]
  display_data <- display_data[order(display_data$window_size), ]
  
  cat("\nWindow | Data Points | R²     | 90% Zone | 95% Zone\n")
  cat("-------|-------------|--------|----------|----------\n")
  
  for (i in seq_len(nrow(display_data))) {
    cat(sprintf("  %3d  |   %8d  | %6.4f |", 
               display_data$window_size[i],
               display_data$n_final[i],
               display_data$r_squared[i]))
    
    if ("zone_90_width" %in% names(display_data)) {
      if (is.finite(display_data$zone_90_width[i])) {
        cat(sprintf("  %6.3f  |", display_data$zone_90_width[i]))
      } else {
        cat("    ---   |")
      }
    }
    
    if ("zone_95_width" %in% names(display_data)) {
      if (is.finite(display_data$zone_95_width[i])) {
        cat(sprintf("  %6.3f", display_data$zone_95_width[i]))
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
reliability_cols <- c("zone_90_width", "zone_95_width")
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
if (any(c("zone_90_width", "zone_95_width") %in% names(summary_table))) {
  
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
      geom_line(size = 1.2) +
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
    
    ggsave("zone_width_comparison.png", p1, width = 12, height = 6, dpi = 300)
    cat("✓ Saved zone width comparison: zone_width_comparison.png\n")
  }
}

# Plot 2: Model quality (R²) comparison
if ("r_squared" %in% names(summary_table)) {
  p2 <- ggplot(summary_table, aes(x = window_size, y = r_squared)) +
    geom_line(color = "blue", size = 1.2) +
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
    ylim(0, max(summary_table$r_squared, na.rm = TRUE) * 1.1)
  
  ggsave("model_quality_comparison.png", p2, width = 10, height = 6, dpi = 300)
  cat("✓ Saved model quality comparison: model_quality_comparison.png\n")
}

# ========================================
# RECOMMENDATIONS
# ========================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("💡 RECOMMENDATIONS\n")
cat(rep("=", 80), "\n", sep = "")

if ("zone_90_width" %in% names(summary_table)) {
  valid_90 <- summary_table[is.finite(summary_table$zone_90_width), ]
  
  if (nrow(valid_90) > 0) {
    best_90 <- valid_90[which.min(valid_90$zone_90_width), ]
    
    cat("For most statistical applications:\n")
    cat(sprintf("→ Use window size %d (90%% zone width = %.3f)\n", 
               best_90$window_size, best_90$zone_90_width))
    
    cat("\nWhy this window size is optimal:\n")
    cat("- Smallest 90% reliability zone (most precise)\n")
    cat("- R² =", sprintf("%.4f", best_90$r_squared), 
        if (best_90$r_squared > 0.1) "(good model fit)" else "(flat model)\n")
    cat("- Analyzed", best_90$n_final, "data points\n")
    
    # Performance context
    zone_range <- range(valid_90$zone_90_width, na.rm = TRUE)
    improvement <- (max(zone_range) - min(zone_range)) / max(zone_range) * 100
    cat(sprintf("- Up to %.1f%% more precise than worst window size\n", improvement))
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

validate_data_format(datasets)

# ========================================
# RUN COMPARISON ANALYSIS
# ========================================

cat("\n--- Comparing Window Sizes ---\n")
start_time <- Sys.time()

all_results <- list()
comparison_summary <- data.frame()

for (i in seq_along(WINDOW_SIZES)) {
  window_size <- WINDOW_SIZES[i]
  
  cat("\n[", i, "/", length(WINDOW_SIZES), "] Analyzing window size:", window_size, "\n")
  
  tryCatch({
    # Run analysis for this window size
    results <- analyze_reliability(
      datasets = datasets,
      window_size = window_size,
      reliability_thresholds = RELIABILITY_THRESHOLDS,
      batch_size = BATCH_SIZE
    )
    
    # Store results
    all_results[[paste0("window_", window_size)]] <- results
    
    # Extract summary information
    summary_row <- data.frame(
      window_size = window_size,
      model_rsq = results$model_rsq,
      baseline_volatility = results$baseline_volatility,
      n_datapoints = results$meta$n_datapoints
    )
    
    # Add zone widths
    for (zone_name in names(results$zones)) {
      zone_info <- results$zones[[zone_name]]
      threshold_pct <- gsub("zone_", "", zone_name)
      width_col <- paste0("zone_", threshold_pct, "_width")
      summary_row[[width_col]] <- zone_info$width
    }
    
    comparison_summary <- rbind(comparison_summary, summary_row)
    
    cat("✓ Window size", window_size, "completed\n")
    
  }, error = function(e) {
    cat("❌ Window size", window_size, "failed:", e$message, "\n")
  })
  
  # Memory cleanup
  gc(verbose = FALSE)
}

end_time <- Sys.time()
total_duration <- as.numeric(difftime(end_time, start_time, units = "mins"))

cat("\n✓ All analyses completed in", round(total_duration, 2), "minutes\n")

# ========================================
# ANALYZE RESULTS
# ========================================

cat("\n--- Comparison Results ---\n")

if (nrow(comparison_summary) == 0) {
  stop("❌ No successful analyses. Check your data and window sizes.")
}

# Display summary table
cat("Summary Table:\n")
print(comparison_summary)

# Find optimal window sizes
cat("\nOptimal Window Sizes:\n")

# Best R² (model fit)
best_rsq_idx <- which.max(comparison_summary$model_rsq)
cat("- Best model fit (highest R²):", comparison_summary$window_size[best_rsq_idx], 
    "(R² =", round(comparison_summary$model_rsq[best_rsq_idx], 4), ")\n")

# Smallest zone widths for each threshold
for (threshold in RELIABILITY_THRESHOLDS) {
  threshold_pct <- round(threshold * 100)
  width_col <- paste0("zone_", threshold_pct, "_width")
  
  if (width_col %in% names(comparison_summary)) {
    valid_widths <- comparison_summary[!is.na(comparison_summary[[width_col]]), ]
    
    if (nrow(valid_widths) > 0) {
      best_width_idx <- which.min(valid_widths[[width_col]])
      optimal_window <- valid_widths$window_size[best_width_idx]
      optimal_width <- valid_widths[[width_col]][best_width_idx]
      
      cat("- Smallest", threshold_pct, "% zone:", optimal_window, 
          "(width =", round(optimal_width, 3), ")\n")
    } else {
      cat("- ", threshold_pct, "% threshold: No valid zones found\n")
    }
  }
}

# ========================================
# CREATE VISUALIZATIONS
# ========================================

cat("\n--- Creating Comparison Plots ---\n")

# Plot 1: Model quality (R²) by window size
p1 <- ggplot(comparison_summary, aes(x = window_size, y = model_rsq)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "blue", size = 3) +
  labs(
    title = "Model Quality by Window Size",
    subtitle = "Higher R² indicates better model fit",
    x = "Window Size",
    y = "Model R²"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = comparison_summary$window_size)

ggsave("model_quality_comparison.png", p1, width = 10, height = 6, dpi = 300)
cat("✓ Saved model quality plot\n")

# Plot 2: Zone widths by window size
if (any(RELIABILITY_THRESHOLDS %in% c(0.90, 0.95))) {
  
  # Prepare data for zone width plot
  zone_data <- data.frame()
  
  for (threshold in RELIABILITY_THRESHOLDS) {
    threshold_pct <- round(threshold * 100)
    width_col <- paste0("zone_", threshold_pct, "_width")
    
    if (width_col %in% names(comparison_summary)) {
      temp_data <- comparison_summary[!is.na(comparison_summary[[width_col]]), 
                                     c("window_size", width_col)]
      
      if (nrow(temp_data) > 0) {
        temp_data$threshold <- paste0(threshold_pct, "%")
        names(temp_data)[2] <- "zone_width"
        zone_data <- rbind(zone_data, temp_data)
      }
    }
  }
  
  if (nrow(zone_data) > 0) {
    p2 <- ggplot(zone_data, aes(x = window_size, y = zone_width, color = threshold)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      labs(
        title = "Reliability Zone Widths by Window Size",
        subtitle = "Smaller widths indicate more precise reliability regions",
        x = "Window Size",
        y = "Zone Width",
        color = "Reliability\nThreshold"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12)
      ) +
      scale_x_continuous(breaks = unique(zone_data$window_size))
    
    ggsave("zone_width_comparison.png", p2, width = 10, height = 6, dpi = 300)
    cat("✓ Saved zone width comparison plot\n")
  }
}

# ========================================
# SAVE RESULTS
# ========================================

cat("\n--- Saving Comparison Results ---\n")

# Save detailed results
detailed_file <- paste0(OUTPUT_NAME, "_detailed.rds")
saveRDS(all_results, detailed_file)

# Save summary table
summary_file <- paste0(OUTPUT_NAME, "_summary.csv")
write.csv(comparison_summary, summary_file, row.names = FALSE)

cat("✓ Saved detailed results:", detailed_file, "\n")
cat("✓ Saved summary table:", summary_file, "\n")

# ========================================
# RECOMMENDATIONS
# ========================================

cat("\n--- Recommendations ---\n")

if (nrow(comparison_summary) > 1) {
  
  # Recommend based on different criteria
  cat("Based on your results:\n\n")
  
  # For model quality
  best_rsq_window <- comparison_summary$window_size[which.max(comparison_summary$model_rsq)]
  cat("1. For best model fit: Use window size", best_rsq_window, "\n")
  
  # For precision (smallest zones)
  for (threshold in RELIABILITY_THRESHOLDS) {
    threshold_pct <- round(threshold * 100)
    width_col <- paste0("zone_", threshold_pct, "_width")
    
    if (width_col %in% names(comparison_summary)) {
      valid_data <- comparison_summary[!is.na(comparison_summary[[width_col]]), ]
      
      if (nrow(valid_data) > 0) {
        best_precision_window <- valid_data$window_size[which.min(valid_data[[width_col]])]
        cat("2. For", threshold_pct, "% precision: Use window size", best_precision_window, "\n")
      }
    }
  }
  
  # Overall recommendation
  cat("\n🎯 OVERALL RECOMMENDATION:\n")
  
  # Find windows that have both good R² and reasonable zone widths
  good_rsq_threshold <- 0.05  # Adjust based on your standards
  good_windows <- comparison_summary[comparison_summary$model_rsq >= good_rsq_threshold, ]
  
  if (nrow(good_windows) > 0) {
    # Among good models, find the one with smallest average zone width
    if ("zone_90_width" %in% names(good_windows)) {
      valid_good <- good_windows[!is.na(good_windows$zone_90_width), ]
      if (nrow(valid_good) > 0) {
        recommended_window <- valid_good$window_size[which.min(valid_good$zone_90_width)]
        cat("Use window size", recommended_window, "for best balance of model fit and precision\n")
      } else {
        cat("Use window size", best_rsq_window, "for best model fit\n")
      }
    } else {
      cat("Use window size", best_rsq_window, "for best model fit\n")
    }
  } else {
    cat("All models show poor fit (R² < 0.05). Consider:\n")
    cat("- Using different window sizes\n")
    cat("- Checking your data for sufficient variability\n")
    cat("- Trying different reliability thresholds\n")
  }
  
} else {
  cat("Only one window size was successfully analyzed.\n")
  cat("Try adding more window sizes to the WINDOW_SIZES list.\n")
}

cat("\n=== WINDOW SIZE COMPARISON COMPLETE ===\n")
cat("Review the plots and recommendations above.\n")
cat("Use the recommended window size in '02_analyze_reliability.R' for your final analysis.\n\n")
