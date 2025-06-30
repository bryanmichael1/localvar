# ========================================
# STEP 4: COMPARE ALL WINDOW SIZES
# ========================================
# Systematically analyzes all window sizes and compares their performance
# Uses the shared analysis function to ensure consistency across window sizes

# Clear workspace and load packages
rm(list = ls())
gc()

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(scales)

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

cat("=== COMPARING ALL WINDOW SIZES ===\n")

# ========================================
# SETTINGS (from config.R - modify config.R to change these)
# ========================================

# Use configuration values
RELIABILITY_THRESHOLDS <- CONFIG$reliability_thresholds
DETAILED_FILE <- CONFIG$get_path("detailed_results")
SUMMARY_FILE <- CONFIG$get_path("summary_results")

cat("📊 Reliability thresholds:", paste(RELIABILITY_THRESHOLDS, collapse = ", "), "\n")
cat("📂 Reading from:", DETAILED_FILE, "\n\n")

# ========================================
# LOAD AVAILABLE WINDOW SIZES
# ========================================

if (!file.exists(DETAILED_FILE)) {
  stop("❌ Detailed results not found: ", DETAILED_FILE, 
       "\n🔧 Run '02_analyze_reliability.R' first!")
}

cat("Loading available window sizes...\n")
detailed_results <- readRDS(DETAILED_FILE)

# Debug: Check what keys we have
cat("Raw keys found:", paste(names(detailed_results), collapse = ", "), "\n")

# Extract numeric window sizes from keys like "window_3", "window_5", etc.
window_keys <- names(detailed_results)[grepl("^window_\\d+$", names(detailed_results))]
available_window_sizes <- sort(as.numeric(sub("window_", "", window_keys)))

cat("✓ Found", length(available_window_sizes), "window sizes:", paste(available_window_sizes, collapse = ", "), "\n\n")

# ========================================
# RUN ANALYSIS FOR ALL WINDOW SIZES
# ========================================

cat("🔄 Running analysis for all window sizes...\n")
cat("This will collect data from all window sizes (no individual plots).\n\n")

all_results <- list()
comparison_data <- data.frame()

for (window_size in available_window_sizes) {
  cat("📊 Analyzing window size", window_size, "...\n")
  
  tryCatch({
    # Get the window data directly without generating plots
    window_key <- paste0("window_", window_size)
    window_data <- detailed_results[[window_key]]
    
    # Calculate reliability zones if not already present
    if (!"zones" %in% names(window_data)) {
      zone_analysis <- calculate_reliability_zones(
        window_data$x_z_data, 
        window_data$score_sd_data, 
        RELIABILITY_THRESHOLDS
      )
      window_data <- c(window_data, zone_analysis)
    }
    
    # Store results
    all_results[[as.character(window_size)]] <- window_data
    
    # Extract summary data for comparison
    summary_row <- data.frame(
      window_size = window_size,
      model_rsq = window_data$model_rsq,
      baseline_volatility = window_data$baseline_volatility,
      n_datapoints = length(window_data$x_z_data)
    )
    
    # Add zone widths and coverage
    if ("zones" %in% names(window_data)) {
      for (threshold in RELIABILITY_THRESHOLDS) {
        zone_name <- paste0("zone_", round(threshold * 100), "pct")
        if (zone_name %in% names(window_data$zones)) {
          zone <- window_data$zones[[zone_name]]
          summary_row[[paste0(zone_name, "_width")]] <- zone$width
          summary_row[[paste0(zone_name, "_coverage")]] <- zone$width / diff(range(window_data$x_z_data, na.rm = TRUE))
        }
      }
    }
    
    comparison_data <- rbind(comparison_data, summary_row)
    cat("  ✓ Complete (R² =", round(window_data$model_rsq, 4), ")\n")
    
  }, error = function(e) {
    cat("  ❌ Error analyzing window size", window_size, ":", e$message, "\n")
  })
}

cat("\n✅ Analysis complete for", nrow(comparison_data), "window sizes!\n\n")

# Check if we have any data to work with
if (nrow(comparison_data) == 0) {
  stop("❌ No window sizes were successfully analyzed. Check that:\n",
       "   1. The detailed results file contains valid data\n",
       "   2. The generate_single_window_analysis function is working\n",
       "   3. Window sizes in the data are numeric")
}

# ========================================
# CLEANUP OLD FILES
# ========================================

cat("🧹 Cleaning up old individual window plot files...\n")

# Remove old individual comparison plots that are no longer needed
old_patterns <- c(
  "comparison_baseline_volatility.png",
  "comparison_model_quality.png", 
  "comparison_zone_widths.png",
  "comparison_window_*"
)

for (pattern in old_patterns) {
  old_files <- list.files("results/", pattern = glob2rx(pattern), full.names = TRUE)
  if (length(old_files) > 0) {
    cat("  Removing", length(old_files), "old files matching:", pattern, "\n")
    file.remove(old_files)
  }
}

cat("  ✓ Cleanup complete\n\n")

# ========================================
# CREATE COMPREHENSIVE VISUALIZATIONS
# ========================================

cat("📈 Creating comprehensive comparison visualizations...\n")

# Custom theme with background for readability
theme_analysis <- theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.background = element_rect(fill = "white", color = "gray90", linewidth = 1),
    panel.background = element_rect(fill = "gray98", color = "gray95"),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.2),
    legend.background = element_rect(fill = "white", color = "gray90"),
    strip.background = element_rect(fill = "gray90", color = "gray80")
  )

# 1. Combined Model Quality and Baseline Volatility
p1 <- comparison_data %>%
  select(window_size, model_rsq, baseline_volatility) %>%
  # Filter out invalid values before plotting
  filter(!is.na(model_rsq) & !is.na(baseline_volatility) & 
         is.finite(model_rsq) & is.finite(baseline_volatility)) %>%
  pivot_longer(cols = -window_size, names_to = "metric", values_to = "value") %>%
  # Additional filter after pivot to ensure all values are valid
  filter(!is.na(value) & is.finite(value)) %>%
  mutate(
    metric_label = case_when(
      metric == "model_rsq" ~ "Model Quality (R²)",
      metric == "baseline_volatility" ~ "Baseline Volatility"
    )
  ) %>%
  ggplot(aes(x = window_size, y = value, color = metric_label)) +
  geom_point(size = 3, alpha = 0.8) +    geom_line(alpha = 0.7, linewidth = 1) +
  facet_wrap(~metric_label, scales = "free_y", ncol = 1) +
  labs(
    title = "Model Performance Across Window Sizes",
    subtitle = "Higher R² = better model fit, Higher volatility = more precise estimates",
    x = "Window Size", 
    y = "Value",
    color = "Metric"
  ) +
  scale_color_manual(values = c("Model Quality (R²)" = "darkgreen", "Baseline Volatility" = "red")) +
  theme_analysis

ggsave("results/comprehensive_model_performance.png", p1, width = 12, height = 8, dpi = 300)
cat("  ✓ Saved: comprehensive_model_performance.png\n")

# 2. Reliability Zone Coverage Heatmap
coverage_cols <- names(comparison_data)[grepl("zone_.*_coverage", names(comparison_data))]
if (length(coverage_cols) > 0) {
  cat("📊 Creating reliability zone coverage heatmap with", length(coverage_cols), "coverage types...\n")
  
  heatmap_data <- comparison_data %>%
    select(window_size, all_of(coverage_cols)) %>%
    pivot_longer(cols = -window_size, names_to = "reliability_threshold", values_to = "coverage") %>%
    mutate(
      threshold_pct = str_extract(reliability_threshold, "\\d+"),
      threshold_label = paste0(threshold_pct, "% Reliability")
    ) %>%
    filter(!is.na(coverage) & is.finite(coverage) & coverage >= 0 & coverage <= 1)  # Coverage should be between 0 and 1
  
  # Report filtering results
  n_total_before <- nrow(comparison_data) * length(coverage_cols)
  n_filtered <- n_total_before - nrow(heatmap_data)
  if (n_filtered > 0) {
    cat("  ⚠️ Filtered out", n_filtered, "invalid coverage data points\n")
  }
  cat("  📊 Valid coverage data points:", nrow(heatmap_data), "\n")
  
  if (nrow(heatmap_data) > 0) {
    p2 <- ggplot(heatmap_data, aes(x = factor(window_size), y = threshold_label, fill = coverage)) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.2f", coverage)), color = "white", fontface = "bold", size = 3) +
      scale_fill_gradient2(
        low = "red", mid = "yellow", high = "darkgreen",
        midpoint = 0.5, name = "Coverage\nRatio",
        labels = scales::percent_format(),
        limits = c(0, 1)  # Explicitly set limits
      ) +
      labs(
        title = "Reliability Zone Coverage Heatmap",
        subtitle = "Higher coverage = more of the data range is reliable at this threshold",
        x = "Window Size",
        y = "Reliability Threshold"
      ) +
      theme_analysis +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave("results/comprehensive_reliability_heatmap.png", p2, width = 12, height = 6, dpi = 300)
    cat("  ✓ Saved: comprehensive_reliability_heatmap.png\n")
  } else {
    cat("  ⚠️ No valid coverage data found after filtering\n")
  }
} else {
  cat("  ⚠️ No coverage columns found in comparison data\n")
}

# 3. All Reliability Curves Stacked
cat("📈 Creating stacked reliability curves...\n")
reliability_data <- data.frame()

for (window_size in names(all_results)) {
  window_data <- all_results[[window_size]]
  if ("reliability_curve" %in% names(window_data)) {
    curve_data <- window_data$reliability_curve
    curve_data$window_size <- as.numeric(window_size)
    reliability_data <- rbind(reliability_data, curve_data)
  }
}

cat("  📊 Total reliability curve data points:", nrow(reliability_data), "\n")

if (nrow(reliability_data) > 0) {
  # Filter out invalid values in the reliability curve data
  reliability_data_clean <- reliability_data %>%
    filter(!is.na(x_z) & !is.na(reliability) & 
           is.finite(x_z) & is.finite(reliability) &
           reliability >= 0 & reliability <= 1)  # Reliability should be between 0 and 1
  
  # Report filtering results
  n_filtered <- nrow(reliability_data) - nrow(reliability_data_clean)
  if (n_filtered > 0) {
    cat("  ⚠️ Filtered out", n_filtered, "invalid data points (NA, infinite, or out-of-range reliability)\n")
  }
  cat("  📊 Valid data points for plotting:", nrow(reliability_data_clean), "\n")
  
  if (nrow(reliability_data_clean) > 0) {
    p3 <- ggplot(reliability_data_clean, aes(x = x_z, y = reliability, color = factor(window_size))) +
      geom_line(alpha = 0.7, linewidth = 0.8) +
      geom_hline(yintercept = c(0.005, 0.01, 0.015, 0.02), linetype = "dashed", alpha = 0.5, color = "gray40") +
      labs(
        title = "Reliability Curves for All Window Sizes",
        subtitle = "Higher reliability = more trustworthy analysis in that region",
        x = "Standardized Position (z-score)",
        y = "Local Reliability",
        color = "Window\nSize"
      ) +
      scale_color_viridis_d(name = "Window\nSize") +
      scale_y_continuous(limits = c(0, max(reliability_data_clean$reliability) * 1.1), 
                        labels = scales::percent_format()) +
      theme_analysis
    
    ggsave("results/comprehensive_reliability_curves.png", p3, width = 14, height = 8, dpi = 300)
    cat("  ✓ Saved: comprehensive_reliability_curves.png\n")
  } else {
    cat("  ⚠️ No valid reliability curve data found after filtering\n")
  }
}

# 4. Zone Width Comparison (Enhanced)
zone_width_cols <- names(comparison_data)[grepl("zone_.*_width", names(comparison_data))]
if (length(zone_width_cols) > 0) {
  cat("📊 Creating zone width comparison with", length(zone_width_cols), "zone types...\n")
  
  plot_data <- comparison_data %>%
    select(window_size, all_of(zone_width_cols)) %>%
    pivot_longer(cols = -window_size, names_to = "zone_type", values_to = "width") %>%
    mutate(
      confidence = str_extract(zone_type, "\\d+"),
      confidence_label = case_when(
        confidence == "20" ~ "2.0% Reliability",
        confidence == "15" ~ "1.5% Reliability", 
        confidence == "10" ~ "1.0% Reliability",
        confidence == "8" ~ "0.8% Reliability",
        confidence == "5" ~ "0.5% Reliability",
        confidence == "3" ~ "0.3% Reliability",
        TRUE ~ paste0(confidence, "% Reliability")
      )
    ) %>%
    filter(!is.na(width) & is.finite(width) & width > 0)  # Additional filter for positive widths
  
  # Report filtering results
  n_total_before <- nrow(comparison_data) * length(zone_width_cols)
  n_filtered <- n_total_before - nrow(plot_data)
  if (n_filtered > 0) {
    cat("  ⚠️ Filtered out", n_filtered, "invalid zone width data points\n")
  }
  cat("  📊 Valid zone width data points:", nrow(plot_data), "\n")
  
  if (nrow(plot_data) > 0) {
    # Generate colors and shapes dynamically based on available data
    unique_confidence <- unique(plot_data$confidence_label)
    n_levels <- length(unique_confidence)
    
    # Use a color palette that works for any number of levels
    colors <- rainbow(n_levels, start = 0, end = 0.8)
    names(colors) <- unique_confidence
    
    # Use different shapes (cycling if more than 6 levels)
    shapes <- c(16, 17, 15, 18, 4, 8)
    if (n_levels > 6) shapes <- rep(shapes, ceiling(n_levels/6))
    shapes <- shapes[1:n_levels]
    names(shapes) <- unique_confidence
    
    p4 <- ggplot(plot_data, aes(x = window_size, y = width, color = confidence_label, shape = confidence_label)) +
      geom_point(size = 3, alpha = 0.8) +
      geom_line(alpha = 0.7, linewidth = 1) +
      scale_color_manual(values = colors) +
      scale_shape_manual(values = shapes) +
      labs(
        title = "Reliability Zone Widths Across Window Sizes",
        subtitle = "Smaller widths = more precise reliability estimates",
        x = "Window Size", 
        y = "Zone Width",
        color = "Reliability Level",
        shape = "Reliability Level"
      ) +
      theme_analysis
    
    ggsave("results/comprehensive_zone_widths.png", p4, width = 12, height = 8, dpi = 300)
    cat("  ✓ Saved: comprehensive_zone_widths.png\n")
  } else {
    cat("  ⚠️ No valid zone width data found after filtering\n")
  }
} else {
  cat("  ⚠️ No zone width columns found in comparison data\n")
}

# Save comparison data
write.csv(comparison_data, "results/comparison_summary.csv", row.names = FALSE)
cat("  ✓ Saved: comparison_summary.csv\n\n")

# ========================================
# CREATE COMPREHENSIVE REPORT
# ========================================

cat("📝 Creating comprehensive analysis report...\n")

# Generate comprehensive report
report_lines <- c(
  "COMPREHENSIVE WINDOW SIZE ANALYSIS REPORT",
  "=========================================",
  paste("Generated:", Sys.time()),
  "",
  "EXECUTIVE SUMMARY:",
  "=================",
  paste("• Analyzed", nrow(comparison_data), "window sizes ranging from", min(comparison_data$window_size), "to", max(comparison_data$window_size)),
  paste("• Best model quality (R²):", sprintf("%.4f", max(comparison_data$model_rsq)), "at window size", comparison_data$window_size[which.max(comparison_data$model_rsq)]),
  paste("• Highest baseline volatility:", sprintf("%.4f", max(comparison_data$baseline_volatility)), "at window size", comparison_data$window_size[which.max(comparison_data$baseline_volatility)]),
  "",
  "OPTIMAL WINDOW SIZES BY RELIABILITY THRESHOLD:",
  "==============================================",
  ""
)

# Add optimal recommendations for each threshold
for (threshold in RELIABILITY_THRESHOLDS) {
  zone_col <- paste0("zone_", round(threshold * 100), "pct_width")
  if (zone_col %in% names(comparison_data)) {
    valid_data <- comparison_data[is.finite(comparison_data[[zone_col]]), ]
    if (nrow(valid_data) > 0) {
      best_idx <- which.min(valid_data[[zone_col]])
      best_window <- valid_data$window_size[best_idx]
      best_width <- valid_data[[zone_col]][best_idx]
      
      report_lines <- c(report_lines,
        paste0(round(threshold * 100), "% Reliability:"),
        paste0("  Optimal window size: ", best_window),
        paste0("  Zone width: ", sprintf("%.3f", best_width)),
        paste0("  Model R²: ", sprintf("%.4f", valid_data$model_rsq[best_idx])),
        ""
      )
    }
  }
}

report_lines <- c(report_lines,
  "DETAILED RESULTS BY WINDOW SIZE:",
  "================================",
  ""
)

# Add details for each window size
for (i in seq_len(nrow(comparison_data))) {
  row_data <- comparison_data[i, ]
  window_info <- c(
    paste("WINDOW SIZE:", row_data$window_size),
    paste("  Model Quality (R²):", sprintf("%.4f", row_data$model_rsq)),
    paste("  Baseline Volatility:", sprintf("%.4f", row_data$baseline_volatility)),
    paste("  Data Points:", row_data$n_datapoints)
  )
  
  # Add zone information
  for (threshold in RELIABILITY_THRESHOLDS) {
    zone_width_col <- paste0("zone_", round(threshold * 100), "pct_width")
    zone_coverage_col <- paste0("zone_", round(threshold * 100), "pct_coverage")
    
    if (zone_width_col %in% names(row_data) && !is.na(row_data[[zone_width_col]])) {
      coverage_text <- ""
      if (zone_coverage_col %in% names(row_data) && !is.na(row_data[[zone_coverage_col]])) {
        coverage_text <- paste0(" (", sprintf("%.1f%%", row_data[[zone_coverage_col]] * 100), " coverage)")
      }
      window_info <- c(window_info,
        paste0("  ", round(threshold * 100), "% zone width: ", sprintf("%.3f", row_data[[zone_width_col]]), coverage_text)
      )
    }
  }
  
  window_info <- c(window_info, "")
  report_lines <- c(report_lines, window_info)
}

report_lines <- c(report_lines,
  "INTERPRETATION GUIDE:",
  "====================",
  "• Model R² shows how well the quadratic volatility model fits the data",
  "• Baseline volatility indicates the overall precision of estimates",
  "• Zone widths show the range where analysis is reliable at each threshold", 
  "• Coverage shows what percentage of the data range is reliable",
  "• Smaller zone widths = more precise reliability estimates",
  "• Higher coverage = more of your data is reliably analyzable",
  "",
  "VISUALIZATION FILES:",
  "===================",
  "• comprehensive_model_performance.png - Model quality and volatility trends",
  "• comprehensive_reliability_heatmap.png - Coverage by window size and threshold",
  "• comprehensive_reliability_curves.png - All reliability curves overlaid",
  "• comprehensive_zone_widths.png - Zone width comparison across thresholds",
  "",
  "RECOMMENDATIONS:",
  "================"
)

# Find best overall recommendation
if ("zone_95pct_width" %in% names(comparison_data)) {
  valid_95 <- comparison_data[is.finite(comparison_data$zone_95pct_width), ]
  if (nrow(valid_95) > 0) {
    best_95 <- valid_95[which.min(valid_95$zone_95pct_width), ]
    report_lines <- c(report_lines,
      paste0("For most applications, use window size ", best_95$window_size, ":"),
      paste0("• Most precise 95% reliability zone (width = ", sprintf("%.3f", best_95$zone_95pct_width), ")"),
      paste0("• Model quality R² = ", sprintf("%.4f", best_95$model_rsq)),
      paste0("• Baseline volatility = ", sprintf("%.4f", best_95$baseline_volatility))
    )
  }
}

# Save comprehensive report
report_file <- "results/comprehensive_analysis_report.txt"
writeLines(report_lines, report_file)
cat("✓ Saved: comprehensive_analysis_report.txt\n\n")

# ========================================
# FIND OPTIMAL WINDOW SIZES
# ========================================

cat("🏆 OPTIMAL WINDOW SIZES\n")
cat(rep("=", 80), "\n", sep = "")

# For different reliability levels
for (threshold in RELIABILITY_THRESHOLDS) {
  zone_col <- paste0("zone_", round(threshold * 100), "pct_width")
  rel_name <- paste0(round(threshold * 100), "%")
  
  if (zone_col %in% names(comparison_data)) {
    valid_data <- comparison_data[is.finite(comparison_data[[zone_col]]), ]
    
    if (nrow(valid_data) > 0) {
      best_idx <- which.min(valid_data[[zone_col]])
      best_window <- valid_data$window_size[best_idx]
      best_width <- valid_data[[zone_col]][best_idx]
      
      cat(sprintf("%s reliability: Window size %d (zone width = %.3f)\n", 
                 rel_name, best_window, best_width))
      
      # Show top 3
      sorted_data <- valid_data[order(valid_data[[zone_col]]), ]
      cat("  Top 3 choices:\n")
      for (j in seq_len(min(3, nrow(sorted_data)))) {
        cat(sprintf("    %d. Window %d (width = %.3f)\n", 
                   j, sorted_data$window_size[j], sorted_data[[zone_col]][j]))
      }
      cat("\n")
    }
  }
}

# ========================================
# SUMMARY REPORT
# ========================================

cat("📊 ANALYSIS SUMMARY\n")
cat(rep("=", 80), "\n", sep = "")
cat("Total window sizes analyzed:", nrow(comparison_data), "\n")
cat("Window size range:", min(comparison_data$window_size), "to", max(comparison_data$window_size), "\n")
cat("Best overall model quality (R²):", max(comparison_data$model_rsq), 
    "at window size", comparison_data$window_size[which.max(comparison_data$model_rsq)], "\n")
cat("Highest baseline volatility:", max(comparison_data$baseline_volatility), 
    "at window size", comparison_data$window_size[which.max(comparison_data$baseline_volatility)], "\n\n")

cat("📁 FILES GENERATED:\n")
cat("==========================================\n")
cat("📊 Comprehensive analysis plots:\n")
comprehensive_plots <- list.files("results/", pattern = "comprehensive_.*\\.png$")
for (plot in comprehensive_plots) {
  cat("  📈", plot, "\n")
}

cat("\n� Data files:\n")
cat("  📊 comparison_summary.csv - Complete analysis data\n")

cat("\n✅ Multi-window comparison complete!\n")
cat("🎯 RECOMMENDATION: Use the optimal window size from the analysis above\n")
cat("📖 TIP: Run '03_view_results.R' to focus on any specific window size\n")
cat("🔍 CHECK: View the comprehensive plots for detailed insights across all window sizes\n\n")
