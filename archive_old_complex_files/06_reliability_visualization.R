# ============================
# Reliability Analysis Visualization and Detailed Reporting
# ============================
# Reads saved reliability results and provides detailed analysis
# Shows where reliability zones stretch for any window size

# Clear environment and load packages
rm(list = ls())
gc()

library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyr)
library(stringr)
devtools::load_all()

cat("=== RELIABILITY ANALYSIS VISUALIZATION ===\n")

# ============================
# Configuration
# ============================

# Input files (adjust if needed)
RDS_FILE <- "multi_window_reliability_detailed.rds"
CSV_FILE <- "multi_window_reliability_summary.csv"

# Window size to analyze in detail (change this as needed)
TARGET_WINDOW_SIZE <- 40

# ============================
# Load Results
# ============================

cat("Loading reliability analysis results...\n")

if (!file.exists(RDS_FILE)) {
  stop("RDS file not found: ", RDS_FILE, "\nPlease run the multi-window analysis first.")
}

detailed_results <- readRDS(RDS_FILE)
cat("✓ Loaded detailed results for", length(detailed_results), "window sizes\n")

if (file.exists(CSV_FILE)) {
  summary_table <- read.csv(CSV_FILE)
  cat("✓ Loaded summary table with", nrow(summary_table), "rows\n")
} else {
  cat("⚠️ Summary CSV not found, will generate from detailed results\n")
  summary_table <- create_reliability_summary_table(detailed_results)
}

# ============================
# Helper Functions
# ============================

#' Extract detailed reliability information for a specific window size
analyze_window_detail <- function(results, window_size) {
  window_key <- paste0("window_", window_size)
  
  if (!window_key %in% names(results)) {
    cat("❌ Window size", window_size, "not found in results\n")
    cat("Available window sizes:", gsub("window_", "", names(results)), "\n")
    return(NULL)
  }
  
  result <- results[[window_key]]
  
  cat("\n🔍 DETAILED ANALYSIS FOR WINDOW SIZE", window_size, ":\n")
  cat("==========================================\n")
  
  # Basic information
  cat("📊 Basic Information:\n")
  cat("   Data points used:", result$n_final, "\n")
  cat("   x_z range:", round(result$x_range[1], 3), "to", round(result$x_range[2], 3), "\n")
  cat("   Baseline volatility:", round(result$baseline, 4), "\n")
  cat("   Model R²:", round(result$model_rsquared, 4), "\n")
  cat("   Reliability range:", round(result$min_reliability, 4), "to", round(result$max_reliability, 4), "\n")
  
  # Model coefficients
  cat("\n📈 Quadratic Model (volatility = a + b*x_z + c*x_z²):\n")
  coefs <- result$model_coefs
  cat("   Intercept (a):", round(coefs["(Intercept)"], 6), "\n")
  cat("   Linear (b):", round(coefs["x_z"], 6), "\n")
  cat("   Quadratic (c):", round(coefs["I(x_z^2)"], 6), "\n")
  
  # Reliability curve analysis
  if (!is.null(result$pred_reliability) && !is.null(result$pred_x_seq)) {
    cat("\n📍 Key Points on Reliability Curve:\n")
    
    rel_seq <- result$pred_reliability
    x_seq <- result$pred_x_seq
    
    # Find extrema
    min_idx <- which.min(rel_seq)
    max_idx <- which.max(rel_seq)
    
    cat("   Minimum reliability:", round(rel_seq[min_idx], 4), "at x_z =", round(x_seq[min_idx], 3), "\n")
    cat("   Maximum reliability:", round(rel_seq[max_idx], 4), "at x_z =", round(x_seq[max_idx], 3), "\n")
    
    # Sample points across the curve
    cat("\n📊 Reliability Curve (20 sample points):\n")
    sample_indices <- seq(1, length(x_seq), length.out = 20)
    for (idx in sample_indices) {
      cat("   x_z =", sprintf("%6.2f", x_seq[idx]), "→ reliability =", sprintf("%.4f", rel_seq[idx]), "\n")
    }
    
    # Reliability at standard x_z values
    standard_points <- c(-3, -2, -1, 0, 1, 2, 3)
    valid_points <- standard_points[standard_points >= result$x_range[1] & standard_points <= result$x_range[2]]
    
    if (length(valid_points) > 0) {
      cat("\n📏 Reliability at Standard x_z Values:\n")
      for (xz_val in valid_points) {
        closest_idx <- which.min(abs(x_seq - xz_val))
        cat("   x_z =", sprintf("%2.0f", xz_val), "→ reliability =", sprintf("%.4f", rel_seq[closest_idx]), "\n")
      }
    }
  }
  
  # Threshold zones
  cat("\n🎯 Reliability Threshold Zones:\n")
  for (thresh_name in names(result$threshold_ranges)) {
    range_vals <- result$threshold_ranges[[thresh_name]]
    thresh_pct <- as.numeric(gsub("Above_|pct", "", thresh_name))
    
    if (!any(is.na(range_vals))) {
      width <- diff(range_vals)
      cat("   ", thresh_pct, "% reliability zone: [", sprintf("%6.3f", range_vals[1]), ",", 
          sprintf("%6.3f", range_vals[2]), "] width =", sprintf("%.3f", width), "\n")
    } else {
      cat("   ", thresh_pct, "% reliability zone: Not achievable (threshold too high)\n")
    }
  }
  
  return(result)
}

#' Create reliability curve plot for a window size
plot_reliability_curve <- function(result, window_size) {
  if (is.null(result$pred_reliability) || is.null(result$pred_x_seq)) {
    cat("⚠️ No prediction data available for plotting\n")
    return(NULL)
  }
  
  # Create dataframe for plotting
  plot_data <- data.frame(
    x_z = result$pred_x_seq,
    reliability = result$pred_reliability
  )
  
  # Create base plot
  p <- ggplot(plot_data, aes(x = .data$x_z, y = .data$reliability)) +
    geom_line(color = "blue", size = 1) +
    labs(
      title = paste("Reliability Curve - Window Size", window_size),
      subtitle = paste("R² =", round(result$model_rsquared, 4), 
                      "| Range:", round(result$min_reliability, 4), "-", round(result$max_reliability, 4)),
      x = "Standardized Predictor (x_z)",
      y = "Reliability (baseline/predicted_volatility)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
  
  # Add threshold lines and zones
  threshold_colors <- c("95" = "red", "90" = "orange", "85" = "yellow", "80" = "green", "75" = "cyan", "70" = "purple")
  
  for (thresh_name in names(result$threshold_ranges)) {
    thresh_pct <- gsub("Above_|pct", "", thresh_name)
    thresh_val <- as.numeric(thresh_pct) / 100
    range_vals <- result$threshold_ranges[[thresh_name]]
    
    if (!any(is.na(range_vals)) && thresh_pct %in% names(threshold_colors)) {
      # Add horizontal threshold line
      p <- p + geom_hline(yintercept = thresh_val, 
                         color = threshold_colors[thresh_pct], 
                         linetype = "dashed", alpha = 0.7)
      
      # Add zone shading
      if (is.finite(range_vals[1]) && is.finite(range_vals[2])) {
        zone_data <- plot_data[plot_data$x_z >= range_vals[1] & plot_data$x_z <= range_vals[2], ]
        if (nrow(zone_data) > 0) {
          p <- p + geom_ribbon(data = zone_data,
                              aes(ymin = thresh_val, ymax = .data$reliability),
                              fill = threshold_colors[thresh_pct], alpha = 0.2)
        }
      }
    }
  }
  
  return(p)
}

#' Compare zone widths across window sizes
plot_zone_comparison <- function(summary_table) {
  if (!"zone_90_width" %in% names(summary_table)) {
    cat("⚠️ Zone width data not available in summary table\n")
    return(NULL)
  }
  
  # Prepare data for plotting
  plot_data <- summary_table %>%
    select(.data$window_size, contains("zone_")) %>%
    tidyr::pivot_longer(cols = -.data$window_size, names_to = "threshold", values_to = "width") %>%
    mutate(
      threshold_pct = as.numeric(stringr::str_extract(.data$threshold, "\\d+")),
      threshold_label = paste0(.data$threshold_pct, "%")
    ) %>%
    filter(!is.na(.data$width) & is.finite(.data$width))
  
  p <- ggplot(plot_data, aes(x = .data$window_size, y = .data$width, color = .data$threshold_label)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
      title = "Reliability Zone Widths by Window Size",
      x = "Window Size",
      y = "Zone Width",
      color = "Reliability\nThreshold"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "right"
    ) +
    scale_x_continuous(breaks = unique(plot_data$window_size))
  
  return(p)
}

# ============================
# Main Analysis
# ============================

# Detailed analysis for target window size
cat("\n", rep("=", 50), "\n", sep = "")
target_result <- analyze_window_detail(detailed_results, TARGET_WINDOW_SIZE)

# Overall comparison
cat("\n", rep("=", 50), "\n", sep = "")
cat("📊 ZONE WIDTH COMPARISON ACROSS WINDOW SIZES:\n")
cat("==============================================\n")

if (nrow(summary_table) > 0) {
  # Show 90% zone widths
  if ("zone_90_width" %in% names(summary_table)) {
    zone_90_data <- summary_table %>%
      select(window_size, zone_90_width) %>%
      arrange(window_size) %>%
      filter(!is.na(zone_90_width) & is.finite(zone_90_width))
    
    cat("90% Reliability Zone Widths:\n")
    for (i in seq_len(nrow(zone_90_data))) {
      cat("   Window", sprintf("%3d", zone_90_data$window_size[i]), 
          ": width =", sprintf("%.3f", zone_90_data$zone_90_width[i]), "\n")
    }
    
    # Find optimal
    if (nrow(zone_90_data) > 0) {
      optimal_idx <- which.min(zone_90_data$zone_90_width)
      cat("\n🏆 Optimal window size (smallest 90% zone):", zone_90_data$window_size[optimal_idx], 
          "with width", sprintf("%.3f", zone_90_data$zone_90_width[optimal_idx]), "\n")
    }
  }
  
  # Model quality comparison
  cat("\n📈 Model Quality (R²) by Window Size:\n")
  r2_data <- summary_table %>%
    select(window_size, r_squared) %>%
    arrange(window_size)
  
  for (i in seq_len(nrow(r2_data))) {
    cat("   Window", sprintf("%3d", r2_data$window_size[i]), 
        ": R² =", sprintf("%.4f", r2_data$r_squared[i]), "\n")
  }
}

# ============================
# Generate Plots
# ============================

cat("\n📊 Generating plots...\n")

# Plot reliability curve for target window size
if (!is.null(target_result)) {
  p1 <- plot_reliability_curve(target_result, TARGET_WINDOW_SIZE)
  if (!is.null(p1)) {
    ggsave(paste0("reliability_curve_window_", TARGET_WINDOW_SIZE, ".png"), 
           p1, width = 10, height = 6, dpi = 300)
    cat("✓ Saved reliability curve plot\n")
  }
}

# Plot zone width comparison
if (nrow(summary_table) > 0) {
  p2 <- plot_zone_comparison(summary_table)
  if (!is.null(p2)) {
    ggsave("zone_width_comparison.png", p2, width = 12, height = 6, dpi = 300)
    cat("✓ Saved zone width comparison plot\n")
  }
}

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Change TARGET_WINDOW_SIZE at the top to analyze different window sizes\n")
