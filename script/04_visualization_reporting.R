# ============================
# Visualization and Reporting
# ============================
# Creates plots and reports from reliability analysis results

# Clear environment and load packages
rm(list = ls())
gc()

library(ggplot2)
library(dplyr)
library(data.table)
devtools::load_all()

cat("=== RELIABILITY VISUALIZATION & REPORTING ===\n")

# ============================
# Load Results
# ============================

# Try to load the most recent reliability analysis
result_files <- list.files(pattern = "reliability_analysis_.*\\.rds$")
if (length(result_files) == 0) {
  stop("No reliability analysis results found. Run 03_reliability_analysis.R first.")
}

# Use the most recent file
latest_file <- sort(result_files, decreasing = TRUE)[1]
cat("Loading results from:", latest_file, "\n")

reliability_results <- readRDS(latest_file)
cat("✓ Loaded results for", length(reliability_results), "window sizes\n")

# Load summary table
summary_files <- list.files(pattern = "reliability_analysis_.*summary\\.csv$")
if (length(summary_files) > 0) {
  latest_summary <- sort(summary_files, decreasing = TRUE)[1]
  summary_df <- read.csv(latest_summary)
  cat("✓ Loaded summary table:", nrow(summary_df), "rows\n")
} else {
  # Create summary if not available
  cat("Creating summary table...\n")
  source("R/10_reliability_analysis.R")
  summary_df <- create_reliability_summary_table(reliability_results)
}

# ============================
# Visualization Functions
# ============================

#' Plot reliability zone widths by window size
plot_zone_widths <- function(summary_df, threshold = "90") {
  col_name <- paste0("zone_", threshold, "_width")
  
  if (!col_name %in% names(summary_df)) {
    stop("Column ", col_name, " not found in summary data")
  }
  
  plot_data <- summary_df[!is.na(summary_df[[col_name]]), ]
  
  ggplot(plot_data, aes_string(x = "window_size", y = col_name)) +
    geom_line(color = "steelblue", size = 1) +
    geom_point(color = "steelblue", size = 2) +
    theme_minimal() +
    labs(
      title = paste("Width of", threshold, "% Reliability Zone by Window Size"),
      x = "Window Size",
      y = paste("Zone Width (", threshold, "% reliability)", sep = ""),
      caption = "Larger widths indicate more stable regions for analysis"
    ) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
}

#' Plot baseline reliability by window size
plot_baseline_reliability <- function(summary_df) {
  ggplot(summary_df, aes(x = window_size, y = baseline)) +
    geom_line(color = "darkgreen", size = 1) +
    geom_point(color = "darkgreen", size = 2) +
    theme_minimal() +
    labs(
      title = "Baseline Reliability by Window Size",
      x = "Window Size",
      y = "Baseline Reliability Score",
      caption = "Higher values indicate better baseline performance"
    ) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
}

#' Plot model R-squared by window size
plot_model_quality <- function(summary_df) {
  ggplot(summary_df, aes(x = window_size, y = r_squared)) +
    geom_line(color = "red", size = 1) +
    geom_point(color = "red", size = 2) +
    theme_minimal() +
    labs(
      title = "Model Quality (R²) by Window Size",
      x = "Window Size", 
      y = "R-squared",
      caption = "Higher R² indicates better model fit to reliability patterns"
    ) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
}

#' Create comprehensive reliability heatmap
plot_reliability_heatmap <- function(summary_df) {
  # Reshape data for heatmap
  threshold_cols <- grep("zone_.*_width", names(summary_df), value = TRUE)
  
  if (length(threshold_cols) == 0) {
    stop("No zone width columns found for heatmap")
  }
  
  # Prepare data
  heatmap_data <- summary_df[, c("window_size", threshold_cols)]
  
  # Reshape to long format
  heatmap_long <- reshape2::melt(heatmap_data, id.vars = "window_size",
                                variable.name = "threshold", value.name = "zone_width")
  
  # Clean threshold names
  heatmap_long$threshold <- gsub("zone_|_width", "", heatmap_long$threshold)
  heatmap_long$threshold <- paste0(heatmap_long$threshold, "%")
  
  ggplot(heatmap_long, aes(x = factor(window_size), y = threshold, fill = zone_width)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_gradient2(
      low = "red", mid = "yellow", high = "green",
      midpoint = median(heatmap_long$zone_width, na.rm = TRUE),
      na.value = "grey50",
      name = "Zone\nWidth"
    ) +
    theme_minimal() +
    labs(
      title = "Reliability Zone Widths Heatmap",
      x = "Window Size",
      y = "Reliability Threshold",
      caption = "Green = wider reliable zones, Red = narrower zones"
    ) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# ============================
# Generate Visualizations
# ============================

cat("\n=== GENERATING VISUALIZATIONS ===\n")

# Create plots directory
if (!dir.exists("plots")) {
  dir.create("plots")
  cat("✓ Created plots directory\n")
}

# Generate individual plots
plots <- list()

# Zone width plots for different thresholds
for (threshold in c("95", "90", "85", "80")) {
  col_name <- paste0("zone_", threshold, "_width")
  if (col_name %in% names(summary_df)) {
    cat("Creating", threshold, "% zone width plot...\n")
    p <- plot_zone_widths(summary_df, threshold)
    plots[[paste0("zone_width_", threshold)]] <- p
    
    ggsave(
      filename = file.path("plots", paste0("zone_width_", threshold, "pct.png")),
      plot = p, width = 10, height = 6, dpi = 300
    )
  }
}

# Baseline reliability plot
cat("Creating baseline reliability plot...\n")
p_baseline <- plot_baseline_reliability(summary_df)
plots$baseline <- p_baseline
ggsave("plots/baseline_reliability.png", p_baseline, width = 10, height = 6, dpi = 300)

# Model quality plot
cat("Creating model quality plot...\n")
p_quality <- plot_model_quality(summary_df)
plots$model_quality <- p_quality
ggsave("plots/model_quality.png", p_quality, width = 10, height = 6, dpi = 300)

# Reliability heatmap
if (requireNamespace("reshape2", quietly = TRUE)) {
  cat("Creating reliability heatmap...\n")
  p_heatmap <- plot_reliability_heatmap(summary_df)
  plots$heatmap <- p_heatmap
  ggsave("plots/reliability_heatmap.png", p_heatmap, width = 12, height = 8, dpi = 300)
} else {
  cat("⚠️ reshape2 package not available - skipping heatmap\n")
}

# ============================
# Generate Report
# ============================

cat("\n=== GENERATING SUMMARY REPORT ===\n")

# Create a summary report
report_text <- paste0(
  "# LOCALVAR Reliability Analysis Report\n",
  "Generated: ", Sys.time(), "\n\n",
  "## Summary\n",
  "- Window sizes analyzed: ", paste(summary_df$window_size, collapse = ", "), "\n",
  "- Total data points: ", sum(summary_df$n_datapoints, na.rm = TRUE), "\n",
  "- Average model R²: ", round(mean(summary_df$r_squared, na.rm = TRUE), 4), "\n\n",
  "## Key Findings\n",
  "### Best performing window size (90% zone width):\n"
)

# Find best window size for 90% reliability
if ("zone_90_width" %in% names(summary_df)) {
  best_90 <- summary_df[which.max(summary_df$zone_90_width), ]
  report_text <- paste0(report_text,
    "- Window size: ", best_90$window_size, "\n",
    "- Zone width: ", round(best_90$zone_90_width, 3), "\n",
    "- Baseline: ", round(best_90$baseline, 4), "\n",
    "- R²: ", round(best_90$r_squared, 4), "\n\n"
  )
}

# Add zone width statistics
report_text <- paste0(report_text, "### Zone Width Statistics (90% reliability):\n")
if ("zone_90_width" %in% names(summary_df)) {
  zone_stats <- summary(summary_df$zone_90_width)
  for (i in 1:length(zone_stats)) {
    report_text <- paste0(report_text, "- ", names(zone_stats)[i], ": ", 
                         round(zone_stats[i], 3), "\n")
  }
}

# Save report
writeLines(report_text, "reliability_analysis_report.txt")

cat("✓ Report saved as: reliability_analysis_report.txt\n")
cat("✓ Plots saved in: plots/ directory\n")

# ============================
# Summary
# ============================

cat("\n=== VISUALIZATION COMPLETE ===\n")
cat("Generated", length(plots), "plots\n")
cat("Files created:\n")
for (file in list.files("plots", full.names = FALSE)) {
  cat("- plots/", file, "\n")
}
cat("- reliability_analysis_report.txt\n")

cat("\n=== SESSION COMPLETE ===\n")
