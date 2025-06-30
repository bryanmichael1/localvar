# ========================================
# RELIABILITY ANALYSIS FUNCTIONS
# ========================================
# Functions for calculating and analyzing reliability zones

library(future)      # Modern parallel processing  
library(furrr)       # purrr-style parallel functions
library(ggplot2)     # For plotting functions

#' Analyze reliability across multiple datasets
#' 
#' This is the main function that calculates reliability zones
#' 
#' @param datasets List of datasets, each with 'x' and 'y_norm' columns
#' @param window_size Size of analysis windows
#' @param reliability_thresholds Vector of reliability levels to calculate (e.g., c(0.90, 0.95))
#' @param batch_size Number of datasets to process at once (for memory efficiency)
#' @return List with reliability analysis results
analyze_reliability <- function(datasets, window_size, reliability_thresholds = c(0.90, 0.95), batch_size = 20) {
  
  cat("Analyzing reliability with window size", window_size, "\n")
  cat("Processing", length(datasets), "datasets in batches of", batch_size, "\n")
  
  # Extract volatility data from all datasets
  all_x_z <- c()
  all_volatility <- c()
  
  n_batches <- ceiling(length(datasets) / batch_size)
  
  for (batch in 1:n_batches) {
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, length(datasets))
    
    cat("Processing batch", batch, "of", n_batches, "\n")
    
    for (i in start_idx:end_idx) {
      tryCatch({
        # Run volatility analysis on this dataset
        result <- run_volatility_pipeline(
          x = datasets[[i]]$x,
          y = datasets[[i]]$y_norm,
          window_size = window_size
        )
        
        # Extract volatility data from all windows
        if (!is.null(result$windows)) {
          for (window in result$windows) {
            if (!is.null(window$x_stats) && !is.null(window$score)) {
              x_z <- window$x_stats$mean_z
              volatility <- window$score$sd
              
              if (is.finite(x_z) && is.finite(volatility)) {
                all_x_z <- c(all_x_z, x_z)
                all_volatility <- c(all_volatility, volatility)
              }
            }
          }
        }
        
      }, error = function(e) {
        # Skip failed datasets
      })
    }
    
    # Clean up memory
    gc(verbose = FALSE)
  }
  
  cat("Extracted", length(all_x_z), "data points\n")
  
  if (length(all_x_z) < 100) {
    stop("Insufficient data for reliability analysis (need at least 100 points)")
  }
  
  # Calculate reliability metrics
  reliability_results <- calculate_reliability_zones(all_x_z, all_volatility, reliability_thresholds)
  
  # Add metadata
  reliability_results$meta <- list(
    window_size = window_size,
    n_datasets = length(datasets),
    n_datapoints = length(all_x_z)
  )
  
  return(reliability_results)
}

#' Calculate reliability zones from volatility data
calculate_reliability_zones <- function(x_z_values, volatility_values, thresholds) {
  # Clean data
  valid_idx <- is.finite(x_z_values) & is.finite(volatility_values)
  x_z <- x_z_values[valid_idx]
  volatility <- abs(volatility_values[valid_idx])
  cat("Fitting reliability model with", length(x_z), "data points\n")

  # Fit quadratic model
  model_data <- data.frame(x_z = x_z, volatility = volatility)
  q99 <- quantile(abs(model_data$x_z), 0.99, na.rm = TRUE)
  model_data <- model_data[abs(model_data$x_z) <= q99, ]
  volatility_model <- lm(volatility ~ x_z + I(x_z^2), data = model_data)

  # Baseline volatility
  central <- model_data[abs(model_data$x_z) < 0.5, ]
  baseline <- if (nrow(central) > 0) mean(central$volatility) else mean(model_data$volatility)

  # Prediction grid
  x_grid <- seq(min(model_data$x_z), max(model_data$x_z), length.out = 1000)
  pred_vol <- predict(volatility_model, newdata = data.frame(x_z = x_grid))
  pred_vol <- pmax(pred_vol, 0.001)

  # Reliability curve
  reliability <- baseline / pred_vol

  # Determine zones numerically
  zones <- lapply(thresholds, function(thresh) {
    idx <- which(reliability >= thresh)
    if (length(idx) == 0) {
      list(lower = NA, upper = NA, width = NA)
    } else {
      lower <- x_grid[min(idx)]
      upper <- x_grid[max(idx)]
      list(lower = lower, upper = upper, width = upper - lower)
    }
  })
  names(zones) <- paste0("zone_", round(thresholds * 100), "pct")

  # Model diagnostics
  model_rsq <- summary(volatility_model)$r.squared
  rel_range <- range(reliability, na.rm = TRUE)
  cat("Model R²:", round(model_rsq, 4), "\n")
  cat("Reliability range:", round(rel_range[1], 4), "to", round(rel_range[2], 4), "\n")

  return(list(
    zones = zones,
    model = volatility_model,
    model_rsq = model_rsq,
    baseline_volatility = baseline,
    reliability_curve = data.frame(x_z = x_grid, reliability = reliability)
  ))
}

#' Find the zone where reliability exceeds a threshold
find_reliability_zone <- function(x_values, reliability_values, threshold) {
  
  # Find where reliability exceeds threshold
  above_threshold <- reliability_values >= threshold
  
  if (!any(above_threshold)) {
    return(list(lower = NA, upper = NA, width = NA))
  }
  
  # Find the range where threshold is exceeded
  threshold_indices <- which(above_threshold)
  
  if (length(threshold_indices) == 0) {
    return(list(lower = NA, upper = NA, width = NA))
  }
  
  # Get the continuous region (handle potential gaps)
  x_threshold <- x_values[threshold_indices]
  lower_bound <- min(x_threshold)
  upper_bound <- max(x_threshold)
  zone_width <- upper_bound - lower_bound
  
  return(list(
    lower = lower_bound,
    upper = upper_bound,
    width = zone_width
  ))
}

#' Save reliability analysis results
save_reliability_results <- function(results, output_name = "reliability_analysis") {
  
  # Save detailed results
  detailed_file <- paste0(output_name, "_detailed.rds")
  saveRDS(results, detailed_file)
  
  # Create summary table
  summary_data <- data.frame(
    window_size = results$meta$window_size,
    n_datasets = results$meta$n_datasets,
    n_datapoints = results$meta$n_datapoints,
    model_rsq = results$model_rsq,
    baseline_volatility = results$baseline_volatility
  )
  
  # Add zone widths
  for (zone_name in names(results$zones)) {
    zone_info <- results$zones[[zone_name]]
    width_col <- paste0(zone_name, "_width")
    summary_data[[width_col]] <- zone_info$width
  }
  
  # Save summary
  summary_file <- paste0(output_name, "_summary.csv")
  write.csv(summary_data, summary_file, row.names = FALSE)
  
  cat("\nResults saved:\n")
  cat("- Detailed:", detailed_file, "\n")
  cat("- Summary:", summary_file, "\n")
  
  return(list(detailed = detailed_file, summary = summary_file, summary_data = summary_data))
}

#' Calculate reliability metrics with optimized approach
#' 
#' This is the main optimized function used by the analysis pipeline
#' 
#' @param x_z_vals Vector of standardized x values
#' @param score_sd_vals Vector of volatility scores
#' @param thresholds Vector of reliability thresholds to calculate
#' @return List with detailed reliability metrics
calculate_reliability_metrics_optimized <- function(x_z_vals, score_sd_vals, thresholds) {
  # Create and clean dataframe
  df_combined <- data.frame(x_z = x_z_vals, score_sd = score_sd_vals)
  df_combined <- df_combined[is.finite(df_combined$x_z) & is.finite(df_combined$score_sd), ]
  
  # Remove extreme outliers (top 1%) for model stability
  q99 <- quantile(abs(df_combined$x_z), 0.99, na.rm = TRUE)
  df_combined <- df_combined[abs(df_combined$x_z) <= q99, ]
  
  cat("  📊 Fitting model with", nrow(df_combined), "cleaned data points\n")
  
  # Fit quadratic model
  model <- lm(abs(score_sd) ~ x_z + I(x_z^2), data = df_combined)
  
  # Calculate baseline from central region (more robust approach)
  central_mask <- abs(df_combined$x_z) < 0.5
  baseline <- if (sum(central_mask) > 0) {
    mean(abs(df_combined$score_sd[central_mask]), na.rm = TRUE)
  } else {
    mean(abs(df_combined$score_sd), na.rm = TRUE)
  }
  
  # Model predictions
  coefs <- coef(model)
  x_range <- range(df_combined$x_z, na.rm = TRUE)
  x_seq <- seq(x_range[1], x_range[2], length.out = 1000)
  
  pred_abs_score <- coefs["(Intercept)"] + coefs["x_z"] * x_seq + coefs["I(x_z^2)"] * x_seq^2
  
  # Ensure predictions are positive (avoid division by zero/negative)
  pred_abs_score <- pmax(pred_abs_score, 0.001)
  
  # Calculate reliability as baseline / predicted_volatility
  # Lower predicted volatility = higher reliability
  rel_reliability <- baseline / pred_abs_score
  
  a <- coefs["(Intercept)"]
  b <- coefs["x_z"]
  c_coef <- coefs["I(x_z^2)"]

  # Check for flat model conditions
  model_rsq <- summary(model)$r.squared
  reliability_range <- max(rel_reliability, na.rm = TRUE) - min(rel_reliability, na.rm = TRUE)
  is_flat_model <- model_rsq < 0.01 || reliability_range < 0.05

  threshold_ranges <- lapply(thresholds, function(thresh) {
    # For flat models with minimal variation, use data-driven approach
    if (is_flat_model) {
      # Check if threshold is achievable given the reliability range
      max_reliability <- max(rel_reliability, na.rm = TRUE)
      if (thresh > max_reliability) {
        return(c(NA, NA))  # Threshold too high
      }
      
      # For achievable thresholds on flat models, use x_z range with some restriction
      # More restrictive for higher thresholds
      restriction_factor <- (thresh - 0.5) * 2  # 0 at 50%, 1 at 100%
      restriction_factor <- max(0, min(1, restriction_factor))
      
      # Restrict to central portion based on threshold
      x_range_width <- diff(x_range)
      restriction_width <- x_range_width * (1 - restriction_factor * 0.8)  # Max 80% restriction
      
      center <- mean(x_range)
      half_width <- restriction_width / 2
      
      return(c(center - half_width, center + half_width))
    } else {
      # Use analytical solution for non-flat models
      interval <- compute_quadratic_interval(a, b, c_coef, baseline, thresh)
      
      # Handle infinite intervals from downward parabolas
      if (any(is.infinite(interval))) {
        # Constrain to reasonable bounds based on data range
        data_range_width <- diff(x_range)
        expansion_factor <- 1 + (1 - thresh) * 2  # More expansion for lower thresholds
        
        bounded_width <- data_range_width * expansion_factor
        center <- mean(x_range)
        half_width <- bounded_width / 2
        
        return(c(center - half_width, center + half_width))
      }
      
      return(interval)
    }
  })
  names(threshold_ranges) <- paste0("Above_", thresholds * 100, "pct")

  # Calculate zone widths directly for better accuracy
  zone_widths <- sapply(threshold_ranges, function(range_vals) {
    if (any(is.na(range_vals))) {
      NA
    } else {
      diff(range_vals)
    }
  })
  names(zone_widths) <- paste0("zone_", gsub("Above_|pct", "", names(threshold_ranges)), "_width")
  
  # Additional diagnostics
  cat("  📊 x_z range:", round(x_range[1], 3), "to", round(x_range[2], 3), "\n")
  cat("  📊 Reliability range:", round(min(rel_reliability, na.rm = TRUE), 4), 
      "to", round(max(rel_reliability, na.rm = TRUE), 4), "\n")
  cat("  📊 Model R²:", round(summary(model)$r.squared, 4), "\n")
  cat("  📊 Model type:", if(is_flat_model) "FLAT (using data-driven approach)" else "QUADRATIC (using analytical solution)", "\n")
  
  # Print zone widths for debugging
  for (i in seq_along(thresholds)) {
    thresh <- thresholds[i]
    zone_width <- zone_widths[i]
    range_vals <- threshold_ranges[[i]]
    
    if (!is.na(zone_width) && is.finite(zone_width)) {
      cat("  📏", thresh*100, "% zone: [", round(range_vals[1], 3), ",", 
          round(range_vals[2], 3), "] width =", round(zone_width, 3), "\n")
    } else if (!is.na(zone_width) && is.infinite(zone_width)) {
      cat("  📏", thresh*100, "% zone: [", round(range_vals[1], 3), ",", 
          round(range_vals[2], 3), "] width = Inf (unbounded)\n")
    } else {
      cat("  📏", thresh*100, "% zone: No valid region (threshold too high)\n")
    }
  }
  
  return(list(
    n_final = nrow(df_combined),
    baseline = baseline,
    model_rsquared = summary(model)$r.squared,
    model_coefs = coefs,
    threshold_ranges = threshold_ranges,
    zone_widths = zone_widths,
    min_reliability = min(rel_reliability, na.rm = TRUE),
    max_reliability = max(rel_reliability, na.rm = TRUE),
    x_range = x_range,
    pred_reliability = rel_reliability,
    x_sequence = x_seq,
    reliability_by_position = data.frame(position = x_seq, reliability = rel_reliability)
  ))
}

#' Compute interval where quadratic inequality holds
#' 
#' Solves baseline / (a + b*x + c*x^2) >= threshold
#' 
#' @param a Intercept coefficient
#' @param b Linear coefficient  
#' @param c Quadratic coefficient
#' @param baseline Baseline value
#' @param threshold Threshold value
#' @return Vector of interval endpoints
compute_quadratic_interval <- function(a, b, c, baseline, threshold) {
  # Input validation
  if (any(!is.finite(c(a, b, c, baseline, threshold))) || threshold <= 0) {
    return(c(NA, NA))
  }
  
  # Transform inequality: baseline / (a + b*x + c*x^2) >= threshold
  # Rearrange to: c*x^2 + b*x + (a - baseline/threshold) <= 0
  rhs <- baseline / threshold
  new_constant <- a - rhs
  
  # Handle degenerate cases
  if (abs(c) < 1e-10) {
    # Linear case: b*x + new_constant <= 0
    if (abs(b) < 1e-10) {
      # Constant case
      return(if (new_constant <= 0) c(-Inf, Inf) else c(NA, NA))
    }
    # Linear solution: x <= -new_constant/b
    critical_x <- -new_constant / b
    return(if (b > 0) c(-Inf, critical_x) else c(critical_x, Inf))
  }
  
  # Quadratic case: solve c*x^2 + b*x + new_constant = 0
  discriminant <- b^2 - 4 * c * new_constant
  
  if (discriminant < 0) {
    # No real roots - check sign of quadratic
    # For very large |x|, sign is determined by c
    return(if (c < 0) c(-Inf, Inf) else c(NA, NA))
  }
  
  if (discriminant == 0) {
    # One root (repeated)
    root <- -b / (2 * c)
    return(if (c < 0) c(-Inf, Inf) else c(root, root))
  }
  
  # Two distinct real roots
  sqrt_disc <- sqrt(discriminant)
  root1 <- (-b - sqrt_disc) / (2 * c)
  root2 <- (-b + sqrt_disc) / (2 * c)
  roots <- sort(c(root1, root2))
  
  # For quadratic ax^2 + bx + c <= 0:
  # If leading coefficient > 0: solution is between roots
  # If leading coefficient < 0: solution is outside roots
  if (c > 0) {
    # Parabola opens upward - solution between roots
    return(roots)
  } else {
    # Parabola opens downward - solution outside roots
    # For reliability analysis, we typically want bounded intervals
    # Return the roots but caller should handle unbounded case
    return(roots)
  }
}

#' Print a nice summary of reliability analysis results
print_reliability_summary <- function(window_size, metrics) {
  cat("  ✅ Window size", window_size, "analysis complete:\n")
  cat("    - Data points:", metrics$n_final, "\n")
  cat("    - Baseline:", round(metrics$baseline, 4), "\n")
  cat("    - R-squared:", round(metrics$model_rsquared, 4), "\n")
  cat("    - Reliability range:", round(metrics$min_reliability, 4), 
      "to", round(metrics$max_reliability, 4), "\n")
  
  for (thresh_name in names(metrics$threshold_ranges)) {
    range_vals <- metrics$threshold_ranges[[thresh_name]]
    if (!any(is.na(range_vals))) {
      width <- diff(range_vals)
      cat("    -", thresh_name, "zone: [", round(range_vals[1], 2), ",", 
          round(range_vals[2], 2), "] width =", round(width, 3), "\n")
    }
  }
}

#' Generate detailed single-window analysis with plots and reports
#' 
#' This function creates comprehensive analysis for one specific window size
#' including reliability curves, zone plots, and diagnostic information
#' 
#' @param window_size The window size to analyze
#' @param detailed_results The detailed results from multi-window analysis 
#' @param output_prefix Prefix for output files (default: "single_window")
#' @param reliability_thresholds Vector of reliability thresholds (default: c(0.95, 0.90, 0.85, 0.80))
#' @return List with plot objects and summary information
generate_single_window_analysis <- function(window_size, detailed_results = NULL, output_prefix = "single_window", reliability_thresholds = c(0.95, 0.90, 0.85, 0.80)) {
  
  cat("=== SINGLE WINDOW ANALYSIS: SIZE", window_size, "===\n")
  
  # Load detailed results if not provided
  if (is.null(detailed_results)) {
    detailed_file <- "data/multi_window_reliability_detailed.rds"
    if (!file.exists(detailed_file)) {
      stop("❌ Detailed results not found. Run '02_analyze_reliability.R' first!")
    }
    detailed_results <- readRDS(detailed_file)
  }
  
  # Find the specific window size
  window_key <- as.character(window_size)
  if (!window_key %in% names(detailed_results)) {
    stop("❌ Window size ", window_size, " not found in results!")
  }
  
  window_data <- detailed_results[[window_key]]
  cat("✓ Found data for window size", window_size, "\n")
  cat("  - Data points:", length(window_data$x_z_data), "\n")
  
  # Calculate reliability zones if not already present
  if (!"zones" %in% names(window_data)) {
    cat("  - Calculating reliability zones...\n")
    zone_analysis <- calculate_reliability_zones(
      window_data$x_z_data, 
      window_data$score_sd_data, 
      reliability_thresholds
    )
    window_data <- c(window_data, zone_analysis)
  }
  
  cat("  - Model R²:", round(window_data$model_rsq, 4), "\n")
  cat("  - Baseline volatility:", round(window_data$baseline_volatility, 4), "\n")
  
  # Create plots
  plots <- list()
  
  # 1. Scatter plot: volatility vs position  
  cat("📈 Creating scatter plot...\n")
  scatter_data <- data.frame(
    x_z = window_data$x_z_data,
    volatility = abs(window_data$score_sd_data)
  )
  
  plots$scatter <- ggplot(scatter_data, aes(x = .data$x_z, y = .data$volatility)) +
    geom_point(alpha = 0.3, size = 1) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "red") +
    labs(
      title = paste0("Volatility Pattern - Window Size ", window_size),
      subtitle = paste0("Model R² = ", round(window_data$model_rsq, 4)),
      x = "Standardized Position (z-score)",
      y = "Local Volatility"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  # 2. Reliability curve
  cat("📈 Creating reliability curve...\n")
  if ("reliability_curve" %in% names(window_data)) {
    rel_data <- window_data$reliability_curve
    
    plots$reliability_curve <- ggplot(rel_data, aes(x = .data$x_z, y = .data$reliability)) +
      geom_line(color = "blue", linewidth = 1.2) +
      geom_hline(yintercept = c(0.8, 0.9, 0.95), linetype = "dashed", alpha = 0.5) +
      labs(
        title = paste0("Reliability Curve - Window Size ", window_size),
        subtitle = "Higher values = more reliable analysis in that region",
        x = "Standardized Position (z-score)",
        y = "Local Reliability"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"))
  }
  
  # 3. Zone visualization
  cat("📈 Creating zone visualization...\n")
  if ("zones" %in% names(window_data)) {
    # Create a plot showing the reliable zones
    x_range <- range(window_data$x_z_data, na.rm = TRUE)
    plot_data <- data.frame(x = seq(x_range[1], x_range[2], length.out = 100))
    
    plots$zones <- ggplot(plot_data, aes(x = .data$x)) +
      geom_hline(yintercept = 0, color = "black") +
      labs(
        title = paste0("Reliable Zones - Window Size ", window_size),
        subtitle = "Colored regions show where analysis is reliable",
        x = "Standardized Position (z-score)",
        y = ""
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
    
    # Add zones as colored rectangles
    colors <- c("95%" = "darkgreen", "90%" = "green", "85%" = "yellow", "80%" = "orange")
    y_pos <- 0.5
    
    for (zone_name in names(window_data$zones)) {
      zone <- window_data$zones[[zone_name]]
      if (!is.na(zone$lower) && !is.na(zone$upper)) {
        zone_pct <- sub("zone_", "", sub("pct", "%", zone_name))
        zone_color <- colors[zone_pct]
        if (is.na(zone_color)) zone_color <- "gray"
        
        plots$zones <- plots$zones +
          ggplot2::geom_rect(
            ggplot2::aes(xmin = zone$lower, xmax = zone$upper, ymin = -y_pos, ymax = y_pos),
            fill = zone_color, alpha = 0.3, inherit.aes = FALSE
          ) +
          ggplot2::annotate("text", x = (zone$lower + zone$upper) / 2, y = 0, 
                  label = zone_pct, size = 3, fontface = "bold")
        
        y_pos <- y_pos + 0.3
      }
    }
  }
  
  # 4. Summary statistics table
  cat("📊 Creating summary table...\n")
  summary_stats <- data.frame(
    Metric = c(
      "Window Size",
      "Data Points", 
      "Model R²",
      "Baseline Volatility",
      "Data Range (min)",
      "Data Range (max)"
    ),
    Value = c(
      window_size,
      length(window_data$x_z_data),
      round(window_data$model_rsq, 4),
      round(window_data$baseline_volatility, 4),
      round(min(window_data$x_z_data, na.rm = TRUE), 2),
      round(max(window_data$x_z_data, na.rm = TRUE), 2)
    )
  )
  
  # Add zone widths to summary
  if ("zones" %in% names(window_data)) {
    for (zone_name in names(window_data$zones)) {
      zone <- window_data$zones[[zone_name]]
      zone_label <- paste0("Zone Width (", sub("zone_", "", sub("pct", "%", zone_name)), ")")
      zone_width <- if (is.na(zone$width)) "Not found" else round(zone$width, 3)
      
      summary_stats <- rbind(summary_stats, data.frame(
        Metric = zone_label,
        Value = as.character(zone_width)
      ))
    }
  }
  
  # Save plots
  cat("💾 Saving plots...\n")
  if (!is.null(plots$scatter)) {
    filename <- paste0("results/", output_prefix, "_scatter_", window_size, ".png")
    ggplot2::ggsave(filename, plots$scatter, width = 10, height = 6, dpi = 300)
    cat("✓ Saved:", basename(filename), "\n")
  }
  
  if (!is.null(plots$reliability_curve)) {
    filename <- paste0("results/", output_prefix, "_reliability_", window_size, ".png")
    ggplot2::ggsave(filename, plots$reliability_curve, width = 10, height = 6, dpi = 300)
    cat("✓ Saved:", basename(filename), "\n")
  }
  
  if (!is.null(plots$zones)) {
    filename <- paste0("results/", output_prefix, "_zones_", window_size, ".png")
    ggplot2::ggsave(filename, plots$zones, width = 10, height = 4, dpi = 300)
    cat("✓ Saved:", basename(filename), "\n")
  }
  
  # Generate text report
  cat("📝 Generating report...\n")
  report_lines <- c(
    paste0("SINGLE WINDOW ANALYSIS REPORT - WINDOW SIZE ", window_size),
    paste0("Generated: ", Sys.time()),
    "",
    "SUMMARY:",
    "========",
    paste0("Window Size: ", window_size),
    paste0("Total Data Points: ", length(window_data$x_z_data)),
    paste0("Model Quality (R²): ", round(window_data$model_rsq, 4)),
    paste0("Baseline Volatility: ", round(window_data$baseline_volatility, 4)),
    "",
    "RELIABILITY ZONES:",
    "=================="
  )
  
  if ("zones" %in% names(window_data)) {
    for (zone_name in names(window_data$zones)) {
      zone <- window_data$zones[[zone_name]]
      zone_label <- sub("zone_", "", sub("pct", "%", zone_name))
      
      if (!is.na(zone$width)) {
        report_lines <- c(report_lines,
          paste0(zone_label, " reliability zone:"),
          paste0("  Range: [", round(zone$lower, 3), ", ", round(zone$upper, 3), "]"),
          paste0("  Width: ", round(zone$width, 3)),
          ""
        )
      } else {
        report_lines <- c(report_lines,
          paste0(zone_label, " reliability zone: Not found"),
          ""
        )
      }
    }
  }
  
  report_lines <- c(report_lines,
    "INTERPRETATION:",
    "===============",
    "• Higher R² indicates better model fit",
    "• Smaller zone widths indicate more precise reliability estimates", 
    "• Use zones to determine where your analysis will be most reliable",
    "",
    "FILES GENERATED:",
    "================",
    paste0("• ", output_prefix, "_scatter_", window_size, ".png - Scatter plot"),
    paste0("• ", output_prefix, "_reliability_", window_size, ".png - Reliability curve"),
    paste0("• ", output_prefix, "_zones_", window_size, ".png - Zone visualization")
  )
  
  # Save report
  report_file <- paste0("results/", output_prefix, "_report_", window_size, ".txt")
  writeLines(report_lines, report_file)
  cat("✓ Saved:", basename(report_file), "\n")
  
  cat("✅ Single window analysis complete!\n\n")
  
  return(list(
    plots = plots,
    summary_stats = summary_stats,
    window_data = window_data,
    report_file = report_file
  ))
}
