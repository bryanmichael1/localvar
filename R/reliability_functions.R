# ========================================
# RELIABILITY ANALYSIS FUNCTIONS
# ========================================
# Functions for calculating and analyzing reliability zones

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
  volatility <- volatility_values[valid_idx]
  
  cat("Fitting reliability model with", length(x_z), "data points\n")
  
  # Fit quadratic model: volatility ~ a + b*x_z + c*x_z^2
  model_data <- data.frame(x_z = x_z, volatility = abs(volatility))
  
  # Remove extreme outliers for model stability
  q99 <- quantile(abs(model_data$x_z), 0.99, na.rm = TRUE)
  model_data <- model_data[abs(model_data$x_z) <= q99, ]
  
  # Fit the model
  volatility_model <- lm(volatility ~ x_z + I(x_z^2), data = model_data)
  
  # Calculate baseline (volatility in center region)
  central_data <- model_data[abs(model_data$x_z) < 0.5, ]
  baseline_volatility <- if (nrow(central_data) > 0) {
    mean(central_data$volatility, na.rm = TRUE)
  } else {
    mean(model_data$volatility, na.rm = TRUE)
  }
  
  # Create prediction grid
  x_range <- range(model_data$x_z, na.rm = TRUE)
  x_grid <- seq(x_range[1], x_range[2], length.out = 1000)
  
  # Predict volatility across the range
  predicted_volatility <- predict(volatility_model, newdata = data.frame(x_z = x_grid, x_z_sq = x_grid^2))
  predicted_volatility <- pmax(predicted_volatility, 0.001)  # Ensure positive
  
  # Calculate reliability (baseline / predicted_volatility)
  reliability <- baseline_volatility / predicted_volatility
  
  # Find reliability zones for each threshold
  zones <- list()
  for (threshold in thresholds) {
    zone <- find_reliability_zone(x_grid, reliability, threshold)
    zone_name <- paste0("zone_", round(threshold * 100))
    zones[[zone_name]] <- zone
  }
  
  # Model diagnostics
  model_rsq <- summary(volatility_model)$r.squared
  reliability_range <- range(reliability, na.rm = TRUE)
  
  cat("Model R²:", round(model_rsq, 4), "\n")
  cat("Reliability range:", round(reliability_range[1], 4), "to", round(reliability_range[2], 4), "\n")
  
  # Print zone information
  for (i in seq_along(zones)) {
    zone_name <- names(zones)[i]
    zone_info <- zones[[i]]
    threshold_pct <- round(thresholds[i] * 100)
    
    if (!is.na(zone_info$width)) {
      cat(threshold_pct, "% zone: [", round(zone_info$lower, 3), ",", 
          round(zone_info$upper, 3), "] width =", round(zone_info$width, 3), "\n")
    } else {
      cat(threshold_pct, "% zone: Not achievable\n")
    }
  }
  
  return(list(
    zones = zones,
    model = volatility_model,
    model_rsq = model_rsq,
    baseline_volatility = baseline_volatility,
    reliability_curve = data.frame(x_z = x_grid, reliability = reliability),
    data_summary = list(
      n_points = length(x_z),
      x_range = x_range,
      reliability_range = reliability_range
    )
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
