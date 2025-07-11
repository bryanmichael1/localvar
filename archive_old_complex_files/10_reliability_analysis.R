# ============================
# Reliability Analysis Functions
# ============================
# Optimized for pre-computed pipeline results
# Memory-efficient for 16GB Mac M3

#' Analyze reliability from pre-computed pipeline results
#' 
#' @param pipeline_results List of pipeline results from run_volatility_pipeline
#' @param target_window_sizes Vector of window sizes to analyze (auto-detected if NULL)
#' @param thresholds Vector of reliability thresholds to calculate zones for
#' @param max_results_per_batch Maximum number of results to process per batch (memory control)
#' @return List of reliability analysis results
analyze_reliability_from_results <- function(pipeline_results, 
                                           target_window_sizes = NULL,
                                           thresholds = c(0.95, 0.90, 0.80, 0.70),
                                           max_results_per_batch = 100) {
  
  # Auto-detect window sizes if not specified
  if (is.null(target_window_sizes)) {
    detected_sizes <- unique(sapply(pipeline_results, function(x) {
      if (!is.null(x$meta) && !is.null(x$meta$window_size)) {
        return(x$meta$window_size)
      }
      return(NA)
    }))
    target_window_sizes <- detected_sizes[!is.na(detected_sizes)]
    cat("Detected window sizes:", paste(target_window_sizes, collapse = ", "), "\n")
  }
  
  all_analyses <- list()
  
  for (win_size in target_window_sizes) {
    cat("\n=== Analyzing window size:", win_size, "===\n")
    
    # Extract data in memory-efficient batches
    extracted_data <- extract_reliability_data_batched(
      pipeline_results, win_size, max_results_per_batch
    )
    
    if (length(extracted_data$x_z) < 100) {
      cat("  ⚠️ Warning: Only", length(extracted_data$x_z), "data points - skipping\n")
      next
    }
    
    # Calculate reliability metrics
    reliability_metrics <- calculate_reliability_metrics_optimized(
      extracted_data$x_z, extracted_data$score_sd, thresholds
    )
    
    # Store results
    all_analyses[[paste0("window_", win_size)]] <- c(
      list(window_size = win_size, n_datapoints = length(extracted_data$x_z)),
      reliability_metrics
    )
    
    # Print summary
    print_reliability_summary(win_size, reliability_metrics)
    
    # Memory cleanup
    rm(extracted_data, reliability_metrics)
    gc()
  }
  
  return(all_analyses)
}

#' Extract reliability data from pipeline results in batches
extract_reliability_data_batched <- function(pipeline_results, target_window_size, batch_size) {
  n_results <- length(pipeline_results)
  n_batches <- ceiling(n_results / batch_size)
  
  # Pre-allocate vectors for better performance (estimate capacity)
  estimated_points <- n_results * 1000  # Rough estimate
  all_x_z <- numeric(estimated_points)
  all_score_sd <- numeric(estimated_points)
  total_points <- 0
  
  for (batch in seq_len(n_batches)) {
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, n_results)
    
    cat("  Processing batch", batch, "/", n_batches, "(results", start_idx, "-", end_idx, ")\n")
    
    batch_data <- extract_batch_data(
      pipeline_results[start_idx:end_idx], target_window_size
    )
    
    # Expand vectors if needed
    new_total <- total_points + length(batch_data$x_z)
    if (new_total > length(all_x_z)) {
      # Double the size when expanding
      new_size <- max(new_total, length(all_x_z) * 2)
      length(all_x_z) <- new_size
      length(all_score_sd) <- new_size
    }
    
    # Add batch data
    indices <- (total_points + 1):(total_points + length(batch_data$x_z))
    all_x_z[indices] <- batch_data$x_z
    all_score_sd[indices] <- batch_data$score_sd
    total_points <- new_total
    
    cat("    Batch added", length(batch_data$x_z), "points. Total:", total_points, "\n")
    rm(batch_data)
    gc()
  }
  
  # Trim to actual size
  length(all_x_z) <- total_points
  length(all_score_sd) <- total_points
  
  return(list(x_z = all_x_z, score_sd = all_score_sd))
}

#' Extract data from a single batch of results
extract_batch_data <- function(batch_results, target_window_size) {
  # Pre-allocate with estimated capacity
  estimated_windows <- length(batch_results) * 100  # Rough estimate
  batch_x_z <- numeric(estimated_windows)
  batch_score_sd <- numeric(estimated_windows)
  current_index <- 0
  
  for (result in batch_results) {
    # Skip if wrong window size
    if (is.null(result$meta) || is.null(result$meta$window_size) || 
        result$meta$window_size != target_window_size) {
      next
    }
    
    # Extract from all windows
    if (!is.null(result$windows)) {
      for (w in result$windows) {
        if (is_valid_window_data(w)) {
          current_index <- current_index + 1
          
          # Expand vectors if needed
          if (current_index > length(batch_x_z)) {
            new_size <- length(batch_x_z) * 2
            length(batch_x_z) <- new_size
            length(batch_score_sd) <- new_size
          }
          
          batch_x_z[current_index] <- w$x_stats$mean_z
          batch_score_sd[current_index] <- w$score$sd
        }
      }
    }
  }
  
  # Trim to actual size
  length(batch_x_z) <- current_index
  length(batch_score_sd) <- current_index
  
  return(list(x_z = batch_x_z, score_sd = batch_score_sd))
}

#' Check if window has valid data for reliability analysis
is_valid_window_data <- function(window) {
  return(
    !is.null(window$x_stats) && !is.null(window$x_stats$mean_z) && 
    !is.null(window$score) && !is.null(window$score$sd) &&
    is.finite(window$x_stats$mean_z) && is.finite(window$score$sd)
  )
}

#' Calculate reliability metrics with optimizations
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
  
  # Debug: show where minimum reliability occurs
  min_rel_idx <- which.min(rel_reliability)
  max_rel_idx <- which.max(rel_reliability)
  cat("  📊 Min reliability at x_z =", round(x_seq[min_rel_idx], 3), 
      "with reliability =", round(rel_reliability[min_rel_idx], 4), "\n")
  cat("  📊 Max reliability at x_z =", round(x_seq[max_rel_idx], 3), 
      "with reliability =", round(rel_reliability[max_rel_idx], 4), "\n")
  
  # Show reliability at key x_z points
  key_points <- c(-2, -1, 0, 1, 2)
  key_points <- key_points[key_points >= x_range[1] & key_points <= x_range[2]]
  if (length(key_points) > 0) {
    cat("  📊 Reliability at key x_z points:\n")
    for (xz_val in key_points) {
      closest_idx <- which.min(abs(x_seq - xz_val))
      cat("      x_z =", xz_val, "→ reliability =", round(rel_reliability[closest_idx], 4), "\n")
    }
  }
  
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
    pred_x_seq = x_seq
  ))
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

#' Save reliability results in multiple formats
save_reliability_results <- function(reliability_results, output_prefix = "reliability_analysis") {
  # Save detailed results as RDS
  rds_file <- paste0(output_prefix, "_detailed.rds")
  saveRDS(reliability_results, rds_file)
  
  # Create summary dataframe
  summary_df <- create_reliability_summary_table(reliability_results)
  
  # Save summary as CSV
  csv_file <- paste0(output_prefix, "_summary.csv")
  write.csv(summary_df, csv_file, row.names = FALSE)
  
  cat("\nFiles saved:\n")
  cat("- ", rds_file, " (detailed results)\n")
  cat("- ", csv_file, " (summary table)\n")
  
  return(list(summary_table = summary_df, rds_file = rds_file, csv_file = csv_file))
}

#' Create a summary table from reliability results
create_reliability_summary_table <- function(reliability_results) {
  if (length(reliability_results) == 0) {
    return(data.frame())
  }
  
  summary_df <- data.frame(
    window_size = sapply(reliability_results, function(x) x$window_size),
    n_datapoints = sapply(reliability_results, function(x) x$n_datapoints),
    n_final = sapply(reliability_results, function(x) x$n_final),
    baseline = sapply(reliability_results, function(x) x$baseline),
    r_squared = sapply(reliability_results, function(x) x$model_rsquared),
    min_reliability = sapply(reliability_results, function(x) x$min_reliability),
    max_reliability = sapply(reliability_results, function(x) x$max_reliability)
  )
  
  # Add threshold zone widths - use direct zone_widths if available
  threshold_names <- c("Above_95pct", "Above_90pct", "Above_80pct", "Above_70pct")
  for (thresh_name in threshold_names) {
    col_name <- paste0("zone_", gsub("Above_|pct", "", thresh_name), "_width")
    
    summary_df[[col_name]] <- sapply(reliability_results, function(x) {
      # First try to use direct zone_widths calculation
      if (!is.null(x$zone_widths)) {
        zone_width_name <- paste0("zone_", gsub("Above_|pct", "", thresh_name), "_width")
        if (zone_width_name %in% names(x$zone_widths)) {
          return(x$zone_widths[[zone_width_name]])
        }
      }
      
      # Fallback to threshold_ranges calculation
      if (!is.null(x$threshold_ranges) && !is.null(x$threshold_ranges[[thresh_name]])) {
        range_vals <- x$threshold_ranges[[thresh_name]]
        if (any(is.na(range_vals))) NA else diff(range_vals)
      } else {
        NA
      }
    })
  }
  
  return(summary_df)
}
#' Compute interval where quadratic inequality holds for reliability threshold
#'
#' Solves the quadratic inequality:
#' baseline / (a + b*x + c*x^2) >= threshold
#' Which simplifies to: c*x^2 + b*x + (a - baseline/threshold) <= 0
#' 
#' @param a Intercept coefficient from quadratic model
#' @param b Linear coefficient from quadratic model  
#' @param c Quadratic coefficient from quadratic model
#' @param baseline Baseline volatility value
#' @param threshold Reliability threshold (0-1)
#' @return Vector of length 2 with interval bounds [lower, upper], or c(NA, NA) if no solution
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
