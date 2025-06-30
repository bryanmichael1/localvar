# ========================================
# HELPER FUNCTIONS
# ========================================
# Utility functions for data validation and formatting

#' Check if data is in the correct format for analysis
validate_data_format <- function(datasets) {
  
  if (!is.list(datasets)) {
    stop("Data must be a list of datasets")
  }
  
  if (length(datasets) == 0) {
    stop("No datasets provided")
  }
  
  # Check first dataset structure
  first_dataset <- datasets[[1]]
  required_columns <- c("x", "y_norm")
  
  if (!all(required_columns %in% names(first_dataset))) {
    missing_cols <- setdiff(required_columns, names(first_dataset))
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Check for numeric data
  if (!is.numeric(first_dataset$x) || !is.numeric(first_dataset$y_norm)) {
    stop("Columns 'x' and 'y_norm' must be numeric")
  }
  
  # Vectorized validation for all datasets
  dataset_valid <- vapply(datasets, function(dataset) {
    all(required_columns %in% names(dataset)) && nrow(dataset) > 0
  }, logical(1))
  
  if (!all(dataset_valid)) {
    invalid_indices <- which(!dataset_valid)
    missing_structure <- sapply(invalid_indices, function(i) {
      dataset <- datasets[[i]]
      if (!all(required_columns %in% names(dataset))) {
        paste0("Dataset ", i, ": missing columns")
      } else {
        paste0("Dataset ", i, ": empty")
      }
    })
    stop("Validation failed:\n", paste(missing_structure, collapse = "\n"))
  }
  
  cat("✓ Data format validation passed\n")
  cat("✓", length(datasets), "datasets with", nrow(first_dataset), "rows each\n")
  
  return(TRUE)
}

#' Create a simple summary of datasets
summarize_datasets <- function(datasets) {
  
  if (length(datasets) == 0) return(NULL)
  
  # Calculate summary statistics
  x_means <- sapply(datasets, function(d) mean(d$x, na.rm = TRUE))
  y_means <- sapply(datasets, function(d) mean(d$y_norm, na.rm = TRUE))
  x_sds <- sapply(datasets, function(d) sd(d$x, na.rm = TRUE))
  y_sds <- sapply(datasets, function(d) sd(d$y_norm, na.rm = TRUE))
  
  summary_stats <- data.frame(
    n_datasets = length(datasets),
    n_rows_per_dataset = nrow(datasets[[1]]),
    x_mean_avg = mean(x_means, na.rm = TRUE),
    x_mean_sd = sd(x_means, na.rm = TRUE),
    x_sd_avg = mean(x_sds, na.rm = TRUE),
    y_mean_avg = mean(y_means, na.rm = TRUE),
    y_mean_sd = sd(y_means, na.rm = TRUE),
    y_sd_avg = mean(y_sds, na.rm = TRUE)
  )
  
  return(summary_stats)
}

#' Print a nice summary of analysis results
print_analysis_summary <- function(results) {
  
  cat("\n=== RELIABILITY ANALYSIS SUMMARY ===\n")
  
  # Basic info
  if (!is.null(results$meta)) {
    cat("Window size:", results$meta$window_size, "\n")
    cat("Datasets analyzed:", results$meta$n_datasets, "\n")
    cat("Data points used:", results$meta$n_datapoints, "\n")
  }
  
  # Model quality
  cat("Model R²:", round(results$model_rsq, 4), "\n")
  cat("Baseline volatility:", round(results$baseline_volatility, 4), "\n")
  
  # Reliability range
  if (!is.null(results$data_summary)) {
    rel_range <- results$data_summary$reliability_range
    cat("Reliability range:", round(rel_range[1], 4), "to", round(rel_range[2], 4), "\n")
  }
  
  # Zone information
  cat("\nReliability Zones:\n")
  for (zone_name in names(results$zones)) {
    zone_info <- results$zones[[zone_name]]
    threshold_pct <- gsub("zone_", "", zone_name)
    
    if (!is.na(zone_info$width)) {
      cat(sprintf("  %s%%: [%6.3f, %6.3f] width = %6.3f\n", 
                  threshold_pct, zone_info$lower, zone_info$upper, zone_info$width))
    } else {
      cat(sprintf("  %s%%: Not achievable\n", threshold_pct))
    }
  }
  
  cat("\n")
}

#' Check system requirements and package availability
check_system_requirements <- function() {
  
  cat("Checking system requirements...\n")
  
  # Check R version
  r_version <- R.version.string
  cat("✓ R version:", r_version, "\n")
  
  # Check required packages
  required_packages <- c("data.table", "dplyr", "ggplot2")
  
  for (pkg in required_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      cat("✓ Package", pkg, "available\n")
    } else {
      cat("❌ Package", pkg, "missing - install with install.packages('", pkg, "')\n")
    }
  }
  
  # Check memory
  memory_limit <- memory.limit()
  if (is.finite(memory_limit)) {
    cat("✓ Memory limit:", memory_limit, "MB\n")
  } else {
    cat("✓ Memory limit: Unlimited\n")
  }
  
  cat("System check complete.\n\n")
}

#' Create example dataset for testing
create_example_data <- function(n_datasets = 10, n_points = 1000) {
  
  cat("Creating", n_datasets, "example datasets with", n_points, "points each...\n")
  
  set.seed(123)  # For reproducible results
  
  datasets <- list()
  
  for (i in 1:n_datasets) {
    # Create predictor variable (t-distributed for realistic complexity)
    x <- rt(n_points, df = 3)
    
    # Keep values within reasonable range
    x <- pmax(pmin(x, 4), -4)
    
    # Create outcome with sinusoidal pattern + noise
    y_norm <- sin(x) + rnorm(n_points, sd = 0.3)
    
    datasets[[i]] <- data.frame(
      x = x,
      y_norm = y_norm,
      dataset_id = i
    )
  }
  
  cat("✓ Example data created\n")
  return(datasets)
}

# ========================================
# DATA PROCESSING UTILITIES
# ========================================
# High-performance utility functions for data processing

#' Efficiently extract window data from analysis results
#' 
#' @param result Analysis result from run_volatility_pipeline
#' @return List with extracted x_z and score_sd values
extract_window_data_optimized <- function(result) {
  if (is.null(result) || is.null(result$windows)) {
    return(list(x_z = numeric(0), score_sd = numeric(0)))
  }
  
  n_windows <- length(result$windows)
  x_z_vals <- numeric(n_windows)
  score_sd_vals <- numeric(n_windows)
  valid_count <- 0
  
  for (i in seq_len(n_windows)) {
    window <- result$windows[[i]]
    if (!is.null(window$x_stats) && !is.null(window$score)) {
      x_z <- window$x_stats$mean_z
      volatility <- window$score$sd
      
      if (is.finite(x_z) && is.finite(volatility)) {
        valid_count <- valid_count + 1
        x_z_vals[valid_count] <- x_z
        score_sd_vals[valid_count] <- volatility
      }
    }
  }
  
  return(list(
    x_z = x_z_vals[1:valid_count],
    score_sd = score_sd_vals[1:valid_count]
  ))
}

#' Robust data combination with pre-allocation
#' 
#' @param data_list List of data vectors to combine
#' @param initial_size Initial pre-allocation size
#' @return Combined vector
combine_data_efficient <- function(data_list, initial_size = 1000) {
  if (length(data_list) == 0) return(numeric(0))
  
  # Estimate total size
  total_size <- sum(vapply(data_list, length, integer(1)))
  if (total_size == 0) return(numeric(0))
  
  # Use more efficient combination for small lists
  if (length(data_list) <= 10) {
    return(unlist(data_list, use.names = FALSE))
  }
  
  # Pre-allocate for large combinations
  result <- numeric(total_size)
  current_pos <- 1
  
  for (data_vec in data_list) {
    if (length(data_vec) > 0) {
      end_pos <- current_pos + length(data_vec) - 1
      result[current_pos:end_pos] <- data_vec
      current_pos <- end_pos + 1
    }
  }
  
  return(result)
}

#' Optimized summary statistics calculation
#' 
#' @param datasets List of datasets
#' @return Summary statistics data frame
summarize_datasets_optimized <- function(datasets) {
  if (length(datasets) == 0) return(NULL)
  
  # Vectorized statistics calculation
  n_datasets <- length(datasets)
  
  # Extract all x and y values efficiently
  all_x <- lapply(datasets, `[[`, "x")
  all_y <- lapply(datasets, `[[`, "y_norm")
  
  # Calculate statistics using vectorized operations
  x_means <- vapply(all_x, mean, numeric(1), na.rm = TRUE)
  y_means <- vapply(all_y, mean, numeric(1), na.rm = TRUE)
  x_sds <- vapply(all_x, sd, numeric(1), na.rm = TRUE)
  y_sds <- vapply(all_y, sd, numeric(1), na.rm = TRUE)
  
  # Build summary efficiently
  data.frame(
    n_datasets = n_datasets,
    n_rows_per_dataset = length(all_x[[1]]),
    x_mean_avg = mean(x_means, na.rm = TRUE),
    x_mean_sd = sd(x_means, na.rm = TRUE),
    x_sd_avg = mean(x_sds, na.rm = TRUE),
    y_mean_avg = mean(y_means, na.rm = TRUE),
    y_mean_sd = sd(y_means, na.rm = TRUE),
    y_sd_avg = mean(y_sds, na.rm = TRUE)
  )
}

# ========================================
# EXISTING FUNCTIONS (updated for compatibility)
# ========================================
