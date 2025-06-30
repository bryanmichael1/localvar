# ========================================
# CORE ANALYSIS PIPELINE
# ========================================
# This file contains the main statistical functions
# You typically don't need to modify this file

#' Main volatility analysis pipeline
#' 
#' Analyzes how much your model's predictions vary across different regions of your data
#' 
#' @param x Predictor variable (numeric vector)
#' @param y Outcome variable (numeric vector) 
#' @param window_size Number of data points per analysis window
#' @param x_type Type of predictor ("continuous" or "discrete")
#' @param y_type Type of outcome ("continuous" or "discrete")
#' @return List with analysis results for each window
run_volatility_pipeline <- function(x, y, window_size, x_type = "continuous", y_type = "continuous") {
  
  # Input validation
  if (length(x) != length(y)) {
    stop("x and y must have the same length")
  }
  
  if (length(x) < window_size * 2) {
    stop("Data too short for specified window size")
  }
  
  # Create analysis windows
  windows <- make_sliding_windows(x, y, window_size)
  
  if (length(windows) == 0) {
    return(list(windows = NULL, meta = list(window_size = window_size, error = "No windows created")))
  }
  
  # Analyze each window
  results <- list()
  for (i in seq_along(windows)) {
    tryCatch({
      # Get window data
      window_data <- windows[[i]]
      
      # Calculate statistics for this window
      x_stats <- compute_window_x_stats(window_data$x)
      y_stats <- compute_window_y_stats(window_data$y)
      volatility_score <- score_window_volatility(window_data$x, window_data$y)
      
      # Store results
      results[[i]] <- list(
        window_id = i,
        x_stats = x_stats,
        y_stats = y_stats,
        score = volatility_score,
        data_points = nrow(window_data)
      )
      
    }, error = function(e) {
      # Skip failed windows
      NULL
    })
  }
  
  # Filter out NULL results
  results <- results[!sapply(results, is.null)]
  
  return(list(
    windows = results,
    meta = list(
      window_size = window_size,
      total_windows = length(results),
      x_type = x_type,
      y_type = y_type
    )
  ))
}

#' Create sliding windows from data
make_sliding_windows <- function(x, y, window_size) {
  n <- length(x)
  if (n < window_size) return(list())
  
  windows <- list()
  window_count <- 0
  
  for (start in 1:(n - window_size + 1)) {
    end <- start + window_size - 1
    
    window_data <- data.frame(
      x = x[start:end],
      y = y[start:end]
    )
    
    # Only keep windows with sufficient variation
    if (length(unique(window_data$x)) > 1 && length(unique(window_data$y)) > 1) {
      window_count <- window_count + 1
      windows[[window_count]] <- window_data
    }
  }
  
  return(windows)
}

#' Calculate statistics for predictor variable in a window
compute_window_x_stats <- function(x) {
  if (length(x) == 0) return(NULL)
  
  list(
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    mean_z = mean(scale(x), na.rm = TRUE)  # Standardized mean
  )
}

#' Calculate statistics for outcome variable in a window  
compute_window_y_stats <- function(y) {
  if (length(y) == 0) return(NULL)
  
  list(
    mean = mean(y, na.rm = TRUE),
    sd = sd(y, na.rm = TRUE),
    variance = var(y, na.rm = TRUE)
  )
}

#' Calculate volatility score for a window
#' This measures how much predictions vary within the window
score_window_volatility <- function(x, y) {
  if (length(x) < 3 || length(y) < 3) return(NULL)
  
  tryCatch({
    # Fit simple linear model within window
    model <- lm(y ~ x)
    
    # Calculate residual standard deviation (our volatility measure)
    residual_sd <- sd(residuals(model), na.rm = TRUE)
    
    list(
      sd = residual_sd,
      model_rsq = summary(model)$r.squared
    )
    
  }, error = function(e) {
    # If model fails, use simple standard deviation
    list(
      sd = sd(y, na.rm = TRUE),
      model_rsq = 0
    )
  })
}
