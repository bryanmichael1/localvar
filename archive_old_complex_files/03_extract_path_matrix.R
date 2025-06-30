extract_window_matrix <- function(results_list, path) {
  path_parts <- strsplit(path, "/", fixed = TRUE)[[1]]

  n_runs <- length(results_list)
  
  # Pre-calculate max windows more efficiently
  max_windows <- 0
  for (i in seq_len(min(10, n_runs))) { # Check only first 10 to estimate
    n_win <- length(results_list[[i]]$windows)
    if (n_win > max_windows) max_windows <- n_win
  }
  
  # Use a more memory-efficient approach for large datasets
  if (n_runs > 500 || max_windows > 1000) {
    cat("Using memory-efficient extraction for large dataset\n")
    return(extract_window_matrix_efficient(results_list, path_parts))
  }

  out_matrix <- matrix(NA_real_, nrow = n_runs, ncol = max_windows)

  for (i in seq_len(n_runs)) {
    windows <- results_list[[i]]$windows
    for (j in seq_along(windows)) {
      current <- windows[[j]]

      for (p in path_parts) {
        if (!is.list(current) || is.null(current[[p]])) {
          current <- NULL
          break
        }
        current <- current[[p]]
      }

      if (!is.null(current) && is.atomic(current) && length(current) == 1) {
        out_matrix[i, j] <- current
      }
    }
  }

  rownames(out_matrix) <- paste0("sim_", seq_len(n_runs))
  colnames(out_matrix) <- paste0("window_", seq_len(max_windows))

  return(out_matrix)
}

# Memory-efficient version for large datasets
extract_window_matrix_efficient <- function(results_list, path_parts) {
  n_runs <- length(results_list)
  
  # Collect values as vectors instead of pre-allocating matrix
  all_values <- list()
  max_windows <- 0
  
  for (i in seq_len(n_runs)) {
    windows <- results_list[[i]]$windows
    run_values <- numeric(length(windows))
    run_values[] <- NA_real_
    
    for (j in seq_along(windows)) {
      current <- windows[[j]]
      
      for (p in path_parts) {
        if (!is.list(current) || is.null(current[[p]])) {
          current <- NULL
          break
        }
        current <- current[[p]]
      }
      
      if (!is.null(current) && is.atomic(current) && length(current) == 1) {
        run_values[j] <- current
      }
    }
    
    all_values[[i]] <- run_values
    if (length(run_values) > max_windows) max_windows <- length(run_values)
    
    # Periodic garbage collection for very large datasets
    if (i %% 100 == 0) {
      gc()
      cat("Processed", i, "of", n_runs, "runs\n")
    }
  }
  
  # Convert to matrix only at the end
  out_matrix <- matrix(NA_real_, nrow = n_runs, ncol = max_windows)
  for (i in seq_len(n_runs)) {
    n_vals <- length(all_values[[i]])
    out_matrix[i, 1:n_vals] <- all_values[[i]]
  }
  
  rownames(out_matrix) <- paste0("sim_", seq_len(n_runs))
  colnames(out_matrix) <- paste0("window_", seq_len(max_windows))
  
  return(out_matrix)
}
