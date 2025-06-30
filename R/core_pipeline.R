# ========================================
# CORE ANALYSIS PIPELINE
# ========================================
# This file contains the main statistical functions
# You typically don't need to modify this file

# ========================================
# CORE ANALYSIS PIPELINE
# ========================================
# This file contains the main statistical functions
# Reflects the original sophisticated design from archived files

#' Main volatility analysis pipeline
#' 
#' Analyzes how much your model's predictions vary across different regions of your data
#' This function implements the full pipeline from your original design:
#' 1. Make windows with sophisticated sorting logic
#' 2. Compute local statistics for each window
#' 3. Compute global baseline statistics
#' 4. Score windows against global baseline
#' 
#' @param x Predictor variable (numeric vector)
#' @param y Outcome variable (numeric vector) 
#' @param window_size Number of data points per analysis window
#' @param step_size Step size for sliding windows (default: 1)
#' @param x_type Type of predictor ("continuous" or "discrete")
#' @param y_type Type of outcome ("continuous" or "discrete")
#' @param x_k Cardinality for discrete x (if applicable)
#' @param y_k Cardinality for discrete y (if applicable)
#' @param sort Sorting strategy: TRUE, FALSE, "x", or c("x", "y")
#' @param local_stats List of statistical functions to compute per window
#' @param global_source Source for global stats: "fold" or "full"
#' @param global_agg Aggregation method: "median" or "mean"
#' @return List with analysis results following original windowed_data structure
run_volatility_pipeline <- function(x, y, 
                                   window_size = 30,
                                   step_size = 1,
                                   x_type = "continuous", 
                                   y_type = "continuous",
                                   x_k = NULL,
                                   y_k = NULL,
                                   sort = TRUE,
                                   local_stats = list(sd = function(x) sd(x, na.rm = FALSE)),
                                   global_source = "fold",
                                   global_agg = "median") {
  
  # Input validation
  if (length(x) != length(y)) {
    stop("x and y must have the same length")
  }
  
  if (length(x) < window_size * 2) {
    stop("Data too short for specified window size")
  }

  # Determine sorting logic based on data types (if provided)
  if (!is.null(x_type) && !is.null(y_type)) {
    sort_logic <- assign_window_logic(x_type, y_type, x_k, y_k)$sort
  } else {
    sort_logic <- sort
  }

  # 1. Make windows using sophisticated windowing system
  obj <- make_sliding_windows(
    x = x, 
    y = y,
    window_size = window_size,
    step_size = step_size,
    sort = sort_logic
  )

  # 2. Compute local window statistics
  obj <- compute_local_stats(
    obj,
    stats = local_stats
  )

  # 3. Compute global baseline statistics
  obj <- global_y_stats(
    obj,
    stats = local_stats,
    source = global_source,
    agg = global_agg
  )

  # 4. Score windows against global baseline
  obj <- score_windows(obj)

  return(obj)
}

#' Assign window logic based on data types (from original design)
#' 
#' Determines sorting strategy based on x and y variable types
#' This recreates the logic from your original 01_assign_window_logic.R
assign_window_logic <- function(x_type, y_type, x_k = NULL, y_k = NULL) {
  x_type <- match.arg(x_type, c("continuous", "discrete"))
  y_type <- match.arg(y_type, c("continuous", "discrete"))
  
  # Continuous x cases
  if (x_type == "continuous") {
    if (y_type == "continuous") return(list(sort = "x"))
    stop("Estimation on discrete y is not supported for continuous–discrete combinations.")
  }
  
  # Discrete x, continuous y
  if (y_type == "continuous") return(list(sort = c("x", "y")))
  
  # Both discrete - need cardinalities
  if (is.null(x_k) || is.null(y_k)) {
    stop("Cardinalities x_k and y_k must be provided for discrete–discrete data.")
  }
  if (x_k <= 2 && y_k <= 2) {
    stop("Binary–binary data is not supported for window-based estimation.")
  }
  if (x_k > y_k) {
    stop("Estimation on lower-cardinality discrete axis is not supported (x_k > y_k).")
  }
  
  return(list(sort = c("x", "y")))
}

#' Create sliding windows using sophisticated design
#' 
#' This implements the sophisticated windowing system from the original design
#' with proper z-scaling, sophisticated sorting, and edge window handling
make_sliding_windows <- function(x, y, window_size = 30, step_size = 1, sort = TRUE) {
  # Basic input validation
  stopifnot(length(x) == length(y))
  stopifnot(step_size >= 1, window_size >= 1)

  # Compute z-scaled version of x
  x_z <- scale(x)[, 1]  # Convert to numeric vector from matrix

  # Determine ordering using your original logic
  index_vec <- seq_along(x)

  is_xy_sort <- function(s) {
    is.character(s) && length(s) == 2 &&
      identical(unname(s), c("x", "y"))
  }

  if (isFALSE(sort)) {
    ord <- index_vec
  } else if (isTRUE(sort)) {
    if (length(unique(x)) <= 2) {
      ord <- order(y)
    } else {
      ord <- order(x)
    }
  } else if (identical(sort, "x")) {
    ord <- order(x)
  } else if (is_xy_sort(sort)) {
    ord <- order(x, y)
  } else {
    stop(
      "'sort' must be TRUE, FALSE, \"x\", or c(\"x\", \"y\").\n",
      "Received: ", deparse(sort)
    )
  }

  # Re-order all vectors
  x         <- x[ord]
  y         <- y[ord]
  x_z       <- x_z[ord]
  index_vec <- index_vec[ord]

  n         <- length(y)
  windows   <- vector("list", 0L)
  fold_id   <- 1

  # Build window function (from your original design)
  build_win <- function(slice_idx) {
    list(
      id = list(
        fold_id   = fold_id,
        start_idx = min(slice_idx),
        end_idx   = max(slice_idx),
        range_x   = range(x[slice_idx]),
        idx       = index_vec[slice_idx]
      ),
      x_stats = list(
        mean     = mean(x[slice_idx]),
        sd       = stats::sd(x[slice_idx]),
        mean_z   = mean(x_z[slice_idx]),
        sd_z     = stats::sd(x_z[slice_idx])
      ),
      y_stats = list(
        mean = mean(y[slice_idx]),
        sd   = stats::sd(y[slice_idx])
      ),
      x_win   = x[slice_idx],
      x_z_win = x_z[slice_idx],
      y_win   = y[slice_idx]
    )
  }

  # Create edge windows at the start (your original design)
  for (w in 2:(window_size - 1)) {
    windows[[fold_id]] <- build_win(1:w)
    fold_id <- fold_id + 1
  }

  # Create main sliding windows
  for (start in seq(1, n - window_size + 1, by = step_size)) {
    windows[[fold_id]] <- build_win(start:(start + window_size - 1))
    fold_id <- fold_id + 1
  }

  # Create edge windows at the end
  for (w in rev(2:(window_size - 1))) {
    windows[[fold_id]] <- build_win((n - w + 1):n)
    fold_id <- fold_id + 1
  }

  # Return windowed_data structure (your original format)
  structure(
    list(
      data    = list(x = x, y = y, x_z = x_z, idx = index_vec),
      windows = windows,
      meta    = list(
        history = list(make_windows = list(timestamp = Sys.time())),
        window_size = window_size,
        step_size   = step_size,
        sort        = sort,
        n_windows   = length(windows),
        n_data      = n
      )
    ),
    class = "windowed_data"
  )
}

#' Compute local statistics for each window (vectorized version)
#' 
#' This implements the sophisticated local statistics computation from the original design
compute_local_stats <- function(obj, stats = list(sd = function(x) sd(x, na.rm = FALSE)), na_rm = FALSE) {
  # Detect object type
  if (inherits(obj, "windowed_data")) {
    windows <- obj$windows
    meta <- obj$meta
    if (is.null(meta)) meta <- list()
  } else {
    stop("Input must be a 'windowed_data' object.")
  }

  # Stat dispatcher - handles different function types (from your original)
  call_stat <- function(fun, values) {
    if (is.character(fun) && length(fun) == 1) fun <- get(fun, envir = parent.frame())
    if (is.list(fun) && is.function(fun[[1]])) return(do.call(fun[[1]], c(list(values), fun[-1])))
    if (is.function(fun)) {
      fn_formals <- names(formals(fun))
      if ("na.rm" %in% fn_formals) return(fun(values, na.rm = na_rm))
      if (isTRUE(attr(fun, "na_aware"))) return(fun(values))
      return(fun(values))
    }
    stop("Each stat must be a function, a string, or list(fun, ...)")
  }

  # Pre-check for NAs across all windows (vectorized)
  if (!na_rm) {
    na_windows <- vapply(windows, function(w) anyNA(w$y_win), logical(1))
    if (any(na_windows)) {
      na_indices <- which(na_windows)
      first_na <- na_indices[1]
      w <- windows[[first_na]]
      stop(sprintf(
        "NA values in fold %d (indices %d:%d); set na_rm = TRUE to bypass.",
        w$id$fold_id, w$id$start_idx, w$id$end_idx
      ))
    }
  }

  # Vectorized computation of statistics for all windows
  for (stat_name in names(stats)) {
    stat_fun <- stats[[stat_name]]
    
    # Compute statistic for all windows at once
    stat_values <- vapply(windows, function(w) {
      call_stat(stat_fun, w$y_win)
    }, numeric(1))
    
    # Assign results back to windows
    for (i in seq_along(windows)) {
      if (is.null(windows[[i]]$y_stats)) windows[[i]]$y_stats <- list()
      windows[[i]]$y_stats[[stat_name]] <- stat_values[i]
    }
  }

  # Update metadata
  if (is.null(obj$meta$history)) obj$meta$history <- list()
  obj$meta$history$compute_local_stats <- list(
    timestamp = Sys.time(),
    stats = names(stats)
  )
  obj$meta$computed_stats <- names(stats)

  # Update windows
  obj$windows <- windows
  
  return(obj)
}

#' Compute global baseline statistics
#' 
#' This implements the sophisticated global statistics computation from the original design
global_y_stats <- function(obj, stats = NULL, source = "fold", agg = "median") {
  # Input handling
  if (inherits(obj, "windowed_data")) {
    windows <- obj$windows
    y_full  <- obj$data$y
    meta    <- obj$meta
    if (is.null(stats)) {
      stats <- meta$computed_stats
      if (is.null(stats)) stop("No `stats` supplied and no record found in metadata.")
    }
  } else {
    stop("Input must be a 'windowed_data' object.")
  }

  # Compute global statistics
  global_stats <- list()
  
  for (stat_name in names(stats)) {
    if (source == "fold") {
      # Aggregate from window-level statistics
      local_vals <- vapply(windows, function(w) {
        if (stat_name %in% names(w$y_stats)) {
          w$y_stats[[stat_name]]
        } else {
          NA_real_
        }
      }, numeric(1))
      
      local_vals <- local_vals[!is.na(local_vals)]
      
      if (length(local_vals) == 0) {
        global_stats[[stat_name]] <- NA_real_
      } else {
        global_stats[[stat_name]] <- if (agg == "median") median(local_vals) else mean(local_vals)
      }
    } else {
      # Compute from full data
      stat_fun <- stats[[stat_name]]
      global_stats[[stat_name]] <- stat_fun(y_full)
    }
  }

  # Store global statistics
  obj$global_y_stats <- global_stats
  
  # Update metadata
  if (is.null(obj$meta$history)) obj$meta$history <- list()
  obj$meta$history$global_y_stats <- list(
    timestamp = Sys.time(),
    source = source,
    agg = agg,
    stats = names(stats)
  )

  return(obj)
}

#' Score windows against global baseline (vectorized version)
#' 
#' This implements the sophisticated window scoring from the original design
score_windows <- function(obj) {
  # Input validation
  if (!inherits(obj, "windowed_data")) stop("Input must be a 'windowed_data' object.")
  if (is.null(obj$windows) || length(obj$windows) == 0)
    stop("No windows found in object.")
  if (is.null(obj$global_y_stats))
    stop("Global statistics not found. Run global_y_stats() before scoring.")

  global_vals <- obj$global_y_stats
  windows     <- obj$windows

  # Get all stat names to score
  all_stat_names <- unique(unlist(lapply(windows, function(w) names(w$y_stats))))
  available_stats <- intersect(all_stat_names, names(global_vals))

  # Vectorized scoring for each statistic
  for (stat_name in available_stats) {
    global_val <- global_vals[[stat_name]]
    
    if (!is.null(global_val) && is.finite(global_val) && global_val > 0) {
      # Extract all local values for this statistic
      local_vals <- vapply(windows, function(w) {
        if (stat_name %in% names(w$y_stats)) {
          w$y_stats[[stat_name]]
        } else {
          NA_real_
        }
      }, numeric(1))
      
      # Vectorized scoring: local / global
      scores <- local_vals / global_val
      
      # Assign scores back to windows
      for (i in seq_along(windows)) {
        if (is.null(windows[[i]]$score)) windows[[i]]$score <- list()
        windows[[i]]$score[[stat_name]] <- scores[i]
      }
    } else {
      # Handle invalid global values
      for (i in seq_along(windows)) {
        if (is.null(windows[[i]]$score)) windows[[i]]$score <- list()
        windows[[i]]$score[[stat_name]] <- NA_real_
      }
    }
  }

  # Update object
  obj$windows <- windows
  
  # Update metadata
  if (is.null(obj$meta$history)) obj$meta$history <- list()
  obj$meta$history$score_windows <- list(
    timestamp = Sys.time(),
    metrics_scored = names(global_vals)
  )

  return(obj)
}
