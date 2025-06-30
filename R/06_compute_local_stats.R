# Compute local statistics for each window
compute_local_stats <- function(obj, 
                                stats = list(iqr = function(x) IQR(x, 
                                                                   na.rm = FALSE, 
                                                                   type = 7)), 
                                na_rm = FALSE) {
  # Detect object type
  if (inherits(obj, "windowed_data")) {
    windows <- obj$windows
    meta <- obj$meta
    if (is.null(meta)) meta <- list()
  } else if (is.list(obj) && all(vapply(obj, is.list, logical(1)))) {
    windows <- obj
    meta <- list()
  } else {
    stop("Input must be a 'windowed_data' object or a list of window folds.")
  }

  # Stat dispatcher - handles different function types
  call_stat <- function(fun, values) {
    if (is.character(fun) && length(fun) == 1) fun <- get(fun, envir = parent.frame())
    if (is.list(fun) && is.function(fun[[1]])) return(do.call(fun[[1]], c(list(values), fun[-1])))
    if (is.function(fun)) {
      fn_formals <- names(formals(fun))
      if ("na.rm" %in% fn_formals) return(fun(values, na.rm = na_rm)) # nolint
      if (isTRUE(attr(fun, "na_aware"))) return(fun(values))
      return(fun(values))
    }
    stop("Each stat must be a function, a string, or list(fun, ...)")
  }

  # Loop over windows and attach stats
  for (i in seq_along(windows)) {
    w <- windows[[i]]
    y_win <- w$y_win
    
    # Check for NAs
    if (!na_rm && anyNA(y_win)) {
      stop(sprintf(
        "NA values in fold %d (indices %d:%d); set na_rm = TRUE to bypass.",
        w$id$fold_id, w$id$start_idx, w$id$end_idx
      ))
    }
    
    # Initialize y_stats if needed
    if (is.null(windows[[i]]$y_stats)) windows[[i]]$y_stats <- list()
    
    # Compute each statistic
    for (stat_name in names(stats)) {
      res <- call_stat(stats[[stat_name]], y_win)
      
      # Validate result
      if (is.recursive(res) && !is.atomic(res)) {
        stop(sprintf(
          "Stat '%s' returned a complex object. Wrap your function to return basic summaries.",
          stat_name
        ))
      }
      
      # Store result based on type
      if (is.atomic(res) && length(res) == 1) {
        windows[[i]]$y_stats[[stat_name]] <- res
      } else if (is.atomic(res) && !is.null(names(res))) {
        for (k in seq_along(res)) {
          windows[[i]]$y_stats[[paste0(stat_name, "_", names(res)[k])]] <- res[[k]]
        }
      } else if (is.atomic(res)) {
        for (k in seq_along(res)) {
          windows[[i]]$y_stats[[paste0(stat_name, "_", k)]] <- res[[k]]
        }
      } else {
        stop(sprintf("Unsupported return from stat '%s'", stat_name))
      }
    }
  }

  # Update metadata
  meta$computed_stats <- stats
  meta$na_rm <- na_rm
  meta$history$compute_local_stats <- list(
    timestamp = Sys.time(),
    stats = names(stats)
  )

  # Return enriched object or list
  if (inherits(obj, "windowed_data")) {
    obj$windows <- windows
    obj$meta <- meta
    return(obj)
  } else {
    return(windows)
  }
}