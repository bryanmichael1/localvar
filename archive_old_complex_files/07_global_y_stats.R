# Compute global statistics for y across all windows
global_y_stats <- function(
    obj,
    stats      = NULL,
    source     = c("fold", "full"),
    agg        = c("median", "mean"),
    trim       = FALSE,
    trim_prob  = 0.05,
    na_rm      = FALSE,
    force      = FALSE
) {
  source <- match.arg(source)
  agg    <- match.arg(agg)

  # Argument consistency checks
  if (!force) {
    if (source == "full" && agg == "median")
      stop("'agg = \"median\"' is only valid when source = \"fold\".")
    if (trim && agg == "median")
      stop("Trimming is not supported with agg = \"median\".")
    if (trim && (trim_prob < 0 || trim_prob > 0.05))
      stop("trim_prob must be in [0, 0.05] unless force = TRUE.")
  } else {
    # Force mode warnings
    if (source == "full" && agg == "median") {
      warning("'agg = \"median\"' is ignored when source = \"full\".")
    }
    if (trim && agg == "median") {
      warning("Trimming is redundant with agg = \"median\".")
    }
    if (trim && (trim_prob < 0 || trim_prob > 0.5)) {
      warning("Using high trim_prob (> 0.05) with force = TRUE. Interpret with care.")
    }
  }

  # Input handling
  if (inherits(obj, "windowed_data")) {
    windows <- obj$windows
    y_full  <- obj$data$y
    x_full  <- obj$data$x
    meta    <- obj$meta
    if (is.null(stats)) {
      stats <- meta$computed_stats
      if (is.null(stats)) stop("No `stats` supplied and no record found in metadata.")
    }
  } else if (is.list(obj) && all(vapply(obj, is.list, logical(1)))) {
    message("Input is not a 'windowed_data' object — inferring series from folds.")
    windows <- obj
    y_full  <- unlist(lapply(windows, `[[`, "y_win"), use.names = FALSE)
    x_full  <- NULL
    if (is.null(stats)) stop("When passing a raw windows list, `stats` must be supplied.")
  } else {
    stop("Input must be a 'windowed_data' object or a list of window folds.")
  }

  # Stat dispatcher - handles different function types
  call_stat <- function(fun, values) {
    if (is.character(fun) && length(fun) == 1)
      fun <- get(fun, envir = parent.frame())
    if (is.list(fun) && is.function(fun[[1]]))
      return(do.call(fun[[1]], c(list(values), fun[-1])))
    if (is.function(fun)) {
      fn_formals <- names(formals(fun))
      if ("na.rm" %in% fn_formals) return(fun(values, na.rm = na_rm)) # nolint
      if (isTRUE(attr(fun, "na_aware"))) return(fun(values))
      return(fun(values))
    }
    stop("Each stat must be a function, string, or list(fun, ...)")
  }

  # Extract statistics from all folds
  get_fold_vector <- function(stat_name, stat_fun) {
    vec <- vapply(windows, function(w) {
      if (!is.null(w$y_stats[[stat_name]]))
        return(w$y_stats[[stat_name]])
      call_stat(stat_fun, w$y_win)
    }, numeric(1))
    # Update windows with computed stats
    if (inherits(obj, "windowed_data")) {
      for (k in seq_along(windows))
        windows[[k]]$y_stats[[stat_name]] <- vec[[k]]
    }
    vec
  }

  # Compute global values
  globals <- list()
  q_bounds <- c(trim_prob, 1 - trim_prob)

  for (stat_name in names(stats)) {
    stat_fun <- stats[[stat_name]]
    
    if (source == "full") {
      # Use full dataset
      vec <- y_full
      if (trim) {
        vec <- vec[
          vec > quantile(vec, q_bounds[1], na.rm = na_rm) &
            vec < quantile(vec, q_bounds[2], na.rm = na_rm)
        ]
      }
      value <- call_stat(stat_fun, vec)
    } else {
      # Use fold-based approach
      fold_vec <- get_fold_vector(stat_name, stat_fun)
      if (trim) {
        fold_vec <- fold_vec[
          fold_vec > quantile(fold_vec, q_bounds[1], na.rm = na_rm) &
            fold_vec < quantile(fold_vec, q_bounds[2], na.rm = na_rm)
        ]
      }
      value <- if (agg == "mean") mean(fold_vec, na.rm = na_rm)
      else                        median(fold_vec, na.rm = na_rm)
    }
    
    # Validate result
    if (is.recursive(value) && !is.atomic(value)) {
      stop(sprintf(
        "Stat '%s' returned a complex object. Use simple return values.",
        stat_name
      ))
    }
    globals[[stat_name]] <- value
  }

  # Return results
  if (inherits(obj, "windowed_data")) {
    obj$windows         <- windows
    obj$global_y_stats  <- globals
    obj$meta$history$global_y_stats <- list(
      timestamp  = Sys.time(),
      source     = source,
      agg        = agg,
      trim       = trim,
      trim_prob  = trim_prob
    )
    return(obj)
  } else {
    return(globals)
  }
}