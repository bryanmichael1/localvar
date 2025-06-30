# Score each window by comparing local to global spread
score_windows <- function(
    obj,
    allowed_spread = c("var", "variance", "sd", "mad", "iqr", "range", "entropy", "shannon")
) {
  # Input validation
  if (!inherits(obj, "windowed_data")) stop("Input must be a 'windowed_data' object.")
  if (is.null(obj$windows) || length(obj$windows) == 0)
    stop("No windows found in object.")
  if (is.null(obj$global_y_stats))
    stop("Global statistics not found. Run global_y_stats() before scoring.")
  if (is.null(obj$meta$history$compute_local_stats$stats))
    stop("No record of computed local stats. Run compute_local_stats() before scoring.")

  global_vals    <- obj$global_y_stats
  windows        <- obj$windows
  computed_stats <- obj$meta$history$compute_local_stats$stats

  # Determine valid metrics (spread-like + previously computed)
  valid_stats <- intersect(intersect(names(global_vals), allowed_spread), computed_stats)
  blocked     <- setdiff(names(global_vals), allowed_spread)
  skipped     <- setdiff(intersect(names(global_vals), computed_stats), valid_stats)

  # Warnings for blocked/skipped metrics
  if (length(blocked) > 0) {
    warning(sprintf(
      "Blocked location-type metrics skipped: %s",
      paste(blocked, collapse = ", ")
    ))
  }
  if (length(skipped) > 0) {
    warning(sprintf(
      "The following metrics were computed but are not in allowed spread metrics: %s",
      paste(skipped, collapse = ", ")
    ))
  }
  if (length(valid_stats) == 0)
    stop("No valid spread-type metrics found to score. Only location-type stats were present.")

  # Vectorized scoring
  for (stat_name in valid_stats) {
    # Extract all local values for this stat
    local_vals <- vapply(windows, function(w) w$y_stats[[stat_name]], numeric(1))
    global_val <- global_vals[[stat_name]]
    
    # Validate global value
    if (is.null(global_val) || is.na(global_val) || !is.finite(global_val) || global_val < 0) {
      stop(sprintf(
        "Missing or invalid global '%s' value. Run global_y_stats() first.",
        stat_name
      ))
    }
    
    # Clamp for division and compute scores
    global_val <- max(global_val, .Machine$double.eps)
    ratio <- local_vals / global_val
    score_vec <- 2 * tanh(log(ratio))
    
    # Assign scores back to windows
    for (i in seq_along(windows)) {
      if (is.null(windows[[i]]$score)) windows[[i]]$score <- list()
      windows[[i]]$score[[stat_name]] <- score_vec[[i]]
    }
  }

  # Update object and return
  obj$windows <- windows
  obj$meta$history$score_windows <- list(
    timestamp       = Sys.time(),
    scored_metrics  = valid_stats,
    skipped_metrics = c(blocked, skipped)
  )

  return(obj)
}