# Run the full volatility scoring pipeline on paired x/y data
run_volatility_pipeline <- function(
    x, y,
    window_size      = 30,
    step_size        = 1,
    x_type           = NULL,
    y_type           = NULL,
    x_k              = NULL,
    y_k              = NULL,
    sort             = TRUE,
    local_stats      = list(iqr = function(x) IQR(x, 
                                                  na.rm = FALSE, 
                                                  type = 7)),
    na_rm            = FALSE,
    global_source    = c("fold", "full"),
    global_agg       = c("median", "mean"),
    global_trim      = FALSE,
    global_trim_prob = 0.05,
    force_global     = FALSE,
    allowed_spread   = c("var", "variance", "sd", "mad", "iqr", "range", "entropy", "shannon")
) {
  # Optionally use assign_window_logic to determine sorting
  if (!is.null(x_type) && !is.null(y_type)) {
    sort_logic <- assign_window_logic(x_type, y_type, x_k, y_k)$sort
  } else {
    sort_logic <- sort
  }

  # 1. Make windows
  obj <- make_windows(
    x, y,
    window_size = window_size,
    step_size   = step_size,
    sort        = sort_logic
  )

  # 2. Compute local window stats
  obj <- compute_local_stats(
    obj,
    stats = local_stats,
    na_rm = na_rm
  )

  # 3. Compute global baseline
  obj <- global_y_stats(
    obj,
    stats      = local_stats,
    source     = match.arg(global_source),
    agg        = match.arg(global_agg),
    trim       = global_trim,
    trim_prob  = global_trim_prob,
    na_rm      = na_rm,
    force      = force_global
  )

  # 4. Score windows against global baseline
  obj <- score_windows(
    obj,
    allowed_spread = allowed_spread
  )

  # Return enriched windowed_data object
  obj
}