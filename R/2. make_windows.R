# Create sliding windows on paired x/y data
make_windows <- function(
    x, y,
    window_size = 30,
    step_size   = 1,
    sort        = TRUE
) {
  # Basic input validation
  stopifnot(length(x) == length(y))
  stopifnot(step_size >= 1, window_size >= 1)
  
  # Determine ordering
  index_vec <- seq_along(x)
  
  # Helper to recognise the c("x","y") case even if attributes differ
  is_xy_sort <- function(s) {
    is.character(s) && length(s) == 2 &&
      identical(unname(s), c("x", "y"))
  }
  
  if (isFALSE(sort)) {
    ord <- index_vec                                       # keep original order
  } else if (isTRUE(sort)) {                               # legacy heuristic
    if (length(unique(x)) <= 2) {
      ord <- order(y)                                      # binary x → sort on y
    } else {
      ord <- order(x)                                      # otherwise sort on x
    }
  } else if (identical(sort, "x")) {
    ord <- order(x)                                        # explicit x-only sort
  } else if (is_xy_sort(sort)) {
    ord <- order(x, y)                                     # x primary, y within x
  } else {
    stop(
      "'sort' must be TRUE, FALSE, \"x\", or c(\"x\", \"y\").\n",
      "Received: ", deparse(sort)
    )
  }
  
  # Re-order vectors (no-op if ord is the identity)
  x <- x[ord]; y <- y[ord]; index_vec <- index_vec[ord]
  
  # Window construction
  n       <- length(y)
  windows <- vector("list", 0L)
  fold_id <- 1
  
  # Helper factory for each window record
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
        mean = mean(x[slice_idx]),
        sd   = stats::sd(x[slice_idx])
      ),
      y_stats = list(
        mean = mean(y[slice_idx]),
        sd   = stats::sd(y[slice_idx])
      ),
      x_win = x[slice_idx],
      y_win = y[slice_idx]
    )
  }
  
  # Growing windows (2 to window_size-1)
  for (w in 2:(window_size - 1)) {
    windows[[fold_id]] <- build_win(1:w)
    fold_id <- fold_id + 1
  }
  
  # Full-size sliding windows
  for (start in seq(1, n - window_size + 1, by = step_size)) {
    windows[[fold_id]] <- build_win(start:(start + window_size - 1))
    fold_id <- fold_id + 1
  }
  
  # Shrinking windows (window_size-1 to 2)
  for (w in rev(2:(window_size - 1))) {
    windows[[fold_id]] <- build_win((n - w + 1):n)
    fold_id <- fold_id + 1
  }
  
  # Return windowed_data object
  structure(
    list(
      data    = list(x = x, y = y, idx = index_vec),
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


