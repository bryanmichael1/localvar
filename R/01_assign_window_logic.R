# Determine sorting logic based on data types
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
