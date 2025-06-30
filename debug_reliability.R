# Debug reliability calculation issues
# Check what's causing 3900 invalid data points

library(dplyr)

# Load the detailed results to inspect
detailed_results <- readRDS("data/multi_window_reliability_detailed.rds")

cat("=== DETAILED RESULTS STRUCTURE ===\n")
cat("Available keys:", paste(names(detailed_results), collapse = ", "), "\n")

# Check one window size to understand the issue
window_data <- detailed_results$window_100  # Use largest window size

cat("\n=== WINDOW 100 DATA STRUCTURE ===\n")
cat("Available fields:", paste(names(window_data), collapse = ", "), "\n")

# Check if reliability_curve exists and what it contains
if ("reliability_curve" %in% names(window_data)) {
  reliability_curve <- window_data$reliability_curve
  cat("\nReliability curve exists with", nrow(reliability_curve), "rows\n")
  cat("Columns:", paste(names(reliability_curve), collapse = ", "), "\n")
  
  if (nrow(reliability_curve) > 0) {
    # Check for problematic values
    na_values <- sum(is.na(reliability_curve$reliability))
    inf_values <- sum(is.infinite(reliability_curve$reliability))
    out_of_range <- sum(reliability_curve$reliability < 0 | reliability_curve$reliability > 1, na.rm = TRUE)
    very_large <- sum(reliability_curve$reliability > 5, na.rm = TRUE)

    cat("NA values:", na_values, "\n")
    cat("Infinite values:", inf_values, "\n") 
    cat("Out of range [0,1]:", out_of_range, "\n")
    cat("Very large (>5):", very_large, "\n")

    # Show distribution of reliability values
    cat("\nReliability value distribution:\n")
    print(summary(reliability_curve$reliability))
  }
} else {
  cat("\nNo reliability_curve field found!\n")
}

# Check what fields contain the actual curve data
cat("\n=== ALL WINDOW SIZES SUMMARY ===\n")
total_invalid <- 0
total_points <- 0

for (window_name in names(detailed_results)) {
  if (grepl("^window_", window_name)) {
    window_data <- detailed_results[[window_name]]
    cat("\n", window_name, ":\n")
    cat("  Fields:", paste(names(window_data), collapse = ", "), "\n")
    
    if ("reliability_curve" %in% names(window_data)) {
      curve_data <- window_data$reliability_curve
      if (!is.null(curve_data) && nrow(curve_data) > 0) {
        invalid_count <- sum(is.na(curve_data$reliability) | 
                            is.infinite(curve_data$reliability) |
                            curve_data$reliability < 0 | 
                            curve_data$reliability > 1)
        
        total_invalid <- total_invalid + invalid_count
        total_points <- total_points + nrow(curve_data)
        
        cat("  Reliability curve:", invalid_count, "/", nrow(curve_data), "invalid\n")
      } else {
        cat("  Reliability curve: EMPTY or NULL\n")
      }
    } else {
      cat("  No reliability_curve field\n")
    }
  }
}

if (total_points > 0) {
  cat("\nTOTAL across all windows:", total_invalid, "/", total_points, "invalid\n")
  cat("Percentage invalid:", round(100 * total_invalid / total_points, 1), "%\n")
} else {
  cat("\nNo reliability curve data found in any window!\n")
}
