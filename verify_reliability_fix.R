# Verify the corrected reliability calculation

# Test the corrected reliability calculation with simple example
test_baseline <- 1.0
test_predictions <- c(1.2, 1.0, 0.8, 0.5, 0.0, 1.5)

# Old formula (problematic): baseline / predicted
old_reliability <- test_baseline / test_predictions
cat("Old formula (baseline / predicted):\n")
cat("Predictions:", test_predictions, "\n")
cat("Old reliability:", old_reliability, "\n")
cat("Range:", range(old_reliability), "\n\n")

# New formula (correct): 1 - (predicted / baseline)
new_reliability <- 1 - (test_predictions / test_baseline)
new_reliability <- pmax(new_reliability, 0)  # Bound to [0,1]
cat("New formula (1 - predicted/baseline, bounded):\n")
cat("Predictions:", test_predictions, "\n")
cat("New reliability:", new_reliability, "\n")
cat("Range:", range(new_reliability), "\n\n")

# Interpretation check
cat("INTERPRETATION:\n")
cat("When predicted = baseline (1.0): reliability =", 1 - (1.0/1.0), "\n")
cat("When predicted < baseline (0.8): reliability =", 1 - (0.8/1.0), "\n") 
cat("When predicted > baseline (1.2): reliability =", max(0, 1 - (1.2/1.0)), "\n")
cat("When predicted = 0 (perfect): reliability =", 1 - (0.0/1.0), "\n")
