# ========================================
# VALIDATION AND TROUBLESHOOTING
# ========================================
# This script checks if everything is working correctly

# Clear workspace and load required packages
rm(list = ls())
gc()

library(data.table)
library(dplyr)
library(ggplot2)
devtools::load_all()

cat("=== LOCALVAR SYSTEM VALIDATION ===\n\n")

# ========================================
# SYSTEM REQUIREMENTS CHECK
# ========================================

cat("Checking system requirements...\n")
check_system_requirements()

# ========================================
# PACKAGE FUNCTION TESTS
# ========================================

cat("--- Testing Core Functions ---\n")

# Test 1: Data creation
cat("1. Testing data creation...\n")
tryCatch({
  test_data <- create_example_data(n_datasets = 3, n_points = 100)
  validate_data_format(test_data)
  cat("   ✓ Data creation works\n")
}, error = function(e) {
  cat("   ❌ Data creation failed:", e$message, "\n")
})

# Test 2: Pipeline analysis
cat("2. Testing volatility pipeline...\n")
tryCatch({
  if (exists("test_data")) {
    result <- run_volatility_pipeline(
      x = test_data[[1]]$x,
      y = test_data[[1]]$y_norm,
      window_size = 10
    )
    
    if (!is.null(result$windows) && length(result$windows) > 0) {
      cat("   ✓ Pipeline analysis works\n")
    } else {
      cat("   ⚠ Pipeline runs but produces no windows\n")
    }
  } else {
    cat("   ⚠ Skipped (no test data)\n")
  }
}, error = function(e) {
  cat("   ❌ Pipeline analysis failed:", e$message, "\n")
})

# Test 3: Reliability analysis
cat("3. Testing reliability analysis...\n")
tryCatch({
  if (exists("test_data")) {
    reliability_result <- analyze_reliability(
      datasets = test_data,
      window_size = 15,
      reliability_thresholds = c(0.90),
      batch_size = 2
    )
    
    if (!is.null(reliability_result$zones)) {
      cat("   ✓ Reliability analysis works\n")
    } else {
      cat("   ⚠ Reliability analysis runs but produces no zones\n")
    }
  } else {
    cat("   ⚠ Skipped (no test data)\n")
  }
}, error = function(e) {
  cat("   ❌ Reliability analysis failed:", e$message, "\n")
})

# ========================================
# FILE ACCESS TESTS
# ========================================

cat("\n--- Testing File Operations ---\n")

# Test file writing
cat("4. Testing file writing...\n")
tryCatch({
  test_file <- "validation_test.txt"
  cat("Test file created by validation script\n", file = test_file)
  
  if (file.exists(test_file)) {
    file.remove(test_file)
    cat("   ✓ File writing works\n")
  } else {
    cat("   ❌ File writing failed\n")
  }
}, error = function(e) {
  cat("   ❌ File writing error:", e$message, "\n")
})

# Test RDS operations
cat("5. Testing RDS save/load...\n")
tryCatch({
  if (exists("test_data")) {
    test_rds <- "validation_test.rds"
    saveRDS(test_data, test_rds)
    loaded_data <- readRDS(test_rds)
    
    if (length(loaded_data) == length(test_data)) {
      file.remove(test_rds)
      cat("   ✓ RDS operations work\n")
    } else {
      cat("   ❌ RDS data integrity issue\n")
    }
  } else {
    cat("   ⚠ Skipped (no test data)\n")
  }
}, error = function(e) {
  cat("   ❌ RDS operations failed:", e$message, "\n")
})

# ========================================
# PLOTTING TESTS
# ========================================

cat("\n--- Testing Plotting ---\n")

cat("6. Testing ggplot2...\n")
tryCatch({
  # Simple test plot
  test_plot_data <- data.frame(x = 1:10, y = 1:10)
  p <- ggplot(test_plot_data, aes(x = x, y = y)) + geom_point()
  
  # Try to save (this will fail if no graphics device, but that's okay)
  test_plot_file <- "validation_test_plot.png"
  ggsave(test_plot_file, p, width = 4, height = 3, dpi = 100)
  
  if (file.exists(test_plot_file)) {
    file.remove(test_plot_file)
    cat("   ✓ Plotting works\n")
  } else {
    cat("   ⚠ Plot created but not saved (may be normal in some environments)\n")
  }
}, error = function(e) {
  cat("   ⚠ Plotting issue (may be normal without graphics):", e$message, "\n")
})

# ========================================
# MEMORY TEST
# ========================================

cat("\n--- Testing Memory Management ---\n")

cat("7. Testing memory usage...\n")
tryCatch({
  # Check initial memory
  initial_memory <- gc()
  
  # Create larger test dataset
  large_test_data <- create_example_data(n_datasets = 10, n_points = 1000)
  
  # Check memory after creation
  after_creation <- gc()
  
  # Clean up
  rm(large_test_data)
  final_memory <- gc()
  
  cat("   ✓ Memory management working\n")
  cat("   Memory used:", round(after_creation[2,2], 1), "MB\n")
  
}, error = function(e) {
  cat("   ❌ Memory management issue:", e$message, "\n")
})

# ========================================
# DATA FILE CHECKS
# ========================================

cat("\n--- Checking Data Files ---\n")

# Check for common data files
data_files <- c(
  "simulated_raw_data_tdist.rds",
  "example_data.rds",
  "my_reliability_analysis_detailed.rds"
)

for (file in data_files) {
  if (file.exists(file)) {
    cat("✓ Found:", file, "\n")
  } else {
    cat("- Not found:", file, "(this may be normal)\n")
  }
}

# ========================================
# WORKFLOW GUIDANCE
# ========================================

cat("\n--- Workflow Guidance ---\n")

cat("📋 Recommended next steps:\n\n")

if (!file.exists("example_data.rds") && !file.exists("simulated_raw_data_tdist.rds")) {
  cat("1. ▶️  Run '01_generate_data.R' to create example data\n")
  cat("2. ⏸️  Then run '02_analyze_reliability.R'\n")
  cat("3. ⏸️  Then run '03_view_results.R'\n")
  cat("4. ⏸️  Finally run '04_compare_window_sizes.R'\n")
} else if (!file.exists("my_reliability_analysis_detailed.rds")) {
  cat("1. ✓ Data available\n")
  cat("2. ▶️  Run '02_analyze_reliability.R' to analyze\n")
  cat("3. ⏸️  Then run '03_view_results.R'\n")
  cat("4. ⏸️  Finally run '04_compare_window_sizes.R'\n")
} else {
  cat("1. ✓ Data available\n")
  cat("2. ✓ Analysis completed\n")
  cat("3. ▶️  Run '03_view_results.R' to view results\n")
  cat("4. ⏸️  Then run '04_compare_window_sizes.R' if needed\n")
}

# ========================================
# TROUBLESHOOTING GUIDE
# ========================================

cat("\n--- Troubleshooting Guide ---\n")

cat("🔧 Common issues and solutions:\n\n")

cat("Issue: 'Package not found' errors\n")
cat("Solution: Install missing packages with install.packages('package_name')\n\n")

cat("Issue: 'Data file not found' errors\n")
cat("Solution: Make sure to run scripts in order (01, 02, 03, 04)\n\n")

cat("Issue: 'Memory allocation' errors\n")
cat("Solution: Close other applications or reduce BATCH_SIZE in analysis scripts\n\n")

cat("Issue: Very poor model fit (R² ≈ 0)\n")
cat("Solution: Try different window sizes or check if your data has sufficient variability\n\n")

cat("Issue: No reliability zones found\n")
cat("Solution: Lower your reliability thresholds or try different window sizes\n\n")

cat("Issue: Analysis takes too long\n")
cat("Solution: Reduce the number of datasets or increase BATCH_SIZE\n\n")

# ========================================
# SUMMARY
# ========================================

cat("=== VALIDATION COMPLETE ===\n")
cat("If you see mostly ✓ marks above, your system is ready to use localvar.\n")
cat("If you see ❌ marks, address those issues before proceeding.\n")
cat("If you see ⚠ marks, the functionality may work but with limitations.\n\n")

cat("📖 For more help, check README_STATISTICIAN.md\n\n")
