# ========================================
# STEP 1: GENERATE SIMULATION DATA
# ========================================
# Creates datasets for testing local volatility analysis
# Simple script for statisticians - just run it!

# Clear workspace and load packages
rm(list = ls())
gc()

library(simstudy)
library(data.table) 
library(dplyr)
devtools::load_all()

cat("=== GENERATING SIMULATION DATA ===\n")

# ========================================
# SETTINGS (You can change these)
# ========================================

N_DATASETS <- 1000        # Number of datasets to create
N_POINTS <- 1000          # Data points per dataset  
OUTPUT_FILE <- "data/simulated_raw_data_tdist.rds"

cat("Settings:\n")
cat("- Number of datasets:", N_DATASETS, "\n")
cat("- Points per dataset:", N_POINTS, "\n")
cat("- Output file:", OUTPUT_FILE, "\n\n")

# ========================================
# GENERATE DATA
# ========================================

cat("Generating", N_DATASETS, "datasets... (this may take a few minutes)\n")
start_time <- Sys.time()

# Generate datasets with t-distribution (more realistic than normal)
datasets <- list()

for (i in 1:N_DATASETS) {
  if (i %% 100 == 0) {
    cat("  Generated", i, "/", N_DATASETS, "datasets\n")
  }
  
  # Create one dataset
  dataset <- data.frame(
    sim_id = i,
    x = rt(N_POINTS, df = 3),  # t-distribution with 3 degrees of freedom
    y_norm = rt(N_POINTS, df = 3)  # independent t-distributed y values
  )
  
  datasets[[i]] <- dataset
}

total_time <- round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 2)

# ========================================
# SAVE DATA
# ========================================

saveRDS(datasets, OUTPUT_FILE)

cat("\n✅ SUCCESS!\n")
cat("Generated", N_DATASETS, "datasets in", total_time, "minutes\n")
cat("Saved to:", OUTPUT_FILE, "\n")
cat("Total data points:", N_DATASETS * N_POINTS, "\n")

# ========================================
# QUICK PREVIEW
# ========================================

cat("\n📊 QUICK PREVIEW:\n")
first_dataset <- datasets[[1]]
cat("First dataset structure:\n")
cat("- Rows:", nrow(first_dataset), "\n") 
cat("- Columns:", paste(names(first_dataset), collapse = ", "), "\n")
cat("- x range:", round(min(first_dataset$x), 2), "to", round(max(first_dataset$x), 2), "\n")
cat("- y_norm range:", round(min(first_dataset$y_norm), 2), "to", round(max(first_dataset$y_norm), 2), "\n")

cat("\n🎯 NEXT STEP: Run '02_analyze_reliability.R'\n")

# Show data summary
cat("\nData Summary:\n")
summary_stats <- summarize_datasets(datasets)
print(summary_stats)

# Show sample of first dataset
cat("\nSample from first dataset:\n")
print(head(datasets[[1]]))

cat("\n=== DATA PREPARATION COMPLETE ===\n")
cat("Your data is ready for analysis!\n")
cat("Next step: Run '02_analyze_reliability.R'\n\n")

# ========================================
# INSTRUCTIONS FOR YOUR OWN DATA
# ========================================

cat("📋 To use your own data:\n")
cat("1. Your data should be a list of datasets\n")
cat("2. Each dataset must have columns 'x' (predictor) and 'y_norm' (outcome)\n")
cat("3. Both columns must be numeric\n")
cat("4. Example format:\n")
cat("   my_data <- list(\n")
cat("     data.frame(x = rnorm(1000), y_norm = rnorm(1000)),\n")
cat("     data.frame(x = rnorm(1000), y_norm = rnorm(1000)),\n")
cat("     # ... more datasets\n")
cat("   )\n")
cat("5. Save with: saveRDS(my_data, 'my_data.rds')\n")
cat("6. Update DATA_FILE variable above to point to your file\n\n")
