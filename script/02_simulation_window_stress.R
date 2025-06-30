# ============================
# Window Size Stress Testing
# ============================
# Tests different window sizes to understand computational limits

# Clear environment
rm(list = ls())
gc()

library(simstudy)
library(data.table)
library(dplyr)
devtools::load_all()

cat("=== WINDOW SIZE STRESS TESTING ===\n")

# Test a single dataset with multiple window sizes
RAW_DATA_FILE <- "simulated_raw_data_tdist.rds"

if (!file.exists(RAW_DATA_FILE)) {
  stop("Raw data file not found: ", RAW_DATA_FILE)
}

raw_datasets <- readRDS(RAW_DATA_FILE)
test_dataset <- raw_datasets[[1]]

# Test different window sizes
test_window_sizes <- c(3, 5, 10, 20, 30, 50, 100, 200)

cat("Testing window sizes:", paste(test_window_sizes, collapse = ", "), "\n")

for (ws in test_window_sizes) {
  cat("Testing window size", ws, "...")
  
  start_time <- Sys.time()
  tryCatch({
    result <- run_volatility_pipeline(
      window_size = ws,
      x = test_dataset$x,
      y = test_dataset$y_norm,
      x_type = "continuous", 
      y_type = "continuous"
    )
    
    duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat(" ✅", length(result$windows), "windows,", round(duration, 2), "sec\n")
    
  }, error = function(e) {
    cat(" ❌ Failed:", e$message, "\n")
  })
}

cat("\n✅ Window size stress test complete\n")
rm(list = ls())
gc()
