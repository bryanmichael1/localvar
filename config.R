# ========================================
# LOCALVAR PROJECT CONFIGURATION
# ========================================
# Central configuration file for all analysis settings
# Modify these values to change behavior across all scripts

# ========================================
# DIRECTORY SETTINGS
# ========================================

CONFIG <- list(
  # Data directories
  data_dir = "data",
  results_dir = "results",
  plots_dir = "plots",
  
  # Input/Output files
  raw_data_file = "data/simulated_raw_data_tdist.rds",
  detailed_results_file = "data/multi_window_reliability_detailed.rds",
  summary_results_file = "results/multi_window_reliability_summary.csv",
  
  # ========================================
  # ANALYSIS PARAMETERS
  # ========================================
  
  # Data generation
  n_datasets = 1000,
  n_points_per_dataset = 1000,
  
  # Window sizes to analyze
  window_sizes = c(3, 5, 10, 15, 20, 25, 30, 40, 50, 75, 100),
  
  # Reliability thresholds for zone analysis (adjusted for realistic values)
  # With the corrected mathematical formula (1 - predicted/baseline), 
  # reliability values are typically much lower (0-5%) in volatility modeling
  reliability_thresholds = c(0.02, 0.015, 0.01, 0.008, 0.005, 0.003),
  
  # Default analysis settings
  default_window_size = 40,
  default_method = "quadratic",  # "quadratic" or "loess"
  
  # ========================================
  # PERFORMANCE SETTINGS
  # ========================================
  
  # Memory and parallelization
  batch_size = 20,
  max_parallel_workers = 4,
  
  # Memory limits
  max_datasets_memory_limit = NULL,  # NULL for no limit
  
  # ========================================
  # VISUALIZATION SETTINGS
  # ========================================
  
  # Plot dimensions
  plot_width = 10,
  plot_height = 6,
  plot_dpi = 300,
  
  # Plot themes and colors
  reliability_colors = c("95%" = "#e31a1c", "90%" = "#ff7f00", "85%" = "#1f78b4", "80%" = "#33a02c"),
  
  # ========================================
  # HELPER FUNCTIONS
  # ========================================
  
  # Function to create directories if they don't exist
  ensure_dirs = function() {
    for (dir in c(CONFIG$data_dir, CONFIG$results_dir, CONFIG$plots_dir)) {
      if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
        cat("Created directory:", dir, "\n")
      }
    }
  },
  
  # Function to get file paths
  get_path = function(key) {
    switch(key,
      "raw_data" = CONFIG$raw_data_file,
      "detailed_results" = CONFIG$detailed_results_file,
      "summary_results" = CONFIG$summary_results_file,
      stop("Unknown file key: ", key)
    )
  },
  
  # Function to validate configuration
  validate = function() {
    errors <- c()
    
    # Check window sizes
    if (any(CONFIG$window_sizes < 3)) {
      errors <- c(errors, "Window sizes must be >= 3")
    }
    
    # Check thresholds
    if (any(CONFIG$reliability_thresholds <= 0 | CONFIG$reliability_thresholds >= 1)) {
      errors <- c(errors, "Reliability thresholds must be between 0 and 1")
    }
    
    # Check method
    if (!CONFIG$default_method %in% c("quadratic", "loess")) {
      errors <- c(errors, "Default method must be 'quadratic' or 'loess'")
    }
    
    if (length(errors) > 0) {
      stop("Configuration validation failed:\n", paste(errors, collapse = "\n"))
    }
    
    cat("✓ Configuration validation passed\n")
    return(TRUE)
  }
)

# Validate configuration on load
CONFIG$validate()

# Create directories
CONFIG$ensure_dirs()

cat("✓ Configuration loaded successfully\n")
cat("  - Window sizes:", paste(CONFIG$window_sizes, collapse = ", "), "\n")
cat("  - Reliability thresholds:", paste(CONFIG$reliability_thresholds * 100, collapse = "%, "), "%\n")
cat("  - Default method:", CONFIG$default_method, "\n")
