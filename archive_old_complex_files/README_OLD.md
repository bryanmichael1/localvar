# LOCALVAR - Local Volatility Analysis Package

**RESTRUCTURED FOR STATISTICIANS** - A simplified R package for analyzing local volatility patterns and reliability zones.

## 🎯 What This Package Does

**localvar** helps you understand how reliable your statistical models are across different regions of your data by:

1. **Analyzing volatility patterns** in sliding windows of your data
2. **Finding reliability zones** where your model performs consistently  
3. **Comparing different analysis scales** to find optimal window sizes
4. **Providing clear visualizations** and interpretable results

## 🚀 Quick Start (4 Simple Steps)

```r
# Step 1: Generate or load your data
source("01_generate_data.R")

# Step 2: Run reliability analysis  
source("02_analyze_reliability.R")

# Step 3: View results and plots
source("03_view_results.R")

# Step 4: Compare window sizes (optional)
source("04_compare_window_sizes.R")
```

## 📁 Simplified File Structure

```
📂 localvar/
├── 📄 README_STATISTICIAN.md        # Detailed guide for statisticians
├── 📄 01_generate_data.R            # Create or load your datasets
├── 📄 02_analyze_reliability.R      # Main reliability analysis
├── 📄 03_view_results.R             # Create plots and reports  
├── 📄 04_compare_window_sizes.R     # Find optimal window size
├── 📄 utils_validation.R            # Check if everything works
└── � R/                            # Core functions (no need to edit)
    ├── 📄 core_pipeline.R           # Main analysis pipeline
    ├── 📄 reliability_functions.R   # Reliability calculations
    └── 📄 helper_functions.R        # Utility functions
```

## ⚙️ Key Settings You Can Adjust

### In `02_analyze_reliability.R`:
```r
WINDOW_SIZE <- 30                     # Analysis window size
RELIABILITY_THRESHOLDS <- c(0.90, 0.95)  # Reliability levels to find
OUTPUT_NAME <- "my_analysis"          # Name for saved files
```

### In `04_compare_window_sizes.R`:
```r
WINDOW_SIZES <- c(10, 20, 30, 50)    # Window sizes to compare
```

## 📊 Understanding Your Results

### Key Output Files:
- `{OUTPUT_NAME}_summary.csv` - Simple results table
- `reliability_curve.png` - Visual reliability analysis
- `zone_widths.png` - Comparison of reliability zones
- `reliability_analysis_report.txt` - Written interpretation

### Key Concepts:
- **Reliability**: How consistent your model is (1.0 = perfect, higher = better)
- **Zone Width**: Size of reliable region (smaller = more precise)
- **R²**: How well the volatility model fits (higher = clearer patterns)

## 🎯 Typical Research Workflow

1. **Validate system**: Run `utils_validation.R` 
2. **Prepare data**: Use `01_generate_data.R` or load your own
3. **Single analysis**: Run `02_analyze_reliability.R` 
4. **Review results**: Use `03_view_results.R`
5. **Optimize**: Run `04_compare_window_sizes.R` to find best window size
6. **Finalize**: Re-run steps 2-4 with optimal settings

## 💾 Data Requirements

Your data must be a **list of datasets**, each containing:
- `x`: Predictor variable (numeric)
- `y_norm`: Outcome variable (numeric)

Example:
```r
my_data <- list(
  data.frame(x = rnorm(1000), y_norm = rnorm(1000)),
  data.frame(x = rnorm(1000), y_norm = rnorm(1000)),
  # ... more datasets
)
saveRDS(my_data, "my_data.rds")
```

## 🆘 Troubleshooting

| Problem | Solution |
|---------|----------|
| Package errors | Install with `install.packages(c("data.table", "dplyr", "ggplot2"))` |
| File not found | Run scripts in order (01 → 02 → 03 → 04) |
| Memory errors | Close other applications, reduce `BATCH_SIZE` |
| Poor model fit (R² ≈ 0) | Try different window sizes |
| No reliability zones | Lower thresholds or change window size |

## 📈 Interpreting Results

### Good Analysis:
- ✅ R² > 0.1 (clear patterns)
- ✅ Zone widths < 2.0 (precise)
- ✅ Multiple thresholds achieved

### Needs Improvement:
- ⚠️ R² ≈ 0 (try different window sizes)
- ⚠️ Large zone widths (try smaller windows)
- ⚠️ No zones found (lower thresholds)

## 🔧 Advanced Usage

- **Custom thresholds**: Edit `RELIABILITY_THRESHOLDS` 
- **Memory optimization**: Adjust `BATCH_SIZE`
- **Different models**: Modify functions in `R/` folder
- **Custom plots**: Edit visualization code in step 3

---

**For Detailed Instructions**: See `README_STATISTICIAN.md`  
**For Validation**: Run `utils_validation.R`  
**Version**: 4.0 (Simplified for Statisticians)  
**Author**: Diemithry Kloppenburg
