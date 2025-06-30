# LOCALVAR - Local Volatility Analysis

**Clean & Simple R Package for Statisticians** 📊

## 🎯 What This Package Does

**localvar** helps you understand how reliable your statistical models are across different regions of your data by:

1. **Analyzing volatility patterns** in sliding windows of your data
2. **Finding reliability zones** where your model performs consistently  
3. **Comparing different analysis scales** to find optimal window sizes
4. **Providing clear visualizations** and interpretable results

## 🚀 Quick Start (4 Simple Steps)

```r
# Step 1: Generate test data
source("scripts/01_generate_data.R")

# Step 2: Run analysis (all window sizes)  
source("scripts/02_analyze_reliability.R")

# Step 3: View results and plots
source("scripts/03_view_results.R")

# Step 4: Compare window sizes to find optimal
source("scripts/04_compare_window_sizes.R")
```

## 📁 Project Organization

```
📂 localvar/
├── 📄 PROJECT_GUIDE.md             # 👈 DETAILED INSTRUCTIONS HERE
├── � README.md                    # This quick start guide
├── �📂 scripts/                     # Main analysis scripts (run these!)
│   ├── 01_generate_data.R          # Create or load datasets
│   ├── 02_analyze_reliability.R    # Main reliability analysis
│   ├── 03_view_results.R           # Create plots and reports
│   └── 04_compare_window_sizes.R   # Find optimal window size
├── 📂 data/                        # Input data (.rds files)
├── 📂 results/                     # Output files & plots (.png, .csv, .txt)
├── 📂 R/                           # Core functions (no need to edit)
└── 📂 archive_old_complex_files/   # Legacy files and old scripts
```

## 💾 Data Files Note

**Important**: Large result files (`data/*_detailed.rds`) are excluded from Git tracking to keep the repository lightweight. These files are automatically generated when you run the analysis scripts. If you're setting up this project for the first time:

1. Run `scripts/01_generate_data.R` to create initial datasets
2. Run `scripts/02_analyze_reliability.R` to generate analysis results
3. The detailed result files will be created automatically

## ⚙️ Key Settings You Can Adjust

The main analysis script `scripts/02_analyze_reliability.R` contains key parameters:

```r
WINDOW_SIZES <- c(20, 30, 40, 50)     # Different window sizes to test
MIN_STABILITY_ZONE <- 10              # Minimum reliable zone width
CONFIDENCE_LEVELS <- c(0.8, 0.9, 0.95) # Reliability thresholds
```

## 📊 What You'll Get

- **CSV summaries** of reliability statistics by window size
- **Visual plots** showing reliability zones and model performance
- **Text reports** with interpretable results and recommendations
- **Comparison charts** to help choose optimal analysis parameters

## 📖 Full Documentation

**➡️ See `PROJECT_GUIDE.md` for complete instructions, customization options, and result interpretation.**

## 🔧 Requirements

- R (≥ 4.0)
- Required packages will be installed automatically when you run the scripts
