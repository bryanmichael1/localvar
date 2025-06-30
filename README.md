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

# Step 2: Collect raw data (all window sizes)
source("scripts/02_analyze_reliability.R")

# Step 3: Focus on single window size analysis
source("scripts/03_view_results.R")

# Step 4: Compare all window sizes systematically
source("scripts/04_compare_window_sizes.R")
```

### What Each Script Does:
- **Script 02**: Data collection only - tests multiple window sizes and saves raw results
- **Script 03**: Single window deep-dive - detailed analysis of one specific window size  
- **Script 04**: Multi-window comparison - systematic comparison of all window sizes

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

### Script 02 (Data Collection)
```r
WINDOW_SIZES <- c(20, 30, 40, 50)     # Different window sizes to test
```

### Scripts 03 & 04 (Analysis)
```r
TARGET_WINDOW_SIZE <- 40              # Which window to analyze (Script 03)
RELIABILITY_THRESHOLDS <- c(0.95, 0.90, 0.85, 0.80)  # Zone confidence levels
```

## 📊 What You'll Get

- **Raw data collection** from Script 02 (no analysis, just systematic data gathering)
- **Single window analysis** from Script 03 (scatter plots, zone analysis, detailed reports)
- **Multi-window comparison** from Script 04 (performance comparison across all window sizes)
- **Optimal window recommendations** based on statistical criteria and visualizations

## 📖 Full Documentation

**➡️ See `PROJECT_GUIDE.md` for complete instructions, customization options, and result interpretation.**

## 🔧 Requirements

- R (≥ 4.0)
- Required packages will be installed automatically when you run the scripts
