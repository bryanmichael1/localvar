# LOCALVAR - Local Volatility Analysis Package

An R package for analyzing local volatility patterns in data using sliding window approaches and reliability zone estimation.

## 📁 Project Structure

### Core Functions (`R/`)
```
R/
├── 01_assign_window_logic.R      # Window sorting logic assignment
├── 02_make_windows.R             # Sliding window creation
├── 03_extract_path_matrix.R      # Data extraction utilities  
├── 04_combine_window_matrices.R  # Matrix combination functions
├── 05_scale_rows_z.R             # Z-score scaling utilities
├── 06_compute_local_stats.R      # Local statistics computation
├── 07_global_y_stats.R           # Global baseline statistics
├── 08_score_windows.R            # Window scoring functions
├── 09_run_volatility_pipeline.R  # Main volatility analysis pipeline
└── 10_reliability_analysis.R     # Reliability zone analysis (NEW)
```

### Analysis Scripts (`script/`)
```
script/
├── 00_system_validation.R        # System validation and testing
├── 01_simulation_manual.R        # Manual simulation and data generation
├── 02_simulation_window_stress.R # Window size stress testing
├── 03_reliability_analysis.R     # Single reliability analysis
├── 04_visualization_reporting.R  # Visualization and reporting
├── 05_multi_window_analysis.R    # Multi-window size analysis (3-100) ⚡
└── 99_project_verification.R     # Project structure & verification
```

## 🚀 Quick Start

### 1. System Validation (Recommended First Step)
```r
source("script/00_system_validation.R")
```

### 2. Generate Raw Simulation Data (if needed)
```r
source("script/01_simulation_manual.R")
```

### 3. Run Single Window Reliability Analysis
```r
source("script/03_reliability_analysis.R")
```

### 4. Multi-Window Analysis (Apply different window sizes to your data)
```r
source("script/05_multi_window_analysis.R")
```

### 5. Generate Visualizations and Reports
```r
source("script/04_visualization_reporting.R")
```

## 📊 Key Features

### Reliability Analysis
- **Memory-efficient**: Optimized for 16GB Mac M3 systems
- **Batch processing**: Processes results in configurable batches
- **Multiple thresholds**: Calculates reliability zones for multiple confidence levels
- **Auto-detection**: Automatically detects window sizes from pipeline results

### Visualization
- Zone width plots by threshold
- Baseline reliability trends
- Model quality (R²) analysis
- Comprehensive reliability heatmaps

## 🔧 Memory Optimization

The codebase has been optimized for systems with limited RAM:

- **Pre-allocated vectors**: Efficient memory allocation with capacity estimation
- **Batch processing**: Configurable batch sizes (default: 20 results per batch)
- **Memory limits**: 1GB allocation limit for Mac M3 compatibility
- **Garbage collection**: Automatic cleanup between batches
- **Efficient data structures**: Minimal memory footprint with vector pre-allocation

## 📈 Analysis Workflow

1. **Pipeline Results**: Start with pre-computed volatility pipeline results
2. **Data Extraction**: Extract x_z and score_sd values in memory-efficient batches
3. **Model Fitting**: Fit quadratic reliability models per window size
4. **Zone Calculation**: Calculate reliability zones for multiple thresholds
5. **Visualization**: Generate comprehensive plots and reports

## 🗂️ Output Files

### Analysis Results
- `reliability_analysis_detailed.rds` - Full analysis results
- `reliability_analysis_summary.csv` - Summary statistics table
- `reliability_analysis_report.txt` - Comprehensive text report

### Visualizations  
- `plots/zone_width_*pct.png` - Zone width plots by threshold
- `plots/baseline_reliability.png` - Baseline trends
- `plots/model_quality.png` - Model R² analysis
- `plots/reliability_heatmap.png` - Comprehensive heatmap

## ⚙️ Configuration

Key parameters can be adjusted in `03_reliability_analysis.R`:

```r
# Analysis settings
RELIABILITY_THRESHOLDS <- c(0.95, 0.90, 0.85, 0.80, 0.75, 0.70)
BATCH_SIZE <- 20  # Optimized for memory efficiency
OUTPUT_PREFIX <- "reliability_analysis"
```

## 💾 Data Requirements

The analysis expects raw simulation data with structure:
```r
# Raw datasets (list of data.tables)
dataset <- list(
  data.table(id = 1:1000, x = numeric, y_norm = numeric, sim_id = integer),
  # ... 1000 datasets total
)
```

**Current data file**: `simulated_raw_data_tdist.rds` (1000 raw datasets)

## 🔍 Troubleshooting

### Memory Issues
- Reduce `BATCH_SIZE` in analysis scripts
- Close other applications during analysis
- Use `gc()` to force garbage collection

### Data Issues  
- Ensure raw data file exists: `simulated_raw_data_tdist.rds`
- Verify each dataset has 'x' and 'y_norm' columns
- Check for sufficient datasets (1000 datasets recommended)

## 📊 Example Usage

```r
# Load package functions
devtools::load_all()

# System validation (recommended first step)
source("script/00_system_validation.R")

# Run full reliability analysis  
source("script/03_reliability_analysis.R")

# Create visualizations
source("script/04_visualization_reporting.R")

# Results will be saved automatically
```

## 📝 Notes

- Optimized for Mac M3 systems with 16GB RAM
- Uses pre-computed pipeline results (no re-computation needed)
- Generates publication-ready visualizations
- Comprehensive error handling and memory management

---

**Authors**: Diemithry Kloppenburg  
**Last Updated**: December 17, 2024  
**Version**: 3.0 (Final Production Ready)
