# 🎯 Local Volatility Analysis - Clean & Simple

**For Statisticians Who Want Results, Not Complexity**

---

## 📋 What This Project Does

Finds the **optimal window size** for local volatility analysis by:
1. Testing multiple window sizes (3, 5, 10, 15, 20, 25, 30, 40, 50, 75, 100)
2. Measuring how reliable each window size is across different data regions
3. Using **analytical methods** (not simulation) for precise results
4. Identifying which window size gives the most precise estimates

---

## 🚀 Quick Start (4 Simple Steps)

```r
# Step 1: Generate test data (run once)
source("01_generate_data.R")

# Step 2: Analyze all window sizes (10-15 minutes)
source("02_analyze_reliability.R")

# Step 3: View detailed results for window size 40
source("03_view_results.R")

# Step 4: Compare all window sizes
source("04_compare_window_sizes.R")
```

---

## 📁 File Organization

### 🔥 Main Scripts (The Only Files You Need to Touch)
| File | Purpose | What It Does |
|------|---------|--------------|
| `01_generate_data.R` | Data creation | Creates 1000 test datasets |
| `02_analyze_reliability.R` | Main analysis | Tests all window sizes |
| `03_view_results.R` | View details | Shows results for one window size |
| `04_compare_window_sizes.R` | Comparison | Finds optimal window size |

### 🔧 Support Files (Don't Edit These)
| Folder/File | Purpose |
|-------------|---------|
| `R/` | Core statistical functions |
| `archive_old_complex_files/` | Old versions (ignore) |

### 📊 Output Files (Generated Automatically)
| File | Content |
|------|---------|
| `simulated_raw_data_tdist.rds` | Your test data |
| `multi_window_reliability_detailed.rds` | Detailed results |
| `multi_window_reliability_summary.csv` | Summary table (Excel-friendly) |
| `*.png` | Plots and visualizations |

---

## 🎛️ Customization (Easy Changes)

### Change Number of Datasets
In `01_generate_data.R`, line 20:
```r
N_DATASETS <- 1000  # Change this number
```

### Change Window Sizes to Test
In `02_analyze_reliability.R`, line 25:
```r
WINDOW_SIZES <- c(3, 5, 10, 15, 20, 25, 30, 40, 50, 75, 100)
```

### View Different Window Size
In `03_view_results.R`, line 17:
```r
TARGET_WINDOW <- 40  # Change to any window size you analyzed
```

---

## 📈 Understanding Your Results

### Key Output: Zone Width
- **Smaller zone width** = more precise method = better! 🎯
- **Larger zone width** = less precise method

### Key Output: R² (Model Quality)
- **R² > 0.1** = good model, curved reliability pattern
- **R² ≈ 0** = flat model, all regions equally reliable

### Example Interpretation
```
Window Size | Zone Width | R²    | Interpretation
------------|------------|-------|---------------
     20     |   2.341    | 0.156 | Good precision, good model
     40     |   1.892    | 0.203 | Better precision, better model ⭐
     75     |   2.108    | 0.134 | Worse precision
```
**Winner: Window size 40** (smallest zone width)

---

## 🔬 The Science Behind It

### What Is Reliability?
Reliability = How consistent your results are across different data regions
- **High reliability** = same conclusions everywhere
- **Low reliability** = conclusions change depending on which data you look at

### How We Measure It
1. **Fit quadratic model**: `volatility = a + b×x_z + c×x_z²`
2. **Calculate reliability**: `reliability = baseline_volatility / predicted_volatility`
3. **Find zones**: Where does reliability stay above 90%? 95%?
4. **Compare**: Which window size gives the smallest reliable zones?

### Why This Method Is Good
- ✅ **Analytical** (not simulation-based) = exact results
- ✅ **Fast** (no Monte Carlo needed)
- ✅ **Comprehensive** (tests many window sizes automatically)
- ✅ **Practical** (tells you exactly which window size to use)

---

## 🆘 Troubleshooting

### "File not found" errors
1. Make sure you're in the right directory
2. Run scripts in order (01 → 02 → 03 → 04)

### "No results" or "all NA" 
- Your data might be too simple (no volatility patterns)
- Try different reliability thresholds in `02_analyze_reliability.R`

### Script runs too slow
- Reduce `N_DATASETS` in `01_generate_data.R`
- Reduce `WINDOW_SIZES` in `02_analyze_reliability.R`

### Want different window sizes
- Add/remove sizes in `WINDOW_SIZES` in `02_analyze_reliability.R`
- Re-run steps 2-4

---

## 📚 For Your Paper/Thesis

### How to Cite This Method
"We used analytical reliability analysis to determine optimal window sizes for local volatility estimation. Window sizes from 3 to 100 were compared using quadratic modeling of volatility patterns, with reliability measured as the ratio of baseline to predicted volatility."

### Key Results to Report
1. **Optimal window size** (smallest 90% reliability zone)
2. **Zone width** of optimal size
3. **R² value** (model quality)
4. **Number of window sizes tested**

### Example Results Section
"Analysis of 1000 simulated datasets revealed that window size 40 provided optimal reliability (90% zone width = 1.892, R² = 0.203). This window size was 23% more precise than the next-best alternative and showed strong quadratic volatility patterns across the predictor range."

---

## 🎯 You're All Set!

This codebase is now **clean, simple, and statistician-friendly**. Each script has a clear purpose, minimal complexity, and helpful output. Just run the 4 scripts in order and you'll have your optimal window size! 

**Questions?** Check the comments in each script - they explain everything in plain English.

**Happy analyzing!** 📊✨
