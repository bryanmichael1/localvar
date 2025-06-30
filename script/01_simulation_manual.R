# ============================
# Libraries & Setup
# ============================
library(simstudy)
library(data.table)
library(dplyr)
library(ggplot2)
devtools::load_all()

# ============================
# Simulation Setup
# ============================

def <- defData(varname = "x_placeholder", dist = "normal", formula = 0, variance = 1)

# Generate 1000 datasets
set.seed(123)
sim_list <- lapply(1:1000, function(i) {
  dt <- genData(1000, def)

  # Replace placeholder with t-distributed x (df = 2.5)
  dt[, x := rt(.N, df = 2.5)]

  # Winsorize to keep values within ±4
  repeat {
    extreme_indices <- which(abs(dt$x) > 4)
    if (length(extreme_indices) == 0) break
    dt[extreme_indices, x := rt(length(extreme_indices), df = 3)]
  }

  # Generate outcome: sinusoidal signal + t-distributed noise
  dt[, y_norm := sin(x) + rt(.N, df = 3) * 0.3]
  dt[, sim_id := i]
  dt[, x_placeholder := NULL]
  return(dt)
})

# Run volatility analysis pipeline
results_list <- lapply(sim_list, function(dt) {
  run_volatility_pipeline(
    window_size = 30,
    x      = dt$x,
    y      = dt$y_norm,
    x_type = "continuous",
    y_type = "continuous"
  )
})

# Save the RAW DATA (sim_list) - not the results!
saveRDS(sim_list, file = "simulated_raw_data_tdist.rds")

# For the analysis below, we still need the results
# So keep results_list in memory (but don't save it permanently)
# results_list is already computed above

# ============================
# Extract and Combine Results
# ============================

x_z_matrix <- extract_window_matrix(results_list, "x_stats/mean_z")
cat("Range of x_z_matrix with winsorization:", range(x_z_matrix, na.rm = TRUE), "\n")

sd_matrix <- extract_window_matrix(results_list, "score/sd")

df_combined <- combine_window_matrices(x_z_matrix, sd_matrix, "x_z", "score_sd")

# ============================
# Summary by Binned x_z
# ============================

df_summary <- df_combined %>%
  mutate(x_bin = round(x_z / 0.025) * 0.025) %>%
  group_by(x_bin) %>%
  summarise(
    n = n(),
    mean_score = mean(score_sd, na.rm = TRUE),
    sd_score   = sd(score_sd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n >= 30)

# ============================
# Visualization
# ============================

# Mean + SD band plot
ggplot(df_summary, aes(x = x_bin, y = mean_score)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_score - sd_score, ymax = mean_score + sd_score), alpha = 0.2) +
  theme_minimal() +
  labs(x = "x_z (binned)", y = "Mean score_sd ± SD")

# Spread plot
ggplot(df_summary, aes(x = x_bin, y = sd_score)) +
  geom_line() +
  theme_minimal() +
  labs(x = "x_z (binned)", y = "Standard Deviation of score_sd", title = "Spread of score_sd across standardized x")

# Fold count plot
df_summary %>%
  ggplot(aes(x = x_bin, y = n)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Number of folds per x_z bin", y = "Count")

# ============================
# Model Fitting
# ============================

lm1 <- lm(score_sd ~ x_z, data = df_combined)
summary(lm1)

lm2 <- lm(abs(score_sd) ~ x_z + I(x_z^2), data = df_combined)
summary(lm2)

# ============================
# Reliability Band Estimation
# ============================

# Coefficients from quadratic model
coefs <- coef(lm2)
b0 <- coefs["(Intercept)"]
b1 <- coefs["x_z"]
b2 <- coefs["I(x_z^2)"]

# Baseline from central region
baseline <- df_combined %>%
  filter(abs(x_z) < 0.5) %>%
  summarise(mean_abs_score = mean(abs(score_sd), na.rm = TRUE)) %>%
  pull(mean_abs_score)

# Generate prediction grid
x_seq <- seq(min(df_combined$x_z, na.rm = TRUE), max(df_combined$x_z, na.rm = TRUE), length.out = 1000)
pred_df <- data.frame(
  x_z = x_seq,
  pred_abs_score = b0 + b1 * x_seq + b2 * x_seq^2
)

# Relative reliability calculation
pred_df$relative_reliability <- baseline / pred_df$pred_abs_score

# Reliability threshold bands
thresholds <- c(0.95, 0.90, 0.80, 0.70)
cross_points <- lapply(thresholds, function(thresh) {
  df_thresh <- pred_df %>% filter(relative_reliability >= thresh)
  if (nrow(df_thresh) > 0) {
    return(range(df_thresh$x_z))
  } else {
    return(c(NA, NA))
  }
})
names(cross_points) <- paste0("Above_", thresholds * 100, "pct")

# Output threshold ranges
cat("\nReliability zones based on relative reliability:\n")
for (name in names(cross_points)) {
  vals <- cross_points[[name]]
  cat(sprintf("%s reliability zone: x_z ≈ %.3f to %.3f\n", name, vals[1], vals[2]))
}

# Output minimum reliability
cat(sprintf("\nMinimum relative reliability: %.3f\n", min(pred_df$relative_reliability)))

# Relative reliability curve
ggplot(pred_df, aes(x = x_z, y = relative_reliability)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = c(0.95, 0.90, 0.80, 0.70), linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(x = "x_z", y = "Relative Reliability (center / predicted)", title = "Relative Reliability Across x_z")
