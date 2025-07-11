% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reliability_functions.R
\name{calculate_reliability_metrics_optimized}
\alias{calculate_reliability_metrics_optimized}
\title{Calculate reliability metrics with optimized approach}
\usage{
calculate_reliability_metrics_optimized(x_z_vals, score_sd_vals, thresholds)
}
\arguments{
\item{x_z_vals}{Vector of standardized x values}

\item{score_sd_vals}{Vector of volatility scores}

\item{thresholds}{Vector of reliability thresholds to calculate}
}
\value{
List with detailed reliability metrics
}
\description{
This is the main optimized function used by the analysis pipeline
}
