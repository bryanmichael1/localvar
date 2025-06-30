combine_window_matrices <- function(mat1, mat2, name1 = "x_z", name2 = "sd") {
  # Ensure same dimensions
  stopifnot(dim(mat1) == dim(mat2))

  n_sims <- nrow(mat1)
  n_windows <- ncol(mat1)

  df <- data.frame(
    simulation = rep(rownames(mat1), each = n_windows),
    window = rep(colnames(mat1), times = n_sims),
    value1 = as.vector(mat1),
    value2 = as.vector(mat2)
  )

  names(df)[3:4] <- c(name1, name2)
  return(df)
}
