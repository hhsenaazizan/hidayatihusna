#' Detect Missing Values in a Dataset
#'
#'This function detects and counts the number of missing values for each column in a dataset.
#' @param data A data frame or matrix.
#' @return A named vector with the count of missing values for each column that has missing data.
#' @export
#'
#' @examples
#' sample_data <- data.frame(
#'   A = c(1, 2, NA, 4, 5),
#'   B = c(NA, 2, 3, 4, 5),
#'   C = c(1, 2, 3, 4, 5)
#' )
#' detect_missing_values(sample_data)
#' @export

detect_missing_values <- function(data) {
  missing_counts <- colSums(is.na(data))
  missing_counts <- missing_counts[missing_counts > 0]
  return(missing_counts)
}
