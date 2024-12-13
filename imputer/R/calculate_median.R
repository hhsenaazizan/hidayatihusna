#' Calculate Median for Each Numeric Column
#'
#' This function calculates the median for each numeric column in a dataset.
#' Non-numeric columns are skipped.
#' @param data A data frame.
#' @return A named vector with medians of numeric columns.
#' @examples
#' sample_data <- data.frame(
#'   A = c(1, 2, NA, 4, 5),
#'   B = c(NA, 2, 3, 4, 5),
#'   C = c("a", "b", "c", "d", "e")
#' )
#' calculate_median(sample_data)
#' @export

calculate_median <- function(data) {
  if(!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  medians <- numeric(0)
  for(col_name in colnames(data)) {
    if(is.numeric(data[[col_name]])) {
      medians("Skipping non-numeric column:", col_name)
    }
  }
  return(medians)
}
cat(paste("Skipping non-numeric column:", col_name, "\n"))
