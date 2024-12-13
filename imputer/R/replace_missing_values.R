#' Replace Missing Values with Median
#'
#' This function replaces missing values in numeric columns with the column median.
#' Non-numeric columns are skipped.
#' @param data A data frame.
#' @return A data frame with missing values replaced by medians.
#' @examples
#' sample_data <- data.frame(
#'   A = c(1, 2, NA, 4, 5),
#'   B = c(NA, 2, 3, 4, 5),
#'   C = c("a", "b", "c", "d", "e")
#' )
#' replace_with_median(sample_data)
#' @export

replace_with_median <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }

  for (col_name in colnames(data)) {
    if (is.numeric(data[[col_name]])) {
      col_median <- median(data[[col_name]], na.rm = TRUE)
      data[[col_name]][is.na(data[[col_name]])] <- col_median
    } else {
      message("Skipping non-numeric column: ", col_name)
    }
  }

  return(data)
}
