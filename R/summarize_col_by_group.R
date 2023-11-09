#' @import dplyr
#' @title Summarize a column by a group
#' @details This function groups the data by a specified column and calculates statistics for another specified column.
#' @param df The data we will manipulate.
#' @param group_col The column the data should be grouped by.
#' @param summary_col The column statistics should be calculated for.
#' @return A new data frame with the summarized statistics including the average, maximum and minimum of the summary column.
#' @examples
#' # Example 1: data with no NA
#' data_with_no_na <- data.frame(
#'   group = c("a", "b", "b"),
#'   summary = c(1, 2, 3)
#' )
#' summarize_col_by_group(data_with_no_na, group_col = "group", summary_col = "summary")
#'
#' # Example 2: data with NA
#' data_with_na <- data.frame(
#'   group = c("a", "b", "b"),
#'   summary = c(NA, NA, 3)
#' )
#' summarize_col_by_group(data_with_na, group_col = "group", summary_col = "summary")
#' @export
#'
summarize_col_by_group <- function(df, group_col, summary_col) {
  if (!is.numeric(df[[summary_col]])) {
    stop("The summary column must be numeric.")
  }
  result <- df %>%
    group_by(.data[[group_col]]) %>%
    filter(!all(is.na(.data[[summary_col]]))) %>%
    summarise(
      average = mean(.data[[summary_col]], na.rm = TRUE),
      max_value = max(.data[[summary_col]], na.rm = TRUE),
      min_value = min(.data[[summary_col]], na.rm = TRUE),
    )
  return(result)
}
