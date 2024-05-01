#' Standardize column names
#'
#' This function standardizes the column names of a dataframe by converting them to lower camel case.
#'
#' @param data A dataframe.
#' @return A dataframe with standardized column names.
#' @importFrom dplyr rename_with everything
#' @importFrom janitor make_clean_names
#' @importFrom snakecase to_lower_camel_case
#' @examples
#' data <- data.frame("First Name" = c("Ram", "Smith"), "birth_month" = c("June", "March"))
#' standardized_data <- standardizeNames(data)
#' colnames(standardized_data)
#' # Output should be c("firstName", "birthMonth")
#' @export

standardizeNames = function(data){
  columns = colnames(data)
  cleaned_names = janitor::make_clean_names(columns)
  data = data |>
    dplyr::rename_with(snakecase::to_lower_camel_case, everything())
  return(data)
}
