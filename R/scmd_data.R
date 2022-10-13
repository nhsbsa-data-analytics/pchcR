#' Manipulate SCMD data
#'
#' @param file
#'
#' @return a data frame
#' @export
#' @importFrom openxlsx getSheetNames
#' @importFrom openxlsx read.xlsx
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#'
#' @examples
#' scmd_data(file = "path")
scmd_data <- function(
  file
) {
  sheets <- openxlsx::getSheetNames(file)

  df <- data.frame()

  for (i in sheets) {
    tmp <- openxlsx::read.xlsx(file, sheet = i)

    tmp |> dplyr::mutate(financial_year = i)

    df <- dplyr::bind_rows(df, tmp)
  }
}
