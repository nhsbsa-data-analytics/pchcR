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
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' scmd_data(file = "path")
scmd_data <- function(
  file
) {
  sheetNames <- openxlsx::getSheetNames("Y:\\Official Stats\\PCHC\\data\\STP_BNF_2021.xlsx")

  icb_bnf_data <- data.frame()

  for(i in sheetNames) {

    df <- readxl::read_excel(
      "Y:\\Official Stats\\PCHC\\data\\STP_BNF_2021.xlsx",
      sheet = i
    ) %>%
      mutate(
        FINANCIAL_YEAR = gsub(" ", "/", i)
      ) %>%
      relocate(FINANCIAL_YEAR) %>%
      rowwise() %>%
      mutate(`Unknown ICB` = sum(across(contains("...")), na.rm = T)) %>%
      rename(
        BNF_SECTION = 2
      ) %>%
      tidyr::pivot_longer(cols = -c(FINANCIAL_YEAR, BNF_SECTION),
                          names_to = "STP",
                          values_to = "COST") %>%
      dplyr::filter(!grepl("\\...", STP))

    icb_bnf_data <- icb_bnf_data %>%
      bind_rows(
        df
      )
    }
  return(icb_bnf_data)
}
