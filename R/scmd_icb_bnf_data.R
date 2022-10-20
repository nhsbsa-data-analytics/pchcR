#' Manipulate SCMD data
#'
#' @param file
#'
#' @return a data frame
#' @export
#'
#' @import dplyr
#'
#' @importFrom openxlsx getSheetNames read.xlsx
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_extract str_extract_all str_length
#'
#' @examples
#' scmd_data(file = "Y:/Official Stats/PCHC/data/ICB_BNF_2022.xlsx")
scmd_icb_bnf_data <- function(
  file
) {
  sheetNames <- openxlsx::getSheetNames(file)

  icb_bnf_data <- data.frame()

  for(i in sheetNames) {

  df <- readxl::read_excel(
    file,
    sheet = i
  ) |>
    dplyr::mutate(
      FINANCIAL_YEAR = gsub(" ", "/", i)
    ) |>
    dplyr::relocate(FINANCIAL_YEAR) |>
    #add .1 to incotinence applicances as needed for future manipulations to BND chapter and section
    mutate(
      `Cost (£)` = case_when(
        `Cost (£)` == "22 - Incontinence Appliances" ~ "22.1 - Incontinence Appliances",
        TRUE ~ `Cost (£)`
      )
    )

  #rename ICB columns to remove number in brackets
  names(df)[3:44] <- substr(names(df)[3:44],1,nchar(names(df)[3:44])-4)

  #build data
  df2 <- df |>
    dplyr::rowwise() |>
    # create unknown ICB column which is sum of unallocated costs
    dplyr::mutate(`Unknown ICB` = sum(across(contains("...")), na.rm = T))|>
    dplyr::rename(
      BNF = 2
    ) |>
    tidyr::pivot_longer(cols = -c(FINANCIAL_YEAR, BNF),
                        names_to = "STP",
                        values_to = "COST") |>
    dplyr::filter(!grepl("\\...", STP)) |>
    dplyr::mutate(
      BNF_CHAPTER = stringr::str_extract(BNF, "[^\\.]+"),
      BNF_SECTION = as.character(stringr::str_extract_all(BNF,"(?<=\\.).+(?= -)")),
      BNF_CHAPTER = case_when(
        stringr::str_length(BNF_CHAPTER) == 1 ~ paste0("0",BNF_CHAPTER),
        TRUE ~ BNF_CHAPTER
      ),
      BNF_SECTION = case_when(
        stringr::str_length(BNF_SECTION) == 1 ~ paste0("0",BNF_SECTION),
        TRUE ~ BNF_SECTION
      ),
      BNF_SECTION = paste0(BNF_CHAPTER, BNF_SECTION)
    ) |>
    dplyr::mutate(
      COST = case_when(
        is.na(COST) ~ 0,
        TRUE ~ COST
      )
    )
  icb_bnf_data <- icb_bnf_data %>%
    dplyr::bind_rows(df2)
  }

  return(icb_bnf_data)
}
