#' @title Easy helper for 'table_8_dwh'
#'
#' @name table_8_dwh
#'
#' @description
#' Extract DWH data to be used in table_8
#'
#' @param con The database connection object to be used
#'
#' @import dplyr
#'
#' @importFrom tidyr pivot_wider
#'
#' @export
#'
#' @example table_8_dwh(con)

table_8_dwh <- function(
    con
    ) {

fact <- dplyr::tbl(src = con,
                   from = "PCHC_FACT_DIM") |>
  dplyr::group_by(
    FINANCIAL_YEAR,
    LVL_5_LTST_TYPE,
    BNF_CHAPTER,
    CHAPTER_DESCR,
    BNF_SECTION,
    SECTION_DESCR,
    ICB_CODE,
    ICB_NAME
  ) |>
  dplyr::summarise(
    ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = "LVL_5_LTST_TYPE",
    values_from = "ITEM_PAY_DR_NIC"
  ) |>
  dplyr::arrange(
    FINANCIAL_YEAR,
    BNF_CHAPTER,
    BNF_SECTION,
    ICB_CODE
  ) |>
  dplyr::collect() |>
  dplyr::rename(
    "Financial Year" = 1,
    "BNF Chapter Code" = 2,
    "BNF Chapter Name" = 3,
    "BNF Section Code" = 4,
    "BNF Section Name" = 5,
    "ICB Code" = 6,
    "ICB" = 7,
    "Primary care prescribing dispensed in the community (GBP)" = 8,
    "Dental prescribing dispensed in the community (GBP)" = 9,
    "Hospital prescribing dispensed in the community (GBP)" = 10
  ) |>
  arrange(
    `Financial Year`,
    `ICB Code` == "-"
  )

return(fact)
}
