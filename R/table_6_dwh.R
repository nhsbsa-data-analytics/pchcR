#' @title Easy helper for 'table_6_dwh'
#'
#' @name table_6_dwh
#'
#' @description
#' Extract DWH data to be used in table_6
#'
#' @param con The database connection object to be used
#'
#' @import dplyr
#'
#' @importFrom tidyr pivot_wider
#'
#' @export
#'
#' @example table_6_dwh(con)

table_6_dwh <- function(
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
    SECTION_DESCR
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
    BNF_SECTION
  ) |>
  dplyr::collect() |>
  dplyr::rename(
    "Financial Year" = 1,
    "BNF Chapter Code" = 2,
    "BNF Chapter Name" = 3,
    "BNF Section Code" = 4,
    "BNF Section Name" = 5,
    "Primary care prescribing dispensed in the community (GBP)" = 6,
    "Dental prescribing dispensed in the community (GBP)" = 7,
    "Hospital prescribing dispensed in the community (GBP)" = 8
  )

return(fact)
}
