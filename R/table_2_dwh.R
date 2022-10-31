#' @title Easy helper for 'table_2_dwh'
#'
#' @name table_2_dwh
#'
#' @description
#' Extract DWH data to be used in table_2
#'
#' @param con The database connection object to be used
#'
#' @import dplyr
#'
#' @importFrom tidyr pivot_wider
#'
#' @export
#'
#' @example
#' table_2_dwh(con)

table_2_dwh <- function(
    con
    ) {

fact <- dplyr::tbl(src = con,
                   from = "PCHC_FACT_DIM") |>
  dplyr::group_by(
    FINANCIAL_YEAR,
    LVL_5_LTST_TYPE
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
    FINANCIAL_YEAR
  ) |>
  dplyr::collect() |>
  dplyr::rename(
    "Financial Year" = 1,
    "Primary care prescribing dispensed in the community (GBP)" = 2,
    "Dental prescribing dispensed in the community (GBP)" = 3,
    "Hospital prescribing dispensed in the community (GBP)" = 4
  )

return(fact)
}
