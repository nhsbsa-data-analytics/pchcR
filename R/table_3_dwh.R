#' @title Easy helper for 'table_3_dwh'
#'
#' @name table_3_dwh
#'
#' @description
#' Extract DWH data to be used in table_3
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
#' table_3_dwh(con)

table_3_dwh <- function(
    con
    ) {

fact <- dplyr::tbl(src = con,
                   from = "PCHC_FACT_DIM") |>
  dplyr::group_by(
    YEAR_MONTH,
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
    YEAR_MONTH
  ) |>
  dplyr::collect() |>
  dplyr::rename(
    "Year Month" = 1,
    "Primary care prescribing dispensed in the community (GBP)" = 2,
    "Dental prescribing dispensed in the community (GBP)" = 3,
    "Hospital prescribing dispensed in the community (GBP)" = 4
  )

return(fact)
}
