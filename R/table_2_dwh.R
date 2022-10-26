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
#' @importFrom stats na.omit
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
    dplyr::mutate(
      prev_year_GP_PRACTICE = lag(`GP PRACTICE / COST CENTRE`,1 ),
      prev_year_DENTIST_PRACTICE = lag(`DENTIST PRACTICE`,1 ),
      prev_year_HOSPITAL = lag(`HOSPITAL`,1 )
    ) |>
    dplyr::mutate(
      `Primary care prescribing dispensed in the community (%)` =
        (`GP PRACTICE / COST CENTRE` - prev_year_GP_PRACTICE) / prev_year_GP_PRACTICE * 100,
      `Dental prescribing dispensed in the community (%)` =
        (`DENTIST PRACTICE` - prev_year_DENTIST_PRACTICE) / prev_year_DENTIST_PRACTICE * 100,
      `Hospital prescribing dispensed in the community (%)` =
        (`HOSPITAL` - prev_year_HOSPITAL) / prev_year_HOSPITAL * 100
    ) |>
    stats::na.omit() |>
    dplyr::select(1,8,9,10)


return(fact)
}
