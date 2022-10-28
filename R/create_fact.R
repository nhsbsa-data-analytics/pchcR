#' @title Easy helper for 'create_fact'
#'
#' @name create_fact
#'
#' @description
#' Build 'PCHC_FACT_DIM' in personal schema
#'
#' @param con The database connection object to be used
#' @param from The first month you wish to collect data from (defaults to 201504)
#' @param to The last month you wish to collect data from (defaults to 202203)
#'
#' @import dplyr
#'
#' @importFrom dbplyr in_schema
#' @importFrom DBI dbExistsTable dbRemoveTable
#'
#' @export
#'
#' @example
#' create_fact(con)

create_fact <- function(
    con,
    from = 201604L,
    to = 202203L) {

  #build time dimension ---------
  tdim <- dplyr::tbl(con,
                     from = dbplyr::in_schema("DIM", "YEAR_MONTH_DIM")) |>
    dplyr::select(FINANCIAL_YEAR, YEAR_MONTH) |>
    dplyr::filter(
      YEAR_MONTH >= 201604L,
      YEAR_MONTH <= 202203L
    )

  # build org dimension -------
  porg <- dplyr::tbl(con,
                     from = dbplyr::in_schema("DIM", "CUR_EP_LEVEL_5_FLAT_DIM")) |>
    dplyr::filter(CUR_CTRY_OU == 1) |>
    dplyr::mutate(
      ICB_NAME = dplyr::case_when(
        CUR_AREA_LTST_CLSD == "Y" ~ "UNKNOWN ICB",
        CUR_AREA_TEAM_LTST_NM %in% c(
          'ENGLISH/WELSH DUMMY DENTAL',
          'UNIDENTIFIED DEPUTISING SERVICES',
          'UNIDENTIFIED DOCTORS'
        ) ~ "UNKNOWN ICB",
        TRUE ~ CUR_FRMTTD_AREA_TEAM_LTST_NM
      ),
      ICB_CODE = dplyr::case_when(
        CUR_AREA_LTST_CLSD == "Y" ~ "-",
        CUR_AREA_TEAM_LTST_NM %in% c(
          'ENGLISH/WELSH DUMMY DENTAL',
          'UNIDENTIFIED DEPUTISING SERVICES',
          'UNIDENTIFIED DOCTORS'
        ) ~ "-",
        TRUE ~ CUR_AREA_TEAM_LTST_ALT_CDE
      )
    ) |>
    dplyr::select(LVL_5_OUPDT,
                  LVL_5_OU,
                  ICB_NAME,
                  ICB_CODE,
                  LVL_5_LTST_TYPE)

  # build drug dimension -------
  drug <- dplyr::tbl(con,
                     from = dbplyr::in_schema("DIM", "CDR_EP_DRUG_BNF_DIM")) |>
    dplyr::select(YEAR_MONTH,
                  BNF_CHAPTER,
                  CHAPTER_DESCR,
                  SECTION_DESCR,
                  BNF_SECTION,
                  RECORD_ID)

  # build nadp
  nadp <- dplyr::tbl(con,
                     from = dbplyr::in_schema("AML", "COUNTRY_NADP")) |>
    dplyr::select(YEAR_MONTH_FACT_LINK,
                  NADP)

  # build fact table
  fact <- dplyr::tbl(con,
                     from = dbplyr::in_schema("AML", "PX_FORM_ITEM_ELEM_COMB_FACT_AV")) |>
    #regular exclusions
    dplyr::filter(
      PAY_DA_END == "N",
      # excludes disallowed items
      PAY_ND_END == "N",
      # excludes not dispensed items
      PAY_RB_END == "N",
      # excludes referred back items
      CD_REQ == "N",
      # excludes controlled drug requisitions
      OOHC_IND == 0L,
      # excludes out of hours dispensing
      PRIVATE_IND == 0L,
      # excludes private dispensers
      IGNORE_FLAG == "N",
      # excludes LDP dummy forms
      PRESC_TYPE_PRNT %NOT IN% c(8L, 54L)
    ) |>
    dplyr::inner_join(tdim,
                      by = c("YEAR_MONTH" = "YEAR_MONTH")) |>
    dplyr::inner_join(porg,
                      by = c("PRESC_TYPE_PRNT" = "LVL_5_OUPDT",
                             "PRESC_ID_PRNT" = "LVL_5_OU")) |>
    dplyr::inner_join(drug,
                      by = c(
                        "CALC_PREC_DRUG_RECORD_ID" = "RECORD_ID",
                        "YEAR_MONTH" = "YEAR_MONTH"
                      )) |>
    dplyr::inner_join(nadp,
                      by = c("YEAR_MONTH" = "YEAR_MONTH_FACT_LINK")) |>
    dplyr::mutate(
      LVL_5_LTST_TYPE = case_when(
        LVL_5_LTST_TYPE == "COMMUNITY NURSE PRESCRIBING CONTRACT" ~ "GP PRACTICE / COST CENTRE",
        TRUE ~ LVL_5_LTST_TYPE
      )
    ) |>
    mutate(ACTUAL_COST = round(((
      ITEM_PAY_DR_NIC - (ITEM_PAY_DR_NIC * NADP)/100 +
        ITEM_PAT_PACK_PAYMENT +
        ITEM_PAY_OOPE_AMT +
        ITEM_CONTAINER_ALLOWANCE
    )
    / 100),
    2)) |>
    dplyr::group_by(
      YEAR_MONTH,
      FINANCIAL_YEAR,
      ICB_NAME,
      ICB_CODE,
      LVL_5_LTST_TYPE,
      BNF_CHAPTER,
      CHAPTER_DESCR,
      BNF_SECTION,
      SECTION_DESCR
    ) |>
    dplyr::summarise(
      ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T) / 100,
      ACTUAL_COST = sum(ACTUAL_COST, na.rm = T),
      .groups = "drop"
    )


  # drop time dimension if exists
  exists <- con |>
    DBI::dbExistsTable(name = "PCHC_FACT_DIM")
  # Drop any existing table beforehand
  if (exists) {
    con |>
      DBI::dbRemoveTable(name = "PCHC_FACT_DIM")
  }

  #build table
  fact |>
    dplyr::compute("PCHC_FACT_DIM", analyze = FALSE, temporary = FALSE)
}
