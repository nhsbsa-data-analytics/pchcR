#' @title Easy helper for 'pchc_options'
#'
#' @name pchc_options
#'
#' @description
#' Set the options used in the PCHC pipeline
#'
#' @export
#'
#' @examples
#' pchc_options()

pchc_options <- function() {
  # prevent scientific notation
  options(scipen = 999)

  # prevent printing of groups from summarise
  options(dplyr.summarise.inform = FALSE)

  # thousand separator for highcharts
  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$thousandsSep <- ","
  hcoptslang$numericSymbols <- c("k","M","B","T","P","E")
  options(highcharter.lang = hcoptslang)
}
