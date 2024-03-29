% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_number.R
\name{format_number}
\alias{format_number}
\title{Easy helper for format_data function}
\usage{
format_number(x, percentage = FALSE, currency = FALSE)
}
\arguments{
\item{x}{The numeric to be formatted}

\item{percentage}{TRUE/FALSE - indicate if the number is a percentage}

\item{currency}{TRUE/FALSE - indicate if the number is a currency}
}
\description{
Function for formatting numbers to 3 significant figures and adding the
appropriate wording if needed
}
\examples{
format_number(123456)
format_number(0.1235)
format_number(1.2345, percentage = TRUE)
format_number(1000, currency = TRUE)
}
