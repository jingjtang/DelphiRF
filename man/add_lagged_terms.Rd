% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feature_engineering.R
\name{add_lagged_terms}
\alias{add_lagged_terms}
\title{Add Lagged Terms Based on Temporal Resolution}
\usage{
add_lagged_terms(
  df,
  value_col,
  refd_col,
  lag_col,
  lagged_term_list = 1:7,
  temporal_resol = "weekly"
)
}
\arguments{
\item{df}{Data frame containing time-series data.}

\item{value_col}{Character. Name of the column containing values to be lagged.}

\item{refd_col}{Character. Name of the reference date column.}

\item{lag_col}{Character. Name of the column containing lag values.}

\item{lagged_term_list}{Numeric vector. List of lag values to be applied. Default is \code{1:7}.}

\item{temporal_resol}{Character. Temporal resolution, either \code{"weekly"} or \code{"daily"}.
Default is \code{"weekly"}. Determines how lag shifts are applied.}
}
\value{
A modified data frame with additional lagged columns.
}
\description{
This function creates lagged versions of a specified value column based on
user-defined time steps and temporal resolution (daily or weekly).
}
\details{
\itemize{
\item If \code{temporal_resol = "weekly"}, lag values are divided by 7 to match weekly steps.
\item If \code{temporal_resol = "daily"}, lag values are applied directly.
\item The function groups data by \code{report_date}, sorts it by \code{refd_col}, and applies lags.
\item New lagged columns are named as \code{"lagX"}, where \code{X} is the lag value.
}
}
