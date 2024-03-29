% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CalculatedCovariateCode.R
\name{getCalculatedCovariateData}
\alias{getCalculatedCovariateData}
\title{Extracts covariates based on specific equations}
\usage{
getCalculatedCovariateData(
  connection,
  oracleTempSchema = NULL,
  cdmDatabaseSchema,
  cdmVersion = "5",
  cohortTable = "#cohort_person",
  cohortId,
  rowIdField,
  aggregated,
  covariateSettings,
  calculatedData = "antibiogram"
)
}
\arguments{
\item{connection}{The database connection}

\item{oracleTempSchema}{The temp schema if using oracle}

\item{cdmDatabaseSchema}{The schema of the OMOP CDM data}

\item{cdmVersion}{version of the OMOP CDM data}

\item{cohortTable}{the table name that contains the target population cohort}

\item{cohortId}{cohort id for the target population cohort}

\item{rowIdField}{string representing the unique identifier in the target population cohort}

\item{covariateSettings}{settings for the covariate cohorts and time periods}
}
\value{
The models will now be in the package
}
\description{
Extracts covariates based on specific equations
}
\details{
The user specifies a cohort and time period and then a covariate is constructed whether they are in the
cohort during the time periods relative to target population cohort index
}
