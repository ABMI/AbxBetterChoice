# Copyright 2018 Observational Health Data Sciences and Informatics
#
# This file is part of SimpleABCtmpsmx
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Extracts covariates based on cohorts
#'
#' @details
#' The user specifies a cohort and time period and then a covariate is constructed whether they are in the
#' cohort during the time periods relative to target population cohort index
#'
#' @param connection  The database connection
#' @param oracleTempSchema  The temp schema if using oracle
#' @param cdmDatabaseSchema  The schema of the OMOP CDM data
#' @param cdmVersion  version of the OMOP CDM data
#' @param cohortTable  the table name that contains the target population cohort
#' @param rowIdField  string representing the unique identifier in the target population cohort
#' @param aggregated  whether the covariate should be aggregated
#' @param cohortId  cohort id for the target population cohort
#' @param covariateSettings  settings for the covariate cohorts and time periods
#'
#' @return
#' The models will now be in the package
#'
#' @export
getCohortCovariateData <- function(connection,
                                   oracleTempSchema = NULL,
                                   cdmDatabaseSchema,
                                   cdmVersion = "5",
                                   cohortTable = "#cohort_person",
                                   rowIdField = "row_id",
                                   aggregated,
                                   cohortId,
                                   covariateSettings) {
  
  # Some SQL to construct the covariate:
  sql <- paste(
    "select a.@row_id_field AS row_id, @covariate_id AS covariate_id,
    {@countval}?{count(distinct b.cohort_start_date)}:{max(1)} as covariate_value",
    "from @cohort_temp_table a inner join @covariate_cohort_schema.@covariate_cohort_table b",
    " on a.subject_id = b.subject_id and ",
    " b.cohort_start_date <= dateadd(day, @endDay, a.cohort_start_date) and ",
    " b.cohort_end_date >= dateadd(day, @startDay, a.cohort_start_date) ",
    "where b.cohort_definition_id = @covariate_cohort_id
    group by a.@row_id_field "
  )
  
  sql <- SqlRender::render(sql,
                           covariate_cohort_schema = covariateSettings$cohortDatabaseSchema,
                           covariate_cohort_table = covariateSettings$cohortTable,
                           covariate_cohort_id = covariateSettings$cohortId,
                           cohort_temp_table = cohortTable,
                           row_id_field = rowIdField,
                           startDay=covariateSettings$startDay,
                           covariate_id = covariateSettings$covariateId,
                           endDay=covariateSettings$endDay,
                           countval = covariateSettings$count)
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  # Retrieve the covariate:
  covariates <- DatabaseConnector::querySql(connection, sql)
  # Convert colum names to camelCase:
  colnames(covariates) <- SqlRender::snakeCaseToCamelCase(colnames(covariates))
  # Construct covariate reference:
  sql <- "select @covariate_id as covariate_id, '@concept_set' as covariate_name,
  456 as analysis_id, -1 as concept_id"
  sql <- SqlRender::render(sql, covariate_id = covariateSettings$covariateId,
                           concept_set=paste(covariateSettings$covariateName,' days before:', covariateSettings$startDay, 'days after:', covariateSettings$endDay))
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  # Retrieve the covariateRef:
  covariateRef  <- DatabaseConnector::querySql(connection, sql)
  colnames(covariateRef) <- SqlRender::snakeCaseToCamelCase(colnames(covariateRef))
  
  analysisRef <- data.frame(analysisId = 456,
                            analysisName = "cohort covariate",
                            domainId = "cohort covariate",
                            startDay = 0,
                            endDay = 0,
                            isBinary = "Y",
                            missingMeansZero = "Y")
  analysisRef <- ff::as.ffdf(analysisRef)
  
  metaData <- list(sql = sql, call = match.call())
  result <- list(covariates = covariates,
                 covariateRef = covariateRef,
                 analysisRef=analysisRef,
                 metaData = metaData)
  class(result) <- "covariateData"
  return(result)
}

#' @export
createCohortCovariateSettings <- function(covariateName, covariateId,
                                          cohortDatabaseSchema, cohortTable, cohortId,
                                          startDay=-30, endDay=0, count=T) {
  covariateSettings <- list(covariateName=covariateName, covariateId=covariateId,
                            cohortDatabaseSchema=cohortDatabaseSchema,
                            cohortTable=cohortTable,
                            cohortId=cohortId,
                            startDay=startDay,
                            endDay=endDay,
                            count=count)
  
  attr(covariateSettings, "fun") <- "getCohortCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}


