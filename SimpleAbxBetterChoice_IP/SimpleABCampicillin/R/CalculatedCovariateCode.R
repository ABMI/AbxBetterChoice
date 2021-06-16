# Copyright 2018 Observational Health Data Sciences and Informatics
#
# This file is part of SimpleABCampicillin
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

#' Extracts covariates based on specific equations
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
#' @param cohortId  cohort id for the target population cohort
#' @param covariateSettings  settings for the covariate cohorts and time periods
#'
#' @return
#' The models will now be in the package
#'
#' @export
getCalculatedCovariateData <- function(connection,
                                   oracleTempSchema = NULL,
                                   cdmDatabaseSchema,
                                   cdmVersion = "5",
                                   cohortTable = "#cohort_person",
                                   cohortId,
                                   rowIdField,
                                   aggregated,
                                   covariateSettings,
                                   calculatedData = 'antibiogram') {
  
  # Some SQL to construct the covariates:
  
  sql <- paste(
    "select a.@row_id_field AS row_id, a.cohort_start_date",
    "from @cohort_temp_table a inner join @covariate_cohort_schema.@covariate_cohort_table b",
    " on a.subject_id = b.subject_id and a.cohort_start_date = b.cohort_start_date",
    "where b.cohort_definition_id = @covariate_cohort_id
    group by a.@row_id_field, a.cohort_start_date "
  )
  
  sql <- SqlRender::render(sql,
                           row_id_field = rowIdField,
                           covariate_cohort_schema = covariateSettings$cohortDatabaseSchema,
                           covariate_cohort_table = covariateSettings$cohortTable,
                           covariate_cohort_id = covariateSettings$cohortId,
                           cohort_temp_table = cohortTable)
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  covariate_1 <- DatabaseConnector::querySql(connection, sql)

  if(!is.null(system.file("settings", paste0(calculatedData,".csv"), package = "SimpleABCampicillin"))){
    covariate_2 <- read.csv(file = system.file("settings", paste0(calculatedData,".csv"), package = "SimpleABCampicillin"))
    covariate_2 <- covariate_2[covariate_2$COVARIATE_ID == covariateSettings$covariateId,]
  }
  # else{
  #   covariate_2 <- data.frame(COHORT_START_DATE = NA, COVARIATE_ID = NA, COVARIATE_VALUE = NA)
  #   for(date in unique(covariates_1$COHORT_START_DATE)){
  #     sql <-"select convert(date,@cohortStartDate) as cohort_start_date, @covariateId as covariate_id, convert(float,count(CASE WHEN b.value_source_value in ('R', 'I') THEN 1 END)) / convert(float, count(*)) * 100 as covariate_value 
  #   from (select * from @cdmDatabaseSchema.measurement where measurement_concept_id = @measurementConceptId1 and value_as_concept_id = @valueAsConceptId1
  #   and measurement_date >= dateadd(day, @startDay, convert(date,@cohortStartDate)) and measurement_date <= dateadd(day, @endDay, convert(date,@cohortStartDate))) a join @cdmDatabaseSchema.fact_relationship fr
  #   on a.measurement_id = fr.fact_id_1 join (select * from @cdmDatabaseSchema.measurement where measurement_concept_id = @measurementConceptId2 
  #   and measurement_date >= dateadd(day, @startDay, convert(date,@cohortStartDate)) and measurement_date <= dateadd(day, @endDay, convert(date,@cohortStartDate))
  #   and value_source_value is not null ) b on fr.fact_id_2 = b.measurement_id where b.value_source_value in ('R', 'S', 'I')"
  #     sql <- SqlRender::render(sql,
  #                              cdmDatabaseSchema = covariateSettings$cdmDatabaseSchema,
  #                              cohortStartDate = paste0('\'',date,'\''),
  #                              covariateId = covariateSettings$covariateId,
  #                              measurementConceptId1 = 3026008,
  #                              valueAsConceptId1 = covariateSettings$conceptId1,
  #                              measurementConceptId2 = covariateSettings$conceptId2,
  #                              startDay=covariateSettings$startDay,
  #                              endDay=covariateSettings$endDay)
  #     sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  #     # Retrieve the covariate:
  #     results <- DatabaseConnector::querySql(connection, sql)
  #     covariate_2 <- rbind(covariate_2, results)
  #   }
  # }
  
  covariate_2$COHORT_START_DATE <- lubridate::as_date(covariate_2$COHORT_START_DATE)
  covariates <- merge(covariate_1, covariate_2, by = "COHORT_START_DATE")
  covariates <- covariates[, c(2,3,4)]
  
  
  # Convert column names to camelCase:
  colnames(covariates) <- SqlRender::snakeCaseToCamelCase(colnames(covariates))
  # Construct covariate reference:
  sql <- "select @covariate_id as covariate_id, '@concept_set' as covariate_name,
  789 as analysis_id, -1 as concept_id"
  sql <- SqlRender::render(sql, covariate_id = covariateSettings$covariateId,
                           concept_set=paste(covariateSettings$covariateName,' days before:', covariateSettings$startDay, 'days after:', covariateSettings$endDay))
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  # Retrieve the covariateRef:
  covariateRef  <- DatabaseConnector::querySql(connection, sql)
  colnames(covariateRef) <- SqlRender::snakeCaseToCamelCase(colnames(covariateRef))
  
  analysisRef <- data.frame(analysisId = 789,
                            analysisName = covariateSettings$covariateName,
                            domainId = "calculated covariate",
                            startDay = covariateSettings$startDay,
                            endDay = covariateSettings$endDay,
                            isBinary = "N",
                            missingMeansZero = "N")
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
createCalculatedCovariateSettings <- function(covariateName, covariateId,
                                          cohortDatabaseSchema, cohortTable, cohortId,
                                          startDay=-30, endDay=0, calculatedData) {
  covariateSettings <- list(covariateName=covariateName, covariateId=covariateId,
                            cohortDatabaseSchema=cohortDatabaseSchema,
                            cohortTable=cohortTable,
                            cohortId=cohortId,
                            startDay=startDay,
                            endDay=endDay,
                            calculatedData = calculatedData)
  
  attr(covariateSettings, "fun") <- "getCalculatedCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}

