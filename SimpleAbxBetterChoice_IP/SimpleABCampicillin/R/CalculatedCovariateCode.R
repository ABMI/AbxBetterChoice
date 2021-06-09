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
#' @param aggregated  whether the covariate should be aggregated
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
                                   rowIdField = "row_id",
                                   aggregated,
                                   cohortId,
                                   covariateSettings) {
  
  # Some SQL to construct the covariates:
  
  sql <- "select subject_id as row_id, cohort_start_date from @cohortDatabaseSchema.@cohortTable where cohort_definition_id = @cohortId"
  sql <- SqlRender::render(sql,
                           cohortDatabaseSchema = covariateSettings$cohortDatabaseSchema,
                           cohortTable = covariateSettings$cohortTable,
                           cohortId = covariateSettings$cohortId
                           )
  sql <- SqlRender::render(sql,
                           cohortDatabaseSchema = "cohortDb.dbo",
                           cohortTable = "SimpleAbxBetterChoice",
                           cohortId = 2091
  )
  connection <- DatabaseConnector::connect(connectionDetails)
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  covariates_1 <- DatabaseConnector::querySql(connection, sql)

  covariate_2 <- data.frame(COHORT_START_DATE = NA, COVARIATE_ID = NA, COVARIATE_VALUE = NA)
  allDate <- seq(as.Date("1999-08-17"), as.Date("2020-05-31"), by = "days")
  # for(i in 1:length(unique(covariates_1$COHORT_START_DATE))){
  
  for(i in 1:length(allDate)){
    sql <-"select convert(date,@cohortStartDate) as cohort_start_date, @covariateId as covariate_id, convert(float,count(CASE WHEN b.value_source_value in ('R', 'I') THEN 1 END)) / convert(float, count(*)) * 100 as covariate_value 
    from (select * from @cdmDatabaseSchema.measurement where measurement_concept_id = @measurementConceptId1 and value_as_concept_id = @valueAsConceptId1
    and measurement_date >= dateadd(day, @startDay, convert(date,@cohortStartDate)) and measurement_date <= dateadd(day, @endDay, convert(date,@cohortStartDate))) a join @cdmDatabaseSchema.fact_relationship fr
    on a.measurement_id = fr.fact_id_1 join (select * from @cdmDatabaseSchema.measurement where measurement_concept_id = @measurementConceptId2 
    and measurement_date >= dateadd(day, @startDay, convert(date,@cohortStartDate)) and measurement_date <= dateadd(day, @endDay, convert(date,@cohortStartDate))
    and value_source_value is not null ) b on fr.fact_id_2 = b.measurement_id where b.value_source_value in ('R', 'S', 'I')"
    
    # sql <- SqlRender::render(sql,
    #                          cdmDatabaseSchema = covariateSettings$cdmDatabaseSchema,
    #                          cohortStartDate = cohortStartDate,
    #                          covariateId = covariateSettings$covariateId,
    #                          measurementConceptId1 = covariateSettings$measurementConceptId1,
    #                          valueAsConceptId1 = covariateSettings$valueAsConceptId1,
    #                          measurementConceptId2 = covariateSettings$measurementConceptId2,
    #                          startDay=covariateSettings$startDay,
    #                          endDay=covariateSettings$endDay)
    
    # --- 3010007 : Ciprofloxacin / 3016774 : Ampicillin / 3034442 : Ceftriaxone / 3000769 : gentamicin / 3035551 levofloxacin
    # --- 3037501 : nitrofurantoin / 3000127 : tetracycline / 3013635 : trimethoprim- sulfamethoxazole
    
    try({
      sql <- SqlRender::render(sql,
                               cdmDatabaseSchema = cdmDatabaseSchema,
                               cohortStartDate = paste0('\'',allDate[i],'\''),
                               covariateId = 40116833000769789,
                               measurementConceptId1 = 3026008,
                               valueAsConceptId1 = 4011683,
                               measurementConceptId2 = 3000769, # antibiotics_concept_id
                               startDay=-365,
                               endDay=0)
      
      sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
      # Retrieve the covariate:
      results <- DatabaseConnector::querySql(connection, sql)
      covariate_2 <- rbind(covariate_2, results)
    })
  }
  
  covariate_2 <- covariate_2[-1,]
  covariate_2$COHORT_START_DATE <- lubridate::as_date(covariate_2$COHORT_START_DATE)
  write.csv(covariate_2, file = file.path(outputFolder, "urineEcoliGentamicinResistance.csv"), row.names = F)
  

  covariates <- merge(covariate_1, covariate_2, by = "cohort_start_date", all.y = T) %>% select(row_id, covariate_id, covariate_value)
  
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
                            analysisName = "calculated covariate",
                            domainId = "calculated covariate",
                            startDay = 0,
                            endDay = 0,
                            isBinary = "N",
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
createCalculatedCovariateSettings <- function(covariateName, covariateId,
                                          cohortDatabaseSchema, cohortTable, cohortId,
                                          startDay=-30, endDay=0, count=T) {
  covariateSettings <- list(covariateName=covariateName, covariateId=covariateId,
                            cohortDatabaseSchema=cohortDatabaseSchema,
                            cohortTable=cohortTable,
                            cohortId=cohortId,
                            startDay=startDay,
                            endDay=endDay,
                            count=count)
  
  attr(covariateSettings, "fun") <- "getCalculatedCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}

