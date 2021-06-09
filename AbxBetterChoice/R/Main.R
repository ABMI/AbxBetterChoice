# Copyright 2018 Observational Health Data Sciences and Informatics
#
# This file is part of AbxBetterChoice
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

#' Execute the Study
#'
#' @details
#' This function executes the AbxBetterChoice Study.
#' 
#' @param connectionDetails    An object of type \code{connectionDetails} as created using the
#'                             \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                             DatabaseConnector package.
#' @param cdmDatabaseSchema    Schema name where your patient-level data in OMOP CDM format resides.
#'                             Note that for SQL Server, this should include both the database and
#'                             schema name, for example 'cdm_data.dbo'.
#' @param cdmDatabaseName      Shareable name of the database 
#' @param cohortDatabaseSchema Schema name where intermediate data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_data.dbo'.
#' @param cohortTable          The name of the table that will be created in the work database schema.
#'                             This table will hold the target population cohorts used in this
#'                             study.
#' @param oracleTempSchema     Should be used in Oracle to specify a schema where the user has write
#'                             priviliges for storing temporary tables.
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/). Do not use a folder on a network drive since this greatly impacts
#'                             performance.
#' @param createProtocol       Creates a protocol based on the analyses specification                             
#' @param createCohorts        Create the cohortTable table with the target population and outcome cohorts?
#' @param runAllAnalyses       Run the model development
#' @param createResultsDoc     Create a document containing the results of each prediction
#' @param createValidationPackage  Create a package for sharing the models 
#' @param analysesToValidate   A vector of analysis ids (e.g., c(1,3,10)) specifying which analysese to export into validation package. Default is NULL and all are exported.
#' @param packageResults       Should results be packaged for later sharing?     
#' @param minCellCount         The minimum number of subjects contributing to a count before it can be included 
#'                             in packaged results.
#' @param createShiny          Create a shiny app with the results
#' @param createJournalDocument Do you want to create a template journal document populated with results?
#' @param analysisIdDocument   Which Analysis_id do you want to create the document for?
#' @param verbosity            Sets the level of the verbosity. If the log level is at or higher in priority than the logger threshold, a message will print. The levels are:
#'                                         \itemize{
#'                                         \item{DEBUG}{Highest verbosity showing all debug statements}
#'                                         \item{TRACE}{Showing information about start and end of steps}
#'                                         \item{INFO}{Show informative information (Default)}
#'                                         \item{WARN}{Show warning messages}
#'                                         \item{ERROR}{Show error messages}
#'                                         \item{FATAL}{Be silent except for fatal errors}
#'                                         }                              
#' @param cdmVersion           The version of the common data model
#' @param cohortVariableSetting the name of the custom cohort covariate settings to use                             
#'
#' @examples
#' \dontrun{
#' connectionDetails <- createConnectionDetails(dbms = "postgresql",
#'                                              user = "joe",
#'                                              password = "secret",
#'                                              server = "myserver")
#' }
#'
#' @export
execute <- function(connectionDetails,
                    cdmDatabaseSchema,
                    cdmDatabaseName = 'friendly database name',
                    cohortDatabaseSchema = cdmDatabaseSchema,
                    cohortTable = "cohort",
                    oracleTempSchema = cohortDatabaseSchema,
                    outputFolder,
                    createCohorts = F,
                    runAllAnalyses = F,
                    analysesToValidate = NULL,
                    minCellCount= 5,
                    analysisIdDocument = 1,
                    verbosity = "INFO",
                    cdmVersion = 5,
                    cohortVariableSetting = NULL) {
  
  if (!file.exists(outputFolder))
    dir.create(outputFolder, recursive = TRUE)
  
  ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))
  
  if (createCohorts) {
    ParallelLogger::logInfo("Creating cohorts")
    AbxBetterChoice::createCohorts(connectionDetails = connectionDetails,
                                  cdmDatabaseSchema = cdmDatabaseSchema,
                                  cohortDatabaseSchema = cohortDatabaseSchema,
                                  cohortTable = cohortTable,
                                  oracleTempSchema = oracleTempSchema,
                                  outputFolder = outputFolder,
                                  cohortVariableSetting = cohortVariableSetting)
  }
  
  if(runAllAnalyses){
    ParallelLogger::logInfo("Running predictions - ABCceftriaxone model")
    outputFolder_model1 <- paste0(outputFolder, "/ABCceftriaxone")
    ABCceftriaxone::execute(connectionDetails,
                            cdmDatabaseSchema,
                            cdmDatabaseName,
                            cohortDatabaseSchema,
                            cohortTable,
                            oracleTempSchema,
                            outputFolder = outputFolder_model1,
                            createProtocol = F,
                            createCohorts = F,
                            runAnalyses = T,
                            createResultsDoc = F,
                            createValidationPackage = F,
                            analysesToValidate = NULL,
                            packageResults = F,
                            minCellCount= 5,
                            createShiny = T,
                            createJournalDocument = F,
                            analysisIdDocument = 1,
                            verbosity = "INFO",
                            cdmVersion = 5,
                            cohortVariableSetting)
    
    ParallelLogger::logInfo("Running predictions - ABCciprofloxacin model")
    outputFolder_model2 <- paste0(outputFolder, "/ABCciprofloxacin")
    ABCciprofloxacin::execute(connectionDetails,
                            cdmDatabaseSchema,
                            cdmDatabaseName,
                            cohortDatabaseSchema,
                            cohortTable,
                            oracleTempSchema,
                            outputFolder = outputFolder_model2,
                            createProtocol = F,
                            createCohorts = F,
                            runAnalyses = T,
                            createResultsDoc = F,
                            createValidationPackage = F,
                            analysesToValidate = NULL,
                            packageResults = F,
                            minCellCount= 5,
                            createShiny = T,
                            createJournalDocument = F,
                            analysisIdDocument = 1,
                            verbosity = "INFO",
                            cdmVersion = 5,
                            cohortVariableSetting)
    
    ParallelLogger::logInfo("Running predictions - ABCtmpsmx model")
    outputFolder_model3 <- paste0(outputFolder, "/ABCtmpsmx")
    ABCtmpsmx::execute(connectionDetails,
                            cdmDatabaseSchema,
                            cdmDatabaseName,
                            cohortDatabaseSchema,
                            cohortTable,
                            oracleTempSchema,
                            outputFolder = outputFolder_model3,
                            createProtocol = F,
                            createCohorts = F,
                            runAnalyses = T,
                            createResultsDoc = F,
                            createValidationPackage = F,
                            analysesToValidate = NULL,
                            packageResults = F,
                            minCellCount= 5,
                            createShiny = T,
                            createJournalDocument = F,
                            analysisIdDocument = 1,
                            verbosity = "INFO",
                            cdmVersion = 5,
                            cohortVariableSetting)
    
    ParallelLogger::logInfo("Running predictions - ABClevofloxacin model")
    outputFolder_model4 <- paste0(outputFolder, "/ABClevofloxacin")
    ABClevofloxacin::execute(connectionDetails,
                       cdmDatabaseSchema,
                       cdmDatabaseName,
                       cohortDatabaseSchema,
                       cohortTable,
                       oracleTempSchema,
                       outputFolder = outputFolder_model4,
                       createProtocol = F,
                       createCohorts = F,
                       runAnalyses = T,
                       createResultsDoc = F,
                       createValidationPackage = F,
                       analysesToValidate = NULL,
                       packageResults = F,
                       minCellCount= 5,
                       createShiny = T,
                       createJournalDocument = F,
                       analysisIdDocument = 1,
                       verbosity = "INFO",
                       cdmVersion = 5,
                       cohortVariableSetting)
    
    ParallelLogger::logInfo("Running predictions - ABCnitrofurantoin model")
    outputFolder_model5 <- paste0(outputFolder, "/ABCnitrofurantoin")
    ABCnitrofurantoin::execute(connectionDetails,
                       cdmDatabaseSchema,
                       cdmDatabaseName,
                       cohortDatabaseSchema,
                       cohortTable,
                       oracleTempSchema,
                       outputFolder = outputFolder_model5,
                       createProtocol = F,
                       createCohorts = F,
                       runAnalyses = T,
                       createResultsDoc = F,
                       createValidationPackage = F,
                       analysesToValidate = NULL,
                       packageResults = F,
                       minCellCount= 5,
                       createShiny = T,
                       createJournalDocument = F,
                       analysisIdDocument = 1,
                       verbosity = "INFO",
                       cdmVersion = 5,
                       cohortVariableSetting)
    
    ParallelLogger::logInfo("Running predictions - ABCampicillin model")
    outputFolder_model6 <- paste0(outputFolder, "/ABCampicillin")
    ABCampicillin::execute(connectionDetails,
                            cdmDatabaseSchema,
                            cdmDatabaseName,
                            cohortDatabaseSchema,
                            cohortTable,
                            oracleTempSchema,
                            outputFolder = outputFolder_model6,
                            createProtocol = F,
                            createCohorts = F,
                            runAnalyses = T,
                            createResultsDoc = F,
                            createValidationPackage = F,
                            analysesToValidate = NULL,
                            packageResults = F,
                            minCellCount= 5,
                            createShiny = T,
                            createJournalDocument = F,
                            analysisIdDocument = 1,
                            verbosity = "INFO",
                            cdmVersion = 5,
                            cohortVariableSetting)
    
    ParallelLogger::logInfo("Running predictions - ABCgentamicin model")
    outputFolder_model7 <- paste0(outputFolder, "/ABCgentamicin")
    ABCgentamicin::execute(connectionDetails,
                            cdmDatabaseSchema,
                            cdmDatabaseName,
                            cohortDatabaseSchema,
                            cohortTable,
                            oracleTempSchema,
                            outputFolder = outputFolder_model7,
                            createProtocol = F,
                            createCohorts = F,
                            runAnalyses = T,
                            createResultsDoc = F,
                            createValidationPackage = F,
                            analysesToValidate = NULL,
                            packageResults = F,
                            minCellCount= 5,
                            createShiny = T,
                            createJournalDocument = F,
                            analysisIdDocument = 1,
                            verbosity = "INFO",
                            cdmVersion = 5,
                            cohortVariableSetting)
    
    ParallelLogger::logInfo("Running predictions - ABCtetracycline model")
    outputFolder_model8 <- paste0(outputFolder, "/ABCtetracycline")
    ABCtetracycline::execute(connectionDetails,
                            cdmDatabaseSchema,
                            cdmDatabaseName,
                            cohortDatabaseSchema,
                            cohortTable,
                            oracleTempSchema,
                            outputFolder = outputFolder_model8,
                            createProtocol = F,
                            createCohorts = F,
                            runAnalyses = T,
                            createResultsDoc = F,
                            createValidationPackage = F,
                            analysesToValidate = NULL,
                            packageResults = F,
                            minCellCount= 5,
                            createShiny = T,
                            createJournalDocument = F,
                            analysisIdDocument = 1,
                            verbosity = "INFO",
                            cdmVersion = 5,
                            cohortVariableSetting)
  }
  
  invisible(NULL)
}




