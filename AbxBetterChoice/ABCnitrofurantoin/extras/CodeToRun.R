library(ABCnitrofurantoin)
# USER INPUTS
#=======================
# The folder where the study intermediate and result files will be written:
outputFolder <- "./ABCnitrofurantoinResults"

# Specify where the temporary files (used by the ff package) will be created:
options(andromedaTempFolder = "location with space to save big data")

# Details for connecting to the server:
dbms <- "you dbms"
user <- 'your username'
pw <- 'your password'
server <- 'your server'
port <- 'your port'

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

# Add the database containing the OMOP CDM data
cdmDatabaseSchema <- 'cdm database schema'
# Add a sharebale name for the database containing the OMOP CDM data
cdmDatabaseName <- 'a friendly shareable  name for your database'
# Add a database with read/write access as this is where the cohorts will be generated
cohortDatabaseSchema <- 'work database schema'

oracleTempSchema <- NULL

# table name where the cohorts will be generated
cohortTable <- 'ABCnitrofurantoinCohort'
#=======================
databaseId <- cdmDatabaseName
databaseName <- cdmDatabaseName
databaseDescription <- "IBM MarketScan® Commercial Claims and Encounters Database (CCAE) represent data from individuals enrolled in United States employer-sponsored insurance health plans. The data includes adjudicated health insurance claims (e.g. inpatient, outpatient, and outpatient pharmacy) as well as enrollment data from large employers and health plans who provide private healthcare coverage to employees, their spouses, and dependents. Additionally, it captures laboratory tests for a subset of the covered lives. This administrative claims database includes a variety of fee-for-service, preferred provider organizations, and capitated health plans." 


# Use this to run the cohorttDiagnostics. The results will be stored in the diagnosticsExport subfolder of the outputFolder. This can be shared between sites.
ABCnitrofurantoin::runCohortDiagnostics(connectionDetails = connectionDetails,
                                     cdmDatabaseSchema = cdmDatabaseSchema,
                                     cohortDatabaseSchema = cohortDatabaseSchema,
                                     cohortTable = cohortTable,
                                     oracleTempSchema = oracleTempSchema,
                                     outputFolder = outputFolder,
                                     databaseId = databaseId,
                                     databaseName = databaseName,
                                     databaseDescription = databaseDescription,
                                     createCohorts = TRUE,
                                     runInclusionStatistics = TRUE,
                                     runIncludedSourceConcepts = TRUE,
                                     runOrphanConcepts = TRUE,
                                     runTimeDistributions = TRUE,
                                     runBreakdownIndexEvents = TRUE,
                                     runIncidenceRates = TRUE,
                                     runCohortOverlap = TRUE,
                                     runCohortCharacterization = TRUE,
                                     runTemporalCohortCharacterization = TRUE,
                                     minCellCount = 5,
                                     cohortVariableSetting = "cohortCovariate")

# To view the results:
# Optional: if there are results zip files from multiple sites in a folder, this merges them, which will speed up starting the viewer:
CohortDiagnostics::preMergeDiagnosticsFiles(file.path(outputFolder, "diagnosticsExport"))

# Use this to view the results. Multiple zip files can be in the same folder. If the files were pre-merged, this is automatically detected: 
CohortDiagnostics::launchDiagnosticsExplorer(file.path(outputFolder, "diagnosticsExport"))


# To explore a specific cohort in the local database, viewing patient profiles:
# Where 123 is the ID of the cohort you wish to inspect.
# CohortDiagnostics::launchCohortExplorer(connectionDetails = connectionDetails,
#                                         cdmDatabaseSchema = cdmDatabaseSchema,
#                                         cohortDatabaseSchema = cohortDatabaseSchema,
#                                         cohortTable = cohortTable,
#                                         cohortId = 123)




execute(connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cdmDatabaseName = cdmDatabaseName,
        cohortDatabaseSchema = cohortDatabaseSchema,
		    oracleTempSchema = oracleTempSchema,
        cohortTable = cohortTable,
        outputFolder = outputFolder,
        createProtocol = F,
        createCohorts = F,
        runAnalyses = F,
        createResultsDoc = F,
        packageResults = F,
        createValidationPackage = F,  
        #analysesToValidate = 1,
        minCellCount= 5,
        createShiny = F,
        createJournalDocument = F,
        analysisIdDocument = 1,
		    cohortVariableSetting = "cohortCovariate")

# Uncomment and run the next line to see the shiny results:
# PatientLevelPrediction::viewMultiplePlp(outputFolder)
