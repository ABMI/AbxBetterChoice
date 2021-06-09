##################################BASELINE CHARACTERISTICS######
library(dplyr)

####Ampicillin #####
studyPop <- readRDS(file = file.path(outputFolder, "SimpleABCampicillin/StudyPop_L1_T2091_O2085_P1.rds"))
covariateSummary <- readRDS(file = file.path(outputFolder, "SimpleABCampicillin/Analysis_1/plpResult/covariateSummary.rds"))

n <- data.frame(covariateName = "n", ampicillin = nrow(studyPop))
outcome <- data.frame(covariateName = "Resistance",
                      ampicillin = paste0(nrow(studyPop[studyPop$outcomeCount == 1,]),
                                          ' (',
                                          round(nrow(studyPop[studyPop$outcomeCount == 1,])/nrow(studyPop)*100,1),
                                          '%)'))
##Age
age <- studyPop %>% summarise(covariateName = "ageMean", meanAge = mean(ageYear), sd = sd(ageYear))
age$ampicillin <- paste0(round(age$meanAge,1), ' ± ', round(age$sd,1))
age <- age %>% select(covariateName, ampicillin)

#CovariateSummary 
ampicillin <- as.data.frame(covariateSummary %>% select(covariateName, CovariateCount, CovariateMean))
ampicillin$CovariateMean <- ampicillin$CovariateMean*100

ampicillin$ampicillin <- paste0(ampicillin$CovariateCount, ' (', round(ampicillin$CovariateMean,1), '%)')
ampicillin <- ampicillin %>% select(covariateName, ampicillin) 

ampicillin <- rbind(n, outcome, age, ampicillin)
######################

####ceftriaxone #####
studyPop <- readRDS(file = file.path(outputFolder, "SimpleABCceftriaxone/StudyPop_L1_T1812_O1815_P1.rds"))
covariateSummary <- readRDS(file = file.path(outputFolder, "SimpleABCceftriaxone/Analysis_1/plpResult/covariateSummary.rds"))

n <- data.frame(covariateName = "n", ceftriaxone = nrow(studyPop))
outcome <- data.frame(covariateName = "Resistance",
                      ceftriaxone = paste0(nrow(studyPop[studyPop$outcomeCount == 1,]),
                                          ' (',
                                          round(nrow(studyPop[studyPop$outcomeCount == 1,])/nrow(studyPop)*100,1),
                                          '%)'))
##Age
age <- studyPop %>% summarise(covariateName = "ageMean", meanAge = mean(ageYear), sd = sd(ageYear))
age$ceftriaxone <- paste0(round(age$meanAge,1), ' ± ', round(age$sd,1))
age <- age %>% select(covariateName, ceftriaxone)

#CovariateSummary 
ceftriaxone <- as.data.frame(covariateSummary %>% select(covariateName, CovariateCount, CovariateMean))
ceftriaxone$CovariateMean <- ceftriaxone$CovariateMean*100

ceftriaxone$ceftriaxone <- paste0(ceftriaxone$CovariateCount, ' (', round(ceftriaxone$CovariateMean,1), '%)')
ceftriaxone <- ceftriaxone %>% select(covariateName, ceftriaxone) 

ceftriaxone <- rbind(n, outcome, age, ceftriaxone)
######################

####ciprofloxacin #####
studyPop <- readRDS(file = file.path(outputFolder, "SimpleABCciprofloxacin/StudyPop_L1_T1808_O1862_P1.rds"))
covariateSummary <- readRDS(file = file.path(outputFolder, "SimpleABCciprofloxacin/Analysis_1/plpResult/covariateSummary.rds"))

n <- data.frame(covariateName = "n", ciprofloxacin = nrow(studyPop))
outcome <- data.frame(covariateName = "Resistance",
                      ciprofloxacin = paste0(nrow(studyPop[studyPop$outcomeCount == 1,]),
                                          ' (',
                                          round(nrow(studyPop[studyPop$outcomeCount == 1,])/nrow(studyPop)*100,1),
                                          '%)'))
##Age
age <- studyPop %>% summarise(covariateName = "ageMean", meanAge = mean(ageYear), sd = sd(ageYear))
age$ciprofloxacin <- paste0(round(age$meanAge,1), ' ± ', round(age$sd,1))
age <- age %>% select(covariateName, ciprofloxacin)

#CovariateSummary 
ciprofloxacin <- as.data.frame(covariateSummary %>% select(covariateName, CovariateCount, CovariateMean))
ciprofloxacin$CovariateMean <- ciprofloxacin$CovariateMean*100

ciprofloxacin$ciprofloxacin <- paste0(ciprofloxacin$CovariateCount, ' (', round(ciprofloxacin$CovariateMean,1), '%)')
ciprofloxacin <- ciprofloxacin %>% select(covariateName, ciprofloxacin) 

ciprofloxacin <- rbind(n, outcome, age, ciprofloxacin)
######################

####fosfomycin #####
studyPop <- readRDS(file = file.path(outputFolder, "SimpleABCfosfomycin/StudyPop_L1_T158_O160_P1.rds"))
covariateSummary <- readRDS(file = file.path(outputFolder, "SimpleABCfosfomycin/Analysis_1/plpResult/covariateSummary.rds"))

n <- data.frame(covariateName = "n", fosfomycin = nrow(studyPop))
outcome <- data.frame(covariateName = "Resistance",
                      fosfomycin = paste0(nrow(studyPop[studyPop$outcomeCount == 1,]),
                                          ' (',
                                          round(nrow(studyPop[studyPop$outcomeCount == 1,])/nrow(studyPop)*100,1),
                                          '%)'))
##Age
age <- studyPop %>% summarise(covariateName = "ageMean", meanAge = mean(ageYear), sd = sd(ageYear))
age$fosfomycin <- paste0(round(age$meanAge,1), ' ± ', round(age$sd,1))
age <- age %>% select(covariateName, fosfomycin)

#CovariateSummary 
fosfomycin <- as.data.frame(covariateSummary %>% select(covariateName, CovariateCount, CovariateMean))
fosfomycin$CovariateMean <- fosfomycin$CovariateMean*100

fosfomycin$fosfomycin <- paste0(fosfomycin$CovariateCount, ' (', round(fosfomycin$CovariateMean,1), '%)')
fosfomycin <- fosfomycin %>% select(covariateName, fosfomycin) 

fosfomycin <- rbind(n, outcome, age, fosfomycin)
######################

####gentamicin #####
studyPop <- readRDS(file = file.path(outputFolder, "SimpleABCgentamicin/StudyPop_L1_T2090_O2084_P1.rds"))
covariateSummary <- readRDS(file = file.path(outputFolder, "SimpleABCgentamicin/Analysis_1/plpResult/covariateSummary.rds"))
n <- data.frame(covariateName = "n", gentamicin = nrow(studyPop))
outcome <- data.frame(covariateName = "Resistance",
                      gentamicin = paste0(nrow(studyPop[studyPop$outcomeCount == 1,]),
                                          ' (',
                                          round(nrow(studyPop[studyPop$outcomeCount == 1,])/nrow(studyPop)*100,1),
                                          '%)'))
##Age
age <- studyPop %>% summarise(covariateName = "ageMean", meanAge = mean(ageYear), sd = sd(ageYear))
age$gentamicin <- paste0(round(age$meanAge,1), ' ± ', round(age$sd,1))
age <- age %>% select(covariateName, gentamicin)

#CovariateSummary 
gentamicin <- as.data.frame(covariateSummary %>% select(covariateName, CovariateCount, CovariateMean))
gentamicin$CovariateMean <- gentamicin$CovariateMean*100

gentamicin$gentamicin <- paste0(gentamicin$CovariateCount, ' (', round(gentamicin$CovariateMean,1), '%)')
gentamicin <- gentamicin %>% select(covariateName, gentamicin) 

gentamicin <- rbind(n, outcome, age, gentamicin)
######################

####levofloxacin #####
studyPop <- readRDS(file = file.path(outputFolder, "SimpleABClevofloxacin/StudyPop_L1_T1942_O1947_P1.rds"))
covariateSummary <- readRDS(file = file.path(outputFolder, "SimpleABClevofloxacin/Analysis_1/plpResult/covariateSummary.rds"))
n <- data.frame(covariateName = "n", levofloxacin = nrow(studyPop))
outcome <- data.frame(covariateName = "Resistance",
                      levofloxacin = paste0(nrow(studyPop[studyPop$outcomeCount == 1,]),
                                          ' (',
                                          round(nrow(studyPop[studyPop$outcomeCount == 1,])/nrow(studyPop)*100,1),
                                          '%)'))
##Age
age <- studyPop %>% summarise(covariateName = "ageMean", meanAge = mean(ageYear), sd = sd(ageYear))
age$levofloxacin <- paste0(round(age$meanAge,1), ' ± ', round(age$sd,1))
age <- age %>% select(covariateName, levofloxacin)

#CovariateSummary 
levofloxacin <- as.data.frame(covariateSummary %>% select(covariateName, CovariateCount, CovariateMean))
levofloxacin$CovariateMean <- levofloxacin$CovariateMean*100

levofloxacin$levofloxacin <- paste0(levofloxacin$CovariateCount, ' (', round(levofloxacin$CovariateMean,1), '%)')
levofloxacin <- levofloxacin %>% select(covariateName, levofloxacin) 

levofloxacin <- rbind(n, outcome, age, levofloxacin)
######################

####nitrofurantoin #####
studyPop <- readRDS(file = file.path(outputFolder, "SimpleABCnitrofurantoin/StudyPop_L1_T1945_O1948_P1.rds"))
covariateSummary <- readRDS(file = file.path(outputFolder, "SimpleABCnitrofurantoin/Analysis_1/plpResult/covariateSummary.rds"))
n <- data.frame(covariateName = "n", nitrofurantoin = nrow(studyPop))
outcome <- data.frame(covariateName = "Resistance",
                      nitrofurantoin = paste0(nrow(studyPop[studyPop$outcomeCount == 1,]),
                                          ' (',
                                          round(nrow(studyPop[studyPop$outcomeCount == 1,])/nrow(studyPop)*100,1),
                                          '%)'))
##Age
age <- studyPop %>% summarise(covariateName = "ageMean", meanAge = mean(ageYear), sd = sd(ageYear))
age$nitrofurantoin <- paste0(round(age$meanAge,1), ' ± ', round(age$sd,1))
age <- age %>% select(covariateName, nitrofurantoin)

#CovariateSummary 
nitrofurantoin <- as.data.frame(covariateSummary %>% select(covariateName, CovariateCount, CovariateMean))
nitrofurantoin$CovariateMean <- nitrofurantoin$CovariateMean*100

nitrofurantoin$nitrofurantoin <- paste0(nitrofurantoin$CovariateCount, ' (', round(nitrofurantoin$CovariateMean,1), '%)')
nitrofurantoin <- nitrofurantoin %>% select(covariateName, nitrofurantoin) 

nitrofurantoin <- rbind(n, outcome, age, nitrofurantoin)
######################

####tetracycline #####
studyPop <- readRDS(file = file.path(outputFolder, "SimpleABCtetracycline/StudyPop_L1_T2089_O2083_P1.rds"))
covariateSummary <- readRDS(file = file.path(outputFolder, "SimpleABCtetracycline/Analysis_1/plpResult/covariateSummary.rds"))
n <- data.frame(covariateName = "n", tetracycline = nrow(studyPop))
outcome <- data.frame(covariateName = "Resistance",
                      tetracycline = paste0(nrow(studyPop[studyPop$outcomeCount == 1,]),
                                          ' (',
                                          round(nrow(studyPop[studyPop$outcomeCount == 1,])/nrow(studyPop)*100,1),
                                          '%)'))
##Age
age <- studyPop %>% summarise(covariateName = "ageMean", meanAge = mean(ageYear), sd = sd(ageYear))
age$tetracycline <- paste0(round(age$meanAge,1), ' ± ', round(age$sd,1))
age <- age %>% select(covariateName, tetracycline)

#CovariateSummary 
tetracycline <- as.data.frame(covariateSummary %>% select(covariateName, CovariateCount, CovariateMean))
tetracycline$CovariateMean <- tetracycline$CovariateMean*100

tetracycline$tetracycline <- paste0(tetracycline$CovariateCount, ' (', round(tetracycline$CovariateMean,1), '%)')
tetracycline <- tetracycline %>% select(covariateName, tetracycline) 

tetracycline <- rbind(n, outcome, age, tetracycline)
######################

####tmpsmx #####
studyPop <- readRDS(file = file.path(outputFolder, "SimpleABCtmpsmx/StudyPop_L1_T1864_O1866_P1.rds"))
covariateSummary <- readRDS(file = file.path(outputFolder, "SimpleABCtmpsmx/Analysis_1/plpResult/covariateSummary.rds"))
n <- data.frame(covariateName = "n", tmpsmx = nrow(studyPop))
outcome <- data.frame(covariateName = "Resistance",
                      tmpsmx = paste0(nrow(studyPop[studyPop$outcomeCount == 1,]),
                                          ' (',
                                          round(nrow(studyPop[studyPop$outcomeCount == 1,])/nrow(studyPop)*100,1),
                                          '%)'))
##Age
age <- studyPop %>% summarise(covariateName = "ageMean", meanAge = mean(ageYear), sd = sd(ageYear))
age$tmpsmx <- paste0(round(age$meanAge,1), ' ± ', round(age$sd,1))
age <- age %>% select(covariateName, tmpsmx)

#CovariateSummary 
tmpsmx <- as.data.frame(covariateSummary %>% select(covariateName, CovariateCount, CovariateMean))
tmpsmx$CovariateMean <- tmpsmx$CovariateMean*100

tmpsmx$tmpsmx <- paste0(tmpsmx$CovariateCount, ' (', round(tmpsmx$CovariateMean,1), '%)')
tmpsmx <- tmpsmx %>% select(covariateName, tmpsmx) 

tmpsmx <- rbind(n, outcome, age, tmpsmx)
######################

#### TOTAL BASELINE #####

baseline <- merge(ampicillin, ceftriaxone, by = "covariateName", all = T)
baseline <- merge(baseline, ciprofloxacin, by = "covariateName", all = T)
baseline <- merge(baseline, fosfomycin, by = "covariateName", all = T)
baseline <- merge(baseline, gentamicin, by = "covariateName", all = T)
baseline <- merge(baseline, levofloxacin, by = "covariateName", all = T)
baseline <- merge(baseline, nitrofurantoin, by = "covariateName", all = T)
baseline <- merge(baseline, tetracycline, by = "covariateName", all = T)
baseline <- merge(baseline, tmpsmx, by = "covariateName", all = T)

write.csv(baseline, file = file.path(outputFolder, "totalBaseline.csv"))

##################################################


####################Prediction results############
performanceEvaluation1 <- readRDS(file = file.path(outputFolder, "SimpleABCampicillin/Analysis_1/plpResult/performanceEvaluation.rds"))
performanceEvaluation2 <- readRDS(file = file.path(outputFolder, "SimpleABCampicillin/Analysis_2/plpResult/performanceEvaluation.rds"))
performanceEvaluation3 <- readRDS(file = file.path(outputFolder, "SimpleABCampicillin/Analysis_3/plpResult/performanceEvaluation.rds"))
performanceEvaluation <- rbind(performanceEvaluation1$evaluationStatistics, performanceEvaluation2$evaluationStatistics, performanceEvaluation3$evaluationStatistics)
evalStatistics <- as.data.frame(performanceEvaluation)
ampicillin <- evalStatistics %>% filter(Eval == "test") %>% mutate(ampicillin = Value) %>% select (analysisId, Metric, ampicillin)

performanceEvaluation1 <- readRDS(file = file.path(outputFolder, "SimpleABCceftriaxone/Analysis_1/plpResult/performanceEvaluation.rds"))
performanceEvaluation2 <- readRDS(file = file.path(outputFolder, "SimpleABCceftriaxone/Analysis_2/plpResult/performanceEvaluation.rds"))
performanceEvaluation3 <- readRDS(file = file.path(outputFolder, "SimpleABCceftriaxone/Analysis_3/plpResult/performanceEvaluation.rds"))
performanceEvaluation <- rbind(performanceEvaluation1$evaluationStatistics, performanceEvaluation2$evaluationStatistics, performanceEvaluation3$evaluationStatistics)
evalStatistics <- as.data.frame(performanceEvaluation)
ceftriaxone <- evalStatistics %>% filter(Eval == "test") %>% mutate(ceftriaxone = Value) %>% select (analysisId, Metric, ceftriaxone)

performanceEvaluation1 <- readRDS(file = file.path(outputFolder, "SimpleABCciprofloxacin/Analysis_1/plpResult/performanceEvaluation.rds"))
performanceEvaluation2 <- readRDS(file = file.path(outputFolder, "SimpleABCciprofloxacin/Analysis_2/plpResult/performanceEvaluation.rds"))
performanceEvaluation3 <- readRDS(file = file.path(outputFolder, "SimpleABCciprofloxacin/Analysis_3/plpResult/performanceEvaluation.rds"))
performanceEvaluation <- rbind(performanceEvaluation1$evaluationStatistics, performanceEvaluation2$evaluationStatistics, performanceEvaluation3$evaluationStatistics)
evalStatistics <- as.data.frame(performanceEvaluation)
ciprofloxacin <- evalStatistics %>% filter(Eval == "test") %>% mutate(ciprofloxacin = Value) %>% select (analysisId, Metric, ciprofloxacin)

# performanceEvaluation <- readRDS(file = file.path(outputFolder, "SimpleABCfosfomycin/Analysis_1/plpResult/performanceEvaluation.rds"))
# evalStatistics <- as.data.frame(performanceEvaluation$evaluationStatistics)
# fosfomycin <- evalStatistics %>% filter(Eval == "test") %>% mutate(fosfomycin = Value) %>% select (analysisId, Metric, fosfomycin)

performanceEvaluation1 <- readRDS(file = file.path(outputFolder, "SimpleABCgentamicin/Analysis_1/plpResult/performanceEvaluation.rds"))
performanceEvaluation2 <- readRDS(file = file.path(outputFolder, "SimpleABCgentamicin/Analysis_2/plpResult/performanceEvaluation.rds"))
performanceEvaluation3 <- readRDS(file = file.path(outputFolder, "SimpleABCgentamicin/Analysis_3/plpResult/performanceEvaluation.rds"))
performanceEvaluation <- rbind(performanceEvaluation1$evaluationStatistics, performanceEvaluation2$evaluationStatistics, performanceEvaluation3$evaluationStatistics)
evalStatistics <- as.data.frame(performanceEvaluation)
gentamicin <- evalStatistics %>% filter(Eval == "test") %>% mutate(gentamicin = Value) %>% select (analysisId, Metric, gentamicin)

performanceEvaluation1 <- readRDS(file = file.path(outputFolder, "SimpleABClevofloxacin/Analysis_1/plpResult/performanceEvaluation.rds"))
performanceEvaluation2 <- readRDS(file = file.path(outputFolder, "SimpleABClevofloxacin/Analysis_2/plpResult/performanceEvaluation.rds"))
performanceEvaluation3 <- readRDS(file = file.path(outputFolder, "SimpleABClevofloxacin/Analysis_3/plpResult/performanceEvaluation.rds"))
performanceEvaluation <- rbind(performanceEvaluation1$evaluationStatistics, performanceEvaluation2$evaluationStatistics, performanceEvaluation3$evaluationStatistics)
evalStatistics <- as.data.frame(performanceEvaluation)
levofloxacin <- evalStatistics %>% filter(Eval == "test") %>% mutate(levofloxacin = Value) %>% select (analysisId, Metric, levofloxacin)

performanceEvaluation1 <- readRDS(file = file.path(outputFolder, "SimpleABCnitrofurantoin/Analysis_1/plpResult/performanceEvaluation.rds"))
performanceEvaluation2 <- readRDS(file = file.path(outputFolder, "SimpleABCnitrofurantoin/Analysis_2/plpResult/performanceEvaluation.rds"))
performanceEvaluation3 <- readRDS(file = file.path(outputFolder, "SimpleABCnitrofurantoin/Analysis_3/plpResult/performanceEvaluation.rds"))
performanceEvaluation <- rbind(performanceEvaluation1$evaluationStatistics, performanceEvaluation2$evaluationStatistics, performanceEvaluation3$evaluationStatistics)
evalStatistics <- as.data.frame(performanceEvaluation)
nitrofurantoin <- evalStatistics %>% filter(Eval == "test") %>% mutate(nitrofurantoin = Value) %>% select (analysisId, Metric, nitrofurantoin)

performanceEvaluation1 <- readRDS(file = file.path(outputFolder, "SimpleABCtetracycline/Analysis_1/plpResult/performanceEvaluation.rds"))
performanceEvaluation2 <- readRDS(file = file.path(outputFolder, "SimpleABCtetracycline/Analysis_2/plpResult/performanceEvaluation.rds"))
performanceEvaluation3 <- readRDS(file = file.path(outputFolder, "SimpleABCtetracycline/Analysis_3/plpResult/performanceEvaluation.rds"))
performanceEvaluation <- rbind(performanceEvaluation1$evaluationStatistics, performanceEvaluation2$evaluationStatistics, performanceEvaluation3$evaluationStatistics)
evalStatistics <- as.data.frame(performanceEvaluation)
tetracycline <- evalStatistics %>% filter(Eval == "test") %>% mutate(tetracycline = Value) %>% select (analysisId, Metric, tetracycline)

performanceEvaluation1 <- readRDS(file = file.path(outputFolder, "SimpleABCtmpsmx/Analysis_1/plpResult/performanceEvaluation.rds"))
performanceEvaluation2 <- readRDS(file = file.path(outputFolder, "SimpleABCtmpsmx/Analysis_2/plpResult/performanceEvaluation.rds"))
performanceEvaluation3 <- readRDS(file = file.path(outputFolder, "SimpleABCtmpsmx/Analysis_3/plpResult/performanceEvaluation.rds"))
performanceEvaluation <- rbind(performanceEvaluation1$evaluationStatistics, performanceEvaluation2$evaluationStatistics, performanceEvaluation3$evaluationStatistics)
evalStatistics <- as.data.frame(performanceEvaluation)
tmpsmx <- evalStatistics %>% filter(Eval == "test") %>% mutate(tmpsmx = Value) %>% select (analysisId, Metric, tmpsmx)

evalResults <- merge(ampicillin, ceftriaxone, by = c("analysisId","Metric"), all = T)
evalResults <- merge(evalResults, ciprofloxacin, by = c("analysisId","Metric"), all = T)
# evalResults <- merge(evalResults, fosfomycin, by = c("analysisId","Metric"), all = T)
evalResults <- merge(evalResults, gentamicin, by = c("analysisId","Metric"), all = T)
evalResults <- merge(evalResults, levofloxacin, by = c("analysisId","Metric"), all = T)
evalResults <- merge(evalResults, nitrofurantoin, by = c("analysisId","Metric"), all = T)
evalResults <- merge(evalResults, tetracycline, by = c("analysisId","Metric"), all = T)
evalResults <- merge(evalResults, tmpsmx, by = c("analysisId","Metric"), all = T)


library(ggplot2)
evalResults1 <- evalResults %>% filter(analysisId == "Analysis_1")
auc1 <- data.frame(analysisId = "Analysis_1",
                  Antibiotics = rownames(t(evalResults1)),
                  value =  t(evalResults1 %>% filter(Metric == "AUC.auc")),
                  lower = t(evalResults1 %>% filter(Metric == "AUC.auc_lb95ci")),
                  upper = t(evalResults1 %>% filter(Metric == "AUC.auc_ub95ci"))[,1])[-1:-2,]
evalResults2 <- evalResults %>% filter(analysisId == "Analysis_2")
auc2 <- data.frame(analysisId = "Analysis_2",
                   Antibiotics = rownames(t(evalResults2)),
                   value =  t(evalResults2 %>% filter(Metric == "AUC.auc")),
                   lower = t(evalResults2 %>% filter(Metric == "AUC.auc_lb95ci")),
                   upper = t(evalResults2 %>% filter(Metric == "AUC.auc_ub95ci"))[,1])[-1:-2,]
evalResults3 <- evalResults %>% filter(analysisId == "Analysis_3")
auc3 <- data.frame(analysisId = "Analysis_3",
                   Antibiotics = rownames(t(evalResults3)),
                   value =  t(evalResults3 %>% filter(Metric == "AUC.auc")),
                   lower = t(evalResults3 %>% filter(Metric == "AUC.auc_lb95ci")),
                   upper = t(evalResults3 %>% filter(Metric == "AUC.auc_ub95ci"))[,1])[-1:-2,]
auc <- rbind(auc1, auc2, auc3)


auc$value <- as.character(auc$value)
auc$value <- as.numeric(auc$value)
auc$lower <- as.character(auc$lower)
auc$lower <- as.numeric(auc$lower)
auc$upper <- as.character(auc$upper)
auc$upper <- as.numeric(auc$upper)
levels(auc$analysisId) <- c("Lasso LR", "Random Forest", "Xgboost")
levels(auc$Antibiotics) <- c("Ampicillin", "AnalysisId", "Ceftriaxone", "Ciprofloxacin",
                             "Gentamicin", "Levofloxacin", "Metric","Nitrofurantoin", "Tetracycline", "TMP/SMX")

tiff(filename = file.path(outputFolder, "auroc.tiff"), width= 20, height=15, units = 'cm', res = 400)
ggplot(data = auc, aes(x = Antibiotics, y = value, group = analysisId, color = analysisId)) +
  theme_classic()+
  labs(title=paste("Area under the reciever operating characteristic curve")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("AUROC") +
  xlab("Antibiotic susceptibility test") +
  labs(shape="ML algorithm", colour = "Algorithm") +
  theme(legend.position = c(.2, .2)) +
  scale_color_manual(values = c('#511281','#4ca1a3','#a5e1ad')) +
  geom_point(size = 2.5) + 
  geom_line(size = 1.2) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
  coord_cartesian(ylim = c(0:1))
dev.off()

####################################
############# AUPRC ##############
library(ggplot2)
evalResults1 <- evalResults %>% filter(analysisId == "Analysis_1")
auprc1 <- data.frame(analysisId = "Analysis_1",
                   Antibiotics = rownames(t(evalResults1)),
                   value =  t(evalResults1 %>% filter(Metric == "AUPRC"))[,1])[-1:-2,]
evalResults2 <- evalResults %>% filter(analysisId == "Analysis_2")
auprc2 <- data.frame(analysisId = "Analysis_2",
                   Antibiotics = rownames(t(evalResults2)),
                   value =  t(evalResults2 %>% filter(Metric == "AUPRC"))[,1])[-1:-2,]
evalResults3 <- evalResults %>% filter(analysisId == "Analysis_3")
auprc3 <- data.frame(analysisId = "Analysis_3",
                   Antibiotics = rownames(t(evalResults3)),
                   value =  t(evalResults3 %>% filter(Metric == "AUPRC"))[,1])[-1:-2,]
auprc <- rbind(auprc1, auprc2, auprc3)


auprc$value <- as.character(auprc$value)
auprc$value <- as.numeric(auprc$value)
levels(auprc$analysisId) <- c("Lasso LR", "Random Forest", "Xgboost")
levels(auprc$Antibiotics) <- c("Ampicillin", "AnalysisId", "Ceftriaxone", "Ciprofloxacin",
                             "Gentamicin", "Levofloxacin", "Metric","Nitrofurantoin", "Tetracycline", "TMP/SMX")

tiff(filename = file.path(outputFolder, "auprc.tiff"), width= 20, height=15, units = 'cm', res = 400)
ggplot(data = auprc, aes(x = Antibiotics, y = value, group = analysisId, color = analysisId)) +
  theme_classic()+
  labs(title=paste("Area under the precision-recall curve")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("AUPRC") +
  xlab("Antibiotic susceptibility test") +
  labs(shape="ML algorithm", colour = "Algorithm") +
  theme(legend.position = c(.2, .2)) +
  scale_color_manual(values = c('#511281','#4ca1a3','#a5e1ad')) +
  geom_point(size = 2.5) + 
  geom_line(size = 1.2) +
  coord_cartesian(ylim = c(0:1))
dev.off()

##################################
######Calibration #################

calibrationPlot <- function(name, performanceEvaluation1,performanceEvaluation2,performanceEvaluation3){
  calibrationStatistics <- rbind(performanceEvaluation1$calibrationSummary, performanceEvaluation2$calibrationSummary, performanceEvaluation3$calibrationSummary)
  calibrationStatistics <- as.data.frame(calibrationStatistics) %>% filter(Eval == "test")
  
  maxVal <- max(calibrationStatistics$averagePredictedProbability,calibrationStatistics$observedIncidence)
  model <- stats::lm(observedIncidence~averagePredictedProbability, data=calibrationStatistics)
  res <- model$coefficients
  names(res) <- c('Intercept','Gradient')
  
  # confidence int
  interceptConf <- stats::confint(model)[1,]
  gradientConf <- stats::confint(model)[2,]
  
  cis <- data.frame(lci = interceptConf[1]+seq(0,1,length.out = nrow(calibrationStatistics))*gradientConf[1],
                    uci = interceptConf[2]+seq(0,1,length.out = nrow(calibrationStatistics))*gradientConf[2],
                    x=seq(0,1,length.out = nrow(calibrationStatistics)))
  
  calibrationStatistics <- cbind(calibrationStatistics, cis)
  
  
  plot <-
    ggplot(data=calibrationStatistics, aes(x=averagePredictedProbability, y=observedIncidence)) +
    theme_classic() +
    labs(title=paste(name)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_ribbon(aes(ymin=lci,ymax=uci, x=x),fill="#687980", alpha=0.2) +
    geom_point(size=1, color='#511281') +
    coord_cartesian(ylim = c(0,1), xlim =c(0,1)) +
    geom_abline(intercept = 0, slope = 1, linetype = 2, size=1,
                show.legend = TRUE) +
    geom_abline(intercept = res['Intercept'], slope = res['Gradient'], linetype = 1,show.legend = TRUE, color='#21094e') +
    scale_x_continuous("Average Predicted Probability") +
    scale_y_continuous("Observed Fraction With Outcome")
  return(plot)
}

performanceEvaluation1 <- readRDS(file = file.path(outputFolder, "SimpleABCampicillin/Analysis_1/plpResult/performanceEvaluation.rds"))
performanceEvaluation2 <- readRDS(file = file.path(outputFolder, "SimpleABCampicillin/Analysis_2/plpResult/performanceEvaluation.rds"))
performanceEvaluation3 <- readRDS(file = file.path(outputFolder, "SimpleABCampicillin/Analysis_3/plpResult/performanceEvaluation.rds"))
Cal_amp <- calibrationPlot(name = "Ampicillin", performanceEvaluation1,performanceEvaluation2,performanceEvaluation3)

performanceEvaluation1 <- readRDS(file = file.path(outputFolder, "SimpleABCceftriaxone/Analysis_1/plpResult/performanceEvaluation.rds"))
performanceEvaluation2 <- readRDS(file = file.path(outputFolder, "SimpleABCceftriaxone/Analysis_2/plpResult/performanceEvaluation.rds"))
performanceEvaluation3 <- readRDS(file = file.path(outputFolder, "SimpleABCceftriaxone/Analysis_3/plpResult/performanceEvaluation.rds"))
Cal_cef <- calibrationPlot(name = "Ceftriaxone", performanceEvaluation1,performanceEvaluation2,performanceEvaluation3)


performanceEvaluation1 <- readRDS(file = file.path(outputFolder, "SimpleABCciprofloxacin/Analysis_1/plpResult/performanceEvaluation.rds"))
performanceEvaluation2 <- readRDS(file = file.path(outputFolder, "SimpleABCciprofloxacin/Analysis_2/plpResult/performanceEvaluation.rds"))
performanceEvaluation3 <- readRDS(file = file.path(outputFolder, "SimpleABCciprofloxacin/Analysis_3/plpResult/performanceEvaluation.rds"))
Cal_cip <- calibrationPlot(name = "Ciprofloxacin", performanceEvaluation1,performanceEvaluation2,performanceEvaluation3)


performanceEvaluation1 <- readRDS(file = file.path(outputFolder, "SimpleABCgentamicin/Analysis_1/plpResult/performanceEvaluation.rds"))
performanceEvaluation2 <- readRDS(file = file.path(outputFolder, "SimpleABCgentamicin/Analysis_2/plpResult/performanceEvaluation.rds"))
performanceEvaluation3 <- readRDS(file = file.path(outputFolder, "SimpleABCgentamicin/Analysis_3/plpResult/performanceEvaluation.rds"))
Cal_gen <- calibrationPlot(name = "Gentamicin", performanceEvaluation1,performanceEvaluation2,performanceEvaluation3)


performanceEvaluation1 <- readRDS(file = file.path(outputFolder, "SimpleABClevofloxacin/Analysis_1/plpResult/performanceEvaluation.rds"))
performanceEvaluation2 <- readRDS(file = file.path(outputFolder, "SimpleABClevofloxacin/Analysis_2/plpResult/performanceEvaluation.rds"))
performanceEvaluation3 <- readRDS(file = file.path(outputFolder, "SimpleABClevofloxacin/Analysis_3/plpResult/performanceEvaluation.rds"))
Cal_lev <- calibrationPlot(name = "Levofloxacin", performanceEvaluation1,performanceEvaluation2,performanceEvaluation3)


performanceEvaluation1 <- readRDS(file = file.path(outputFolder, "SimpleABCnitrofurantoin/Analysis_1/plpResult/performanceEvaluation.rds"))
performanceEvaluation2 <- readRDS(file = file.path(outputFolder, "SimpleABCnitrofurantoin/Analysis_2/plpResult/performanceEvaluation.rds"))
performanceEvaluation3 <- readRDS(file = file.path(outputFolder, "SimpleABCnitrofurantoin/Analysis_3/plpResult/performanceEvaluation.rds"))
Cal_Nit <- calibrationPlot(name = "Nitrofurantoin", performanceEvaluation1,performanceEvaluation2,performanceEvaluation3)


performanceEvaluation1 <- readRDS(file = file.path(outputFolder, "SimpleABCtetracycline/Analysis_1/plpResult/performanceEvaluation.rds"))
performanceEvaluation2 <- readRDS(file = file.path(outputFolder, "SimpleABCtetracycline/Analysis_2/plpResult/performanceEvaluation.rds"))
performanceEvaluation3 <- readRDS(file = file.path(outputFolder, "SimpleABCtetracycline/Analysis_3/plpResult/performanceEvaluation.rds"))
Cal_tet <- calibrationPlot(name = "Tetracycline", performanceEvaluation1,performanceEvaluation2,performanceEvaluation3)

performanceEvaluation1 <- readRDS(file = file.path(outputFolder, "SimpleABCtmpsmx/Analysis_1/plpResult/performanceEvaluation.rds"))
performanceEvaluation2 <- readRDS(file = file.path(outputFolder, "SimpleABCtmpsmx/Analysis_2/plpResult/performanceEvaluation.rds"))
performanceEvaluation3 <- readRDS(file = file.path(outputFolder, "SimpleABCtmpsmx/Analysis_3/plpResult/performanceEvaluation.rds"))
Cal_tmp <- calibrationPlot(name = "TMP/SMX", performanceEvaluation1,performanceEvaluation2,performanceEvaluation3)

library(ggpubr)
tiff(filename = file.path(outputFolder, "calibration.tiff"), width= 40, height=20, units = 'cm', res = 300)
ggarrange(Cal_amp, Cal_cef, Cal_cip, Cal_gen, Cal_lev, Cal_Nit, Cal_tet, Cal_tmp, ncol = 4, nrow = 2)
dev.off()


############################################
#########     Distribution    ##############

prediction1 <- readRDS(file = file.path(outputFolder, "SimpleABCampicillin/Analysis_1/plpResult/prediction.rds"))
prediction1$analysisId <- "Analysis_1"
prediction1 <- prediction1 %>% select(analysisId, value)
prediction2 <- readRDS(file = file.path(outputFolder, "SimpleABCampicillin/Analysis_2/plpResult/prediction.rds"))
prediction2$analysisId <- "Analysis_2"
prediction2 <- prediction2 %>% select(analysisId, value)
prediction3 <- readRDS(file = file.path(outputFolder, "SimpleABCampicillin/Analysis_3/plpResult/prediction.rds"))
prediction3$analysisId <- "Analysis_3"
prediction3 <- prediction3 %>% select(analysisId, value)
prediction <- rbind(prediction1, prediction2, prediction3)
prediction <- as.data.frame(prediction)

levels(prediction$analysisId) <- c("Lasso LR", "Random forest", "Xgboost")

Dist_ampicillin <- ggplot(data = prediction, aes(x  = value, group = analysisId, color = analysisId, fill = analysisId)) +
  theme_classic()+
  geom_density(alpha = 0.2) +
  scale_color_manual(labels = levels(prediction$analysisId), 
                     name = "ML algorithm", values = c('#511281','#4ca1a3','#a5e1ad')) +
  scale_fill_manual(labels = levels(prediction$analysisId),
                    name = "ML algorithm", values = c('#511281','#4ca1a3','#a5e1ad')) +
  labs(title=paste("Ampicillin")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(.8, .8)) +
  ylab("Density") +
  xlab("Predicted value")

prediction1 <- readRDS(file = file.path(outputFolder, "SimpleABCceftriaxone/Analysis_1/plpResult/prediction.rds"))
prediction1$analysisId <- "Analysis_1"
prediction1 <- prediction1 %>% select(analysisId, value)
prediction2 <- readRDS(file = file.path(outputFolder, "SimpleABCceftriaxone/Analysis_2/plpResult/prediction.rds"))
prediction2$analysisId <- "Analysis_2"
prediction2 <- prediction2 %>% select(analysisId, value)
prediction3 <- readRDS(file = file.path(outputFolder, "SimpleABCceftriaxone/Analysis_3/plpResult/prediction.rds"))
prediction3$analysisId <- "Analysis_3"
prediction3 <- prediction3 %>% select(analysisId, value)
prediction <- rbind(prediction1, prediction2, prediction3)
prediction <- as.data.frame(prediction)

levels(prediction$analysisId) <- c("Lasso LR", "Random forest", "Xgboost")

Dist_ceftriaxone <- ggplot(data = prediction, aes(x  = value, group = analysisId, color = analysisId, fill = analysisId)) +
  theme_classic()+
  geom_density(alpha = 0.2) +
  scale_color_manual(labels = levels(prediction$analysisId), 
                     name = "ML algorithm", values = c('#511281','#4ca1a3','#a5e1ad')) +
  scale_fill_manual(labels = levels(prediction$analysisId),
                    name = "ML algorithm", values = c('#511281','#4ca1a3','#a5e1ad')) +
  labs(title=paste("Ceftriaxone")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(.8, .8)) +
  ylab("Density") +
  xlab("Predicted value")

prediction1 <- readRDS(file = file.path(outputFolder, "SimpleABCciprofloxacin/Analysis_1/plpResult/prediction.rds"))
prediction1$analysisId <- "Analysis_1"
prediction1 <- prediction1 %>% select(analysisId, value)
prediction2 <- readRDS(file = file.path(outputFolder, "SimpleABCciprofloxacin/Analysis_2/plpResult/prediction.rds"))
prediction2$analysisId <- "Analysis_2"
prediction2 <- prediction2 %>% select(analysisId, value)
prediction3 <- readRDS(file = file.path(outputFolder, "SimpleABCciprofloxacin/Analysis_3/plpResult/prediction.rds"))
prediction3$analysisId <- "Analysis_3"
prediction3 <- prediction3 %>% select(analysisId, value)
prediction <- rbind(prediction1, prediction2, prediction3)
prediction <- as.data.frame(prediction)

levels(prediction$analysisId) <- c("Lasso LR", "Random forest", "Xgboost")

Dist_ciprofloxacin <- ggplot(data = prediction, aes(x  = value, group = analysisId, color = analysisId, fill = analysisId)) +
  theme_classic()+
  geom_density(alpha = 0.2) +
  scale_color_manual(labels = levels(prediction$analysisId), 
                     name = "ML algorithm", values = c('#511281','#4ca1a3','#a5e1ad')) +
  scale_fill_manual(labels = levels(prediction$analysisId),
                    name = "ML algorithm", values = c('#511281','#4ca1a3','#a5e1ad')) +
  labs(title=paste("Ciprofloxacin")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(.8, .8)) +
  ylab("Density") +
  xlab("Predicted value")

prediction1 <- readRDS(file = file.path(outputFolder, "SimpleABCgentamicin/Analysis_1/plpResult/prediction.rds"))
prediction1$analysisId <- "Analysis_1"
prediction1 <- prediction1 %>% select(analysisId, value)
prediction2 <- readRDS(file = file.path(outputFolder, "SimpleABCgentamicin/Analysis_2/plpResult/prediction.rds"))
prediction2$analysisId <- "Analysis_2"
prediction2 <- prediction2 %>% select(analysisId, value)
prediction3 <- readRDS(file = file.path(outputFolder, "SimpleABCgentamicin/Analysis_3/plpResult/prediction.rds"))
prediction3$analysisId <- "Analysis_3"
prediction3 <- prediction3 %>% select(analysisId, value)
prediction <- rbind(prediction1, prediction2, prediction3)
prediction <- as.data.frame(prediction)

levels(prediction$analysisId) <- c("Lasso LR", "Random forest", "Xgboost")

Dist_gentamicin <- ggplot(data = prediction, aes(x  = value, group = analysisId, color = analysisId, fill = analysisId)) +
  theme_classic()+
  geom_density(alpha = 0.2) +
  scale_color_manual(labels = levels(prediction$analysisId), 
                     name = "ML algorithm", values = c('#511281','#4ca1a3','#a5e1ad')) +
  scale_fill_manual(labels = levels(prediction$analysisId),
                    name = "ML algorithm", values = c('#511281','#4ca1a3','#a5e1ad')) +
  labs(title=paste("Gentamicin")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(.8, .8)) +
  ylab("Density") +
  xlab("Predicted value")

prediction1 <- readRDS(file = file.path(outputFolder, "SimpleABClevofloxacin/Analysis_1/plpResult/prediction.rds"))
prediction1$analysisId <- "Analysis_1"
prediction1 <- prediction1 %>% select(analysisId, value)
prediction2 <- readRDS(file = file.path(outputFolder, "SimpleABClevofloxacin/Analysis_2/plpResult/prediction.rds"))
prediction2$analysisId <- "Analysis_2"
prediction2 <- prediction2 %>% select(analysisId, value)
prediction3 <- readRDS(file = file.path(outputFolder, "SimpleABClevofloxacin/Analysis_3/plpResult/prediction.rds"))
prediction3$analysisId <- "Analysis_3"
prediction3 <- prediction3 %>% select(analysisId, value)
prediction <- rbind(prediction1, prediction2, prediction3)
prediction <- as.data.frame(prediction)

levels(prediction$analysisId) <- c("Lasso LR", "Random forest", "Xgboost")

Dist_levofloxacin <- ggplot(data = prediction, aes(x  = value, group = analysisId, color = analysisId, fill = analysisId)) +
  theme_classic()+
  geom_density(alpha = 0.2) +
  scale_color_manual(labels = levels(prediction$analysisId), 
                     name = "ML algorithm", values = c('#511281','#4ca1a3','#a5e1ad')) +
  scale_fill_manual(labels = levels(prediction$analysisId),
                    name = "ML algorithm", values = c('#511281','#4ca1a3','#a5e1ad')) +
  labs(title=paste("Levofloxacin")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(.8, .8)) +
  ylab("Density") +
  xlab("Predicted value")


prediction1 <- readRDS(file = file.path(outputFolder, "SimpleABCnitrofurantoin/Analysis_1/plpResult/prediction.rds"))
prediction1$analysisId <- "Analysis_1"
prediction1 <- prediction1 %>% select(analysisId, value)
prediction2 <- readRDS(file = file.path(outputFolder, "SimpleABCnitrofurantoin/Analysis_2/plpResult/prediction.rds"))
prediction2$analysisId <- "Analysis_2"
prediction2 <- prediction2 %>% select(analysisId, value)
prediction3 <- readRDS(file = file.path(outputFolder, "SimpleABCnitrofurantoin/Analysis_3/plpResult/prediction.rds"))
prediction3$analysisId <- "Analysis_3"
prediction3 <- prediction3 %>% select(analysisId, value)
prediction <- rbind(prediction1, prediction2, prediction3)
prediction <- as.data.frame(prediction)

levels(prediction$analysisId) <- c("Lasso LR", "Random forest", "Xgboost")

Dist_nitrofurantoin <- ggplot(data = prediction, aes(x  = value, group = analysisId, color = analysisId, fill = analysisId)) +
  theme_classic()+
  geom_density(alpha = 0.2) +
  scale_color_manual(labels = levels(prediction$analysisId), 
                     name = "ML algorithm", values = c('#511281','#4ca1a3','#a5e1ad')) +
  scale_fill_manual(labels = levels(prediction$analysisId),
                    name = "ML algorithm", values = c('#511281','#4ca1a3','#a5e1ad')) +
  labs(title=paste("Nitrofurantoin")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(.8, .8)) +
  ylab("Density") +
  xlab("Predicted value")

prediction1 <- readRDS(file = file.path(outputFolder, "SimpleABCtetracycline/Analysis_1/plpResult/prediction.rds"))
prediction1$analysisId <- "Analysis_1"
prediction1 <- prediction1 %>% select(analysisId, value)
prediction2 <- readRDS(file = file.path(outputFolder, "SimpleABCtetracycline/Analysis_2/plpResult/prediction.rds"))
prediction2$analysisId <- "Analysis_2"
prediction2 <- prediction2 %>% select(analysisId, value)
prediction3 <- readRDS(file = file.path(outputFolder, "SimpleABCtetracycline/Analysis_3/plpResult/prediction.rds"))
prediction3$analysisId <- "Analysis_3"
prediction3 <- prediction3 %>% select(analysisId, value)
prediction <- rbind(prediction1, prediction2, prediction3)
prediction <- as.data.frame(prediction)

levels(prediction$analysisId) <- c("Lasso LR", "Random forest", "Xgboost")

Dist_tetracycline <- ggplot(data = prediction, aes(x  = value, group = analysisId, color = analysisId, fill = analysisId)) +
  theme_classic()+
  geom_density(alpha = 0.2) +
  scale_color_manual(labels = levels(prediction$analysisId), 
                     name = "ML algorithm", values = c('#511281','#4ca1a3','#a5e1ad')) +
  scale_fill_manual(labels = levels(prediction$analysisId),
                    name = "ML algorithm", values = c('#511281','#4ca1a3','#a5e1ad')) +
  labs(title=paste("Tetracycline")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(.8, .8)) +
  ylab("Density") +
  xlab("Predicted value")


prediction1 <- readRDS(file = file.path(outputFolder, "SimpleABCtmpsmx/Analysis_1/plpResult/prediction.rds"))
prediction1$analysisId <- "Analysis_1"
prediction1 <- prediction1 %>% select(analysisId, value)
prediction2 <- readRDS(file = file.path(outputFolder, "SimpleABCtmpsmx/Analysis_2/plpResult/prediction.rds"))
prediction2$analysisId <- "Analysis_2"
prediction2 <- prediction2 %>% select(analysisId, value)
prediction3 <- readRDS(file = file.path(outputFolder, "SimpleABCtmpsmx/Analysis_3/plpResult/prediction.rds"))
prediction3$analysisId <- "Analysis_3"
prediction3 <- prediction3 %>% select(analysisId, value)
prediction <- rbind(prediction1, prediction2, prediction3)
prediction <- as.data.frame(prediction)

levels(prediction$analysisId) <- c("Lasso LR", "Random forest", "Xgboost")

Dist_tmpsmx <- ggplot(data = prediction, aes(x  = value, group = analysisId, color = analysisId, fill = analysisId)) +
  theme_classic()+
  geom_density(alpha = 0.2) +
  scale_color_manual(labels = levels(prediction$analysisId), 
                     name = "ML algorithm", values = c('#511281','#4ca1a3','#a5e1ad')) +
  scale_fill_manual(labels = levels(prediction$analysisId),
                    name = "ML algorithm", values = c('#511281','#4ca1a3','#a5e1ad')) +
  labs(title=paste("TMP/SMX")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(.8, .8)) +
  ylab("Density") +
  xlab("Predicted value")


library(ggpubr)
tiff(filename = file.path(outputFolder, "distribution.tiff"), width= 40, height=20, units = 'cm', res = 300)
ggarrange(Dist_ampicillin, Dist_ceftriaxone, Dist_ciprofloxacin, Dist_gentamicin, Dist_levofloxacin, Dist_nitrofurantoin, Dist_tetracycline, Dist_tmpsmx, ncol = 4, nrow = 2)
dev.off()


library(ggplot2)
ggplot(data = covariate_2, aes(x = COHORT_START_DATE, y = COVARIATE_VALUE)) +
  geom_line() +
  theme_classic()+
  labs(title=paste("Ampicillin resistance rate of E.coli")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Resistance (%)") +
  xlab("Date") +
  ylim(0,100)
  
  