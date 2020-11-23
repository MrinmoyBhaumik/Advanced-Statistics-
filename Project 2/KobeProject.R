library(readxl)
library(dplyr)
library(GGally)
library(MASS)
library(InformationValue)
library(regclass)
library(MLmetrics)
library(questionr)
Kobe <-read_excel("C:/Users/Mrinmoy/Documents/School/Applied Statistics/Project 2/project2KobeData.xlsx",sheet = 1)
colnames(Kobe)[colSums(is.na(Kobe)) > 0]
KobePred <-read_excel("C:/Users/Mrinmoy/Documents/School/Applied Statistics/Project 2/project2KobeData.xlsx",sheet = 2)

#Convert timestamp
Kobe$game_date <- do.call("c",lapply(Kobe$game_date, function(x) as.Date(x, origin = "1899-12-30")))
KobePred$game_date <- do.call("c",lapply(KobePred$game_date, function(x) as.Date(x, origin = "1899-12-30")))

#Changing Categorical Data to Ordinal
KobeTrain$shot_type <- factor (Kobe$shot_type, ordered = TRUE, 
                               levels = c("3PT Field Goal","2PT Field Goal"))

KobeTrain$shot_zone_range <- factor(Kobe$shot_zone_range, ordered = TRUE,
                                    levels = c("Back Court Shot","24+ ft.","16-24 ft."
                                               ,"8-16 ft.","Less Than 8 ft."))

KobeTest$shot_type <- factor (Kobe$shot_type, ordered = TRUE, 
                               levels = c("3PT Field Goal","2PT Field Goal"))

KobeTest$shot_zone_range <- factor(Kobe$shot_zone_range, ordered = TRUE,
                                    levels = c("Back Court Shot","24+ ft.","16-24 ft."))
 

KobePred$shot_type <- factor (KobePred$shot_type, ordered = TRUE, 
                               levels = c("3PT Field Goal","2PT Field Goal"))

KobePred$shot_zone_range <- factor(KobePred$shot_zone_range, ordered = TRUE,
                                    levels = c("Back Court Shot","24+ ft.","16-24 ft."
                                               ,"8-16 ft.","Less Than 8 ft."))
                                               

set.seed(3)
ind <- sample(seq(1,dim(Kobe)[1],1), round(.75 *dim(Kobe)[1]))
KobeTrain <- Kobe[ind,]
KobeTest <- Kobe[-ind,] 

KobeFinalTrain <-Kobe

#Remove ID columns
KobeTrain$game_event_id <- NULL
KobeTrain$game_id <- NULL
KobeTrain$recId <- NULL
KobeTrain$shot_id <- NULL
KobeTrain$team_id <- NULL
KobeTrain$team_name<-NULL

KobeFinalTrain$game_event_id <- NULL
KobeFinalTrain$game_id <- NULL
KobeFinalTrain$recId <- NULL
KobeFinalTrain$shot_id <- NULL
KobeFinalTrain$team_id <- NULL
KobeFinalTrain$team_name<-NULL

KobeTest$game_event_id <- NULL
KobeTest$game_id <- NULL
KobeTest$recId <- NULL
KobeTest$shot_id <- NULL
KobeTest$team_id <- NULL
KobeTest$team_name<-NULL


#Multi-Colinearity
Kobe %>% select(arena_temp,attendance,avgnoisedb,lat,loc_x,loc_y,lon,minutes_remaining,seconds_remaining,shot_distance) %>% ggpairs(diag = list(discrete = "barDiag"))

#Log Transformation
KobeLog <- Kobe
KobeLog$logShot_distance <- log(Kobe$shot_distance)
KobeLog$logLat <- log(KobeLog$lat)
KobeLog$logLoc_y <- log(KobeLog$loc_y)
KobeLog$logShot_distance <- log(KobeLog$shot_distance)

KobeLog %>% dplyr::select(arena_temp,attendance,avgnoisedb,logLat,loc_x,logLoc_y,lon,minutes_remaining,seconds_remaining,logShot_distance) %>% ggpairs(diag = list(discrete = "barDiag"))




#Remove loc_x and loc_y
KobeTrain1 <- KobeTrain
KobeTest1 <- KobeTest
KobeTrain1$loc_x <- NULL
KobeTrain1$loc_y <- NULL
KobeTest1$loc_x <- NULL
KobeTest1$loc_y <- NULL


#Remove lon and lat
KobeTrain2 <- KobeTrain
KobeTest2 <- KobeTest
KobeTrain2$lon <- NULL
KobeTrain2$lat <- NULL
KobeTest2$lon <- NULL
KobeTest2$lat <- NULL
KobeFinalTrain$lon <- NULL
KobeFinalTrain$lat <- NULL


#Log transformation
KobeTrain3 <- KobeTrain
KobeTest3 <- KobeTest
KobeTrain3$logShot_distance <- log(KobeTrain3$shot_distance)
KobeTest3$logShot_distance <- log(KobeTest3$shot_distance)
KobeTrain3$loc_x <- NULL
KobeTrain3$loc_y <- NULL
KobeTest3$loc_x <- NULL
KobeTest3$loc_y <- NULL

#Step-Wise Models
model1 <- glm(shot_made_flag ~.,data= KobeTrain1,family = binomial) %>% stepAIC(trace = FALSE)

model2 <- glm(shot_made_flag ~.,data= KobeTrain2,family = binomial) %>% stepAIC(trace = FALSE)

newModel2 <- glm(shot_made_flag ~.,data= KobeFinalTrain,family = binomial) %>% stepAIC(trace = FALSE)


orgModel <- glm(shot_made_flag ~.,data= KobeTrain,family = binomial) %>% stepAIC(trace = FALSE)

model3 <- glm(shot_made_flag ~.,data= KobeTrain3,family = binomial) %>% stepAIC(trace = FALSE)

prediction1 <- predict(model1,KobeTest1,type = "response")
optCutOff1 <- optimalCutoff(KobeTest1$shot_made_flag, prediction1)[1]
misClassError(KobeTest1$shot_made_flag, prediction1, threshold = optCutOff1)
plotROC(KobeTest1$shot_made_flag, prediction1)
sensitivity(KobeTest1$shot_made_flag, prediction1, threshold = optCutOff1)
specificity(KobeTest1$shot_made_flag, prediction1, threshold = optCutOff1)
LogLoss(prediction1,KobeTest1$shot_made_flag)

prediction2 <- predict(model2,KobeTest2,type = "response")
optCutOff2 <- optimalCutoff(KobeTest2$shot_made_flag, prediction2)[1]
misClassError(KobeTest2$shot_made_flag, prediction2, threshold = optCutOff2)
plotROC(KobeTest2$shot_made_flag, prediction2)
sensitivity(KobeTest2$shot_made_flag, prediction2, threshold = optCutOff2)
specificity(KobeTest2$shot_made_flag, prediction2, threshold = optCutOff2)
LogLoss(prediction2,KobeTest2$shot_made_flag)

testPrediction <- predict(testModel,KobeTest2,type = "response")
optCutOffTest <- optimalCutoff(KobeTest2$shot_made_flag, testPrediction)[1]
misClassError(KobeTest2$shot_made_flag, testPrediction, threshold = optCutOffTest)
plotROC(KobeTest2$shot_made_flag, testPrediction)
sensitivity(KobeTest2$shot_made_flag, testPrediction, threshold = optCutOffTest)
specificity(KobeTest2$shot_made_flag, testPrediction, threshold = optCutOffTest)
LogLoss(testPrediction,KobeTest2$shot_made_flag)


predictionOrg <- predict(orgModel,KobeTest,type = "response")
optCutOffOrg <- optimalCutoff(KobeTest$shot_made_flag, predictionOrg)[1]
misClassError(KobeTest$shot_made_flag, predictionOrg, threshold = optCutOffOrg)
plotROC(KobeTest$shot_made_flag, predictionOrg)
sensitivity(KobeTest$shot_made_flag, predictionOrg, threshold = optCutOffOrg)
specificity(KobeTest$shot_made_flag, predictionOrg, threshold = optCutOffOrg)
LogLoss(predictionOrg,KobeTest$shot_made_flag)

#No Step-wise
model4 <- glm(shot_made_flag ~ action_type + opponent + arena_temp + attendance 
              + avgnoisedb + minutes_remaining + period + playoffs + seconds_remaining,data= KobeTrain,family = binomial)

prediction4 <- predict(model4,KobeTest,type = "response")
optCutOff4 <- optimalCutoff(KobeTest$shot_made_flag, prediction4)[1]
misClassError(KobeTest$shot_made_flag, prediction4, threshold = optCutOff4)
plotROC(KobeTest$shot_made_flag, prediction4)
sensitivity(KobeTest$shot_made_flag, prediction4, threshold = optCutOff4)
specificity(KobeTest$shot_made_flag, prediction4, threshold = optCutOff4)
LogLoss(predictionOrg,KobeTest$shot_made_flag)

#Odds Ratio Objective Model
oddsModel <- glm(shot_made_flag~shot_distance, data = KobeTest, family = binomial)
questionr::odds.ratio(oddsModel, level = .95)

#Write Prediction file
finalPrediction <- predict(model2, KobePred, type = "response")
finalPredictionValue <- sapply(finalPrediction, function(x) if (x > optCutOff2){1} else {0})

TotalPrediction <- data.frame(finalPrediction,finalPredictionValue)
write.csv(TotalPrediction,file = "C:/Users/Mrinmoy/Documents/School/Applied Statistics/Project 2/PredictionData.csv")
