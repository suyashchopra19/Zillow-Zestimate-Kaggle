##############load library###############
library(caret)
library(randomForest)
library(dplyr)
train = readRDS("~/Zillow-Price/group_data/TrainClned.rds")
fullset = readRDS("~/Zillow-Price/group_data/PropertiesClned.rds")


#############remove year, region_zip column#####################
train = train[, -which(names(train) %in% c("year", "region_zip"))]

############ add feature: price per area ###############
train = train %>% dplyr::mutate(tax_area = tax_total/area_total_calc)


############## split data #############################
## Data splitting based on the outcome
set.seed(123)
trainIndex <- createDataPartition(train$logerror, 
                                  p = 0.5, 
                                  list = FALSE, 
                                  times = 1)
## training set
subTrain <- train[ trainIndex,-1]
## testing set
subTest  <- train[-trainIndex,-1]

## define metric - MAE
maeSummary <- function(data, lev = NULL, model = NULL) {
  mae_score <- sum(abs(data$obs - data$pred)) / nrow(data)
  names(mae_score) <- "MAE"
  mae_score
}

################## grid search ##########################
gridSearch <- trainControl(method = "cv",
                           number = 3,
                           summaryFunction = maeSummary,
                           verboseIter = TRUE)

rfGrid <-  expand.grid(mtry = 2:7)

set.seed(8)
rf.logerror <- train(logerror ~ .,
                     data = subTrain, 
                     method = "rf", 
                     metric = "MAE",
                     maximize = FALSE,
                     tuneGrid = rfGrid,
                     trControl = gridSearch,
                     importance = TRUE)

# Warning message:
#   In train.default(x, y, weights = w, ...) :
#   The metric "MAE" was not in the result set. RMSE will be used instead.


rf.logerror
# mtry  MAE       
# 2     0.05761628
# 3     0.05750186
# 4     0.05747278
# 5     0.05754893
# 6     0.05768034
# 7     0.05783864


plot(rf.logerror)
rf.logerror$bestTune
#   mtry
# 4    5

########## p = 0.5 ####
#   mtry
# 3    4

gbmImp <- varImp(rf.logerror, scale = FALSE)
plot(gbmImp, top = 20)

results <- data.frame(obs = subTest$logerror, 
                      pred = predict(rf.logerror, newdata = subTest))
maeSummary(results)
#       MAE 
# 0.05722843 

########## p = 0.5 ####
#       MAE 
# 0.05733978 

cor(results)
#           obs      pred
# obs  1.0000000 0.1264429
# pred 0.1264429 1.0000000

########## p = 0.5 ####
#             obs       pred
# obs  1.00000000 0.09878084
# pred 0.09878084 1.00000000

saveRDS(rf.logerror, "rf_logerror_mtrycv_nozip_1.rds")


########### manually tuning number of trees #####################
control <- trainControl(method="cv", 
                        number=3,
                        summaryFunction = maeSummary,
                        verboseIter = TRUE)
tunegrid <- expand.grid(mtry=4)
modellist <- list()
for (ntree in c(50, 100, 250, 500, 1000, 1500)) {
  set.seed(8)
  fit <- train(logerror~., 
               data=subTrain, 
               method="rf", 
               metric="MAE", 
               tuneGrid=tunegrid, 
               trControl=control, 
               ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}

# compare results
result_ntree <- resamples(modellist)
summary(result_ntree)
# summary.resamples(object = result_ntree)
# Models: 100, 250, 500, 1000, 1500 
# Number of resamples: 3 
# 
# MAE 
#            Min.    1st Qu.     Median       Mean    3rd Qu.       Max. NA's
# 50   0.05698184 0.05737217 0.05776250 0.05770776 0.05807071 0.05837892    0
# 100  0.05686142 0.05724621 0.05763100 0.05757854 0.05793710 0.05824319    0
# 250  0.05676963 0.05717059 0.05757154 0.05749845 0.05786286 0.05815417    0
# 500  0.05673381 0.05714623 0.05755866 0.05748335 0.05785812 0.05815758    0
# 1000 0.05672166 0.05713677 0.05755188 0.05747443 0.05785081 0.05814973    0
# 1500 0.05671044 0.05712165 0.05753286 0.05746732 0.05784575 0.05815865    0

dotplot(result_ntree)

##### ntree = 500 seems to be good, but choose 1500 for best score


################ Fitting all training set with Best parameters #############

rf.logerror$bestTune

fitBestModel <- trainControl(method = "none",
                             summaryFunction = maeSummary)
set.seed(8)
rfFit <- train(logerror ~ .,
               data = train[,-1], 
               method = "rf", 
               metric = "MAE",
               maximize = FALSE,
               trControl = fitBestModel,
               tuneGrid = rf.logerror$bestTune,
               verbose = TRUE,
               ntree = 800,
               importance = TRUE)

results1 <- data.frame(obs = train$logerror, 
                       pred = predict(rfFit, newdata = train))
maeSummary(results1)
#       MAE 
# 0.05058911 

#       MAE 
# 0.05393332 

#       MAE 
# 0.05399062 

saveRDS(rfFit, "rf.rds")



########################### Making prediction for submission
test_data <- fullset %>% 
  select(intersect(names(fullset), names(train)))
test_data = test_data %>% dplyr::mutate(tax_area = tax_total/area_total_calc)
test_data <- test_data %>% rename( parcelid = id_parcel)


makePrediction <- function(model, newdata, months, labels) {
  predictions <- newdata[, "parcelid", drop=FALSE]
  for(i in 1:length(months)) {
    newdata$month <- months[i]
    predictions[, labels[i]] <- predict(model, newdata = newdata)
  }
  write.csv(x = predictions, file = "submission.csv", 
            quote = FALSE, row.names = FALSE)
  return(predictions)
}

makePrediction(rfFit, newdata = test_data, months = c(10, 11, 12, 22, 23, 24), 
               labels = c("201610", "201611", "201612", "201710", "201711", "201712"))
