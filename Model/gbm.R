library(gbm)
library(data.table)

all_data = cbind(clean, imp[,-1])
all_data$logerror_q3 = NULL

# convert logical to factor
log_cols = colnames(all_data)[sapply(all_data, class) == "logical"]
for (log_col in log_cols) {
  all_data[[log_col]] = as.factor(all_data[[log_col]])
}

col_names = colnames(all_data)
rm_names = c("id_parcel", "fips_blockid", "date")

cn = col_names[(!(col_names %in% rm_names))]

# train / test data split
set.seed(2344)
train_inx = sample(1:nrow(all_data), nrow(all_data)*0.7)
train_data = all_data[train_inx, cn, with=F]
test_data = all_data[-train_inx, cn, with=F]

# set.seed(123)
# train_inx <- createDataPartition(all_data$logerror, 
#                                   p = .75, 
#                                   list = FALSE, 
#                                   times = 1)
# ## training set
# train_data <- train_data[ train_inx,-1]
# ## testing set
# test_data  <- train_data[-train_inx,-1]

NUM_TREES = 800
INTERACTION_DEPTH = 20

fit.gbm = gbm(logerror ~ ., 
              data=train_data, 
              distribution = "laplace", 
              n.trees = NUM_TREES, 
              shrinkage = 0.01,
              interaction.depth = INTERACTION_DEPTH, 
              n.minobsinnode = 100,
              #cv.folds = 10,
              n.cores = 7,
              verbose=T)

# laplace: 0.03211207



ntree = c(seq(5, NUM_TREES, 1), NUM_TREES)
err = numeric(length(ntree))

for(i in (1:length(ntree))) {
  pred = predict(fit.gbm, test_data, ntree[i])
  err[i] = median(abs(test_data$logerror - pred))
}
plot(ntree, err)

bestTreeNr = ntree[which(err == min(err))[1]]

pred = predict(fit.gbm, test_data, bestTreeNr)

err = test_data$logerror - pred
cat("median abs err: ", median(abs(err)), "\n")
cat("baseline median abs err", median(abs(mean(all_data$logerror)-test_data$logerror)), "\n")



####################################################################################################
# create submit


newdata = cbind(clean_prop, imp_prop[,-1])
newdata$logerror_q3 = NULL

newdata$parcelid = newdata$id_parcel


months = c(10, 11, 12, 10, 11, 12)
labels = c("201610", "201611", "201612", "201710", "201711", "201712")

predictions <- newdata[, "parcelid", drop=FALSE]
for(i in 1:length(months)) {
  cat("month: ", months[i], "\n")
  newdata$month <- months[i]
  newdata$month_factor = factor(newdata$month, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
  #newdata$month_factor = as.factor(newdata$month)
  predictions[, labels[i]] <- predict(fit.gbm, newdata, bestTreeNr) 
}

write.csv(x = predictions, file = "submission_gbm.csv", 
          quote = FALSE, row.names = FALSE)


