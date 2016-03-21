# Read data files
dataTst <- read.csv("./pml-testing.csv", na.strings = c("NA", "", "#DIV/0!"))
dataTrn <- read.csv("./pml-training.csv", na.strings = c("NA", "", "#DIV/0!"))

#####
# Exploratory
#####
#names(dataTrn)
summary(dataTrn$classe)
#A    B    C    D    E 
#5580 3797 3422 3216 3607
#Class A corresponds to the specified execution of the exercise, while the other 4 classes correspond to common mistakes

summary(dataTrn$user_name)
#adelmo carlitos  charles   eurico   jeremy    pedro 
#3892     3112     3536     3070     3402     2610
summary(dataTrn$cvtd_timestamp)

library(dplyr)
length(names(select(dataTrn, contains("_belt"))))
length(names(select(dataTrn, contains("_arm"))))
length(names(select(dataTrn, contains("_dumbbell"))))
length(names(select(dataTrn, contains("_forearm"))))
# Each have 38  38 * 4 = 152
# Others (7): X, user_name, time(3), window(2)
# classe

#####
# Data Cleaning
#####
# Remove variables which are metadata, eg. ids, names, times E.g. cols 1-7
dataMeta <- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window")
dataTrn.noMeta <- dataTrn %>% select(-one_of(dataMeta))
dataTst.noMeta <- dataTst %>% select(-one_of(dataMeta))
remove(dataMeta)

# Some variables have NAs or '#DIV/0!' or '' which were converted to NA when the CSV was read
dataTrn.noMeta.NoNA <- dataTrn.noMeta[,colSums(is.na(dataTrn.noMeta)) == 0]
dataTst.noMeta.NoNA <- dataTst.noMeta[,colSums(is.na(dataTst.noMeta)) == 0]

# Classify the classe variable into two cases
#dataTrn.noMeta$classe <- as.factor(ifelse (dataTrn.noMeta$classe == "A", "Good", "Bad"))

#####
# Data Splitting
#####
library(caret)
set.seed(1958) 
inTrain <- createDataPartition(dataTrn.noMeta.NoNA$classe, p = 0.7, list = FALSE)
dataTrn.train <- dataTrn.noMeta.NoNA[inTrain, ]
dataTrn.valid <- dataTrn.noMeta.NoNA[-inTrain, ]
remove(inTrain)


#####
# Classification Tree
#####
control <- trainControl(method = "cv", number = 5)
fit_rpart <- train(classe ~ ., data = dataTrn.train, method = "rpart", trControl = control)
print(fit_rpart, digits = 4)

library(rattle)
fancyRpartPlot(fit_rpart$finalModel)

# predict outcomes using validation set
predict_rpart <- predict(fit_rpart, dataTrn.valid)
# Show prediction result
conf_rpart <- confusionMatrix(dataTrn.valid$classe, predict_rpart)
accuracy_rpart <- conf_rpart$overall["Accuracy"]

conf_rpart
accuracy_rpart

#####
# Random Forest
#####
fit_rf <- train(classe ~ ., data = dataTrn.train, method = "rf", trControl = control)
print(fit_rf, digits = 4)

# predict outcomes using validation set
predict_rf <- predict(fit_rf, dataTrn.valid)
# Show prediction result
conf_rf <- confusionMatrix(dataTrn.valid$classe, predict_rf)
accuracy_rf <- conf_rf$overall["Accuracy"]

conf_rf
accuracy_rf

#####
# Prediction on Testing Set

dataTst.noMeta.NoNA.predict <- predict(fit_rf, dataTst.noMeta.NoNA)
dataTst.noMeta.NoNA.predict
#####
