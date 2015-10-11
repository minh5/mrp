#Loading libraries
setwd("~/mrp/")
library(caret)

load("data.RData")

#####pre-proces#
#break up ages and education
ageGroups <- as.data.frame(cut(tmpData$demAgeFull, breaks = c(0,18,35,55,Inf), labels = c("under 18","18-35","35-55", "55+")))
names(ageGroups) <- "ageGroups"
eduGroups <- as.data.frame(cut(tmpData$demEduFull, breaks = c(0,3,7,9), labels = c("Up to HS", "Up to College", "PostGraduate")))
names(eduGroups) <- "eduGroups"

#cleaning up dataset
dataPrep <- tmpData[,c("nr1","demGender","demRace","demHisp")]
dataPrep <- cbind(dataPrep, ageGroups, eduGroups) 

#explicitly typecast numeric into factor variables
names(dataPrep)
for (i in 2:5){
  dataPrep[,i] <- as.factor(dataPrep[,i])
}

#dummying variables
dummies <- dummyVars(nr1 ~ ., data= dataPrep)
data <- as.data.frame(predict(dummies, newdata = dataPrep))
data <- cbind(data, tmpData$demState)
names(data) <- c("male","female","native","asian","black","white","other.race","hisp","notHisp",
                  "under18","age18.35","age35.55","age55plus", "hs","college","postgraduate","demState")

#splitting regions according to census - since dummy all 50 states would make the data too wide for prediction
#http://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf
data$newengland <- ifelse(data$demState %in% c(7, 20, 22, 30, 40, 46), 1,0)
data$midatlantic <- ifelse(data$demState %in% c(31, 33, 39),1,0)
data$eastcentral <- ifelse(data$demState %in% c(16, 14, 23, 36, 50),1,0)
data$westcentral <- ifelse(data$demState %in% c(16, 17, 24, 26, 28, 35, 42),1,0)
data$southatlantic <- ifelse(data$demState %in% c(8, 9, 10, 11, 21, 34, 41, 47, 49),1,0)
data$southeast <- ifelse(data$demState %in% c(1,18, 25, 43), 1, 0)
data$southwest <- ifelse(data$demState %in% c(2, 19, 37, 44), 1, 0)
data$moutain <-  ifelse(data$demState %in% c(3, 6, 13, 32, 27,45, 51), 1, 0)
data$pacific <- ifelse(data$demState %in% c(2,5, 12, 38, 48), 1, 0)
data <- data[,-grep("^demState",names(data))]

#####first model - country right/wrong#####
#creating columns where we are trying to measure predict
dv <- as.data.frame(ifelse(tmpData$nr1 == 1, 1, 0))
names(dv) <- "dv"

data <- cbind(dv, data)
data$dv <- as.factor(data$dv)
levels(data$dv) <- c("No", "Yes")

#create training and test set
set.seed(938479382)
split <- createDataPartition(data$dv, p=.8)[[1]]
train<-data[split,]
test<-data[-split,]


#creating controller for training
control <- trainControl(method = "cv", 
                        number = 10, 
                        savePredictions = TRUE, 
                        classProbs = TRUE, 
                        summaryFunction = twoClassSummary)

#creating results table
results <- as.data.frame(test$dv)
names(results)[1] <- 'obs'

#Running random forest
require(randomForest)
tuningGrid <- data.frame(.mtry = floor(seq(1 , ncol(train) , length = 6)))
rf_tune1 <- train(dv ~ . , 
                 data=train, 
                 method = "rf" ,
                 metric = "ROC",
                 tuneGrid = tuningGrid,
                 trControl = control)
rf_tune1
results$rf <- predict.train(rf_tune1, newdata=test, type = 'prob')

#Good ol' logistic regression
logit_tune1 <- train(dv ~.,
                   method = 'glm',
                    data = train,
                    trControl = control)
logit_tune1
results$logit <- predict(logit_tune1, newdata = test, type = 'prob')

#Classification and Regression Tree, because why not.
require(rpart)
cart_tune1 = train(dv ~ . , 
                  data = train ,
                  method = "rpart" ,
                  metric = "ROC" ,
                  tuneLength = 25 ,
                  trControl = control)
cart_tune1
results$cart <- predict(cart_tune1, newdata = test, type = 'prob')

#Alot of talk has been on the accuracy of neural network algorithm, having a go at it here.
require(nnet)
tuningGrid<-expand.grid(.size = 1:5, .decay = c(0, 2, by = .5))
nnet_tune1 <- train(dv ~ .,
                   data = train,
                   method = "nnet",
                   metric = "ROC",
                   tuneGrid = tuningGrid,
                   trControl = control)
nnet_tune1
results$nnet <- predict(nnet_tune1, newdata = test, type = 'prob')


eval <- as.data.frame(results$obs)
for (i in 2:ncol(results)){
  sub <- as.data.frame(results[,i][2])
  eval <- cbind(eval,sub)
}
eval <- as.data.frame(eval)
names(eval) <- names(results)
eval$obs <- ifelse(eval$obs == 'Yes', 1, 0)

#model evaluation
require(scoring)

mean(brierscore(obs ~ rf, data = eval))
mean(brierscore(obs ~ logit, data = eval))
mean(brierscore(obs ~ cart, data = eval))
mean(brierscore(obs ~ nnet, data = eval)) #winner

# applying models
predictdf <- data
predictdf$predictionRight <- predict(nnet_tune1, newdata = predictdf, type = 'prob')[2]

######second - obama job approval#####
#creating columns where we are trying to measure predict
data$dv <- ifelse(tmpData$nr2 <= 2, 1, 0)
data$dv <- as.factor(data$dv)
levels(data$dv) <- c("No", "Yes")

#create training and test set
set.seed(938479382)
split <- createDataPartition(data$dv, p=.8)[[1]]
train<-data[split,]
test<-data[-split,]


#creating results table
results <- as.data.frame(test$dv)
names(results)[1] <- 'obs'

# random forest
require(randomForest)
tuningGrid <- data.frame(.mtry = floor(seq(1 , ncol(train) , length = 6)))
rf_tune2 <- train(dv ~ . , 
                 data=train, 
                 method = "rf" ,
                 metric = "ROC",
                 tuneGrid = tuningGrid,
                 trControl = control)
rf_tune2
results$rf <- predict.train(rf_tune2, newdata=test, type = 'prob')

#logistic regression
logit_tune2 <- train(dv ~.,
                    method = 'glm',
                    data = train,
                    trControl = control)
logit_tune2
results$logit <- predict(logit_tune2, newdata = test, type = 'prob')

#Classification and Regression Tree
require(rpart)
cart_tune2 = train(dv ~ . , 
                  data = train ,
                  method = "rpart" ,
                  metric = "ROC" ,
                  tuneLength = 25 ,
                  trControl = control)
cart_tune2
results$cart <- predict(cart_tune2, newdata = test, type = 'prob')

#neural network
require(nnet)
tuningGrid<-expand.grid(.size = 1:5, .decay = c(0, 2, by = .5))
nnet_tune2 <- train(dv ~ .,
                   data = train,
                   method = "nnet",
                   metric = "ROC",
                   tuneGrid = tuningGrid,
                   trControl = control)
nnet_tune2
results$nnet <- predict(nnet_tune2, newdata = test, type = 'prob')

#evaulation
eval <- as.data.frame(results$obs)
for (i in 2:ncol(results)){
  sub <- as.data.frame(results[,i][2])
  eval <- cbind(eval,sub)
}
eval <- as.data.frame(eval)
names(eval) <- names(results)
eval$obs <- ifelse(eval$obs == 'Yes', 1, 0)

#model evaluation
require(scoring)

mean(brierscore(obs ~ rf, data = eval))
mean(brierscore(obs ~ logit, data = eval))
mean(brierscore(obs ~ cart, data = eval))
mean(brierscore(obs ~ nnet, data = eval)) #winner

# applying models
predictdf$predictionApprove <- predict(nnet_tune2, newdata = predictdf, type = 'prob')[2]


#####third model - economy as top priority#####
#creating columns where we are trying to measure predict
data$dv <- ifelse(tmpData$nr3 == 1, 1, 0)
data$dv <- as.factor(data$dv)
levels(data$dv) <- c("No", "Yes")

#create training and test set
set.seed(938479382)
split <- createDataPartition(data$dv, p=.8)[[1]]
train<-data[split,]
test<-data[-split,]

#creating results table
results <- as.data.frame(test$dv)
names(results)[1] <- 'obs'

# random forest
require(randomForest)
tuningGrid <- data.frame(.mtry = floor(seq(1 , ncol(train) , length = 6)))
rf_tune3 <- train(dv ~ . , 
                 data=train, 
                 method = "rf" ,
                 metric = "ROC",
                 tuneGrid = tuningGrid,
                 trControl = control)
rf_tune3
results$rf <- predict.train(rf_tune3, newdata=test, type = 'prob')

#logistic regression
logit_tune3 <- train(dv ~.,
                    method = 'glm',
                    data = train,
                    trControl = control)
logit_tune3
results$logit <- predict(logit_tune3, newdata = test, type = 'prob')

#Classification and Regression Tree
require(rpart)
cart_tune3 = train(dv ~ . , 
                  data = train ,
                  method = "rpart" ,
                  metric = "ROC" ,
                  tuneLength = 25 ,
                  trControl = control)
cart_tune3
results$cart <- predict(cart_tune3, newdata = test, type = 'prob')

#neural network
require(nnet)
tuningGrid<-expand.grid(.size = 1:5, .decay = c(0, 2, by = .5))
nnet_tune3 <- train(dv ~ .,
                   data = train,
                   method = "nnet",
                   metric = "ROC",
                   tuneGrid = tuningGrid,
                   trControl = control)
nnet_tune3
results$nnet <- predict(nnet_tune3, newdata = test, type = 'prob')

#evaulation
eval <- as.data.frame(results$obs)
for (i in 2:ncol(results)){
  sub <- as.data.frame(results[,i][2])
  eval <- cbind(eval,sub)
}
eval <- as.data.frame(eval)
names(eval) <- names(results)
eval$obs <- ifelse(eval$obs == 'Yes', 1, 0)

#model evaluation
require(scoring)

mean(brierscore(obs ~ rf, data = eval)) 
mean(brierscore(obs ~ logit, data = eval)) 
mean(brierscore(obs ~ cart, data = eval))
mean(brierscore(obs ~ nnet, data = eval)) #winner 

# applying models
predictdf$predictionEconomy <- predict(nnet_tune3, newdata = predictdf, type = 'prob')[2]

#####post-stratification
#combining individual level census data from https://www.kaggle.com/c/2013-american-community-survey/data
census <- rbind(data.table::fread('census/ss13pusa.csv', select = c('ST','SEX','AGEP','RAC1P','SCHL','HISP','PWGTP'), data.table = FALSE),
                data.table::fread('census/ss13pusb.csv', select = c('ST','SEX', 'AGEP','RAC1P','SCHL','HISP','PWGTP'), data.table = FALSE))

#refactoring variables to match modeling methods
census$gender <- ifelse(census$SEX == 1, "Male", "Female")
census$age <- cut(census$AGEP, breaks = c(-Inf,18,35,55,Inf), labels = c("under 18","18-35","35-55", "55+"))
census$SCHL[which(is.na(census$SCHL))] <- 0
census$education <- cut(census$SCHL, breaks = c(-Inf,16, 21, Inf), labels = c('Up to HS', 'Up to College', 'PostGraduate'))
census$race <- factor(census$RAC1P)
levels(census$race) <- c('White', 'Black', 'Native', 'Native', 'Native', 'Asian', 'Asian', 'Other', 'Other')
census$hisp <- ifelse(census$HISP == 1, "Hisp", "Not Hisp")

#dummying census voter file
dummiesCensus <- dummyVars(ST ~ . , data= census[,c('ST','gender','age','education','race','hisp')])
census2 <- as.data.frame(predict(dummiesCensus, newdata = census))
names(census2) <- c("female","male","under18","age18.35","age35.55","age55plus","hs","college",
                    "postgraduate","white","black","native","asian","other.race","hisp","notHisp")

#matching the census state codes to the demState codes
census2 <- cbind(census2, census$ST)
names(census2)[ncol(census2)] <- 'ST'
census2$demState <- factor(census2$ST)
table(census2$demState)
levels(census2$demState) <- seq(1,51)

#dummying out regional again
census2$newengland <- ifelse(census2$demState %in% c(7, 20, 22, 30, 40, 46), 1,0)
census2$midatlantic <- ifelse(census2$demState %in% c(31, 33, 39),1,0)
census2$eastcentral <- ifelse(census2$demState %in% c(16, 14, 23, 36, 50),1,0)
census2$westcentral <- ifelse(census2$demState %in% c(16, 17, 24, 26, 28, 35, 42),1,0)
census2$southatlantic <- ifelse(census2$demState %in% c(8, 9, 10, 11, 21, 34, 41, 47, 49),1,0)
census2$southeast <- ifelse(census2$demState %in% c(1,18, 25, 43), 1, 0)
census2$southwest <- ifelse(census2$demState %in% c(2, 19, 37, 44), 1, 0)
census2$moutain <-  ifelse(census2$demState %in% c(3, 6, 13, 32, 27,45, 51), 1, 0)
census2$pacific <- ifelse(census2$demState %in% c(2,5, 12, 38, 48), 1, 0)

#applying the algorithm
census2$predictionRight <- predict(nnet_tune1, newdata = census2, type = 'prob')[2]
census2$predictionApprove <- predict(nnet_tune2, newdata = census2, type = 'prob')[2]
census2$predictionEcon <- predict(nnet_tune3, newdata = census2, type = 'prob')[2]


#applying census weights  to scores
census2 <- cbind(census2, census$PWGTP)
names(census2)[ncol(census2)] <- 'weights'

census2$rightScore <- census2$predictionRight * census2$weights 
census2$approveScore <- census2$predictionApprove * census2$weights 
census2$econScore <- census2$predictionEcon * census2$weights 

#tabulating scores by state
rScore <- as.data.frame(tapply(as.numeric(unlist(census2$rightScore)),census2$ST,function(x) sum(x)/length(x)))
aScore <- as.data.frame(tapply(as.numeric(unlist(census2$approveScore)),census2$ST,function(x) sum(x)/length(x)))
eScore <- as.data.frame(tapply(as.numeric(unlist(census2$econScore)),census2$ST,function(x) sum(x)/length(x)))
final <- cbind(as.data.frame(levels(as.factor(census2$ST))),rScore,aScore,eScore)
names(final) <- c("ST","right","approve","econ")


#writing results to csv to create maps
write.csv(final, "predictions.csv",row.names=F)