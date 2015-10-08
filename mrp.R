#Loading libraries
setwd("~/mrp/")
library(caret)

load("data.RData")

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

#creating columns where we are trying to measure predict
right <- as.data.frame(ifelse(tmpData$nr1 == 1, 1, 0))
names(right) <- "right"
data <- cbind(right, data)
data$right <- as.factor(data$right)
levels(data$right) <- c("No", "Yes")

#create training and test set
set.seed(938479382)
split <- createDataPartition(data$right, p=.8)[[1]]
train<-data[split,]
test<-data[-split,]


#creating controller for training
control <- trainControl(method = "cv", 
                        number = 10, 
                        savePredictions = TRUE, 
                        classProbs = TRUE, 
                        summaryFunction = twoClassSummary)

#creating results table
results <- as.data.frame(test$right)
names(results)[1] <- 'obs'

#Running random forest
require(randomForest)
tuningGrid <- data.frame(.mtry = floor(seq(1 , ncol(train) , length = 6)))
rf_tune <- train(right ~ . , 
                 data=train, 
                 method = "rf" ,
                 metric = "ROC",
                 tuneGrid = tuningGrid,
                 trControl = control)
rf_tune
results$rf <- predict.train(rf_tune, newdata=test, type = 'prob')

#Good ol' logistic regression
logit_tune <- train(right ~.,
                    method = 'glm',
                    data = train,
                    trControl = control)
logit_tune
results$logit <- predict(logit_tune, newdata = test, type = 'prob')

#Classification and Regression Tree, because why not.
require(rpart)
cart_tune = train(right ~ . , 
                  data = train ,
                  method = "rpart" ,
                  metric = "ROC" ,
                  tuneLength = 25 ,
                  trControl = control)
cart_tune
results$cart <- predict(cart_tune, newdata = test, type = 'prob')

#Alot of talk has been on the accuracy of neural network algorithm, having a go at it here.
require(nnet)
tuningGrid<-expand.grid(.size = 1:5, .decay = c(0, 2, by = .5))
nnet_tune <- train(right ~ .,
                   data = train,
                   method = "nnet",
                   metric = "ROC",
                   tuneGrid = tuningGrid,
                   trControl = control)
nnet_tune
results$nnet <- predict(nnet_tune, newdata = test, type = 'prob')


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
data$prediction <- predict(nnet_tune, newdata = data, type = 'prob')[2]

#grabbing cohorts from census file for post stratification
post <- cbind(data, dataPrep)
post$race <- factor(post$demRace)
levels(post$race) <- c('Native', 'Asian', 'Black', 'White', 'Other')
post$gender <- ifelse(post$demGender == 1, "Male", "Female")
post$hisp <-ifelse(post$demHisp == 1, "Hisp", "Not Hisp") 
post <- cbind(post, tmpData$demState)
names(post)[ncol(post)] <- 'demState'
stateLookup <- data.frame(demState = seq(1,52), state = c(1,2,4,5,6,8,9,10,11,12,13,15,
                                                      16,17,18,19,20,21,22,23,24,25,
                                                      26,27,28,29,30,31,32,33,24,25,
                                                      36,37,38,39,40,41,42,44,45,46,
                                                      47,48,49,50,51,53,54,55,56,72))

post <- merge(post, stateLookup, by = "demState", all.x = T)

post$cohort <- paste(post$state, post$gender, post$ageGroups, post$eduGroups, post$race, post$hisp, sep=',')
census <- read.csv('census/post-strat.csv')                      

#post-stratification
post <- merge(post, census, by = 'cohort', all.x = T)
table(is.na(post$weights)) #noticed some weights are missing because the cohorts are not represented

# created a table of average weights to impute into missing values
means <- as.data.frame(tapply(post$weights, post$state, function(x) mean(x, na.rm = T)))
names(means) <- "means"
match <- merge(post, means, by.x = 'state', by.y = 0)
post$weights[which(is.na(post$weights))] <- match[which(is.na(match$weights)), 'means']

#post-stratification calcuations
post$finalScore <- post$prediction[[1]] * post$weights
tapply(post$finalScore, post$state, function(x) sum(x)*100)

#notice that Montana is missing
#to estimate the score, I'm going to grab the average sample population makeup of ID, WY, ND, and SD because they neighbor Montana. I have no reason to think that their population
#make is significantly different from Montana's

#grabbing from data set with the original tmpData frame as the index
mountain <- data[tmpData$demState %in% c(13, 35, 42, 51), ]

#loop to grab the averages of the dataset and append it onto Montana's
montana <- data.frame()
for (i in 2:26){
  montana <- rbind(montana, mean(mountain[,i]))
}

#reformatting dataset to match the training and test sets
montana <- as.data.frame(t(montana))
names(montana) <- names(train)[2:26]
row.names(montana) <- "montana"

#applying prediction models
montana$predictions <- predict(nnet_tune, newdata = montana, type = 'prob')[2]
montana$finalScore <- 
append(tapply(post$finalScore, post$state, sum), montana$predictions)

