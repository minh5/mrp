#Loading libraries
setwd("~/mrp/")
library(caret)

load("data.RData")

#break up ages, education, and political ideology
ageGroups <- as.data.frame(cut(tmpData$demAgeFull, breaks = c(0,18,35,55,Inf), labels = c("under 18","18-35","35-55", "55+")))
names(ageGroups) <- "ageGroups"
eduGroups <- as.data.frame(cut(tmpData$demEduFull, breaks = c(0,3,7,9), labels = c("Up to HS", "Up to College", "PostGraduate")))
names(eduGroups) <- "eduGroups"

#cleaning up dataset
dataPrep <- tmpData[,c("nr1","demGender","demRace","demHisp","xpid3")]
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
names(data) <- c("male","female","native","asian","black","white","other.race","hisp","notHisp","dem","gop","other.par",
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
names(data)
data <- data[,-grep("^demState",names(data))]

#creating columns where we are trying to measure predict
right <- as.data.frame(ifelse(tmpData$nr1 == 1, 1, 0))
names(right) <- "right"
data <- cbind(right, data)
data$right <- as.factor(data$right)
levels(data$right) <- c("No", "Yes")

#create training and test set
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
results$rf <- predict.train(rf_tune, newdata=test)


#Linear Discriminant Analysis
require(lda)
lda_tune <- train(right ~.,
                  method = 'lda',
                  data = train,
                  trControl = control)
lda_tune
results$lda<-predict(lda_tune, newdata = test)

#Good ol' logistic regression
logit_tune <- train(right ~.,
                    method = 'glm',
                    data = train,
                    trControl = control)
logit_tune
results$logit <- predict(logit_tune, newdata = test)

#Classification and Regression Tree, because why not.
require(rpart)
cart_tune = train(right ~ . , 
                  data = train ,
                  method = "rpart" ,
                  metric = "ROC" ,
                  tuneLength = 25 ,
                  trControl = control)
cart_tune
results$cart <- predict(cart_tune, newdata = test)

#Boosting because I heard it's really good machine learning algorithm
require(gbm)
tuningGrid <- expand.grid(.n.trees = seq(200,1000,by = 200), 
                          .interaction.depth = seq(1,7, by = 3 ),
                          .shrinkage = seq(.001, .01, by = .001),
                          .n.minobsinnode = seq(20,60, by = 20))

gbm_tune = train(right ~ . , 
                 data = train ,
                 method = "gbm" ,
                 metric = "ROC" ,
                 tuneGrid = tuningGrid,
                 trControl = control)
gbm_tune
results$gbm <- predict(gbm_tune, newdata = test)

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
results$nnet <- predict(nnet_tune, newdata = test)

#Approaching the problem like a computer scientist, with SVM
require(kernlab)
tuningGrid<-expand.grid(.sigma = c(.5,1), .C = c(1,2,3))
svm_tune <- train(right ~ .,
                  data = train,
                  method = "svmRadial",
                  metric = "ROC",
                  tuneGrid = tuningGrid,
                  trControl = control)
svm_tune
results$svm <- predict(svm_tune , newdata = test)

#We can't go without doing some bayes, Naive Bayes
require(e1071)
tuningGrid = expand.grid(.usekernel = c(TRUE, FALSE), .fL = c(1,2))
nb_tune <- train(right ~ .,
                 data = train,
                 method = "nb",
                 metric = "ROC",
                 tuneGrid = tuningGrid,
                 trControl = control)
nb_tune
results$nb <- predict(nb_tune, newdata = test)

#Looking at the results thus far and creating a confusion matrix for model selection
names(results)[1]<-"obs"

#confusion matrix for LDA
confusionMatrix(lda_tune , norm = 'average')
confusionMatrix(results$lda , results$obs)

# Logistic Regression
confusionMatrix(logit_tune , norm = 'average')
confusionMatrix(results$logit , results$obs)

# CART
confusionMatrix(cart_tune , norm = 'average')
confusionMatrix(results$cart , results$obs)

# Random Forest
confusionMatrix(rf_tune , norm = 'average')
confusionMatrix(results$rf , results$obs)

# Boosting
confusionMatrix(gbm_tune , norm = 'average')
confusionMatrix(results$gbm , results$obs)

# SVM Radial
confusionMatrix(svm_tune , norm = 'average')
confusionMatrix(results$svm , results$obs)

# Naive Bayes
confusionMatrix(nb_tune , norm = 'average')
confusionMatrix(results$nb , results$obs)

# Neural Net
confusionMatrix(nnet_tune, norm = 'average')
confusionMatrix(results$nnet, results$obs)
