#Loading libraries
library(caret)

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
#create training and test set
split <- createDataPartition(data$right, p=.8)[[1]]
train<-data[split,]
test<-data[-split,]
