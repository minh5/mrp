#loading up individual level census daa
setwd('~/mrp/census/')
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


#applying census weights and calculating final scores
census2 <- cbind(census2, census$PWGTP)
names(census2)[ncol(census2)] <- 'weights'

census2$rightScore <- census2$predictionRight * census2$weights 
census2$approveScore <- census2$predictionApprove * census2$weights 
census2$econScore <- census2$predictionEcon * census2$weights 

rScore <- as.data.frame(tapply(as.numeric(unlist(census2$rightScore)),census2$ST,function(x) sum(x)/length(x)))
aScore <- as.data.frame(tapply(as.numeric(unlist(census2$approveScore)),census2$ST,function(x) sum(x)/length(x)))
eScore <- as.data.frame(tapply(as.numeric(unlist(census2$econScore)),census2$ST,function(x) sum(x)/length(x)))
final <- cbind(as.data.frame(levels(as.factor(census2$ST))),rScore,aScore,eScore)
names(final) <- c("ST","right","approve","econ")

write.csv(final, "predictions.csv",row.names=F)
