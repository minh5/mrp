#loading up individual level census daa
setwd('~/mrp/census/')
census <- rbind(data.table::fread('census/ss13pusa.csv', select = c('ST','SEX','AGEP','RAC1P','SCHL','HISP'), data.table = FALSE),
                data.table::fread('census/ss13pusb.csv', select = c('ST','SEX', 'AGEP','RAC1P','SCHL','HISP'), data.table = FALSE))
                
#refactoring variables to match modeling methods
census$gender <- ifelse(census$SEX == 1, "Male", "Female")
census$age <- cut(census$AGEP, breaks = c(-Inf,18,35,55,Inf), labels = c("under 18","18-35","35-55", "55+"))
census$education <- cut(census$SCHL, breaks = c(0,16, 21, Inf), labels = c('Up to HS', 'Up to College', 'PostGraduate'))
census$race <- factor(census$RAC1P)
levels(census$race) <- c('White', 'Black', 'Native', 'Native', 'Native', 'Asian', 'Asian', 'Other', 'Other')
census$hisp <- ifelse(census$HISP == 1, "Hisp", "Not Hisp")

#creating cohort levels to get proportions
census$cohort <- paste(census$ST, census$gender, census$age, census$education, census$race, census$hisp, sep=',')
head(census$cohort)
#custom function to grab proportions of each cohort by state
options(scipen=9999)
porpFunct <- function(x) {prop.table(table(x))}
proportions <- tapply(census$cohort, census$ST, function(x) prop.table(table(x))) 
proportions <- do.call(rbind, lapply(proportions, data.frame))
names(proportions) <- c("cohort","weights")

write.csv(proportions, 'census/post-strat.csv', row.names=F)
