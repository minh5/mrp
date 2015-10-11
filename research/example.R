rm(list=ls(all=TRUE))

library("arm")
library("foreign")

#read in megapoll and attach
data <- read.dta("portfolio/mrp//research/gay_marriage_megapoll.dta", convert.underscore = TRUE) 

#read in state-level dataset

state <- read.dta("portfolio/mrp//research/state_level_update.dta",convert.underscore = TRUE)
state <- state[order(state$sstate.initnum),]

#read in census data
census <- read.dta("portfolio/mrp//research/poststratification 2000.dta",convert.underscore = TRUE)
census <- census[order(census$cstate),]
census$cstate.initnum <-  match(census$cstate, state$sstate)

#Create index variables

#At level of megapoll

data$race.female <- (data$female *3) + data$race.wbh# from 1 for white males to 6 for hispanic females
data$age.edu.cat <- 4 * (data$age.cat -1) + data$edu.cat# from 1 for 18-29 with low edu to 16 for 65+ with high edu
data$p.evang.full <- state$p.evang[data$state.initnum]# proportion of evangelicals in respondent's state
data$p.mormon.full <-state$p.mormon[data$state.initnum]# proportion of mormon's in respondent's state
data$p.relig.full <- data$p.evang.full + data$p.mormon.full# combined evangelical + mormom proportions
data$p.kerry.full <- state$kerry.04[data$state.initnum]# kerry's % of 2-party vote in respondent's state in 2004

#At census level (same coding as above for all variables)

census$crace.female <- (census$cfemale *3) + census$crace.WBH 
census$cage.edu.cat <- 4 * (census$cage.cat -1) + census$cedu.cat 
census$cp.evang.full<-  state$p.evang[census$cstate.initnum]
census$cp.mormon.full <- state$p.mormon[census$cstate.initnum]
census$cp.relig.full <- census$cp.evang.full + census$cp.mormon.full
census$cp.kerry.full <-  state$kerry.04[census$cstate.initnum]


#run individual-level opinion model

individual.model <- glmer(formula = yes.of.all ~ (1|race.female) + (1|age.cat) 
                          + (1|edu.cat) + (1|age.edu.cat) + (1|state) + (1|region) + (1|poll) + p.relig.full 
                          + p.kerry.full,data=data, family=binomial(link="logit"))
display(individual.model)

#examine random effects and standard errors for race-female
ranef(individual.model)$race.female
se.ranef(individual.model)$race.female

#create vector of state ranefs and then fill in missing ones
state.ranefs <- array(NA,c(51,1))
dimnames(state.ranefs) <- list(c(state$sstate),"effect")
for(i in state$sstate){
  state.ranefs[i,1] <- ranef(individual.model)$state[i,1]
}
state.ranefs[,1][is.na(state.ranefs[,1])] <- 0 #set states with missing REs (b/c not in data) to zero


#create a prediction for each cell in census data
cellpred <- invlogit(fixef(individual.model)["(Intercept)"]
                     +ranef(individual.model)$race.female[census$crace.female,1]
                     +ranef(individual.model)$age.cat[census$cage.cat,1]
                     +ranef(individual.model)$edu.cat[census$cedu.cat,1]
                     +ranef(individual.model)$age.edu.cat[census$cage.edu.cat,1]
                     +state.ranefs[census$cstate,1]
                     +ranef(individual.model)$region[census$cregion,1]   
                     +(fixef(individual.model)["p.relig.full"] *census$cp.relig.full)
                     +(fixef(individual.model)["p.kerry.full"] *census$cp.kerry.full)
)

#weights the prediction by the freq of cell                                       
cellpredweighted <- cellpred * census$cpercent.state

#calculates the percent within each state (weighted average of responses)
statepred <- 100* as.vector(tapply(cellpredweighted,census$cstate,sum))
statepred
