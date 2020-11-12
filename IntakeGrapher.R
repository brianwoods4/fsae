library(ggplot2)

#import data (EDIT THIS PART)
setwd("D:/Clubs/FSAE/Personal/IntakeOptimization/Plots")
rawdata<- read.csv("10-27_dummy_intake_10000rpm_exhaustsweep_ryan_powergraph.csv", header = TRUE, sep = ",",stringsAsFactors=FALSE)
intakeRpm <- 10000
exhaustRpm1 <- 4000
testName <- "Power"
#graphRange <-c(20,70) #for torque
graphRange <-c(0,65) #for power
#graphRange <-c(0,220) #for airflow
#graphRange <-c(0,1.1) #for efficiency

#clean up data
names(rawdata)<-c("EngineSpeed", "Value")
rawdata<-rawdata[-1,]
rawdata$EngineSpeed <- as.numeric(rawdata$EngineSpeed)
rawdata$Value <- as.numeric(rawdata$Value)

#assign case numbers
caseNumber = 0
for (i in 1:nrow(rawdata)){
  if (abs(as.numeric(rawdata$EngineSpeed[i])-15000) < 100){
    caseNumber <- caseNumber+1
    if(caseNumber%%3==1 && caseNumber!=1){
      exhaustRpm1 <- exhaustRpm1 + 1000
    }
  }
  rawdata$caseNum[i]<-caseNumber
  rawdata$ExhaustRpm1[i]<-exhaustRpm1
}
rawdata$caseNum <-as.factor(rawdata$caseNum)

#split data by case
rpmSplit <-split(rawdata,rawdata$caseNum)

#graph each run separately TODO
fileName = paste("Intake RPM ",toString(intakeRpm)," ", testName, " Individual.pdf", sep = "")
pdf(fileName)
for(case in rpmSplit){
  titleString = paste("Intake RPM:",toString(intakeRpm),"Case:",toString(case$caseNum[1]), sep = " ")
  print(ggplot(data = case, aes(x=EngineSpeed, y=Value)) +geom_line() + 
          lims(x=c(0,16000), y=graphRange) + labs(title = titleString) + ylab(testName))
}
dev.off()

# #split data by case
# rpmSplit <-split(rawdata,rawdata$ExhaustRpm1)
# 
# #graph each case separately (3 to a plot)
# fileName = paste("Intake RPM ",toString(intakeRpm)," ", testName, ".pdf", sep = "")
# pdf(fileName)
# for(case in rpmSplit){
#   titleString = paste("Intake RPM:",toString(intakeRpm),"Exhaust Rpm 1:",toString(case$ExhaustRpm1[1]), sep = " ")
#   print(ggplot(data = case, aes(x=EngineSpeed, y=Value)) +geom_line(aes(color=caseNum)) + 
#           lims(x=c(0,16000), y=graphRange) + labs(title = titleString) + ylab(testName))
# }
# dev.off()
# 
# #graph each case together
# fileName = paste("Intake RPM Combined ",toString(intakeRpm)," ", testName, ".pdf", sep = "")
# titleString = paste("Intake RPM:",toString(intakeRpm), sep = " ")
# pdf(fileName, width = 7, height = 15)
# print(ggplot(data = rawdata, aes(x=EngineSpeed, y=Value)) + facet_grid(rows = vars(ExhaustRpm1))+geom_line(aes(color=caseNum)) + 
#         lims(x=c(0,16000), y=graphRange) + labs(title = titleString)+ ylab(testName))
# dev.off()