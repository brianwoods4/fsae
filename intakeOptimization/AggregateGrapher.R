library(ggplot2)

#import data (EDIT THIS PART)
setwd("D:/Clubs/FSAE/Personal/IntakeOptimization/Plots")
rawdata<- read.csv("PowerAggregate.csv", header = TRUE, sep = ",",stringsAsFactors=FALSE)
exhaustRpm1 <- 4000
testName <- "Power"
#graphRange <-c(20,70) #for torque
graphRange <-c(0,65) #for power
#graphRange <-c(0,220) #for airflow
#graphRange <-c(0,1.1) #for efficiency

#clean up data
names(rawdata)<-c("EngineSpeed", "Value", "IntakeRpm")
rawdata<-rawdata[-1,]
rawdata$EngineSpeed <- as.numeric(rawdata$EngineSpeed)
rawdata$Value <- as.numeric(rawdata$Value)
rawdata$IntakeRpm <- as.numeric(rawdata$IntakeRpm)

#assign case numbers
caseNumber = 0
for (i in 1:nrow(rawdata)){
  if (abs(as.numeric(rawdata$EngineSpeed[i])-15000) < 100){
    caseNumber <- caseNumber+1
    if(caseNumber%%3==1 && caseNumber!=1){
      exhaustRpm1 <- exhaustRpm1 + 1000
    }
    if(caseNumber==19){
      caseNumber = 1
    }
    if(exhaustRpm1==10000){
      exhaustRpm1 <- 4000
    }
  }
  rawdata$caseNum[i]<-caseNumber
  rawdata$ExhaustRpm1[i]<-exhaustRpm1
}
rawdata$caseNum <-as.factor(rawdata$caseNum)

#graph everything together
fileName = paste(testName," Aggregate.pdf", sep = "")
titleString = paste(testName," Aggregate", sep = "")
pdf(fileName, width = 20, height = 15)
#LABELS
print(ggplot(data = rawdata, aes(x=EngineSpeed, y=Value)) + facet_grid(rows = vars(ExhaustRpm1), cols = vars(IntakeRpm))+geom_line(aes(color=caseNum)) +
        lims(x=c(0,16000), y=graphRange) + labs(title = titleString)+ ylab(testName))
dev.off()