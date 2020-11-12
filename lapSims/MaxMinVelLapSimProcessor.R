library(ggplot2)

printHistogram<-function(fileName){
  #import data
  rawdata<- read.csv(fileName, header = TRUE, sep = ",",stringsAsFactors=FALSE, skip = 80)
  rawdata<-rawdata[-c(1,2),] #remove unit lines
  rawdata<-rawdata[,c(1,2,5)]#select specific columns
  
  #assign numbers to the accel segments
  flip <-FALSE
  iterator <-0
  rawdata$accelNum<-0
  for(i in 1:nrow(rawdata)){
    if(rawdata$longitudinalAcceleration[i]>0){
      if(!flip){
        iterator<-iterator+1
      }
      flip = TRUE
      rawdata$accelNum[i]<-iterator
    }
    if(rawdata$longitudinalAcceleration[i]<0){
      flip = FALSE
    }
    if(rawdata$longitudinalAcceleration[i]==0){
      rawdata$accelNum[i]<-rawdata$accelNum[i-1]
    }
  }
  
  #remove negative accel data
  rawdata<-rawdata[-which(rawdata$accelNum==0),]
  
  #split the accel data by segment
  accelRunSplit<-split(rawdata, rawdata$accelNum)
  
  #find the starting and ending velocity of each segment
  accelSpeeds<-data.frame(matrix(ncol = 2, nrow = length(accelRunSplit)))
  colnames(accelSpeeds)<-c("StartingVel", "EndingVel")
  for(i in 1:length(accelRunSplit)){
    accelSpeeds$StartingVel[i]<-accelRunSplit[[i]]$speed[1]
    accelSpeeds$EndingVel[i]<-accelRunSplit[[i]]$speed[nrow(accelRunSplit[[i]])] 
  }
  
  accelSpeeds$StartingVel <- as.numeric(accelSpeeds$StartingVel)
  accelSpeeds$EndingVel <- as.numeric(accelSpeeds$EndingVel)
  
  #plot histogram of starting and ending velocities
  fileNameNew<-substr(fileName, 1, nchar(fileName)-4)
  #pdf(paste("speedHistogram_",fileNameNew,".pdf", sep=""))
  print(ggplot(accelSpeeds) + 
    geom_histogram(aes(x=StartingVel, y=..density..), fill = "blue",  alpha = 0.5, bins = 25) + 
    geom_histogram(aes(x=EndingVel, y=..density..), fill = "red", alpha = 0.5, bins = 25) +
    geom_density(aes(x=StartingVel),alpha=.2, fill="blue") +
    geom_density(aes(x=EndingVel),alpha=.2, fill="red") +
    geom_vline(aes(xintercept=mean(StartingVel)),
                 color="blue", linetype="dashed", size=1)+
    geom_vline(aes(xintercept=mean(EndingVel)),
               color="red", linetype="dashed", size=1)+
    labs(x="speed", title=fileNameNew)+
    xlim(c(0,150)))
  #dev.off()
}

setwd("D:/Clubs/FSAE/Personal/LapSims")
fileNames <- Sys.glob("*.csv")

pdf("speedHistogram_7.pdf")
for (fileName in fileNames) {
  printHistogram(fileName)
  
}
dev.off()
