library(ggplot2)
library(gtools)

printSpeedHistogram<-function(fileName){
  #import data
  rawdata<- read.csv(fileName, header = TRUE, sep = ",",stringsAsFactors=FALSE, skip = 78)#80 in my data
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
  rawdata<-subset(rawdata, accelNum!=0)
  rawdata$speed<-as.numeric(rawdata$speed)
  
  #plot histogram of starting and ending velocities
  fileNameNew<-substr(fileName, 1, nchar(fileName)-4)
  print(ggplot(rawdata, aes(x=speed)) +
          geom_histogram(aes(y=..density..), fill = "blue",  alpha = 0.5, bins = 25) +
          geom_density(alpha=.2, fill="blue") +
          geom_vline(aes(xintercept=mean(speed)),
                     color="blue", linetype="dashed", size=1)+
          labs(x="speed", title=fileNameNew)+
          scale_x_continuous(breaks = seq(25,140,20), limits = c(25,140)))
}

printRpmHistogram<-function(fileName){
  #import data
  rawdata<- read.csv(fileName, header = TRUE, sep = ",",stringsAsFactors=FALSE, skip = 78)#80 in my data
  rawdata<-rawdata[-c(1,2),] #remove unit lines
  rawdata<-rawdata[,c(8,2,5)]#select specific columns
  
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
  rawdata<-subset(rawdata, accelNum!=0)
  rawdata$engineSpeed<-as.numeric(rawdata$engineSpeed)

  
  #plot histogram of starting and ending velocities
  fileNameNew<-substr(fileName, 1, nchar(fileName)-4)
  print(ggplot(rawdata, aes(x=engineSpeed)) +
          geom_histogram(aes(y=..density..), fill = "blue",  alpha = 0.5, binwidth = 250) +
          geom_density(alpha=.2, fill="blue") +
          geom_vline(aes(xintercept=mean(engineSpeed)),
                     color="blue", linetype="dashed", size=1)+
          labs(x="engineSpeed", title=fileNameNew)+
          scale_x_continuous(breaks = seq(4000,15000,1000),minor_breaks = seq(4000,15000,250), limits = c(4000,15000)))
}

setwd("D:/Clubs/FSAE/Personal/LapSims/10000RPM--Sutton/Acceleration") #put the csvs that need to be graphed together in this location
fileNames <- Sys.glob("*.csv")
fileNames<- mixedsort(sort(fileNames))#sort them in a logical order

pdf("speedHistograms_10_acc.pdf")
for (fileName in fileNames) {
  printSpeedHistogram(fileName)
}
dev.off()

pdf("rpmHistograms_10_acc.pdf")
for (fileName in fileNames) {
  printRpmHistogram(fileName)
}
dev.off()
