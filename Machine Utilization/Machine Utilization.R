#List to deliverd
#Character : Machine Name


setwd("E:\\R Advanced\\Machine Utilization")
Machine.Data <- read.csv("Machine-Utilization.csv",na.strings = c(""))
tail(Machine.Data,30)
str(Machine.Data)
summary(Machine.Data)

#Replace Na values
#Derive util.liztion column
Machine.Data$Utilization <- 1 - Machine.Data$Percent.Idle

#Date Time format
#?POSIXct #seconds passed since 1970 
Machine.Data$PosixTime <-   as.POSIXct(Machine.Data$Timestamp,format="%d/%m/%Y %H:%M")
#Re-arrange the columns
Machine.Data$Timestamp <- NULL
Machine.Data <- Machine.Data[,c(4,1,2,3)]

#R List can conatin element of any types .like python list
RL1.Data <- Machine.Data[Machine.Data$Machine == "RL1",]
summary(RL1.Data)

#Character: Machine name
#Vector: (min, mean, max) utilisation for the month (excluding unknown hours)
#Logical: Has utilisation ever fallen below 90%? TRUE / FALSE
#Vector: All hours where utilisation is unknown (NA's)
#Dataframe: For this machine
#Plot: For all machines


util_stats_RL1 <- c(min(RL1.Data$Utilization,na.rm=TRUE),
                    max(RL1.Data$Utilization,na.rm = TRUE),
                    mean(RL1.Data$Utilization,na.rm = TRUE))



Is_Util_Below_90 <-  length(which(RL1.Data$Utilization <0.9)) >0 #which values are less than 0.9


list_RL1 <- list(Machine="RL1",MachineStat=util_stats_RL1,LowThreashold=Is_Util_Below_90)

list_RL1$UnkownHours <- RL1.Data[is.na(RL1.Data$Utilization),"PosixTime"] #All hours where utilisation is unknown (NA's)
list_RL1$RL1Data <- RL1.Data


#Subsetting a list

#list_RL1$UnkownHours[1]
#list_RL1[[4]][1]
#list_RL1[1:4]
#list_RL1[c(1,4)]


#summary(list_RL1)
#str(list_RL1)
#names(list_RL1) <- c("Machine_Name","Stats","Low Threashold")
#rm(list_RL1)

#list_RL1[[2]][2]#maximum utilization from MachineStat vector inside a list_RL1
#list_RL1$MachineStat[2] #maximum utilization from MachineStat vector inside a list_RL1
#list_RL1[[3]]
#list_RL1$LowThreashold

#Build a timeseries plot for all machines

plot <- ggplot(data=Machine.Data)

plot <- plot+geom_line(aes(x=PosixTime ,y=Utilization,color=Machine))+
  facet_grid(Machine~ .,scales = "free")+geom_hline(yintercept = 0.9,color="blue",size=1.2,linetype=3)

list_RL1$AllMachinePlot <- plot


#return the list to any programe that can utilize this data














