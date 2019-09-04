setwd("E:\\R Advanced\\Titanic data set\\titanic")

train.data <- read.csv("train.csv")


#clean the data and find outliers


#Find the mean for Fares 

#box plot for Fare only
fare.boxplot.data <- ggplot(data = train.data)
fare.boxplot.data+geom_boxplot(aes(x="",y=Fare))
mean.fare <- round(mean(train.data$Fare),2)

#Interpretation: On an average, passengers have paid $32.2  to board the titanic.


#Age with highest frequency

#age_mode <- train.data[train.data$Age == max(train.D)]

#Median Fare

median.fare <- round(median(train.data$Fare),2)

#Interpretation: The mid value of Fare variable is $14.45. This means $14.45 divides the data into two halves

#Range of Fare
range.fare <- range(train.data$Fare)

#box plot for fare ,age and Pclass

#box plot for Fare and class 
train.data$Pclass <- factor(train.data$Pclass)
ggboxPlotData <- ggplot(data=train.data,aes(x=Pclass,y=Fare))
ggboxPlotData+geom_boxplot(size=1.2,alpha=0.5)


train.data$Pclass

summary(train.data)
