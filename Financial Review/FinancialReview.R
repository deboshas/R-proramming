#set the  working directory

setwd("E:\\R Advanced\\Financial Review")
#import the dataset to R ,replace emtpy data with na in data frame
Financial.Data <- read.csv("Future-500-Data.csv",na.strings = c(""))

str(Financial.Data) 

#Changing non factor to factor 

Financial.Data$ID <- factor(Financial.Data$ID)

Financial.Data$Inception <- factor(Financial.Data$Inception)

#Chaning factor to non factor Factor variable Trap-- VVI this is  a pitfall

a <- c("12","13","14","15","16","17")
#typeof(a)
 b <- as.numeric(a)#typecasting
b 
#typeof(b)

#Convert factor to numeric
 z <- factor(c("12","13","14","15","16","17"))
y <- as.numeric(z)
y 
typeof(y)

 y <- as.numeric(as.character(z))x
 y
 typeof(y)

 
 #Changing Factor to non-Factor in data frame
 Financial.Data$Profit <- factor(Financial.Data$Profit)
 head(Financial.Data)
 
 summary(Financial.Data)
 str(Financial.Data)
 #Convert factor to non factor this is very much important  
 Financial.Data$Profit  <- as.numeric(as.character(Financial.Data$Profit))
 
 str(Financial.Data)
 
 #gsub and sub pattern matching ,regex gsub would replace all instace
 
 Financial.Data$Expenses <- as.numeric(  gsub(" Dollars", "",Financial.Data$Expenses))
 Financial.Data$Expenses <-  as.numeric(gsub(",","",Financial.Data$Expenses))
 Financial.Data$Revenue <- as.numeric(gsub("\\$","",Financial.Data$Revenue))
 Financial.Data$Revenue <- as.numeric(gsub(",","",Financial.Data$Revenue))
 Financial.Data$Growth <- as.numeric(gsub("%","",Financial.Data$Growth))
 
 #Convert Revenue Expenses and Growth to numeric( factor to non-factor)
 Financial.Data$Expenses <- as.numeric(Financial.Data$Expenses)
 Financial.Data$Revenue <- as.numeric(Financial.Data$Revenue)
 Financial.Data$Growth <- as.numeric(Financial.Data$Growth)
 
 
 #What is NA --logical constraint
 
 ?NA
 
 #FALSE ==NA
 
 #Locate Missing Data
 head(Financial.Data,24)
 
 complete.cases(Financial.Data)
 
 #get the subset of the entire data set that has NA in any of teh columns
 
 Financial.Data[!complete.cases(Financial.Data),]
 
 
 #Filter the data frame ,which to filter the NA dvalues ,only true values will be taken ,NA values will be ignored
 
 Financial.Data[which(Financial.Data$Revenue == 9746272),]
 
 head(Financial.Data[which(Financial.Data$Employees >20),])
 
 
 #take only NA values,filter with Na values
 Financial.Data[is.na(Financial.Data$Expenses),]
 
 Financial.Data[is.na(Financial.Data$State),]

 #Removing missing data for important features 
 Financial.Data.Backup <- Financial.Data
 #Remove rows where NA for Industry column
 Financial.Data <- Financial.Data[!is.na(Financial.Data$Industry),]
 Financial.Data[is.na(Financial.Data$Industry),] 
 
 #Reset the data frame index here 14 15 row no is missing
 rownames(Financial.Data) <- seq(1:nrow(Financial.Data))
 rownames(Financial.Data) <- NULL
 
 #Replacing missing data with factual analysis,like expenses(Revune-Profit), state column
Financial.Data[is.na(Financial.Data$State),]
Financial.Data[is.na(Financial.Data$State) & Financial.Data$City == "New York","State"] <- "NY"   
Financial.Data[is.na(Financial.Data$State) & Financial.Data$City == "San Francisco","State"]  <- "CA"


median_emp_retail <- median(Financial.Data[Financial.Data$Industry == "Retail","Employees"],na.rm=TRUE)
Financial.Data[is.na(Financial.Data$Employees) & Financial.Data$Industry=="Retail","Employees"] <- median_emp_retail
  

median_emp_finseve <-median(Financial.Data[Financial.Data$Industry == "Financial Services","Employees"],na.rm=TRUE)
Financial.Data[is.na(Financial.Data$Employees) & Financial.Data$Industry=="Financial Services","Employees"] <- median_emp_retail

Financial.Data[!complete.cases(Financial.Data),]
  
 
  
 
 
 
 
 
 
 