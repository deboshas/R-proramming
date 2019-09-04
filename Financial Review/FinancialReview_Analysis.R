#set the  working directory

setwd("E:\\R Advanced\\Financial Review")
#import the dataset to R 
Financial.Data <- read.csv("Future-500-Data.csv",na.strings = c(""))

str(Financial.Data) 

#Changing non factor to factor 

Financial.Data$ID <- factor(Financial.Data$ID)

Financial.Data$Inception <- factor(Financial.Data$Inception)``

#Changing Factor to non-Factor
#gsub and sub pattern matching ,regex gsub would replace all instace

Financial.Data$Expenses <- gsub(" Dollars","",Financial.Data$Expenses)
Financial.Data$Expenses <- gsub(",","",Financial.Data$Expenses)
Financial.Data$Revenue <- gsub("\\$","",Financial.Data$Revenue)
Financial.Data$Revenue <- gsub(",","",Financial.Data$Revenue)
Financial.Data$Growth <- gsub("%","",Financial.Data$Growth)

#Convert Revenue Expenses and Growth to numeric
Financial.Data$Expenses <- as.numeric(Financial.Data$Expenses)
Financial.Data$Revenue <- as.numeric(Financial.Data$Revenue)
Financial.Data$Growth <- as.numeric(Financial.Data$Growth)

#Check for NA's and filter the data set accroding to it
complete.cases(Financial.Data)
Financial.Data[!complete.cases(Financial.Data),]
head(Financial.Data,24)

#Which for Na data

Financial.Data[which(Financial.Data$Expenses == 1130700),]

#is.na for missing data

Financial.Data[is.na(Financial.Data$Expenses),]

#Removing record with missing data in one factor column
Financial.Data.backup <-Financial.Data

Financial.Data <- Financial.Data[!is.na(Financial.Data$Industry),]

#Resetting row no's 
rownames(Financial.Data) <-c(1:nrow(Financial.Data))

rownames(Financial.Data) <- NULL

#Replacing missing data with factual analysis,like expenses(Revune-Profit), state column

Financial.Data[is.na(Financial.Data$State) & Financial.Data$City == "New York","State"] <- "NY"

Financial.Data[is.na(Financial.Data$State) & Financial.Data$City == "San Francisco","State"] <- "CA"

#Replace epmployee value for retail sector as median of retail sector

 mid_emp_retail <- median(Financial.Data[Financial.Data$Industry == "Retail","Employees"],na.rm = TRUE)
 Financial.Data[is.na(Financial.Data$Employee) & Financial.Data$Industry == "Retail","Employees"] <- mid_emp_retail
 
mid_emp_finserv <-median(Financial.Data[Financial.Data$Industry == "Financial Services","Employees"],na.rm=TRUE)
Financial.Data[is.na(Financial.Data$Employees) & Financial.Data$Industry == "Financial Services","Employees"] <-mid_emp_finserv

#Replace revenue values

mid_revenue_construction <- median(Financial.Data[Financial.Data$Industry == "Construction","Revenue"],na.rm=TRUE)
Financial.Data[is.na(Financial.Data$Revenue) & Financial.Data$Industry == "Construction","Revenue"] <- mid_revenue_construction
Financial.Data[is.na(Financial.Data$Expenses),]

#Replace expense values 

mid_expense_construction <- median(Financial.Data[Financial.Data$Industry == "Construction","Expenses"],na.rm = TRUE)
Financial.Data[is.na(Financial.Data$Expenses)  & Financial.Data$Industry == "Construction","Expenses"] <- mid_expense_construction

mid_expense_it_serv<- Financial.Data[Financial.Data$Industry == "IT Services" & is.na(Financial.Data$Expenses),"Revenue"] - Financial.Data[Financial.Data$Industry == "IT Services" & is.na(Financial.Data$Expenses),"Profit"]
Financial.Data[is.na(Financial.Data$Expenses)  & Financial.Data$Industry == "IT Services","Expenses"] <- mid_expense_it_serv


#Repalce profit ,deriving value method

profit <- Financial.Data[is.na(Financial.Data$Profit),"Revenue"] - Financial.Data[is.na(Financial.Data$Profit),"Expenses"]

Financial.Data[is.na(Financial.Data$Profit),"Profit"] <- Financial.Data[is.na(Financial.Data$Profit),"Revenue"] - Financial.Data[is.na(Financial.Data$Profit),"Expenses"]

#Relace growth 

mid_growth_construction <- median(Financial.Data[Financial.Data$Industry == "Construction" ,"Growth"],na.rm = TRUE)
Financial.Data[is.na(Financial.Data$Growth),"Growth"] <- mid_growth_construction


#Visalization 
#scatter plot

plt <- ggplot(data=Financial.Data)

plt+geom_point(aes(x=Revenue,y=Expenses,color=Industry,size=Profit))+xlab("Revenue")+ylab("Expense")



#Facet grid

plt+geom_point(aes(x=Revenue,y=Expenses,color=Industry,size=Profit))+facet_grid(.~Industry,scales="free")

#Boxplot

plt+geom_boxplot(aes(x=Industry,y=Growth,color=Industry))



















