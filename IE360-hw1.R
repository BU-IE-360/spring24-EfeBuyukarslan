library(readxl)
library(xts)
library(ggplot2)
library(lubridate)
library(dplyr)
library(gridExtra)
library(zoo)
library(skimr)
library(forecast)
library(car)
require(data.table)
require(lubridate)
require(skimr)
require(repr)
setwd("C:/Users/efe_b/Desktop/IE360/HW1/data")

data = read_excel("unemployment-airline-newfirm.xlsx")
unem_data = data$`Unemployment Rate`
newfirm_data = data$`Newly Established Firms`
airline_data = data$Airlines
date_data = data$`Date`
data = data.frame(data)
#Pairwise Correlations
cor(unem_data, airline_data)
cor(newfirm_data, airline_data)
cor(unem_data, newfirm_data)

#Date Information in Date Format
date_data = as.Date(paste(date_data, "-01", sep=""))
head(data)
data$covid <- 0
start_date <- as.Date("2020-03-01")
end_date <- as.Date("2021-06-01")
#Defining condition for covid period
condition <- date_data >= start_date & date_data < end_date
data$covid[condition] <- 1
data$covid[!condition] <- 0

exchangerate_data = data$ExchangeRate
consumerconfidenceindex_data = data$ConsumerConfidenceIndex
capacityutilization_data = data$Capacity.Utilization.Rate
foodexpenditure_data = data$Food.Expenditure
visadata = data$Visa.Search.Volume
data$trend = 1:dim(data)[1]
month = 1:12
data <- cbind(data,month)
data$year = year(date_data)




###############################UNEMPLOYMENT####################################
###Introduction
#Research Question: Is there a relationship between  career websites/platforms and unemployment? Do these platforms affect unemployment numbers?
#To investigate this relationship, I determined the keyword "????kur" and fetched its search volume data from Google Trends. After that I plotted this data with the unemployment data.
#Keyword-????kur
# Plotting
plot(date_data, unem_data, type = "l", xlab = "Date", ylab = "Value", main = "Unemployment Time Series Plot")

###1st Part - Data Manipulation and Visualization

iskur_excel_data = "iskursearchvolume.csv"
searchvolumeiskur = fread(iskur_excel_data)

searchvolumeiskur$Date = as.Date(paste(searchvolumeiskur$Date, "-01", sep=""))
iskurdata = searchvolumeiskur$IskurSearchVolume


#head(monthly_summary_linkedin)
cor(iskurdata ,unem_data)

plot1 <- ggplot(data, aes(x = date_data, y = unem_data)) + geom_line() + labs(title = "Unemployment Data", x = "Date", y = "Unemployment Rate")
plot2 <- ggplot(data, aes(x = date_data, y = iskurdata)) + geom_line(color = "red") +  labs(title = "Iskur Search Volume Data", x = "Date", y = "Search Volume")
grid.arrange(plot1, plot2, ncol = 1)



##First, I obtained the Google Trends data for Iskur search. I selected the same time horizon with the unemployment data. However, the frequency of search volume data was weekly. In order to convert it into monthly data, I summed up the weekly volumes for each month. Then, I plotted both the search volume and unemployment data to check the relation. After plotting, I couldn't observe significant trend/relation between the unemployment and Linkedin. Similarly, I conducted same steps for the keyword "????KUR" but again I couldn't get a meaningful result. Therefore, I   career platforms such Linkedin and ????KUR.
##When I examined the plots, I have observed an increasing trend in the Linkedin data whereas, a decreasing trend in the unemployment data. Looking at the time interval between 2021-2022, fluctuations in opposite directions are observed. Then, I calculated correlation between them and found that they have -0.51 value of correlation. This value demonstrates that the relevancy between the Linkedin searches and unemployment is in the expected direction.


###2nd Part - Time Series Regression Analysis


cor(unem_data, capacityutilization_data)
cor(unem_data, foodexpenditure_data)

fit_trend <- lm(unem_data~capacityutilization_data,data = data)
summary(fit_trend)
checkresiduals(fit_trend)
fit_trend_seasonality <- lm(unem_data~as.factor(data$month),data = data)
summary(fit_trend_seasonality)
fit_capacity <- lm(unem_data~capacityutilization_data,data = data)
summary(fit_capacity)
fit_foodexpenditure <- lm(unem_data~foodexpenditure_data,data = data)
summary(fit_foodexpenditure)
fit2 <- lm(unem_data~iskurdata+data$trend+as.factor(data$month),data = data)
summary(fit2)
fit_4 <- lm(unem_data~capacityutilization_data+data$trend+as.factor(data$month),data = data)
summary(fit_4)
fit_5 <- lm(unem_data~capacityutilization_data+data$trend+as.factor(data$month)+consumerconfidenceindex_data,data = data)
summary(fit_5)
fit_final <- lm(unem_data~iskurdata++exchangerate_data+capacityutilization_data+foodexpenditure_data+as.factor(data$month)+as.factor(data$covid)+consumerconfidenceindex_data,data = data)
summary(fit_final)
checkresiduals(fit_final,40)
plot(fit2$residuals)


###Conclusion



####################AIRLINE########################################
###Introduction
#Research Question: Are the debit and credit card expenditures on airlines related to the vacations abroad?
#Keyword-In order to examine whether there is relation between the expenditures on airlines and vacations abroad, I looked the keyword "yurt d?????? tatilleri" on Google Trends and plotted its search volume data with the expenditure data on airlines.

###1st Part - Data Manipulation and Visualization


yurtdisitatil_excel_data = "yurtdisitatilsearchvolume.csv"
searchvolumeyurtdisitatil = fread(yurtdisitatil_excel_data)


searchvolumeyurtdisitatil$Date = as.Date(paste(searchvolumeyurtdisitatil$Date, "-01", sep=""))
yurtdisitatildata = searchvolumeyurtdisitatil$YurtdisiTatilSearchVolume



cor(yurtdisitatildata,airline_data)

plot1 <- ggplot(data, aes(x = date_data, y = airline_data)) + geom_line() + labs(title = "Airline Data", x = "Date", y = "Total Debit and Credit Card Number of Transactions on Airlines")
plot2 <- ggplot(data, aes(x = date_data, y = yurtdisitatildata)) + geom_line(color = "red") + labs(title = "Yurt Disi Tatil Search Volume Data", x = "Date", y = "Search Volume")
grid.arrange(plot1, plot2, ncol = 1)




###2nd Part - Time Series Regression Analyses

cor(visadata,yurtdisitatildata)

fit1 <- lm(airline_data~yurtdisitatildata,data = data)
summary(fit1)
fit2 <- lm(airline_data~visadata+as.factor(month)+trend+as.factor(covid)+yurtdisitatildata,data = data)
summary(fit2)
checkresiduals(fit2,5)
plot(fit2$residuals)
fit3 <- lm(airline_data~visadata+as.factor(month)+as.factor(covid)+trend,data = data)
summary(fit3)
checkresiduals(fit3,5)
plot(fit3$residuals)
###Conclusion


###############################TOTAL NEWLY ESTABLISHED FIRMS####################################
###Introduction
#Research Question: Are entrepreneurs' decisions affected by the grants/fundings given? Is there a relationship between entrepreneurial/financial support and the number of newly established firms?
#To analyze this relation, I decided to use the search volume data of keyword "Giri??imci Deste??i" on Google Trends and compared it with the newly established firm data.
###1st Part - Data Manipulation and Visualization

girisimcidestegi_excel_data = "girisimcidestegisearchvolume.csv"
searchvolumegirisimcidestegi = fread(girisimcidestegi_excel_data)

searchvolumegirisimcidestegi$Date = as.Date(paste(searchvolumegirisimcidestegi$Date, "-01", sep=""))
girisimcidata = searchvolumegirisimcidestegi$GirisimciDestegiSearchVolume



cor(girisimcidata ,newfirm_data)

plot1 <- ggplot(data, aes(x = date_data, y = newfirm_data)) + geom_line() + labs(title = "Newly Established Firm Data", x = "Date", y = "Total Number of Firms Newly Established")
plot2 <- ggplot(data, aes(x = date_data, y = girisimcidata)) + geom_line(color = "red") + labs(title = "Girisimci Destegi Search Volume Data", x = "Date", y = "Search Volume")
grid.arrange(plot1, plot2, ncol = 1)




###2nd Part - Time Series Regression Analysis

fit1 <- lm(newfirm_data~`trend`,data = data)
summary(fit1)
checkresiduals(fit1)
fit2 <- lm(newfirm_data~trend+as.factor(month)+girisimcidata,data = data)
summary(fit2)
checkresiduals(fit2)
fit3 <- lm(newfirm_data~trend+as.factor(month)+girisimcidata+exchangerate_data,data = data)
summary(fit3)
checkresiduals(fit3)
fit4 <- lm(newfirm_data~trend+as.factor(month)+exchangerate_data+consumerconfidenceindex_data+girisimcidata+data$year,data = data)
summary(fit4)
checkresiduals(fit4)
plot(fit3$residuals)

###Conclusion

