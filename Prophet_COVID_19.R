library(data.table)
library(lubridate)
library(forecast)
library(prophet)
setwd("/kaggle/input/covid19-global-forecasting-week-2")
Data_Train=fread("train.csv")
Data_Test=fread("test.csv")
Submission=fread("submission.csv")

Data_TA=Data_Train
Data_TA[,key:=paste0(Country_Region,"/",Province_State)]
Data_TA=Data_TA[,.(sum(ConfirmedCases),sum(Fatalities)),by=.(key,Date)]
setnames(Data_TA,c("V1","V2"),c("Total_Confirmed","Total_Fatalities"))

############ Prophet ################
for(j in unique(Data_TA$key)){
  Data = Data_TA[key %in% j][,.(key,Date,Total_Confirmed)]
  Data = Data[,.SD[order(Date)]]
  Data[,Total_Confirmed:=ifelse(Total_Confirmed==0,runif(1,min=1,max=6),Data$Total_Confirmed)]
  lam = forecast::BoxCox.lambda(Data$Total_Confirmed,method="guerrero")
  Data$ds=Data$Date
  Data$y = forecast::BoxCox(Data$Total_Confirmed, lam)
  Data1=Data[,.(ds,y)]
  m <- prophet(Data1,weekly.seasonality = T,daily.seasonality = T)
  future <- make_future_dataframe(m, periods = 43)
  forecast <- predict(m, future)
  inverse_forecast <- forecast
  inverse_forecast$yhat_untransformed = forecast::InvBoxCox(forecast$yhat, lam)
  Output=data.table::as.data.table(inverse_forecast)
  Output=Output[,.(ds,yhat_untransformed)]
  Output=Output[ds>=as.Date(min(Data_Test$Date))]
  Output_C = data.table(Country_Region_Province = unique(Data$key),Date=Output$ds,
                      Confirmed_Cases = Output$yhat_untransformed)
  Collated_Output = rbind(if(exists("Collated_Output")) Collated_Output, Output_C)
  }
Collated_Output_bkp=Collated_Output
########################## holts #######################3
Days_To_Project = as.data.table(seq(as.Date(max(Data_Train$Date))+1, by = "day", length.out = 33))

for(j in unique(Data_TA$key)){
  S1 = Data_TA[key == j]
  S_train = S1[,.SD[order(Date)]]
  #S_train[,Total_Fatalities:=ifelse(Total_Fatalities==0,runif(1,min=0.8,max=2),S_train$Total_Fatalities)]
  S_train1 = ts(S_train$Total_Confirmed,frequency = 17)
  model1 = arima(S_train1,order=c(0,2,0)) 
  Output = data.table(Country_Region_Province = unique(S_train$key),Date=Days_To_Project$V1,
                      Confirmed_Cases = forecast(model1)$mean[1:33])
  Collated_Output_Fatal = rbind(if(exists("Collated_Output_Fatal")) Collated_Output_Fatal, Output)
}

Days_To_Project_1 = as.data.table(seq(as.Date(min(Data_Test$Date)), by = "day", length.out = 10))
for(j in unique(Data_TA$key)){
  S1 = Data_TA[key == j]
  S_train = S1[Date<"2020-03-20",.SD[order(Date)]]
  S_train1 = ts(S_train$Total_Fatalities)
  model1 = holt(S_train1,h=10) 
  Output = data.table(Country_Region_Province = unique(S_train$key),Date=Days_To_Project_1$V1,
                      Fatal_Cases = forecast(model1)$mean[1:10])
  Collated_Output_test = rbind(if(exists("Collated_Output_test")) Collated_Output_test, Output)
}

Collated_Output_fatal=rbind(Collated_Output_test,Collated_Output_Fatal)
Collated_Output_fatal=Collated_Output_fatal[order(Country_Region_Province,Date)]


Data_T=Data_Test[,Country_Region_Province:=paste0(Country_Region,"/",Province_State)]
Data_T=Data_T[,.(ForecastId,Country_Region_Province,Date)]
Data_T$Date=as.Date(Data_T$Date)
Collated_Output=Collated_Output[Date<"2020-05-01"]
Collated_Output$Date=as.Date(Collated_Output$Date)
Data_Final=Data_T[Collated_Output_fatal,on=.(Country_Region_Province,Date)]
Data_Final=Collated_Output[Data_Final,on=.(Country_Region_Province,Date)]

Data_Final_1=Data_Final[,.(ForecastId,Confirmed_Cases,Fatal_Cases)]
setnames(Data_Final_1,c("Confirmed_Cases","Fatal_Cases"),c("ConfirmedCases","Fatalities"))


setwd("/kaggle/working")                                     
fwrite(Submission_Final,"submission.csv")