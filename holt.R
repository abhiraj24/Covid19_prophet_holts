########## COVID 19 analysis ############
library(data.table)
library(lubridate)
library(forecast)

setwd("/kaggle/input/covid19-global-forecasting-week-2")
Data_Train=fread("train.csv")
Data_Test=fread("test.csv")
Submission=fread("submission.csv")

Data_TA=Data_Train
#Data_TA[is.na(Id)]
Data_TA[,key:=paste0(Country_Region,"/",Province_State)]
Data_TA=Data_TA[,.(sum(ConfirmedCases),sum(Fatalities)),by=.(key,Date)]
setnames(Data_TA,c("V1","V2"),c("Total_Confirmed","Total_Fatalities"))
############################### Confirmed Cases #####################################################
Days_To_Project = as.data.table(seq(as.Date(max(Data_Train$Date))+1, by = "day", length.out = 31))

############ Taking account whole data ################
for(j in unique(Data_TA$key)){
  S1 = Data_TA[key == j]
  S_train = S1[,.SD[order(Date)]]
  S_train[,Total_Fatalities:=ifelse(Total_Fatalities==0,runif(1,min=0.8,max=15),S_train$Total_Fatalities)]
  S_train1 = ts(S_train$Total_Confirmed)
  model1 = holt(S_train1,h=31) 
  Output = data.table(Country_Region_Province = unique(S_train$key),Date=Days_To_Project$V1,
                      Confirmed_Cases = forecast(model1)$mean[1:31])
  Collated_Output = rbind(if(exists("Collated_Output")) Collated_Output, Output)
}
#rm(Collated_Output)
############### For testing Puposes ##################
Days_To_Project_1 = as.data.table(seq(as.Date(min(Data_Test$Date)), by = "day", length.out = 12))
for(j in unique(Data_TA$key)){
  S1 = Data_TA[key == j]
  S_train = S1[Date<"2020-03-20",.SD[order(Date)]]
  S_train[,Total_Fatalities:=ifelse(Total_Fatalities==0,runif(1,min=0.8,max=15),S_train$Total_Fatalities)]
  S_train1 = ts(S_train$Total_Confirmed)
  model1 = holt(S_train1,h=12) 
  Output = data.table(Country_Region_Province = unique(S_train$key),Date=Days_To_Project_1$V1,
                      Confirmed_Cases = forecast(model1)$mean[1:12])
  Collated_Output_test = rbind(if(exists("Collated_Output_test")) Collated_Output_test, Output)
}
########## Plot train vs fit models #####################
# plot.ts(S_train1,main = "Smoothed Timeseries", col = "blue")
# lines(fitted(model1),col = "red")
# 
# ########### Decmpose into level trend and seasonality#####
# states <- model1$model$states[,1:3]
# colnames(states) <- cbind('Level','Trend','Seasonality')
# plot(states,col = "blue", main = "Decompostion of time series")
#rm(Collated_Output_test)
Collated_Output_confirmed=rbind(Collated_Output,Collated_Output_test)
Collated_Output_confirmed=Collated_Output_confirmed[order(Country_Region_Province,Date)]


################################## Fatalties############################################################
Days_To_Project = as.data.table(seq(as.Date(max(Data_Train$Date))+1, by = "day", length.out = 31))

############ Taking account whole data ################
for(j in unique(Data_TA$key)){
  S1 = Data_TA[key == j]
  S_train = S1[,.SD[order(Date)]]
  S_train[,Total_Fatalities:=ifelse(Total_Fatalities==0,runif(1,min=0.8,max=2),S_train$Total_Fatalities)]
  S_train1 = ts(S_train$Total_Fatalities)
  model1 = holt(S_train1,h=31) 
  Output = data.table(Country_Region_Province = unique(S_train$key),Date=Days_To_Project$V1,
                      Fatal_Cases = forecast(model1)$mean[1:31])
  Collated_Output_fatal = rbind(if(exists("Collated_Output_fatal")) Collated_Output_fatal, Output)
}
#rm(Collated_Output_fatal)
############### For testing Puposes ##################
Days_To_Project_1 = as.data.table(seq(as.Date(min(Data_Test$Date)), by = "day", length.out = 12))
for(j in unique(Data_TA$key)){
  S1 = Data_TA[key == j]
  S_train = S1[Date<="2020-03-20",.SD[order(Date)]]
  S_train[,Total_Fatalities:=ifelse(Total_Fatalities==0,runif(1,min=0.8,max=2),S_train$Total_Fatalities)]
  S_train1 = ts(S_train$Total_Fatalities)
  model1 = holt(S_train1,h=12) 
  Output = data.table(Country_Region_Province = unique(S_train$key),Date=Days_To_Project_1$V1,
                      Fatal_Cases = forecast(model1)$mean[1:12])
  Collated_Output_Fatal_test = rbind(if(exists("Collated_Output_Fatal_test")) Collated_Output_Fatal_test, Output)
}
#rm(Collated_Output_Fatal_test)
Collated_Output_fatal=rbind(Collated_Output_Fatal_test,Collated_Output_fatal)
Collated_Output_fatal=Collated_Output_fatal[order(Country_Region_Province,Date)]

COl_output=Collated_Output_confirmed[Collated_Output_fatal,on=.(Country_Region_Province,Date)]
#COl_output[,Confirmed_Cases:=ceiling(Confirmed_Cases)]
#COl_output[,Fatal_Cases:=ceiling(Fatal_Cases)]

Data_Test[,key:=paste0(Country_Region,"/",Province_State,Date)]
COl_output[,key:=paste0(Country_Region_Province,Date)] 

O_P=Data_Test[,.(key,ForecastId)][COl_output[,.(key,Confirmed_Cases,Fatal_Cases)],on=.(key)]  
Submission_Final=Submission[O_P[,.(ForecastId,Confirmed_Cases,Fatal_Cases)],on=.(ForecastId)]                                     
Submission_Final$ConfirmedCases=NULL
Submission_Final$Fatalities=NULL                                     
setnames(Submission_Final,c("Confirmed_Cases","Fatal_Cases"),c("ConfirmedCases","Fatalities"))    
#Submission_Final=Submission_Final[!is.na(ForecastId)]

setwd("/kaggle/working")                                     
fwrite(Submission_Final,"submission.csv")