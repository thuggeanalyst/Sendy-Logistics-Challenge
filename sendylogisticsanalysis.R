#Importing data to r checking the structure and na values
mydata=read.csv(file.choose(), header = T)
columns=View(colnames(mydata))


attach(mydata)
library(dplyr)

na_count <-sapply(mydata, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

mydata=select(mydata,-Precipitation.in.millimeters)
library(na.tools)
mid=median(na.omit(Temperature))
mid
mydata$Temperature= na.replace(Temperature, mid)
#mydata=na.omit(mydata)
mydata=select(mydata,-Arrival.at.Destination...Day.of.Month,
              -Arrival.at.Destination...Weekday..Mo...1.,
              -Arrival.at.Destination...Time)

na_count1 <-sapply(mydata, function(y) sum(length(which(is.na(y)))))
na_count1 <- data.frame(na_count1)
na_count1


View(mydata)
mydata=select(mydata,-User.Id,-Vehicle.Type,-Rider.Id)
View(mydata)
#View(is.na(mydata))
# mydata$Platform.Type=as.factor(mydata$Platform.Type)
# str(mydata$Platform.Type)


mydata$Platform.Type=factor(mydata$Platform.Type, 
                            levels=c(1,2,3,4),
                            labels=c("level one" ,"level two", 
                                     "level three" 
                                     ,"level four"))
str(mydata$Platform.Type)
View(mydata)
str(mydata$Placement...Weekday..Mo...1.)
mydata$Placement...Weekday..Mo...1.=factor(mydata$Placement...Weekday..Mo...1.,
                                           levels=c(1,2,3,4,5,6,7),
                                           labels=c("Monday" ,"Tuesday",
                                                    "Wednesday","Thursday",
                                                    "Friday","Saturday",
                                                    "Sunday"))
mydata$Confirmation...Weekday..Mo...1.=factor(mydata$Confirmation...Weekday..Mo...1.,
                                              levels=c(1,2,3,4,5,6,7),
                                              labels = c("Monday" ,"Tuesday",
                                                         "Wednesday","Thursday",
                                                         "Friday","Saturday",
                                                         "Sunday"))
mydata$Pickup...Weekday..Mo...1.=factor(mydata$Pickup...Weekday..Mo...1.,
                                        levels=c(1,2,3,4,5,6,7),
                                        labels=c("Monday" ,"Tuesday",
                                                 "Wednesday","Thursday",
                                                 "Friday","Saturday",
                                                 "Sunday"))
mydata$Arrival.at.Pickup...Weekday..Mo...1.=factor(mydata$Arrival.at.Pickup...Weekday..Mo...1.,
                                                   levels=c(1,2,3,4,5,6,7),
                                                   labels=c("Monday" ,"Tuesday",
                                                            "Wednesday","Thursday",
                                                            "Friday","Saturday",
                                                            "Sunday"))
View(as.numeric(mydata$Arrival.at.Pickup...Weekday..Mo...1.))



mydata$Confirmation...Time <- as.character(mydata$Confirmation...Time)
str(mydata$Confirmation...Time)
Confirmation...Time=format(strptime(mydata$Confirmation...Time, "%I:%M:%S %p"), format="%H:%M:%S")
View(Confirmation...Time)
str(Confirmation...Time)
library(chron)
mydata$Confirmation...Time=chron(times=Confirmation...Time)
View(mydata$Confirmation...Time)
str(mydata$Confirmation...Time)



mydata$Arrival.at.Pickup...Time=as.character(mydata$Arrival.at.Pickup...Time)
str(mydata$Arrival.at.Pickup...Time)
Arrival.at.Pickup...Time=format(strptime(mydata$Arrival.at.Pickup...Time, "%I:%M:%S %p"), format="%H:%M:%S")
View(Arrival.at.Pickup...Time)
str(Arrival.at.Pickup...Time)
library(chron)
mydata$Arrival.at.Pickup...Time=chron(times=Arrival.at.Pickup...Time)
View(mydata$Arrival.at.Pickup...Time)
str(mydata$Arrival.at.Pickup...Time)


mydata$Placement...Time=as.character(mydata$Placement...Time)
str(mydata$Placement...Time)
Placement...Time=format(strptime(mydata$Placement...Time, "%I:%M:%S %p"), format="%H:%M:%S")
View(Placement...Time)
str(Placement...Time)
library(chron)
mydata$Placement...Time=chron(times=Placement...Time)
View(mydata$Placement...Time)
str(mydata$Placement...Time)


mydata$Pickup...Time=as.character(mydata$Pickup...Time)
str(mydata$Pickup...Time)
Pickup...Time=format(strptime(mydata$Pickup...Time, "%I:%M:%S %p"), format="%H:%M:%S")
View(Pickup...Time)
str(Pickup...Time)
library(chron)
mydata$Pickup...Time=chron(times=Pickup...Time)
View(mydata$Pickup...Time)
str(mydata$Pickup...Time)



mydata$Placement...Day.of.Month=as.factor(mydata$Placement...Day.of.Month)
mydata$Confirmation...Day.of.Month=as.factor(mydata$Confirmation...Day.of.Month)
mydata$Arrival.at.Pickup...Day.of.Month=as.factor(mydata$Arrival.at.Pickup...Day.of.Month)
mydata$Pickup...Day.of.Month=as.factor(mydata$Pickup...Day.of.Month)

traindata=mydata
  View(traindata)

  
  write.csv(mydata,'CLEANTRAIN.csv')
  
  time2 <- ifelse(mydata$Pickup...Time >= '05:00:00' & mydata$Pickup...Time <= '11:00:00', "Morning",
                             ifelse(mydata$Pickup...Time > '11:00:00' & mydata$Pickup...Time<= '16:00:00', "Afternoon",
                                    ifelse(mydata$Pickup...Time > '16:00:00' & mydata$Pickup...Time <= '19:00:00', "Evening", "Night")))
  View(time2)
  View(data.frame(time2,mydata$Pickup...Time))
  
  traindata1=data.frame(time2, traindata)
  View(traindata1)
  
 
  
  ordered <- traindata1 %>% select(Order.No, everything())
  View(ordered )
  
  
 
  
  names(traindata1)[23] <- "TimeTaken"
  View(traindata1)
  colnames(traindata1)
  
  colnames(traindata1)
  traindata1 <- traindata1 %>% select(TimeTaken, everything())
  View(traindata1 )
  attach(traindata1)
  
  
  
  
  
  
  
 dt= sort(sample(nrow(traindata1), nrow(traindata1)*.7))
  train<-traindata1[dt,]
  test<-traindata1[-dt,]
  View(test)
  View(train)
  attach(train)
  attach(test)
  
  
  
  
  
  
  
  library(rpart)
  fit1 <- rpart(TimeTaken ~ Distance..KM.+Personal.or.Business+Pickup...Time+
                  Platform.Type+time2+
                 Pickup...Day.of.Month+
                  Destination.Lat+Pickup.Lat+
                  Destination.Long+Pickup.Long+Pickup...Weekday..Mo...1.,
                method="anova", data=train)
  p1=predict(fit1,test)
  #View(p1)
  MAE=function(actual, predicted){mean(abs(actual-predicted))}
  MAE(test$TimeTaken,p1)
  library(Metrics)
  rmse(test$TimeTaken,p1)
  
  library(rpart)
  fit2 <- rpart(TimeTaken ~ Distance..KM.+Personal.or.Business+Pickup...Time+
                  time2+
                  Pickup...Day.of.Month+
                  Destination.Lat+Pickup.Lat+
                  Destination.Long+Pickup.Long+Pickup...Weekday..Mo...1.,
                method="anova", data=train)
  fit2
  p2=predict(fit2,test)
 # View(p2)
  MAE=function(actual, predicted){mean(abs(actual-predicted))}
  MAE(test$TimeTaken,p2)
  library(Metrics)
  rmse(test$TimeTaken,p2)
  
  
  
  library(rpart)
  fit3 <- rpart(TimeTaken ~ Distance..KM.+
                  Personal.or.Business+Pickup...Time+
                  as.numeric(Pickup...Day.of.Month)+
                  Temperature+
                  Pickup...Time+
                  Pickup.Long+
                  Pickup.Lat+
                  Pickup...Weekday..Mo...1.,
                method="anova", data=train)
  fit3
  p3=predict(fit3,test)
  # View(p3)
  MAE=function(actual, predicted){mean(abs(actual-predicted))}
  MAE(test$TimeTaken,p3)
  library(Metrics)
  rmse(test$TimeTaken,p3)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
 
  
  
  
  model1 <- lm(TimeTaken ~ Distance..KM.+
                Personal.or.Business+Pickup...Time+Pickup.Lat+Pickup.Long+
                +Destination.Lat+Destination.Long+Platform.Type+time2+
                 Pickup...Weekday..Mo...1., data = train)
  
  summary(model1)
  m1=predict(model1,test)
  View(m1)
  MAE=function(actual, predicted){mean(abs(actual-predicted))}
  MAE(test$TimeTaken,m1)
  library(Metrics)
  rmse(test$TimeTaken,m1)
  
  
  
  model2 <- lm(TimeTaken ~ Distance..KM.+
                 Personal.or.Business+Pickup...Time+Pickup.Long+
                 +Destination.Lat+Pickup.Lat+
                 Pickup...Weekday..Mo...1., data = train)
  
  summary(model2)
  m2=predict(model2,test)
  #View(m2)
  MAE=function(actual, predicted){mean(abs(actual-predicted))}
  MAE(test$TimeTaken,m2)
  library(Metrics)
  rmse(test$TimeTaken,m2)
  
  
  
  
  model3 <- lm(TimeTaken ~ Distance..KM.+
                 as.numeric(Personal.or.Business)+
                 Temperature+
                 time2+
                 as.numeric(Platform.Type)+
                 Pickup...Time+
                 Pickup.Long+
                 Pickup.Lat+
                 as.numeric(Pickup...Weekday..Mo...1.)+
                 as.numeric(Pickup...Day.of.Month),
                  data = train)
  
  summary(model3)
  m3=predict(model3,test)
  #View(m3)
  MAE=function(actual, predicted){mean(abs(actual-predicted))}
  MAE(train$TimeTaken,m3)
  library(Metrics)
  rmse(test$TimeTaken,m3)
  
  
  
  
  
  
  
  # boxplot(train$TimeTaken)
  # OutVals = boxplot(train$TimeTaken)$out
  # OutVals
  # which(train$TimeTaken %in% OutVals)
  # shapiro.test(train$TimeTaken)
  # 
  # 
  # #Do shapiro test with only the first 5000 records
  # shapiro.test(train$TimeTaken[0:5000])
  # shapiro.test(log10(log10(train$TimeTaken[0:5000])))
  # shapiro.test(1/(train$TimeTaken[0:5000]))
  # shapiro.test(sample(train$TimeTaken,length(train$TimeTaken),prob = 0.5,size = 100,replace = FALSE))
  # 
  # #Anderson-Darling normality test
  # library(nortest)
  # ad.test(train$TimeTaken)
  # 
  # library(nortest)
  # ad.test(log(train$Temperature))
  # library(nortest)
  # ad.test(log10(train$Distance..KM.))
  # library(nortest)
  # ad.test( train$Platform.Type)
  # library(normalr)
  # nor=normalise(train$Temperature, method = "standardize", range = c(0, 10), margin = 1L, on.constant = "quiet")
  # library(nortest)
  # ad.test(nor)
  # 
  
str(traindata1)
View(traindata1)



#Importing data to r checking the structure and na values
mydatatest=read.csv(file.choose(), header = T)
columns=View(colnames(mydatatest))


attach(mydatatest)
library(dplyr)

na_count <-sapply(mydatatest, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

mydatatest=select(mydatatest,-Precipitation.in.millimeters)
select(mydatatest, mydatatest$Order.No=="Order_No_768")
test<-subset(mydatatest, mydatatest$Order.No=='Order_No_21373')
View(test)
#mydatatest=na.omit(mydatatest)
test<-subset(mydatatest, mydatatest$Order.No=='Order_No_21373')
View(test)
mydatatest$Temperature=na.replace(mydatatest$Temperature,
                                  median(na.omit(mydatatest$Temperature)))

na_count1 <-sapply(mydatatest, function(y) sum(length(which(is.na(y)))))
na_count1 <- data.frame(na_count1)
na_count1


View(mydatatest)
mydatatest=select(mydatatest,-User.Id,-Vehicle.Type,-Rider.Id)
View(mydatatest)


mydatatest$Platform.Type=factor(mydatatest$Platform.Type, 
                            levels=c(1,2,3,4),
                            labels=c("level one" ,"level two", 
                                     "level three" 
                                     ,"level four"))
str(mydatatest$Platform.Type)
View(mydatatest)
str(mydatatest$Placement...Weekday..Mo...1.)
mydatatest$Placement...Weekday..Mo...1.=factor(mydatatest$Placement...Weekday..Mo...1.,
                                           levels=c(1,2,3,4,5,6,7),
                                           labels=c("Monday" ,"Tuesday",
                                                    "Wednesday","Thursday",
                                                    "Friday","Saturday",
                                                    "Sunday"))
mydatatest$Confirmation...Weekday..Mo...1.=factor(mydatatest$Confirmation...Weekday..Mo...1.,
                                              levels=c(1,2,3,4,5,6,7),
                                              labels = c("Monday" ,"Tuesday",
                                                         "Wednesday","Thursday",
                                                         "Friday","Saturday",
                                                         "Sunday"))
mydatatest$Pickup...Weekday..Mo...1.=factor(mydatatest$Pickup...Weekday..Mo...1.,
                                        levels=c(1,2,3,4,5,6,7),
                                        labels=c("Monday" ,"Tuesday",
                                                 "Wednesday","Thursday",
                                                 "Friday","Saturday",
                                                 "Sunday"))
mydatatest$Arrival.at.Pickup...Weekday..Mo...1.=factor(mydatatest$Arrival.at.Pickup...Weekday..Mo...1.,
                                                   levels=c(1,2,3,4,5,6,7),
                                                   labels=c("Monday" ,"Tuesday",
                                                            "Wednesday","Thursday",
                                                            "Friday","Saturday",
                                                            "Sunday"))

mydatatest$Confirmation...Time <- as.character(mydatatest$Confirmation...Time)
str(mydatatest$Confirmation...Time)
Confirmation...Time=format(strptime(mydatatest$Confirmation...Time, "%I:%M:%S %p"), format="%H:%M:%S")
View(Confirmation...Time)
str(Confirmation...Time)
library(chron)
mydatatest$Confirmation...Time=chron(times=Confirmation...Time)
View(mydatatest$Confirmation...Time)
str(mydatatest$Confirmation...Time)



mydatatest$Arrival.at.Pickup...Time=as.character(mydatatest$Arrival.at.Pickup...Time)
str(mydatatest$Arrival.at.Pickup...Time)
Arrival.at.Pickup...Time=format(strptime(mydatatest$Arrival.at.Pickup...Time, "%I:%M:%S %p"), format="%H:%M:%S")
View(Arrival.at.Pickup...Time)
str(Arrival.at.Pickup...Time)
library(chron)
mydatatest$Arrival.at.Pickup...Time=chron(times=Arrival.at.Pickup...Time)
View(mydatatest$Arrival.at.Pickup...Time)
str(mydatatest$Arrival.at.Pickup...Time)


mydatatest$Placement...Time=as.character(mydatatest$Placement...Time)
str(mydatatest$Placement...Time)
Placement...Time=format(strptime(mydatatest$Placement...Time, "%I:%M:%S %p"), format="%H:%M:%S")
View(Placement...Time)
str(Placement...Time)
library(chron)
mydatatest$Placement...Time=chron(times=Placement...Time)
View(mydatatest$Placement...Time)
str(mydatatest$Placement...Time)


mydatatest$Pickup...Time=as.character(mydatatest$Pickup...Time)
str(mydatatest$Pickup...Time)
Pickup...Time=format(strptime(mydatatest$Pickup...Time, "%I:%M:%S %p"), format="%H:%M:%S")
View(Pickup...Time)
str(Pickup...Time)
library(chron)
mydatatest$Pickup...Time=chron(times=Pickup...Time)
View(mydatatest$Pickup...Time)
str(mydatatest$Pickup...Time)



mydatatest$Placement...Day.of.Month=as.factor(mydatatest$Placement...Day.of.Month)
mydatatest$Confirmation...Day.of.Month=as.factor(mydatatest$Confirmation...Day.of.Month)
mydatatest$Arrival.at.Pickup...Day.of.Month=as.factor(mydatatest$Arrival.at.Pickup...Day.of.Month)
mydatatest$Pickup...Day.of.Month=as.factor(mydatatest$Pickup...Day.of.Month)

time2 <- ifelse(mydatatest$Pickup...Time >= '05:00:00' & mydatatest$Pickup...Time <= '11:00:00', "Morning",
                ifelse(mydatatest$Pickup...Time > '11:00:00' & mydatatest$Pickup...Time<= '16:00:00', "Afternoon",
                       ifelse(mydatatest$Pickup...Time > '16:00:00' & mydatatest$Pickup...Time <= '19:00:00', "Evening", "Night")))
View(time2)
View(data.frame(time2,mydatatest$Pickup...Time))

Testdata=data.frame(time2, mydatatest)
View(Testdata)
test<-subset(Testdata, Testdata$Order.No=='Order_No_768')
View(test)



# ordered <- Testdata %>% select(Order.No, everything())
# View(ordered )



# 
# names(Testdata)[23] <- "TimeTaken"
# View(traindata1)
# colnames(Testdata)

colnames(Testdata)
Testdata<- Testdata %>% select(Order.No, everything())
str(Testdata)
str(traindata1)


library(rpart)
MODEL <- rpart(TimeTaken ~ Distance..KM.+Personal.or.Business+Pickup...Time+
                Platform.Type+time2+
                Pickup...Day.of.Month+
                Destination.Lat+Pickup.Lat+
                Destination.Long+Pickup.Long+Pickup...Weekday..Mo...1.,
              method="anova", data=traindata1)
MODEL
summary(MODEL)
PREDICTED=predict(MODEL,Testdata)
View(PREDICTED)


MODEL1 <- lm(TimeTaken ~ Distance..KM.+
               as.numeric(Personal.or.Business)+Pickup...Time+Pickup.Long+
               Pickup.Lat+
               as.numeric(Pickup...Day.of.Month)+
               as.numeric(Pickup...Weekday..Mo...1.), data = traindata1)

summary(MODEL1)
PREDICTED1=predict(MODEL1,Testdata)
View(PREDICTED1)
submissionfile=data.frame(" Time from Pickup to Arrival"=PREDICTED1,"Order_No "=Testdata$Order.No)
View(submissionfile)
# names(submissionfile)[1] <- " Time from Pickup to Arrival"
# names(submissionfile)[2] <- " Order_No"
attach(submissionfile)
#submissionfile<-submissionfile%>% select(Order_No, everything())
submissionfile<- submissionfile[, c(2, 1)]
View(submissionfile)
test1<-subset(submissionfile, submissionfile$Order_No=='Order_No_11299')
View(test1)

write.csv(submissionfile,'First Submission.csv',row.names=FALSE)





write.csv(submissionfile,'C:/Users/Administrator/Documents/SendyLogistics/file.csv',index=False)
check=read.csv(file.choose(), header = T)
check
test1<-subset(check, check$Order.No=='Order_No_1199')
View(test1)




#Rider Data
Riderdata=read.csv(file.choose(), header = T)
View(Riderdata)
na_count1 <-sapply(Riderdata, function(y) sum(length(which(is.na(y)))))
na_count1 <- data.frame(na_count1)
na_count1

merge=read.csv(file.choose(), header = T)
attach(merge)
library(na.tools)
library(dplyr)
merge=select(merge,-Precipitation.in.millimeters)
mid=median(na.omit(merge$Temperature))
merge$Temperature=na.replace(merge$Temperature, mid)
na_count1 <-sapply(merge, function(y) sum(length(which(is.na(y)))))
na_count1 <- data.frame(na_count1)
na_count1
attach(merge)
merge1=data.frame(Rider.Id=merge$Rider.Id,TimeTaken=merge$Time.from.Pickup.to.Arrival)
View(merge1)
Totaldata=merge(Riderdata,merge1,by='Rider.Id')
View(Totaldata)
na_count1 <-sapply(Totaldata, function(y) sum(length(which(is.na(y)))))
na_count1 <- data.frame(na_count1)
na_count1
final=mutate(Totaldata,Distance..KM.=merge$Distance..KM.)
View(final)
library("PerformanceAnalytics")
final<- final[, c(2,3,4,5,6,7)]
a=chart.Correlation(final, histogram=TRUE, pch=19)

a

