************************************************
*	Author : Ankur Singh & Shashank Gupta  *
*	Programming Lang. : R programming      *
*	Model : Xgboost                        *
*************************************************


#Load Data
train=read.csv("Data/Train.csv",header = T)
test=read.csv("Data/Test.csv",header = T)

summary(train)

str(train)

#Clean Data
library(dplyr)
allData<-bind_rows(train,test)

impute<-allData[c("Direction_Of_Wind","Average_Breeze_Speed","Max_Breeze_Speed","Min_Breeze_Speed"
                  ,"Var1","Average_Atmospheric_Pressure","Max_Atmospheric_Pressure",
                  "Min_Atmospheric_Pressure","Min_Ambient_Pollution","Max_Ambient_Pollution",
                  "Average_Moisture_In_Park","Max_Moisture_In_Park","Min_Moisture_In_Park")]

library('mice')

set.seed(144)

imputed=complete(mice(impute,m=1))

allData$Direction_Of_Wind=imputed$Direction_Of_Wind
allData$Average_Breeze_Speed=imputed$Average_Breeze_Speed
allData$Max_Breeze_Speed=imputed$Max_Breeze_Speed
allData$Min_Breeze_Speed=imputed$Min_Breeze_Speed
allData$Var1=imputed$Var1
allData$Average_Atmospheric_Pressure=imputed$Average_Atmospheric_Pressure
allData$Max_Atmospheric_Pressure=imputed$Max_Atmospheric_Pressure
allData$Min_Atmospheric_Pressure=imputed$Min_Atmospheric_Pressure
allData$Min_Ambient_Pollution=imputed$Min_Ambient_Pollution
allData$Max_Ambient_Pollution=imputed$Max_Ambient_Pollution
allData$Average_Moisture_In_Park=imputed$Average_Moisture_In_Park
allData$Max_Moisture_In_Park=imputed$Max_Moisture_In_Park
allData$Min_Moisture_In_Park=imputed$Min_Moisture_In_Park

#Date
datetxt <- allData[,3]
Date <- format(as.Date(datetxt,format="%d-%m-%Y"), "%d")
Month <- format(as.Date(datetxt,format="%d-%m-%Y"), "%m")
Year <- format(as.Date(datetxt,format="%d-%m-%Y"), "%Y")
allData$Date1 <-as.integer(Date)
allData$Month <- as.integer(Month)
allData$Year <- as.integer(Year)

#Weekend
df <- data.frame(allData$Date)
df <- as.Date(df$allData.Date,"%d-%m-%Y")
weekend <- as.POSIXlt(df)$wday
weekend1 <- ifelse(weekend==6|weekend==0,1,0)
allData$weekend <- weekend1



summary(allData)

train_data <- allData[1:114539,]
test_data<- allData[114540:153959,]
test_data$Footfall<- NULL




feature.names <- names(train_data)
feature.names <- feature.names[feature.names!= "ID" &
                                 feature.names!=  "Date" &
                                 feature.names!=  "Date1" &
                                 feature.names!= "Year" &
                                 feature.names!= "Footfall"
                                 
]

set.seed(1960)
h<-sample(nrow(train_data),floor(0.3*nrow(train_data)))
train_sample <- train_data[-h,]
train_val <- train_data[h,]



dval<-xgb.DMatrix(data=data.matrix(train_val[,feature.names]),label=train_val[,18],missing=NA)
dtrain<-xgb.DMatrix(data=data.matrix(train_sample[,feature.names]),label=train_sample[,18],missing=NA)
watchlist<-list(val=dval,train=dtrain)

xg.test <- test_data[,feature.names]

param <- list(  objective           = "reg:linear", 
                booster             = "gbtree",
                eta                 = 0.48,
                max_depth           = 4, #7
                subsample           = 0.9,
                colsample_bytree    = 0.9
)
set.seed(1429)
clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 2000, 
                    verbose             = 0,
                    early.stop.round    = 100,
                    watchlist           = watchlist,
                    maximize            = TRUE
)

importance_matrix <- xgb.importance(feature.names, model = clf)
xgb.plot.importance(importance_matrix)

pred_noexp=predict(clf,data.matrix(test_data[,feature.names]))

solutionXgBoost<- data.frame(ID = test_data$ID, Footfall = pred_noexp)

write.csv(solutionXgBoost, file = 'solutionXgBoost16x.csv', row.names = F)


