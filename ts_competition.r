library(tidyverse)
library(forecast)
library(seasonal)
library(fpp2)
library(urca)
library(gridExtra)
library(lubridate)
library(astsa)
library(rugarch)
library(tsdl)
library(caret)

#==================
#process


# 1. Data transformation

# 2. Exploratory data analyss /Feature engineering 

#3.  Model fitting 




# loading the data 

train<- read.csv("Train_SU63ISt.csv",stringsAsFactors = FALSE)
test<- read.csv("Test_0qrQsBZ.csv", stringsAsFactors = FALSE)
test$Count<-NA

full_data<- rbind(train,test)

# assesing the data stracutre

str(train)

#checking the number of unique values 
sapply(train,function(x){ length(unique(x)) }) 

#data summary 

summary(train$Count)

train %>% ggplot(aes(x = train$Count)) + geom_histogram()
# The count dstribution is trongly right skewed 


#=================================
#1. Data cleaning 

#splitting data time attriute in date and time 

date_split<- strsplit(full_data$Datetime,' ')
date<- NULL
time<- NULL
for( i in seq(1,length(date_split))){
  
date[i]<-date_split[[i]][1]
time[i]<- date_split[[i]][2]
}

full_data$date<- as.Date(date,format = "%d-%m-%Y")
full_data$time<-time

# number of days in train data 
diff(range(full_data[which(!is.na(full_data$Count)),'date'] ))

# checking  traffic distribution by hour 

full_data %>% filter(is.na(Count) == FALSE ) %>% group_by(time) %>% summarise(count = sum(Count)) %>% 
  ggplot(aes(x = time, y = count)) + geom_bar(stat = 'identity')

#traffic stays constant high from 10 AM until 11 PM, whith high picks during lunch and after working hours 

#=====================================
#Feature Engineering

#creating months of the year, days of the year, weeks of the year, quarter, week days, variables 

full_data$year<- year(full_data$date)
full_data$month<- month(full_data$date)
full_data$day<- day(full_data$date)
full_data$week<- week(full_data$date)
full_data$quarter<- quarter(full_data$date)
full_data$weekdays <- weekdays(full_data$date)
full_data<- mutate(full_data,is_working_day = ifelse(weekdays %in% 
                                                       c("Saturday","Sunday"),0,1), easter =  ifelse(date %in% easter,1,0)  )



# exploratory data analysis 

full_data %>% filter(is.na(Count) == FALSE ) %>% group_by(weekdays,time) %>% summarise(count = mean(Count)) %>% 
  ggplot(aes(x = time, y = count)) + geom_bar(stat = 'identity') + facet_grid(.~weekdays) 
# weekends are the least busy in terms of traffic, the busiest hour on Saturday is at 12 AM, possible due to night out.

full_data %>% filter(is.na(Count) == FALSE ) %>% group_by(month,time) %>% summarise(count = mean(Count)) %>% 
  ggplot(aes(x = time, y = count)) + geom_bar(stat = 'identity') + facet_grid(.~month) + coord_flip()

# during a 12 month period the traffic reach a peack on average at 11 AM and 12 PM, and in the evening 19:00, 20:00
# The traffic  on avereage increase from JUnuary until september and than fall ddrastically in the last three months of the year.
# low traffic time is in general from 12:00 AM untl 9:00 AM.
# August and september are the months with the highest traffic 

full_data %>% filter(is.na(Count) == FALSE ) %>% group_by(quarter,time) %>% summarise(count = mean(Count)) %>% 
  ggplot(aes(x = time, y = count)) + geom_bar(stat = 'identity') + facet_grid(.~quarter) 
# third quarter is the period with the highest traffic, while the 4th quarter is the one with the lowest.

full_data %>% filter(is.na(Count) == FALSE ) %>% group_by(year,time) %>% summarise(count = mean(Count)) %>% 
  ggplot(aes(x = time, y = count)) + geom_bar(stat = 'identity') + facet_grid(.~year) 
# traffic has been increasing exponenitally over the past 3 years.

#==========================================
#creating temp dummy variables for week  days, in order to calculate the number of week days  in each month 

dummywd<- dummyVars(~weekdays, data = full_data)
dummy_weekdays<- predict(dummywd,full_data)
full_data<- data.frame(cbind(full_data,dummy_weekdays))

# calculating the number of weekdays  by month quarter and year 

days_month<-full_data %>% group_by(year,month) %>% 
  summarise(n_month_mondays = sum(weekdaysMonday), 
           n_month_tuesday = sum(weekdaysTuesday),
           n_month_wednesday = sum(weekdaysWednesday),
           n_month_thursday = sum(weekdaysThursday),
           n_month_friday = sum(weekdaysFriday),
           n_month_saturday = sum(weekdaysSaturday),
           n_month_sunday = sum(weekdaysSunday))


quarter_month<-full_data %>% group_by(year,quarter) %>% 
  summarise(n_quarter_mondays = sum(weekdaysMonday), 
            n_quarter_tuesday = sum(weekdaysTuesday),
            n_quarter_wednesday = sum(weekdaysWednesday),
            n_quarter_thursday = sum(weekdaysThursday),
            n_quarter_friday = sum(weekdaysFriday),
            n_quarter_saturday = sum(weekdaysSaturday),
            n_quarter_sunday = sum(weekdaysSunday))

full_data_v2<- full_data %>% left_join(days_month,by = c("year","month")) %>% left_join(quarter_month, by = c("year","quarter"))

#  checking for variables with high correlation
vals<-sapply(full_data_v2,function(x) {is.numeric(x) }  )

numeric_names<-names(vals[which(vals == TRUE & names(vals) != "Count")])

corrs<- cor(full_data_v2[,numeric_names])
high_corr_vars<- findCorrelation(corrs) 





