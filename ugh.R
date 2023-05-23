# script to read in raw data and organize it

library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(stringi)
library(lubridate)


library(ggmap)

library(ggplot2)

library(caTools)

#setwd('/Users/emmaboudreau/Documents/GitHub/697proj/')
setwd('/Users/samuelesquivel/Documents/GitHub/697project/')

# read in the data-----
data = read.csv('export.csv')


#---- 1 dataframe we could use
data2 = select(data,-c(dist_dirc_exit, age, max_injr_svrty_cl,
                       numb_fatal_injr, numb_nonfatal_injr,injy_stat_descr,
                       vehc_unit_numb,crash_status,max_injr_svrty_vl, pers_numb))%>%
  filter(!is.na(speed_limit))%>% #filter out anything without speed limit
  rename("severity" = "crash_severity_descr")%>% #changed column name
  rename("weather" = "weath_cond_descr")%>%
  mutate(severity=ifelse(severity=="Property damage only (none injured)",0, 
                         ifelse(severity=="Non-fatal injury",1,ifelse(severity=="Fatal injury",2,3)
                         )
  )
  )%>%
  filter(severity!="3")%>% #remove any unreported or unknown severity levels
  filter(severity!="2")%>% #remove fatal injuries
  drop_na()%>% #drop any row with NA
  mutate(weather = ifelse(weather =="Clear/Clear","Clear",ifelse(weather == "Rain/Rain","Rain",
                                                                 ifelse(weather=="Not Reported","Unknown",ifelse(weather=="Snow/Snow","Snow",
                                                                                                                 ifelse(weather=="Cloudy/Rain","Rain",ifelse(weather=="Clear/Cloudy", "Cloudy",
                                                                                                                                                             ifelse(weather=="Snow/Cloudy","Snow",ifelse(weather=="Clear/Blowing sand, snow","Snow",
                                                                                                                                                                                                         ifelse(weather=="Rain/Sleet, hail (freezing rain or drizzle)","Rain", ifelse(weather=="Snow/Blowing sand, snow", "Snow",
                                                                                                                                                                                                                                                                                      ifelse(weather=="Clear/Rain","Rain",ifelse(weather=="Cloudy/Cloudy","Cloudy",
                                                                                                                                                                                                                                                                                                                                 ifelse(weather=="Rain/Cloudy","Rain",ifelse(weather=="Rain/Severe crosswinds","Rain",
                                                                                                                                                                                                                                                                                                                                                                             ifelse(weather=="Snow/Sleet, hail (freezing rain or drizzle)", "Snow",weather)))))))))))
                                                                 )))))%>% #concatenating weather conditions 
  mutate(weather=ifelse(weather=="Clear",0, 
                        ifelse(weather=="Cloudy",1,ifelse(weather=="Snow",2, 
                                                          ifelse(weather=="Rain",3,
                                                                 ifelse(weather=="Unknown",5,6)
                                                          )
                        )
                        )
  )
  )%>% #creating numerical code for different weather conditions
  #filter(weather!="5")
  filter(weather!="5")%>% #filtering out any unknown or other weather descriptions that are not frequently used/ambiguous
  filter(weather!="6")

WS<-as.Date("12/15/2018", format=  "%m/%d/%Y") #winter solstice
SE<-as.Date("03/15/2018", format=  "%m/%d/%Y") #spring equinox
SS<-as.Date("06/15/2018", format=  "%m/%d/%Y") #summer solstice
FE<-as.Date("09/15/2018", format=  "%m/%d/%Y") #fall equinox

#seasons code: winter = 0, spring = 1, summer = 2, fall = 3  
data3 = data2%>%  
  rename("date"="crash_date")%>%
  mutate(date = as.Date(date,format= "%m/%d/%Y"))%>%
  mutate(season = ifelse(date>=WS | date<SE, "0", ifelse(date>=SE & date< SS, "1",
                                                         ifelse(date>=SS&date< FE, "2", "3"))))


###REGARDING TIME###
#step 1
#convert existing date and time to standard POSIX format
data3 <- data3 %>%
  mutate(crash_time_2 = str_replace(crash_time_2, "\\s(AM|PM)", " \\1"),  # Remove the space before AM/PM
         crash_time_2 = as.POSIXct(crash_time_2, format = "%I:%M %p"),  # Convert to POSIXct format
         crash_date = as.Date(date))  # Convert 'date' to Date format

data3 <- data3 %>%
  mutate(crash_date_time_standard = as.POSIXct(paste(crash_date, format(crash_time_2, "%H:%M:%S")), format = "%Y-%m-%d %H:%M:%S"))
#step 2
#drop the excess columns we don't need anymore and make new polished data frame
data4 <- data3 %>%
  select(-crash_date, -date, -crash_time_2,-crash_numb,-city_town_name)
#drop_na2()%>% #drop any row with NA
#step 3
#do some analysis on the timing of events
#extract the hour from the crash_date_time_standard column
data4 <- data4 %>%
  mutate(hour = hour(crash_date_time_standard))
#calculate the crash count for each hour
crash_count <- data4 %>%
  count(hour)
#plot the crash count by hour
plot(crash_count$hour, crash_count$n, type = "l", xlab = "Hour of Day", ylab = "Crash Count", main = "Crash Count by Hour")


###REGARDING LOCATION###
# Load required libraries
library(ggplot2)
library(ggmap)
#set the latitude and longitude boundaries for Boston area
boston_bounds <- c(left = -71.1912, bottom = 42.2279, right = -70.8085, top = 42.3974)
#get the map background using ggmap and specify the map type
boston_map <- get_stamenmap(boston_bounds, maptype = "toner-lite")
#plot the map of Boston
ggmap(boston_map) +
  #add points representing crash locations
  geom_point(data = data4, aes(x = lon, y = lat), color = "red", alpha = 0.5) +
  #adjust the transparency and color of the points
  guides(alpha = FALSE) +
  labs(title = "Crashes in Boston, MA") +
  theme(plot.title = element_text(hjust = 0.5))





#---- logistic regression

data4 <- na.omit(data4)

# Loading caret library
library(caret)
# Splitting the data into train and test
index <- createDataPartition(data4$severity, p = .70, list = FALSE)
train <- data4[index, ]
test <- data4[-index, ]
# Training the model
logistic_model <- glm(severity ~ ., family = binomial(), train)
# Checking the model
summary(logistic_model)


pred_prob_lg <- predict(logistic_model, test, type = "response")




train$pred_class <- ifelse(logistic_model$fitted.values>=0.5, "Yes", "No")
# Generating the classification table
ctab_train <- table(train$severity, train$pred_class)
ctab_train

# Converting from probability to actual output
test$pred_class <- ifelse(pred_prob_lg >= 0.5, "Yes", "No")
# Generating the classification table
ctab_test <- table(test$severity, test$pred_class)
ctab_test



#Accuracy = (TP + TN)/(TN + FP + FN + TP)
# Accuracy in Training dataset
accuracy_train <- sum(diag(ctab_train))/sum(ctab_train)*100
accuracy_train


# Accuracy in Test dataset
accuracy_test <- sum(diag(ctab_test))/sum(ctab_test)*100
accuracy_test

Recall <- (ctab_train[2, 2]/sum(ctab_train[2, ]))*100
Recall


TNR <- (ctab_train[1, 1]/sum(ctab_train[1, ]))*100
TNR

Precision <- (ctab_train[2, 2]/sum(ctab_train[, 2]))*100
Precision

F_Score <- (2 * Precision * Recall / (Precision + Recall))/100
F_Score



#library(pROC)
#roc <- roc(train$Class, logistic_model$fitted.values)
#auc(roc)



#---- SVM
library(e1071)


svm_model <- svm(formula = severity ~ ., kernel = "radial", data = train)


y_pred = predict(svm_model, newdata = test)

y_pred = ifelse(y_pred>=.5, "1","0")

a = 100*mean(y_pred==test[,1])

accuracy <- sum(y_pred == test$severity) / nrow(test)
print(paste("Accuracy:", accuracy))


