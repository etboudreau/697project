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


###PREP/MUNGE###
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

#convert existing date and time to standard POSIX format
data3 <- data3 %>%
  mutate(crash_time_2 = str_replace(crash_time_2, "\\s(AM|PM)", " \\1"),  # Remove the space before AM/PM
         crash_time_2 = as.POSIXct(crash_time_2, format = "%I:%M %p"),  # Convert to POSIXct format
         crash_date = as.Date(date))  # Convert 'date' to Date format
#mutate data3 to standard time 
data3 <- data3 %>%
  mutate(crash_date_time_standard = as.POSIXct(paste(crash_date, format(crash_time_2, "%H:%M:%S")), format = "%Y-%m-%d %H:%M:%S"))
#drop the excess columns we don't need anymore and make new polished data frame
data4 <- data3 %>%
  select(-crash_date, -date, -crash_time_2,-crash_numb,-city_town_name)
#drop_na2()%>% #drop any row with NA






###BASIC ANALYSIS###
###Crash count vs time of day###
# Do some analysis on the timing of events
# Extract the hour from the crash_date_time_standard column
data4 <- data4 %>%
  mutate(hour = hour(crash_date_time_standard))

# Calculate the crash count for each hour
crash_count <- data4 %>%
  count(hour)

# Create a bar plot for crash count by hour
barplot(crash_count$n, names.arg = crash_count$hour, xlab = "Hour of Day", ylab = "Crash Count",
        main = "Crash Count by Hour", col = "skyblue", border = "black", ylim = c(0, max(crash_count$n) + 10))



###Location in Boston map###
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
  geom_point(data = data4, aes(x = lon, y = lat), color = "blue", alpha = 0.5) +
  #adjust the transparency and color of the points
  guides(alpha = FALSE) +
  labs(title = "Crashes in Boston, MA") +
  theme(plot.title = element_text(hjust = 0.5))








###MODEL ANALYSIS###
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



#---- SVM-Radial 
library(e1071)
# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(data4), 0.7 * nrow(data4))  # 70% for training
train_data <- data4[train_indices, ]
test_data <- data4[-train_indices, ]

# Train the SVM model
svm_model <- svm(formula = severity ~ ., kernel = "radial", data = train_data)
# Make predictions on the test data
y_pred <- predict(svm_model, newdata = test_data)
y_pred <- ifelse(y_pred >= 0.5, "1", "0")
# Calculate accuracy
accuracy <- sum(y_pred == test_data$severity) / nrow(test_data)
print(paste("Accuracy:", accuracy))
# Calculate confusion matrix
confusion <- table(Actual = test_data$severity, Predicted = y_pred)
print("Confusion Matrix:")
print(confusion)
# Calculate precision, recall, and F1-score
#precision <- confusion[2, 2] / sum(confusion[, 2])
#recall <- confusion[2, 2] / sum(confusion[2, ])
f1_score <- 2 * precision * recall / (precision + recall)
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1-Score:", f1_score))
# Calculate AUC-ROC (if applicable)
if (length(levels(test_data$severity)) == 2) {
  library(pROC)
  roc_obj <- roc(test_data$severity, as.numeric(y_pred))
  auc_roc <- auc(roc_obj)
  print(paste("AUC-ROC:", auc_roc))
}
# Calculate training accuracy
train_predictions <- predict(svm_model, newdata = train_data)
train_accuracy <- sum(train_predictions == train_data$severity) / nrow(train_data)
print(paste("Training Accuracy:", train_accuracy))

# Calculate testing accuracy
test_accuracy <- sum(y_pred == test_data$severity) / nrow(test_data)
print(paste("Testing Accuracy:", test_accuracy))

# Calculate True Negative Rate (TNR)
TN <- confusion[1, 1]  # True negatives
FP <- confusion[1, 2]  # False positives
TNR <- TN / (TN + FP)
print(paste("TNR:", TNR))







###SUBSEQUENT ANALYSIS###

#---- GGplotsss

season_counts <- data4 %>%
  group_by(season, severity) %>%
  summarise(count = n()) %>%
  ungroup()

#season labels
season_counts$season <- case_when(
  season_counts$season == "0" ~ "Winter",
  season_counts$season == "1" ~ "Spring",
  season_counts$season == "2" ~ "Summer",
  season_counts$season == "3" ~ "Fall"
)

#bar plot for season
pl1 = ggplot(season_counts, aes(x = season, y = count, fill = as.factor(severity))) +
  geom_bar(stat = "identity") +
  labs(x = "Season", y = "Count", color = "grey", fill = "Severity") +
  geom_text(aes(label=count),vjust=1.6,
            size=3.5, color = "white") +
  scale_fill_brewer(palette = "Paired",
                    labels = c("Non-Injury", "Injury")) +
  theme_minimal()

#injury (severity 1) and non-injury (severity 0) in each weather condition
weather_counts <- data4 %>%
  group_by(weather, severity) %>%
  summarise(count = n()) %>%
  ungroup()

#weather code
weather_counts$weather <- case_when(
  weather_counts$weather == 0 ~ "Clear",
  weather_counts$weather == 1 ~ "Cloudy",
  weather_counts$weather == 2 ~ "Rain",
  weather_counts$weather == 3 ~ "Snow"
)

#bar plot
pl2 = ggplot(weather_counts, aes(x = weather, y = count, fill = as.factor(severity))) +
  geom_bar(stat = "identity") +
  labs(x = "Weather Condition", y = "Count",color = "grey", fill = "Severity") +
  geom_text(aes(label=count),vjust=1.6,
            size=3.5, color = "white") +
  scale_fill_brewer(palette = "Paired",
                    labels = c("Non-Injury", "Injury")) +
  theme_minimal()


# Calculate the count of injury and non-injury occurrences for each speed limit
speed_limit_counts <- data4 %>%
  group_by(speed_limit, severity) %>%
  summarise(count = n()) %>%
  ungroup()

# Plot the scatter plot
pl3 = ggplot(speed_limit_counts, aes(x = speed_limit, y = count, color = as.factor(severity))) +
  geom_point() +
  labs(x = "Speed Limit", y = "Count", color = "Severity") +
  scale_fill_brewer(palette = "Paired",
                    labels = c("Non-Injury", "Injury")) +
  theme_minimal()


# Define the age categories
age_categories <- c("<16", "16-20", "21-24", "25-34", "35-44", "45-54", "55-64", "65-74", "74+")

# Categorize the driver's age into age categories
data4 <- data4 %>%
  mutate(age_category = case_when(
    driver_age < 16 ~ "<16",
    driver_age >= 16 & driver_age <= 20 ~ "16-20",
    driver_age >= 21 & driver_age <= 24 ~ "21-24",
    driver_age >= 25 & driver_age <= 34 ~ "25-34",
    driver_age >= 35 & driver_age <= 44 ~ "35-44",
    driver_age >= 45 & driver_age <= 54 ~ "45-54",
    driver_age >= 55 & driver_age <= 64 ~ "55-64",
    driver_age >= 65 & driver_age <= 74 ~ "65-74",
    driver_age >= 75 ~ "74+"
  ))

# Calculate the count of injury and non-injury occurrences for each age category
age_counts <- data4 %>%
  group_by(age_category, severity) %>%
  summarise(count = n()) %>%
  ungroup()

# Plot the bar plot
pl4 = ggplot(age_counts, aes(x = age_category, y = count, fill = as.factor(severity))) +
  geom_bar(stat = "identity") +
  labs(x = "Age Category", y = "Count", fill = "Severity") +
  scale_fill_brewer(palette = "Paired",
                    labels = c("Non-Injury", "Injury"))  +
  theme_minimal() +
  scale_x_discrete(labels = age_categories)

library(ggpubr)
ggarrange(pl1, pl2, pl4, ncol = 2,nrow = 2)




