train_weatherAUS <- weatherAUS_noNA_dates[weatherAUS_noNA_dates$Year%in%c(2009,2010,2011),]
train_weatherAUS <- train_weatherAUS[train_weatherAUS$Location!="Hobart",]
train_weatherAUS <- train_weatherAUS[train_weatherAUS$Location!="Cobar",]

train_weatherAUS <- train_weatherAUS[,2:23]

train_locations <- list()
for (location in unique(train_weatherAUS$Location)) {
  train_locations <- rbind(train_locations,c(location,sum(train_weatherAUS[,2]==location)))
  # print(paste(location,sum(train_weatherAUS[,2]==location)))
}

test_weatherAUS <- weatherAUS_noNA_dates[weatherAUS_noNA_dates$Year%in%c(2012),]
test_weatherAUS <- test_weatherAUS[,2:23]

test_locations <- list()
for (location in unique(train_weatherAUS$Location)) {
  test_locations <- rbind(test_locations,c(location,sum(test_weatherAUS[,2]==location)))
  # print(paste(location,sum(test_weatherAUS[,2]==location)))
}

# data$age[which(data$age=="18-24")] <- "20"
# data$age[which(data$age=="35-44")] <- "40"
# data$age[which(data$age=="45-54")] <- "50"

train_weatherAUS$RainToday[which(train_weatherAUS$RainToday=="No")] <- 0
train_weatherAUS$RainToday[which(train_weatherAUS$RainToday=="Yes")] <- 1

train_weatherAUS$RainTomorrow[which(train_weatherAUS$RainTomorrow=="No")] <- 0
train_weatherAUS$RainTomorrow[which(train_weatherAUS$RainTomorrow=="Yes")] <- 1

train_weatherAUS$WindGustDir[which(train_weatherAUS$WindGustDir=="N")] <- 1
train_weatherAUS$WindGustDir[which(train_weatherAUS$WindGustDir=="NNE")] <- 2
train_weatherAUS$WindGustDir[which(train_weatherAUS$WindGustDir=="NE")] <- 3
train_weatherAUS$WindGustDir[which(train_weatherAUS$WindGustDir=="ENE")] <- 4
train_weatherAUS$WindGustDir[which(train_weatherAUS$WindGustDir=="E")] <- 5
train_weatherAUS$WindGustDir[which(train_weatherAUS$WindGustDir=="ESE")] <- 6
train_weatherAUS$WindGustDir[which(train_weatherAUS$WindGustDir=="SE")] <- 7
train_weatherAUS$WindGustDir[which(train_weatherAUS$WindGustDir=="SSE")] <- 8
train_weatherAUS$WindGustDir[which(train_weatherAUS$WindGustDir=="S")] <- 9
train_weatherAUS$WindGustDir[which(train_weatherAUS$WindGustDir=="SSW")] <- 10
train_weatherAUS$WindGustDir[which(train_weatherAUS$WindGustDir=="SW")] <- 11
train_weatherAUS$WindGustDir[which(train_weatherAUS$WindGustDir=="WSW")] <- 12
train_weatherAUS$WindGustDir[which(train_weatherAUS$WindGustDir=="W")] <- 13
train_weatherAUS$WindGustDir[which(train_weatherAUS$WindGustDir=="WNW")] <- 14
train_weatherAUS$WindGustDir[which(train_weatherAUS$WindGustDir=="NW")] <- 15
train_weatherAUS$WindGustDir[which(train_weatherAUS$WindGustDir=="NNW")] <- 16

train_weatherAUS$WindDir9am[which(train_weatherAUS$WindDir9am=="N")] <- 1
train_weatherAUS$WindDir9am[which(train_weatherAUS$WindDir9am=="NNE")] <- 2
train_weatherAUS$WindDir9am[which(train_weatherAUS$WindDir9am=="NE")] <- 3
train_weatherAUS$WindDir9am[which(train_weatherAUS$WindDir9am=="ENE")] <- 4
train_weatherAUS$WindDir9am[which(train_weatherAUS$WindDir9am=="E")] <- 5
train_weatherAUS$WindDir9am[which(train_weatherAUS$WindDir9am=="ESE")] <- 6
train_weatherAUS$WindDir9am[which(train_weatherAUS$WindDir9am=="SE")] <- 7
train_weatherAUS$WindDir9am[which(train_weatherAUS$WindDir9am=="SSE")] <- 8
train_weatherAUS$WindDir9am[which(train_weatherAUS$WindDir9am=="S")] <- 9
train_weatherAUS$WindDir9am[which(train_weatherAUS$WindDir9am=="SSW")] <- 10
train_weatherAUS$WindDir9am[which(train_weatherAUS$WindDir9am=="SW")] <- 11
train_weatherAUS$WindDir9am[which(train_weatherAUS$WindDir9am=="WSW")] <- 12
train_weatherAUS$WindDir9am[which(train_weatherAUS$WindDir9am=="W")] <- 13
train_weatherAUS$WindDir9am[which(train_weatherAUS$WindDir9am=="WNW")] <- 14
train_weatherAUS$WindDir9am[which(train_weatherAUS$WindDir9am=="NW")] <- 15
train_weatherAUS$WindDir9am[which(train_weatherAUS$WindDir9am=="NNW")] <- 16

train_weatherAUS$WindDir3pm[which(train_weatherAUS$WindDir3pm=="N")] <- 1
train_weatherAUS$WindDir3pm[which(train_weatherAUS$WindDir3pm=="NNE")] <- 2
train_weatherAUS$WindDir3pm[which(train_weatherAUS$WindDir3pm=="NE")] <- 3
train_weatherAUS$WindDir3pm[which(train_weatherAUS$WindDir3pm=="ENE")] <- 4
train_weatherAUS$WindDir3pm[which(train_weatherAUS$WindDir3pm=="E")] <- 5
train_weatherAUS$WindDir3pm[which(train_weatherAUS$WindDir3pm=="ESE")] <- 6
train_weatherAUS$WindDir3pm[which(train_weatherAUS$WindDir3pm=="SE")] <- 7
train_weatherAUS$WindDir3pm[which(train_weatherAUS$WindDir3pm=="SSE")] <- 8
train_weatherAUS$WindDir3pm[which(train_weatherAUS$WindDir3pm=="S")] <- 9
train_weatherAUS$WindDir3pm[which(train_weatherAUS$WindDir3pm=="SSW")] <- 10
train_weatherAUS$WindDir3pm[which(train_weatherAUS$WindDir3pm=="SW")] <- 11
train_weatherAUS$WindDir3pm[which(train_weatherAUS$WindDir3pm=="WSW")] <- 12
train_weatherAUS$WindDir3pm[which(train_weatherAUS$WindDir3pm=="W")] <- 13
train_weatherAUS$WindDir3pm[which(train_weatherAUS$WindDir3pm=="WNW")] <- 14
train_weatherAUS$WindDir3pm[which(train_weatherAUS$WindDir3pm=="NW")] <- 15
train_weatherAUS$WindDir3pm[which(train_weatherAUS$WindDir3pm=="NNW")] <- 16

test_weatherAUS$RainToday[which(test_weatherAUS$RainToday=="No")] <- 0
test_weatherAUS$RainToday[which(test_weatherAUS$RainToday=="Yes")] <- 1

test_weatherAUS$RainTomorrow[which(test_weatherAUS$RainTomorrow=="No")] <- 0
test_weatherAUS$RainTomorrow[which(test_weatherAUS$RainTomorrow=="Yes")] <- 1

test_weatherAUS$WindGustDir[which(test_weatherAUS$WindGustDir=="N")] <- 1
test_weatherAUS$WindGustDir[which(test_weatherAUS$WindGustDir=="NNE")] <- 2
test_weatherAUS$WindGustDir[which(test_weatherAUS$WindGustDir=="NE")] <- 3
test_weatherAUS$WindGustDir[which(test_weatherAUS$WindGustDir=="ENE")] <- 4
test_weatherAUS$WindGustDir[which(test_weatherAUS$WindGustDir=="E")] <- 5
test_weatherAUS$WindGustDir[which(test_weatherAUS$WindGustDir=="ESE")] <- 6
test_weatherAUS$WindGustDir[which(test_weatherAUS$WindGustDir=="SE")] <- 7
test_weatherAUS$WindGustDir[which(test_weatherAUS$WindGustDir=="SSE")] <- 8
test_weatherAUS$WindGustDir[which(test_weatherAUS$WindGustDir=="S")] <- 9
test_weatherAUS$WindGustDir[which(test_weatherAUS$WindGustDir=="SSW")] <- 10
test_weatherAUS$WindGustDir[which(test_weatherAUS$WindGustDir=="SW")] <- 11
test_weatherAUS$WindGustDir[which(test_weatherAUS$WindGustDir=="WSW")] <- 12
test_weatherAUS$WindGustDir[which(test_weatherAUS$WindGustDir=="W")] <- 13
test_weatherAUS$WindGustDir[which(test_weatherAUS$WindGustDir=="WNW")] <- 14
test_weatherAUS$WindGustDir[which(test_weatherAUS$WindGustDir=="NW")] <- 15
test_weatherAUS$WindGustDir[which(test_weatherAUS$WindGustDir=="NNW")] <- 16

test_weatherAUS$WindDir9am[which(test_weatherAUS$WindDir9am=="N")] <- 1
test_weatherAUS$WindDir9am[which(test_weatherAUS$WindDir9am=="NNE")] <- 2
test_weatherAUS$WindDir9am[which(test_weatherAUS$WindDir9am=="NE")] <- 3
test_weatherAUS$WindDir9am[which(test_weatherAUS$WindDir9am=="ENE")] <- 4
test_weatherAUS$WindDir9am[which(test_weatherAUS$WindDir9am=="E")] <- 5
test_weatherAUS$WindDir9am[which(test_weatherAUS$WindDir9am=="ESE")] <- 6
test_weatherAUS$WindDir9am[which(test_weatherAUS$WindDir9am=="SE")] <- 7
test_weatherAUS$WindDir9am[which(test_weatherAUS$WindDir9am=="SSE")] <- 8
test_weatherAUS$WindDir9am[which(test_weatherAUS$WindDir9am=="S")] <- 9
test_weatherAUS$WindDir9am[which(test_weatherAUS$WindDir9am=="SSW")] <- 10
test_weatherAUS$WindDir9am[which(test_weatherAUS$WindDir9am=="SW")] <- 11
test_weatherAUS$WindDir9am[which(test_weatherAUS$WindDir9am=="WSW")] <- 12
test_weatherAUS$WindDir9am[which(test_weatherAUS$WindDir9am=="W")] <- 13
test_weatherAUS$WindDir9am[which(test_weatherAUS$WindDir9am=="WNW")] <- 14
test_weatherAUS$WindDir9am[which(test_weatherAUS$WindDir9am=="NW")] <- 15
test_weatherAUS$WindDir9am[which(test_weatherAUS$WindDir9am=="NNW")] <- 16

test_weatherAUS$WindDir3pm[which(test_weatherAUS$WindDir3pm=="N")] <- 1
test_weatherAUS$WindDir3pm[which(test_weatherAUS$WindDir3pm=="NNE")] <- 2
test_weatherAUS$WindDir3pm[which(test_weatherAUS$WindDir3pm=="NE")] <- 3
test_weatherAUS$WindDir3pm[which(test_weatherAUS$WindDir3pm=="ENE")] <- 4
test_weatherAUS$WindDir3pm[which(test_weatherAUS$WindDir3pm=="E")] <- 5
test_weatherAUS$WindDir3pm[which(test_weatherAUS$WindDir3pm=="ESE")] <- 6
test_weatherAUS$WindDir3pm[which(test_weatherAUS$WindDir3pm=="SE")] <- 7
test_weatherAUS$WindDir3pm[which(test_weatherAUS$WindDir3pm=="SSE")] <- 8
test_weatherAUS$WindDir3pm[which(test_weatherAUS$WindDir3pm=="S")] <- 9
test_weatherAUS$WindDir3pm[which(test_weatherAUS$WindDir3pm=="SSW")] <- 10
test_weatherAUS$WindDir3pm[which(test_weatherAUS$WindDir3pm=="SW")] <- 11
test_weatherAUS$WindDir3pm[which(test_weatherAUS$WindDir3pm=="WSW")] <- 12
test_weatherAUS$WindDir3pm[which(test_weatherAUS$WindDir3pm=="W")] <- 13
test_weatherAUS$WindDir3pm[which(test_weatherAUS$WindDir3pm=="WNW")] <- 14
test_weatherAUS$WindDir3pm[which(test_weatherAUS$WindDir3pm=="NW")] <- 15
test_weatherAUS$WindDir3pm[which(test_weatherAUS$WindDir3pm=="NNW")] <- 16

write.csv(train_weatherAUS,".\\data\\train.csv", row.names = FALSE)
write.csv(test_weatherAUS,".\\data\\test.csv", row.names = FALSE)