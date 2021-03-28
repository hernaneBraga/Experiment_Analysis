train_weatherAUS <- weatherAUS_noNA_dates[weatherAUS_noNA_dates$Year%in%c(2009,2010,2011),]
train_weatherAUS <- train_weatherAUS[train_weatherAUS$Location!="Hobart",]
train_weatherAUS <- train_weatherAUS[train_weatherAUS$Location!="Cobar",]

train_locations <- list()
for (location in unique(train_weatherAUS$Location)) {
  train_locations <- rbind(train_locations,c(location,sum(train_weatherAUS[,2]==location)))
  # print(paste(location,sum(train_weatherAUS[,2]==location)))
}

test_weatherAUS <- weatherAUS_noNA_dates[weatherAUS_noNA_dates$Year%in%c(2012),]

test_locations <- list()
for (location in unique(train_weatherAUS$Location)) {
  test_locations <- rbind(test_locations,c(location,sum(test_weatherAUS[,2]==location)))
  # print(paste(location,sum(test_weatherAUS[,2]==location)))
}

# write.csv2(train_weatherAUS,".\\data\\train.csv", row.names = FALSE, sep = ";", dec = ".")
# write.csv2(test_weatherAUS,".\\data\\test.csv", row.names = FALSE, sep = ";", dec = ".")# write.csv2(test_weatherAUS,".\\data\\test.csv", row.names = FALSE, sep = ";", dec = ".")