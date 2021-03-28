library(readr)
specs_csv = cols(
  .default = col_double(),
  Date = col_date(format = ""),
  Location = col_character(),
  Evaporation = col_double(),
  Sunshine = col_double(),
  WindGustDir = col_character(),
  WindDir9am = col_character(),
  WindDir3pm = col_character(),
  RainToday = col_character(),
  RainTomorrow = col_character()
)
weatherAUS <- read_csv("data/weatherAUS.csv", col_types = specs_csv)
weatherAUS_noNA <- weatherAUS_noNA <- na.omit(weatherAUS)
weatherAUS_noNA_dates <- cbind(weatherAUS_noNA, 
                               Year = format(weatherAUS_noNA$Date, format = "%Y"),
                               Month = format(weatherAUS_noNA$Date, format = "%m"),
                               Day = format(weatherAUS_noNA$Date, format = "%d"))
locations <- list()
for (location in unique(weatherAUS_noNA_dates$Location)) {
  locations <- rbind(locations,c(location,sum(weatherAUS_noNA_dates[,2]==location)))
  # print(paste(location,sum(weatherAUS_noNA_dates[,2]==location)))
}

years <- list()
for (yearD in unique(weatherAUS_noNA_dates$Year)) {
  years <- rbind(years,c(years,sum(weatherAUS_noNA_dates[,24]==yearD)))
  print(paste(yearD,sum(weatherAUS_noNA_dates[,24]==yearD)))
}

loc_year <- list()

for (location in unique(weatherAUS_noNA_dates$Location)) {
  loc_indx <- which(weatherAUS_noNA_dates$Location == location)
  loc_data <- weatherAUS_noNA_dates[loc_indx,]
  for (yearD in unique(weatherAUS_noNA_dates$Year)){
    loc_year <- rbind(loc_year,c(location,yearD,sum(loc_data[,24]==yearD)))
    # print(paste(location, yearD, sum(loc_data[,24]==yearD)))
  }
}