#Prep Crime data (sloppy version)
  require(dplyr)
  require(lubridate)

#Set up maindata set
  setwd("/Users/jeff/Documents/Github/textbook/chapter4/data/")
  crimes <- readr::read_csv("~/Downloads/Crimes_-_2001_to_present.csv")
  colnames(crimes) <- gsub("[[:space:][:punct:]]", ".",tolower(colnames(crimes)))
  crimes <- crimes[,-c(19,22, 16,17, 15)]
  crimes$arrest <- crimes$arrest == "true"
  crimes$domestic <- crimes$domestic == "true"
  save(crimes, file = "chicago_crime.Rda")
  
  
#Reshape example
  #Convert to date form and extract the the quarter
  
  crimes$date1 <- mdy_hms(crimes$date)
  crimes$period <- format(crimes$date1, "%Y-%m")
  crimes <- crimes[complete.cases(crimes),]
  
  crimes_mon <- crimes  %>% 
    group_by(period, ward) %>% 
    summarise(arrest.rate = 100 * mean(arrest))
  
  crimes_mon <- as.data.frame(crimes_mon)
  
  crimes_ward <- reshape(crimes_mon,
          idvar = "period",
          timevar = "ward",
          direction = "wide")
  colnames(crimes_ward) <- gsub("arrest.rate", "ward",  colnames(crimes_ward))
  save(crimes_ward, file = "crimes_ward.Rda")
  
  
  