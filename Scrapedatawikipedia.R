#To scrape world events from Wikipedia

#Load packages
library(xml2)
library(rvest)
library(stringr)
library(dplyr)
library(tidyr)

#Wikipedia Timeline from 2000 till 2021
event = data.frame()

for (year in 2000:2021){
  site = read_html(paste0("https://en.wikipedia.org/wiki/",year))
  fnames = html_nodes(site,"#mw-content-text h3+ ul li")
  eventdesc = html_text(fnames)

  temp<-data.frame(eventdesc, stringsAsFactors = FALSE)
  temp$Year = year
  
  #Extract month from event description
  temp1 <- mutate(temp, month = 
                     case_when(grepl("January", substr(eventdesc,1,20), ignore.case= TRUE) ~ "Jan", 
                               grepl("February", substr(eventdesc,1,20), ignore.case= TRUE) ~ "Feb",
                               grepl("March", substr(eventdesc,1,20), ignore.case= TRUE) ~ "Mar",
                               grepl("April", substr(eventdesc,1,20), ignore.case= TRUE) ~ "Apr",
                               grepl("May", substr(eventdesc,1,20), ignore.case= TRUE) ~ "May",
                               grepl("June", substr(eventdesc,1,20), ignore.case= TRUE) ~ "Jun",
                               grepl("July", substr(eventdesc,1,20), ignore.case= TRUE) ~ "Jul",
                               grepl("August", substr(eventdesc,1,20), ignore.case= TRUE) ~ "Aug",
                               grepl("September", substr(eventdesc,1,20), ignore.case= TRUE) ~ "Sep",
                               grepl("October", substr(eventdesc,1,20), ignore.case= TRUE) ~ "Oct",
                               grepl("November", substr(eventdesc,1,20), ignore.case= TRUE) ~ "Nov",
                               grepl("December", substr(eventdesc,1,20), ignore.case= TRUE) ~ "Dec",
                     ))

  #impute Month for events that happen on the same day
  temp2 <- fill(temp1, month)
  
  #create month number first
  temp2$monthnum = match(temp2$month, month.abb)
  temp3 <- mutate(temp2, monthnum.check = case_when(lag(monthnum) > monthnum & monthnum !=12~ 1))
  temp4 <- fill(temp3, monthnum.check, .direction = "down")
  temp5 <- filter(temp4, is.na(monthnum.check))
  
  event = rbind(event,temp5)
  }

#replace wikipedia citation with []

event$eventdesc2 <- gsub("[[0-9]*]","",event$eventdesc)

event2 = event %>% select(Year, month, eventdesc2)

names(event2)[2] <- "Month"
names(event2)[3] <- "World_Event"

#write.csv(event2, "C:/Users/Lee Voon Chung/Desktop/UM/Courses/20212022S2/WQD7001/Assignments/Group/Submission/Dataset/worldevent.csv", row.names = FALSE)

saveRDS(event2,file = "world_event")


