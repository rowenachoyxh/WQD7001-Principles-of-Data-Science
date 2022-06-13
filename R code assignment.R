#################
#####Setting#####
#################

setwd("Desktop/WQD7001 - Shiny App/")

library(dplyr)
library(rlang)
library(tidyr)
library(readxl)
library(ggplot2)
library(gridExtra)
library(tidyverse)

#Read data#

tourismdat = read.csv("MonthlyArrivalsbyCountry2000to2021.csv")
head(tourismdat)
names(tourismdat)
summary(tourismdat)

################################################
######Exploratory Data Analysis using China#####
################################################

summarytourismdat <- tourismdat %>% group_by(Year) %>% filter(Destination == "China") 

summarytourismdat

plot1 <- ggplot(summarytourismdat, aes(x=Year, y = Jan...Residence)) +
          geom_bar(stat="identity")

plot2 <- ggplot(summarytourismdat, aes(x=Year, y = Jan...Nationality)) +
  geom_bar(stat="identity")

grid.arrange(plot1, plot2, nrow=2)

#need to sum up residence, earlier years only residence have positive numbers, later years only nationality have numbers

#########################
######Data Clearning#####
#########################

#rename column "Destination" to "Country"
tourismdat <- tourismdat %>% rename(Country = Destination)

#for loop to sum up Residence and Nationality columns for all months
for (month in month.abb) {
  eval(parse_expr(paste0("tourismdat$", month," = ", "tourismdat$", month, "...Residence", 
    " + ", "tourismdat$", month,"...Nationality")))
  }

#drop columns with Residence or Nationality  
tourismdat1 <- tourismdat %>% select(!contains(c("Residence", "Nationality")))

#use gather function to convert month columns into rows
tourismdat2 <- tourismdat1 %>% gather("month", "Arrivals", 3:14)

#create an indicator to label year_month with positive arrivals
tourismdat3 <- mutate(tourismdat2, Positive = case_when(Arrivals>0 ~ 1, Arrivals<=0 ~ 0))
summarytourismdat3 <- group_by(tourismdat3, Country) %>% summarise(NumberPositive = sum(Positive)) %>% arrange(NumberPositive)

#exclude countries with less than 48 months of positive arrivals
summarytourismdat3[summarytourismdat3$NumberPositive < 48,]

tourismdat4 <- filter(tourismdat3, !Country %in% c("Sudan","Qatar","Uzbekistan","Kuwait","Nigeria",
                                                     "Austria","Brazil"))

##########################
######Data Enrichment#####
##########################

###1. Include country region, country code and lat long of capital cities using left join
countryregion <- read_xlsx("CountryRegion.xlsx", col_names = TRUE)

tourismdat5 <- left_join(tourismdat4, countryregion, by = "Country")

###2. Rename_country_name
tourismdat5$Country <- sub("Korea \\(ROK\\)","South Korea",tourismdat5$Country)
tourismdat5$Country <- sub("Chinese Taipei","Taiwan",tourismdat5$Country)

###3. Summarise by year
toursimdat_yearly <- tourismdat5 %>% select(Year, Country, Region, CountryCode, long, lat, Arrivals) %>%
  group_by(Year, Country, Region, CountryCode, long, lat) %>%
  summarise(Arrivals = sum(Arrivals))

###4. Add month rank onto monthly data
tourismdat_monthly <- tourismdat5 %>% select(Year, month, Country, Region, CountryCode, long, lat, Arrivals) %>%
  group_by(Year, Country, Region, CountryCode, long, lat) %>%
  mutate(rankmonth = rank(desc(Arrivals), ties.method="first"))


###Export into csv files
#write.csv(toursimdat_yearly, "C:/Users/Lee Voon Chung/Desktop/UM/Courses/20212022S2/WQD7001/Assignments/Group/Submission/Dataset/tourismdat_yearly.csv", row.names = FALSE)
#write.csv(tourismdat_monthly, "C:/Users/Lee Voon Chung/Desktop/UM/Courses/20212022S2/WQD7001/Assignments/Group/Submission/Dataset/tourismdat_monthly.csv", row.names = FALSE)

###Export into RDS files
saveRDS(toursimdat_yearly, file = "tourism_yearly.rds")
saveRDS(tourismdat_monthly, file = "tourism_monthly.rds")
