
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')
wash2 <- wash
ny2 <- ny
chi2 <- chi
wash2[,c("Gender","Birth.Year")] <- NA
wash2[,"City"] <- "Washington"
ny2[,"City"] <- "New York"
chi2[,"City"] <- "Chicago"
allcities <- rbind(ny2, wash2, chi2)

head(allcities)

library(ggplot2)
library(dplyr)
library(tidyr)

head(ny)

head(wash)

head(chi)

#Quick look at how trip duration varies by city
by(allcities$Trip.Duration, allcities$City, summary)

allcities %>%
  mutate(City = as.factor(City)) %>%
  summary()

# Your solution code goes here

qplot(x = Gender, y = Trip.Duration/60,
     data = subset(rbind(chi2, ny2), Gender != ''),
     geom = 'boxplot',
     main = 'Trip Duration by Gender') +
     coord_cartesian(ylim = c(0,60)) +
     ylab('Trip Duration (Minutes)') +
     facet_wrap(~City)

#

# Your solution code goes here


wash.toptrips <- wash %>%
  #mutate(Trip = paste0(Start.Station, ' to ', End.Station)) %>%
  group_by(Start.Station, End.Station) %>%
  summarise(Count = n()) %>%
  arrange(-Count) %>%
  subset(Count >= 20) %>%
  droplevels

dim(wash.toptrips)

wash.toptrip.stations <- (c(levels(wash.toptrips$Start.Station),levels(wash.toptrips$End.Station)))


# Subset looking at all routes between two stations associated with the top trips.
wash3 <- wash %>%
   subset(Start.Station %in% wash.toptrip.stations) %>%
   subset(End.Station %in% wash.toptrip.stations)

wash3.ranked <- wash3 %>%
   subset(as.character(Start.Station) == as.character(End.Station)) %>%
   group_by(Start.Station) %>%
   summarise(Count = n(), MedDurationMins = median(Trip.Duration)/60) %>%
   arrange(desc(Count)) %>%
   mutate(rank = row_number()) %>%
   select('Station' = Start.Station, rank)



wash4 <-  wash3 %>%
   left_join(wash3.ranked, by = c('Start.Station' = 'Station')) %>%
   left_join(wash3.ranked, by = c('End.Station' = 'Station')) %>%
   rename('rank.start' = rank.x, 'rank.end' = rank.y)

wash4 %>%
   group_by(Start.Station, End.Station, rank.start, rank.end) %>%
   summarise(Count = n(), MedDurationMins = median(Trip.Duration)/60) %>%
   
   ggplot(aes(x = reorder(End.Station, rank.end), y = reorder(Start.Station, desc(rank.start)))) + 
   geom_point(aes(size = Count, color = MedDurationMins)) +
   scale_x_discrete(position = 'top') +
   theme(axis.text.x = element_text(angle = 90)) +
   labs(x = 'End Station', y = 'Start Station', title = 'Frequency of Trips by Start and End Station') +
   scale_color_gradient2(low = 'red', mid = 'violet', high = 'cyan', trans = 'log') 

plot.stations <- c(wash.toptrip.stations[1:3], wash.toptrip.stations[26:31])


wash5 <- wash4 %>%
  mutate(Start.Hour = substr(as.character(Start.Time), 12, 13)) %>%
  mutate(End.Hour = substr(as.character(End.Time), 12, 13))

wash5 %>%
   subset(Start.Station %in% plot.stations) %>%
   ggplot(aes(x = End.Hour)) + 
   geom_histogram(stat = 'count') +
   labs(x = 'Hour of Trip End', title = 'Trip Count by Hour of Day for Trips Ending at Columbus Circle / Union Station') +
   facet_wrap(~ reorder(Start.Station, desc(rank.start))) +
   theme(axis.text.x = element_text(size = 5))   

#system('python -m nbconvert Explore_bikeshare_data.ipynb')
