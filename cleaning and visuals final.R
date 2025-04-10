# ---------------------------------- Capstone Final Project-------------------------------- #
# ---------------------------------- Jennifer Ruffin -------------------------------- #
# Research question: How does station-level demand (in terms of trip origins 
# and destinations) vary across different months and times of day, and what 
# are the resulting peak usage periods for the most popular stations?

# Load necessary libraries
library(dplyr)
library(lubridate)
library(MASS)
library(leaps)
library(ggplot2)

# read data
bike.dat = read.csv(file.choose())
attach(bike.dat)

# ----------------------- PRELIMINARY DATA CLEANING ------------------- #
# convert columns to date time formatting
bike.dat <- bike.dat %>%
  mutate(
    started_date = as.POSIXct(started_date, format = "%Y-%m-%d"),
    ended_date = as.POSIXct(ended_date, format = "%Y-%m-%d"),
    started_time = as.POSIXct(started_time, format = "%H:%M:%S"),
    ended_time = as.POSIXct(ended_time, format = "%H:%M:%S"),
  )

# str(bike.dat) #check to make sure that the values are the right type

# Get top 5 most common start_station_name values
top5_stations = bike.dat %>%
  count(start_station_name, sort = TRUE) %>%
  top_n(5, n)

# bar chart showing distribution of top 5 start stations
ggplot(top5_stations, aes(x = reorder(start_station_name, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +  # flipped for readability of axes titles
  labs(title = "Top 5 Most Frequent Start Stations",
       x = "Start Station Name",
       y = "Count") +
  theme_minimal()


# -----------------  EXTRACT MORE GRANULAR INFORMATION -------------- #
# function to get the month
getMonth = function(dates) {
  format(as.Date(dates), "%B")
}

# function to get time of Day
getTOD <- function(datetimes) {
  hour <- as.numeric(format(datetimes, "%H"))
  time_category <- case_when(
    hour >= 6 & hour < 12 ~ "Morning",
    hour >= 12 & hour < 18 ~ "Afternoon",
    hour >= 18 & hour < 22 ~ "Evening",
    TRUE ~ "Night"
  )
  return(time_category)
}

# get the hour ride took place
getHour <- function(datetimes) {
  as.numeric(format(datetimes, "%H"))
}

# Assign startmonth to dataset
bike.dat = bike.dat %>%
  mutate(startmonth = getMonth(started_date),
  )

# Assign startTOD to dataset
bike.dat = bike.dat %>%
  mutate(
    startTOD = getTOD(started_time)
  )

# Assign start hour to dataset
bike.dat = bike.dat %>%
  mutate(
    start_hour = getHour(started_time))

# Assign day of the week ride took place to dataset
bike.dat = bike.dat %>%
  mutate(
    start_day_of_week = wday(started_date, label = TRUE, abbr = FALSE),
    end_day_of_week = wday(ended_date, label = TRUE, abbr = FALSE)
  )

# -------------------- FIND MOST 5 MOST POPULAR STATIONS -------------------- #
# find the top 5 start stations
top_n = 5
most_popular_start_stations = bike.dat %>%
  group_by(start_station_name) %>%
  summarise(start_count = n(), .groups = 'drop') %>%
  arrange(desc(start_count)) %>%
  head(top_n)

print("Most Popular Start Stations:")
print(most_popular_start_stations)

# find the top 5 end stations
most_popular_end_stations = bike.dat %>%
  group_by(end_station_name) %>%
  summarise(end_count = n(), .groups = 'drop') %>%
  arrange(desc(end_count)) %>%
  head(top_n)

print("Most Popular End Stations:")
print(most_popular_end_stations)

# Find the top 5 most popular stations overall
popular_start = bike.dat %>%
  group_by(station = start_station_name) %>%
  summarise(start_count = n(), .groups = 'drop')

popular_end = bike.dat %>%
  group_by(station = end_station_name) %>%
  summarise(end_count = n(), .groups = 'drop')

most_popular_overall = popular_start %>%
  full_join(popular_end, by = "station") %>%
  mutate(total_activity = start_count + end_count) %>%
  arrange(desc(total_activity)) %>%
  head(top_n)

print("Most Popular Stations Overall:")
print(most_popular_overall)

# ----------------------- SUBSET DATA BY TOP 5 STATIONS  ------------------- #
top5_station_names = most_popular_overall$station # vector of station names

top5_data = bike.dat %>%
  filter(start_station_name %in% top5_station_names)

top_by_month = top5_data %>% 
  filter(startmonth %in% c("May", "June", "July", "August"))

# we're only looking at origin stations
subset_by_month_and_tod = subset(top_by_month, select = -c(started_date, end_day_of_week, end_station_name, ended_date, 
                                        ended_time)) 
# not needed, only needed for overall calculation 

write.csv(subset_by_month_and_tod, "/Users/jenniferruffin/Desktop/capstoneR.csv")

bike.dat2 = read.csv(file.choose())

# -------------------- EXTRACT COUNTS --------------------- #

# calculate the amount of rides for "start" variables
start_counts = bike.dat2 %>%
  group_by(startmonth, startTOD, start_day_of_week) %>%
  summarise(start_count = n(), .groups = 'drop') %>%
  arrange(startmonth, startTOD, start_day_of_week, desc(start_count))

print("Ride Counts by Start Month, Start TOD, and Start Day of Week:")
print(start_counts)

# Calculate ride counts by start month and hour of the day:
start_counts_hourly = bike.dat2 %>%
  group_by(startmonth, start_hour) %>%
  summarise(start_count_hourly = n(), .groups = 'drop') %>%
  arrange(startmonth, start_hour, desc(start_count_hourly))

print("Ride Counts by Start Month and Start Hour:")
print(start_counts_hourly)


# ----------------------------- VISUALIZATIONS -------------------------- #
# HW 6 Graphics fixed

top5_stations = bike.dat2 %>%
  count(start_station_name, sort = TRUE) %>%
  top_n(5, n)

# bar chart showing distribution of top 5 start stations
ggplot(top5_stations, aes(x = reorder(start_station_name, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +  # flipped for readability of axes titles
  labs(title = "Top 5 Most Frequent Start Stations",
       x = "Start Station Name",
       y = "Count") +
  theme_minimal()

# Heat map of Ride frequency by hour and month, looking at origin destinations
ggplot(start_counts_hourly, aes(x = factor(start_hour), y = factor(startmonth))) +
  geom_tile(aes(fill = start_count_hourly)) +
  scale_fill_viridis_c() +
  labs(title = "Ride Frequency by Hour and Month (Trip Starts)",
       x = "Hour of Day", y = "Month", fill = "Ride Count") +
  theme_minimal()

# ------ Line plot: hour of day by ride count ------ #
# Filter data for top stations
top_starts = bike.dat2 %>% filter(start_station_name %in% top5_station_names)
#most_popular_start_stations$start_station_name)

# Count by station and hour
top_station_hourly = top_starts %>%
  group_by(start_station_name, start_hour) %>%
  summarise(count = n(), .groups = 'drop')

ggplot(top_station_hourly, aes(x = start_hour, y = count, color = start_station_name)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Hourly Ride Counts for Top 5 Start Stations",
       x = "Hour of Day", y = "Ride Count", color = "Station") +
  theme_minimal()