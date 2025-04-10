# ---------------------------------- DS340H Homework 5 -------------------------- #
# ---------------------------------- Jennifer Ruffin -------------------------- #

# ----- Initial Cleaning and Merging Process of the 9 Datasets from Blue Bike website-----# 
# -- Merge Data Sets -- #
# there were so many datasets, so I asked Gemini for a useful package in merging
# a significant amount of large datasets, and it gave me the package data.table.
# I used the data.table package for most of the data cleaning process. Lots of looking
# things up!

install.packages("data.table")
library(data.table)

getwd()
setwd("/Users/jenniferruffin/Desktop/Capstone")
getwd()
# my csv file paths
file_paths <- list.files(path = "Assignment 5", pattern = "*.csv", full.names = TRUE)

# read all csvs into a list of data.tables
data_list <- lapply(file_paths, fread)

# combine the data.tables into a single data.table
bike.dat <- rbindlist(data_list)

# -- Remove unnecessary variables -- #
# - we're looking primarily at locations and trip durations - #
cleanBike = subset(bike.dat, select = -c(1,2,6,8,9,10,11,12,13))

# -- Next, I want to create a new column: trip duration. The challenge is that
# --- the start and end times are attached to the start and dates. The data.table
# --- library helps with this! I feel there's a more concise strategy, but I'm learning!

# convert started_at and ended_at to POSIXct date-time objects
cleanBike[, started_at := as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S")]
cleanBike[, ended_at := as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")]

# extract data and time components for start and end
cleanbike[, started_date := as.Date(started_at)]
cleanbike[, started_time := format(started_at, "%H:%M:%S")]

cleanbike[, ended_date := as.Date(ended_at)]
cleanbike[, ended_time := format(ended_at, "%H:%M:%S")]

# now to calculate trip duration in minutes
cleanbike[, trip_duration := as.numeric(difftime(ended_at, started_at, units = "mins"))]

# final dataset
bluebike.dat = subset(cleanbike, select = -c(1,2,9))

# save to assignment 5 folder on my computer
path <- file.path("Assignment 5", "blueBike_csv")
write.csv(bluebike.dat, file = full_path, row.names = FALSE)




