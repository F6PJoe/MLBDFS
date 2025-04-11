# clear console and workspace
cat("\014")
rm(list = ls())

# load packages
library(XML)
library(RCurl)
library(stringr)
library(rjson)   
library(plyr)
library(dplyr)
library(httr)
library(jsonlite)
library(tidyverse)
library(magrittr)
library(googlesheets4)
library(googledrive)
library(lubridate)

# Path to your service account JSON key file
#json_file <- "C:/Users/jbond/OneDrive/Documents/FBBall Bluecollar Script/nba-dfs-451519-a7658fdd6619.json"

# Authenticate with the service account
#gs4_auth(path = json_file)

# Use the pre-saved token from the .secrets folder

#gs4_auth(cache = ".secrets", email = "joebond008@gmail.com")

gs4_auth(path = "C:/Users/jbond/OneDrive/Documents/MLB Data/triple-baton-456523-e4-b9ec3cbd6e3d.json")

# # Read the sheet without manual authentication
# sheet_url <- "https://docs.google.com/spreadsheets/d/your_google_sheet_id_here"
# df <- read_sheet(sheet_url)
# 
# print(df)

# set working directory
setwd("C:/Users/jbond/OneDrive/Documents/")
source("FBBall Bluecollar Script/functions.R")
#excludeT <- c("DET", "WAS")

# Define API endpoint for FanDuel data
response <- GET("https://bluecollardfs.com/api/mlb_fanduel",
                add_headers(
                  Authorization = "FantasySixPack",
                  `Content-Type` = "application/json"
                ))

# Parse JSON response
data <- content(response, "parsed", simplifyVector = TRUE)

# Extract slate information
slates <- data$slates

# Identify the correct slate column
text_columns <- names(slates)[sapply(slates, is.character)]
slate_col <- text_columns[which(sapply(text_columns, function(col) any(grepl("MAIN", slates[[col]], ignore.case = TRUE))))]

if (length(slate_col) == 0) {
  stop("No relevant column found containing slate names.")
}

slate_desc_column <- slate_col[1]

# Identify the relevant slate index
slate_index <- which(grepl("MAIN", slates[[slate_desc_column]], ignore.case = TRUE))
if (length(slate_index) == 0) {
  slate_index <- which(grepl("ALL|ALL DAY", slates[[slate_desc_column]], ignore.case = TRUE))
}
if (length(slate_index) == 0) {
  stop("No matching slate found.")
}
slate_index <- slate_index[1]

# Extract player data for the identified slate
df <- data$slates$info[[slate_index]]

# Rename columns for consistency
names(df) <- c("Opp", "Player", "ID", "Pos", "Team", "Proj", "Salary", "Beta", "Value")

# Convert necessary columns to numeric and round values
df$Proj <- round(as.numeric(df$Proj), 2)
df$Salary <- as.numeric(df$Salary)
df$Value <- round(as.numeric(df$Value), 1)

# Retain relevant columns
df <- df[, c("Opp", "Player", "ID", "Pos", "Team", "Proj", "Salary", "Beta", "Value")]

# Remove NA values and filter for projected scores greater than or equal to 5
df <- df[!is.na(df$Proj) & df$Proj > 0, ]
fd <- df

# Process positional data for dual-position players
fd$OptPos <- fd$Pos
dualPos <- grepl("/", fd$Pos)
fd$Pos2 <- ""
fd$Pos2[dualPos] <- sub("/", "", str_extract(fd$Pos[dualPos], "/[A-Z0-9]{1,2}$"))
fd$Pos[dualPos] <- sub("/", "", str_extract(fd$Pos[dualPos], "^[A-Z0-9]{1,2}/"))
fd$Pos1 <- fd$Pos
fd$Pos <- fd$OptPos

# Arrange players by highest projections
fd <- arrange(fd, desc(Proj))

#Normalize player names
#fd$Player <- sapply(fd$Player, replaceName)

# Normalize team names
#fd$Team <- recode(fd$Team, "GSW" = "GS", "NYK" = "NY", "PHO" = "PHX", "NOR" = "NO", "NOP" = "NO", "SAN" = "SA", "SAS" = "SA")

# Remove any remaining NA values
#fd <- fd[!is.na(fd$Player) & !is.na(fd$Proj), ]

# Export processed data to Google Sheets
sheet_url_fd <- "https://docs.google.com/spreadsheets/d/1dWsEg3HLa9KY1YES31P1Mam0vLFK9zrR91rOsDSKsA8/edit?gid=1051077545#gid=1051077545"
sheet_write(fd[, c("Player", "Proj", "Salary", "Value", "Pos", "Team", "Opp")], sheet = "FD MLB DFS", ss = sheet_url_fd)

# Update timestamp in "MLB Update Time" sheet
sheet_url_time <- "https://docs.google.com/spreadsheets/d/1dWsEg3HLa9KY1YES31P1Mam0vLFK9zrR91rOsDSKsA8/edit?gid=2012207599#gid=2012207599"

# Get current time in Eastern Time (ET)
update_time <- with_tz(Sys.time(), "America/New_York")

# Format date as "Month Day, YYYY"
formatted_date <- format(update_time, "%B %d, %Y")

# Format time as "HH:MM AM/PM ET"
formatted_time <- format(update_time, "%I:%M %p ET")

# Write the date to A2
range_write(ss = sheet_url_time, data = data.frame(Date = formatted_date), sheet = "MLB Update Time", range = "A2", col_names = FALSE)

# Write the time to B2
range_write(ss = sheet_url_time, data = data.frame(Time = formatted_time), sheet = "MLB Update Time", range = "B2", col_names = FALSE)

# Define API endpoint for DraftKings data
response <- GET("https://bluecollardfs.com/api/mlb_draftkings",
                add_headers(
                  Authorization = "FantasySixPack",
                  `Content-Type` = "application/json"
                ))

# Parse DraftKings data
data <- content(response, "parsed", simplifyVector = TRUE)
slates <- data$slates

# Identify the correct slate for DraftKings
text_columns <- names(slates)[sapply(slates, is.character)]
slate_col <- text_columns[which(sapply(text_columns, function(col) any(grepl("MAIN", slates[[col]], ignore.case = TRUE))))]
if (length(slate_col) == 0) {
  stop("No relevant column found containing slate names.")
}
slate_desc_column <- slate_col[1]
slate_index <- which(grepl("MAIN", slates[[slate_desc_column]], ignore.case = TRUE))
if (length(slate_index) == 0) {
  slate_index <- which(grepl("ALL|ALL DAY", slates[[slate_desc_column]], ignore.case = TRUE))
}
if (length(slate_index) == 0) {
  stop("No matching slate found.")
}
slate_index <- slate_index[1]

df <- data$slates$info[[slate_index]]
names(df) <- c("Opp", "Player", "ID", "Pos", "Team", "Proj", "Salary", "Beta", "Value")
df$Proj <- round(as.numeric(df$Proj), 2)
df$Salary <- as.numeric(df$Salary)
df$Value <- round(as.numeric(df$Value), 1)
df <- df[, c("Opp", "Player", "ID", "Pos", "Team", "Proj", "Salary", "Beta", "Value")]
df <- df[!is.na(df$Proj) & df$Proj > 0, ]
dk <- df

# Process DraftKings positional data
dk$OptPos <- dk$Pos
dualPos <- grepl("/", dk$Pos)
dk$Pos2 <- ""
dk$Pos2[dualPos] <- sub("/", "", str_extract(dk$Pos[dualPos], "/[A-Z0-9]{1,2}$"))
dk$Pos[dualPos] <- sub("/", "", str_extract(dk$Pos[dualPos], "^[A-Z0-9]{1,2}/"))
dk$Pos1 <- dk$Pos
dk$Pos <- dk$OptPos

# Arrange DraftKings players by highest projections
dk <- arrange(dk, desc(Proj))

#dk$Player <- sapply(dk$Player, replaceName)

#dk$Team <- recode(dk$Team, "GSW" = "GS", "NYK" = "NY", "NOR" = "NO", "NOP" = "NO", "SAN" = "SA", "SAS" = "SA")
#dk <- dk[!is.na(dk$Player) & !is.na(dk$Proj), ]

# Export processed data to Google Sheets
sheet_url_fd <- "https://docs.google.com/spreadsheets/d/1dWsEg3HLa9KY1YES31P1Mam0vLFK9zrR91rOsDSKsA8/edit?gid=1051077545#gid=1051077545"
sheet_write(fd[, c("Player", "Proj", "Salary", "Value", "Pos", "Team", "Opp")], sheet = "FD MLB DFS", ss = sheet_url_fd)

sheet_url_dk <- "https://docs.google.com/spreadsheets/d/1dWsEg3HLa9KY1YES31P1Mam0vLFK9zrR91rOsDSKsA8/edit?gid=720123516#gid=720123516"
sheet_write(dk[, c("Player", "Proj", "Salary", "Value", "Pos", "Team", "Opp")], sheet = "DK MLB DFS", ss = sheet_url_dk)

# Get current time in Eastern Time (ET)
update_time <- with_tz(Sys.time(), "America/New_York")

# Format date as "Month Day, YYYY"
formatted_date <- format(update_time, "%B %d, %Y")

# Format time as "HH:MM AM/PM ET"
formatted_time <- format(update_time, "%I:%M %p ET")

# Write the date to A2
range_write(ss = sheet_url_time, data = data.frame(Date = formatted_date), sheet = "MLB Update Time", range = "A2", col_names = FALSE)

# Write the time to B2
range_write(ss = sheet_url_time, data = data.frame(Time = formatted_time), sheet = "MLB Update Time", range = "B2", col_names = FALSE)


