#Tommy Heideman 
#10/23/2023 
#HW 4 

rm(list=ls())

#PROBLEM 1 
names <- character(0)

years <- character(0)

wins <- character(0)

losses <- character(0)

user_agent <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36"
start <- Sys.time()

# Loop through pages
for (i in 1:24){
  url <- paste("https://www.scrapethissite.com/pages/forms/?page_num=", i, sep="")
  page <- read_html(url, user_agent)
  start_time <- Sys.time()
  #Sys.sleep(5)
  
  # This extracts from the web scraping 
  name <- xml_text(xml_find_all(page, "//td[@class='name']"))
  names <- c(names, name)
  year <- xml_text(xml_find_all(page, "//td[@class='year']"))
  years <- c(years, year)
  win <- xml_text(xml_find_all(page, "//td[@class='wins']"))
  wins <- c(wins, win)
  loss <- xml_text(xml_find_all(page, "//td[@class='losses']"))
  losses <- c(losses, loss)
  print(paste("Scraping Page", i))
  nhl <- data.frame(Team = names, Year = years, Wins = wins, Losses = losses)
  rand_num <- sample(2:20, 1)
  Sys.sleep(rand_num)
}

stop <- Sys.time()
print(start)
print(stop)
print(stop-start)

#PROBLEM 2 
install.packages(stringr)
library(stringr)  # for the str_replace_all function

nhl <- nhl %>%
  # Remove '\n' and trim whitespace
  mutate(across(everything(), ~ str_replace_all(., "\n", ""))) %>%
  mutate(across(everything(), ~ str_trim(.))) %>%
  # Convert specified columns to integer data type
  mutate(
    Year = as.integer(Year),
    Wins = as.integer(Wins),
    Losses = as.integer(Losses)
  )

#Checks to see that the right data types are set to each column 
str(nhl)

# Check for newline characters in all columns of 'nhl'- FALSE means correct 
lapply(nhl, function(x) any(grepl("\n", x)))

# Check for leading or trailing whitespace in all columns of 'nhl' - FALSE means correct 
lapply(nhl, function(x) any(grepl("^\\s|\\s$", x)))



#PROBLEM 3 
install.packages("readxl")
library(readxl)

#Read the file and look at the data types 
nhl_expanded <- read_excel("nhl_2012-2021.xlsx")


#Column headings need to be changed 
# Set the first row as column names
colnames(nhl_expanded) <- nhl_expanded[1, ]
# Remove the first row
nhl_expanded <- nhl_expanded[-1, ]


#Look at data types for the change to match nhl 
str(nhl)
str(nhl_expanded)

# Loop over each column
nhl_expanded[] <- lapply(names(nhl_expanded), function(colname) {
  # If the column name is not "Team" or "Season", attempt conversion to integer
  if(colname != "Team" && colname != "Season") {
    as.integer(as.character(nhl_expanded[[colname]]))
  } else {
    # If it is "Team" or "Season", just return it as is
    nhl_expanded[[colname]]
  }
})

#Verified data types were changed 
str(nhl_expanded)


#Removal of the * in Team names 
nhl_expanded$Team <- sub("\\*$", "", nhl_expanded$Team)

colnames(nhl_expanded)[colnames(nhl_expanded) == "W"] <- "Wins"
colnames(nhl_expanded)[colnames(nhl_expanded) == "L"] <- "Losses"

nhl_subset <- nhl[, c("Team", "Year", "Wins", "Losses")]
nhl_expanded_subset <- nhl_expanded[, c("Team", "Season", "Wins", "Losses")]

colnames(nhl_expanded_subset)[colnames(nhl_expanded_subset) == "Season"] <- "Year"


nhl2 <- rbind(nhl_subset, nhl_expanded_subset)

#R would not let me use Win% like in the directions 
nhl2$Win_Percent <- nhl2$Wins / (nhl2$Wins + nhl2$Losses)



#PROBLEM 4 
# Read the data from the CSV file
arena <- read.csv("nhl_hockey_arenas.csv", stringsAsFactors = FALSE)

# Merge horizontally with nhl2
merged_data <- merge(nhl2, arena, by = "Team", all.x = TRUE)

# Check the first few rows of the merged data
head(merged_data)

colnames(arena)

# Rename the desired columns in the arena dataframe
colnames(arena)[colnames(arena) == "Team.Name"] <- "Team"
colnames(arena)[colnames(arena) == "Arena.Name"] <- "Arena"
colnames(arena)[colnames(arena) == "Arena.Location"] <- "Location"
colnames(arena)[colnames(arena) == "Seating.Capacity"] <- "Capacity"

# Horizontally merge nhl2 with the selected columns of arena
nhl3 <- merge(nhl2, arena[, c("Team", "Arena", "Location", "Capacity")], by = "Team", all.x = TRUE)

#PROBLEM 5 
write.csv(nhl3, file = "hockey_data.csv", row.names = FALSE)





