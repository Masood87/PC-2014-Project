# step 1: compile raw data

library(readxl)
library(tidyverse)
rm(list = ls())

# set directory to the folder with all the excel files
setwd("~/Documents/PC Elections/")

# store the names of excel files--there should be 34 files for 34 provinces
(xlsx <- list.files(path = "raw data 2014", pattern = ".xlsx"))

# exclude Kabul because the format is different
(xlsx <- xlsx[-grep(pattern = "Kabul.xlsx", x = xlsx)])

# read all xlsx files
dflist <- lapply(xlsx, function(x){
  read_xlsx(path = paste0("raw data 2014/", x))
})

# length of dlist and xlsx should be the same
length(dflist) == length(xlsx)

# create an empty data.frame that will store final product in the "province loop"
pc14_basic <- data.frame()

# PROVINCE LOOP
for (i in seq_len(length(dflist))) {
  
  # take each province separetely
  df <- dflist[[i]]
  
  # store candidate names
  cand_names <- df[[2]][4:(nrow(df)-1)]
  
  # indeces
  index_end <- df[1, ] %>% as.character() %>% grep(pattern = "Total")
  index_start <- c(3, c(index_end+1)[-length(index_end)])
  
  # column names
  df[3, ] <- df[3, ] %>% as.character() %>% {case_when(.=="1" ~ "Male", .=="2" ~ "Female")}
  names(df) <- paste0("station_", df[2, ], "_", df[3, ])
  
  # polling center codes
  poll_center_codes <- df[1, index_start] %>% as.numeric()
  
  # create two empty dataframes to be used in the following loop
  DF <- temp <- data.frame()
  
  # POLLING CENTER LOOP
  for (j in seq_len(length(index_end))) {
    
    # save one polling center data in temp
    temp <- df[-c(1:3, nrow(df)), index_start[j]:index_end[j]]
    
    # add polling center codes
    temp$poll_center_code <- poll_center_codes[j]
    
    # add candidate names
    # DF$candidate_names <- cand_names
    
    # bind polling centers data on top of each other, save it in DF
    DF <- bind_rows(DF, temp)
  }
  
  # change all columns to numeric
  DF <- map_df(DF, as.numeric)
  
  # add candidate names
  DF$candidate_names <- rep(cand_names, length(index_end))
  
  # aggregate male and female station votes
  DF$male_stations_votes <- select(DF, ends_with("_Male")) %>% rowSums(na.rm = T)
  DF$female_stations_votes <- select(DF, ends_with("Female")) %>% rowSums(na.rm = T)
  
  # add province names
  DF$province <- gsub(".xlsx", "", xlsx)[i]
  
  # keep relevant columns
  DF <- select(DF, province, poll_center_code, candidate_names, male_stations_votes, female_stations_votes, station_NA_NA)
  
  # bind provinces on top of each other
  pc14_basic <- bind_rows(pc14_basic, DF)
}

# candidate total vote in each polling center
pc14_basic$center_total_vote <- pc14_basic$male_stations_votes + pc14_basic$female_stations_votes

# check if the new "total vote in each polling center" is equal to provided one
# setdiff(pc14_basic$station_NA_NA, pc14_basic$center_total_vote)

# check further if the new "total vote in each polling center" is equal to provided one
# pc14_basic$diff <- pc14_basic$station_NA_NA - pc14_basic$center_total_vote
# table(pc14_basic$diff)

# clean R environment
rm(list = c("df", "DF", "dflist", "temp", "cand_names", "i", "j", "index_end", "index_start", "poll_center_codes", "xlsx"))

# keep relevant columns
pc14_basic <- select(pc14_basic, province, poll_center_code, candidate_names, male_stations_votes, female_stations_votes, center_total_vote)

# read Kabul data
kabul <- read_xlsx("raw data 2014/Kabul.xlsx", sheet = 2, skip = 4) #note Kabul data does not indicate male/female stations
kabul <- kabul[, c(1:2, length(kabul))]

# consistent names for columns
names(kabul) <- c("poll_center_code", "candidate_names", "center_total_vote")

# fill empty cells of polling center codes
kabul <- fill(kabul, poll_center_code, .direction = "down") #from package tidyr/tidyverse

# remove empty rows
kabul <- kabul[!is.na(kabul$candidate_names), ]

# add province column
kabul$province <- "Kabul"
kabul$poll_center_code <- as.numeric(kabul$poll_center_code)

# bind 33 provinces with Kabul
pc14_basic <- bind_rows(kabul, pc14_basic); rm(kabul)
pc14_basic <- select(pc14_basic, province, poll_center_code, candidate_names, male_stations_votes, female_stations_votes, center_total_vote)

# save data
write_csv(pc14_basic, "pc14_basic.csv"); rm(pc14_basic)
