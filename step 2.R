library(tidyverse)
rm(list = ls())

setwd("~/Documents/PC Elections/")

# import step 1
step1 <- read_csv("pc14_basic.csv")

# import dta file that include gender of candidates and location of polling centers
dta <- list.files(path = "raw data 2014", pattern = ".dta")
dta <- readstata13::read.dta13(paste0("raw data 2014/", dta))

# rename step1 columns to match dta file
step1 <- rename(step1, "pccode" = "poll_center_code", "name" = "candidate_names")

# remove repeated whitespaces in candidate names
step1$name <- str_squish(step1$name)

# merge step1 and dta--note output message for whether the merge is successful or not
ifelse(test = anti_join(step1, dta, by = c("pccode", "name")) %>% nrow() == 0, 
       yes = capture.output(c(full <- inner_join(step1, dta, by = c("pccode", "name")), print("successfully merge"))), 
       no = capture.output(print("not fully merged")))

# clean R environment
rm(list = c("step1", "dta"))

# keep relevant columns and assign better names
full <- select(full, pcprovinceid, province.x, districtid, districtname, pccode, pcname, id, name, gender, male_stations_votes, female_stations_votes, center_total_vote)
full <- rename(full, "provinceid" = "pcprovinceid", "province" = "province.x", "district" = "districtname", 
       "poll_center_code" = "pccode", "candidate_id" = "id", "candidate_names" = "name")

# save data
write_csv(full, "pc14_full.csv")
