library(tidyverse)
library(readr)

# import dataset without coordinates
dat1 <- read_csv("./data/washington-post-police-shootings-export-20230219.csv",
                 col_names = T)
victim_names <- na.omit(unique(dat1$name)) # extract unique, recorded victim names

# import dataset with coordinates
dat2 <- read_csv("./data/fatal-police-shootings-data.csv",
                 col_names = T) %>% 
  select(name, longitude, latitude) %>% 
  filter(name %in% victim_names)

# create merged dataset
merged_dat <- left_join(dat1, dat2, by = "name") %>% 
  mutate(year = as.numeric(format(date,'%Y')))

# save merged dataset as RData
save(merged_dat, file = "./data/merged_dat.Rdata")







                
                