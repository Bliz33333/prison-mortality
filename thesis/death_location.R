library(pacman)
p_load(tidyverse, readr, tidygeocoder)

tji <- read_csv("tji_custodial-deaths.csv")

addresses <- 
  tji %>% 
  select(death_location_county, death_location_city, death_location_street_address)

addresses$state <- "TX"
colnames(addresses) <- c("County", "City", "Address", "State")

addresses <-
  addresses %>%
  filter(!is.na(Address)) %>% 
  mutate(Combined = paste0(Address, " ", City, ", ",State))

loc_table <- 
  table(addresses$Combined) %>% 
  sort(decreasing = T)

coords <- geo(names(loc_table), method = "census")
#save(coords, file = "2_geoloc")

coords_2 <- geo(names(loc_table)[1:25])
#save(coords, file = "2_geoloc")

geo(street = "600 Peachtree Street NE", city = "Atlanta",
    state = "Georgia", method = "census")
