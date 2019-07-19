#### Rebuild Whitehorse data ###################################################

### Libraries ##################################################################

library(tidyverse)
library(sf)
library(cancensus)
library(osmdata)


### Load geographies ###########################################################

WH <-
  get_census(dataset = "CA16", regions = list(CSD = "6001009"), level = "CSD",
             geo_format = "sf") %>% 
  st_transform(3347) %>% 
  select(geometry)

WH_streets <- 
  getbb("Whitehorse") %>% 
  opq() %>% 
  add_osm_feature(key = "highway") %>% 
  osmdata_sf()

WH_streets <- 
  rbind(WH_streets$osm_polygons %>% st_cast("LINESTRING"),
        WH_streets$osm_lines) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(32608) %>%
  st_intersection(Whitehorse) %>% 
  select(osm_id, name, geometry)


### Import property files ######################################################

## Canada property file

all_property <- read_csv(
  "data/ca_Property_Match_2019-05-28.csv",
  col_type = cols_only(
    `Property ID` = col_character(),
    `Listing Title` = col_character(),
    `Property Type` = col_character(),
    `Listing Type` = col_factor(),
    `Created Date` = col_date(format = ""),
    `Last Scraped Date` = col_date(format = ""),
    Latitude = col_double(),
    Longitude = col_double(),
    City = col_character(),
    Bedrooms = col_double(),
    `Airbnb Property ID` = col_double(),
    `Airbnb Host ID` = col_double(),
    `HomeAway Property ID` = col_character(),
    `HomeAway Property Manager` = col_character())) %>% 
  set_names(
    c("Property_ID", "Listing_Title", "Property_Type", "Listing_Type",
      "Created", "Scraped", "Latitude", "Longitude", "City", "Bedrooms", 
      "Airbnb_PID", "Airbnb_HID", "HomeAway_PID", "HomeAway_HID")) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(3347) %>% 
  select(Property_ID, Listing_Title, Property_Type, Listing_Type, Created, 
         Scraped, City, Bedrooms, Airbnb_PID, Airbnb_HID, HomeAway_PID, 
         HomeAway_HID) %>% 
  mutate(Housing = if_else(Property_Type %in% c(
    "House", "Private room in house", "Apartment", "Cabin",
    "Entire condominium", "Townhouse", "Condominium", "Entire apartment",
    "Private room", "Loft", "Place", "Entire house", "Villa", "Guesthouse",
    "Private room in apartment", "Guest suite", "Shared room in dorm",
    "Chalet", "Dorm", "Entire chalet", "Shared room in loft", "Cottage",
    "Resort", "Serviced apartment", "Other", "Bungalow", "Farm stay",
    "Private room in villa", "Entire loft", "Entire villa",
    "Private room in guesthouse", "Island", "Entire cabin", "Vacation home",
    "Entire bungalow", "Earth house", "Nature lodge", "In-law",
    "Entire guest suite", "Shared room in apartment", "Private room in loft",
    "Tiny house", "Castle", "Earth House", "Private room in condominium",
    "Entire place", "Shared room", "Hut", "Private room in guest suite",
    "Private room in townhouse", "Timeshare", "Entire townhouse",
    "Shared room in house", "Entire guesthouse", "Shared room in condominium",
    "Cave", "Private room in cabin", "Dome house",
    "Private room in vacation home", "Private room in dorm",
    "Entire serviced apartment", "Private room in bungalow",
    "Private room in serviced apartment", "Entire Floor", "Entire earth house",
    "Entire castle", "Shared room in chalet", "Shared room in bungalow",
    "Shared room in townhouse", "Entire cottage", "Private room in castle",
    "Private room in chalet", "Private room in nature lodge", "Entire in-law",
    "Shared room in guesthouse", "Casa particular", "Serviced flat", "Minsu",
    "Entire timeshare", "Shared room in timeshare", "Entire vacation home",
    "Entire nature lodge", "Entire island", "Private room in in-law",
    "Shared room in serviced apartment", "Shared room in cabin", "Entire dorm",
    "Entire cave", "Private room in timeshare", "Shared room in guest suite",
    "Private room in cave", "Entire tiny house",
    "Private room in casa particular (cuba)", "Casa particular (cuba)",
    "Private room in cottage", "Private room in tiny house",
    "Entire casa particular", ""), TRUE, FALSE)) %>% 
  mutate(Host_ID = if_else(!is.na(Airbnb_HID), 
                           as.character(Airbnb_HID), HomeAway_HID)) %>% 
  arrange(Property_ID)


## Whitehorse property file

property <- 
  all_property %>% 
  st_intersection(WH) %>% 
  st_transform(32608)

WH <- st_transform(WH, 32608)


## Multilisting property file

ML_PIDs <- 
  all_property %>% 
  filter((!is.na(Host_ID))) %>% 
  filter(Host_ID %in% property$Host_ID) %>% 
  filter(!(Property_ID %in% property$Property_ID)) %>% 
  pull(Property_ID) %>% 
  c(property$Property_ID)

ML_property <-
  all_property %>% 
  filter(Property_ID %in% ML_PIDs)

rm(ML_PIDs)


## Territories property file

territories <- get_census("CA16", regions = list(PR = c("60", "61", "62")), 
                          level = "CSD", geo_format = "sf") %>% 
  st_transform(3347) %>% 
  select(name, Population, geometry)

territories_property <- 
  all_property %>% 
  st_intersection(territories)


### Import daily files #########################################################

## Canada daily file

all_daily <- 
  tibble(Property_ID = character(),
         Date = as.Date(x = integer(0), origin = "1970-01-01"),
         Status = character(),
         Price = double())

for (i in 1:7) {
  
  daily_temp <- read_csv(paste0("data/daily", i, ".csv"), 
                         col_names = c("Property_ID", "Date", "Status", "Booked",
                                       "Price", "Price_native", "Currency",
                                       "Res_ID", "Airbnb_PID", "HomeAway_PID"),
                         col_type = cols_only(Property_ID = col_character(),
                                              Date = col_date(format = ""),
                                              Status = col_character(),
                                              Price = col_double()))
  
  daily_temp <-
    daily_temp %>% 
    filter(Property_ID %in% all_property$Property_ID)
  
  daily_temp <- 
    all_property %>% 
    st_drop_geometry() %>% 
    select(Property_ID, Host_ID, Listing_Type, Created, Scraped, Housing) %>% 
    left_join(daily_temp, .)
  
  daily_temp <- 
    daily_temp %>% 
    filter((Date >= Created | is.na(Created)), Date - 30 <= Scraped,
           Status != "U")
  
  all_daily <- 
    rbind(all_daily, daily_temp)
  
}


## Whitehorse daily file

daily <- 
  all_daily %>% 
  filter(Property_ID %in% property$Property_ID)


## Territories daily file

territories_daily <- 
  all_daily %>% 
  filter(Property_ID %in% territories_property$Property_ID)


## Multilisting daily file

ML_daily <- 
  all_daily %>% 
  filter(Property_ID %in% ML_property$Property_ID)


### Fill in missing Created dates ##############################################

property <- 
  daily %>% 
  group_by(Property_ID) %>% 
  summarize(Created2 = if_else(any(is.na(Created)), min(Date), 
                               min(Created))) %>% 
  left_join(property, .) %>% 
  mutate(Created = if_else(is.na(Created), Created2, Created)) %>% 
  select(-Created2)

ML_property <- 
  ML_daily %>% 
  group_by(Property_ID) %>% 
  summarize(Created2 = if_else(any(is.na(Created)), min(Date), 
                               min(Created))) %>% 
  left_join(ML_property, .) %>% 
  mutate(Created = if_else(is.na(Created), Created2, Created)) %>% 
  select(-Created2)

territories_property <- 
  territories_daily %>% 
  group_by(Property_ID) %>% 
  summarize(Created2 = if_else(any(is.na(Created)), min(Date), 
                               min(Created))) %>% 
  left_join(territories_property, .) %>% 
  mutate(Created = if_else(is.na(Created), Created2, Created)) %>% 
  select(-Created2)


### Add revenue ################################################################

exchange_rate <- mean(1.2873,	1.3129, 1.3130, 1.3041, 1.3037, 1.3010, 1.3200,
                      1.3432, 1.3301, 1.3206, 1.3368, 1.3378)

property <- 
  daily %>% 
  filter(Date >= "2018-05-01", Status == "R") %>% 
  group_by(Property_ID) %>% 
  summarize(Revenue = sum(Price) * exchange_rate) %>% 
  select(Property_ID, Revenue) %>% 
  left_join(property, .) %>% 
  select(Property_ID, Host_ID, Listing_Title:Listing_Type, Created, Scraped,
         Housing, Revenue, City:geometry)

ML_property <- 
  ML_daily %>% 
  filter(Date >= "2018-05-01", Status == "R") %>% 
  group_by(Property_ID) %>% 
  summarize(Revenue = sum(Price) * exchange_rate) %>% 
  select(Property_ID, Revenue) %>% 
  left_join(ML_property, .) %>% 
  select(Property_ID, Host_ID, Listing_Title:Listing_Type, Created, Scraped,
         Housing, Revenue, City:geometry)

territories_property <- 
  territories_daily %>% 
  filter(Date >= "2018-05-01", Status == "R") %>% 
  group_by(Property_ID) %>% 
  summarize(Revenue = sum(Price) * exchange_rate) %>% 
  select(Property_ID, Revenue) %>% 
  left_join(territories_property, .) %>% 
  select(Property_ID, Host_ID, Listing_Title:Listing_Type, Created, Scraped,
         Housing, Revenue, City:geometry)


### Calculate multilistings ####################################################

ML_table <- 
  ML_daily %>%
  filter(Listing_Type != "Shared room") %>% 
  group_by(Listing_Type, Host_ID, Date) %>%
  summarize(Count = n()) %>% 
  ungroup() %>%
  filter(Count >= 2) %>% 
  filter((Listing_Type == "Entire home/apt" & Count >=2 |
            (Listing_Type == "Private room" & Count >= 3))) %>% 
  mutate(ML = TRUE) %>% 
  select(-Count)

ML_daily <- left_join(ML_daily, ML_table) %>%
  mutate(ML = if_else(is.na(ML), FALSE, ML))

daily <- 
  left_join(daily, ML_daily)


### Create LTM_property ########################################################

LTM_property <- property %>% 
  filter(Created <= "2019-04-30", Scraped >= "2019-04-30", Housing == TRUE)


### Calculate FREH and GH listings #############################################

FREH <- 
  daily %>% 
  strr_FREH("2015-09-30", "2019-04-30", cores = 1) %>% as_tibble() %>% 
  filter(FREH == TRUE) %>% 
  select(-FREH)

GH <- 
  property %>% 
  filter(Housing == TRUE) %>% 
  strr_ghost(Property_ID, Host_ID, Created, Scraped, "2014-10-01", "2019-04-30",
             Listing_Type, cores = 1)


### Export files to disk #######################################################

## Geometries

save(WH, file = "data/WH.Rdata")
save(WH_streets, file = "data/WH_streets.Rdata")
save(territories, file = "data/WH_territories.Rdata")

## Property files

save(property, file = "data/WH_property.Rdata")
save(all_property, file = "data/WH_all_property.Rdata")
save(territories_property, file = "data/WH_territories_property.Rdata")
save(ML_property, file = "data/WH_ML_property.Rdata")
save(LTM_property, file = "data/WH_LTM_property.Rdata")

## Daily files

save(daily, file = "data/WH_daily.Rdata")
save(territories_daily, file = "data/WH_territories_daily.Rdata")
save(ML_daily, file = "data/WH_ML_daily.Rdata")

## FREH and GH

save(FREH, file = "data/WH_FREH.Rdata")
save(GH, file = "data/WH_GH.Rdata")
