### ACTIVE DAILY LISTINGS ######################################################

## Figure 1

active_graph <- 
  daily %>% 
  filter(Housing == TRUE) %>% 
  group_by(Date) %>% 
  summarize(Listings = n()) %>% 
  ggplot() +
  geom_line(aes(Date, Listings), colour = "#A85A42", size = 1.5) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = scales::comma) +
  scale_x_date(name = NULL) +
  theme(text = element_text(family = "Futura"))

ggsave("output/figure_1.pdf", plot = active_graph, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)


## Table 1

territories_table <- 
  territories_property %>% 
  st_drop_geometry() %>% 
  group_by(name) %>% 
  summarize(`Active listings` = length(
    Property_ID[Created <= "2019-04-30" & Scraped >= "2019-04-30" & Housing == TRUE]),
            `Listings per 1000 residents` = 1000 * `Active listings` / mean(Population),
            `Annual revenue` = sum(Revenue, na.rm = TRUE),
            `Revenue per listing` = `Annual revenue` / `Active listings`) %>% 
  arrange(desc(`Active listings`)) %>% 
  slice(1:5) %>% 
  mutate(City = c("Yellowknife", "Whitehorse", "Iqaluit", 
                  "Macpherson-Grizzly Valley", "Dawson")) %>% 
  select(City, everything(), -name)


## Figure 2

territories_graph <- 
  territories_property %>% 
  st_drop_geometry() %>% 
  select(Property_ID, name) %>% 
  left_join(territories_daily, .) %>% 
  filter(Housing == TRUE) %>% 
  group_by(Date, name) %>% 
  summarize(Listings = n()) %>% 
  summarize(Yellowknife = sum(Listings[name == "Yellowknife (CY)"]),
            Whitehorse =  sum(Listings[name == "Whitehorse (CY)" ]),
            Iqaluit =     sum(Listings[name == "Iqaluit (CY)"    ]),
            Macpherson  = sum(Listings[name == "Macpherson-Grizzly Valley (NO)"]),
            Dawson = sum(Listings[name == "Dawson (T)"]),
            Other = sum(Listings) - (Yellowknife + Whitehorse + Iqaluit + 
                                       Macpherson + Dawson)) %>% 
  gather(Yellowknife, Whitehorse, Iqaluit, Macpherson, Dawson, Other, 
         key = "City", value = "Listings") %>% 
  filter(Date >= "2016-09-01") %>% 
  ggplot() +
  geom_line(aes(Date, Listings, colour = City), size = 1.5) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = scales::comma) +
  scale_x_date(name = NULL) +
  scale_colour_brewer(palette = "Set3") +
  theme(text = element_text(family = "Futura"),
        legend.title = element_text(family = "Futura", face = "bold", 
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10),
        legend.position = "bottom")

ggsave("output/figure_2.pdf", plot = territories_graph, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)



### SPATIAL DISTRIBUTION OF LISTINGS ###########################################

## Figure 3

property_2016 <- 
  property %>% 
  filter(Housing == TRUE, Created <= "2016-04-30", Scraped >= "2016-04-30")

property_2016 <- 
  daily %>% 
  filter(Status == "R", Date >= "2015-05-01", Date <= "2016-04-30") %>% 
  group_by(Property_ID) %>% 
  summarize(Revenue = sum(Price)) %>% 
  left_join(property_2016, .) %>% 
  mutate(Year = "2016") %>% 
  filter(Revenue > 0)

property_2017 <- 
  property %>% 
  filter(Housing == TRUE, Created <= "2017-04-30", Scraped >= "2017-04-30")

property_2017 <- 
  daily %>% 
  filter(Status == "R", Date >= "2016-05-01", Date <= "2017-04-30") %>% 
  group_by(Property_ID) %>% 
  summarize(Revenue = sum(Price)) %>% 
  left_join(property_2017, .) %>% 
  mutate(Year = "2017") %>% 
  filter(Revenue > 0)

property_2018 <- 
  property %>% 
  filter(Housing == TRUE, Created <= "2018-04-30", Scraped >= "2018-04-30")

property_2018 <- 
  daily %>% 
  filter(Status == "R", Date >= "2017-05-01", Date <= "2018-04-30") %>% 
  group_by(Property_ID) %>% 
  summarize(Revenue = sum(Price)) %>% 
  left_join(property_2018, .) %>% 
  mutate(Year = "2018") %>% 
  filter(Revenue > 0)

map <- 
  LTM_property %>% 
  filter(Revenue > 0) %>%
  mutate(Year = "2019") %>% 
  rbind(property_2016, property_2017, property_2018) %>%
  ggplot() +
  geom_sf(data = WH_streets, colour = alpha("grey", 0.5)) +
  geom_sf(aes(size = Revenue, colour = Listing_Type), alpha = 0.5, 
          show.legend = "point") +
  facet_wrap(vars(Year)) +
  scale_colour_manual(name = "Listing type",
                      values = c("#7570B3", "#D95F02")) +
  scale_size_continuous(breaks = c(5000, 25000, 45000, 65000, 85000),
                        labels = c("$5,000", "$25,000", "$45,000", "$65,000",
                                   "$85,000")) +
  guides(
    colour = guide_legend(override.aes = list(fill = c("#7570B3", "#D95F02"),
                                              alpha = 1))) +
  theme(legend.position = c(0, 0),
        legend.justification = c(0, 0),
        legend.spacing = unit(0, "pt"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        rect = element_blank(),
        #text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold", 
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10),
        strip.text = element_text(family = "Futura", face = "bold", size = 12)) 

ggsave("output/figure_3.pdf", plot = map, width = 8, height = 11, units = "in",
       useDingbats = FALSE)

  
  
### LISTING TYPE PREVALENCE ####################################################

## Table 2

listing_type_table <- 
  property %>% 
  filter(Housing == TRUE) %>% 
  rename(`Listing type` = Listing_Type) %>% 
  st_drop_geometry() %>% 
  filter(Created <= "2019-04-30", Scraped >= "2019-04-30") %>% 
  group_by(`Listing type`) %>% 
  summarize(`Active listings` = n(),
            `Annual rev.` = sum(Revenue, na.rm = TRUE),
            `Rev. per listing` = `Annual rev.` / n()) %>% 
  mutate(
    `% of all listings` = scales::percent(`Active listings` /
                                            sum(`Active listings`)),
    `% of annual rev.` = `Annual rev.` / sum(`Annual rev.`)) %>% 
  mutate(
    `Annual rev.` = round(`Annual rev.`),
    `Annual rev.` = paste0("$", str_sub(`Annual rev.`, 1, -7), ".",
                           str_sub(`Annual rev.`, -6, -6), " million"),
    `% of annual rev.` = round(`% of annual rev.`, 3),
    `% of annual rev.` = paste0(100 * `% of annual rev.`, "%"),
    `Rev. per listing` = round(`Rev. per listing`),
    `Rev. per listing` = paste0("$", str_sub(`Rev. per listing`, 1, -4),
                                ",", str_sub(`Rev. per listing`, -3, -1))
  ) %>% 
  select(`Listing type`, `Active listings`, `Annual rev.`,
         `% of all listings`, `% of annual rev.`, `Rev. per listing`)

  

### BEDROOM BREAKDOWN ##########################################################

## Figure 4

var <- filter(LTM_property, Listing_Type == "Entire home/apt")$Bedrooms
nrows <- 20
df <- expand.grid(y = 1:nrows, x = 1:nrows)
categ_table <- round(table(var) * ((nrows*nrows)/(length(var))))
categ_table[1:4] <- categ_table[1:4] + 1
names(categ_table) <- c("Studio", "1", "2", "3", "4", "5")
df$category <- factor(rep(names(categ_table), categ_table))  
  
bedroom_graph <- 
  ggplot(df, aes(x = x, y = y, fill = category)) + 
  geom_tile(color = "white", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_brewer(name = "Bedrooms", palette = "Set3") +
  theme(plot.title = element_text(size = rel(1.2)),
        panel.border = element_rect(size = 1, fill = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(family = "Futura", face = "bold",
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10)
        )

ggsave("output/figure_4.pdf", plot = bedroom_graph, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

bedrooms <- 
  LTM_property %>% 
  st_drop_geometry() %>% 
  filter(Listing_Type == "Entire home/apt") %>% 
  group_by(Bedrooms) %>% 
  summarize(Count = n()) %>% 
  mutate(Percentage = Count / sum(Count))


### REVENUE DISTRIBUTION #######################################################

## Figure 5

revenue_graph <- 
  daily %>%
  filter(Housing == TRUE, Date >= "2018-05-01", Status == "R") %>%
  group_by(Host_ID) %>%
  summarize(rev = sum(Price)) %>%
  filter(rev > 0) %>%
  summarize(
    `Top 1%`  = sum(rev[rev > quantile(rev, c(0.99))] / sum(rev)),
    `Top 5%`  = sum(rev[rev > quantile(rev, c(0.95))] / sum(rev)),
    `Top 10%` = sum(rev[rev > quantile(rev, c(0.90))] / sum(rev)),
    `Top 20%` = sum(rev[rev > quantile(rev, c(0.80))] / sum(rev))) %>% 
  gather(`Top 1%`, `Top 5%`, `Top 10%`, `Top 20%`, key = "percentile", 
         value = "value") %>% 
  mutate(percentile = factor(percentile, levels = c('Top 1%', 'Top 5%', 
                                                    'Top 10%', 'Top 20%'))) %>% 
  ggplot() +
  geom_bar(aes(percentile, value, fill = percentile), stat = "identity") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual("Host revenue percentile", 
                    values = alpha(c("#FCD7AD", "#F6C28B", "#F79D65", 
                                     "#A85A42"), 1)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold", 
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10),
        legend.position = "none")

ggsave("output/figure_5.pdf", plot = revenue_graph, width = 8, height = 4, 
       units = "in", useDingbats = FALSE)


## Table 3

table_3 <- 
  property %>% 
  filter(Revenue > 0, Housing == TRUE) %>% 
  pull(Revenue) %>% 
  quantile() %>% 
  as.list() %>% 
  as_tibble() %>% 
  select(-`0%`) %>% 
  set_names(c("25th percentile", "Median", "75th percentile", 
              "100th percentile")) %>% 
  mutate_all(round, -2) %>% 
  mutate_all(~paste0("$", str_sub(., 1, -4), ",", str_sub(., -3, -3), "00"))


## Figure 6

ML_graph <- 
  daily %>% 
  group_by(Date) %>% 
  summarize(Listings = mean(ML),
            Revenue = sum(Price * (Status == "R") * ML, na.rm = TRUE) / 
              sum(Price * (Status == "R"), na.rm = TRUE)) %>% 
  gather(Listings, Revenue, key = `Multilisting percentage`, value = Value) %>% 
  ggplot() +
  geom_line(aes(Date, Value, colour = `Multilisting percentage`), alpha = 0.2) +
  geom_smooth(aes(Date, Value, colour = `Multilisting percentage`), se = FALSE,
              method = "loess", span = 0.2) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = scales::percent, limits = c(0, 0.5)) +
  scale_x_date(name = NULL, limits = c(as.Date("2016-05-01"), NA)) +
  scale_colour_manual(values = c("#A85A42", "#F6C28B")) +
  theme(legend.position = "bottom",
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold", 
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10))

ggsave("output/figure_6.pdf", plot = ML_graph, width = 8, height = 7, 
       units = "in", useDingbats = FALSE)


## Other ML calculations

ML_daily %>% 
  filter(Housing == TRUE, Date >= "2018-05-01", Status == "R") %>% 
  group_by(Host_ID) %>% 
  summarize(Revenue = sum(Price) * exchange_rate) %>% 
  arrange(desc(Revenue))

ML_2019 <- 
  daily %>%
  filter(Date >= "2018-05-01", ML == TRUE, Housing == TRUE) %>% 
  group_by(Property_ID) %>% 
  summarize()

non_local_hosts <- 
  property %>% 
  st_drop_geometry() %>% 
  filter(Property_ID %in% ML_2019$Property_ID) %>%
  group_by(Host_ID) %>% 
  tally() %>% 
  arrange(n) %>% 
  slice(6:27)

ML_property %>% 
  filter(Host_ID %in% non_local_hosts$Host_ID, Created <= "2019-04-30", 
         Scraped >= "2018-05-01", Housing -- TRUE, 
         !(Property_ID %in% ML_2019$Property_ID))


### HOUSING LOSS ###############################################################

## Figure 7

housing_loss <- 
  tibble(Date = as.Date(as.Date(
    "2015-09-30", origin = "1970-01-01"):as.Date(
      "2019-04-30", origin = "1970-01-01"), origin = "1970-01-01"))

housing_loss <- 
  FREH %>% 
  group_by(Date) %>% 
  summarize(`Entire home/apt` = n()) %>% 
  left_join(housing_loss, .)

housing_loss <- 
  GH %>% 
  st_drop_geometry() %>% 
  group_by(date) %>% 
  summarize(`Private room` = sum(housing_units)) %>% 
  rename(Date = date) %>% 
  left_join(housing_loss, .)

housing_loss <- 
  housing_loss %>% 
  gather(`Entire home/apt`, `Private room`, key = `Listing type`,
         value = `Housing units`)

housing_graph <- 
  ggplot(housing_loss) +
  geom_col(aes(Date, `Housing units`, fill = `Listing type`),
           lwd = 0) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = scales::comma) +
  scale_x_date(name = NULL, limits = c(as.Date("2015-09-30"), NA)) +
  scale_fill_manual(values = c("#A85A42", "#F6C28B")) +
  theme(legend.position = "bottom",
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold", 
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10))

ggsave("output/figure_7.pdf", plot = housing_graph, width = 8, height = 7, 
       units = "in", useDingbats = FALSE)



## Other housing calculations

daily %>% 
  filter(Listing_Type == "Entire home/apt", Date >= "2018-04-01",
         Date <= "2018-09-30", Housing == TRUE, Status == "R") %>% 
  group_by(Property_ID) %>% 
  tally() %>% 
  filter(n >= 90, !(Property_ID %in% filter(FREH, 
                                            Date == "2019-04-30")$Property_ID))


daily %>% 
  filter(Listing_Type == "Entire home/apt", Date <= "2018-09-30",
         Date >= "2018-04-01", Status == "R") %>% 
  group_by(Property_ID) %>% 
  summarize(n_reserved = n()) %>% 
  summarize(FREH = n())



### LISTINGS LIKELY NOT IN PRINCIPAL RESIDENCES ################################

## LFRML calculations

# Add ML field to property file
property <- 
  daily %>% 
  filter(Date == "2019-04-30") %>% 
  select(Property_ID, ML) %>% 
  left_join(property, .) %>% 
  mutate(ML = if_else(is.na(ML), FALSE, ML))

# Add n_reserved and n_available fields
property <- 
  daily %>% 
  filter(Status == "R") %>% 
  group_by(Property_ID) %>% 
  summarize(n_reserved = n()) %>% 
  left_join(property, .)

property <- 
  daily %>% 
  filter(Status == "R" | Status == "A") %>% 
  group_by(Property_ID) %>% 
  summarize(n_available = n()) %>% 
  left_join(property, .)

# Add LFRML field
property <- 
  property %>%
  group_by(Host_ID, Listing_Type) %>% 
  mutate(LFRML = case_when(
    Listing_Type != "Entire home/apt" ~ FALSE,
    ML == FALSE                       ~ FALSE,
    n_available == min(n_available)   ~ TRUE,
    TRUE                              ~ FALSE)) %>% 
  ungroup()


# Resolve ties
property <- 
  property %>% 
  group_by(Host_ID, Listing_Type) %>% 
  mutate(prob = sample(0:10000, n(), replace = TRUE),
         LFRML = if_else(
           sum(LFRML) > 1 & prob != max(prob), FALSE, LFRML)) %>% 
  select(-prob)


# Add GH status
GH_list <-
  GH %>% 
  filter(date == "2019-04-30") %>% 
  pull(property_IDs) %>%
  unlist() %>%
  unique()

property <-
  property %>% 
  mutate(GH = if_else(Property_ID %in% GH_list, TRUE, FALSE))

# Add FREH status
property <- 
  FREH %>% 
  filter(Date == "2019-04-30") %>% 
  mutate(FREH = TRUE) %>% 
  left_join(property, .) %>% 
  mutate(FREH = if_else(is.na(FREH), FALSE, FREH))

# Add Legal field
legal <- 
  property %>%
  filter(Housing == TRUE, Created <= "2019-04-30", Scraped >= "2019-04-30") %>% 
  mutate(Legal = case_when(
    GH == TRUE                     ~ FALSE,
    Listing_Type == "Shared room"  ~ TRUE,
    Listing_Type == "Private room" ~ TRUE,
    FREH == TRUE                   ~ FALSE,
    LFRML == TRUE                  ~ TRUE,
    ML == TRUE                     ~ FALSE,
    TRUE                           ~ TRUE))

mean(legal$FREH, na.rm = TRUE)
mean(legal$GH, na.rm = TRUE)
mean(legal$LFRML, na.rm = TRUE)
mean(legal$ML, na.rm = TRUE)
mean(legal$Legal, na.rm = TRUE)


## Alternate approach

# Active listings
property %>%
  st_drop_geometry() %>% 
  filter(Housing == TRUE, Created <= "2019-04-30", Scraped >= "2019-04-30") %>% 
  nrow()

# EH ML
property %>%
  st_drop_geometry() %>% 
  filter(Housing == TRUE, Created <= "2019-04-30", Scraped >= "2019-04-30",
         Listing_Type == "Entire home/apt", ML == TRUE) %>% 
  nrow()

# LFRMLs
property %>% 
  st_drop_geometry() %>% 
  filter(Housing == TRUE, Created <= "2019-04-30", Scraped >= "2019-04-30",
         Listing_Type == "Entire home/apt", LFRML == TRUE) %>% 
  nrow()

# Leftover EH MLs
property %>%
  st_drop_geometry() %>% 
  filter(Housing == TRUE, Created <= "2019-04-30", Scraped >= "2019-04-30") %>%
  filter(Listing_Type == "Entire home/apt") %>% 
  filter((ML == TRUE & LFRML == FALSE)) %>% 
  nrow()

property %>%
  st_drop_geometry() %>% 
  filter(Housing == TRUE, Created <= "2019-04-30", Scraped >= "2019-04-30") %>%
  filter(Listing_Type == "Entire home/apt") %>% 
  filter((ML == TRUE & LFRML == FALSE) | (FREH == TRUE)) %>% 
  nrow()

property %>%
  st_drop_geometry() %>% 
  filter(Housing == TRUE, Created <= "2019-04-30", Scraped >= "2019-04-30") %>% 
  filter(GH == TRUE) %>% 
  nrow()
