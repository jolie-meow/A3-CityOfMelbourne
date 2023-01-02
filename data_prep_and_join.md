city_of_melbourne
================
Jolie Pham
2023-01-02

``` r
library(dplyr) # For wrangling data
library(magrittr) # For pipes 
library(stringr)
library(tidyr)
library(readxl)
library(ggplot2)
library(Hmisc)
library(forecast)
library(here)
```

``` r
# load house price by suburbs
median_house_price <- read_excel(here("Suburb_Housev4_14May2021.xls"), skip=1)
```

    ## New names:
    ## • `` -> `...1`
    ## • `change` -> `change...14`
    ## • `change` -> `change...15`

``` r
median_house_price <- median_house_price[-1, c(1, 10:12)]
colnames(median_house_price)[1] <- "Suburb"

city_melb_suburbs <- c("Carlton", "Carlton North", "Docklands", "East Melbourne", "Flemington", "Fishermans Bend", "Jolimont", "Kensington", "Melbourne", "North Melbourne", "Parkville", "Port Melbourne", "Southbank", "South Whalf", "South Yarra", "West Melbourne")

median_house_price %<>% 
  mutate(Suburb = str_to_title(Suburb)) %>%
  filter(Suburb %in% city_melb_suburbs) %>%
  pivot_longer(c(2:4), names_to = "Year", values_to = "Median_Price")

median_house_price$Year <- as.integer(median_house_price$Year)
median_house_price$Median_Price <- as.integer(median_house_price$Median_Price)

glimpse(median_house_price, width = 70)
```

    ## Rows: 30
    ## Columns: 3
    ## $ Suburb       <chr> "Carlton", "Carlton", "Carlton", "Carlton North…
    ## $ Year         <int> 2018, 2019, 2020, 2018, 2019, 2020, 2018, 2019,…
    ## $ Median_Price <int> 1600000, 1308000, 1415000, 1445000, 1555000, 15…

``` r
facilities <- read.csv(here("Cafes_and_restaurants__with_seating_capacity.csv"))
facilities <- facilities[, c(1,6,9)]
colnames(facilities) <- c("Year", "Suburb", "Industry")
  
facilities %<>%
  filter(Year %in% c(2018:2020) & Industry %in% c("Cafes and Restaurants", "Takeaway Food Services")) %>%
  group_by(Year, Suburb, Industry) %>%
  summarise(Facilities_Number = n()) %>%
  pivot_wider(names_from = Industry, values_from = Facilities_Number)

colnames(facilities)[c(3,4)] <- c("Cafes_Restaurants", "Takeaway_Food")

facilities %<>%
  mutate(Suburb = case_when(
    Suburb %in% c("Melbourne (CBD)", "Melbourne (Remainder)") ~ "Melbourne",
    Suburb %in% c("West Melbourne (Residential)", "West Melbourne (Industrial)") ~ "West Melbourne",
    TRUE ~ Suburb)) %>%
  group_by(Suburb, Year) %>%
  summarise_at(c("Cafes_Restaurants", "Takeaway_Food"), sum, na.rm = TRUE)

glimpse(facilities, width=70)
```

    ## Rows: 33
    ## Columns: 4
    ## Groups: Suburb [11]
    ## $ Suburb            <chr> "Carlton", "Carlton", "Carlton", "Dockland…
    ## $ Year              <int> 2018, 2019, 2020, 2018, 2019, 2020, 2018, …
    ## $ Cafes_Restaurants <int> 315, 315, 291, 285, 285, 297, 64, 64, 57, …
    ## $ Takeaway_Food     <int> 38, 38, 38, 38, 38, 39, 8, 8, 8, 10, 10, 1…

#### Social Indicators 2018 - 2020

``` r
social_ind_2018 <- read.csv(here("Social_Indicators_for_City_of_Melbourne_Residents_2018.csv"))
social_ind_2019 <- read.csv(here("Social_Indicators_for_City_of_Melbourne_Residents_2019.csv"))
social_ind_2020 <- read.csv(here("Social_Indicators_for_City_of_Melbourne_Residents_2020.csv"))

social_ind <- rbind(social_ind_2018, social_ind_2019, social_ind_2020)
social_ind <- social_ind[,c(4,6,7,9)]
colnames(social_ind) <- c("Indicator", "Year", "Suburb", "Percentage")
```

``` r
# pivot 
social_safety <- unique(str_subset(social_ind$Indicator, "safe"))[c(-5)]
social_ind %<>%
  filter(Indicator %in% social_safety) %>%
  separate(Indicator, into = c('Indicator', 'Day_or_Night'), sep= " - ") %>%
  pivot_wider(names_from = Indicator, values_from = Percentage) %>%
  mutate(Day_or_Night = case_when(str_detect(Day_or_Night, "day") ~ "Day",
                                  TRUE ~ "Night"))
colnames(social_ind) <- c("Day_or_Night", "Year", "Suburb", "Neighborhood_Safety", "Public_Trans_Safety" )
social_ind$Day_or_Night <- as.factor(social_ind$Day_or_Night)
```

``` r
social_ind %<>%
  # filter age groups out
  filter(!str_detect(Suburb, "year")) %>%
  # filter gender out
  filter(!str_detect(tolower(Suburb), "male")) %>%
  # separate rows with multiple suburbs
  mutate(Suburb = strsplit(as.character(Suburb), split = "/")) %>%
  unnest(Suburb) %>%
  # remove the postcode
  mutate(Suburb = str_remove(Suburb, "\\d+")) %>%
  # trim white space from both sides
  mutate(Suburb = str_trim(Suburb, side="both"))

glimpse(social_ind)
```

    ## Rows: 88
    ## Columns: 5
    ## $ Day_or_Night        <fct> Day, Day, Day, Day, Day, Day, Day, Day, Day, Day, …
    ## $ Year                <int> 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 20…
    ## $ Suburb              <chr> "City of Melbourne", "Carlton", "Docklands", "East…
    ## $ Neighborhood_Safety <dbl> 89.0, 86.0, 88.4, 86.7, 88.6, 88.6, 90.2, 87.4, 87…
    ## $ Public_Trans_Safety <dbl> 86.5, 89.8, 82.5, 74.2, 86.3, 86.3, 87.3, 81.1, 81…

``` r
# replace duplicate suburb names
setdiff(unique(social_ind$Suburb), city_melb_suburbs)
```

    ## [1] "City of Melbourne"         "Flemingon"                
    ## [3] "South Wharf"               "Melbourne (St Kilda Road)"
    ## [5] "St Kilda Road"

``` r
social_ind %<>%
  mutate(Suburb = str_replace_all(Suburb, "Flemingon", "Flemington")) %>%
  mutate(Suburb = str_replace_all(Suburb, "South Wharf", "South Whalf")) %>%
  mutate(Suburb = case_when(
    Suburb %in% c("Melbourne (CBD)", "Melbourne (Remainder)", "City of Melbourne", "Melbourne (St Kilda Road)", "St Kilda Road") ~ "Melbourne", TRUE ~ Suburb)) %>%
  group_by(Suburb, Year, Day_or_Night) %>%
  summarise_at(c("Neighborhood_Safety", "Public_Trans_Safety"), mean, na.rm = TRUE)
```

## Join together: city_of_melbourne dataset

``` r
city_of_melbourne <- social_ind %>%
  full_join(facilities, by = c("Year", "Suburb")) %>%
  left_join(median_house_price, by = c("Year", "Suburb"))

write.csv(city_of_melbourne, file = "city_of_melbourne.csv")
```
