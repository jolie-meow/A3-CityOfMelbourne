handle_NA_and_outliers
================
Jolie Pham
2023-01-02

``` r
library(dplyr) # For wrangling data
library(magrittr) # For pipes 
library(tidyr)
library(here)
library(Hmisc)
```

``` r
city_of_melbourne <- read.csv(here("city_of_melbourne.csv"))
glimpse(city_of_melbourne)
```

    ## Rows: 75
    ## Columns: 9
    ## $ X                   <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,…
    ## $ Suburb              <chr> "Carlton", "Carlton", "Carlton", "Carlton", "Carlt…
    ## $ Year                <int> 2018, 2018, 2019, 2019, 2020, 2020, 2018, 2018, 20…
    ## $ Day_or_Night        <chr> "Day", "Night", "Day", "Night", "Day", "Night", "D…
    ## $ Neighborhood_Safety <dbl> 86.0, 57.6, 88.0, 61.1, 78.1, 56.1, 88.4, 67.6, 89…
    ## $ Public_Trans_Safety <dbl> 89.8, 53.8, 83.4, 55.7, 78.6, 48.1, 82.5, 66.3, 86…
    ## $ Cafes_Restaurants   <int> 315, 315, 315, 315, 291, 291, 285, 285, 285, 285, …
    ## $ Takeaway_Food       <int> 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 39, 39, 8,…
    ## $ Median_Price        <int> 1600000, 1600000, 1308000, 1308000, 1415000, 14150…

Suburb : The suburban area that belongs to City of Melbourne. There are
16 of them.

Year : The year in which the information was collected / measured,
ranging from 2018 to 2020

Neighborhood_Safety : The perception of safety by yourself in the
neighborhood

Public_Trans_Safety : The perception of safety on public transport in
and around City of Melbourne

Day_or_Night : Whether the perception of safety is for day time or night
time

Cafes_Restaurants : The number of cafes and restaurants, including both
indoor and outdoor

Takeaway_Food : The number of takeaway food services

Median_Price : The median price of property

## total number of missing values we have in our dataset

``` r
# total missing values
colSums(is.na(city_of_melbourne))
```

    ##                   X              Suburb                Year        Day_or_Night 
    ##                   0                   0                   0                   3 
    ## Neighborhood_Safety Public_Trans_Safety   Cafes_Restaurants       Takeaway_Food 
    ##                   3                   3                  12                  12 
    ##        Median_Price 
    ##                  24

``` r
city_of_melbourne %>%
  filter(is.na(Neighborhood_Safety))
```

    ##    X         Suburb Year Day_or_Night Neighborhood_Safety Public_Trans_Safety
    ## 1 73 Port Melbourne 2018         <NA>                  NA                  NA
    ## 2 74 Port Melbourne 2019         <NA>                  NA                  NA
    ## 3 75 Port Melbourne 2020         <NA>                  NA                  NA
    ##   Cafes_Restaurants Takeaway_Food Median_Price
    ## 1                29             0      1488000
    ## 2                31             0      1400000
    ## 3                31             0      1525000

``` r
city_of_melbourne %<>%
  drop_na(Neighborhood_Safety)

city_of_melbourne %>%
  filter(is.na(Cafes_Restaurants)) %>%
  group_by(Suburb) %>%
  summarise(n())
```

    ## # A tibble: 2 × 2
    ##   Suburb      `n()`
    ##   <chr>       <int>
    ## 1 Flemington      6
    ## 2 South Whalf     6

``` r
city_of_melbourne %>%
  filter(is.na(Median_Price)) %>%
  group_by(Suburb) %>%
  summarise(n())
```

    ## # A tibble: 4 × 2
    ##   Suburb      `n()`
    ##   <chr>       <int>
    ## 1 Docklands       6
    ## 2 Melbourne       6
    ## 3 South Whalf     6
    ## 4 Southbank       6

## impute some central values to replace the NAs

``` r
# check distribution
hist.data.frame(city_of_melbourne[,c(6,7,8)])
```

![](handle_NA_files/figure-gfm/impute%20NA-1.png)<!-- -->

``` r
# replace NAs with median number in each column per year

city_of_melbourne %<>%
  mutate(across(Cafes_Restaurants, ~replace_na(., median(., na.rm=TRUE))))


city_of_melbourne$Cafes_Restaurants[is.na(
  city_of_melbourne$Cafes_Restaurants)] <- median(
    city_of_melbourne$Cafes_Restaurants, na.rm = TRUE)

city_of_melbourne$Takeaway_Food[is.na(
  city_of_melbourne$Takeaway_Food)] <- median(
    city_of_melbourne$Takeaway_Food, na.rm = TRUE)

city_of_melbourne$Median_Price[is.na(
  city_of_melbourne$Median_Price)] <- median(
    city_of_melbourne$Median_Price, na.rm = TRUE)
```
