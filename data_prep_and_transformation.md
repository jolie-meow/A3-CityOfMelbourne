handle_NA_and_outliers
================
Jolie Pham
2023-01-02

``` r
library(dplyr)    # For wrangling data
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

## Outliers.

#### Transform Median Price

``` r
median_price_outliers = boxplot(city_of_melbourne$Median_Price, 
                                horizontal = TRUE)
```

![](handle_NA_files/figure-gfm/outliers%20in%20Median_Price-1.png)<!-- -->

``` r
par(mfrow = c(2,2))

# original median price
hist(city_of_melbourne$Median_Price, main = "Median Price") 
qqnorm(city_of_melbourne$Median_Price)
qqline(city_of_melbourne$Median_Price)

# base e log transformation
log_median_price <-log(city_of_melbourne$Median_Price)
hist(log_median_price, main = "Log Transformation \nof Median Price")
qqnorm(log_median_price)
qqline(log_median_price)
```

![](handle_NA_files/figure-gfm/tranform%20median%20price-1.png)<!-- -->

``` r
# square root transformation
sqrt_median_price <- sqrt(city_of_melbourne$Median_Price)
hist(sqrt_median_price, main = "Squareroot Transformation \nof Median Price") 
qqnorm(sqrt_median_price)
qqline(sqrt_median_price)

# Box-Cox transformation
boxcox_median_price <- BoxCox(city_of_melbourne$Median_Price, lambda = "auto")
hist(boxcox_median_price, main = "Box-Cox Transformation \nof Median Price")
qqnorm(boxcox_median_price)
qqline(boxcox_median_price)
```

![](handle_NA_files/figure-gfm/tranform%20median%20price-2.png)<!-- -->

``` r
shapiro.test(log_median_price)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  log_median_price
    ## W = 0.87994, p-value = 5.255e-06

``` r
shapiro.test(boxcox_median_price)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  boxcox_median_price
    ## W = 0.62888, p-value = 3.339e-12

#### Facilities

``` r
city_of_melbourne %<>%
  mutate(Total_Facilities = Cafes_Restaurants + Takeaway_Food) 

par(mfrow=c(2,2))
boxplot(city_of_melbourne$Cafes_Restaurants, horizontal = TRUE,
        main = "Number of \nCafes and Restaurants")
boxplot(city_of_melbourne$Takeaway_Food, horizontal = TRUE,
        main = "Number of \nTakeaway Food Services")

facilities_outliers <- boxplot(city_of_melbourne$Total_Facilities,
                              horizontal = TRUE,
                              main = "Total Facilities in \nCity of Melbourne")$out
city_of_melbourne %>%
  filter(Total_Facilities %in% facilities_outliers) %>%
  dplyr::select(Suburb, Year, Total_Facilities)
```

    ##      Suburb Year Total_Facilities
    ## 1 Melbourne 2018             1831
    ## 2 Melbourne 2018             1831
    ## 3 Melbourne 2019             1814
    ## 4 Melbourne 2019             1814
    ## 5 Melbourne 2020             1622
    ## 6 Melbourne 2020             1622

![](handle_NA_files/figure-gfm/outliers%20in%20facilities-1.png)<!-- -->

``` r
# Total Facilities in City of Melbourne during 2012-2022 
city_of_melbourne %>%
  group_by(Suburb, Year) %>%
  summarise(sum_facilities = sum(Total_Facilities)) %>%
  ggplot() +
  geom_line(aes(x=Year, y=sum_facilities, colour = Suburb))
```

![](handle_NA_files/figure-gfm/line%20chart-1.png)<!-- -->

``` r
par(mfrow = c(2,2))

# original total facilities
hist(city_of_melbourne$Total_Facilities, main = "Total Facilities") 
qqnorm(city_of_melbourne$Total_Facilities)
qqline(city_of_melbourne$Total_Facilities)

# base e log transformation
log_facilities <-log(city_of_melbourne$Total_Facilities)
hist(log_facilities, main = "Log Transformation \nof Total Facilities")
qqnorm(log_facilities)
qqline(log_facilities)
```

![](handle_NA_files/figure-gfm/transform%20Facilities-1.png)<!-- -->

``` r
# square root transformation
sqrt_facilities <- sqrt(city_of_melbourne$Total_Facilities)
hist(sqrt_facilities, main = "Squareroot Transformation \nof Total Facilities")
qqnorm(sqrt_facilities)
qqline(sqrt_facilities)


# Box-Cox transformation
boxcox_facilities <- BoxCox(city_of_melbourne$Total_Facilities, lambda = "auto")
hist(boxcox_facilities, main = "Box-Cox Transformation \nof Total Facilities")
qqnorm(boxcox_facilities)
qqline(boxcox_facilities)
```

![](handle_NA_files/figure-gfm/transform%20Facilities-2.png)<!-- -->

#### Neighborhood_Safety and Public_Trans_Safety

``` r
city_of_melbourne %>%
  ggplot(aes(Neighborhood_Safety, col = Day_or_Night)) +
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](handle_NA_files/figure-gfm/Neighborhood_Safety%20and%20Public_Trans_Safety-1.png)<!-- -->

``` r
city_of_melbourne %>%
  ggplot(aes(Public_Trans_Safety, col = Day_or_Night)) +
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](handle_NA_files/figure-gfm/Neighborhood_Safety%20and%20Public_Trans_Safety-2.png)<!-- -->

``` r
boxplot(city_of_melbourne$Neighborhood_Safety, horizontal = TRUE,
        main = "Perception of Safety in Neighborhood")
```

![](handle_NA_files/figure-gfm/Neighborhood_Safety%20and%20Public_Trans_Safety-3.png)<!-- -->

``` r
boxplot(city_of_melbourne$Public_Trans_Safety, horizontal = TRUE,
        main = "Perception of Safety on Public Transport")
```

![](handle_NA_files/figure-gfm/Neighborhood_Safety%20and%20Public_Trans_Safety-4.png)<!-- -->

#### Bivariate outliers

``` r
ggplot(city_of_melbourne,
       aes(Neighborhood_Safety, Public_Trans_Safety , colour= Day_or_Night, palette(hsv))) +
  geom_point(size = 3, alpha=0.5)
```

![](handle_NA_files/figure-gfm/bivariate%20outliers-1.png)<!-- -->

``` r
city_of_melbourne %>%
  filter(Neighborhood_Safety < 40)
```

    ##    X    Suburb Year Day_or_Night Neighborhood_Safety Public_Trans_Safety
    ## 1 46 Parkville 2019        Night                36.9                49.5
    ##   Cafes_Restaurants Takeaway_Food Median_Price Total_Facilities
    ## 1                52            17      1480000               69
