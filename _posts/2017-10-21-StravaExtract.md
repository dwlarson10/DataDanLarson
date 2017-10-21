---
layout: post
title:  "Building a Strava Dashboard Post 1: Extracting Data"
tags:
- R
- tidyverse
- ggplot2
---


What is the project?
====================

The goal of this blog is for me to share various techniques as I learn them. The next series of posts is my attempt at learning to build a dashboard for my Strava data. I am training for a Half in December and what better way to stay motivated than to monitor my progress. In this post, I will extract the Strava data using their API and build a few visualizations to see if I have any patterns.

Extracting data from Strava.
----------------------------

The first step is to set up the API to pull. Before you can write any code you need to set up your Strava app \[<https://www.strava.com/settings/api>\]. Once you have set up your Strava app you can extract data.

The code below sets up the key, authorization and access. Then it sets up the authentication token. Next, we use the jsonlite package to do the actual data extraction.

``` r
library(httr)
library(httpuv)
my_app <- oauth_app("strava",
                    key = "[your key]",
                    secret = "[your secret key]"
)


my_endpoint <- oauth_endpoint(
  request = NULL,
  authorize = "https://www.strava.com/oauth/authorize",
  access = "https://www.strava.com/oauth/token"
)


sig <- oauth2.0_token(my_endpoint, my_app, scope = "view_private",  
                      type = NULL, use_oob = FALSE, as_header = FALSE,   
                      use_basic_auth = TRUE,
                      cache = TRUE)




library(jsonlite)
jsonData <- fromJSON("https://www.strava.com/api/v3/athlete/activities?access_token=cb576993aa710c4bf32ac7b9a006a0f110add218&per_page=200",flatten = TRUE)
```

Once I have loaded the data into our R console, I can start working with it. There are already 49 variables but there are a few that still need some work. First, we turn the data set into a tibble. A tibble is similar to a dataframe but interacts with the Tidyverse. We can then use the Mutate function to create the variables we want. We first modify the date to be a more R friendly format. Next we turn the distance variable from meters to miles. We then create a year, month, weekday, and hour of day variables from the time. For now, these variables are all we need. However, if we decide that we want more we can away use the mutate variable to quickly add more.

``` r
require(tidyverse)
```

    ## Loading required package: tidyverse

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Warning: package 'dplyr' was built under R version 3.4.2

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
require(lubridate)
```

    ## Loading required package: lubridate

    ##
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ##
    ##     date

``` r
strava <- as.tibble(jsonData)%>%
  mutate(start_date_local = ymd_hms(start_date_local),
         miles = distance / 1609.344,
         year = year(start_date_local),
         wday = wday(start_date_local,label=TRUE),
         month = month(start_date_local,label=TRUE),
         hour = hour(start_date_local))
```

Quick static analysis
---------------------

To start, I looked at what my running distance by month has been since starting to run in 2015. Interestingly, I steadily ran more in 2015 than I did last year. However, this year I am on track to have run more.

``` r
strava%>%group_by(year,month)%>%
  summarise(distance = sum(miles),heart = mean(average_heartrate),runs = n())%>%
  ggplot(aes(x=month,y=distance,group=year,color=factor(year)))+
  geom_line(stat='identity')+
  geom_point()+
  ggtitle("Total distance by month of year")
```

<p><img src="http://danlarson.io/static/img/TotalDistance-1.png" alt="Total distance by month" /></p>

This could be deceiving especially knowing when I typically do half marathons. So, I also looked at the average length for each month. By adding this view of the data I am able to learn that while I ran more miles in 2015 than in 2016 my average run for each month was longer in 2016. By create a dashboard for these data, I will be able to remind myself of the trend I am hoping to create and hopefully encourage myself to meet my goals.

``` r
strava%>%group_by(year,month)%>%
  summarise(distance = mean(miles),heart = mean(average_heartrate),runs = n())%>%
  ggplot(aes(x=month,y=distance,group=year,color=factor(year)))+
  geom_line(stat='identity')+
  geom_point()+
  ggtitle("Average distance by month of year")
```

<p><img src="http://danlarson.io/static/img/AverageDistance-1.png" alt="Average distance by month" /></p>

In addition to running further I have also been training to go faster. While some of my efforts appear to have worked in the beginning of 2017, I have a ways to go to break out of the pace I appear to be currently occupying.

``` r
strava%>%group_by(year,month)%>%
  summarise(distance = sum(miles), speed = mean(average_speed*(25/11)),heart = mean(average_heartrate),runs = n())%>%
  ggplot(aes(x=month,y=speed,group=year,color=factor(year)))+
  geom_line(stat='identity')+
  geom_point()+
  ggtitle("Average Speed by month of year")
```
<p><img src="http://danlarson.io/static/img/AverageSpeed-1.png" alt="Average distance by month" /></p>

In this post, I shared how to extract data using an R script and created a few charts that demonstrated why I am looking to build a dashboard to help improve my running routine. The next steps will be to learn write a shiny app to extract the Strava data when needed and build a dashboard. I hope to use the package FlexDashboards by RStudio to create the dashboard.

If you found this interesting please let me know by finding me on Twitter.
