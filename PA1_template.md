---
title: "First project John Hopkins"
author: "Akanksha"
date: "2/22/2022"
output:
  html_document: 
    keep_md: yes
  pdf_document: default
  word_document: 
    keep_md: yes
---


Lets begin with step 1 and read the data here.



```r
df <- read.csv("activity.csv" , header = TRUE)
##Taking a sneak peak inside data to make sure its read correctly.
head(df)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
For a better understanding of data, we can see its glimpse using the glimpse function.
we would need to load Tidyverse package in order to use this fucntion

```r
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
```

```
## v ggplot2 3.3.5     v purrr   0.3.4
## v tibble  3.1.6     v dplyr   1.0.7
## v tidyr   1.1.4     v stringr 1.4.0
## v readr   2.1.1     v forcats 0.5.1
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
glimpse(df)
```

```
## Rows: 17,568
## Columns: 3
## $ steps    <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
## $ date     <chr> "2012-10-01", "2012-10-01", "2012-10-01", "2012-10-01", "2012~
## $ interval <int> 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 100, 105, 110, ~
```
we notice that date column of the dataset is "character" type. we need to change this using lubridate package and converting it into date type.

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
df$date <- ymd(df$date)
```
To begin with the code for step 2 , plotting Histogram of total number of steps taken each day , we first need to calculate the total steps for each day.

```r
histdf <- df%>%group_by(date)%>%summarise("total_steps" = sum(steps))
head(histdf)
```

```
## # A tibble: 6 x 2
##   date       total_steps
##   <date>           <int>
## 1 2012-10-01          NA
## 2 2012-10-02         126
## 3 2012-10-03       11352
## 4 2012-10-04       12116
## 5 2012-10-05       13294
## 6 2012-10-06       15420
```
Here we get a tibble object displaying the summarised data where each date is followed by a column stating the total steps taken on that day. 
Now we will proceed with the histogram.

```r
hist(histdf$total_steps, xlab = "total steps", main = "Histogram of the total number of steps taken each day" , col = "orange")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
For step 3, we can use Tidyverse package to summarise data in terms of mean and median.

```r
dfstep3 <- df%>%group_by(date)%>%summarise("mean_steps" = mean(steps)
                                           ,"median_steps" = median(steps))
head(dfstep3)
```

```
## # A tibble: 6 x 3
##   date       mean_steps median_steps
##   <date>          <dbl>        <dbl>
## 1 2012-10-01     NA               NA
## 2 2012-10-02      0.438            0
## 3 2012-10-03     39.4              0
## 4 2012-10-04     42.1              0
## 5 2012-10-05     46.2              0
## 6 2012-10-06     53.5              0
```
Here we proceed to next step. First we will summarise average steps for each interval across all days, then we will proceed with it's plot.

```r
dfstep4 <- df%>%group_by(interval,date)%>%summarise("avgsteps" = mean(steps))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the `.groups` argument.
```

```r
library(ggplot2)
dfstep4%>%ggplot(mapping = aes( x = interval,  y = avgsteps))+
  geom_line()+
  labs( x = "Interval across all days", y = "Average steps taken",title =       "plot for average steps taken in each time interval")
```

```
## Warning: Removed 2 row(s) containing missing values (geom_path).
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
to get the interval containing maximum number of steps, we can make use  of above tibble and calculate max

```r
dfstep4[which.max(dfstep4$avgsteps),]
```

```
## # A tibble: 1 x 3
## # Groups:   interval [1]
##   interval date       avgsteps
##      <int> <date>        <dbl>
## 1      615 2012-11-27      806
```
here we get the interval for which we have maximum number of average steps.

Now for the next task, we first need to report the total number of missing values in the dataset 

```r
nrow(df[!complete.cases(df),])
```

```
## [1] 2304
```
we will now devise a strategy to fill all of the NA values and replace it with 0 instead.

```r
 df$steps <- df$steps%>%replace_na("0")
```
Now we have a new dataset where all the missing values i.e NA is now replaced with zero.
For the next step of assignment , we will see if this brings any change to the plots we earlier made.

```r
df$steps <- as.numeric(df$steps)
histdf <- df%>%group_by(date)%>%summarise("total_steps"=(sum(steps)))
hist(histdf$total_steps, xlab = "total steps", main = "Histogram of the total number of steps taken each day" , col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
The histogram plotting doesn't seem much changed. We will next check it for other reportings calculated for mean and median.

```r
dfstep7 <- df%>%group_by(date)%>%summarise("mean_steps" = mean(steps)
                                           ,"median_steps" = median(steps))
head(dfstep7)
```

```
## # A tibble: 6 x 3
##   date       mean_steps median_steps
##   <date>          <dbl>        <dbl>
## 1 2012-10-01      0                0
## 2 2012-10-02      0.438            0
## 3 2012-10-03     39.4              0
## 4 2012-10-04     42.1              0
## 5 2012-10-05     46.2              0
## 6 2012-10-06     53.5              0
```
as per our observations, it turns out there is no significant difference in the graphs for both missing values and without missing values , 
supposedly the reason being we replaced values with 0 nor some other value such as mean or median. This preserves the correct calculations for our data.

Now here we go for the next task that is to create new factor variable with two levels "weekdays" and "weekend"


```r
df$date <-  as.POSIXct(df$date, format = "%Y-%m-%d")
df <- df%>%mutate("Day_of_week" =  weekdays(date))
df$Day_of_week <- recode("Monday" = "weekday","Tuesday"="weekday","Wednesday"="weekday","Thursday"="weekday",
                         "Friday" = "weekday","Saturday"="weekend",
                         "Sunday"="weekend",df$Day_of_week)

df$Day_of_week <- as.factor(df$Day_of_week)
head(df, 10)
```

```
##    steps                date interval Day_of_week
## 1      0 2012-10-01 05:30:00        0     weekday
## 2      0 2012-10-01 05:30:00        5     weekday
## 3      0 2012-10-01 05:30:00       10     weekday
## 4      0 2012-10-01 05:30:00       15     weekday
## 5      0 2012-10-01 05:30:00       20     weekday
## 6      0 2012-10-01 05:30:00       25     weekday
## 7      0 2012-10-01 05:30:00       30     weekday
## 8      0 2012-10-01 05:30:00       35     weekday
## 9      0 2012-10-01 05:30:00       40     weekday
## 10     0 2012-10-01 05:30:00       45     weekday
```
Now we can proceed with plotting the panel plot for time series plot.

