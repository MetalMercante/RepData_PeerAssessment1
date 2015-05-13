# Reproducible Research - Peer Assignment 01

## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]
The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

date: The date on which the measurement was taken in YYYY-MM-DD format

interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Assignment

This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.


### Loading and preprocessing the data
Show any code that is needed to

Load the data (i.e. read.csv())

Process/transform the data (if necessary) into a format suitable for your analysis

> I found no need to process the data for this exercise except the date transformations in the last part


```r
library(dplyr, warn.conflicts = F)
library(tidyr)
library(knitr)
library(ggplot2)
library(lubridate)
library(scales)

setwd("D:\\Google_Drive\\R\\Reproducible_1")
base <- read.csv("activity.csv")
base$date <- as.Date(ymd(base$date))
```


###What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day


```r
stepsaday <- base %>% group_by(date) %>% summarise(stepsaday=sum(steps,na.rm=T))
#plot(stepsaday)
ggplot(stepsaday,aes(x=date,y=stepsaday))+
        geom_bar(stat="identity", fill="skyblue")+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        scale_x_date(labels = date_format("%m/%d"))+
        ggtitle("Total number of steps taken per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
#hist(stepsaday$stepsaday, breaks=20)
ggplot(stepsaday,aes(x=stepsaday))+
        geom_histogram(fill="skyblue")+
        ggtitle("Histogram - Total number of steps taken each day")+
        theme_bw()
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day


```r
summary(stepsaday$stepsaday)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10400    9354   12810   21190
```


### What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
step5 <- base %>% group_by(interval) %>%
                summarise(stepsaday=mean(steps,na.rm=T))

#plot(x = step5$interval, y = step5$stepsaday, type="l")
ggplot(step5, aes(x=interval,y=stepsaday, group=1))+
        geom_line(size=1, color="red", alpha=0.4)+
        theme_bw()+
        ggtitle("Steps per Time Interval")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
head(step5 %>% arrange(desc(stepsaday)),1)
```

```
## Source: local data frame [1 x 2]
## 
##   interval stepsaday
## 1      835  206.1698
```

### Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(base$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

> 2. and 3. are treated together in the code below. First I create a dataframe that contains only missing values, then I merge this dataframe with the step5 dataframe (mean values) by interval.

> This results in two separate dataframes. The original data without NAs and the modiffied dataframe with NAs equal to the mean.

> Merge the two dataframes and arrange

> * This would probably be easier with Base R operations


```r
base2 <- base
base3 <- base2 %>% filter(is.na(steps))
NAMean <- merge(base3,step5, by="interval") %>%
        select(-steps) %>% 
        mutate(steps = stepsaday) %>%
        select(-stepsaday)

base4 <- base2 %>% filter(!is.na(steps))
base5 <- merge(base4, NAMean, all=T)
base5 <- base5 %>% arrange(date,interval)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
stepsadayNA <- base5 %>% group_by(date) %>% summarise(stepsaday=sum(steps,na.rm=T))

#hist(stepsadayNA$stepsaday,col='skyblue',border=F, breaks=20)
ggplot(stepsadayNA,aes(x=stepsaday))+
        geom_histogram(fill="red", alpha=0.4)+
        ggtitle("Histogram - Total number of steps taken - NA Adjusted")+
        theme_bw()
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

```r
summary(stepsadayNA$stepsaday)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```

```r
summary(stepsaday$stepsaday)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10400    9354   12810   21190
```

> The strategy for filling missing values just transforms NAs into the median value. This results in a 'jump' from zeroes to the median value seen in the chart below.


```r
#hist(stepsadayNA$stepsaday,col='skyblue',border=F, breaks=20)
#hist(stepsaday$stepsaday,add=T,col=scales::alpha('red',.5),border=F,breaks=20)
steps <- data.frame(NAAdj=stepsadayNA$stepsaday,Original=stepsaday$stepsaday)
steps <- steps %>% gather("StepData","Value",1:2)
ggplot(steps,aes(x=Value, fill=StepData))+
        geom_histogram(position="dodge")+
        ggtitle("Histogram - Total number of steps taken - NA Adjusted")+
        theme_bw()
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

### Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

> I live in Brazil. All date functions return dates in Portuguese, so I have to change that.


```r
Sys.setlocale("LC_ALL","C")
```

```
## [1] "C"
```

```r
weekends <- c("Saturday","Sunday")
base6 <- base5 %>% mutate(weekday=weekdays(ymd(base5$date)),
                          type=ifelse(weekday %in% weekends,"weekend","weekday"))
```
        
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
step5NA <- base6 %>% group_by(interval,type) %>%
        summarise(stepsaday=mean(steps,na.rm=T))

ggplot(step5NA, aes(x=interval,y=stepsaday, group=1))+
        geom_line(aes(colour=type))+facet_grid(type~.)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 


