---
title: "PA1_template.md"
output: html_document
---
## Load Data

```{r}
data <- read.csv("activity.csv", header = TRUE, sep = ",", na.strings = "NA")

```

## Transforming Data

```{r}

data$date <- as.Date(data$date, format = "%Y-%m-%d")
data$interval <- factor(data$interval)

```

## Subsitting data to ignore missing values


```{r}
NA_index <- is.na(as.character(data$steps))
data_no_NA <- data[!NA_index,]
head(data_no_NA)

```

## Agreting steps taken each day

```{r}
steps_each_day <- aggregate(steps ~ date, data = data_no_NA, sum)
colnames(steps_each_day) <- c("date", "steps")

```


## Creating Histogram

```{r}
hist(as.numeric(steps_each_day$steps), breaks = 20, col = "red", xlab = "Number of Steps", main= "Histogram of the total number of steps taken each day")

```


## Mean

```{r}
mean(steps_each_day$steps)

```

## Median

```{r}
median(steps_each_day$steps)

```

## Average number of steps taken

```{r}
steps_per_interval <- aggregate(data_no_NA$steps, by=list(interval=data_no_NA$interval), FUN=mean)
colnames(steps_per_interval) <- c("interval", "average_steps")

```

## Ploting Average daily Activity

```{r}
plot(as.integer(levels(steps_per_interval$interval)), steps_per_interval$average_steps, type="l",
     xlab = "Interval", ylab = "Average Number of Steps", main = "Average Daily Activity Pattern",  col ="blue")

```

## Maximu number of average steps

```{r}
max_steps <- max(steps_per_interval$average_steps)
max_steps

```


## 5 minute intervals containing maximum number of steps

```{r}
intervale_max_steps<-steps_per_interval[which.max(steps_per_interval$average_steps),]$interval
intervale_max_steps

```


## Missing values - steps variable

```{r}
sum(is.na(as.character(data$steps)))

```


## Missing values - date variable

```{r}
sum(is.na(as.character(data$date)))

```

## Missing Value - Interval Variable

```{r}
sum(is.na(as.character(data$interval)))

```


## Filling missing values in dataset



```{r}
NA_index <- which(is.na(as.character(data$steps)))
complete_data <- data
```

#Imputing missing values using the mean for that 5-minute interval

```{r}
complete_data[NA_index, ]$steps<-unlist(lapply(NA_index, FUN=function(NA_index){
  steps_per_interval[data[NA_index,]$interval==steps_per_interval$interval,]$average_steps
}))

```

## Histogram for total number of steps taken each day to complete data set

#Creating a data frame with the steps taken for each day

```{r}
steps_each_day_complete <- aggregate(steps ~ date, data = complete_data, sum)
colnames(steps_each_day_complete) <- c("date", "steps")


```

# The histogram

```{r}
hist(as.numeric(steps_each_day_complete$steps), breaks = 20, col = "red", xlab = "Number of Steps", main= "Histogram of the total number of steps taken each day")


```


## Mean and Median to complete data set 

# Mean

```{r}
mean(steps_each_day_complete$steps)

```

# Median

```{r}
median(steps_each_day_complete$steps)

```

## Diffrence in Activity patterns between weekdays and weekends

# Factor variable "day "to store the day of the week

```{r}
complete_data$day <- as.factor(weekdays(complete_data$date))

```


# Logical variable "is_weekday" (weekday=TRUE, weekend = FALE)

```{r}
complete_data$is_weekday <- ifelse(!(complete_data$day %in% c("Saturday","Sunday")), TRUE, FALSE) 

```


# Average number of steps for weekdays

```{r}
weekdays_data <- complete_data[complete_data$is_weekday,]
steps_per_interval_weekdays <- aggregate(weekdays_data$steps, by=list(interval=weekdays_data$interval), FUN=mean)

```


# Calculating the average number of steps for weekends

```{r}
weekends_data <- complete_data[!complete_data$is_weekday,]
steps_per_interval_weekends <- aggregate(weekends_data$steps, by=list(interval=weekends_data$interval), FUN=mean)


```


# Adding columns names

```{r}
colnames(steps_per_interval_weekdays) <- c("interval", "average_steps")
colnames(steps_per_interval_weekends) <- c("interval", "average_steps")
#Adding a column to indecate the day
steps_per_interval_weekdays$day <- "Weekday"
steps_per_interval_weekends$day <- "Weekend"

```


# Merging the two togather

```{r}
week_data <- rbind(steps_per_interval_weekends, steps_per_interval_weekdays)
#Converting the day variabke to a factor
week_data$day <- as.factor(week_data$day)

```


# Ploting

```{r}
library(lattice)
xyplot(average_steps ~  interval | day, data = week_data, layout = c(1,2), type ="l", ylab="Number of Steps")


```







