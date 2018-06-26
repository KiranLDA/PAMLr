# Package PAMLr

Pamela. This packages provides a set of functions to analyse data collected by SOI-GDL3pam loggers (developped by the Swiss Ornithological Institute www.vogelwarte.ch/en ). These measure atmospheric pressure (P), activity (A), magnetisim (M) and light (L) for analysis in R.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

### Prerequisites

These packages used by this package are `utils`

```r
install.packages("utils") # usually already in base r

```

### Installing

To install this package from github, make sure you first have `devtools` installed.

```r
install.packages("devtools")
```

Once devtools is installed, type:

```r
library(devtools)
devtools::install_github("KiranLDA/PAMLr")
```

## Load and test

To make sure the package works run the following

```r
# load library
library(PAMLr)

#specify the data location
data(PAM_data)
str(PAM_data)

#plot the raw light data
plot(PAM_data$light$date[3000:5000], PAM_data$light$obs[3000:5000],
        type="o", xlab="Date", ylab="Light Intensity", 
        col=ifelse(PAM_data$light$obs[3000:5000]>0,"darkgoldenrod1","azure3"))


#plot the activity data with daylight periods in yellow and nightime periods in grey
plot(PAM_data$acceleration$date[6000:9000], PAM_data$acceleration$act[6000:9000],
        type="o", xlab="Date", ylab="Light Intensity", pch=20,
        col=ifelse(PAM_data$light$obs[6000:9000]>0,"darkgoldenrod1","azure3"))

# at first glance it looks like the logger was removed off a birds and left in arucksack
#  # so remove un-needed data
PAM_data$acceleration = PAM_data$acceleration[(PAM_data$acceleration$date >= "2016-07-30" & PAM_data$acceleration$date <= "2017-06-01"),]

# classify bird's behaviour based on  activity, assume that if a bird is active for more than 
# 3x5 minutes = 15 minutes, then the bird is migration
behaviour = classify_flapping(dta = PAM_data$acceleration, flapping_duration = 3)

# check the classification
col=col=c("brown","cyan4","gold","black")
plot(PAM_data$acceleration$date[2000:4000],PAM_data$acceleration$act[2000:4000],
  col=col[behaviour$classification][2000:4000], type="o", pch=20, xlab="Date", ylab="Activity")
legend( PAM_data$acceleration$date[2000],60 , c("No activity", "Low activity", "High activity", "Migration" ) ,
  col = col[c(behaviour$no_activity, behaviour$low_activity,behaviour$high_activity, behaviour$migration)], pch=20)

# look at timetable
behaviour$timetable
```

## Authors

Kiran Dhanjal-Adams

## License

This project is licensed under the Apache 2.0 License - see the [LICENSE](https://github.com/KiranLDA/PAMLr/blob/master/LICENSE) file for details
