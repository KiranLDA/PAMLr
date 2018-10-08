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

Once devtools is installed, the github package can be installed:

```r
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
```
## Plot the raw light data as an interactive plot

This will create an interactive dygraph plot to compare different measurements recorded by the logger. If you are in Rstudio, it will open this in the viewer pane and use a lot of ram. This bit of code allows you to open this window instead in a browser and the file can later be saved as an html file.

```r
# In Rstudio, it will display in the viewer by default and use a lot of ram, and is better in html
backup_options <- options() 
options(viewer=NULL) # ensure it is viewed in internet browser
dygraphPAM(dta = PAM_data) # plot
options(backup_options) # restore previous viewer settings
```
## Looking at data quality

From looking at the interactive plot, it's possible to tell that there was a point where the logger was taken off and probably in a rucksack, remove these periods where the logger was not in fact on the bird to remove any potential future errors in the analysis.

```r
#plot the activity data with daylight periods in yellow and nightime periods in grey
plot(PAM_data$acceleration$date[6000:9000], PAM_data$acceleration$act[6000:9000],
        type="o", xlab="Date", ylab="Light Intensity", pch=20,
        col=ifelse(PAM_data$light$obs[6000:9000]>0,"darkgoldenrod1","azure3"))

# at first glance it looks like the logger was removed off a birds and left in a rucksack
# so we should remove any un-needed data
PAM_data$acceleration = PAM_data$acceleration[(PAM_data$acceleration$date >= "2016-07-30" & PAM_data$acceleration$date <= "2017-06-01"),]
```
## Looking at data quality

Classify bird's behaviour based on  activity (relevant for birds which flap, such as a hoopoe). Here we assume that if a bird is active for more than 15 minutes, then the bird is flying. Because loggers record every 5 minutes, we use a period of 3 (i.e. 3x5min=15min)

```r
behaviour = classifyFLAP(dta = PAM_data$acceleration, period = 3)

# plot the classification
col=col=c("brown","cyan4","gold","black")
plot(PAM_data$acceleration$date[2000:4000],PAM_data$acceleration$act[2000:4000],
  col=col[behaviour$classification][2000:4000], 
  type="o", pch=20, xlab="Date", ylab="Activity")
legend( PAM_data$acceleration$date[2000],60 , 
        c("No activity", "Low activity", "High activity", "Migration" ) ,
        col = col[c(behaviour$no_activity, behaviour$low_activity,
                    behaviour$high_activity, behaviour$migration)],
        pch=20)

# look at timetable
behaviour$timetable
```

## Authors

Kiran Dhanjal-Adams

## License

This project is licensed under the GNU General Public License version 3 - see the [LICENSE](https://github.com/KiranLDA/PAMLr/blob/master/LICENSE) file for details
