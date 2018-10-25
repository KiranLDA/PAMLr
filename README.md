# Package PAMLr

Pamela. This packages provides a set of functions to analyse data collected by SOI-GDL3pam loggers (developped by the Swiss Ornithological Institute www.vogelwarte.ch/en ). These measure atmospheric pressure (P), activity (A), magnetisim (M) and light (L) for analysis in R.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

### Prerequisites

The packages used by this package are `dplyr`,`dygraphs`,`graphics`,`grDevices`,`htmltools`,`stats`,`tcltk`,`utils`,`xts`,`zoo`. If there are any issues with installing PAMLr, please ensure these packages are installed and working.

### Installing

To install this package from github, make sure you first have `devtools` installed.

```r
install.packages("devtools")
```
The github package can be installed:

```r
devtools::install_github("KiranLDA/PAMLr")

```

## Load and test

To make sure the package works run the following

```r
# load library
library(PAMLr)

# get an example of PAM data
data(PAM_data)

# Look at that data
str(PAM_data)
```


To import your own data use:
```r
importPAM("C:/Put/your/path/here")`
```

This is what it should look like:

```
# # List of 6
# #  $ id          : chr "16AJ"
# #  $ pressure    :'data.frame':	37412 obs. of  2 variables:
# #   ..$ date: POSIXct[1:37412], format:  ...
# #   ..$ obs : int [1:37412] 969 969 969 969 969 969 969 969 969 969 ...
# #  $ light       :'data.frame':	112401 obs. of  2 variables:
# #   ..$ date: POSIXct[1:112401], format:  ...
# #   ..$ obs : int [1:112401] 0 0 0 0 0 0 0 0 0 0 ...
# #  $ acceleration:'data.frame':	111900 obs. of  3 variables:
# #   ..$ date: POSIXct[1:111900], format:  ...
# #   ..$ pit : int [1:111900] 10 10 10 10 10 10 11 11 11 11 ...
# #   ..$ act : int [1:111900] 0 0 0 0 0 0 2 0 0 0 ...
# #  $ temperature :'data.frame':	36818 obs. of  2 variables:
# #   ..$ date: POSIXct[1:36818], format:  ...
# #   ..$ obs : int [1:36818] 33 33 33 33 33 33 33 33 33 33 ...
# #  $ magnetic    :'data.frame':	1559 obs. of  7 variables:
# #   ..$ date: POSIXct[1:1559], format:  ...
# #   ..$ gX  : int [1:1559] 849 -487 211 505 725 -2048 454 -126 919 -886 ...
# #   ..$ gY  : int [1:1559] -2035 -2182 -2601 -2581 -2507 -1847 -2582 -2650 -2437 -2574 ...
# #   ..$ gZ  : int [1:1559] -1642 -1962 351 -20 118 -1626 41 -76 -152 -1327 ...
# #   ..$ mX  : int [1:1559] -1600 -947 -2779 7 -1852 5844 1061 -118 -2196 2493 ...
# #   ..$ mY  : int [1:1559] 15645 15610 15259 15549 15561 14545 16631 14548 15195 10924 ...
# #   ..$ mZ  : int [1:1559] 5559 4627 6374 6147 5887 10881 5177 9000 6810 13793 ...
```


## Plot the raw light data as an interactive plot

Within `PAMLr` it is possible to create interactive `dygraphPAM()` plots which allow you to compare different measurements recorded by the logger. These might for instance include light, temperature, pressure, activity, pitch and magnetism. 

If you are **working from Rstudio**, this bit of code should be run:

```r
# In Rstudio, it will display in the viewer by default and use a lot of ram, and is better in html
backup_options <- options() 
options(viewer=NULL) # ensure it is viewed in internet browser
dygraphPAM(dta = PAM_data) # plot
options(backup_options) # restore previous viewer settings
```
If you are **working from base R** use this instead:

```r
dygraphPAM(dta = PAM_data) # plot
```
The reason there is additional code for Rstudio, is that by default it will open this graphic in the viewer pane and use up a lot of ram. This the additional code allows you to open this window in a browser instead of r studio, and the file can later be saved as an html file.

<img align="center" src="https://raw.githubusercontent.com/KiranLDA/PAMLr/master/graphics/dygraphPAM.png">

With this interactive plot, you can then zoom in and out of different plots to help get a feel for the data. For instance, this is a great way of seeing changes in the data which might be due to a logger being in a rucksack and no longer on the birds, or to look at how acticity or pressure might look during migration periods.

It is possible to select areas to zoom into by right clicking and highighting certain regions, and to double click to zoom out. All plots are synched to the same time period and have a timeline at the bottom to increase or decrease the time over which the data is observed.


## Looking at data quality

From looking at the interactive plot, it's possible to tell that there was a point where the logger was taken off the bird and probably left in a rucksack, car or lab before the data were downloaded. These periods should therefore be removed from the data. This will depend on which sensors are on the tag, and should be edited accordingly

```r
# light
PAM_data$light = PAM_data$light[(PAM_data$light$date >= "2016-07-30" & PAM_data$light$date <= "2017-06-01"),]

# acceleration
PAM_data$acceleration = PAM_data$acceleration[(PAM_data$acceleration$date >= "2016-07-30" & PAM_data$acceleration$date <= "2017-06-01"),]

# pressure
PAM_data$pressure = PAM_data$pressure[(PAM_data$pressure$date >= "2016-07-30" & PAM_data$pressure$date <= "2017-06-01"),]

# temperature
PAM_data$temperature = PAM_data$temperature[(PAM_data$temperature$date >= "2016-07-30" & PAM_data$temperature$date <= "2017-06-01"),]

#magnetic
PAM_data$magnetic = PAM_data$magnetic[(PAM_data$magnetic$date >= "2016-07-30" & PAM_data$magnetic$date <= "2017-06-01"),]
```
## Classifying bird migration

Before starting to classify bird migration from PAM data, it's important to consider what type of flight the bird has. Does is flap, soar, soar-flap or soar-glide for isntance? These will have a large bearing on how behaviour is classified. The easiest type of flight to classify is a  continuously flapping bird, such as a kingfisher, a hoopoe or a shrike.

### Classifying flapping behaviour

Continuously flapping birds have higher activity than soaring birds. You can therefore use the `classifyFLAP()` function to classify bird behaviour. This function assumes that if the bird has displayed high activity for x number of minutes, then it is flapping. It is therefore important to think about what constitutes high activity and how long this period should be. At the moment, the function uses k-means clustering to identify the threshold between high and low activity. The using `toPLOT = TRUE` allows you to see where that threshold was drawn. The period of high activity is set by default to `period = 3`. This is because activity is recorded (on this logger) every 5 minutes and we assume that after 15 minutes of high activity, the bird must be flapping, thus "high activity duration" / "data resolution" = "period" and 5 minutes / 15 minutes = period of 3.


```r
behaviour = classifyFLAP(dta = PAM_data$acceleration, period = 3)
```
<img align="center" src="https://raw.githubusercontent.com/KiranLDA/PAMLr/master/graphics/classification_thershold.png">

This will give you various information. It will tell you how the data were classidied in `type`. It will also give you a `timetable` of when the bird was flying, with start and end times, and the duration of this flight in hours. This can be manipulated as needed. It then also gives you the `classification` of each time point and specified what each classification stands for, which value represents `low_activity`, `high_activity`, `migration` or `no_activity`. And finally what the `threshold` was between high and low activity.

```r
behaviour
```
```
# # $`type`
# # [1] "flapping"
# # 
# # $timetable
# #                  start                 end Duration (h)
# # 3  2016-08-06 20:20:00 2016-08-07 01:50:00    5.5000000
# # 4  2016-08-07 19:40:00 2016-08-08 09:15:00   13.5833333
# # 5  2016-08-08 19:30:00 2016-08-09 04:10:00    8.6666667
# # 6  2016-08-09 21:15:00 2016-08-10 01:30:00    4.2500000
# # 7  2016-08-10 22:30:00 2016-08-10 23:50:00    1.3333333
# # 8  2016-08-21 18:45:00 2016-08-22 04:15:00    9.5000000
# # 9  2016-08-22 18:40:00 2016-08-23 04:35:00    9.9166667
# # 10 2016-08-23 18:35:00 2016-08-24 04:50:00   10.2500000
# # 11 2016-08-24 18:35:00 2016-08-25 03:50:00    9.2500000
# # 12 2016-08-25 05:45:00 2016-08-25 06:00:00    0.2500000
# # 13 2016-08-25 19:15:00 2016-08-26 01:05:00    5.8333333
# # 14 2016-08-26 05:30:00 2016-08-26 06:00:00    0.5000000
# # 15 2016-08-26 06:25:00 2016-08-26 06:40:00    0.2500000
# # 16 2016-08-27 02:55:00 2016-08-27 03:25:00    0.5000000
# # 17 2016-08-27 05:30:00 2016-08-27 05:45:00    0.2500000
# # 18 2016-08-27 20:30:00 2016-08-27 22:00:00    1.5000000
# # 19 2016-08-28 20:15:00 2016-08-28 20:55:00    0.6666667
# # 20 2016-08-30 00:10:00 2016-08-30 04:00:00    3.8333333
# # 21 2016-08-31 20:50:00 2016-09-01 00:40:00    3.8333333
# # 22 2016-09-17 18:45:00 2016-09-18 01:30:00    6.7500000
# # 23 2016-09-18 02:25:00 2016-09-18 05:15:00    2.8333333
# # 24 2016-09-18 18:35:00 2016-09-18 20:00:00    1.4166667
# # 25 2016-09-20 00:25:00 2016-09-20 02:45:00    2.3333333
# # 26 2016-09-23 02:45:00 2016-09-23 03:15:00    0.5000000
# # 27 2016-09-23 18:40:00 2016-09-23 21:45:00    3.0833333
# # 28 2016-10-04 19:55:00 2016-10-05 02:20:00    6.4166667
# # 29 2016-10-05 23:45:00 2016-10-06 01:20:00    1.5833333
# # 30 2017-03-10 19:10:00 2017-03-11 06:00:00   10.8333333
# # 31 2017-03-11 19:00:00 2017-03-12 04:15:00    9.2500000
# # 32 2017-03-12 19:00:00 2017-03-13 06:10:00   11.1666667
# # 33 2017-03-13 19:05:00 2017-03-14 05:50:00   10.7500000
# # 34 2017-03-14 18:45:00 2017-03-15 02:20:00    7.5833333
# # 35 2017-03-16 20:20:00 2017-03-16 20:35:00    0.2500000
# # 36 2017-03-16 20:55:00 2017-03-16 21:15:00    0.3333333
# # 37 2017-03-24 19:00:00 2017-03-25 04:45:00    9.7500000
# # 38 2017-03-30 18:30:00 2017-03-31 04:15:00    9.7500000
# # 39 2017-03-31 18:25:00 2017-04-01 04:20:00    9.9166667
# # 40 2017-04-02 19:35:00 2017-04-02 21:30:00    1.9166667
# # 41 2017-04-03 01:10:00 2017-04-03 04:25:00    3.2500000
# # 42 2017-04-03 19:00:00 2017-04-04 02:15:00    7.2500000
# # 43 2017-04-04 05:15:00 2017-04-04 05:30:00    0.2500000
# # 44 2017-04-05 10:35:00 2017-04-05 10:50:00    0.2500000
# # 45 2017-04-06 05:15:00 2017-04-06 05:30:00    0.2500000
# # 46 2017-05-02 05:35:00 2017-05-02 05:50:00    0.2500000
# # 47 2017-05-23 08:55:00 2017-05-23 09:15:00    0.3333333
# # 48 2017-05-24 17:25:00 2017-05-24 17:40:00    0.2500000
# # 49 2017-05-25 06:35:00 2017-05-25 06:50:00    0.2500000
# # 50 2017-05-29 06:35:00 2017-05-29 06:50:00    0.2500000
# # 51 2017-05-31 05:20:00 2017-05-31 05:35:00    0.2500000
# # 52 2017-05-31 14:05:00 2017-05-31 14:25:00    0.3333333
# # 
# # $classification
# #    [1] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
# #   [43] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 2 2 2 2 2 2 2 2 2 2 4 2
# #   [85] 4 4 4 4 2 2 4 2 2 2 4 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 4 2 2 2
# #  [127] 4 2 2 2 4 2 4 2 2 2 2 2 2 4 2 2 2 2 2 4 2 2 2 2 2 2 2 2 2 4 4 4 2 4 2 4 4 2 4 4 4 4
# #  [169] 2 4 2 2 2 2 2 2 4 2 2 2 4 4 2 4 2 2 4 4 4 4 4 4 2 2 2 4 2 2 2 2 2 4 4 2 2 2 2 2 2 2
# #  [211] 2 2 2 2 4 1 2 2 2 4 2 2 2 4 2 2 2 2 4 2 4 2 4 2 4 2 2 2 4 4 4 2 4 4 4 4 2 4 4 1 4 4
# #  [253] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
# #  [295] 4 4 4 4 4 4 4 2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
# #  [337] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 2 2 2 2 4 4 4 4 4 4 1 4 4 4 4 4 4 4 4
# #  [379] 4 4 4 4 4 4 4 4 4 2 4 4 4 4 4 4 4 4 2 2 2 2 4 4 4 4 4 4 4 4 2 4 4 4 4 4 2 2 2 2 2 2
# #  [421] 2 2 2 4 4 2 2 4 4 4 2 2 4 2 2 4 4 2 2 2 2 2 2 2 4 2 2 4 4 4 4 4 2 2 4 4 2 2 2 2 2 2
# #  [463] 2 4 2 2 2 2 2 4 2 2 2 2 4 2 2 2 4 4 4 4 4 4 4 4 4 4 4 4 4 2 4 4 4 4 2 2 4 4 4 4 4 4
# #  [505] 4 2 2 1 2 2 2 2 4 2 2 4 4 2 2 4 4 2 4 4 4 4 4 2 4 2 2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
# #  [547] 4 4 4 2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
# #  [589] 4 4 4 4 4 4 4 4 4 4 4 4 2 4 4 4 2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
# #  [631] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 2 4 2 2 4 4 2 2 2 4 2 2 2 2 2 1 2 2 4
# #  [673] 2 4 2 2 2 2 2 2 2 2 4 4 2 4 4 4 2 2 4 4 2 2 2 4 2 2 2 4 4 4 4 4 4 2 2 4 2 2 2 2 2 2
# #  [715] 2 4 2 2 4 2 2 2 2 4 4 2 4 2 2 2 2 4 4 2 2 2 4 2 2 4 4 2 2 2 2 2 2 2 2 2 4 2 2 4 4 4
# #  [757] 2 2 4 2 2 4 2 2 2 4 2 2 2 2 2 2 2 2 2 2 4 2 2 4 4 2 2 2 4 2 2 2 4 2 4 4 4 4 4 4 4 4
# #  [799] 2 2 2 2 2 2 2 2 4 2 2 2 4 4 4 2 2 2 4 4 1 2 2 2 2 4 2 2 4 4 4 4 4 4 4 4 4 4 4 4 4 4
# #  [841] 4 4 4 2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
# #  [883] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
# #  [925] 4 4 4 4 4 4 4 4 4 4 4 4 4 2 2 2 2 2 2 2 4 4 2 4 2 2 4 4 4 4 2 4 2 2 2 2 2 2 4 4 4 4
# #  [967] 4 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 4 2 4 2 2 2 2 2 2 2
# #  [ reached getOption("max.print") -- omitted 87129 entries ]
# # 
# # $low_activity
# # [1] 2
# # 
# # $high_activity
# # [1] 1
# # 
# # $migration
# # [1] 3
# # 
# # $no_activity
# # [1] 4
# # 
# # $threshold
# # [1] 13.5
```
It is therefore then possible to use this information to plot the classification to see if it makes sense

```r
# plot the classification
col=col=c("brown","cyan4","gold","black")
plot(PAM_data$acceleration$date[2000:3400],PAM_data$acceleration$act[2000:3400],
  col=col[behaviour$classification][2000:3400], 
  type="o", pch=20, xlab="Date", ylab="Activity")
legend( PAM_data$acceleration$date[2000],50 , 
        c("No activity", "Low activity", "High activity", "Migration" ) ,
        col = col[c(behaviour$no_activity, behaviour$low_activity,
                    behaviour$high_activity, behaviour$migration)],
        pch=20)
```

<img align="center" src="https://raw.githubusercontent.com/KiranLDA/PAMLr/master/graphics/classification.png">

Interestingly we can see that the bird does these long migratory flights of > 10 hours, let's look to see, using the light data, whether these flights occur during night or day

```r
#plot the activity data with daylight periods in yellow and nightime periods in grey
plot(PAM_data$acceleration$date[2000:3400], PAM_data$acceleration$act[2000:3400],
        type="o", xlab="Date", ylab="Activity", pch=20, 
        cex= ifelse(behaviour$classification[2000:3400] == behaviour$migration,1.5,0.7),
        col=ifelse(PAM_data$light$obs[2000:3400]>0,"darkgoldenrod1","azure3"))
legend(PAM_data$acceleration$date[2000],45, 
        c("Daytime","Nightime") ,
        col = c("darkgoldenrod1","azure3"),
        pch=20)
```
<img align="center" src="https://raw.githubusercontent.com/KiranLDA/PAMLr/master/graphics/nightime_daytime.png">

### Classifying soar-gliding behaviour

Still under development


## Authors

Kiran Dhanjal-Adams

## Relevant references

Dhanjal-Adams, K.L., Bauer, S., Emmenegger, T., Hahn, S., Lisovski, S., & Liechti, F. (2018) [Spatiotemporal Group Dynamics in a Long-Distance Migratory Bird](https://www.cell.com/current-biology/fulltext/S0960-9822(18)30845-5?_returnURL=https%3A%2F%2Flinkinghub.elsevier.com%2Fretrieve%2Fpii%2FS0960982218308455%3Fshowall%3Dtrue). Current Biology, 28, 2824–2830.e3. 

Liechti, F., Bauer, S., Dhanjal-Adams, K.L., Emmenegger, T., Zehtindjiev, P., & Hahn, S. (2018) [Miniaturized multi-sensor loggers provide new insight into year-round flight behaviour of small trans-Sahara avian migrants](https://movementecologyjournal.biomedcentral.com/articles/10.1186/s40462-018-0137-1). Movement Ecology, 6, 19. 

## License

This project is licensed under the GNU General Public License version 3 - see the [LICENSE](https://github.com/KiranLDA/PAMLr/blob/master/LICENSE) file for details
