# Package PAMLr

Pamela. This packages provides a set of functions to analyse data collected by SOI-GDL3pam loggers (developped by the Swiss Ornithological Institute www.vogelwarte.ch/en ). These measure atmospheric pressure (P), activity (A), magnetisim (M) and light (L) for analysis in R.

## Getting Started

These instructions will get the user a copy of the project up and running on the userr local machine for development and testing purposes.

### Prerequisites

To install this package from github, make sure the user first have `devtools` installed.

```r
install.packages("devtools")
```

The install SGAT from github using `devtools::install_github("SWotherspoon/SGAT")` followed by TwGeos using `devtools::install_github("SLisovski/TwGeos")`.

Other packages used by PAMLr are `dplyr`,`dygraphs`,`graphics`,`grDevices`,`htmltools`,`stats`,`tcltk`,`utils`,`xts`,`zoo`, `depmixS4`. If there are any issues with installing PAMLr, please ensure these packages are installed and working.

### Installing


The github package can be installed:

```r
devtools::install_github("KiranLDA/PAMLr")

```

## Load and test

To make sure the package works run the following

```
# load library
library(PAMLr)
```

```r
# get an example of PAM data
data(hoopoe)
PAM_data = hoopoe

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

For a quick look at the data, it's possible to use `quickPLOT`. The user can specify which arguments to use, using `measurements`. There's a choice between different combinations of `"pressure"`, `"light"`, `"acceleration"`, `"temperature"` and `"magnetic"`.

```r
par(mar=c(3,4,0.5,0.5))
quickPLOT(hoopoe, col="cornflowerblue",
          measurements = c("pressure", "light", "acceleration")
)
```

<img align="center" src="https://raw.githubusercontent.com/KiranLDA/PAMLr/master/graphics/quickPLOT.png">

However, this graphic is quite noisy. To have a better overview of the data, it is possible to create interactive `dygraphPAM()` plots which allow the user to compare different measurements recorded by the logger. These might for instance include light, temperature, pressure, activity, pitch and magnetism. 

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
The reason there is additional code for Rstudio, is that by default it will open this graphic in the viewer pane and use up a lot of ram. This  additional code allows the user to open this window in a browser instead of r studio, and the file can later be saved as an html file.

<img align="center" src="https://raw.githubusercontent.com/KiranLDA/PAMLr/master/graphics/dygraphPAM.png">

With this interactive plot, the user can then zoom in and out of different plots to help get a feel for the data. For instance, this is a great way of seeing changes in the data which might be due to a logger being in a rucksack and no longer on the birds, or to look at how acticity or pressure might look during migration periods.

It is possible to select areas to zoom into by right clicking and highighting certain regions, and to double click to zoom out. All plots are synched to the same time period and have a timeline at the bottom to increase or decrease the time over which the data is observed.


## Removing unwanted data

As mentionned above, logger data often come with periods when a logger is not on the bird. To remove these periods there is a function called `curPAM`.

```r
start = as.POSIXct("2016-07-01","%Y-%m-%d", tz="UTC")
end = as.POSIXct("2017-06-01","%Y-%m-%d", tz="UTC")

PAM_data = cutPAM(PAM_data,start,end)
```

`hoopoe` has now been overwritten by the cropped (by date) dataset. Beware of over-writing until you are happy with the cropping dates. This could, for example, be done using `quickPLOT(cutPAM(PAM_data,start,end), measurements = "pressure")`


# Actinogram

An actinogram (activity over time) can tell us a lot about a bird's behaviour, for instance whether it is a nocturnal or a diurnal migrant. `PAMLr` allows the user to plot one of these using `plotACTOGRAM`

```r
par(mar=c(4,4,1,6))
plotACTOGRAM(date = PAM_data$acceleration$date, activity = PAM_data$acceleration$act)
```
<img align="center" src="https://raw.githubusercontent.com/KiranLDA/PAMLr/master/graphics/daymiddle.png">


It is also possible to have night in the middle and then to plot sunrise and sunsets onto this, to see how activity changes as a function of sunlight.

```r
#estimate sunrises and sunsets
twilights <- GeoLight::twilightCalc(PAM_data$light$date, PAM_data$light$obs, 
                                    LightThreshold = 2, ask = F)

#plot
par(mar=c(4,4,1,6))
offset=12
plotACTOGRAM(date = PAM_data$acceleration$date,activity = PAM_data$acceleration$act, offset=offset,col=c("black",viridis::cividis(90)))
addTWL(twilights$tFirst, offset=offset, 
col= ifelse(twilights$type == 1,  "goldenrod","cornflowerblue"), 
pch=16, cex=0.5)
```
<img align="center" src="https://raw.githubusercontent.com/KiranLDA/PAMLr/master/graphics/nightmiddle.png">

# Classifying bird migration

Before starting to classify bird migration from PAM data, it's important to consider what type of flight the bird has. Does is flap, soar, soar-flap or soar-glide for isntance? These will have a large bearing on how behaviour is classified. The easiest type of flight to classify is a  continuously flapping bird, such as a kingfisher, a hoopoe or a shrike.

### Classifying flapping behaviour

Continuously flapping birds have higher activity than soaring birds (see the above actinogram). You can therefore use the `classifyFLAP()` function to classify flapping behaviour. This function assumes that if the bird has displayed high activity for x number of minutes, then it is flapping. It is therefore important to think about what constitutes high activity and how long this period should be. At the moment, the function uses k-means clustering to identify the threshold between high and low activity. Using `toPLOT = TRUE` then allows the user to see where that threshold was drawn. The period of high activity is set by default to `period = 3`. This is because activity is recorded (on this logger) every 5 minutes and we assume that after 15 minutes of high activity, the bird must be flapping. Thus "high activity duration" / "data resolution" = "period" and 15 minutes / 5 minutes = period of 3.


```r
behaviour = classifyFLAP(dta = PAM_data$acceleration, period = 3)
```
<img align="center" src="https://raw.githubusercontent.com/KiranLDA/PAMLr/master/graphics/classification_thershold.png">

This will give the user various information. It will tell the user how the data were classidied in `type`. It will also give the user a `timetable` of when the bird was flying, with start and end times, and the duration of this flight in hours. This can be manipulated as needed. It then also gives the user the `classification` of each time point and specified what each classification stands for, which value represents `low_activity`, `high_activity`, `migration` or `no_activity`. And finally what the `threshold` was between high and low activity.

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
index= 6500:8000
plot(PAM_data$acceleration$date[index],PAM_data$acceleration$act[index],
  bg=col[behaviour$classification][index], 
  type="o", pch=21, xlab="Date", ylab="Activity")
legend( PAM_data$acceleration$date[index[1]],56 , 
        c("No activity", "Low activity", "High activity", "Migration" ) ,
        col = col[c(behaviour$no_movement, behaviour$low_movement,
                    behaviour$high_movement, behaviour$migration)],
        pch=20)
```

<img align="center" src="https://raw.githubusercontent.com/KiranLDA/PAMLr/master/graphics/classification.png">

Interestingly we can see that the bird does these long migratory flights of > 10 hours, let's look to see, using the light data, whether these flights occur during night or day

```r
#plot the activity data with daylight periods in yellow and nightime periods in grey

index= 6500:8000
plot(PAM_data$acceleration$date[index], PAM_data$acceleration$act[index],
        type="o", xlab="Date", ylab="Activity", pch=20, 
        cex= ifelse(behaviour$classification[index] == behaviour$migration,1.5,0.7),
        col=ifelse(PAM_data$light$obs[index]>0,"darkgoldenrod1","azure3"))
legend(PAM_data$acceleration$date[index[1]],55, 
        c("Daytime","Nightime") ,
        col = c("darkgoldenrod1","azure3"),
        pch=20)
```
<img align="center" src="https://raw.githubusercontent.com/KiranLDA/PAMLr/master/graphics/nightime_daytime.png">

# Classifying soar-gliding behaviour

## Using Light

This classification assumes that when soar-gliding, a bird is in continuous light. The rest of the time, when light is less constant, the bird may be resting or it may be nighttime. This classification therefore assumes that (i) bird is soaring and therefore migrating during the day, (ii) that the logger is sensitive enough to distinguish between constant and intermittent light (not all loggers are likely to be).

### Defining the light threshold

Anyone performing geolocator analyses will be familiar with this step.

First we start by plotting a short section of light over time. This is used to define the light threshold `threshold` between night and day (plotted in red). Note how in some areas, the light is noisy, and in others it is very flat. The function is designed to identify these flat periods. 

```r
# import the example dataset
data(bee_eater)

start = as.POSIXct("2015-07-30","%Y-%m-%d", tz="UTC")
end = as.POSIXct("2016-07-01","%Y-%m-%d", tz="UTC")
PAM_data = cutPAM(bee_eater, start, end)


# plot
par(mfrow=c(1,1))

index=which(PAM_data$light$date >= "2015-08-30" &
            PAM_data$light$date <= "2015-09-03")
plot(PAM_data$light$date[index],
     PAM_data$light$obs[index],
     type="l",
     ylab="Light")


threshold= 100
abline(h=threshold, col="brown")
```
<img align="center" src="https://raw.githubusercontent.com/KiranLDA/PAMLr/master/graphics/light_threshold.png">

### The light classification

Then we use the `classifyLIGHT` function to separate the data into "no light", "a little light", "high light", "continuous light" (i.e. soaring) and "long periods of continuous light" (i.e. migration). The function starts by taking the light data, and finding all the "no light" periods as those below the threshold, everything else is "a little light", then it finds "high light" and  finds "continuous light" within this "high light".

The function then takes the continuous light and divides them based on how long they last. The user can manually define a threshold using `method = "manual"` and `duration_threshold` which is set in hours. Or it is possible to use kmeans clustering with `method = "kmeans"`. When k-means is used, then duration_threshold is overwritten by the clustering threshold.

```r
# Classify using light
classification <- classifyLIGHT(PAM_data$light, 
                                method="kmeans",
                                keep_one_off_missclassifications = T,
                                duration_threshold = 6)$classification
classification= as.data.frame(classification)
classification = cbind(PAM_data$light$date, classification)
colnames(classification) = c("date","classification")

# create a dataset to plot the light classification on pressure
toplot= merge(PAM_data$pressure,classification)

# Plot
col=c("grey","goldenrod","olivedrab","royalblue", "orange")
par(mfrow=c(2,1))
plot(toplot$date, toplot$obs, 
     type="o", 
     bg=col[toplot$classification +1], 
     col="black",
     ylab="Pressure (hPa)",
     cex=ifelse(toplot$classification %in% c(3,4),1,0),
     pch=21)
id= 1300:3000
plot(toplot$date[id], toplot$obs[id], 
     type="o", 
     bg=col[toplot$classification[id] +1], 
     col="black",
     ylab="Pressure (hPa)",
     cex=ifelse(toplot$classification[id] %in% c(3,4),1,0),
     pch=21)

```

In the below plot, blue represents continuous light (i.e. soaring), while yellow represents extended continuous light (i.e. migration).

<img align="center" src="https://raw.githubusercontent.com/KiranLDA/PAMLr/master/graphics/classifyLIGHT.png">

## Using Activity

### Plot activity

Let's start by looking at the actinograms of both hoopoes (which flap) and bee-eaters, which soar-glide.

```r
# start by differentiating the two birds and cropping the data

# The flapping bird
data("hoopoe")
start = as.POSIXct("2016-07-01","%Y-%m-%d", tz="UTC")
end = as.POSIXct("2017-06-01","%Y-%m-%d", tz="UTC")
hoopoe = cutPAM(hoopoe,start,end)

# The  soar-gliding bird
data("bee_eater")
start = as.POSIXct("2015-07-30","%Y-%m-%d", tz="UTC")
end = as.POSIXct("2016-07-01","%Y-%m-%d", tz="UTC")
bee_eater = cutPAM(bee_eater, start, end)

# Estimate sunrises and sunsets
hoopoe_twl <- GeoLight::twilightCalc(hoopoe$light$date, hoopoe$light$obs, 
                                    LightThreshold = 2, ask = F)
bee_eater_twl <- GeoLight::twilightCalc(bee_eater$light$date, bee_eater$light$obs, 
                                    LightThreshold = 2, ask = F)

# plot the two side by site
par(mfrow=c(1,2), mar=c(4,4,1,6))

# center plot around midday
offset = 0

# hoopoe
plotACTOGRAM(date = hoopoe$acceleration$date,
             activity = hoopoe$acceleration$act,
             offset=offset,
             col=c("black",viridis::cividis(90)))

addTWL(hoopoe_twl$tFirst, offset=offset, 
       col= ifelse(hoopoe_twl$type == 1,  
                   "goldenrod","cornflowerblue"),
       pch=16, cex=0.5)
       
# bee_eater
plotACTOGRAM(date = bee_eater$acceleration$date,
             activity = bee_eater$acceleration$act,
             offset=offset,
             col=c("black",viridis::cividis(90)))

addTWL(bee_eater_twl$tFirst, offset=offset, 
       col= ifelse(bee_eater_twl$type == 1,  
                   "goldenrod","cornflowerblue"),
       pch=16, cex=0.5)
       
```
<img align="center" src="https://raw.githubusercontent.com/KiranLDA/PAMLr/master/graphics/hoopoe_bee_comp.png">

Note that the first big difference is that hoopes (on the left) are active at night, while bee-eaters (on the right) are not. However, with bee-eaters, though the activity is does not peak as must during migration, it is instead constantly low.

### The activity classification

Then we use the `sustainedACT` function to separate the data into "no activity", "activity", "sustained activity", (i.e. soaring) and "endurance activity" (i.e. migration). The function starts by taking the activity data, and finding all the "no activity" periods as those below the `act_threshold`, everything else is "activity", then it finds "finds "continuous activity" within this "activity".

The function then takes the continuous activity and divides them based on how long they last. The user can manually define a threshold using `method = "manual"` and `duration_threshold` which is set in hours. Or it is possible to use kmeans clustering with `method = "kmeans"`. When k-means is used, then duration_threshold is overwritten by the clustering threshold.

```r
# Classify using activity
classification <- sustainedACT(bee_eater$acceleration, 
                                method="manual",
                                keep_one_off_missclassifications = TRUE,
                                duration_threshold = 2)$classification
classification= as.data.frame(classification)
classification = cbind(bee_eater$light$date, classification)
colnames(classification) = c("date","classification")

# create a dataset to plot the light classification on pressure
toplot= merge(bee_eater$pressure,classification)


# Plot
col=c("goldenrod","olivedrab","royalblue", "orange")
par(mfrow=c(2,1), mar=c(3,4,0.5,0.5))
plot(toplot$date, toplot$obs, 
     type="o", 
     bg=col[toplot$classification +1], 
     col="black",
     ylab="Pressure (hPa)",
     cex=ifelse(toplot$classification %in% c(2,3),1,0),
     pch=21)
id= 1300:3000
plot(toplot$date[id], toplot$obs[id], 
     type="o", 
     bg=col[toplot$classification[id] +1], 
     col="black",
     ylab="Pressure (hPa)",
     cex=ifelse(toplot$classification[id] %in% c(2,3),1,0),
     pch=21)

```
<img align="center" src="https://raw.githubusercontent.com/KiranLDA/PAMLr/master/graphics/sustainedACT.png">



## Using Atmospheric Pressure and hidden markov models



Still under development.

Soar-gliding birds do not display the same kind of sustained high activity as flapping birds. However, because soar-gliding is associated with large changes in altitude, we can use pressure change as a proxy for altitude change to classify this type of flight. For instance, we know that a pressure change of more than 3 hPa within a 30 minute interval is likely due to flight, not weather. There is therefore a function `SOARprep` which finds every single one of these flight events and then summarises what happened during that flight event. Summary information includes, pressure the night before and the night after this flight event, how much pressure changed during this flight event, how active the bird was during this flight event, how long the flight event lasted, etc...

The idea is to then use this information about the flight event to classify migration periods using a hidden markov model (or k-means clustering, but the hmm seems to work better). This is where it gets complicated as different birds have different behaviours. For instance, some birds can be better at thermalling and therefore flap less, thus activity during flight will not be good at classifying behaviour. Other birds may be less good at thermalling, and therefore flap a great deal during migration, in which case activity is very useful for identifying this behaviour.

Overall, the daily duration of flight time seems good for classifying migration flight events, as is the total pressure changes throughout the day, and the difference in pressure from the night before and the night after. Indeed, overnighting in a different altitude/pressure zone is likely to mean the bird is overnighting in a different place and was therefore on the move during the day.

Note that soar-gliders migrate during the day.

```r
# The  soar-gliding bird
data("bee_eater")
start = as.POSIXct("2015-07-30","%Y-%m-%d", tz="UTC")
end = as.POSIXct("2016-07-01","%Y-%m-%d", tz="UTC")
bee_eater = cutPAM(bee_eater, start, end)

# find sunrise and sunset events
twl = GeoLight::twilightCalc(bee_eater$light$date, bee_eater$light$obs,
LightThreshold = 2, ask = F)

# specify which variables are available on the PAM logger, sometimes one is not recorded or might have broken
availavariable = c("pressure", "light", "acceleration")

# create a dataset of flight events, with information about each flight event
TOclassify = SOARprep(dta = bee_eater, availavariable = availavariable, twl = twl)

#classify each flight event into multiple states
classification = classifyPAM(TOclassify$night_P_diff
                              * TOclassify$total_daily_duration
                              * TOclassify$total_daily_P_change
                              * TOclassify$sum_activity, states=2, "hmm")$cluster
                              
# add this classification
pressure_classification = classification2PAM(from = TOclassify$start,
                                              to =TOclassify$end,
                                              classification = classification,
                                              addTO = bee_eater$pressure)
```


Plot the data

```r
plot(bee_eater$pressure$date, bee_eater$pressure$obs, 
      bg= viridis::viridis(max(classification)+1)[pressure_classification+1], 
      col="black",  type="o", pch=21,
      xlab="Date", ylab="Pressure (hPa)")
      
```

<img align="center" src="https://raw.githubusercontent.com/KiranLDA/PAMLr/master/graphics/pressure_classification.png">


# Classify Soar-flapping (swift)


```r
data(swift)
PAM_data = swift

# have a quick look at the data
quickPLOT(PAM_data, measurements=c("light","pressure","acceleration"))

# from this, we can tell thatlight stopped recording and that the timeseries needs to be cut

# crop the data to get rid of no good periods
start = as.POSIXct("2016-09-01","%Y-%m-%d", tz="UTC")
end = as.POSIXct("2017-04-21","%Y-%m-%d", tz="UTC")
PAM_data = cutPAM(PAM_data, start, end)

# backup_options <- options()
# options(viewer=NULL) # ensure it is viewed in internet browser
# dygraphPAM(dta = PAM_data) # plot
# options(backup_options) # restore previous viewer settings

# make sure it looks ok
quickPLOT(PAM_data, measurements=c("light","pressure","acceleration"))

# derive a whole bunch of measures which can be used to classifc the data later

twl = GeoLight::twilightCalc(PAM_data$light$date, PAM_data$light$obs, LightThreshold = 2, ask = F)

TOclassify = SOARprep(dta = PAM_data,
                      availavariable = c("pressure", "acceleration","light"),
                      twl=twl, diff_P=15)
str(TOclassify)
TOclassify = TOclassify[complete.cases(TOclassify),]



test = classifySWIFT(addTO = PAM_data$pressure,
                     dta = TOclassify,
                     method = "hmm", # or kmeans
                     states = 3,
                     availavariable = c("light", "pressure", "acceleration"))

plot(PAM_data$pressure$date,PAM_data$pressure$obs,
     col=viridis::viridis(max(test$classification)+1)[test$classification+1],
     type="o",
     pch=16, cex=ifelse(test$classification == test$migration, 0.6, 0) )

```


## Calculate altitude

Pressure can give a general estimate of what altitude a bird is flying or stopping at. The function `altitudeCALC` will estimate this for the user based on a standard formula. The parameters for this formula are easily adjusted, see `?altitudeCALC` for more details.


```r
data(hoopoe)
PAM_data = hoopoe
altitude = altitudeCALC(P = PAM_data$pressure$obs)
plot(PAM_data$pressure$date[2:8000], altitude[2:8000], type="o",pch=16, xlab="Date", ylab="Altitude (m)")
```
<img align="center" src="https://raw.githubusercontent.com/KiranLDA/PAMLr/master/graphics/Altitude.png">


```r
data(bee_eater)
PAM_data = bee_eater
altitude = altitudeCALC(P = PAM_data$pressure$obs[PAM_data$pressure$date %in% PAM_data$temperature$date], 
                        T0 = PAM_data$temperature$obs[PAM_data$temperature$date %in% PAM_data$pressure$date] +273.15)
plot(PAM_data$pressure$date[2:8000], altitude[2:8000], type="o",pch=16, xlab="Date", ylab="Altitude (m)")
```

## Authors

Kiran Dhanjal-Adams

## Relevant references

Dhanjal-Adams, K.L., Bauer, S., Emmenegger, T., Hahn, S., Lisovski, S., & Liechti, F. (2018) [Spatiotemporal Group Dynamics in a Long-Distance Migratory Bird](https://www.cell.com/current-biology/fulltext/S0960-9822(18)30845-5?_returnURL=https%3A%2F%2Flinkinghub.elsevier.com%2Fretrieve%2Fpii%2FS0960982218308455%3Fshowall%3Dtrue). Current Biology, 28, 2824–2830.e3. 

Liechti, F., Bauer, S., Dhanjal-Adams, K.L., Emmenegger, T., Zehtindjiev, P., & Hahn, S. (2018) [Miniaturized multi-sensor loggers provide new insight into year-round flight behaviour of small trans-Sahara avian migrants](https://movementecologyjournal.biomedcentral.com/articles/10.1186/s40462-018-0137-1). Movement Ecology, 6, 19. 

## License

This project is licensed under the GNU General Public License version 3 - see the [LICENSE](https://github.com/KiranLDA/PAMLr/blob/master/LICENSE) file for details
