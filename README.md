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
This is what it should look like

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

To import your own data use `importPAM("C:/Put/your/path/here")`


## Plot the raw light data as an interactive plot


<img align="center" src="https://raw.githubusercontent.com/KiranLDA/PAMLr/master/graphics/dygraphPAM.png">

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
PAM_data$pressure = AM_data$pressure[(PAM_data$pressure$date >= "2016-07-30" & PAM_data$pressure$date <= "2017-06-01"),]

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
behaviour
```

```r
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
![classification](https://raw.githubusercontent.com/KiranLDA/PAMLr/master/graphics/classification.png)


```r
#plot the activity data with daylight periods in yellow and nightime periods in grey
plot(PAM_data$acceleration$date[6000:9000], PAM_data$acceleration$act[6000:9000],
        type="o", xlab="Date", ylab="Light Intensity", pch=20,
        col=ifelse(PAM_data$light$obs[6000:9000]>0,"darkgoldenrod1","azure3"))
```

![activity during night and day](https://raw.githubusercontent.com/KiranLDA/PAMLr/master/graphics/nightime_daytime.png)

## Classify migration periods from PAM-loggers


## Authors

Kiran Dhanjal-Adams

## Relevant references

Dhanjal-Adams, K.L., Bauer, S., Emmenegger, T., Hahn, S., Lisovski, S., & Liechti, F. (2018) [Spatiotemporal Group Dynamics in a Long-Distance Migratory Bird](https://www.cell.com/current-biology/fulltext/S0960-9822(18)30845-5?_returnURL=https%3A%2F%2Flinkinghub.elsevier.com%2Fretrieve%2Fpii%2FS0960982218308455%3Fshowall%3Dtrue). Current Biology, 28, 2824–2830.e3. 

Liechti, F., Bauer, S., Dhanjal-Adams, K.L., Emmenegger, T., Zehtindjiev, P., & Hahn, S. (2018) [Miniaturized multi-sensor loggers provide new insight into year-round flight behaviour of small trans-Sahara avian migrants](https://movementecologyjournal.biomedcentral.com/articles/10.1186/s40462-018-0137-1). Movement Ecology, 6, 19. 

## License

This project is licensed under the GNU General Public License version 3 - see the [LICENSE](https://github.com/KiranLDA/PAMLr/blob/master/LICENSE) file for details
