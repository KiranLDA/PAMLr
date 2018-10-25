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

#specify the data location
data(PAM_data)
str(PAM_data)
```
## Plot the raw light data as an interactive plot

<img align="left" src="https://raw.githubusercontent.com/KiranLDA/PAMLr/master/graphics/dygraphPAM.png">

Within `PAMLr` it is possible to create interactive `dygraphPAM()` plots which allow you to compare different measurements recorded by the logger. These might for instance include light, temperature, pressure, activity, pitch and magnetism. 

If you are working from Rstudio, this bit of code should be run:
```r
# In Rstudio, it will display in the viewer by default and use a lot of ram, and is better in html
backup_options <- options() 
options(viewer=NULL) # ensure it is viewed in internet browser
dygraphPAM(dta = PAM_data) # plot
options(backup_options) # restore previous viewer settings
```
If you are working from base R:
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
