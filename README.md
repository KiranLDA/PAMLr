# Package pamlr

This package combines a set of functions for analysing multisensor geolocator data such as Pressure, Activity, Magnetism, Temperature or Light. This includes functions for importing and visualising any or all of these sensor data, formatting the data for analysis (with some functions already setup for birds), deriving classifications (using cluster analysis or hidden markove models), and finally for comparing classification accuracy between different models.

## Access the package Manual 

The manual is the best way to get started with the package, and can be found at the following link:
[https://kiranlda.github.io/PAMLrManual/index.html](https://kiranlda.github.io/PAMLrManual/index.html)


## Installation

### Prerequisites

To install this package from github, make sure the user first have `devtools` installed.

```r
install.packages("devtools")
```
Then the package can be installed from github:

```r
devtools::install_github("KiranLDA/pamlr")
```
## Import data

Data can be imported as follows:

```r
PAM_data = create_import(pathname = "C:/Put/your/path/here",
                     measurements = c(".pressure", 
                                      ".glf",
                                      ".acceleration", 
                                      ".temperature", 
                                      ".magnetic")`
```

An example dataset is 

```r
data(hoopoe)
```

## Cropping the data

Note that very often, a logger continues to record data before and after it is removed from a bird. For example, it might be transported in a rucksack or left in a laboratory until data are downloaded. It's therefore important to remove these incorrect datapoints. This can be done using `create_crop`.

```r
# make sure the cropping period is in the correct date format
start = as.POSIXct("2016-07-01","%Y-%m-%d", tz="UTC")
end = as.POSIXct("2017-06-01","%Y-%m-%d", tz="UTC")

# Crop the data
PAM_data= create_crop(hoopoe,start,end)
```

## Visualising data

### Quick multiplots

For a quick look at the data, it's possible to use `plot_timeseries`. The user can specify which arguments to use, using `measurements`. There's a choice between different combinations of `"pressure"`, `"light"`, `"acceleration"`, `"temperature"` and `"magnetic"`. You can add any parameters from `?plot`, here I illustrate it with `col="cornflowerblue"` and by showing how to  restrict the x-axis limits `xlim` with the date format, to zoom into the post breeding mihration period of a hoopoe

```r
par(mar=c(3,4,0.5,0.5))
plot_timeseries(hoopoe, col="cornflowerblue",
                measurements = c("pressure", "light", "acceleration"),
                xlim=c(as.POSIXct("2016-08-20","%Y-%m-%d", tz="UTC"),
                        as.POSIXct("2016-09-01","%Y-%m-%d", tz="UTC")))
```
<img align="center" src="https://github.com/KiranLDA/PAMLrManual/blob/master/_bookdown_files/PAMLrManual_files/figure-html/unnamed-chunk-14-1.png">


### Interactive timeseries

To have a better overview of the data, it is possible to create interactive `plot_interactive_timeseries()` plots which allow the user to compare different measurements recorded by the logger. These might for instance include light, temperature, pressure, activity, pitch and magnetism. 

If you are **working from Rstudio**, this bit of code should be run:

```r
# In Rstudio, it will display in the viewer by default and use a lot of ram, and is better in html
backup_options <- options() 
options(viewer=NULL) # ensure it is viewed in internet browser
plot_interactive_timeseries(dta = PAM_data) # plot
options(backup_options) # restore previous viewer settings
```

If you are **working from base R** use this instead:

To save space here we only plot only one variable - pressure .

```r
plot_interactive_timeseries(dta = PAM_data, toPLOT = c("pressure")) 
```

The reason there is additional code for Rstudio, is that by default it will open this graphic in the viewer pane and use up a lot of ram. This  additional code allows the user to open this window in a browser instead of r studio, and the file can later be saved as an html file.

With this interactive plot, the user can then zoom in and out of different plots to help get a feel for the data. For instance, this is a great way of seeing changes in the data which might be due to a logger being in a rucksack and no longer on the birds, or to look at how acticity or pressure might look during migration periods.

It is possible to select areas to zoom into by right clicking and highighting certain regions, and to double click to zoom out. All plots are synched to the same time period and have a timeline at the bottom to increase or decrease the time over which the data is observed.

### Sensor images

Actograms are often used to plot activity over time at different hours of the day. However, the same approach can be used to plot any sensor data, not just activity. For simplicity, we name these “sensor images”. Plotting all sensors side by side is an important step for visualising data and developing an understanding of data patterns, and to start thinking about the behaviours that may be driving the observed patterns. __PAMLr__ offers a function `plot_sensorimage()`for plotting sensor images, which can be implemented as follows.

```r
# Create plots with 3 together (mfrow)
par( mfrow= c(1,3), oma=c(0,2,0,6))

par(mar =  c(4,2,4,2))
plot_sensorimage(PAM_data$acceleration$date, ploty=FALSE,
          PAM_data$acceleration$act, main = "Activity",
          col=c("black",viridis::cividis(90)), cex=1.2, cex.main = 2)

par(mar =  c(4,2,4,2))
plot_sensorimage(PAM_data$pressure$date, plotx=TRUE, ploty=FALSE, labely=FALSE,
          PAM_data$pressure$obs,  main="Pressure",
          col=c("black",viridis::cividis(90)), cex=1.2, cex.main = 2)

par(mar =  c(4,2,4,2))
plot_sensorimage(PAM_data$temperature$date, labely=FALSE,
          PAM_data$temperature$obs,  main="Temperature",
          col=c("black",viridis::cividis(90)), cex=1.2, cex.main = 2)


```
<img align="center" src="https://github.com/KiranLDA/PAMLrManual/blob/master/_bookdown_files/PAMLrManual_files/figure-html/unnamed-chunk-17-1.png">

### Histograms and three-dimensional scatterplots

Histograms can provide a first impression of whether some of the data may be aggregated and therefore clustered. Indeed, sensor images may not always well-suited for visualising tri-axial data, such as magnetic field or acceleration. By plotting data in three dimensions (hereafter “3D”) using the function `plot_interactive_3d` it’s possible to find patterns or clusters of datapoints which would not otherwise be apparent in the data. Here we plot magnetic data. 

```r
plot_interactive_3d(PAM_data$magnetic$gX, PAM_data$magnetic$gY, PAM_data$magnetic$gZ,
       xlab= "X", ylab= "Y", zlab= "Z",
       xlim=c(-3000,3000), ylim=c(-3000,3000), zlim=c(-3000,3000))
```

### Spherical projections

#### g-sphere

A _g-sphere_ is a method of visualising tri-axial *acceleration* data. This involves centering the data and plotting it on a sphere.The function `calculate_triaxial_acceleration` allows the user to center this data (as well as calculating  pitch, roll and yaw from the data)

```r
# plot an g-phere
calibration = calculate_triaxial_acceleration(dta = PAM_data$magnetic)
plot_interactive_sphere(x = calibration$centered_accx,
          y = calibration$centered_accy,
          z = calibration$centered_accz,
          ptcol = "royalblue",
          ptsize = 0.03,
          linecolor ="orange",
          spherecolor="orange",
          arrows=TRUE)


```

#### m-sphere

An _m-sphere_ is a method of visualising tri-axial *magnetometer* data. This involves centering the data and correcting the data, before  plotting it on a sphere.The function `calculate_triaxial_magnetic` calibrates the data. This provides the animal's bearing.

```r
# plot a m-phere
calibration = calculate_triaxial_magnetic(dta = PAM_data$magnetic)
plot_interactive_sphere(x = calibration$calib_magx,
          y = calibration$calib_magy,
          z = calibration$calib_magz,
          ptcol = "orange",
          ptsize = 0.03,
          linecolor ="royalblue",
          spherecolor="royalblue",
          arrows=TRUE,
          cex=2)
```


## Classifying migratory flapping flight in Hoopoes  (an example classification)

## Performing the classification

Because flapping is widespread in birds, **PAMLr** integrates a pre-defined function `classify_flap()` to classify this behaviour.

This function assumes that if the bird has displayed **high activity** for x number of consecutive minutes, then it is flapping. It is therefore important to think about what constitutes high activity and how long this period should be. At the moment, the function uses **k-means clustering** to identify the threshold between high and low activity. Using `toPLOT = TRUE` then allows the user to see where that threshold was drawn. The period of high activity is set by default to `period = 3`. This is because activity is recorded (on this logger) every 5 minutes and we assume that after an hour of high activity, the bird must be flapping. 

Thus "high activity duration" / "data resolution" = "period" and 60 minutes / 5 minutes = period of 12.

```r
# Classify behaviour
behaviour = classify_flap(dta = PAM_data$acceleration, period = 12)
str(behaviour)
```
<img align="center" src="https://github.com/KiranLDA/PAMLrManual/blob/master/_bookdown_files/PAMLrManual_files/figure-html/unnamed-chunk-50-1.png">


This classification therefore provides different pieces of infomration.

* **timetable** shows when a migratory flapping flight started and stopped, and how long it lasted (in hours)
* **classification** is the output from the classification. In this case, each cluster/classs/state is represented by numbers between one 1 and 4. To find out what behaviour each of these numbers represent, we can refer to **low_movement**, **high_movement**, **migration** and **no_movement** 
* **threshold** represents the threshold between high and low activity.

Using these information, it's therefore possible to plot the classification:


```r
#Plot behaviour
col=col=c("black","royalblue4","brown","gold")
index= 7300:8000
plot(PAM_data$acceleration$date[index],PAM_data$acceleration$act[index],
  type="l", xlab="Date", ylab="Activity")
points(PAM_data$acceleration$date[index],PAM_data$acceleration$act[index],
  col=col[behaviour$classification+1][index], 
  pch=16,)
legend( PAM_data$acceleration$date[index[1]],60 , 
        c("No activity", "Low activity", "High activity", "Migration" ) ,
        col = col[c(behaviour$no_movement, behaviour$low_movement,
                    behaviour$high_movement, behaviour$migration)+1],
        pch=20)
```
<img align="center" src="https://github.com/KiranLDA/PAMLrManual/blob/master/docs/08-flapping_files/figure-html/unnamed-chunk-5-1.png">

## Plot the classification as a sensor image

Another way of looking at a classification is to use a sensor image of the results and to plot it side by side with the raw data to see if the same patterns are being picked out. We can also add (for instance sunset and sunrise events)

```r
par(mfrow= c(1,3), # number of panels
    oma=c(0,2,0,6), # outer margin around all panels
    mar =  c(4,1,4,1)) # inner margin around individual fivure

plot_sensorimage(PAM_data$acceleration$date, ploty=FALSE,
          PAM_data$acceleration$act, main = "Activity",
          col=c("black",viridis::cividis(90)), cex=1.2, cex.main = 2)
legend("bottomright",cex=1.2,
   c("No Activity", "Low Activity", "High Activity" ) , fill = c("black","royalblue3", "orange"), xpd = NA)


plot_sensorimage(PAM_data$pressure$date, plotx=TRUE, ploty=FALSE, labely=FALSE,
          PAM_data$pressure$obs,  main="Pressure",
          col=c("black",viridis::cividis(90)), cex=1.2, cex.main = 2)
legend("bottomright",cex=1.2,
   c("Low Pressure", "High Pressure" ) , fill = c("royalblue3", "orange"), xpd = NA)


plot_sensorimage(PAM_data$acceleration$date, labely=FALSE,
          behaviour$classification, 
          main="Classification",
          col=col,
          cex=1.2, cex.main = 2)
legend("bottomright",cex=1.2,
  # grconvertX(1, "device"), grconvertY(1, "device"),
   c("Resting", "Active", "Flapping", "Migrating" ) , fill = col, xpd = NA)


```

<img align="center" src="https://github.com/KiranLDA/PAMLrManual/blob/master/docs/08-flapping_files/figure-html/unnamed-chunk-6-1.png">



## Authors

Kiran Dhanjal-Adams

## Relevant references

Dhanjal-Adams, K.L., Bauer, S., Emmenegger, T., Hahn, S., Lisovski, S., & Liechti, F. (2018) [Spatiotemporal Group Dynamics in a Long-Distance Migratory Bird](https://www.cell.com/current-biology/fulltext/S0960-9822(18)30845-5?_returnURL=https%3A%2F%2Flinkinghub.elsevier.com%2Fretrieve%2Fpii%2FS0960982218308455%3Fshowall%3Dtrue). Current Biology, 28, 2824–2830.e3. 

Liechti, F., Bauer, S., Dhanjal-Adams, K.L., Emmenegger, T., Zehtindjiev, P., & Hahn, S. (2018) [Miniaturized multi-sensor loggers provide new insight into year-round flight behaviour of small trans-Sahara avian migrants](https://movementecologyjournal.biomedcentral.com/articles/10.1186/s40462-018-0137-1). Movement Ecology, 6, 19. 

## License

This project is licensed under the GNU General Public License version 3 - see the [LICENSE](https://github.com/KiranLDA/PAMLr/blob/master/LICENSE) file for details
