#' Plot PAM data as an interactive timeseries
#'
#' @description This opens a java dygraph application which allows the user to zoom in and out. In Rstudio it will open in the viewer pane and in base R in an html readers. Note that this can be a bit slow
#'
#' @param to_classify rolling window data to plot
#' @param classification_datetime  dates associated with classification
#' @param classification output of the classification
#' @param to_plot names of the variables to plot. For now this includes `light`, `pressure`, `acceleration` and `temperature`
#'
#' @return a plot of all the measurements
#'
#' @references Vanderkam, D., Allaire, J., Owen, J., Gromer, D., Shevtsov, P. and Thieurmel, B., dygraphs: Interface to Dygraphs Interactive Time Series Charting Library, 2015. URL http://CRAN. R-project. org/package= dygraphs. R package version 0.4, 5, p.7.
#'
#' @examples
#' \dontrun{
#' #load dummy data
#' data(hoopoe)
#' PAM_data=hoopoe
#'
#' # This bit is for Rstudio users to prevent html from opening in Viewer pane and crashing
#' # It opens in web browser instead
#' backup_options <- options()
#' options(viewer=NULL)
#'
#' # Plot interactive graphics
#' plot_interactive_timeseries(dta = PAM_data)
#'
#' # restore Rstudio settings from before plot
#' options(backup_options)
#' }
#'
#' @importFrom dygraphs dygraph dyRangeSelector dyHighlight dyLegend dyOptions dyShading "%>%"
#' @importFrom htmltools browsable tagList
#' @importFrom xts xts
#' @importFrom RColorBrewer brewer.pal
#' @export
plot_interactive_timeseries_classification <- function(to_classify,
                                                       classification_datetime,
                                                       classification,
                                                       to_plot = c("light", "pressure","temperature","acceleration", "magnetic")) {

  # backup_options <- options()
  from = to_classify$date[1]
  to = to_classify$date[length(to_classify$date)]
  len = ifelse( ("id" %in% to_plot) , length(names(to_plot))-1, length(names(to_plot)))

  dates = as.POSIXct((as.numeric(classification_datetime[2:length(classification_datetime)]) +
                        as.numeric(classification_datetime[1:(length(classification_datetime)-1)])) / 2, origin ='1970-01-01')

  class_colors = brewer.pal(n = length(unique(classification)), name = "Dark2")

  dy_graph = list()

  if ("light" %in% to_plot ){
    dy_graph$light = dygraph(xts(x = to_classify$light, order.by = to_classify$date),
                             xlab = "Time",
                             ylab = "Light",
                             group = to_classify$date, #This is useful for synchronosing multiple graphs
                             main="Light as a function of time") %>%
      dyRangeSelector(dateWindow = c(from, to)) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyLegend(hideOnMouseOut = TRUE, width = 600) %>%
      dyOptions(colors ="#66C2A5") #%>%

    for (i in 1:(length(dates)-1)) {
      dy_graph$light <- dy_graph$light %>% dyShading(from =dates[i], to = dates[i+1],
                                                     color= class_colors[classification[i+1]])#colors()[classification+2][i+1])
    }


                # drawPoints = TRUE,
                #   pointSize = 1,
                #   pointShape = c("dot", "triangle","square", "diamond",
                #                  "pentagon", "hexagon", "circle", "star",
                #                  "plus", "ex")[classification]
                #   )

  }
  if ("temperature" %in% to_plot ){
    dy_graph$temperature = dygraph(xts(x = to_classify$temperature, order.by = to_classify$date),
                                   xlab = "Time",
                                   ylab = "Temperature (C)",
                                   group = to_classify$date,#dta$temperature$date, #This is useful for synchronosing multiple graphs
                                   main="Temperature as a function of time") %>%
      dyRangeSelector(dateWindow = c(from, to)) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyLegend(hideOnMouseOut = TRUE, width = 600) %>%
      dyOptions(colors ="#A6D854")
    for (i in 1:(length(dates)-1)) {
      dy_graph$temperature <- dy_graph$temperature %>% dyShading(from =dates[i], to = dates[i+1],
                                                     color= class_colors[classification[i+1]])#colors()[classification+2][i+1])
    }
  }
  if ("bodytemperature" %in% to_plot ){
    dy_graph$bodytemperature = dygraph(xts(x = to_classify$bodytemperature, order.by = to_classify$date),
                                   xlab = "Time",
                                   ylab = "Temperature (C)",
                                   group = to_classify$date,#dta$temperature$date, #This is useful for synchronosing multiple graphs
                                   main="Body temperature as a function of time") %>%
      dyRangeSelector(dateWindow = c(from, to)) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyLegend(hideOnMouseOut = TRUE, width = 600) %>%
      dyOptions(colors ="#FC8D62")
    for (i in 1:(length(dates)-1)) {
      dy_graph$bodytemperature <- dy_graph$bodytemperature %>% dyShading(from =dates[i], to = dates[i+1],
                                                                 color= class_colors[classification[i+1]])#colors()[classification+2][i+1])
    }
  }
  if ("pressure" %in% to_plot ){
    dy_graph$pressure = dygraph(xts(x = to_classify$pressure, order.by = to_classify$date),
                                xlab = "Time",
                                ylab = "Pressure (hPa)",
                                group = to_classify$date,#dta$pressure$date, #This is useful for synchronosing multiple graphs
                                main="Pressure as a function of time") %>%
      dyRangeSelector(dateWindow = c(from, to)) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyLegend(hideOnMouseOut = TRUE, width = 600) %>%
      dyOptions(colors ="#FC8D62")
    for (i in 1:(length(dates)-1)) {
      dy_graph$pressure <- dy_graph$pressure %>% dyShading(from =dates[i], to = dates[i+1],
                                                                 color= class_colors[classification[i+1]])#colors()[classification+2][i+1])
    }
  }
  if ("acceleration" %in% to_plot ){
    dy_graph$activity = dygraph(xts(x = to_classify$act, order.by = to_classify$date),
                                xlab = "Time",
                                ylab = "Activity",
                                group = to_classify$date,#dta$acceleration$date, #This is useful for synchronosing multiple graphs
                                main="Activity as a function of time") %>%
      dyRangeSelector(dateWindow = c(from, to)) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyLegend(hideOnMouseOut = TRUE, width = 600) %>%
      dyOptions(colors ="#8DA0CB")
    for (i in 1:(length(dates)-1)) {
      dy_graph$activity <- dy_graph$activity%>% dyShading(from =dates[i], to = dates[i+1],
                                                                 color= class_colors[classification[i+1]])#colors()[classification+2][i+1])
    }
    dy_graph$pitch = dygraph(xts(x = to_classify$pit, order.by = to_classify$date),
                             xlab = "Time",
                             ylab = "Pitch",
                             group = to_classify$date,#dta$acceleration$date, #This is useful for synchronosing multiple graphs
                             main="Pitch as a function of time") %>%
      dyRangeSelector(dateWindow = c(from, to)) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyLegend(hideOnMouseOut = TRUE, width = 600) %>%
      dyOptions(colors ="#E78AC3")
    for (i in 1:(length(dates)-1)) {
      dy_graph$pitch <- dy_graph$pitch %>% dyShading(from =dates[i], to = dates[i+1],
                                                                 color= class_colors[classification[i+1]])#colors()[classification+2][i+1])
    }
  }
  if ("magnetic" %in% to_plot ){
    dy_graph$magneticg = dygraph(xts(x = to_classify[,c("mX","mY","mZ")], order.by = to_classify$date),
                                 xlab = "Time",
                                 ylab = "Magnetic mX, mY,and mZ",
                                 group = to_classify$date,#dta$temperature$date, #This is useful for synchronosing multiple graphs
                                 main="Mgnetism m as a function of time") %>%
      dyRangeSelector(dateWindow = c(from, to)) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyLegend(hideOnMouseOut = TRUE, width = 600) %>%
      dyOptions(colors = c("#8DA0CB","#A6D854","#FC8D62"))
    for (i in 1:(length(dates)-1)) {
      dy_graph$magneticg <- dy_graph$magneticg %>% dyShading(from =dates[i], to = dates[i+1],
                                                                 color= class_colors[classification[i+1]])#colors()[classification+2][i+1])
    }
    dy_graph$magneticm = dygraph(xts(x = to_classify[,c("gX", "gY","gZ")], order.by = to_classify$date),
                                 xlab = "Time",
                                 ylab = "Magnetic gX, gY,and gZ",
                                 group = to_classify$date,#dta$temperature$date, #This is useful for synchronosing multiple graphs
                                 main="Magnetism g as a function of time") %>%
      dyRangeSelector(dateWindow = c(from, to)) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyLegend(hideOnMouseOut = TRUE, width = 600) %>%
      dyOptions(colors = c("#8DA0CB","#A6D854","#FC8D62"))
    for (i in 1:(length(dates)-1)) {
      dy_graph$magneticm <- dy_graph$magneticm %>% dyShading(from =dates[i], to = dates[i+1],
                                                                 color= class_colors[classification[i+1]])#colors()[classification+2][i+1])
    }
  }


  # options(viewer=NULL)
  browsable(tagList(dy_graph))
  # options(backup_options)
}




