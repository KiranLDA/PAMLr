#' Plot PAM data as an interactive timeseries
#'
#' @description This opens a java dygraph application which allows the user to zoom in and out. In Rstudio it will open in the viewer pane and in base R in an html readers. Note that this can be a bit slow
#'
#' @param dta PAM data to be plotted
#' @param from date that plotting starts
#' @param to date that plotting ends
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
#'
#' }
#'
#' @importFrom dygraphs dygraph dyRangeSelector dyHighlight dyLegend dyOptions dyShading "%>%"
#' @importFrom htmltools browsable tagList
#' @importFrom xts xts
#' @export
plot_interactive_timeseries <- function(dta,
                       from = dta$light$date[1],
                       to = dta$light$date[length(dta$light$date)],
                       to_plot = names(dta)) {
  # backup_options <- options()
  len = ifelse( ("id" %in% to_plot) , length(names(dta))-1, length(names(dta)))

  dy_graph = list()

  if ("light" %in% to_plot ){
    dy_graph$light = dygraph(xts(x = dta$light$obs, order.by = dta$light$date),
                             xlab = "Time",
                             ylab = "Light",
                             group = dta$light$date, #This is useful for synchronosing multiple graphs
                             main="Light as a function of time") %>%
      dyRangeSelector(dateWindow = c(from, to)) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyLegend(hideOnMouseOut = TRUE, width = 600) %>%
      dyOptions(colors ="#66C2A5")
  }
  if ("temperature" %in% to_plot ){
    dy_graph$temperature = dygraph(xts(x = dta$temperature$obs, order.by = dta$temperature$date),
                                   xlab = "Time",
                                   ylab = "Temperature (C)",
                                   group = dta$light$date,#dta$temperature$date, #This is useful for synchronosing multiple graphs
                                   main="Temperature as a function of time") %>%
      dyRangeSelector(dateWindow = c(from, to)) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyLegend(hideOnMouseOut = TRUE, width = 600) %>%
      dyOptions(colors ="#A6D854")
  }
  if ("bodytemperature" %in% to_plot ){
    dy_graph$bodytemperature = dygraph(xts(x = dta$bodytemperature$obs, order.by = dta$bodytemperature$date),
                                   xlab = "Time",
                                   ylab = "Temperature (C)",
                                   group = dta$light$date,#dta$temperature$date, #This is useful for synchronosing multiple graphs
                                   main="Body temperature as a function of time") %>%
      dyRangeSelector(dateWindow = c(from, to)) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyLegend(hideOnMouseOut = TRUE, width = 600) %>%
      dyOptions(colors ="#FC8D62")
  }
  if ("pressure" %in% to_plot ){
    dy_graph$pressure = dygraph(xts(x = dta$pressure$obs, order.by = dta$pressure$date),
                                xlab = "Time",
                                ylab = "Pressure (hPa)",
                                group = dta$light$date,#dta$pressure$date, #This is useful for synchronosing multiple graphs
                                main="Pressure as a function of time") %>%
      dyRangeSelector(dateWindow = c(from, to)) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyLegend(hideOnMouseOut = TRUE, width = 600) %>%
      dyOptions(colors ="#FC8D62")
  }
  if ("acceleration" %in% to_plot ){
    dy_graph$activity = dygraph(xts(x = dta$acceleration$act, order.by = dta$acceleration$date),
                                xlab = "Time",
                                ylab = "Activity",
                                group = dta$light$date,#dta$acceleration$date, #This is useful for synchronosing multiple graphs
                                main="Activity as a function of time") %>%
      dyRangeSelector(dateWindow = c(from, to)) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyLegend(hideOnMouseOut = TRUE, width = 600) %>%
      dyOptions(colors ="#8DA0CB")
    dy_graph$pitch = dygraph(xts(x = dta$acceleration$pit, order.by = dta$acceleration$date),
                             xlab = "Time",
                             ylab = "Pitch",
                             group = dta$light$date,#dta$acceleration$date, #This is useful for synchronosing multiple graphs
                             main="Pitch as a function of time") %>%
      dyRangeSelector(dateWindow = c(from, to)) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyLegend(hideOnMouseOut = TRUE, width = 600) %>%
      dyOptions(colors ="#E78AC3")
  }
  if ("magnetic" %in% to_plot ){
    dy_graph$magneticg = dygraph(xts(x = dta$magnetic[,c("gX", "gY","gZ")], order.by = dta$magnetic$date),
                                 xlab = "Time",
                                 ylab = "Magnetic gX, gY,and gZ",
                                 group = dta$light$date,#dta$temperature$date, #This is useful for synchronosing multiple graphs
                                 main="Mgnetism g as a function of time") %>%
      dyRangeSelector(dateWindow = c(from, to)) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyLegend(hideOnMouseOut = TRUE, width = 600) %>%
      dyOptions(colors = c("#8DA0CB","#A6D854","#FC8D62"))
    dy_graph$magneticm = dygraph(xts(x = dta$magnetic[,c("mX", "mY","mZ")], order.by = dta$magnetic$date),
                                 xlab = "Time",
                                 ylab = "Magnetic mX, mY,and mZ",
                                 group = dta$light$date,#dta$temperature$date, #This is useful for synchronosing multiple graphs
                                 main="Magnetism m as a function of time") %>%
      dyRangeSelector(dateWindow = c(from, to)) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyLegend(hideOnMouseOut = TRUE, width = 600) %>%
      dyOptions(colors = c("#8DA0CB","#A6D854","#FC8D62"))
  }


  # options(viewer=NULL)
  browsable(tagList(dy_graph))
  # options(backup_options)
}




