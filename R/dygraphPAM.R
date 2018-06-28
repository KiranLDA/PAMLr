#' Plot PAM data
#'
#' @param dta PAM data to be plotted
#'
#' @return a plot of all the measurements
#'
#' @examples
#' #specify the data location
#' data(PAM_data)
#' str(PAM_data)
#'
#' #plot the activity to see if it looks ok
#' plot(PAM_data$acceleration$date, PAM_data$acceleration$act, xlab="Time", ylab="activity")
#'
#' # at first glance it looks like the logger was removed off a birds and left in arucksack
#  # so remove un-needed data
#' PAM_data$acceleration = PAM_data$acceleration[((PAM_data$acceleration$date >= "2016-07-30")
#' & (PAM_data$acceleration$date <= "2017-06-01")),]
#'
#'
#' @export
dygraphPAM <- function(dta,
                       from = dta$light$date[1],
                       to = dta$light$date[length(dta$light$date)],
                       toPLOT = names(dta)) {
  len = ifelse( ("id" %in% toPLOT) , length(names(dta))-1, length(names(dta)))
#
#
#   x11()     #Use X11() or quartz() if on linux or mac.
#   par(mfrow=c(len,1))
#   prompt  = "Hit spacebar to close plots"
#   extra   = "Use left mouse click to zoom in and double click to zoom out"
#   # capture = tk_messageBox(message = prompt, detail = extra)
#
#   library(htmltools)
#   dy_graph <- list(
#     dygraphs::dygraph(temperature, group="temp_rain", main="temperature"),
#     dygraphs::dygraph(rainfall, group="temp_rain", main="rainfall")
#   )  # end list
#
#   # render the dygraphs objects using htmltools
#   htmltools::browsable(htmltools::tagList(dy_graph))
#
  dy_graph = list()

  if ("light" %in% toPLOT ){
    dy_graph$light = dygraph(xts(x = dta$light$obs, order.by = dta$light$date),
                             xlab = "Time",
                             ylab = "Light",
                             group = dta$light$date, #This is useful for synchronosing multiple graphs
                             main="Light as a function of time") %>%
      # dyRangeSelector(dateWindow = c(from, to)) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyLegend(hideOnMouseOut = TRUE, width = 600)
  }
  if ("pressure" %in% toPLOT ){
    dy_graph$pressure = dygraph(xts(x = dta$pressure$obs, order.by = dta$pressure$date),
                             xlab = "Time",
                             ylab = "Pressure (hPa)",
                             group = dta$pressure$date, #This is useful for synchronosing multiple graphs
                             main="Pressure as a function of time") %>%
      # dyRangeSelector(dateWindow = c(from, to)) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyLegend(hideOnMouseOut = TRUE, width = 600)
  }
  if ("acceleration" %in% toPLOT ){
    dy_graph$activity = dygraph(xts(x = dta$acceleration$act, order.by = dta$acceleration$date),
                                xlab = "Time",
                                ylab = "Activity",
                                group = dta$acceleration$date, #This is useful for synchronosing multiple graphs
                                main="Activity as a function of time") %>%
      # dyRangeSelector(dateWindow = c(from, to)) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyLegend(hideOnMouseOut = TRUE, width = 600)
    dy_graph$pitch = dygraph(xts(x = dta$acceleration$pit, order.by = dta$acceleration$date),
                                    xlab = "Time",
                                    ylab = "Pitch",
                                    group = dta$acceleration$date, #This is useful for synchronosing multiple graphs
                                    main="Pitch as a function of time") %>%
      # dyRangeSelector(dateWindow = c(from, to)) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyLegend(hideOnMouseOut = TRUE, width = 600)
  }
  if ("temperature" %in% toPLOT ){
    dy_graph$temperature = dygraph(xts(x = dta$temperature$obs, order.by = dta$temperature$date),
                                    xlab = "Time",
                                    ylab = "Temperature (C)",
                                    group = dta$temperature$date, #This is useful for synchronosing multiple graphs
                                    main="Temperature as a function of time") %>%
      # dyRangeSelector(dateWindow = c(from, to)) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyLegend(hideOnMouseOut = TRUE, width = 600)
  }

  # htmltools::browsable(htmltools::tagList(dy_graph))
}




