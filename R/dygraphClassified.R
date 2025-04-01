#' Plot PAM data with dygraphs
#'
#' @description This opens a java application which allows the user to zoom in and out. In Rstudio it will open in the viewer pane and in base R in an html readers. Note that this can be a bit slow
#'
#' @param dta PAM data to be plotted
#' @param from date that plotting starts
#' @param to date that plotting ends
#' @param toPLOT names of the variables to plot. For now this includes `light`, `pressure`, `acceleration` and `temperature`
#' @param timetable classification of start/stop
#' @return a plot of all the measurements
#'
#' @examples
#' ##load dummy data
#' #data(hoopoe)
#' #PAM_data=hoopoe
#'
#' ## This bit is for Rstudio users to prevent html from opening in Viewer pane and crashing
#' ## It opens in web browser instead
#' #backup_options <- options()
#' #options(viewer=NULL)
#'
#' ## Classify bir's behaviour
#' #behaviour = classifyFLAP(dta = PAM_data$acceleration, period = 5)
#'
#' ## Plot interactive graphics
#' #dygraphClassified(dta = PAM_data, timetable = behaviour$timetable)
#'
#' ## restore Rstudio settings from before plot
#' #options(backup_options)
#'
#' @importFrom dygraphs dygraph dyRangeSelector dyHighlight dyLegend dyOptions dyShading "%>%"
#' @importFrom htmltools browsable tagList
#' @importFrom xts xts
#'
#' @export
dygraphClassified <- function(dta,
                       from = dta$light$date[1],
                       to = dta$light$date[length(dta$light$date)],
                       toPLOT = names(dta),
                       timetable = timetable) {
  print("Error: This function is deprecated, use plot_interactive_timeseries in latest version of PAMLr which can be installed by running devtools::install_github('KiranLDA/PAMLr')")

#   # backup_options <- options()
#   # col = c("#66C2A5","#A6D854","#FC8D62","#8DA0CB")
#
#   ok_periods <- list()
#   for (i in 1:nrow(timetable)){
#     ok_periods[[i]] = list(from = timetable$start[i], to = timetable$end[i])
#   }
#
#
#
#   len = ifelse( ("id" %in% toPLOT) , length(names(dta))-1, length(names(dta)))
#
#   dy_graph = list()
#
#   if ("light" %in% toPLOT ){
#
#     dy_graph$light = dygraph(xts(x = dta$light$obs, order.by = dta$light$date),
#                              xlab = "Time",
#                              ylab = "Light",
#                              group = dta$light$date, #This is useful for synchronosing multiple graphs
#                              main="Light as a function of time") %>%
#       dyRangeSelector(dateWindow = c(from, to)) %>%
#       dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
#       dyLegend(hideOnMouseOut = TRUE, width = 600) %>%
#       dyOptions(colors ="#66C2A5")
#
#     #add shades
#     for( period in ok_periods ) {
#       dy_graph$light <- dyShading(dy_graph$light, from = period$from , to = period$to, color = "#6D6A69")
#     }
#
#
#   }
#   if ("temperature" %in% toPLOT ){
#     dy_graph$temperature = dygraph(xts(x = dta$temperature$obs, order.by = dta$temperature$date),
#                                    xlab = "Time",
#                                    ylab = "Temperature (C)",
#                                    group = dta$light$date,#dta$temperature$date, #This is useful for synchronosing multiple graphs
#                                    main="Temperature as a function of time") %>%
#       dyRangeSelector(dateWindow = c(from, to)) %>%
#       dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
#       dyLegend(hideOnMouseOut = TRUE, width = 600) %>%
#       dyOptions(colors ="#A6D854")
#     #add shades
#     for( period in ok_periods ) {
#       dy_graph$temperature <- dyShading(dy_graph$temperature, from = period$from , to = period$to, color = "#6D6A69" )
#     }
#   }
#   if ("pressure" %in% toPLOT ){
#     dy_graph$pressure = dygraph(xts(x = dta$pressure$obs, order.by = dta$pressure$date),
#                                 xlab = "Time",
#                                 ylab = "Pressure (hPa)",
#                                 group = dta$light$date,#dta$pressure$date, #This is useful for synchronosing multiple graphs
#                                 main="Pressure as a function of time") %>%
#       dyRangeSelector(dateWindow = c(from, to)) %>%
#       dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
#       dyLegend(hideOnMouseOut = TRUE, width = 600) %>%
#       dyOptions(colors ="#FC8D62")
#     #add shades
#     for( period in ok_periods ) {
#       dy_graph$pressure <- dyShading(dy_graph$pressure, from = period$from , to = period$to, color = "#6D6A69" )
#     }
#   }
#   if ("acceleration" %in% toPLOT ){
#     dy_graph$activity = dygraph(xts(x = dta$acceleration$act, order.by = dta$acceleration$date),
#                                 xlab = "Time",
#                                 ylab = "Activity",
#                                 group = dta$light$date,#dta$acceleration$date, #This is useful for synchronosing multiple graphs
#                                 main="Activity as a function of time") %>%
#       dyRangeSelector(dateWindow = c(from, to)) %>%
#       dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
#       dyLegend(hideOnMouseOut = TRUE, width = 600) %>%
#       dyOptions(colors ="#8DA0CB")
#     #add shades
#     for( period in ok_periods ) {
#       dy_graph$activity <- dyShading(dy_graph$activity, from = period$from , to = period$to, color = "#6D6A69" )
#     }
#
#     dy_graph$pitch = dygraph(xts(x = dta$acceleration$pit, order.by = dta$acceleration$date),
#                              xlab = "Time",
#                              ylab = "Pitch",
#                              group = dta$light$date,#dta$acceleration$date, #This is useful for synchronosing multiple graphs
#                              main="Pitch as a function of time") %>%
#       dyRangeSelector(dateWindow = c(from, to)) %>%
#       dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
#       dyLegend(hideOnMouseOut = TRUE, width = 600) %>%
#       dyOptions(colors ="#E78AC3")
#     #add shades
#     for( period in ok_periods ) {
#       dy_graph$pitch <- dyShading(dy_graph$pitch, from = period$from , to = period$to, color = "#6D6A69" )
#     }
#   }
#   if ("magnetic" %in% toPLOT ){
#     dy_graph$magneticg = dygraph(xts(x = dta$magnetic[,c(2,3,4)], order.by = dta$magnetic$date),
#                                  xlab = "Time",
#                                  ylab = "Magnetic gX, gY,and gZ",
#                                  group = dta$light$date,#dta$temperature$date, #This is useful for synchronosing multiple graphs
#                                  main="Mgnetism g as a function of time") %>%
#       dyRangeSelector(dateWindow = c(from, to)) %>%
#       dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
#       dyLegend(hideOnMouseOut = TRUE, width = 600) %>%
#       dyOptions(colors = c("#8DA0CB","#A6D854","#FC8D62"))
#     #add shades
#     for( period in ok_periods ) {
#       dy_graph$magneticg <- dyShading(dy_graph$magneticg, from = period$from , to = period$to, color = "#6D6A69" )
#     }
#
#     dy_graph$magneticm = dygraph(xts(x = dta$magnetic[,c(5,6,7)], order.by = dta$magnetic$date),
#                                  xlab = "Time",
#                                  ylab = "Magnetic mX, mY,and mZ",
#                                  group = dta$light$date,#dta$temperature$date, #This is useful for synchronosing multiple graphs
#                                  main="Magnetism m as a function of time") %>%
#       dyRangeSelector(dateWindow = c(from, to)) %>%
#       dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
#       dyLegend(hideOnMouseOut = TRUE, width = 600) %>%
#       dyOptions(colors = c("#8DA0CB","#A6D854","#FC8D62"))
#     for( period in ok_periods ) {
#       dy_graph$magneticm <- dyShading(dy_graph$magneticm, from = period$from , to = period$to, color = "#6D6A69" )
#     }
#   }
#
#
#   # options(viewer=NULL)
#   browsable(tagList(dy_graph))
#   # options(backup_options)
 }




