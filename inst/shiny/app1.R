launch.browser <- function(url) {
  # modify URL, which has no effect for app1 :(
  url <- sprintf('%s/?lang=%s', url, 'de')
  invisible(.Call("rs_shinyviewer", url, getwd(), 3))
}
#
library("shiny")
runApp(system.file('shiny', 'app1', package='demoShiny'), launch.browser=launch.browser)