library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyExample)

toNum <- function(x, min=-Inf, max=+Inf) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x)!=1) return(NA)
  if(is.na(x)) return(NA)  
  if (x<min) return(NA)  
  if (x>max) return(NA)
  x 
}

library("dbscan")
library("rio")
x <- scale(import("BANK2.sav"))



ui <- dashboardPage(
  dashboardHeader(title="", titleWidth=, disable=),
  dashboardSidebar(collapsed=, width=, disable=,
                   uiOutput("outputId"="UIeps"),
                   uiOutput("outputId"="UIpts"),
                   shiny::tags$div(align="center",
                                   shiny::tags$hr(),
                                   shiny::tags$a(href = 'https://github.com/sigbertklinke/shinyExample', 'Created with shinyExample'),
                                   shiny::tags$br(),
                                   shiny::tags$a(target="_blank", href="https://www.wihoforschung.de/de/flipps-1327.php",  'Supported by BMBF')
                   )
  ),
  dashboardBody(
    shiny::plotOutput("outputId"="plot",
                      "width"="100%",
                      "height"="400px",
                      "inline"=FALSE)
  )
)

server <- function(input, output, session) {
  seed <- list(inBookmark=FALSE)
  
  onBookmark(function(state) {
    state$seed <- seed
  })
  
  onRestore(function(state) {
    seed <- state$seed
    seed$inBookmark <- TRUE
  })
  
  onRestored(function(state) {
    seed$inBookmark <- FALSE
  })
  
  onStop(function() {
#   if (isLocal()) {
#      count <- getMMstat('lang', 'stats', 'count')
#      cat(sprintf('gettext("%s"); // %.0f\n', names(count), count))
#    }
  })
  
  value <- function(val) {
    param <- substitute(val)
    if(param=="input$eps") { v<-toNum(val, min=0, max=1); if(is.na(v)) return(0.5) else return(v) }
    if(param=="input$pts") { v<-toNum(val, min=2, max=10); if(is.na(v)) return(5) else return(v) }
    return(val)
  }
  
  observe({
    
    sel  <- value(isolate(input$eps))
    shiny::updateSliderInput("session"=session,
                             "inputId"="eps",
                             "label"=("Core distance"),
                             "value"=sel,
                             "min"=0,
                             "max"=1,
                             "step"=0.01)
  })
  observe({
    
    sel  <- value(isolate(input$pts))
    shiny::updateSliderInput("session"=session,
                             "inputId"="pts",
                             "label"=("Minimal neighbour "),
                             "value"=sel,
                             "min"=2,
                             "max"=10,
                             "step"=1)
  })
  
  output$plot <- shiny::renderPlot({
    
    #/home/sigbert/syncthing/projekte/R/shinyApp/inst/app/dbscan/dbscan2.R
    # shinyApp/inst/app/dbscan/dbscan2.R
    db  <- dbscan(x[,c(4,6)], value(input$eps), value(input$pts))
    col <- c('grey', rainbow(max(db$cluster)))
    plot(x[,c(4,6)], col=col[1+db$cluster], pch=19, asp=TRUE)
  })
  output$UIeps<- renderUI({
    shiny::sliderInput("inputId"="eps",
                       "label"=("Core distance"),
                       "min"=0,
                       "max"=1,
                       "value"=0.5,
                       "step"=0.01,
                       "round"=FALSE,
                       "ticks"=TRUE,
                       "animate"=FALSE,
                       "width"=NULL,
                       "sep"=",",
                       "pre"=NULL,
                       "post"=NULL,
                       "timeFormat"=NULL,
                       "timezone"=NULL,
                       "dragRange"=TRUE)
  })
  output$UIpts<- renderUI({
    shiny::sliderInput("inputId"="pts",
                       "label"=("Minimal neighbour "),
                       "min"=2,
                       "max"=10,
                       "value"=5,
                       "step"=1,
                       "round"=FALSE,
                       "ticks"=TRUE,
                       "animate"=FALSE,
                       "width"=NULL,
                       "sep"=",",
                       "pre"=NULL,
                       "post"=NULL,
                       "timeFormat"=NULL,
                       "timezone"=NULL,
                       "dragRange"=TRUE)
  })
}

shinyApp(ui, server)
