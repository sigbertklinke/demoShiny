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

ui <- dashboardPage(
  dashboardHeader("title"="MM*Stat",
                  "titleWidth"=NULL,
                  "disable"=FALSE),
  dashboardSidebar("disable"=FALSE,
                   "width"=NULL,
                   "collapsed"=FALSE,
                   uiOutput("outputId"="UIr"),
                   uiOutput("outputId"="UIn"),
                   
                   shiny::tags$div(align="center",
                                   shiny::tags$hr(),
                                   shiny::tags$a(href = "https://github.com/sigbertklinke/shinyApp", "Created with shinyApp"),
                                   shiny::tags$br(),
                                   shiny::tags$a(target="_blank", href="https://www.wihoforschung.de/de/flipps-1327.php",  "Supported by BMBF"))),
  dashboardBody(shiny::plotOutput("outputId"="plot",
                                  "width"="100%",
                                  "height"="400px",
                                  "inline"=FALSE))
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
    #if (isLocal()) {
    #  count <- getMMstat('lang', 'stats', 'count')
    #  cat(sprintf('gettext("%s"); // %.0f\n', names(count), count))
    #}
  })
  
  value <- function(val) {
    param <- substitute(val)
    if(param=="input$r") { v<-toNum(val, min=-1, max=1); if(is.na(v)) return(0) else return(v) }
    if(param=="input$n") { v<-toNum(val, min=30, max=500); if(is.na(v)) return(100) else return(v) }
    return(val)
  }
  
  observe({
    
    sel  <- value(isolate(input$r))
    shiny::updateSliderInput("session"=session,
                             "inputId"="r",
                             "label"=("r_xy"),
                             "value"=sel,
                             "min"=-1,
                             "max"=1,
                             "step"=0.01)
  })
  observe({
    
    sel  <- value(isolate(input$n))
    shiny::updateSliderInput("session"=session,
                             "inputId"="n",
                             "label"=("n"),
                             "value"=sel,
                             "min"=30,
                             "max"=500,
                             "step"=10)
  })
  
  output$plot <- shiny::renderPlot({
    
    #/home/sigbert/syncthing/projekte/R/shinyApp/inst/app/correlation/corr.R
    # shinyApp/inst/app/correlation/corr.R
    library("mvtnorm")
    n <- value(input$n)
    r <- value(input$r)
    repeat{
      out <- rmvnorm(n, mean = c(0,0), sigma = matrix(c(1,r,r,1), ncol=2))
      rr  <- cor(out)[1,2]
      if (abs(rr-r)<0.002) break
    }
    plot(out, pch=19, xlim=c(-3,3), ylim=c(-3,3), asp=TRUE, axes=FALSE, xlab="x", ylab="y",
         main=sprintf("Korrelation: %.2f", rr), cex=1/log10(n))
    box()
  })
  output$UIr<- renderUI({
    shiny::sliderInput("inputId"="r",
                       "label"=("r_xy"),
                       "min"=-1,
                       "max"=1,
                       "value"=0,
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
  output$UIn<- renderUI({
    shiny::sliderInput("inputId"="n",
                       "label"=("n"),
                       "min"=30,
                       "max"=500,
                       "value"=100,
                       "step"=10,
                       "round"=TRUE,
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
