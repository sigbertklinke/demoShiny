library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyExample)

library("scagnostics")
data(Boston, package = "MASS")
scag <- scagnostics(Boston)


getList <- function(...) { list(...) }

toInt <- function(x, min=-Inf, max=+Inf) {
  x <- suppressWarnings(as.integer(x))
  if (length(x)!=1) return(NA_integer_)
  if(is.na(x)) return(NA_integer_)
  if (x<min) return(NA_integer_)  
  if (x>max) return(NA_integer_)  
  x
}

toNum <- function(x, min=-Inf, max=+Inf) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x)!=1) return(NA)
  if(is.na(x)) return(NA)  
  if (x<min) return(NA)  
  if (x>max) return(NA)
  x 
}

ui <- dashboardPage(
  dashboardHeader(title="", titleWidth=, disable=),
  dashboardSidebar(collapsed=, width=, disable=,
                   uiOutput("outputId"="UIcoef"),
                   uiOutput("outputId"="UIindex"),
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
#    if (isLocal()) {
#      count <- getMMstat('lang', 'stats', 'count')
#      cat(sprintf('gettext("%s"); // %.0f\n', names(count), count))
#    }
  })
  
  value <- function(val) {
    param <- substitute(val)
    if(param=="input$coef") { v<-toInt(val, min=1); if(is.na(v)) return(1) else return(v) }
    if(param=="input$index") { v<-toNum(val, min=1, max=91); if(is.na(v)) return(1) else return(v) }
    return(val)
  }
  
  observe({
    
    sel<-value(isolate(input$coef))
    shiny::updateSelectInput("session"=session,
                             "inputId"="coef",
                             "label"=("Select coefficient"),
                             "choices"=getList("Outlying"=1,"Skewed"=2,"Clumpy"=3,"Sparse"=4,"Striated"=5,"Convex"=6,"Skinny"=7,"Stringy"=8,"Monotonic"=9),
                             "selected"=sel)
  })
  observe({
    
    sel  <- value(isolate(input$index))
    shiny::updateSliderInput("session"=session,
                             "inputId"="index",
                             "label"=("Plot number"),
                             "value"=sel,
                             "min"=1,
                             "max"=91,
                             "step"=NULL)
  })
  
  output$plot <- shiny::renderPlot({
    
    #/home/sigbert/syncthing/projekte/R/shinyApp/inst/app/scagnostics/scagnostics.R
    # shinyApp/inst/app/scagnostic/scagnostics.R
    vlist   <- strsplit(colnames(scag), ' * ', fixed=TRUE)
    coef    <- value(input$coef)
    pnr     <- value(input$index)
    o       <- order(scag[coef,])
    main    <- c("Outlying", "Skewed", "Clumpy", "Sparse", "Striated", "Convex", "Skinny", "Stringy", "Monotonic")
    main    <- sprintf("%s(%.0f): %.3f", main[coef], pnr, scag[coef,o[pnr]])
    par(mfrow=c(1,2))
    plot(Boston[,vlist[[o[pnr]]]], pch=19, main=main, cex=0.5)
    plot(Boston[,rev(vlist[[o[pnr]]])], pch=19, main=main, cex=0.5)
  })
  output$UIcoef <- renderUI({
    shiny::selectInput("inputId"="coef",
                       "label"=("Select coefficient"),
                       "choices"=getList("Outlying"=1,"Skewed"=2,"Clumpy"=3,"Sparse"=4,"Striated"=5,"Convex"=6,"Skinny"=7,"Stringy"=8,"Monotonic"=9),
                       "selected"=NULL,
                       "multiple"=FALSE,
                       "selectize"=TRUE,
                       "width"=NULL,
                       "size"=NULL)
  })
  output$UIindex<- renderUI({
    shiny::sliderInput("inputId"="index",
                       "label"=("Plot number"),
                       "min"=1,
                       "max"=91,
                       "value"=1,
                       "step"=NULL,
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
