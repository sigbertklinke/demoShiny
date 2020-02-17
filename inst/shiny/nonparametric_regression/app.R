library(shiny)
library(shinydashboard)
library("MASS")
data(mcycle)

ui <- dashboardPage(
  dashboardHeader(title = "MM*Stat"),
  dashboardSidebar(
    tags$div(HTML("<h3>Nonparametric regression<h3>"), align="center"),
    sliderInput("x", "X", ceiling(min(mcycle$times)), floor(max(mcycle$times)),  30, step=0.5),
    sliderInput("w", "Window width", 0.5, floor(0.6*max(mcycle$times)), 10, step=0.5),
    checkboxGroupInput("option", "Options", 
                       choices=list("Linear fit"=1, 
                                    "Connect"=2, 
                                    "Nonparametric fit"=3))
  ),
  dashboardBody(
    tags$head(tags$style("#plot1{height:80vh !important;}")),
    plotOutput("plot1", width="100%")
  )
)

fit <- function(x, y, xh, w, linear) {
  ind <- outer(x, xh, w=w, function(x1, x2, w) {
    (x1-w <= x2) & (x2 <= x1+w)
  })
  yh <- matrix(NA, nrow=length(xh), ncol=3)
  for (i in 1:ncol(ind)) {
    index <- which(ind[,i])
    if (linear) {
      lmfit <- lm(y~x, subset=index)
      yh[i,] <- lmfit$coefficient[1]+lmfit$coefficient[2]*c(xh[i], xh[i]-w, xh[i]+w)
    } else {
      yh[i,] <- mean(y[index])
    }
  }
  yh
}

server <- function(input, output) { 
  
  rv <- reactiveValues(x=NULL, y=NULL, draw=0)
  
  observeEvent(input$w, {
    rv$x <- numeric(0)
    rv$y <- numeric(0)   
    isolate(rv$draw <- rv$draw+1)
  })
  
  observe({
    if ("1" %in% input$option) {
      rv$x <- numeric(0)
      rv$y <- numeric(0)    
    }
    isolate(rv$draw <- rv$draw+1)
  })
  
  output$plot1 <- renderPlot({
#    browser()
    isolate(w <- input$w)
    isolate(linear  <- ("1" %in% input$option))
    isolate(connect <- ("2" %in% input$option))
    isolate(fullfit <- ("3" %in% input$option))
    rv$draw
    x <- input$x
    plot(accel~times, data=mcycle, main="Motorcycle data", type="n")
    usr <- par("usr")
    polygon(c(x-w, x+w, x+w, x-w), c(usr[3], usr[3], usr[4], usr[4]), col="gray", border=NA)
    points(accel~times, data=mcycle, pch=19, cex=0.5)
    
    yh <- fit(mcycle$times, mcycle$accel, input$x, w, linear)
    index <- (x-w<=mcycle$times) & (mcycle$times<=x+w)
    isolate(rv$x <- cbind(rv$x, input$x))
    isolate(rv$y <- cbind(rv$y, yh[1,1]))
    lines(c(input$x-w, input$x+w), yh[1,2:3], col="red")
    if (connect) {
      o <- order(rv$x)
      lines(rv$x[o], rv$y[o], col="red", lwd=2)
    }
    points(rv$x, rv$y, col="red", pch=19, cex=0.75)
    if (fullfit) {
      xh <- seq(min(mcycle$times), max(mcycle$times), length.out=301)
      yh <- fit(mcycle$times, mcycle$accel, xh, w, linear)
      lines(xh, yh[,1], col="blue", lwd=2)
    }
  })
  
}

shinyApp(ui, server)