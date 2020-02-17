library(shinydashboard)
library("viridisLite")
data("Boston", package="MASS")

col <- new.env()

palette <- function(name, begin = 0, end = 1, direction = 1, option = "D") {
  col$dark   <- viridis(length(name), 1, begin, end, direction, option)
  col$normal <- viridis(length(name), 0.75, begin, end, direction, option)  
  col$light  <- viridis(length(name), 0.5, begin, end, direction, option)
  col$gray   <- rgb(0, 0, 0, c(1, 0.6, 0.2))
  names(col$dark) <- names(col$normal) <- names(col$light) <- name
}

dark <- function(name) {
  ind <- pmatch(name, names(col$dark))
  if (is.na(ind)) return(col$gray[1])
  col$dark[ind]
}

color <- function(name) {
  ind <- pmatch(name, names(col$dark))
  if (is.na(ind)) return(col$gray[2])
  col$light[ind]
}

light <- function(name) {
  ind <- pmatch(name, names(col$dark))
  if (is.na(ind)) return(col$gray[3])
  col$light[ind]
}

palette(c("population", "sample", begin=0.4))

ui <- dashboardPage(
  dashboardHeader(title = "MM*Stat"),
  dashboardSidebar(
    shiny::actionButton('draw', 'Draw sample(s)'),
    shiny::sliderInput("ndraw", "Number of samples:",  1, 10, 1),
    shiny::actionButton('reset', 'Reset'),
    shiny::sliderInput("mu0", "Hypothetical mean:", min(Boston$medv), max(Boston$medv), median(Boston$medv)),
    shiny::sliderInput("n", "Sample size:", 30, 10*floor(length(Boston$medv)/30), max(30, 10*floor(length(Boston$medv)/50)))
  ),
  dashboardBody(
    tags$head(tags$style("#plot{height:80vh !important;}")),
    shiny::plotOutput("outputId"="plot", width="100%")
  )
)

relative <- function(x, range, na.rm=TRUE) {
  if (length(range)==1) {
    ri <- 0
    ra <- range
  } else {
    ri <- min(range, na.rm=na.rm)
    ra <- max(range, na.rm=na.rm)
  }
  if (x[1]>=2) {
    n <- as.integer(x)
    ret <- ri+(ra-ri)*(0:(n-1))/n
  } else {
    ret <- ri+(ra-ri)*x
  }
  ret
}

draw.dnorm <- function(x, mean, sd, border="black", col=NULL) {
  x <- relative(500, x)
  y <- dnorm(x, mean=mean, sd=sd)
  if (!is.null(col)) {
    for (i in 1:499) {
      polygon(c(x[i], x[i+1], x[i+1], x[i]), c(0, 0, y[i+1], y[i]), col=col, border=NA)      
    }
  }
  lines(x, y, col=border)
  lines(x, rep(0, 500), col=border)
}

ceiling <- function (x, accuracy=1, base=0) {
  base+base::ceiling((x-base)/accuracy)*accuracy  
}

floor <- function (x, accuracy=1, base=0) {
  base+base::floor((x-base)/accuracy)*accuracy  
}


server <- function(input, output) {

  rv <- reactiveValues(means=c())
    
  observeEvent(input$reset, { rv$means=c() })
  
  output$plot <- renderPlot({
    input$draw
    # compute new means
    xb <- list()
    for (B in 1:input$ndraw) {
      xb[[B]] <- sample(Boston$medv, size=input$n)
    }
    isolate(rv$means <- c(rv$means, sapply(xb, mean)))
    # make draw
    n    <- max(30, 10*floor(length(Boston$medv)/50))
    ymax <- sqrt(n)/sqrt(2*pi*var(Boston$medv))
    isolate({
      d      <- diff(qnorm(c(0.001, 0.999), input$mu0, sd=sd(Boston$medv)/sqrt(input$n)))/20
      breaks <- seq(min(Boston$medv), max(Boston$medv), by=d)
      h      <- hist(rv$means, breaks=breaks, plot=FALSE)
      ymax   <- max(ymax, h$density)
      #browser()
    })
    ymax <- ceiling(ymax, 0.2)
    #
    yt   <- -relative(0.1, ymax/2)
    yl   <- -relative(c(0.05, 0.15), ymax/2)
    plot(x=Boston$medv, y=rep(yt, length(Boston$medv)), xlim=range(Boston$medv), ylim=c(-ymax/2, ymax), col=light("p"), pch=19, cex=0.5, ylab="")
    lines(c(mean(Boston$medv), mean(Boston$medv)), yl, col=dark("p"), lwd=2)
    #
    xg <- input$mu0+c(1.96, -1.96)*sd(Boston$medv)/sqrt(input$n)
    draw.dnorm(xg, 
               mean=input$mu0, sd=sd(Boston$medv)/sqrt(input$n), col=light("x"), border=light("d"))
    draw.dnorm(Boston$medv, 
               mean=input$mu0, sd=sd(Boston$medv)/sqrt(input$n), border=dark("d"))

    abline(v=c(xg, input$mu0), col=dark("d"))
    #
    yb <- -relative(0.25+0.6*(0:9)/9, ymax/2)
    yd <- relative(0.05, ymax/2)
    for (B in 1:input$ndraw) {
      points(xb[[B]], rep(yb[B], input$n), col=light("s"), pch=19, cex=0.5)
      lines(c(mean(xb[[B]]), mean(xb[[B]])), c(yb[B]-yd,yb[B]+yd), col=dark("s"),lwd=2)
    }
    h <- hist(rv$means, breaks=breaks, col=color("s"), add=TRUE, border=NA, ylab="", freq=FALSE, warn.unused=FALSE)
    rug(rv$means, col=dark("s"))
    text(relative(-0.01, Boston$medv), relative(-0.25, ymax/2), "Samples", col=dark("s"), srt=90, pos=2)
    text(relative(1.01, Boston$medv), 0, "Population", col=dark("p"), srt=90, pos=1)
    text(min(Boston$medv), ymax, expression(paste("Theoretical distribution of ", bar(X))), col=dark("e"), pos=4)
    n <- length(rv$means)
    labels <- bquote("Empirical distribution of "~bar(X)~" based on "~n==.(n)~" sample(s)")
    text(max(Boston$medv), ymax, col=dark("s"), pos=2, labels=labels)
  })
}

shinyApp(ui, server)