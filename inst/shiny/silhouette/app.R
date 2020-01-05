library("shinydashboard")
library("cluster")
library("gettext")

n  <- 25
cl <- matrix(rnorm(4*n), ncol=2)

ui <- dashboardPage(
  dashboardHeader(title = "Silhouette"),
  dashboardSidebar(
    sliderInput('cdist', 'Cluster distances', 0, 10, 5, 0.1),
    sliderInput('pdist', 'Point location', -7, 7, 0, 0.1),
    radioButtons('pid',  'Point belongs to', 
                choiceNames = c('red cluster', 'blue cluster'),
                choiceValues = 1:2)
  ),
  dashboardBody(
    plotOutput("cluster", height = 400)
  )
)

server <- function(input, output, session) {
  
  output$cluster <- renderPlot({
    x1 <- cl[1:n,1]-input$cdist/2
    y1 <- cl[1:n,2]
    x2 <- cl[n+(1:n),1]+input$cdist/2
    y2 <- cl[n+(1:n),2]
    par(mfrow=c(1,2))
    plot(0,0, type="n", xlim=c(-7,7), ylim=c(-3,3), asp=TRUE, main="Cluster assignment")
    abline(h=0, col="darkgrey")
    points(x1, y1, col="red", pch=19)
    points(x2, y2, col="blue", pch=19)  
    xp <- input$pdist
    yp <- 0
    points(xp, yp, col=ifelse(input$pid==1, "salmon", "lightblue"), pch=19)
    points(xp, yp, col="black")
    di  <- dist(cbind(c(xp, x1, x2), c(yp, y1, y2)))
    cl  <- as.integer(c(input$pid, rep(1, n), rep(2, n)))
    si  <- silhouette(cl, di)
    col <- c(ifelse(input$pid==1, 'salmon', 'lightblue'), rep("red", n), rep("blue", n))
    sic <- order(si[,'cluster'], -si[,'sil_width'])
    plot(si, col=col[sic], main="Silhouettes")
  })
}

shinyApp(ui, server)