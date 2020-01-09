library("shiny")

server <- function(input, output) {
  output$distPlot <- renderPlot({
    breaks <- seq(min(faithful$eruptions), max(faithful$eruptions), len=input$nbin+1)
    hist(faithful$eruptions, breaks=breaks, 
         xlab="Eruption time", ylab="Frequency", main="Old Faithful geyser")
  })
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("nbin", "Number of bins:", min = 2, max = 50, value = 20)
    ),
    mainPanel(plotOutput("distPlot"))
  )
)

shinyApp(ui = ui, server = server)
