library(shiny)
x <- 1
print(x)

ui <- fluidPage(
  actionButton("x_go","go")
)
server <- function(input,output){
  
  observeEvent(input$x_go,{
    print(x)
    x <- x+1
    print(x)
  })
}
shinyApp(ui=ui,server=server)