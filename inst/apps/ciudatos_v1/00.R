library(shiny)
library(shinyjs)


shinyApp(
  ui = fluidPage(
    useShinyjs(),  # Set up shinyjs
    actionButton("btn", "Click me"),
    p(id = "element", "Watch what happens to me")
  ),
  server = function(input, output) {
    observeEvent(input$btn, {
      # Change the following line for more examples
      html("element", paste0("The date is ", today()))
    })
  }
)
