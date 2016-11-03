output$vizTypes <- renderUI({
  objTypes <- paste(readLines("objTypes.html"),collapse = "")
  subTypes <- paste(readLines("subTypes.html"),collapse = "")
  ipsTypes <- paste(readLines("ipsTypes.html"),collapse = "")

  list(
    HTML(objTypes)
    # conditionalPanel("input.db == 'objetivos'",
    #                  fluidRow(
    #                    HTML(objTypes)
    #                 )
    #                  ),
    # conditionalPanel("input.db == 'subjetivos'",
    #                  fluidRow(
    #                    HTML(subTypes)
    #                  )
    # ),
    # conditionalPanel("input.db == 'ips'",
    #                  fluidRow(
    #                    HTML(ipsTypes)
    #                  )
    #)
  )
})
