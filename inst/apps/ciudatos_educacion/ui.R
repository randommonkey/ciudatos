library(shiny)
library(readr)
library(ciudatos)

objDict <- read_csv(sysfile("data/educacion/objetivos-edu-dict.csv"))
objVarChoices <- as.list(objDict$names[4:17])
names(objVarChoices) <- objDict$description[4:17]

objDictBog <-  read_csv(sysfile("data/educacion/objetivos-bog-edu-dict.csv"))
objVarChoicesBog <- as.list(objDictBog$Variable[4:105])
names(objVarChoicesBog) <- objDictBog$Indicador[4:105]


# Define the overall UI
shinyUI(
  fluidPage(

    # Generate a row with a sidebar
    sidebarLayout(

      # Define the sidebar with one input
      sidebarPanel(
        selectInput("db","Base de datos:",c("Comparada ciudades","Bogotá"),selected = 1),
        conditionalPanel("input.db == 'Comparada ciudades'",
                         selectInput("selectedObjVar", "Variable:",choices=objVarChoices)
        ),
        conditionalPanel("input.db == 'Bogotá'",
                         selectInput("selectedObjVarBog", "Variable:",choices=objVarChoicesBog)
        ),
        hr(),
        helpText("Seleccione el indicador objetivo a visualizar")
      ),

      # Create a spot for the barplot
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Mapa",
                             selectInput("selectedYear", "Año:",choices= 2011:2015, selected = 2015),
                             conditionalPanel("input.db == 'Comparada ciudades'",
                                              uiOutput("geo")
                             ),
                             conditionalPanel("input.db == 'Bogotá'",
                                              uiOutput("geoBog")
                             )
                    ),
                    tabPanel("Evolución",
                             selectInput("selectedChartType", "Tipo:",
                                         choices= list("Barras"="column","Líneas"="line")),
                             conditionalPanel("input.db == 'Comparada ciudades'",
                                              uiOutput("time")
                             ),
                             conditionalPanel("input.db == 'Bogotá'",
                                              uiOutput("timeBog")
                             )
                    ),
                    tabPanel("Datos",
                             conditionalPanel("input.db == 'Comparada ciudades'",
                                              uiOutput("table")
                             ),
                             conditionalPanel("input.db == 'Bogotá'",
                                              uiOutput("tableBog")
                             )
                             ),
                    tabPanel("Diccionario de Datos",
                             conditionalPanel("input.db == 'Comparada ciudades'",
                                              uiOutput("table2")
                             ),
                             conditionalPanel("input.db == 'Bogotá'",
                                              uiOutput("table2Bog")
                             )
                             )
        )


      )

    )
  )
)
