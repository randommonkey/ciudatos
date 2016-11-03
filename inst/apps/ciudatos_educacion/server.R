library(shiny)
library(readr)
library(leaflet)
library(scales)
library(lazyeval)
library(DT)
library(ciudatos)


dComparada <- read_csv(sysfile("data/educacion/objetivos-edu-data.csv"))
dBogota <- read_csv(sysfile("data/educacion/objetivos-bog-edu-data.csv"))

dl <- list("Comparada ciudades"=dComparada,"Bogotá"=dBogota)

objDict <- read_csv(sysfile("data/educacion/objetivos-edu-dict.csv"))
objVarChoices <- as.list(objDict$names[4:17])
names(objVarChoices) <- objDict$description[4:17]

objDictBog <-  read_csv(sysfile("data/educacion/objetivos-bog-edu-dict.csv"))
objVarChoicesBog <- as.list(objDictBog$Variable[4:105])
names(objVarChoicesBog) <- objDictBog$Indicador[4:105]

# Define a server for the Shiny app
shinyServer(function(input, output) {
  data <- reactive({
    dl[[input$db]]
  })
  output$geo <- renderUI({
    selectedYear <- input$selectedYear %||% 2011
    selectedVar <- input$selectedObjVar %||% "v1_d_001"
    message(selectedVar)
    #selectedYear <- 2011
    fixedVars <- c("Ciudad")
    vars <- c(fixedVars,selectedVar)
    d0 <- data()
    d <- d0 %>% filter(Anio == selectedYear) %>% select_(.dots = vars) %>% rename_("value"=selectedVar)
    geo <- read_csv(sysfile("data/aux/geoPoints.csv"))
    d <- d %>% left_join(geo[c("municipio","latitud","longitud")],c("Ciudad" = "municipio"))
    title <- objDict$description[objDict$names == selectedVar]
    d$info <- paste(d$Ciudad,title,sep="<br>") %>% paste(d$value,sep=": ")
    leaf <- plotLeafletCol(d)
    list(
      renderLeaflet(leaf)
    )
  })
  output$geoBog <- renderUI({
    selectedYear <- input$selectedYear %||% 2011
    selectedVar <- input$selectedObjVarBog %||% "v04_e_014"
    fixedVars <- c("Localidad")
    vars <- c(fixedVars,selectedVar)
    d0 <- data()
    d <- d0 %>%
      filter(Localidad != "Total Bogotá") %>%
      filter(Anio == selectedYear) %>% select_(.dots = vars) %>% rename_("value"=selectedVar)
    geo <- read_csv(sysfile("data/aux/geoPointsLocalidades.csv"))
    d <- d %>% left_join(geo[c("name","latitud","longitud")],c("Localidad" = "name"))
    title <- objDictBog$Indicador[objDictBog$Variable == selectedVar]
    d$info <- paste(d$Localidad,title,sep="<br>") %>% paste(d$value,sep=": ")
    leaf <- plotLeafletBog(d)
    list(
      renderLeaflet(leaf)
    )
  })

  output$time <- renderUI({
    selectedVar <- input$selectedObjVar %||% "v1_d_001"
    fixedVars <- c("Ciudad","Anio")
    vars <- c(fixedVars,selectedVar)
    d0 <- data()
    d <- d0 %>% select_(.dots = vars)  %>% rename_("value"=selectedVar)
    title <- objDict$description[objDict$names == selectedVar]
    dd <- spread(d,Ciudad,value)
    chartType <- input$selectedChartType
    h <- plotChronoCities(dd, type = chartType, title = title)
    list(
      renderHighchart(h)
    )
  })
  output$timeBog <- renderUI({
    selectedVar <- input$selectedObjVarBog %||% "v04_e_014"
    fixedVars <- c("Localidad","Anio")
    vars <- c(fixedVars,selectedVar)
    d0 <- data()
    d <- d0 %>% select_(.dots = vars) %>% filter(Localidad != "Total Bogotá") %>% rename_("value"=selectedVar)
    d <- na.omit(d)
    dd <- spread(d,Localidad,value)
    title <- objDictBog$Indicador[objDictBog$Variable == selectedVar]
    chartType <- input$selectedChartType
    h <- plotChronoLocalidades(dd, type = chartType, title = title)
    list(
      renderHighchart(h)
    )
  })

  output$table <- renderUI({
    d0 <- data()
    t <- datatable(d0)
    list(
      downloadButton('downloadData', 'Descargar datos'),
      renderDataTable(t)
    )
  })

  output$table2 <- renderUI({
    t <- datatable(objDict)
    list(
      downloadButton('downloadDict', 'Descargar diccionario'),
      renderDataTable(t)
    )
  })

  output$tableBog <- renderUI({
    d0 <- data()
    t <- datatable(d0)
    list(
      downloadButton('downloadData', 'Descargar datos'),
      renderDataTable(t)
    )
  })

  output$table2Bog <- renderUI({
    t <- datatable(objDictBog)
    list(
      downloadButton('downloadDict', 'Descargar diccionario'),
      renderDataTable(t)
    )
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      "data.csv"
    },
    content = function(file) {
      d <- dl[[input$db]]
      write.csv(d,file)
    }
  )

  output$downloadDict <- downloadHandler(
    filename = function() {
      "data.csv"
    },
    content = function(file) {
      if(input$db == "Comparada ciudades"){
        d <- objDict
      }
      if(input$db == "Bogotá"){
        d <- objDictBog
      }
      write.csv(d,file)
    }
  )




})



