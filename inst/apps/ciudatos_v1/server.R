# ciudatos_V1
library(shiny)
library(tidyverse)

library(leaflet)
library(scales)
library(lazyeval)
library(DT)
library(ciudatos)

library(shinyjs)

library(lazyeval)


dbInfo <- list_fringe()
dbs <- unique(dbInfo$group)
names(dbs) <- unique(dbInfo$groupLabel)
#frs <- load_fringes(group = "objetivos")
frs <- load_fringes(n_max = Inf)
availableTables <- dbInfo$id
names(availableTables) <- dbInfo$label

fixedVarNames <- list(
  "objetivos" = list(ciudadVar = "Ciudad", yearVar = "Anio"),
  "subjetivos" = list(ciudadVar = "CIUDAD", yearVar = "AÑO"),
  "ips" = list(ciudadVar = "Ciudad", yearVar = "anio")
)




# Define a server for the Shiny app
shinyServer(function(input, output) {

  output$dbList <- renderUI({
    list(
      selectInput("db","",dbs, selected = 1)
    )
  })

  output$navbar <- renderUI({
    if(is.null(input$selectedTable)) return()
    if(grepl("ods",input$db))
      h <- paste(readLines("navbar-clean.html"),collapse="")
    else{
      if(grepl("comparada|interciudades",input$selectedTable))
        h <- paste(readLines("navbar.html"),collapse="")
      else
        h <- paste(readLines("navbar-clean.html"),collapse="")

    }
    HTML(h)
  })

  output$dbTables <- renderUI({
    if(grepl("ods",input$db)) return()
    tables <- dbInfo %>% filter_(interp(~ col == input$db, col = as.name("group"))) %>% .$id
    names(tables) <- dbInfo %>% filter_(interp(~ col == input$db, col = as.name("group"))) %>% .$label
    str(tables)
    list(
      selectInput("selectedTable", "Tabla:",choices=tables,selected = 1)
    )
  })

  dbType <- reactive({
    if(is.null(input$selectedTable)) return()
    if(grepl("comparada",input$selectedTable))
      return("comparada")
    "ciudad"
  })


  output$objCities <- renderUI({
    cities <- dbInfo %>% filter_(interp(~ col == input$db, col = as.name("group"))) %>% .$id
    cities <- cities[-1]
    nms <- dbInfo %>% filter_(interp(~ col == input$db, col = as.name("group"))) %>% .$label
    names(cities) <- nms[-1]
    list(
      selectInput("selectedCity", "Ciudad:",choices=cities,selected = 1)
    )
  })

  fringe <- reactive({
    if(is.null(input$selectedTable)) return()
    selectedTable <- input$selectedTable
    #selectedTable <- 1
    frs[[selectedTable]]
  })


  output$dbVars <- renderUI({
    if(grepl("ods",input$db)) return()

    f <- fringe()
    dic <- f$dic_$d
    varsNu <- selectDicCtypes(f,"Nu",as_list = TRUE)
    list(
      selectInput("selectedVar", "Variable:",choices=varsNu,selected = 1)
    )
  })



  output$geoYears <- renderUI({
    if(is.null(input$selectedVar)) return()
    selectedVar <- input$selectedVar
    f <- fringe()
    selectedVarNu <- input$selectedVar
    ciudadVar <- fixedVarNames[[input$db]]$ciudadVar
    yearVar <- fixedVarNames[[input$db]]$yearVar
    fgeo <- selectFringeCols(f,c(ciudadVar,yearVar,selectedVarNu))
    availableYears <- fgeo$d %>% group_by(b) %>%
      summarize(undefined = all(is.na(c))) %>%
      filter(!undefined) %>% .$b %>% sort(decreasing = TRUE)
    selectInput("selectedGeoYear", "Año:",choices= availableYears, selected = 1)
  })


  output$geo <- renderUI({
    selectedYear <- input$selectedGeoYear
    if(is.null(input$selectedVar)) return()
    selectedVarNu <- input$selectedVar
    f <- fringe()
    ciudadVar <- fixedVarNames[[input$db]]$ciudadVar
    yearVar <- fixedVarNames[[input$db]]$yearVar
    fgeo <- selectFringeCols(f,c(ciudadVar,yearVar,selectedVarNu))
    fgeo <- keepFringeRows(fgeo,yearVar,selectedYear) %>%
      selectFringeCols(c(ciudadVar,selectedVarNu))
    leaf <- lflt_co_mun(fgeo)
    list(
      renderLeaflet(leaf)
    )
  })



  output$chrono <- renderUI({
    f <- fringe()
    selectedVarNu <- input$selectedVar
    ciudadVar <- fixedVarNames[[input$db]]$ciudadVar
    yearVar <- fixedVarNames[[input$db]]$yearVar
    fchrono <- selectFringeCols(f,c(ciudadVar,yearVar ,selectedVarNu))
    if(input$selectedChronoType == "column")
      h <- hgch_bar_cyn(fchrono)
    if(input$selectedChronoType == "lines")
      h <- hgch_lines_cyn(fchrono)
    list(
      renderHighchart(h)
    )
  })

  output$rankYears <- renderUI({
    if(is.null(input$selectedVar)) return()
    selectedVar <- input$selectedVar
    f <- fringe()
    selectedVarNu <- input$selectedVar
    ciudadVar <- fixedVarNames[[input$db]]$ciudadVar
    yearVar <- fixedVarNames[[input$db]]$yearVar
    f <- selectFringeCols(f,c(ciudadVar,yearVar,selectedVarNu))
    availableYears <- f$d %>% group_by(b) %>%
      summarize(undefined = all(is.na(c))) %>%
      filter(!undefined) %>% .$b %>% sort(decreasing = TRUE)
    selectInput("selectedRankYear", "Año:",choices= availableYears, selected = 1)
  })

  output$rank <- renderUI({
    selectedYear <- input$selectedRankYear
    selectedVarNu <- input$selectedVar
    f <- fringe()
    ciudadVar <- fixedVarNames[[input$db]]$ciudadVar
    yearVar <- fixedVarNames[[input$db]]$yearVar
    frank <- keepFringeRows(f,yearVar,selectedYear) %>%
      selectFringeCols(c(ciudadVar,selectedVarNu))
    if(input$rankType == "Treemap"){
      h <- hgch_treemap(frank)
    }else{
      h <- hgch_bar_top(frank)
    }
    list(
      renderHighchart(h)
    )
  })


  output$scatterOpts <- renderUI({
    if(is.null(input$selectedVar)) return()
    selectedVar <- input$selectedVar
    f <- fringe()
    varsNu <- selectDicCtypes(f,"Nu",as_list = TRUE)
    selectedVarNu <- input$selectedVar
    ciudadVar <- fixedVarNames[[input$db]]$ciudadVar
    yearVar <- fixedVarNames[[input$db]]$yearVar
    f <- selectFringeCols(f,c(ciudadVar,yearVar,selectedVarNu))
    availableYears <- f$d %>% group_by(b) %>%
      summarize(undefined = all(is.na(c))) %>%
      filter(!undefined) %>% .$b %>% sort(decreasing = TRUE)
    list(
      selectInput("selectedScatterVarNu2", "Variable para cruzar:",choices= varsNu, selected = varsNu[2]),
      selectInput("selectedScatterYear", "Año:",choices= availableYears, selected = 1)
    )
  })

  output$scatter <- renderUI({
    f <- fringe()
    selectedVarNu <- c(input$selectedVar,input$selectedScatterVarNu2)
    selectedYear <- input$selectedScatterYear
    ciudadVar <- fixedVarNames[[input$db]]$ciudadVar
    yearVar <- fixedVarNames[[input$db]]$yearVar
    fgap <- f %>% keepFringeRows(yearVar,selectedYear) %>%
      selectFringeCols(c(ciudadVar,selectedVarNu))
    h <- hgch_scatter(fgap)
    list(
      renderHighchart(h)
    )
  })

  output$table <- renderUI({
    f <- fringe()
    selectedVarNu <- input$selectedVar
    ciudadVar <- fixedVarNames[[input$db]]$ciudadVar
    yearVar <- fixedVarNames[[input$db]]$yearVar

    f <- selectFringeCols(f,c(ciudadVar,yearVar,selectedVarNu))
    d <- f$data
    names(d) <- getClabels(f)
    t <- datatable(d,options = list(
      scrollX = TRUE,
      language = list(url = "//cdn.datatables.net/plug-ins/f2c75b7247b/i18n/Spanish.json")
    ))
    list(
      downloadButton('downloadData', 'Descargar datos'),
      renderDataTable(t)
    )
  })

  ## CIUDAD
  output$ciudad <- renderUI({
    list(
      uiOutput("ciudadControls"),
      uiOutput("ciudadControls2"),
      uiOutput("ciudadViz")
    )
  })

  output$ciudadControls <- renderUI({
    f <- fringe()
    varsNu <- selectDicCtypes(f,"Nu",as_list = TRUE)
    list(
      selectInput("ciudadVizType","Tipo de visualización",
                  choices = c("Líneas" = "line","Barras" = "bar")),
      radioButtons("ciudadCompareOpts","Comparar",
                   c("No","Con Otro Indicador","Con Otra Ciudad"))
    )
  })


  output$debug <- renderPrint({
    # tables <- dbInfo %>% filter_(interp(~ col == input$db, col = as.name("group"))) %>% .$id
    # names(tables) <- dbInfo %>% filter_(interp(~ col == input$db, col = as.name("group"))) %>% .$label
    # str(tables)
    #getCnames(fringe())
    fringe2()
  })

  output$dbTables2 <- renderUI({
    if(grepl("ods",input$db)) return()
    tables <- dbInfo %>% filter_(interp(~ col == input$db, col = as.name("group"))) %>% .$id
    names(tables) <- dbInfo %>% filter_(interp(~ col == input$db, col = as.name("group"))) %>% .$label
    tables <- tables[-1]
    tables <- tables %>% keep( ~ . != input$selectedTable)
    list(
      selectInput("selectedTable2", "Tabla 2:",choices=tables,selected = 1)
    )
  })

  output$dbVars2 <- renderUI({
    if(grepl("ods",input$db)) return()

    f <- fringe2()
    dic <- f$dic_$d
    varsNu <- selectDicCtypes(f,"Nu",as_list = TRUE)
    list(
      selectInput("selectedVar2", "Variable:",choices=varsNu,selected = 1)
    )
  })

  fringe2 <- reactive({
    if(is.null(input$selectedTable2)) return()
    selectedTable <- input$selectedTable2
    #selectedTable <- 1
    frs[[selectedTable]]
  })


  fringeCiudad <- reactive({
    f <- fringe()
    #selectedVarNu <- input$selectedVar
    if(input$ciudadCompareOpts != "Con Otro Indicador"){
      selectedVarNu <- input$selectedVar
    }else{
      selectedVarNu <- c(input$selectedVar,input$selectedCiudadVarNu2)
    }
    #ciudadVar <- fixedVarNames[[input$db]]$ciudadVar
    yearVar <- fixedVarNames[[input$db]]$yearVar
    f <- selectFringeCols(f,c(yearVar,selectedVarNu))
    f
  })

  fringeCiudad2 <- reactive({
    f <- fringe2()
    selectedVarNu <- input$selectedVar2
    yearVar <- fixedVarNames[[input$db]]$yearVar
    f <- selectFringeCols(f,c(yearVar,selectedVarNu))
    f
  })


  output$ciudadControls2 <- renderUI({
    f <- fringe()
    varsNu <- selectDicCtypes(f,"Nu",as_list = TRUE)
    #f2 <- fringe2()
    # varsNu2 <- selectDicCtypes(f2,"Nu",as_list = TRUE)

    list(
      conditionalPanel("input.ciudadCompareOpts == 'Con Otro Indicador'",
                       selectInput("selectedCiudadVarNu2", "Variable para cruzar:",choices= varsNu, selected = varsNu[2])
      ),
      conditionalPanel("input.ciudadCompareOpts == 'Con Otra Ciudad'",
                       uiOutput("dbTables2"),
                       uiOutput("dbVars2")
      ),
      br()
    )
  })

  output$ciudadViz <- renderUI({
    f <- fringeCiudad()
    ciudadVizType <- input$ciudadVizType
    h <- hgch_multilines_ynp(f,type = ciudadVizType)
    h2 <- NULL
    if(input$ciudadCompareOpts == "Con Otra Ciudad"){
      f2 <- fringeCiudad2()
      h2 <- hgch_multilines_ynp(f2,type = ciudadVizType)
    }
    list(
      renderHighchart(h),
      renderHighchart(h2)
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

  ### Ods

  output$odsControls <- renderUI({
    ods <- read_csv(sysfile("data/fringe/ods/objetivos-data.csv"))
    obj <- ods$Número
    names(obj) <- ods$Título

    odsFull <- read_csv(sysfile("data/fringe/ods/ods-full-data.csv"))
    ciudades <- unique(odsFull$Ciudad)

    list(
      selectInput("odsObjetivo","Seleccione Objetivo",
                  choices = obj),
      selectInput("odsCiudad","Seleccione Ciudad",
                  choices = ciudades)
    )
  })

  output$odsImage <- renderUI({
    list(
      img(class="img-center",src=paste0(input$odsObjetivo,".png"),width = "125px")
    )
  })

  output$ods <- renderUI({
    ods <- read_csv(sysfile("data/fringe/ods/ods-full-data.csv"))
    objNumber <- paste0("Objetivo",input$odsObjetivo)

    ods <- ods %>% filter(Objetivos == objNumber)
    if(!is.null(input$odsCiudad)){
      ods <- ods %>% filter(Ciudad == input$odsCiudad)
    }
    t <- datatable(ods,options = list(
      scrollX = TRUE,
      pageLength = 5,
      language = list(url = "//cdn.datatables.net/plug-ins/f2c75b7247b/i18n/Spanish.json")
    ))
    list(
      renderDataTable(t)
      #img(src="ods-logo.png",width = "100%")
    )
  })




  observe({

    if(!is.null(dbType())){
      if(dbType() == "ciudad" & !grepl("ips",input$selectedTable) ){
        shinyjs::hide(id = "geoPage", anim = TRUE,animType = "fade")
        shinyjs::hide(id = "chronoPage", anim = TRUE,animType = "fade")
        shinyjs::hide(id = "rankPage", anim = TRUE,animType = "fade")
        shinyjs::hide(id = "scatterPage", anim = TRUE,animType = "fade")
        shinyjs::hide(id = "dataPage", anim = TRUE,animType = "fade")
        shinyjs::hide(id = "odsPage", anim = TRUE,animType = "fade")
        shinyjs::hide(id = "odsControlsContainer", anim = TRUE,animType = "fade")
        shinyjs::show(id = "ciudadPage", anim = TRUE,animType = "fade")
      }else{
        shinyjs::show(id = "geoPage", anim = TRUE,animType = "fade")
        shinyjs::hide(id = "chronoPage", anim = TRUE,animType = "fade")
        shinyjs::hide(id = "rankPage", anim = TRUE,animType = "fade")
        shinyjs::hide(id = "scatterPage", anim = TRUE,animType = "fade")
        shinyjs::hide(id = "dataPage", anim = TRUE,animType = "fade")
        shinyjs::hide(id = "odsPage", anim = TRUE,animType = "fade")
        shinyjs::hide(id = "odsControlsContainer", anim = TRUE,animType = "fade")
        shinyjs::hide(id = "ciudadPage", anim = TRUE,animType = "fade")

        if(grepl("ods",input$db)){
          shinyjs::hide(id = "geoPage", anim = TRUE,animType = "fade")
          shinyjs::hide(id = "chronoPage", anim = TRUE,animType = "fade")
          shinyjs::hide(id = "rankPage", anim = TRUE,animType = "fade")
          shinyjs::hide(id = "scatterPage", anim = TRUE,animType = "fade")
          shinyjs::hide(id = "dataPage", anim = TRUE,animType = "fade")
          shinyjs::show(id = "odsPage", anim = TRUE,animType = "fade")
          shinyjs::show(id = "odsControlsContainer", anim = TRUE,animType = "fade")
        }
      }
    }

    shinyjs::onclick("geoNav",{
      shinyjs::show(id = "geoPage", anim = TRUE,animType = "fade")
      shinyjs::hide(id = "chronoPage", anim = TRUE,animType = "fade")
      shinyjs::hide(id = "rankPage", anim = TRUE,animType = "fade")
      shinyjs::hide(id = "scatterPage", anim = TRUE,animType = "fade")
      shinyjs::hide(id = "dataPage", anim = TRUE,animType = "fade")
      shinyjs::hide(id = "odsPage", anim = TRUE,animType = "fade")
      shinyjs::hide(id = "odsControlsContainer", anim = TRUE,animType = "fade")
      addClass("geoNavLi", "active")
      removeClass("chronoNavLi", "active")
      removeClass("rankNavLi", "active")
      removeClass("scatterNavLi", "active")
      removeClass("dataNavLi", "active")
    })
    shinyjs::onclick("chronoNav",{
      shinyjs::hide(id = "geoPage", anim = TRUE,animType = "fade")
      shinyjs::show(id = "chronoPage", anim = TRUE,animType = "fade")
      shinyjs::hide(id = "rankPage", anim = TRUE,animType = "fade")
      shinyjs::hide(id = "scatterPage", anim = TRUE,animType = "fade")
      shinyjs::hide(id = "dataPage", anim = TRUE,animType = "fade")
      shinyjs::hide(id = "odsPage", anim = TRUE,animType = "fade")
      shinyjs::hide(id = "odsControlsContainer", anim = TRUE,animType = "fade")
      removeClass("geoNavLi", "active")
      addClass("chronoNavLi", "active")
      removeClass("rankNavLi", "active")
      removeClass("scatterNavLi", "active")
      removeClass("dataNavLi", "active")

    })
    shinyjs::onclick("rankNav",{
      shinyjs::hide(id = "geoPage", anim = TRUE,animType = "fade")
      shinyjs::hide(id = "chronoPage", anim = TRUE,animType = "fade")
      shinyjs::show(id = "rankPage", anim = TRUE,animType = "fade")
      shinyjs::hide(id = "scatterPage", anim = TRUE,animType = "fade")
      shinyjs::hide(id = "dataPage", anim = TRUE,animType = "fade")
      shinyjs::hide(id = "odsPage", anim = TRUE,animType = "fade")
      shinyjs::hide(id = "odsControlsContainer", anim = TRUE,animType = "fade")
      removeClass("geoNavLi", "active")
      removeClass("chronoNavLi", "active")
      addClass("rankNavLi", "active")
      removeClass("scatterNavLi", "active")
      removeClass("dataNavLi", "active")

      disable("dbRank")
    })
    shinyjs::onclick("scatterNav",{
      shinyjs::hide(id = "geoPage", anim = TRUE,animType = "fade")
      shinyjs::hide(id = "chronoPage", anim = TRUE,animType = "fade")
      shinyjs::hide(id = "rankPage", anim = TRUE,animType = "fade")
      shinyjs::show(id = "scatterPage", anim = TRUE,animType = "fade")
      shinyjs::hide(id = "dataPage", anim = TRUE,animType = "fade")
      shinyjs::hide(id = "odsPage", anim = TRUE,animType = "fade")
      shinyjs::hide(id = "odsControlsContainer", anim = TRUE,animType = "fade")
      removeClass("geoNavLi", "active")
      removeClass("chronoNavLi", "active")
      removeClass("rankNavLi", "active")
      addClass("scatterNavLi", "active")
      removeClass("dataNavLi", "active")

      disable("dbscatter")
    })
    shinyjs::onclick("dataNav",{
      shinyjs::hide(id = "geoPage", anim = TRUE,animType = "fade")
      shinyjs::hide(id = "chronoPage", anim = TRUE,animType = "fade")
      shinyjs::hide(id = "rankPage", anim = TRUE,animType = "fade")
      shinyjs::hide(id = "scatterPage", anim = TRUE,animType = "fade")
      shinyjs::show(id = "dataPage", anim = TRUE,animType = "fade")
      shinyjs::hide(id = "odsPage", anim = TRUE,animType = "fade")
      shinyjs::hide(id = "odsControlsContainer", anim = TRUE,animType = "fade")
      removeClass("geoNavLi", "active")
      removeClass("chronoNavLi", "active")
      removeClass("rankNavLi", "active")
      removeClass("scatterNavLi", "active")
      addClass("dataNavLi", "active")
      disable("dbscatter")
    })
  })


})



