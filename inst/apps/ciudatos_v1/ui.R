library(shiny)
library(readr)
library(ciudatos)

library(shinyjs)

library(leaflet)

styles <- "
@import url('https://fonts.googleapis.com/css?family=Raleway');
body {
font-family: 'Raleway', sans-serif;
}
h1,h2{
color: #545657;
}

h5{
color: #BFC1C2;
font-weight: 400;
}

p{
color: #95989A;
}

hr{
border-top: 2px solid #eee;
}

#plot-container {
position: relative;
}
.loading-spinner {
position: absolute;
left: 50%;
top: 50%;
z-index: -1;
margin-top: -33px;  /* half of the spinner's height */
margin-left: -33px; /* half of the spinner's width */
}
#plot.recalculating {
z-index: -2;
}


#loading-content {
position: absolute;
background: #FFFFFF;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #CCC;
}

#nav {
border: 1px solid #452383;
background-color: #452383;
}

#nav > .col-sm-8 {
background-color: #452383;
}

#nav > .col-sm-4 {
background-color: white;
}

.viztypes{
  background-color: #452383;
  font-size: 18px;
  padding: 10px 10px;
  margin-bottom: 0px;
  margin-left: 20px;
}

.viztypes > li > a {
color: white !important;
text-decoration: none;
}
.viztypes > li > a:hover{
text-decoration: none;
}



.viztypes > li > a{
    margin-left: 10px;
}
.viztypes > li > a:hover,
.viztypes > li > a:focus,
.viztypes > .active > a,
.viztypes > .active > a:hover,
.viztypes > .active > a:focus {
     border-bottom: 2px solid white;
padding-bottom: 2px;
}



.light{
background-color: white !important;
color: #452383 !important;
}

.navbar-right{
    padding-left: 50px;
}

#dbList{
  padding-top: 15px;
  padding-left: 15px;
  margin-bottom: -12px;
}

#dbList > * {
  margin-bottom: 0px;
}

#dbList .form-group {
  display: inline-flex;
}

#dbList .form-group >div {
  margin-top: -7px;
  width: 55%;
}

#dbList .selectize-input {
  border: 0px solid #cccccc;
}

.selectize-input{
  border-radius: 0px;
  border: 1px solid #452383;
}
.selectize-input > *{
 color: #452383;
}

.selectize-dropdown single{
//border-radius: 0px;
}

.selectize-input.focus{
border-radius: 0px;
border-color: #452383;
}

.selectize-dropdown,
.selectize-input,
.selectize-input input {
  color: #452383;
}

label{
color: #452383;
}

.selectize-control.single .selectize-input:after{
  border-color: #452383 transparent transparent transparent;
}

.selectize-input.focus{
box-shadow: inset 0px 0px 0px
}

#colControls{
background-color: white
}

#colViz{
padding-left: 0;
}

.img-center{
    display: block;
    margin: 0 auto;
}

body {
margin-right:10px;
}

.shiny-output-error { visibility: ; }

#chronoPage{
  padding-right: 20px;
}
#rankingPage{
  padding-right: 20px;
}

"




# Define the overall UI
shinyUI(
  fluidPage(
    useShinyjs(),
    includeCSS("styles.css"),
    inlineCSS(styles),
    # Generate a row with a sidebar
    fluidRow(id="container",
             fluidRow(id="nav",
                      column(4,id="colControls",
                             uiOutput("dbList")
                      ),
                      column(8,
                             uiOutput("navbar")
                             #uiOutput("vizTypes")
                      )
             ),
             column(12,
                    fluidRow(
                      column(4,
                             uiOutput("dbTables"),
                             uiOutput("dbVars"),
                             hr(),
                             hidden(
                               div(id="odsControlsContainer",
                                   uiOutput("odsControls"),
                                   uiOutput("odsImage")
                               )
                             ),
                             #verbatimTextOutput("debug"),
                             br()
                             #helpText("Seleccione el indicador objetivo a visualizar"),
                      ),
                      column(8,id="colViz",
                             #verbatimTextOutput("debug"),
                             div(id="geoPage",
                                 uiOutput("geoYears"),
                                 uiOutput("geo"),
                                 br()
                             ),
                             hidden(
                               #div(id = "dbTypeComparada",
                               div(id= "chronoPage",
                                   selectInput("selectedChronoType", "Tipo:",
                                               choices= list("Líneas"="lines","Barras"="column")),
                                   uiOutput("chrono"),
                                   #uiOutput("time"),
                                   br()
                               ),
                               div(id= "rankPage",
                                   uiOutput("rankYears"),
                                   selectInput("rankType", "Tipo:",
                                               choices= list("Treemap"="Treemap","Barras"="Barras")),
                                   uiOutput("rank")

                               ),
                               div(id= "scatterPage",
                                   uiOutput("scatterOpts"),
                                   uiOutput("scatter")
                               ),
                               div(id = "compPage",
                                   h2("Rank page")
                                   # sliderInput("selectedYear2", "Año:",min =2011,max=2015,
                                   #             value = 2013,animate = TRUE),
                                   # conditionalPanel("input.db == 'Comparada ciudades'",
                                   #                  selectInput("selectedObjVar2", "Variable:",choices=objVarChoices, selected = 2)
                                   # ),
                                   # conditionalPanel("input.db == 'Bogotá'",
                                   #                  selectInput("selectedObjVarBog2", "Variable:",choices=objVarChoicesBog, selected = 2)
                                   # ),                             conditionalPanel("input.db == 'Comparada ciudades'",
                                   #                                                 uiOutput("gap")
                                   # ),
                                   # conditionalPanel("input.db == 'Bogotá'",
                                   #                  uiOutput("gapBog")
                                   # )
                               ),
                               div(id="dataPage",
                                   uiOutput("table")
                               ),
                               div(id="odsPage",
                                   uiOutput("ods")
                               )
                               #)
                             ),
                             hidden(
                               div(id="ciudadPage",
                                   uiOutput("ciudad")

                               )
                             )
                      )
                    )
             )
    )
  )
)
