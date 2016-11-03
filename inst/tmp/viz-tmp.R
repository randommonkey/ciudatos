
hgch_gapminder <- function(f, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL){
  xAxisTitle <- xAxisTitle %||% getClabels(f)[2]
  yAxisTitle <- yAxisTitle %||% getClabels(f)[3]
  title <-  title %||% ""
  d <- f$d %>% filter(!is.na(c),!is.na(d))

  #dd <- d %>% complete(nesting(c, d), b) %>% select(a,b,c,d) %>% arrange(a,b)
  #tmp <- expand(d,a,b)
  d <- d %>% filter(a %in% c("Barranquilla","Bogotá","Cali"))

  # funciona para serie normal
  series <- d %>%
    rename(name = a) %>%
    group_by(name) %>%
    do(data = unname(as.matrix(.[c("c","d")]))) %>%
    transpose()

  #
  series <- d %>%
    rename(name = a) %>%
    group_by(name) %>%
    do(data = unname(as.matrix(.[c("c","d")]))) %>%
    by_row()
  transpose()

  series <- d %>%
    by_row(~unlist(unname(c(.$c,.$d)))) %>%
    rename(name = a) %>%
    slice_rows("name") %>%
    by_slice(~ list(sequence = .$.out)) %>%
    rename(data = .out) %>%
    transpose()

  series <- d %>%
    #by_row(~unlist(unname(c(.$c,.$d)))) %>%
    rename(name = a) %>%
    group_by(name) %>%
    do(data = list(list(sequence = .$c),list(sequence = .$d))) %>%
    transpose()

  highchart() %>%
    hc_chart(type = "bubble") %>%
    hc_motion(enabled = TRUE, labels = unique(d$b), series = 0:2, autoplay = TRUE, updateInterval = 1) %>%
    hc_xAxis(min = 0) %>%
    hc_yAxis(min = 0) %>%
    hc_add_series_list(series)




  highchart() %>%
    hc_chart(type = "column") %>%
    hc_motion(enabled = TRUE, labels = d$b, series = 0:2, autoplay = TRUE, updateInterval = 1) %>%
    hc_xAxis(min = 0) %>%
    hc_yAxis(min = 0) %>%
    hc_add_series_list(series)

  hchart(d, type = "bubble", x = c, y = d, color = a) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle)) %>%
    hc_plotOptions(
      series = list(dataLabels = list(enabled = TRUE,format= '{point.a}'))
    ) %>%
    hc_motion(enabled = TRUE,
              #labels = 2000:2003,
              series = c(0,1,2))



  highchart() %>%
    hc_chart(type = "bubble") %>%
    hc_add_series(name = "B",
                  data = list(
                    list(sequence = list(c(1,2),c(3,4))),
                    list(sequence = list(c(1,2),c(3,4)))
                  )) %>%
    hc_add_series(name = "C",
                  data = list(
                    list(sequence = list(c(1,2),c(3,4))),
                    list(sequence = list(c(1,2),c(3,4)))
                  )) %>%
    hc_motion(enabled = TRUE,
              #labels = 2000:2003,
              series = c(0,1))



  highchart() %>%
    hc_chart(type = "bubble") %>%
    hc_add_series(name = "B",
                  data = list(
                    list(sequence = c(1,2,3,4)),
                    list(sequence = c(3,2,1,3)),
                    list(sequence = c(2,5,4,3))
                  )) %>%
    hc_add_series(name = "C",
                  data = list(
                    list(sequence = c(3,2,1,3)),
                    list(sequence = c(2,5,4,3)),
                    list(sequence = c(1,2,3,4))
                  )) %>%
    hc_motion(enabled = TRUE,
              labels = 2000:2003,
              series = c(1,2))


}

####


#' @export
plotLeaflet <- function(d,selectedVar,scope = "comparada"){
  # scope
  path <- paste0("data/aux/geoPoints-",scope,".csv")
  if(scope == "comparada"){
    bounds <- c(-76,0,-71,8)
    geoVar <- "municipio"
    dgeoVar <- "Ciudad"
  }
  if(scope == "bogota"){
    bounds <- c(-74.18, 4.42, -74.03, 4.75)
    geoVar <- "name"
    dgeoVar <- "Localidad"
  }
  if(all(is.na(d$value))){
    warning("all NA for selected var")
    leaf <- leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
    return(leaf)
  }
  geo <- read_csv(sysfile(path))

  vars <- c(dgeoVar,"value")
  selectedVarName <- names(selectedVar)
  dd <- d %>% select_(.dots = vars)
  names(geoVar) <- dgeoVar
  dd <- dd %>% left_join(geo[c(geoVar,"latitud","longitud")],geoVar)
  #dd <- na.omit(dd)
  #if(nrow(dd) == 0) return()
  dd <- dd %>% filter(!is.na(latitud))
  dd$info <- as.vector(pystr_format(pystr_format("{{dgeoVar}}<br>{selectedVarName}: {value}",
                                                 list(selectedVarName = selectedVarName, dgeoVar = dgeoVar))
                                    ,dd))
  dd <- dd %>% filter(!is.na(value))
  leaflet(dd) %>%
    #addTiles() %>%
    addProviderTiles("CartoDB.Positron") %>%
    fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
    addCircleMarkers(lng = ~longitud, lat = ~latitud, weight = 3,
                     radius = ~rescale(sqrt(value), to = c(5,20)),
                     popup = ~info
    )
}

#' @export
plotChrono <- function(dd, numCol, catCol, yearCol,
                       type = "column", title = "", xAxisTitle = "", yAxisTitle = ""){
  dd <- dd %>% rename_("value"= numCol,"category"= catCol, "year" = yearCol)
  dd <- dd %>% filter(!is.na(value))
  hchart(dd, type = type, x = year, y = value, group = category) %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))
}

#' @export
plotTreemap <- function(dd,  numCol, catCol,
                        type = "column", title = "", xAxisTitle = "", yAxisTitle = ""){
  dd <- dd %>% rename_("value"= numCol,"category"= catCol)
  dd <- dd %>% filter(!is.na(value))
  hchart(dd, "treemap", x = category, value = value, color = value) %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))
}


#' @export
plotScatter <- function(dd,num1Col,num2Col,catCol,
                        title = "",
                        xAxisTitle = "", yAxisTitle = ""){
  dd <- dd %>% rename_("v1"= num1Col,"v2"= num2Col,"category"= catCol) %>% mutate(label = category)
  dd <- dd %>% filter(!is.na(v1),!is.na(v2))
  hchart(dd, "bubble", x = v1, y = v2, color=category) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle)) %>%
    hc_plotOptions(
      series = list(dataLabels = list(enabled = TRUE,format= '{point.label}'))
    )

}







#####

#' @export
plotLeafletCol <- function(d){
  leaflet(d) %>%
    #addTiles() %>%
    addProviderTiles("CartoDB.Positron") %>%
    fitBounds(-76, 0, -71, 8.5) %>%
    addCircleMarkers(lng = ~longitud, lat = ~latitud, weight = 3,
                     radius = ~rescale(sqrt(value), to = c(5,20)),
                     popup = ~info
    )
}

#' @export
plotLeafletBog <- function(d){
  leaflet(d) %>%
    #addTiles() %>%
    addProviderTiles("CartoDB.Positron") %>%
    fitBounds(-74.18, 4.42, -74.03, 4.75) %>%
    addCircleMarkers(lng = ~longitud, lat = ~latitud, weight = 3,
                     radius = ~rescale(sqrt(value), to = c(5,20)),
                     popup = ~info
    )
}


#' @export
plotChronoCities <- function(dd, type = "column", title = NULL){
  highchart() %>%
    hc_chart(type = type) %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_title(text = title) %>%
    hc_xAxis(categories = dd$Anio) %>%
    hc_add_series(data = dd[,"Barranquilla"][[1]], name = "Barranquilla") %>%
    hc_add_series(data = dd[,"Bogotá"][[1]], name = "Bogotá") %>%
    hc_add_series(data = dd[,"Bucaramanga Metropolitana"][[1]], name = "Bucaramanga Metropolitana") %>%
    hc_add_series(data = dd[,"Cali"][[1]], name = "Cali") %>%
    hc_add_series(data = dd[,"Cartagena"   ][[1]], name = "Cartagena"   ) %>%
    hc_add_series(data = dd[,"Medellín"][[1]], name = "Medellín") %>%
    hc_add_series(data = dd[,"Ibagué"][[1]], name = "Ibagué") %>%
    hc_add_series(data = dd[,"Manizales"][[1]], name = "Manizales") %>%
    hc_add_series(data = dd[,"Pereira"][[1]], name = "Pereira") %>%
    hc_add_series(data = dd[,"Valledupar"  ][[1]], name = "Valledupar"  ) %>%
    hc_add_series(data = dd[,"Yumbo"][[1]], name = "Yumbo")
}

#' @export
plotChronoLocalidades <- function(dd, type = "column", title = NULL){
  highchart() %>%
    hc_chart(type = type) %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_title(text = title) %>%
    hc_xAxis(categories = dd$Anio) %>%
    hc_add_series(data = dd[,"Usaquén"][[1]], name = "Usaquén") %>%
    hc_add_series(data = dd[,"Chapinero"][[1]], name = "Chapinero") %>%
    hc_add_series(data = dd[,"Santa Fe"][[1]], name = "Santa Fe") %>%
    hc_add_series(data = dd[,"San Cristóbal"][[1]], name = "San Cristóbal") %>%
    hc_add_series(data = dd[,"Usme"][[1]], name = "Usme") %>%
    hc_add_series(data = dd[,"Tunjuelito"][[1]], name = "Tunjuelito") %>%
    hc_add_series(data = dd[,"Bosa"][[1]], name = "Bosa") %>%
    hc_add_series(data = dd[,"Kennedy"][[1]], name = "Kennedy") %>%
    hc_add_series(data = dd[,"Fontibón"][[1]], name = "Fontibón") %>%
    hc_add_series(data = dd[,"Engativá"][[1]], name = "Engativá") %>%
    hc_add_series(data = dd[,"Suba"][[1]], name = "Suba") %>%
    hc_add_series(data = dd[,"Barrios Unidos"][[1]], name = "Barrios Unidos") %>%
    hc_add_series(data = dd[,"Teusaquillo"][[1]], name = "Teusaquillo") %>%
    hc_add_series(data = dd[,"Los Mártires"][[1]], name = "Los Mártires") %>%
    hc_add_series(data = dd[,"Antonio Nariño"][[1]], name = "Antonio Nariño") %>%
    hc_add_series(data = dd[,"Puente Aranda"][[1]], name = "Puente Aranda") %>%
    hc_add_series(data = dd[,"La Candelaria"][[1]], name = "La Candelaria") %>%
    hc_add_series(data = dd[,"Rafael Uribe Uribe"][[1]], name = "Rafael Uribe Uribe") %>%
    hc_add_series(data = dd[,"Ciudad Bolívar"][[1]], name = "Ciudad Bolívar") %>%
    hc_add_series(data = dd[,"Sumapaz"][[1]], name = "Sumapaz")
}



# plotChoropleth <- function(dd,selvars, selregion, scale = 2){
#   mapName <- "co_municipalities"
#   opts <- list(
#     defaultFill = "#FFFFFF",
#     borderColor = "#CCCCCC",
#     borderWidth = 0.3,
#     highlightFillColor = "#999999",
#     #highlightBorderColor = "#0000FF",
#     highlightBorderWidth = 1,
#     #legend = TRUE,
#     #legendTitle = "Grupo",
#     #legendDefaultFillTitle = "No hay datos",
#     palette = "PuBu",
#     choroLegend = list(shapeWidth = 40, label = selvars),
#     #choroLegend = list(show = TRUE),
#     projectionOpts = list(
#       scale = scale
# #       center = c(input$centerX,input$centerY),
# #       translate = c(input$translateX,input$translateY),
# #       rotate = c(input$rotateX,input$rotateY),
# #       distance = input$distance %||% 1,
# #       clipAngle = input$clipAngle %||% 90,
# #       tilt = input$tilt %||% 60
#     )
#   )
#   message(opts$projectionOpts$scale)
#   dd$info <- paste(dd$municipio," - ",dd$departamento,", <br>",
#                   selvars,": <strong>",dd[,selvars],"</strong>")
#   dd$code <- sprintf("%05d", dd$code)
#   dmaps(mapName, data = dd,
#         valueCol = selvars,
#         codeCol = "code",
#         regions = selregion,
#         opts = opts)
# }


# plotChoropleth2d <- function(d,selvars, selregion, scale = 2){

#   var1 <- cut2(d[,selvars[1]],g=3)
#   levels(var1) <- c("x1","x2","x3")
#   var2 <- cut2(d[,selvars[2]],g=3)
#   levels(var2) <- c("y1","y2","y3")
#   d$group <- paste(var1,var2,sep="")
#   groups2d <- apply(expand.grid(paste0("x",1:3),paste0("y",1:3)),1,
#                     function(r)paste0(r[1],r[2]))
#   colors2d <- c("#e8e8e8","#e4acac","#c85a5a","#b0d5df","#ad93a5","#985356","#64acbe","#62718c","#574249")
#   customPalette <- data.frame(group = groups2d, color = colors2d)

#   opts <- list(
#     defaultFill = "#FFFFFF",
#     borderColor = "#CCCCCC",
#     borderWidth = 0.3,
#     highlightFillColor = "#999999",
#     highlightBorderWidth = 1,
#     palette = "PuBu",
#     customPalette = customPalette,
#     choroLegend = list(show = FALSE),
#     bivariateLegend = list(show = TRUE,
#                            var1Label = unname(selvars[1]),
#                            var2Label = unname(selvars[2])),
#     #projectionOpts = regionOpts[[selregion]]
#     projectionOpts = list(
#       scale = scale
#       #       center = c(input$centerX,input$centerY),
#       #       translate = c(input$translateX,input$translateY),
#       #       rotate = c(input$rotateX,input$rotateY),
#       #       distance = input$distance %||% 1,
#       #       clipAngle = input$clipAngle %||% 90,
#       #       tilt = input$tilt %||% 60
#     )
#   )
#   d$info <- paste(d$municipio," - ",d$departamento,", <br>",
#                   selvars[1],": <strong>",d[,selvars[1]],"</strong><br>",
#                   selvars[2],": <strong>",d[,selvars[2]],"</strong>")
#   d$code <- sprintf("%05d", d$code)
#   mapName <- "co_municipalities"

#   dmaps(mapName, data = d,
#         groupCol = "group",
#         codeCol = "code",
#         regions = selregion,
#         opts = opts)

# }


