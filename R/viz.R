#'
#' @export
lflt_co_mun <- function(fgeo){
  bounds <- c(-76,0,-71,10)
  geoVar <- "Ciudad"
  path <- paste0("data/aux/geoPoints-","comparada",".csv")
  geo <- read_csv(sysfile(path)) %>% rename(a = municipio)
  varLabel <- getClabels(fgeo)[2]
  ### SACA EL PROMEDIO POR DEFECTO SI HAY Ca REPETIDOS
  dgeo <- fgeo$d %>% na.omit() %>% group_by(a) %>% summarise(b = mean(b))
  d <- dgeo %>% left_join(geo[c("a","latitud","longitud")],"a")
  d$info <- pystr_format("{a}<br>{selectedVarName}: {b}",d) %>% pystr_format(list(selectedVarName = varLabel))
  dd <- d %>% filter(!is.na(b))
  leaflet(dd) %>%
    #addTiles() %>%
    addProviderTiles("CartoDB.Positron") %>%
    fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
    addCircleMarkers(lng = ~longitud, lat = ~latitud, weight = 3,
                     radius = ~rescale(sqrt(b), to = c(5,20)),
                     popup = ~info
    )
}

#' hgch_multilines
#' @name hgch_multilines
#' @export
#' @section ftype: Ye-Nu*
hgch_multilines <- function(fchrono, type = "column",
                            title = NULL, xAxisTitle = NULL, yAxisTitle = NULL){
  xAxisTitle <- xAxisTitle %||% getClabels(fchrono)[1]
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% ""
  d <- fchrono$d %>% gather(variable,value, -a) %>%
    filter(!is.na(value))
  codes <- data_frame(variable = letters[1:ncol(fchrono$d)], to = getClabels(fchrono))
  d <- d %>%
    mutate(variable = fct_recode_df(d,"variable",codes))
  hchart(d, type = type, x = a, y = value, group = variable) %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))
}

#' hgch_multilines_ynp
#' @name hgch_multilines_ynp
#' @export
#' @section ftype: Ye-Nu*
hgch_multilines_ynp <- function(f, type = "column",
                            title = NULL, xAxisTitle = NULL, yAxisTitle = NULL){
  xAxisTitle <- xAxisTitle %||% getClabels(f)[1]
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% ""
  d <- f$d %>% gather(variable,value, -a) %>%
    filter(!is.na(value)) %>% group_by(a,variable) %>%
    summarise(value = mean(value)) %>% ungroup()
  codes <- data_frame(variable = letters[1:ncol(f$d)], to = getClabels(f))
  d <- d %>%
    mutate(variable = fct_recode_df(d,"variable",codes))
  hchart(d, type = type, x = a, y = value, group = variable) %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))
}

#' hgch_bar_cyn
#' @name hgch_bar_cyn
#' @export
#' @section ftype: Ca-Ye-Nu
hgch_bar_cyn <- function(f, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL){
  xAxisTitle <- xAxisTitle %||% getClabels(f)[2]
  yAxisTitle <- yAxisTitle %||% getClabels(f)[3]
  title <-  title %||% ""
  d <- f$d %>% na.omit() %>% group_by(a,b) %>% summarise(c = mean(c))
  if(nrow(d)==0) return()
  #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
  hchart(d, type = "column", x = b, y = c, group = a) %>%
    #hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))
}

#' hgch_lines_cyn
#' @name hgch_lines_cyn
#' @export
#' @section ftype: Ca-Ye-Nu
hgch_lines_cyn <- function(f, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL){
  xAxisTitle <- xAxisTitle %||% getClabels(f)[2]
  yAxisTitle <- yAxisTitle %||% getClabels(f)[3]
  title <-  title %||% ""
  d <- f$d %>% na.omit() %>% group_by(a,b) %>% summarise(c = mean(c))
  if(nrow(d)==0) return()
  #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
  hchart(d, type = "line", x = b, y = c, group = a) %>%
    #hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))
}

#' hgch_treemap
#' @name hgch_treemap
#' @export
#' @section ftype: Ca-Nu
hgch_treemap <- function(f, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL){
  xAxisTitle <- xAxisTitle %||% getClabels(f)[1]
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% getClabels(f)[2]
  d <- f$d %>% na.omit() %>% group_by(a) %>% summarise(b = mean(b))
  hchart(d, "treemap", x = a, value = b, color = b) %>%
    hc_title(text = title)
}

#' hgch_bar_top
#' @name hgch_bar_top
#' @export
#' @section ftype: Ca-Nu
hgch_bar_top <- function(f, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL){
  xAxisTitle <- xAxisTitle %||% ""
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% getClabels(f)[2]
  d <- f$d
  if(nrow(d)==0) return()
  d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
  hchart(d, type = "bar", x = a, y = b) %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))
}

#' hgch_bar_cn
#' @name hgch_bar_cn
#' @export
#' @section ftype: Ca-Nu
hgch_bar_cn <- function(f, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL){
  xAxisTitle <- xAxisTitle %||% getClabels(f)[1]
  yAxisTitle <- yAxisTitle %||% getClabels(f)[2]
  title <-  title %||% ""
  d <- f$d
  d <- na.omit(d)
  if(nrow(d)==0) return()
  d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
  hchart(d, type = "bar", x = a, y = b) %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))
}

#' hgch_bar_c
#' @name hgch_bar_c
#' @export
#' @section ftype: Ca-Nu
hgch_bar_c <- function(f, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL){
  xAxisTitle <- xAxisTitle %||% getClabels(f)[1]
  yAxisTitle <- yAxisTitle %||% getClabels(f)[2]
  title <-  title %||% ""
  d <- f$d
  if(nrow(d)==0) return()
  d <- d %>% group_by(a) %>% summarise(b = n()) %>% arrange(desc(b))
  hchart(d, type = "bar", x = a, y = b) %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))
}

#' hgch_scatter
#' @name hgch_scatter
#' @export
#' @section ftype: Ca-Nu-Nu
hgch_scatter <- function(f, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL){
  xAxisTitle <- xAxisTitle %||% getClabels(f)[2]
  yAxisTitle <- yAxisTitle %||% getClabels(f)[3]
  title <-  title %||% ""
  d <- f$d %>% filter(!is.na(b),!is.na(c)) %>% group_by(a) %>% summarise(b = mean(b),c = mean(c))
  hchart(d, type = "bubble", x = b, y = c, color = a) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle)) %>%
    hc_plotOptions(
      series = list(dataLabels = list(enabled = TRUE,format= '{point.a}'))
    )
}



#' @export
vizList <- function(){
  db <- Rd_db("ciudatos")
  meta <- unname(map_chr(db, tools:::.Rd_get_name))
  keep(meta, ~ grepl("^hgch_.*$",.))
}


#' @export
hgchFtype <- function(hgch = NULL){
  db <- Rd_db("ciudatos")
  db <- db[grepl("^hgch_.*$",names(db))]
  meta <- lapply(db, tools:::.Rd_get_section, "section")
  cleanFtypeDoc <- function(ftype){
    ftype <- as.character(ftype[[2]][[2]])
    strsplit(gsub(" |\n","",ftype),",")[[1]]
  }
  meta <- lapply(meta,cleanFtypeDoc)
  names(meta) <- gsub(".Rd","",names(meta))
  if(!is.null(hgch)) return(meta[[hgch]])
  meta
}

