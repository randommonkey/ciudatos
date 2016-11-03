library(devtools)
load_all()
document()
install()
# test()

## Objetivos

d0 <- read_csv(sysfile("data/educacion/objetivos-edu-data.csv"))


library(leaflet)
library(scales)

library(lazyeval)

names(d0)

selectedYear <- 2011
selectedVar <- "v1_d_001"

fixedVars <- c("Ciudad")
vars <- c(fixedVars,selectedVar)

d <- d0 %>% filter(Anio == selectedYear) %>% select_(.dots = vars) %>% rename_("value"=selectedVar)

geo <- read_csv(sysfile("data/aux/geoPoints.csv"))

d <- d %>% left_join(geo[c("municipio","latitud","longitud")],c("Ciudad" = "municipio"))
d$info <- paste("hola", d$Ciudad, d$value)

plotLeafletCol(d)


library(highcharter)

fixedVars <- c("Ciudad","Anio")
vars <- c(fixedVars,selectedVar)
d <- d0 %>% select_(.dots = vars)  %>% rename_("value"=selectedVar)
title <- selectedVar
dd <- spread(d,Ciudad,value)

plotChronoCities(dd)

plotChronoCities(dd, type = "line")


hchart(d, "column", x = Anio, y = value, group = Ciudad) %>% hc_plotOptions(column = list(
  #dataLabels = list(enabled = FALSE),
  #enableMouseTracking = FALSE,
  stacking = "normal")
)
hchart(d, "line", x = "Anio", y = "value", group = "Ciudad")
hchart(d, "line", x = d$Anio, y = d$value, group = d$Ciudad)



###
fh <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_plotOptions(column = list(stacking = "normal")) %>%
  hc_title(text = selectedVar)

cities <- unique(d$Ciudad)
l <- lapply(cities, function(c){
  hc_add_series(fh,data = dd[,c][[1]], name = c)
})

Compose <- function(...) {
  fs <- list(...)
  function(...) Reduce(function(x, f) f(x),
                       fs,
                       ...)
}

Compose(l)

l[[1]](l[[2]])

str(l[[1]])


Curry <- function(FUN,...) {
  .orig = list(...);
  function(...) do.call(FUN,c(.orig,list(...)))
}

Curry(l)


Reduce(`%>%`,l)




