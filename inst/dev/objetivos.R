
library(devtools)
load_all()

document()
#install()

d0 <- read_csv(sysfile("data/clean/objetivos/bogota/objetivos-bogota-localidades.csv"))
problems(d0)
dic <- read_csv(sysfile("data/clean/objetivos/bogota/objetivos-bogota-localidades-dic.csv"))
problems(d0)


library(leaflet)
library(scales)

library(lazyeval)

names(d0)

selectedYear <- 2011
names(d0)

numVars <- dic %>% filter(ctype == "Nu") %>% .$id
names(numVars) <- dic %>% filter(ctype == "Nu") %>% .$name

selectedVar <- sample(numVars,1)
selectedVarName <- names(selectedVar)
fixedVars <- c("Localidad")
vars <- c(fixedVars,unname(selectedVar))

nm <- unname(selectedVar)
d <- d0 %>%
  filter(Localidad != "Total Bogotá") %>%
  filter(Anio == selectedYear) %>% select_(.dots = vars) %>%
  rename_("value"= nm)

geo <- read_csv(sysfile("data/aux/geoPoints-bogota.csv"))

d <- d %>% left_join(geo[c("name","latitud","longitud")],c("Localidad" = "name"))
d$info <- pystr_format(pystr_format("{selectedVarName}: {value}",
                                     list(selectedVarName = selectedVarName))
                        ,d)
plotLeafletBog(d)

##

library(highcharter)

fixedVars <- c("Localidad","Anio")
vars <- c(fixedVars,unname(selectedVar))
d <- d0 %>% select_(.dots = vars) %>% filter(Localidad != "Total Bogotá") %>% rename_("value"=selectedVar)
#d <- na.omit(d)
title <- selectedVar
dd <- spread(d,Localidad,value)

plotChronoLocalidades(dd)
plotChronoLocalidades(dd, type = "line")

## Gapminder

selectedYear <- 2013

selectedVars <- sample(numVars,3)
# Empresas canceladas
# "v4_ee_005"
# Matricula Privada primera infancia total
# "v4_e_028"
# Ocupaciones ilegales identificadas
# "v7_v_054"
selectedVarNames <- names(selectedVars)
nms <- unname(selectedVars)
fixedVars <- c("Localidad","Anio")
vars <- c(fixedVars,unname(selectedVars))
category <- "Localidad"
d <- d0 %>% select_(.dots = vars) %>%
  filter(Anio == selectedYear) %>%
  filter(Localidad != "Total Bogotá") %>%
  rename_("v1"=nms[1]) %>%
  rename_("v2"=nms[2]) %>%
  rename_("v3"=nms[3]) %>%
  mutate(v1=as.numeric(v1)) %>%
  mutate(v2=as.numeric(v2)) %>%
  mutate(v3=as.numeric(v3))

#d <- na.omit(d)
any(map(d,is.na) %>% map_lgl(all))

hchart(d, "bubble", x = v1, y = v3, size = v2, color=category) %>%
  hc_xAxis(title = list(text=selectedVarNames[1])) %>%
  hc_yAxis(title = list(text=selectedVarNames[2])) %>%
  hc_plotOptions(
    series = list(dataLabels = list(enabled = TRUE,format= '{point.Localidad}'))
  )





# %>%
#   hc_motion(enabled = TRUE,
#             labels = 2000:2003,
#             series = c(0,1,2))
#   hc_legend(enabled= TRUE)
#   hc_colorAxis(categories = d$Localidad) %>%
#   #hc_yAxis(type = "logarithmic") %>%
#   #hc_title(text = "Our nearest Stars") %>%
#   #hc_subtitle(text = "In a Hertzsprung-Russell diagram") %>%
#   hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = tltip)


hc_legend()

dd <- spread(d,Localidad,value)








