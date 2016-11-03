library(devtools)
load_all()
document()
install()
# test()

## Objetivos

d0 <- read_csv(sysfile("data/educacion/objetivos-bog-edu-data.csv"))

library(leaflet)
library(scales)

library(lazyeval)

names(d0)

selectedYear <- 2011
names(d0)
selectedVar <- "v04_e_014"

fixedVars <- c("Localidad")
vars <- c(fixedVars,selectedVar)

d <- d0 %>%
  filter(Localidad != "Total Bogotá") %>%
  filter(Anio == selectedYear) %>% select_(.dots = vars) %>% rename_("value"=selectedVar)
geo <- read_csv(sysfile("data/aux/geoPointsLocalidades.csv"))

d <- d %>% left_join(geo[c("name","latitud","longitud")],c("Localidad" = "name"))
d$info <- paste(d$Localidad,selectedVar,d$value)


plotLeafletBog(d)

library(highcharter)

fixedVars <- c("Localidad","Anio")
vars <- c(fixedVars,selectedVar)
d <- d0 %>% select_(.dots = vars) %>% filter(Localidad != "Total Bogotá") %>% rename_("value"=selectedVar)
d <- na.omit(d)
title <- selectedVar
dd <- spread(d,Localidad,value)

plotChronoLocalidades(dd)

plotChronoLocalidades(dd, type = "line")






