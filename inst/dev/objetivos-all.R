library(devtools)
load_all()

document()
#install()

library(ciudatos)

####

dbInfo <- list_fringe()
dbInfo <- list_fringe(groups = "subjetivos")
dbs <- unique(dbInfo$group)

frs <- load_fringes(n_max = 1000)
#frs <- load_fringes()
#write_rds(frs, "inst/data/fringe/fringes.rds",compress = "gz")
#frs <- read_rds("inst/data/fringe/fringes.rds")
names(frs)

## VIZ COMPARADAS
selectedTable <- "objetivos/objetivos-comparada"
f0 <- frs[[selectedTable]]

ciudadVar <- "Ciudad"
yearVar <- "Anio"

### GEO
f <- f0
varsNu <- selectDicCtypes(f,"Nu",as_list = TRUE)
selectedVarNu <- flatten_chr(sample(varsNu,1))
selectedVarNu
f <- selectFringeCols(f,c(ciudadVar,yearVar,selectedVarNu))

availableYears <- f$d %>% group_by(b) %>%
  summarize(undefined = all(is.na(c))) %>%
  filter(!undefined) %>% .$b %>% sort(decreasing = TRUE)
selectedYear <- sample(availableYears,1)

f <- keepFringeRows(f,yearVar,selectedYear) %>% selectFringeCols(c(ciudadVar,selectedVarNu))
lflt_co_mun(f)





selectedTable <- availableTables[2]
page <- "geo"

f <- frs[[selectedTable]]
varsNu <- selectDicCtypes(f,"Nu",as_list = TRUE)
selectedVarNu <- flatten_chr(sample(varsNu,1))
selectedVarNu
fgeo <- selectFringeCols(f,c("Ciudad","Anio",selectedVarNu))

availableYears <- fgeo$d %>% group_by(b) %>%
  summarize(undefined = all(is.na(c))) %>%
  filter(!undefined) %>% .$b %>% sort(decreasing = TRUE)

selectedYear <- sample(availableYears,1)

fgeo <- keepFringeRows(fgeo,"Anio",selectedYear)
lflt_co_mun(fgeo)


## CHRONO
selectedTable <- availableTables[1]
f <- frs[[selectedTable]]
varsNu <- selectDicCtypes(f,"Nu",as_list = TRUE)
selectedVarNu <- flatten_chr(sample(varsNu,3))
selectedVarNu
#fchrono <- selectFringeCols(f,c("Anio" ,selectedVarNu))
#hgch_multilines(fchrono, type = "line")

fchrono <- selectFringeCols(f,c("Ciudad","Anio" ,flatten_chr(selectedVarNu)))
hgch_bar_cyn(fchrono)


# RANKINGS
selectedTable <- availableTables[2]
f <- frs[[selectedTable]]
varsNu <- selectDicCtypes(f,"Nu",as_list = TRUE)
selectedVarNu <- flatten_chr(sample(varsNu,1))
selectedVarNu
frank <- selectFringeCols(f,c("Ciudad","Anio",selectedVarNu))

availableYears <- frank$d %>% group_by(b) %>%
  summarize(undefined = all(is.na(c))) %>%
  filter(!undefined) %>% .$b %>% sort(decreasing = TRUE)
selectedYear <- sample(availableYears,1)

frank <- keepFringeRows(frank,"Anio",selectedYear) %>%  selectFringeCols(c("Ciudad",selectedVarNu))

hgch_treemap(frank)
hgch_bar_top(frank)

# SCATTER

selectedTable <- availableTables[2]
f <- frs[[selectedTable]]
varsNu <- selectDicCtypes(f,"Nu",as_list = TRUE)
selectedVarNu <- flatten_chr(sample(varsNu,2))
selectedVarNu
fgap <- selectFringeCols(f,c("Ciudad","Anio",selectedVarNu))

availableYears <- fgap$d %>% group_by(b) %>%
  summarize(undefined = all(is.na(c))) %>%
  filter(!undefined) %>% .$b %>% sort(decreasing = TRUE)
selectedYear <- sample(availableYears,1)

fgap <- fgap %>% keepFringeRows("Anio",selectedYear) %>%  selectFringeCols(c("Ciudad",selectedVarNu))
hgch_scatter(fgap)


# GAPMINDER

selectedTable <- availableTables[2]
f <- frs[[selectedTable]]
varsNu <- selectDicCtypes(f,"Nu",as_list = TRUE)
selectedVarNu <- flatten_chr(sample(varsNu,2))
selectedVarNu
fgap <- selectFringeCols(f,c("Ciudad","Anio",selectedVarNu))

hgch_gapminder(fgap)











