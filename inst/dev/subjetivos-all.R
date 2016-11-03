library(devtools)
load_all()

document()
#install()

### LOAD DATA

dbInfo <- list_fringe()
dbInfo <- list_fringe(groups = "subjetivos")
dbs <- unique(dbInfo$group)


frs <- load_fringes(n_max = 10000)
#frs <- load_fringes()
#write_rds(frs, "inst/data/fringe/fringes.rds",compress = "gz")
frs <- read_rds("inst/data/fringe/fringes.rds")
names(frs)




## VIZ COMPARADAS

selectedTable <- "subjetivos/subjetivos-comparada"
f0 <- frs[[selectedTable]]

ciudadVar <- "CIUDAD"
yearVar <- "AÑO"

### GEO

f <- f0
varsNu <- selectDicCtypes(f,"Nu",as_list = TRUE)
selectedVarNu <- flatten_chr(sample(varsNu,1))
selectedVarNu
f <- selectFringeCols(f,c(ciudadVar,yearVar,selectedVarNu))

availableYears <- f$d %>% group_by(b) %>%
  summarize(undefined = all(is.na(c))) %>%
  filter(!undefined) %>% .$b %>% sort(decreasing = TRUE)
selectedYear <- sample2(availableYears,1)

f <- keepFringeRows(f,yearVar,selectedYear) %>% selectFringeCols(c(ciudadVar,selectedVarNu))
lflt_co_mun(f)

## CHRONO
f <- f0
varsNu <- selectDicCtypes(f,"Nu",as_list = TRUE)
selectedVarNu <- flatten_chr(sample(varsNu,1))
selectedVarNu

f <- selectFringeCols(f,c(ciudadVar,yearVar ,selectedVarNu))
d <- f$d
hgch_bar_cyn(f)
hgch_lines_cyn(f)

# RANKINGS
f <- f0
varsNu <- selectDicCtypes(f,"Nu",as_list = TRUE)
selectedVarNu <- flatten_chr(sample(varsNu,1))
selectedVarNu
f <- selectFringeCols(f,c(ciudadVar,yearVar,selectedVarNu))

availableYears <- f$d %>% group_by(b) %>%
  summarize(undefined = all(is.na(c))) %>%
  filter(!undefined) %>% .$b %>% sort(decreasing = TRUE)
selectedYear <- sample(availableYears,1)

f <- keepFringeRows(f,yearVar,selectedYear) %>%  selectFringeCols(c(ciudadVar,selectedVarNu))

hgch_treemap(f)
hgch_bar_top(f)


# SCATTER

f <- f0
varsNu <- selectDicCtypes(f,"Nu",as_list = TRUE)
selectedVarNu <- flatten_chr(sample(varsNu,2))
selectedVarNu
f <- selectFringeCols(f,c(ciudadVar,yearVar,selectedVarNu))

availableYears <- f$d %>% group_by(b) %>%
  summarize(undefined = all(is.na(c))) %>%
  filter(!undefined) %>% .$b %>% sort(decreasing = TRUE)
selectedYear <- sample(availableYears,1)

f <- f %>% keepFringeRows(yearVar,selectedYear) %>%  selectFringeCols(c(ciudadVar,selectedVarNu))
hgch_scatter(f)





### VIZ CIUDADES
yearVar <- "AÑO"

availableTables <- dbInfo$id[!grepl("comparada",dbInfo$id)]
names(availableTables) <- dbInfo$label[!grepl("comparada",dbInfo$id)]

selectedTable <- sample(availableTables,1)
f0 <- frs[[selectedTable]]

varsNu <- selectDicCtypes(f0,"Nu",as_list = TRUE)
varsCa <- selectDicCtypes(f0,"Ca",as_list = TRUE)

# Ye-Nu
selectedVarNu <- sample(varsNu,1)
selectedCols <- c("AÑO",flatten_chr(c(selectedVarNu)))
f <- selectFringeCols(f0,selectedCols)
d <- f$d
getClabels(f)
hgch_multilines_ynp(f)
hgch_multilines_ynp(f,type = "line")

# Ye-Nu-Nu
selectedVarNu <- sample(varsNu,2)
selectedCols <- c("AÑO",flatten_chr(c(selectedVarNu)))
f <- selectFringeCols(f0,selectedCols)
d <- f$d
getClabels(f)
hgch_multilines_ynp(f)
hgch_multilines_ynp(f,type = "line")



# CN -- for a fixed year

selectedVarNu <- flatten_chr(sample(varsNu,1))
selectedVarCa <- flatten_chr(sample(varsCa,1))

f <- selectFringeCols(f0,c(yearVar,selectedVarNu, selectedVarCa))
f$d
f <- f %>% keep_not_na_FringeRows()
availableYears <- f$d %>% .$a %>% unique()
selectedYear <- sample2(availableYears,1)

f <- keepFringeRows(f,"AÑO",selectedYear) %>%
  selectFringeCols(c(selectedVarCa,selectedVarNu))
d <- f$d
hgch_bar_cn(f)


# C
selectedVarCa <- flatten_chr(sample(varsCa,1))

f <- selectFringeCols(f0,c(yearVar,selectedVarCa)) %>%
  keep_not_na_FringeRows()
availableYears <- f$d %>% .$a %>% unique()
selectedYear <- sample2(availableYears,1)

f <- keepFringeRows(f,"AÑO",selectedYear) %>%
  selectFringeCols(c(selectedVarCa))
d <- f$d
getClabels(f)
hgch_bar_c(f)


# Ca-Ye ... bars 100%






