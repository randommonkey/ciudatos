library(devtools)
load_all()
document()
install()

dictBog <- read_csv(sysfile("data/original/objetivos-bog-dict.csv"))
numVars <- grepl("^[[:digit:]]",dictBog$Variable)

tmp <- dictBog %>% group_by(Variable) %>% summarise(n = n())

x <- dictBog %>% filter( `Capitulo (Bogotá)`  =="4. Educación") %>%
  mutate(Variable = paste0("v",gsub("-","_",Variable))) %>% bind_rows(dictBog[1:3,],.)

write_csv(x,"inst/data/educacion/objetivos-bog-edu-dict.csv")


d0 <- read_csv(sysfile("data/original/objetivos-bog-data.csv"))
names(d0)[numVars] <- paste0("v",gsub("-","_",names(d0)[numVars]))

d <- d0[x$Variable]
d[d=="."] <- NA

write_csv(d,"inst/data/educacion/objetivos-bog-edu-data.csv")
