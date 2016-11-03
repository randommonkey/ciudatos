library(devtools)
load_all()
document()
#install()
# test()


dbs <- read_csv(sysfile("data/clean/dbs.csv"))

objetivos <- dbs %>% filter(type == "objetivos")

## Add ctype to dic
addCtypes <- function(f){
  #f <- objetivos$path[1]
  dic <- read_csv(sysfile(gsub(".csv","-dic.csv",f)))
  d <- read_csv(sysfile(f))
  dic$ctype <- guessCtypes(d)
  dic$ctype[dic$id == "Anio"] <- "Ye"
  write_csv <- write_csv(dic,sysfile(gsub(".csv","-dic.csv",f)))
}
map(objetivos$path,addCtypes)

loadDb <- function(f){
  #f <- objetivos$path[1]
  dic <- read_csv(sysfile(gsub(".csv","-dic.csv",f)))
  d <- read_csv(sysfile(f))
  list(data = d, dic = dic)
}
objs <- map(objetivos$path,loadDb)
names(objs) <- objetivos$id
saveRDS(objs,"inst/data/clean/objetivos.rds")

