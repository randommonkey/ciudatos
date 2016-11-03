library(devtools)
load_all()
document()
#install()
# test()

dbog <- read_csv("inst/data/fringe/subjetivos/subjetivos-bogota-data.csv", col_types = cols(.default = "c"))
dbogDic <- read_csv("inst/data/fringe/subjetivos/subjetivos-bogota-dic.csv")
ctypes <- guessCtypes(dbog)
dbogDic$ctype <- ctypes
write_csv(dbogDic[c(1,3,2)],"inst/data/fringe/subjetivos/subjetivos-bogota-dic_.csv")



