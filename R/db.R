
#' @export
list_fringe <- function(groups = NULL){
  dbs <- read_csv(sysfile("data/fringe/dbs.csv"))  %>% filter(!is.na(id))
  groups <- groups %||% unique(dbs$group)
  dbs <- dbs %>% filter(group %in% groups)
  fs <- list.files(sysfile("data/fringe"),recursive = TRUE)
  dbFiles <- dbs %>% select(id,withDic,group) %>%
    mutate(data = paste0(id,"-data.csv"), dic = paste0(id,"-dic_.csv"))
  dbFilesWithDic <- dbFiles %>% filter(withDic) %>%
    select(data,dic) %>% flatten_chr
  if(!all(dbFilesWithDic %in% fs))
    stop("db: data and dic not in folder :",
         paste(dbFilesWithDic[!dbFilesWithDic %in% fs],collapse="\n"))
  #dbs %>% separate(id,c("type","name"),extra = "merge")
  dbs
}

#' @export
load_fringes <- function(groups = NULL, n_max = Inf){
  frs <- list_fringe()
  groups <- groups %||% unique(frs$group)
  frs <- list_fringe(groups = groups)
  paths <- file.path(sysfile("data/fringe"),frs$id)
  names(paths) <- frs$id
  #f <- readFringe(paths[5],name="hola")
  lfr <- map2(paths,frs$withDic, ~ readFringe(.x, forceDic = .y,verbose = TRUE, n_max = n_max))
  lfr
}
