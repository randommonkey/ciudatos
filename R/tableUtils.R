
#' @export
checkCommonKeys <- function(ltbls){
  keys <- Reduce(intersect,lapply(ltbls,names))
  keys
}


