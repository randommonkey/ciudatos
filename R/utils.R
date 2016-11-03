
#' @export
`%||%` <- function (x, y)
{
  if (is.empty(x))
    return(y)
  else if (is.null(x) || is.na(x))
    return(y)
  else if( class(x)=="character" && nchar(x)==0 )
    return(y)
  else x
}

#' @export
is.empty <- function(x){
  #   !is.null(x)
  !as.logical(length(x))
}
