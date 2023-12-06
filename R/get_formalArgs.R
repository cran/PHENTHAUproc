#' Get formalArgs of a function from a list of parameter
#'
#' @param p parameter list
#' @param f function
#' @family Helper
#' @keywords internal

get_formalArgs <- function(p, f){

  fA <- names(formals(f))
  p <- p[names(p) %in% fA]

  return(p)

}
