#' is_stable Function
#'
#' This function returns the stability of a network by calculating its eigenvalues
#' @param M A matrix
#' @export

is_stable = function(M){
  list = eigen(M)$values
  for (i in list){
    if (Re(i)>=0){
      return (FALSE)
    }
  }
  return (TRUE)
}
