#' random_matrix Function
#'
#' This function returns a random matrix corresponding to a network
#' @param S the size of the population
#' @param mean the mean used in the gaussian distribution
#' @param sd the standard deviation used in the gaussian distribution
#' @param C the connectance
#' @export
random_matrix = function(S,mean,sd,C){
  may = diag(-1,S,S)
  for (i in 1:S-1){
    for (j in (i+1):S){
      if (runif(1)<=C){
        may[i,j] = rnorm(1,mean,sd)
        may[j,i] = rnorm(1,mean,sd)
      }
      else{
        may[i,j] = 0
        may[j,i] = 0
      }
    }
  }
  return (may)
}