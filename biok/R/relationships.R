#' relationships Function
#'
#' This function returns the number of edges in the network and 
#' two list showing every single relationship of the network.~
#' @param type the type of the network
#' @param S size of the population
#' @param mean used in the gaussian law
#' @param sd standard deviation used in the gaussian law
#' @param C connectance of the network
#' @param precision the treshold size
#' @export
relationships = function(type,S,mean,sd,C,precision){
  M = diag(-1,S,S)
  count = 0
  avg = 0
  part_1 = c()
  part_2 = c()
  for (i in 1:S){
    temp_avg = 0
    for (j in 1:S){
      if (i<S & j>i){
        if (runif(1)<=C){
          M[i,j] = rnorm(1,mean,sd)
          if (type == "random") M[j,i] = rnorm(1,mean,sd)
          if (type == "pred_prey" & M[i,j]>=0) M[j,i] = -abs(norm(1,mean,sd))
          if (type == "pred_prey" & M[i,j]<0) M[j,i] = abs(rnorm(1,mean,sd))
          if (type == "mixture" & M[i,j]>=0) M[j,i] = abs(rnorm(1,mean,sd))
          if (type == "mixture" & M[i,j]<0) M[j,i] = -abs(rnorm(1,mean,sd))
        }
        else {
          M[i,j] = 0
          M[j,i] = 0
        }
      }
      if (M[i,j] > precision ){
        part_1 = c(part_1,i)
        part_2 = c(part_2,j)
        if (i<S & j>i){
          count = count + 1
          temp_avg = temp_avg + 1
        } 
      }
    }
    avg = avg + temp_avg/S
  }
  return (list(count,part_1,part_2,avg,is_stable(matrix)))
}