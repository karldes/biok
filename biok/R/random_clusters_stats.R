#' random_clusters_stats Function
#'
#' This function returns 3 graphs presenting:
#' - The number of clusters for different sizes of population
#' - The average size of clusters for different sizes of population
#' - The stability of netowrks for different number of clusters
#' @param from the smallest number of species in the network (default = 2)
#' @param to the biggest number of species in the network (default = 50)
#' @param MC the number of Monte Carlo simulations (default = 10)
#' @param mean the mean (default = 0)
#' @param sd the standard deviation (default = 0.1)
#' @param C the connectance (default = 0.1)
#' @param precision the precision used to convert matrix from quantitative coefficients to binary coefficients (treshold size) (default = 0.001)
#' @export
random_clusters_stats = function(from,to,MC,mean,sd,C,precision){
  S = c(from:to)
  x_1_random = c()
  x_2_random = c()        
  x_3_random = c()
  x_4_random = c()
  for (i in 1:(to-from+1)){
    nb_clusters = 0
    size_clusters = 0
    stable = 0
    avg_degree = 0
    for (j in 1:MC){     
      info = clusters_info("random",S[i],mean,sd,C,precision)
      nb_clusters = nb_clusters + as.numeric(info[1])
      size_clusters = size_clusters + as.numeric(info[2])
      avg_degree = avg_degree + as.numeric(info[3])
      if (info[4] == TRUE) stable = stable + 1
    }
    x_1_random = c(x_1_random,nb_clusters/MC)
    x_2_random = c(x_2_random,size_clusters/MC)
    x_3_random = c(x_3_random, stable/MC)
    x_4_random = c(x_4_random, avg_degree/MC)
  }
  #par(mfrow=c(3,1))
  #plot(S,x_1_random,type='l',col='blue',ylab='Number of clusters',xlab='Size of population (Random)')
  #plot(S,x_2_random,type='l',col='blue',ylab='Avg size of clusters',xlab='Size of population (Random)')
  #plot(x_1_random,x_3_random,col='blue',ylab='1 <=> Stability',xlab='Number of clusters (Random)')
  return(list(x_1_random,x_2_random,x_3_random,x_4_random))
}