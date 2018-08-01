#'pred_prey_clusters_stats Function
#'
#' This function returns 3 graphs presenting:
#' - The number of clusters for different sizes of population
#' - The average size of clusters for different sizes of population
#' - The stability of netowrks for different number of clusters
#' @param from the smallest number of species in the network (default = 2)
#' @param to the biggest number of species in the network (default = 70)
#' @param MC the number of Monte Carlo simulations (default = 10)
#' @param mean the mean (default = 0)
#' @param sd the standard deviation (default = 0.1)
#' @param C the connectance (default = 0.1)
#' @param precision the precision used to convert matrix from quantitative coefficients to binary coefficients (treshold size) (default = 0.001)
#' @export
pred_prey_clusters_stats = function(from,to,MC = 1,mean = 0,sd = 0.1,C = 0.1,precision = 0.001){
  S = c(from:to)
  x_1_predprey = c()
  x_2_predprey = c()
  x_3_predprey = c()
  x_4_predprey = c()
  for (i in 1:(to-from+1)){
    nb_clusters = 0
    size_clusters = 0
    stable = 0
    avg_degree = 0
    for (j in 1:MC){        
      info = clusters_info("pred_prey",S[i],mean,sd,C,precision)
      nb_clusters = nb_clusters + as.numeric(info[1])
      size_clusters = size_clusters + as.numeric(info[2])
      avg_degree = avg_degree + as.numeric(info[3])
      if (info[4] == TRUE) stable = stable + 1
    }
    x_1_predprey = c(x_1_predprey,nb_clusters/MC)
    x_2_predprey = c(x_2_predprey,size_clusters/MC)
    x_3_predprey = c(x_3_predprey, stable/MC)
    x_4_predprey = c(x_4_predprey, avg_degree/MC)
  }
  #par(mfrow=c(3,1))
  #plot(S,x_1_predprey,type='l',col='blue',ylab='Number of clusters',xlab='Size of population (Pred/Prey)')
  #plot(S,x_2_predprey,type='l',col='blue',ylab='Avg size of clusters',xlab='Size of population (Pred/Prey)')
  #plot(x_1_predprey,x_3_predprey,col='blue',ylab='1 <=> Stability',xlab='Number of clusters (Pred/Prey)')
  return (list(x_1_predprey,x_2_predprey,x_3_predprey,x_4_predprey))
  
}
  