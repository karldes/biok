#' sd_clusters Function
#'
#' This function returns the graph of the size of clusters for different values of standard deviation (in the case of a random matrix)
#' @param from the smallest number of species in the network (default = 2)
#' @param to the biggest number of species in the network (default = 50)
#' @param sd the standard deviation used (default = 0.1)
#' @param mean the mean used if "random" is chosen (default = c(0.01,0.25,0.8))
#' @param C the connectance (default = 0.1)
#' @export
sd_clusters = function(from = 2, to = 50, sd = c(0.01,0.25,0.8),mean = 0, C = 0.1){
  S=c(from:to)
  x_1 = c()
  for (k in 1:length(sd)){
    sd_loc = sd[k]
    size_clusters = 0
    for (i in 1:(to-from+1)){
      matrix = random_matrix(S[i],mean,sd_loc,C)
      info = clusters_info(matrix,S[i],precision)
      size_clusters = size_clusters + as.numeric(info[2])
      x_1 = c(x_1,size_clusters)
    }
  }
  
    plot(S,x_1[1:(to-1)],type='l',col='blue',ylab='Size of clusters',xlab='Size of population (Mixture)')+
    lines(S,x_1[to:(2*to-from)],col='red')+
    lines(S,x_1[(2*to-from+1):(3*to-2*from+1)],col='green')
}