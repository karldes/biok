#' c_clusters Function
#'
#' This function returns the graph of the number of clusters for different sizes of population
#' @param type can be "random", "pred_prey" or "mixture" depending on the type of matrix we want to use for the network
#' @param from the smallest number of species in the network (default = 2)
#' @param to the biggest number of species in the network (default = 50)
#' @param sd the standard deviation used if "random" is chosen (default = 0.1)
#' @param mean the mean used if "random" is chosen (default = 0)
#' @param C the vector of size 3 containing different  values for the connectance C (default = c(0.01,0.1,0.5))
#' @export
c_clusters = function(type, from = 2, to = 50, sd = 0.1,mean = 0, C = c(0.01,0.1,0.5)){
  S=c(from:to)
  x_1 = c()             
  for (k in 1:length(C)){
    C_loc = C[k]
    nb_clusters = 0
    for (i in 1:(to-from+1)){
      if (type == "random") matrix = random_matrix(S[i],mean,sd,C_loc)
      if (type == "pred_prey") matrix = pred_prey_matrix(S[i],mean,sd,C_loc)
      if (type == "mixture") matrix = mixture_matrix(S[i],mean,sd,C_loc)
      info = clusters_info(matrix,S[i],precision)
      nb_clusters = nb_clusters + as.numeric(info[1])
      x_1 = c(x_1,nb_clusters)
    }
  }
  plot(S,x_1[1:(to-1)],type='l',col='blue',ylab='Number of clusters',xlab='Size of population')+
  lines(S,x_1[to:(2*to-from)],col='red')+
  lines(S,x_1[(2*to-from+1):(3*to-2*from+1)],col='green')
}